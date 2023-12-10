{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Common.Pagination where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.OpenApi qualified as O
import Data.Positive
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits
import Optics.Core
import Servant
import Servant.OpenApi

import FloraWeb.Servant.Common

data PaginationSpec = PaginationSpec
  { offset :: Maybe Word
  , limit :: Maybe (Positive Word)
  }
  deriving stock (Eq, Ord, Show)

fromPage :: Positive Word -> (Word, Word)
fromPage pageNumber =
  let
    limit = positiveVal @30
    offset = 30 * unPositive pageNumber - 30
   in
    (offset, unPositive limit)

data PaginationParameters (settings :: PageSize)

data PageSize
  = PageSize Natural
  | UnlimitedPageSize
  deriving stock (Eq, Ord, Show)

class KnownPaginationPageSize (settings :: PageSize) where
  settingDefPageSize :: Maybe (Positive Word)

instance KnownPositive pageSize => KnownPaginationPageSize ('PageSize pageSize) where
  settingDefPageSize = Just (positiveVal @pageSize)

instance KnownPaginationPageSize 'UnlimitedPageSize where
  settingDefPageSize = Nothing

-- | How servant sees 'PaginationParams' under the hood.
type PaginationParamsExpanded subApi =
  QueryParam "offset" Word
    :> QueryParam "limit" (Positive Word)
    :> subApi

instance
  ( HasServer subApi ctx
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  , KnownPaginationPageSize settings
  )
  => HasServer (PaginationParameters settings :> subApi) ctx
  where
  type
    ServerT (PaginationParameters settings :> subApi) m =
      PaginationSpec -> ServerT subApi m

  route =
    inRouteServer @(PaginationParamsExpanded subApi) route $
      \handler offset limit ->
        handler
          PaginationSpec
            { offset = Just $ offset ?: 0
            , limit = limit <|> settingDefPageSize @settings
            }

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy @subApi) pc nt . s

-- OpenApi instance
instance
  (HasOpenApi api, KnownPaginationPageSize settings)
  => HasOpenApi (PaginationParameters settings :> api)
  where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & #allOperations
      % #parameters
      <>~ [O.Inline offsetParam, O.Inline limitParam]
    where
      offsetParam :: O.Param
      limitParam :: O.Param
      offsetParam =
        mempty
          & #name
          .~ "offset"
          & #description
          ?~ "Pagination parameter. How many items to skip from the beginning."
          & #required
          ?~ False
          & #in
          .~ O.ParamQuery
          & #schema
          ?~ O.Inline offsetParamSchema
      offsetParamSchema =
        mempty
          & #type
          ?~ O.OpenApiInteger
          & #format
          ?~ "int32"

      limitParam =
        mempty
          & #name
          .~ "limit"
          & #description
          ?~ mconcat
            [ "Pagination parameter. Maximum number of items to return.\n"
            , defaultPageSizeDesc
            ]
          & #required
          ?~ False
          & #in
          .~ O.ParamQuery
          & #schema
          ?~ O.Inline limitParamSchema
      limitParamSchema =
        mempty
          & #type
          ?~ O.OpenApiInteger
          & #format
          ?~ "int32"
          & #pattern
          ?~ "^\\d*[1-9]\\d*$"
      defaultPageSizeDesc :: Text
      defaultPageSizeDesc = case settingDefPageSize @settings of
        Nothing -> "By default, no limit will be applied."
        Just s -> "Defaults to " <> Text.pack (show $ unPositive s) <> "."

instance
  ( a ~ O.Operation
  , b ~ O.Operation
  )
  => LabelOptic "allOperations" A_Traversal O.OpenApi O.OpenApi a b
  where
  labelOptic = #paths % traversed % gplate
  {-# INLINE labelOptic #-}

(<>~) :: (Is k A_Setter, Semigroup b) => Optic k is s t b b -> b -> s -> t
l <>~ n = over l (<> n)
infixr 4 <>~

(?:) :: Maybe a -> a -> a
mA ?: b = fromMaybe b mA
{-# INLINE (?:) #-}
infixr 0 ?:
