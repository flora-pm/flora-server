{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.ConfVar where

import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Data.Dynamic (Typeable)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Deriving.Aeson
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Flag (unFlagName)
import Distribution.Version (isAnyVersion)

import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Distribution.Orphans.Version ()

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''OS)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Arch)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', sumEncoding = ObjectWithSingleField} ''ConfVar)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', sumEncoding = ObjectWithSingleField} ''Condition)

instance (ToJSON c, Typeable c) => ToField (Condition c) where toField = toJSONField
instance (FromJSON c, Typeable c) => FromField (Condition c) where fromField = fromJSONField

instance Display ConfVar where
  displayBuilder = \case
    OS os -> displayBuilder ("os(" <> Text.pack (prettyShow os) <> ")")
    Arch arch -> displayBuilder ("arch(" <> Text.pack (prettyShow arch) <> ")")
    PackageFlag flagName -> displayBuilder ("flag(" <> Text.pack (unFlagName flagName) <> ")")
    Impl compiler range ->
      displayBuilder ("impl(" <> display compiler <> (if isAnyVersion range then "" else " " <> display range) <> ")")

data ConditionContext
  = TopLevel
  | InConjunction
  | InDisjunction
  | InNegation
  deriving stock (Eq)

instance Display (Condition ConfVar) where
  displayBuilder = displayBuilder . go TopLevel
    where
      go :: ConditionContext -> Condition ConfVar -> Text
      go _ (Var confVar) = display confVar
      go _ (Lit b) = if b then "true" else "false"
      go _ (CNot condition) = "!" <> go InNegation condition
      go context (CAnd left right) =
        parenthesiseUnless (context `elem` [TopLevel, InConjunction]) $
          go InConjunction left <> " && " <> go InConjunction right
      go context (COr left right) =
        parenthesiseUnless (context `elem` [TopLevel, InDisjunction]) $
          go InDisjunction left <> " || " <> go InDisjunction right

      parenthesiseUnless :: Bool -> Text -> Text
      parenthesiseUnless True text = text
      parenthesiseUnless False text = "(" <> text <> ")"
