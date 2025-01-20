{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Positive where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeNats (KnownNat, Natural, natVal)
import Servant (FromHttpApiData (..), ToHttpApiData (..))

newtype Positive a = PositiveUnsafe {unPositive :: a}
  deriving (Eq, Ord, Show)
  deriving
    (Num)
    via a

-- >>> toPositive 42
toPositive :: (Num a, Ord a, Show a) => a -> Either Text (Positive a)
toPositive a
  | a > 0 = Right $ PositiveUnsafe a
  | otherwise = Left $ "Non-positive value: " <> Text.pack (show a)

unsafeToPositive :: (HasCallStack, Num a, Ord a, Show a) => a -> Positive a
unsafeToPositive = either (error . Text.unpack) id . toPositive

type family IsNotZero (k :: Natural) :: Constraint where
  IsNotZero 0 = TypeError ('Text "0 is now allowed here")
  IsNotZero k = ()

type KnownPositive k = (KnownNat k, IsNotZero k)

positiveVal :: forall k i. (KnownPositive k, Num i) => Positive i
positiveVal = PositiveUnsafe . fromIntegral $ natVal @k Proxy

instance (FromHttpApiData a, Num a, Ord a, Show a) => FromHttpApiData (Positive a) where
  parseUrlPiece t = parseUrlPiece @a t >>= toPositive

instance ToHttpApiData a => ToHttpApiData (Positive a) where
  toUrlPiece = toUrlPiece @a . unPositive
