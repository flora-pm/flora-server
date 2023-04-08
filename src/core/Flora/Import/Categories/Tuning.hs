module Flora.Import.Categories.Tuning where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as S8
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding qualified as T
import Data.Text.Internal.Builder qualified as TB
import GHC.Generics
import Language.Eclair qualified as Eclair
import System.IO (stderr)

type CName = Text

-- | This is a tag for our datalog program
data Categoriser = Categoriser
  deriving
    (Eclair.Program)
    via Eclair.ProgramOptions Categoriser "categorise" '[UserPackageCategory, NormalisedPackageCategory, NormaliseIssue]

-- | A package name and category provided by users. Input to our program.
data UserPackageCategory = UserPackageCategory Text
  deriving anyclass (Eclair.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Eclair.Fact)
    via Eclair.FactOptions UserPackageCategory "user_package_category" 'Eclair.Input

instance Display UserPackageCategory where
  displayBuilder (UserPackageCategory txt) = TB.fromText txt

-- | A normalised pair of package name and category. Output to our program.
data NormalisedPackageCategory = NormalisedPackageCategory CName
  deriving anyclass (Eclair.Marshal, FromJSON, ToJSON)
  deriving stock (Generic, Eq, Show)
  deriving
    (Eclair.Fact)
    via Eclair.FactOptions NormalisedPackageCategory "normalised_package_category" 'Eclair.Output

-- | A report that arises if no normalisation could be done.
data NormaliseIssue = NormaliseIssue CName
  deriving anyclass (Eclair.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Eclair.Fact)
    via Eclair.FactOptions NormaliseIssue "normalise_issue" 'Eclair.Output

instance Display NormaliseIssue where
  displayBuilder (NormaliseIssue name) = TB.fromText name

data Results = Results
  { normalisedCategories :: [NormalisedPackageCategory]
  , normalisationIssues :: [NormaliseIssue]
  }
  deriving stock (Generic, Eq, Show)

data SourceCategories = SourceCategories
  deriving
    (Eclair.Program)
    via Eclair.ProgramOptions SourceCategories "categorise" '[CanonicalCategory]

data CanonicalCategory = CanonicalCategory Text Text Text
  deriving anyclass (Eclair.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Eclair.Fact)
    via Eclair.FactOptions CanonicalCategory "flora_category" 'Eclair.Output

-- | Entrypoint to the Soufflé datalog engine.
normalise :: [UserPackageCategory] -> IO Results
normalise input = do
  result <-
    liftIO $
      Eclair.withEclair Categoriser $! \case
        Nothing ->
          error "Failed to load Eclair program!"
        Just prog -> do
          Eclair.addFacts prog input
          Eclair.run prog
          Results <$> Eclair.getFacts prog <*> Eclair.getFacts prog
  if (not . null) result.normalisationIssues
    then do
      logStdErr $! "[!] Could not normalise these categories: " <> display result.normalisationIssues
      pure result
    else pure result

sourceCategories :: IO [CanonicalCategory]
sourceCategories = do
  Eclair.withEclair SourceCategories $! \case
    Nothing ->
      error "Failed to load Eclair program!"
    Just prog -> do
      Eclair.run prog
      Eclair.getFacts prog

logStdErr :: Text -> IO ()
logStdErr = S8.hPutStrLn stderr . T.encodeUtf8
