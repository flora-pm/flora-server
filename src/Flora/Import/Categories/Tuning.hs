module Flora.Import.Categories.Tuning where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as S8
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding qualified as T
import Data.Text.Internal.Builder qualified as TB
import GHC.Generics
import Language.Souffle.Interpreted qualified as Souffle
import System.IO (stderr)

type CName = Text

-- | This is a tag for our datalog program
data Categoriser = Categoriser
  deriving
    (Souffle.Program)
    via Souffle.ProgramOptions Categoriser "categorise" '[UserPackageCategory, NormalisedPackageCategory, NormaliseIssue]

-- | A package name and category provided by users. Input to our program.
data UserPackageCategory = UserPackageCategory Text
  deriving anyclass (Souffle.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Souffle.Fact)
    via Souffle.FactOptions UserPackageCategory "user_package_category" 'Souffle.Input

instance Display UserPackageCategory where
  displayBuilder (UserPackageCategory txt) = TB.fromText txt

-- | A normalised pair of package name and category. Output to our program.
data NormalisedPackageCategory = NormalisedPackageCategory CName
  deriving anyclass (Souffle.Marshal, FromJSON, ToJSON)
  deriving stock (Generic, Eq, Show)
  deriving
    (Souffle.Fact)
    via Souffle.FactOptions NormalisedPackageCategory "normalised_package_category" 'Souffle.Output

-- | A report that arises if no normalisation could be done.
data NormaliseIssue = NormaliseIssue CName
  deriving anyclass (Souffle.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Souffle.Fact)
    via Souffle.FactOptions NormaliseIssue "normalise_issue" 'Souffle.Output

instance Display NormaliseIssue where
  displayBuilder (NormaliseIssue name) = TB.fromText name

data Results = Results
  { normalisedCategories :: [NormalisedPackageCategory]
  , normalisationIssues :: [NormaliseIssue]
  }
  deriving stock (Generic, Eq, Show)

data SourceCategories = SourceCategories
  deriving
    (Souffle.Program)
    via Souffle.ProgramOptions SourceCategories "categorise" '[CanonicalCategory]

data CanonicalCategory = CanonicalCategory Text Text Text
  deriving anyclass (Souffle.Marshal)
  deriving stock (Generic, Eq, Show)
  deriving
    (Souffle.Fact)
    via Souffle.FactOptions CanonicalCategory "flora_category" 'Souffle.Output

-- | Entrypoint to the Soufflé datalog engine.
normalise :: [UserPackageCategory] -> IO Results
normalise input = do
  result <- liftIO $
    Souffle.runSouffle Categoriser $ \case
      Nothing ->
        error "Failed to load Souffle program!"
      Just prog -> do
        Souffle.addFacts prog input
        Souffle.run prog
        Results <$> Souffle.getFacts prog <*> Souffle.getFacts prog
  if (not . null) result.normalisationIssues
    then do
      logStdErr $ "[!] Could not normalise these categories: " <> display result.normalisationIssues
      pure result
    else pure result

sourceCategories :: IO [CanonicalCategory]
sourceCategories = do
  Souffle.runSouffle SourceCategories $ \case
    Nothing ->
      error "Failed to load Souffle program!"
    Just prog -> do
      Souffle.run prog
      Souffle.getFacts prog

logStdErr :: Text -> IO ()
logStdErr = S8.hPutStrLn stderr . T.encodeUtf8
