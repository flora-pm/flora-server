module Flora.Import.Categories.Tuning where

import qualified Language.Souffle.Interpreted as Souffle 
import GHC.Generics
import Control.Monad.IO.Class

import Flora.Model.Category
import Flora.Model.Package

-- | This is a tag for our datalog program
data Categoriser = Categoriser

instance Souffle.Program Categoriser where
  type ProgramFacts Categoriser =
    '[ UserPackageCategory
     , NormalisedPackageCategory
     , NormaliseIssue
     ]

  programName = const "categorise"

-- | A package name and category provided by users. Input to our program.
data UserPackageCategory
  = UserPackageCategory PackageName CategoryName
  deriving (Generic, Show)

-- | A normalised pair of package name and category. Output to our program.
data NormalisedPackageCategory
  = NormalisedPackageCategory PackageName CategoryName
  deriving (Generic, Show)

-- | A report that arises if no normalisation could be done.
data NormaliseIssue
  = NormaliseIssue PackageName CategoryName
  deriving (Generic, Show)

data Results
  = Results [NormalisedPackageCategory] [NormaliseIssue]
  deriving Show

instance Souffle.Marshal UserPackageCategory
instance Souffle.Marshal NormalisedPackageCategory
instance Souffle.Marshal NormaliseIssue

instance Souffle.Fact UserPackageCategory where
  type FactDirection UserPackageCategory = 'Souffle.Input
  factName = const "user_package_category"

instance Souffle.Fact NormalisedPackageCategory where
  type FactDirection NormalisedPackageCategory = 'Souffle.Output
  factName = const "normalised_package_category"

instance Souffle.Fact NormaliseIssue where
  type FactDirection NormaliseIssue = 'Souffle.Output
  factName = const "normalise_issue"


normalise :: (MonadIO m) => [UserPackageCategory] -> m Results
normalise input = do
  Souffle.runSouffle Categoriser $ \case
    Nothing -> liftIO $ print "Failed to load Souffle program!"
    Just prog -> do
      Souffle.addFacts prog input
      Souffle.run prog
      Results <$> Souffle.getFacts prog <*> Souffle.getFacts prog
