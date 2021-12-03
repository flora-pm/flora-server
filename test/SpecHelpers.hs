module SpecHelpers where

import Control.Monad
import System.Process.Typed
import Test.Hspec
import Database.PostgreSQL.Transact

instance Example (DBT IO a) where
  evaluateExample 


migrate :: IO ()
migrate = 
  void $ runProcess "make db-setup"
