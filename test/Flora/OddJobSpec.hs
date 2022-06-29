{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.OddJobSpec where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Password.Argon2 (mkPassword)
import Data.UUID
import Database.PostgreSQL.Entity.DBT
import Distribution.Types.Version
import Optics.Core
import Servant.Server
import Test.Tasty
import Test.Tasty.HUnit

import Flora.Environment
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.Model.User.Query
import Flora.OddJobs
import Flora.TestUtils
import FloraWeb.Client as Client
import FloraWeb.Routes.Pages.Sessions

-- TODO aeson roundtrip tests
spec :: TestTree
spec =
  testGroup
    "odd-job"
    [ testCase "Readme aeson fixture" $
        encode
          ( MkReadme
              ( MkReadmePayload
                  { mpPackage = PackageName "uwu"
                  , mpReleaseId = ReleaseId $ fromWords 12 13 14 15
                  , mpVersion = MkIntAesonVersion (mkVersion [1, 2, 3])
                  }
              )
          )
          @?= "{\"contents\":{\"mpPackage\":\"uwu\",\"mpReleaseId\":\"0000000c-0000-000d-0000-000e0000000f\",\"mpVersion\":[1,2,3]},\"tag\":\"MkReadme\"}"
    ]
