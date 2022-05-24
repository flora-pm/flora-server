{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.OddJobSpec where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Password.Argon2 (mkPassword)
import Database.PostgreSQL.Entity.DBT
import Distribution.Types.Version
import Flora.Model.Release
import Optics.Core
import Servant.Server
import Test.Tasty

import Data.Aeson
import Data.UUID
import Flora.Environment
import Flora.Model.Package.Types
import Flora.Model.User
import Flora.Model.User.Query
import Flora.OddJobs
import Flora.TestUtils
import FloraWeb.Client as Client
import FloraWeb.Routes.Pages.Sessions
import Test.Tasty.HUnit

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
