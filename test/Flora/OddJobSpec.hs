module Flora.OddJobSpec where

import Data.Aeson
import Data.UUID
import Distribution.Types.Version
import Test.Tasty
import Test.Tasty.HUnit

import Flora.Model.Job
import Flora.Model.Package.Types
import Flora.Model.Release.Types

-- TODO aeson roundtrip tests
spec :: TestTree
spec =
  testGroup
    "odd-job"
    [ testCase "Readme aeson fixture" $
        encode
          ( FetchReadme
              ( ReadmeJobPayload
                  { mpPackage = PackageName "uwu"
                  , mpReleaseId = ReleaseId $ fromWords 12 13 14 15
                  , mpVersion = MkIntAesonVersion (mkVersion [1, 2, 3])
                  }
              )
          )
          @?= "{\"contents\":{\"mpPackage\":\"uwu\",\"mpReleaseId\":\"0000000c-0000-000d-0000-000e0000000f\",\"mpVersion\":[1,2,3]},\"tag\":\"FetchReadme\"}"
    ]
