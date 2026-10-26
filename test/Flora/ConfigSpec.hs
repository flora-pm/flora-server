module Flora.ConfigSpec where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import KDL qualified
import RequireCallStack
import Test.Tasty.HUnit qualified as Test

import Flora.Environment.Config
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Pool configuration"
    [ testThis "importWorkers defaults to 4" $ do
        poolConfig <- decodeOrFail "pool { timeout 10; connections 50; }"
        assertEqual "importWorkers" 4 poolConfig.importWorkers
    , testThis "handlers and import workers fill the handler pool exactly" $ do
        let limits connections importWorkers = do
              poolConfig <- decodeOrFail $ poolNode connections importWorkers
              pure (handlerConnections poolConfig, jobWorkerLimit poolConfig)
        limits 15 6 >>= assertEqual "15 connections, 6 import workers" (9, 3)
        limits 20 4 >>= assertEqual "20 connections, 4 import workers" (14, 10)
        limits 50 4 >>= assertEqual "50 connections, 4 import workers" (44, 40)
    , testThis "rejects importWorkers below 1" $
        assertBool . isLeft $
          decode (poolNode 50 0)
    , testThis "rejects a pool without room for one job worker" $ do
        assertBool . isLeft $ decode (poolNode (arbiterConnections + 4) 4)
        assertBool . not . isLeft $ decode (poolNode (arbiterConnections + 5) 4)
    ]
  where
    decode :: Text -> Either KDL.DecodeError PoolConfig
    decode = KDL.decodeWith (KDL.document (KDL.nodeWith "pool" poolConfigDecoder))

    decodeOrFail :: Text -> TestEff PoolConfig
    decodeOrFail input = either (liftIO . Test.assertFailure . show) pure $ decode input

    poolNode :: Int -> Int -> Text
    poolNode connections importWorkers =
      "pool { timeout 10; connections "
        <> Text.show connections
        <> "; importWorkers "
        <> Text.show importWorkers
        <> "; }"
