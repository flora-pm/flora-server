module Flora.NoThunksSpec where

import Effectful.Reader.Static (ask)
import NoThunks.Class (noThunks)
import RequireCallStack

import Flora.Environment.Env (FloraEnv)
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "NoThunks tests"
    [ testThis "FloraEnv carries no unexpected thunks" testFloraEnvNoThunks
    ]

-- | Assert that the long-lived 'FloraEnv' is free of unexpected thunks, up until WHNF.
testFloraEnvNoThunks :: RequireCallStack => TestEff ()
testFloraEnvNoThunks = do
  env <- ask @FloraEnv
  mThunk <- liftIO $ noThunks [] env
  case mThunk of
    Nothing -> pure ()
    Just info ->
      assertFailure $
        "Unexpected thunk in FloraEnv (possible space leak): " <> show info
