-- | Represents the various jobs that can be run
module Flora.OddJobs
  ( scheduleReadmeJob
  , jobTableName
  , runner

    -- * exposed for testing

  --   prefer using smart constructors.
  , ReadmePayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import qualified Commonmark
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson (Result (..), fromJSON)
import Data.Pool
import Data.Text
import Data.Text.Display
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Entity.DBT
import qualified Database.PostgreSQL.Simple as PG
import Distribution.Types.Version
import GHC.Stack
import Log
import qualified Lucid
import Network.HTTP.Types (notFound404)
import OddJobs.Job (Job (..), createJob)
import Optics.Core
import Servant.Client (ClientError (..))
import Servant.Client.Core (ResponseF (..))

import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Release.Update (updateReadme)
import Flora.OddJobs.Types
import Flora.ThirdParties.Hackage.API (VersionedPackage (..))
import qualified Flora.ThirdParties.Hackage.Client as Hackage

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob conn rid package version =
  withResource conn $ \res ->
    createJob
      res
      jobTableName
      (MkReadme $ MkReadmePayload package rid $ MkIntAesonVersion version)

makeReadme :: HasCallStack => Pool PG.Connection -> ReadmePayload -> JobsRunnerM ()
makeReadme pool pay@MkReadmePayload{..} = localDomain ("for-package " <> display mpPackage) $ do
  logInfo "making readme" pay
  let payload = VersionedPackage mpPackage mpVersion
  gewt <- Hackage.request $ Hackage.getPackageReadme payload
  case gewt of
    Left e@(FailureResponse _ response) -> do
      -- If the README simply doesn't exist, we skip it by marking it as successful.
      if response ^. #responseStatusCode == notFound404
        then liftIO $ withPool pool $ updateReadme mpReleaseId Nothing
        else throw e
    Left e -> throw e
    Right bodyText -> do
      logInfo "got a body" bodyText

      htmlTxt <- do
        -- let extensions = emojiSpec
        -- Commonmark.commonmarkWith extensions ("readme " <> show mpPackage) bodyText
        pure (Commonmark.commonmark ("readme " <> show mpPackage) bodyText)
          >>= \case
            Left exception -> throw (MarkdownFailed exception)
            Right (y :: Commonmark.Html ()) -> pure $ Commonmark.renderHtml y

      let readmeBody :: Lucid.Html ()
          readmeBody = Lucid.toHtmlRaw @Text $ TL.toStrict htmlTxt

      liftIO $ withPool pool $ updateReadme mpReleaseId (Just $ MkTextHtml readmeBody)

runner :: Pool PG.Connection -> Job -> JobsRunnerM ()
runner pool job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logAttention "decode error" str
    Success val -> case val of
      DoNothing -> logInfo "doing nothing" ()
      MkReadme x -> makeReadme pool x
