{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Commonmark.Extensions (emojiSpec)
import Control.Exception
import qualified Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (fromJSON, Result (..))
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Pool
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Entity (updateFieldsBy)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (field)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types
import Distribution.Pretty
import Distribution.Types.Version
import GHC.Stack
import Log
import qualified Lucid
import qualified Network.Wreq as Wreq
import OddJobs.Job hiding (Success)

import Flora.Model.Package
import Flora.Model.Release
import Flora.OddJobs.Types

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob conn rid package version =
  withResource conn $ \res ->
    createJob
      res
      jobTableName
      (MkReadme $ MkReadmePayload package rid $ MkIntAesonVersion version)

makeReadme :: HasCallStack => Pool PG.Connection -> ReadmePayload -> JobsRunnerM ()
makeReadme pool pay@MkReadmePayload{..} = localDomain ("for-package " <> package) $ do
  logInfo "making readme" pay
  gewt <-
    liftIO $
      Wreq.get $
        "https://hackage.haskell.org/package/"
          <> unpack package
          <> "-"
          <> prettyShow mpVersion
          <> "/readme.txt"

  logInfo "got a response" ()

  let respBody :: ByteString
      respBody = gewt L.^. Wreq.responseBody

  bodyText <- case decodeUtf8' (toStrict respBody) of
    Left exception -> liftIO $ throwIO $ DecodeFailed exception
    Right x -> pure x

  logInfo "got a body" bodyText

  let extensions = emojiSpec
  htmlTxt <-
    Commonmark.commonmarkWith extensions ("readme " <> unpack package) bodyText
      >>= \case
        Left exception -> liftIO $ throwIO $ MarkdownFailed exception
        Right (y :: Commonmark.Html ()) -> pure $ Commonmark.renderHtml y

  let readmeBody :: Lucid.Html ()
      readmeBody = Lucid.toHtmlRaw @Text $ TL.toStrict htmlTxt

  void $
    liftIO $
      withPool pool $
        updateFieldsBy @Release
          [[field| readme |]]
          ([field| release_id |], mpReleaseId)
          (Only $ Just $ MkTextHtml readmeBody)
  where
    package = unPackageName mpPackage

runner :: Pool PG.Connection -> Job -> JobsRunnerM ()
runner pool job = localDomain "job-runner" $
  case fromJSON (jobPayload job) of
    Error str -> logAttention "decode error" str
    Success val -> case val of
      DoNothing -> logInfo "doing nothing" ()
      MkReadme x -> makeReadme pool x
