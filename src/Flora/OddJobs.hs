{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Represents the various jobs that can be run
module Flora.OddJobs
  ( scheduleReadmeJob
  , runner
  , jobTableName

    -- * exposed for testing

  --   prefer using smart constructors.
  , ReadmePayload (..)
  , FloraOddJobs (..)
  , IntAesonVersion (..)
  )
where

import qualified Commonmark
import Control.Exception
import qualified Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aes
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Pool
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Entity (updateFieldsBy)
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (field)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types
import Distribution.Pretty
import Distribution.Types.Version
import Flora.Model.Package
import Flora.Model.Release
import GHC.Generics
import GHC.Stack
import Log
import qualified Lucid
import qualified Network.Wreq as Wreq
import OddJobs.Job

newtype IntAesonVersion = MkIntAesonVersion {unIntAesonVersion :: Version}
  deriving newtype (Pretty)

instance ToJSON IntAesonVersion where
  toJSON (MkIntAesonVersion x) = Aes.toJSON $ versionNumbers x

instance FromJSON IntAesonVersion where
  parseJSON val = MkIntAesonVersion . mkVersion <$> Aes.parseJSON val

data ReadmePayload = MkReadmePayload
  { mpPackage :: PackageName
  , mpReleaseId :: ReleaseId -- needed to write the readme in db
  , mpVersion :: IntAesonVersion
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- these represent the possible odd jobs we can run.
data FloraOddJobs
  = MkReadme ReadmePayload
  | DoNothing -- needed to keep this type tagged
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

scheduleReadmeJob :: Pool PG.Connection -> ReleaseId -> PackageName -> Version -> IO Job
scheduleReadmeJob conn rid package version =
  withResource conn $ \res ->
    createJob
      res
      jobTableName
      (MkReadme $ MkReadmePayload package rid $ MkIntAesonVersion version)

jobTableName :: QualifiedIdentifier
jobTableName = "oddjobs"

runner :: Pool PG.Connection -> Job -> LogT IO ()
runner pool job = localDomain "job-runner" $
  case Aes.fromJSON (jobPayload job) of
    Aes.Error str -> logAttention "decode error" str
    Aes.Success val -> case val of
      DoNothing -> logInfo "doing nothing" ()
      MkReadme x -> makeReadme pool x

makeReadme :: HasCallStack => Pool PG.Connection -> ReadmePayload -> LogT IO ()
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

  htmlTxt <- case Commonmark.commonmark ("readme " <> unpack package) bodyText of
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

data OddJobException where
  DecodeFailed :: HasCallStack => UnicodeException -> OddJobException
  MarkdownFailed :: HasCallStack => Commonmark.ParseError -> OddJobException
  deriving (Exception)

instance Show OddJobException where
  show (DecodeFailed x) = renderExceptionWithCallstack x "DecodeFailed"
  show (MarkdownFailed x) = renderExceptionWithCallstack x "MarkdownFailed"

renderExceptionWithCallstack :: (HasCallStack, Show a) => a -> String -> String
renderExceptionWithCallstack errors valueConstructor =
  "("
    <> valueConstructor
    <> " $ "
    <> show errors
    <> "/*"
    <> prettyCallStack callStack
    <> " */)"
