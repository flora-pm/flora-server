module Flora.Model.BlobStore.API
  ( -- | Effect
    BlobStoreAPI
  , get
  , put
  , hashByteString
    -- | Handlers
  , runBlobStoreFS
  , runBlobStorePure
  )
where

import Crypto.Hash.SHA256 qualified as SHA
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Display (display)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret, reinterpret, send)
import Effectful.State.Static.Local (evalState, gets, modify)

import Flora.Model.BlobStore.Types

data BlobStoreAPI :: Effect where
  Get :: Sha256Sum -> BlobStoreAPI m (Maybe ByteString)
  Put :: Sha256Sum -> ByteString -> BlobStoreAPI m ()

type instance DispatchOf BlobStoreAPI = Dynamic

get :: BlobStoreAPI :> es => Sha256Sum -> Eff es (Maybe ByteString)
get = send . Get

put :: BlobStoreAPI :> es => Sha256Sum -> ByteString -> Eff es ()
put hash content = send (Put hash content)

hashByteString :: ByteString -> Sha256Sum
hashByteString = Sha256Sum . SHA.hash

-- | Run a blob store in a local filepath
runBlobStoreFS
  :: forall es a
   . IOE :> es
  => FilePath
  -> Eff (BlobStoreAPI : es) a
  -> Eff es a
runBlobStoreFS fp e = do
  liftIO $ createDirectoryIfMissing True fp
  interpret (const handler) e
  where
    -- To avoid excessive entries in one directory we create directories of first
    -- two characters
    doesHashExist :: Sha256Sum -> Eff es (FilePath, Bool)
    doesHashExist hash = liftIO $ do
      let hashStr = T.unpack (display hash)
          dir = fp </> take 2 hashStr
          file = dir </> drop 2 hashStr
      createDirectoryIfMissing True dir
      exists <- doesFileExist file
      pure (file, exists)

    -- Need to tie the handler type to the top level type
    handler :: BlobStoreAPI (Eff localEs) a' -> Eff es a'
    handler = \case
      Get hash -> do
        (file, exists) <- doesHashExist hash
        if exists
          then Just <$> liftIO (BS.readFile file)
          else pure Nothing
      Put hash content -> do
        (file, exists) <- doesHashExist hash
        if exists then pure () else liftIO $ BS.writeFile file content

-- | Nun a pure in memory implementation of the blob store
--
-- This should only really be used for testing
runBlobStorePure :: Eff (BlobStoreAPI : es) a -> Eff es a
runBlobStorePure = reinterpret (evalState $ M.empty @Sha256Sum @ByteString) $
  const $
    \case
      Get hash -> gets $ M.lookup hash
      Put hash content -> modify $ M.insert hash content
