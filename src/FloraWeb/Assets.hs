module FloraWeb.Assets where

import Crypto.Hash
import Crypto.Hash.Conduit (hashFile)
import Data.Text.Display (display)
import Data.Text (Text)
import Language.Haskell.TH (runIO, stringE)
import Language.Haskell.TH.Syntax (qAddDependentFile, Q, Exp)
import qualified Data.Text as Text
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as B64

-- | Returns a base64-encoded sha256 hash of the file
hashBundle :: FilePath -> IO Text
hashBundle path = do
  digest :: Digest SHA256 <- hashFile path
  pure . display . B64.encodeBase64 . BA.convert $ digest

jsHash :: Q Exp
jsHash = do
  let path = "./static/js/app.js"
  qAddDependentFile path
  result <- runIO $ hashBundle path
  stringE (Text.unpack result)

cssHash :: Q Exp
cssHash = do
  let path = "./static/css/app.css"
  qAddDependentFile path
  result <- runIO $ hashBundle path
  stringE (Text.unpack result)

