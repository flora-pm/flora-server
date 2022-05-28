module Flora.ThirdParties.Hackage.Importer where

import Control.Exception (try)
import Data.Text as T (Text, pack, splitOn)
import GHC.IO.Handle (Handle, hGetLine)
import System.Process (shell, withCreateProcess)

data CabalListOutput = CabalListOutput {packageName :: Text, packageVersion :: Text}


-- | Similar to 'foldAllPackages', but uses the 'Monoid' instance rather than a folding function.
foldMapAllPackages :: Monoid t => (CabalListOutput -> t) -> IO t
foldMapAllPackages f = foldAllPackages (mappend . f) mempty

-- | Calls "cabal list --simple" to get a complete list of all available Haskell packages, and folds
-- over the packages using the provided function. 
-- Folding should be done lazily, so packages can be processed in constant space. TODO: verify laziness somehow
foldAllPackages :: (CabalListOutput -> t -> t) -> t -> IO t
foldAllPackages f initialValue = withCreateProcess (shell "cabal list --simple") createProcessFn
  where
    -- Builds a 'CabalListOutput' from a list of strings
    cabalListOutput (a : b : _) = Just $ CabalListOutput a b
    cabalListOutput _ = Nothing

    -- Parses the output of "cabal list --simple", which is 'packageName packageVersion'
    parseCabalOutput = cabalListOutput . T.splitOn " " . pack

    f' line acc = maybe acc (`f` acc) (parseCabalOutput line)
    createProcessFn _ (Just stdOutHandle) _ _ = foldHandleLines stdOutHandle f' initialValue
    createProcessFn _ _ _ _ = pure initialValue

foldHandleLines :: Handle -> (String -> t -> t) -> t -> IO t
foldHandleLines handle f initialValue = go initialValue
  where
    go acc =
      (try @IOError $ hGetLine handle)
        >>= \case
          Right line -> go (f line acc)
          Left _ -> pure acc

downloadPackage :: FilePath -> CabalListOutput -> IO ()
downloadPackage = undefined