module FloraJobs.ThirdParties.GitHub.Client where

import Data.Proxy
import Data.Text
import GitHub

runRequest :: Auth -> Request k Content -> IO (Either Error Content)
runRequest auth request =
  github auth request

fetchFundingFile :: Text -> Text -> Request k Content
fetchFundingFile textOwner textRepo =
  let owner = mkName (Proxy :: Proxy Owner) textOwner
      repo = mkName (Proxy :: Proxy Repo) textRepo
      filePath = ".github/FUNDING.yml"
   in contentsForR owner repo filePath Nothing
