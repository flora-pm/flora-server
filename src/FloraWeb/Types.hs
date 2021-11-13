module FloraWeb.Types where
import Control.Monad.Reader (ReaderT)
import Flora.Environment (FloraEnv)
import Servant (Handler)

type FloraM = ReaderT FloraEnv Handler

