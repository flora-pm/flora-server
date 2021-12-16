module FloraWeb.Types where

import Control.Monad.Reader (ReaderT)
import Flora.Environment
import Servant (Handler)

type FloraM = ReaderT FloraEnv Handler
