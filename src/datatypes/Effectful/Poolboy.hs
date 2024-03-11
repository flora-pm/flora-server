module Effectful.Poolboy where

import Data.Poolboy (PoolboySettings, WorkQueue)
import Data.Poolboy qualified as Poolboy
import Effectful
import Effectful.Dispatch.Static

data Poolboy :: Effect

type instance DispatchOf Poolboy = Static WithSideEffects
newtype instance StaticRep Poolboy = Poolboy WorkQueue

runPoolboy
  :: IOE :> es
  => PoolboySettings
  -> Eff (Poolboy : es) a
  -> Eff es a
runPoolboy settings action = do
  workQueue <- liftIO $ Poolboy.newPoolboy settings
  evalStaticRep (Poolboy workQueue) action
