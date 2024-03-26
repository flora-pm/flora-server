module Effectful.Poolboy where

import Data.Poolboy (PoolboySettings, WorkQueue)
import Data.Poolboy qualified as Poolboy
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

data Poolboy :: Effect

type instance DispatchOf Poolboy = Static WithSideEffects
newtype instance StaticRep Poolboy = Poolboy WorkQueue

newPoolboy :: IOE :> es => PoolboySettings -> Eff es WorkQueue
newPoolboy = liftIO . Poolboy.newPoolboy

runPoolboy
  :: IOE :> es
  => PoolboySettings
  -> Eff (Poolboy : es) a
  -> Eff es a
runPoolboy settings action = do
  workQueue <- liftIO $ Poolboy.newPoolboy settings
  evalStaticRep (Poolboy workQueue) action

enqueue
  :: Poolboy :> es
  => Eff es ()
  -- ^ Action to insert in the work queue
  -> Eff es ()
enqueue action = do
  Poolboy workqueue <- getStaticRep @Poolboy
  unsafeEff $ \es0 -> do
    es <- cloneEnv es0
    Poolboy.enqueue workqueue $ unEff action es
