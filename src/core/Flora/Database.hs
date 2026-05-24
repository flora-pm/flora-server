{-# LANGUAGE AllowAmbiguousTypes #-}

module Flora.Database
  ( AccessMode (..)
  , withReadWritePool
  , withReadOnlyPool
  , queryOne
  , queryOne_
  , upsert
  , withTransaction
  ) where

import Control.Monad
import Data.Maybe
import Data.Pool (Pool)
import Data.Pool.Introspection (Resource (..))
import Data.Pool.Introspection qualified as Pool
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity hiding (upsert)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Log
import Effectful.PostgreSQL qualified as DB
import Effectful.PostgreSQL.Connection
import Log qualified

import Flora.Monad

type data AccessMode
  = ReadOnly
  | ReadWrite

withPool
  :: forall a es
   . (IOE :> es, Log :> es)
  => Pool PG.Connection
  -> Eff (WithConnection ': es) a
  -> FloraM es a
withPool pool action = do
  runWithConnectionPool pool $
    DB.withTransaction action

runWithConnectionPool
  :: (HasCallStack, IOE :> es, Log :> es)
  => Pool.Pool PG.Connection
  -> Eff (WithConnection : es) a
  -> Eff es a
runWithConnectionPool pool = interpret $ \env -> \case
  WithConnection f -> do
    loggerEnv <- getLoggerEnv
    localSeqUnlift env $ \unlift -> do
      unliftedWithResource loggerEnv.leLogger pool $ unlift . f

unliftedWithResource :: MonadUnliftIO m => Logger -> Pool Connection -> (Connection -> m b) -> m b
unliftedWithResource logger pool action = withRunInIO $ \io ->
  liftIO $ Pool.withResource pool $ \resource -> do
    runEff $
      Log.runLogT "" logger LogInfo $
        Log.logInfo "Database connection acquired" $
          object
            [ "stripe" .= resource.stripeNumber
            , "label" .= resource.poolLabel
            , "available" .= resource.availableResources
            , "time" .= resource.acquisitionTime
            , "acquisition" .= show (resource.acquisition)
            ]
    io $ action resource.resource

withReadWritePool
  :: forall a es
   . (IOE :> es, Log :> es)
  => Pool PG.Connection
  -> Eff (Labeled ReadWrite WithConnection ': es) a
  -> FloraM es a
withReadWritePool pool action = do
  runLabeled (withPool pool) action

withReadOnlyPool
  :: forall a es
   . (IOE :> es, Log :> es)
  => Pool PG.Connection
  -> Eff (Labeled ReadOnly WithConnection ': es) a
  -> FloraM es a
withReadOnlyPool pool action = do
  Log.logInfo_ "Acquiring read-only pool connection"
  runLabeled (withPool pool) action

queryOne
  :: ( HasCallStack
     , IOE :> es
     , Labeled ReadOnly WithConnection :> es
     , PG.FromRow result
     , PG.ToRow params
     )
  => PG.Query
  -> params
  -> Eff es (Maybe result)
queryOne q params =
  labeled @ReadOnly @WithConnection $
    listToMaybe <$> DB.query q params

queryOne_
  :: ( HasCallStack
     , IOE :> es
     , Labeled ReadOnly WithConnection :> es
     , PG.FromRow result
     )
  => PG.Query
  -> Eff es (Maybe result)
queryOne_ q =
  labeled @ReadOnly @WithConnection $
    listToMaybe <$> DB.query_ q

upsert
  :: forall e es values
   . (Entity e, IOE :> es, Labeled ReadWrite WithConnection :> es, ToRow values)
  => values
  -- ^ Entity to insert
  -> Vector Field
  -- ^ Fields to replace in case of conflict
  -> Eff es ()
upsert entity fieldsToReplace = void $ labeled @ReadWrite @WithConnection $ DB.execute (_insert @e <> _onConflictDoUpdate conflictTarget fieldsToReplace) entity
  where
    conflictTarget = Vector.singleton $ primaryKey @e
