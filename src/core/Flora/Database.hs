{-# LANGUAGE AllowAmbiguousTypes #-}

module Flora.Database
  ( -- * Effects
    ReadDB
  , WriteDB

    -- * Read operations
  , query
  , query_
  , queryOne
  , queryOne_
  , queryCount
  , queryCount_

    -- * Write operations
  , execute
  , execute_
  , executeMany
  , upsert
  , upsertMany

    -- * Interpreters
  , withReadOnlyPool
  , withReadWritePool
  ) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Data.Pool.Introspection (Resource (..))
import Data.Pool.Introspection qualified as Pool
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (Entity, primaryKey, _insert, _onConflictDoUpdate)
import Database.PostgreSQL.Entity.Types (Field)
import Database.PostgreSQL.Simple (Connection, FromRow, Only (..), Query, ToRow)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Transaction qualified as PGTransaction
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Log
import Log qualified

import Flora.Monad (FloraM)

-- | The read capability: queries that return rows.
data ReadDB :: Effect where
  Query :: (FromRow r, ToRow q) => Query -> q -> ReadDB m [r]
  Query_ :: FromRow r => Query -> ReadDB m [r]

type instance DispatchOf ReadDB = Dynamic

-- | The write capability: statements that change rows.
data WriteDB :: Effect where
  Execute :: ToRow q => Query -> q -> WriteDB m Int64
  Execute_ :: Query -> WriteDB m Int64
  ExecuteMany :: ToRow q => Query -> [q] -> WriteDB m Int64

type instance DispatchOf WriteDB = Dynamic

-- | Run a query with parameters.
query :: (FromRow r, ReadDB :> es, ToRow q) => Query -> q -> Eff es [r]
query q params = send (Query q params)

-- | Run a parameterless query.
query_ :: (FromRow r, ReadDB :> es) => Query -> Eff es [r]
query_ q = send (Query_ q)

-- | Run a query expecting at most one row.
queryOne :: (FromRow r, ReadDB :> es, ToRow q) => Query -> q -> Eff es (Maybe r)
queryOne q params = listToMaybe <$> query q params

-- | Run a parameterless query expecting at most one row.
queryOne_ :: (FromRow r, ReadDB :> es) => Query -> Eff es (Maybe r)
queryOne_ q = listToMaybe <$> query_ q

-- | Run a count query returning a single @Int@ column, yielding 0 when no row
-- comes back.
queryCount :: (Integral n, ReadDB :> es, ToRow q) => Query -> q -> Eff es n
queryCount q params = maybe 0 (\(Only n) -> fromIntegral (n :: Int)) <$> queryOne q params

-- | Parameterless 'queryCount'.
queryCount_ :: (Integral n, ReadDB :> es) => Query -> Eff es n
queryCount_ q = maybe 0 (\(Only n) -> fromIntegral (n :: Int)) <$> queryOne_ q

-- | Run a statement with parameters, returning the number of affected rows.
execute :: (ToRow q, WriteDB :> es) => Query -> q -> Eff es Int64
execute q params = send (Execute q params)

-- | Run a parameterless statement.
execute_ :: WriteDB :> es => Query -> Eff es Int64
execute_ q = send (Execute_ q)

-- | Run a statement once per parameter row.
executeMany :: (ToRow q, WriteDB :> es) => Query -> [q] -> Eff es Int64
executeMany q params = send (ExecuteMany q params)

upsertQuery :: forall e. Entity e => Vector Field -> Query
upsertQuery fieldsToReplace =
  _insert @e <> _onConflictDoUpdate (Vector.singleton (primaryKey @e)) fieldsToReplace

-- | Insert an entity, replacing the given fields on primary-key conflict.
upsert
  :: forall e es values
   . (Entity e, ToRow values, WriteDB :> es)
  => values
  -- ^ Entity to insert
  -> Vector Field
  -- ^ Fields to replace in case of conflict
  -> Eff es ()
upsert entity fieldsToReplace =
  void $ execute (upsertQuery @e fieldsToReplace) entity

-- | Bulk upsert. Callers must not pass two rows with the same primary key
-- in one batch, because `ON CONFLICT DO UPDATE` rejects a repeated key.
upsertMany
  :: forall e es values
   . (Entity e, ToRow values, WriteDB :> es)
  => [values]
  -> Vector Field
  -> Eff es ()
upsertMany entities fieldsToReplace =
  void $ executeMany (upsertQuery @e fieldsToReplace) entities

-- | Discharge 'ReadDB' against a fixed connection.
interpretReadDB :: IOE :> es => Connection -> Eff (ReadDB ': es) a -> Eff es a
interpretReadDB conn = interpret $ \_ -> \case
  Query q params -> liftIO (PG.query conn q params)
  Query_ q -> liftIO (PG.query_ conn q)

-- | Discharge 'WriteDB' against a fixed connection.
interpretWriteDB :: IOE :> es => Connection -> Eff (WriteDB ': es) a -> Eff es a
interpretWriteDB conn = interpret $ \_ -> \case
  Execute q params -> liftIO (PG.execute conn q params)
  Execute_ q -> liftIO (PG.execute_ conn q)
  ExecuteMany q params -> liftIO (PG.executeMany conn q params)

-- | Run a read-only action on a __single__ pooled connection wrapped in a
-- single @READ ONLY@ transaction. Only 'ReadDB' is in scope, so writes are a
-- compile error here.
withReadOnlyPool
  :: forall a es
   . (IOE :> es, Log :> es)
  => Pool Connection
  -> Eff (ReadDB ': es) a
  -> FloraM es a
withReadOnlyPool pool action =
  runTransaction ReadOnly pool $ \conn -> interpretReadDB conn action

-- | Run a read-write action on a __single__ pooled connection wrapped in a
-- single read-write transaction. Both 'WriteDB' and 'ReadDB' are discharged
-- against that one connection, so reads issued by a write path share its
-- transaction instead of drawing a second connection.
withReadWritePool
  :: forall a es
   . (IOE :> es, Log :> es)
  => Pool Connection
  -> Eff (WriteDB ': ReadDB ': es) a
  -> FloraM es a
withReadWritePool pool action =
  runTransaction ReadWrite pool $ \conn -> interpretReadDB conn (interpretWriteDB conn action)

-- | Acquire one pooled connection in the transaction mode implied by the
-- 'AccessMode' and run the supplied interpreter against it. The same
-- 'AccessMode' drives both the connection log label and the Postgres
-- transaction mode, so the two cannot drift.
runTransaction
  :: forall a es
   . (IOE :> es, Log :> es)
  => AccessMode
  -> Pool Connection
  -> (Connection -> Eff es a)
  -> FloraM es a
runTransaction mode pool run = do
  loggerEnv <- getLoggerEnv
  withRunInIO $ \io ->
    unliftedWithResource mode loggerEnv pool $ \conn ->
      PGTransaction.withTransactionMode (transactionMode mode) conn $
        io (run conn)

data AccessMode
  = ReadOnly
  | ReadWrite
  deriving stock (Eq, Ord, Show)

instance Display AccessMode where
  displayBuilder ReadOnly = "read_only"
  displayBuilder ReadWrite = "read_write"

transactionMode :: AccessMode -> PGTransaction.TransactionMode
transactionMode ReadOnly =
  PGTransaction.TransactionMode PGTransaction.defaultIsolationLevel PGTransaction.ReadOnly
transactionMode ReadWrite =
  PGTransaction.TransactionMode PGTransaction.defaultIsolationLevel PGTransaction.ReadWrite

unliftedWithResource
  :: MonadUnliftIO m
  => AccessMode
  -> LoggerEnv
  -> Pool Connection
  -> (Connection -> m b)
  -> m b
unliftedWithResource accessMode loggerEnv pool action = withRunInIO $ \io ->
  liftIO $ Pool.withResource pool $ \resource -> do
    runEff $
      Log.runLogT loggerEnv.leComponent loggerEnv.leLogger LogInfo $
        Log.logInfo "Database connection acquired" $
          object
            [ "stripe" .= resource.stripeNumber
            , "label" .= resource.poolLabel
            , "available" .= resource.availableResources
            , "time" .= resource.acquisitionTime
            , "acquisition" .= show resource.acquisition
            , "access_mode" .= display accessMode
            ]
    io $ action resource.resource
