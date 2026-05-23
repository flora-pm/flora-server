{-# LANGUAGE AllowAmbiguousTypes #-}

module Flora.Database
  ( AccessMode (..)
  , withReadWritePool
  , withReadOnlyPool
  , queryOne
  , queryOne_
  , upsert
  ) where

import Control.Monad
import Data.Maybe
import Data.Pool (Pool)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity hiding (upsert)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as DB
import GHC.Stack

type data AccessMode
  = ReadOnly
  | ReadWrite

withPool
  :: forall a es
   . IOE :> es
  => Pool PG.Connection
  -> Eff (WithConnection ': es) a
  -> Eff es a
withPool pool action = do
  DB.runWithConnectionPool pool $
    DB.withTransaction action

withReadWritePool
  :: forall a es
   . IOE :> es
  => Pool PG.Connection
  -> Eff (Labeled ReadWrite WithConnection ': es) a
  -> Eff es a
withReadWritePool pool action = runLabeled (withPool pool) action

withReadOnlyPool
  :: forall a es
   . IOE :> es
  => Pool PG.Connection
  -> Eff (Labeled ReadOnly WithConnection ': es) a
  -> Eff es a
withReadOnlyPool pool action = runLabeled (withPool pool) action

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
