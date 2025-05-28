{-# LANGUAGE RecordWildCards #-}

module Flora.DB.Exception where

import Control.Exception (Exception)
import Data.ByteString
import Data.Data (Typeable)
import Database.PostgreSQL.Simple (ExecStatus, SqlError (..))
import GHC.Stack
import RequireCallStack

data DBException = DBException
  { sqlState :: StrictByteString
  , sqlExecStatus :: ExecStatus
  , sqlErrorMsg :: StrictByteString
  , sqlErrorDetail :: StrictByteString
  , sqlErrorHint :: StrictByteString
  , dbCallStack :: CallStack
  }
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

sqlErrorToDBException :: RequireCallStack => SqlError -> DBException
sqlErrorToDBException SqlError{..} =
  let dbCallStack = callStack
   in DBException{..}
