module Miso.IndexDb.Tx
  ( Transaction(..)
  , TxMode(..)
  , abort
  , commit
  , TransactionCallbacks
  , TransactionAwait
  , withTransaction
  , onTransactionComplete
  , onTransactionError
  , onTransactionAbort
  , awaitTransaction
  ) where

import Miso.IndexDb.Internal
import Miso.Promise
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import UnliftIO (newEmptyMVar, putMVar, readMVar)
import Prelude

newtype Transaction = Transaction JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

data TxMode = ReadOnly | ReadWrite | VersionChange

instance ToJSVal TxMode where
  toJSVal ReadOnly = toJSVal ("readonly" :: MisoString)
  toJSVal ReadWrite = toJSVal ("readwrite" :: MisoString)
  toJSVal VersionChange = toJSVal ("versionchange" :: MisoString)

transaction :: MonadIO m =>
  Database ->
  [MisoString] ->
  TxMode ->
  m Transaction
transaction (Database db) names mode =
  Transaction <$> (db # "transaction" $ (names, mode))

abort :: MonadIO m => TransactionAwait s pending -> m ()
abort (Typed raw _) = void $ raw # "abort" $ ()

commit :: MonadIO m => TransactionAwait s pending -> m ()
commit (Typed raw _) = void $ raw # "commit" $ ()

type TransactionCallbacks = '["oncomplete", "onerror", "onabort"]

data TransactionCore s = TransactionCore Promise JSVal JSVal

type TransactionAwait (s :: Type) (pending :: [Symbol]) =
  Typed pending (TransactionCore s)

transactionAwait :: PromiseM m =>
  Transaction ->
  m (TransactionAwait s TransactionCallbacks)
transactionAwait (Transaction raw) = do
  resMv <- newEmptyMVar
  rejMv <- newEmptyMVar
  p <- newPromise $ \res rej -> putMVar resMv res >> putMVar rejMv rej
  res <- readMVar resMv
  rej <- readMVar rejMv
  pure $ Typed raw (TransactionCore p res rej)

withTransaction :: PromiseM m =>
  Database ->
  [MisoString] ->
  TxMode ->
  (forall s. TransactionAwait s TransactionCallbacks ->
    m (TransactionAwait s '[])) ->
  m ()
withTransaction db names mode f = do
  tx <- transaction db names mode
  initial <- transactionAwait tx
  done <- f initial
  awaitTransaction done

resolveTx :: PromiseM m => TransactionCore s -> m ()
resolveTx (TransactionCore _ res _) = void $ apply (Function res) ()

rejectTx :: PromiseM m => TransactionCore s -> JSVal -> m ()
rejectTx (TransactionCore _ _ rej) v = void $ apply (Function rej) v

onTransactionComplete :: forall s pending rest m.
  ( PromiseM m
  , Elem "oncomplete" pending ~ 'True
  , Drop "oncomplete" pending ~ rest
  ) =>
  m () ->
  TransactionAwait s pending ->
  m (TransactionAwait s rest)
onTransactionComplete cb req@(Typed _ core) =
  setCallback @"oncomplete" req $ \_ -> do
    cb
    resolveTx core

onTransactionError :: forall s pending rest m.
  ( PromiseM m
  , Elem "onerror" pending ~ 'True
  , Drop "onerror" pending ~ rest
  ) =>
  (DbError -> m ()) ->
  TransactionAwait s pending ->
  m (TransactionAwait s rest)
onTransactionError cb req@(Typed _ core) =
  setCallback @"onerror" req $ \ev -> do
    err <- DbError <$> eventError ev
    cb err
    rejectTx core (unDbError err)

onTransactionAbort :: forall s pending rest m.
  ( PromiseM m
  , Elem "onabort" pending ~ 'True
  , Drop "onabort" pending ~ rest
  ) =>
  (DbError -> m ()) ->
  TransactionAwait s pending ->
  m (TransactionAwait s rest)
onTransactionAbort cb req@(Typed _ core) =
  setCallback @"onabort" req $ \ev -> do
    err <- DbError <$> eventError ev
    cb err
    rejectTx core (unDbError err)

awaitTransaction :: PromiseM m => TransactionAwait s '[] -> m ()
awaitTransaction (Typed _ (TransactionCore p _ _)) = void $ await p
