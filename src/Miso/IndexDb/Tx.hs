module Miso.IndexDb.Tx
  ( Transaction(..)
  , TxMode(..)
  , abort
  , commit
  , TransactionCallbacks
  , TransactionAwait
  , transactionAwait
  , onTransactionComplete
  , onTransactionError
  , onTransactionAbort
  , awaitTransaction
  ) where

import Miso.IndexDb.Internal
import Miso.Promise
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
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

abort :: MonadIO m => Transaction -> m ()
abort (Transaction tx) = void $ tx # "abort" $ ()

commit :: MonadIO m => Transaction -> m ()
commit (Transaction tx) = void $ tx # "commit" $ ()

type TransactionCallbacks = '["oncomplete", "onerror", "onabort"]

data TransactionCore = TransactionCore Promise JSVal JSVal

type TransactionAwait (pending :: [Symbol]) = Typed pending TransactionCore

transactionAwait :: PromiseM m => Transaction -> m (TransactionAwait TransactionCallbacks)
transactionAwait (Transaction raw) = do
  resMv <- newEmptyMVar
  rejMv <- newEmptyMVar
  p <- newPromise $ \res rej -> putMVar resMv res >> putMVar rejMv rej
  res <- readMVar resMv
  rej <- readMVar rejMv
  pure $ Typed raw (TransactionCore p res rej)

resolveTx :: PromiseM m => TransactionCore -> m ()
resolveTx (TransactionCore _ res _) = void $ apply (Function res) ()

rejectTx :: PromiseM m => TransactionCore -> JSVal -> m ()
rejectTx (TransactionCore _ _ rej) v = void $ apply (Function rej) v

onTransactionComplete :: forall pending rest m.
  ( PromiseM m
  , Elem "oncomplete" pending ~ 'True
  , Drop "oncomplete" pending ~ rest
  )
  => m () -> TransactionAwait pending -> m (TransactionAwait rest)
onTransactionComplete cb req@(Typed _ core) =
  setCallback @"oncomplete" req $ \_ -> do
    cb
    resolveTx core

onTransactionError :: forall pending rest m.
  ( PromiseM m
  , Elem "onerror" pending ~ 'True
  , Drop "onerror" pending ~ rest
  )
  => (DbError -> m ()) -> TransactionAwait pending -> m (TransactionAwait rest)
onTransactionError cb req@(Typed _ core) =
  setCallback @"onerror" req $ \ev -> do
    err <- DbError <$> eventError ev
    cb err
    rejectTx core (unDbError err)

onTransactionAbort :: forall pending rest m.
  ( PromiseM m
  , Elem "onabort" pending ~ 'True
  , Drop "onabort" pending ~ rest
  )
  => (DbError -> m ()) -> TransactionAwait pending -> m (TransactionAwait rest)
onTransactionAbort cb req@(Typed _ core) =
  setCallback @"onabort" req $ \ev -> do
    err <- DbError <$> eventError ev
    cb err
    rejectTx core (unDbError err)

awaitTransaction :: PromiseM m => TransactionAwait '[] -> m ()
awaitTransaction (Typed _ (TransactionCore p _ _)) = void $ await p
