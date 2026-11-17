{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.IndexDb
  ( -- * Typestate
    Typed
  , Request
  , RequestCallbacks
  , OpenCallbacks
  , DeleteCallbacks
  , result
  , onSuccess
  , onSuccessWith
  , onError
  , onErrorWith
  , onUpgradeNeeded
  , onBlocked
    -- * Typed objects
  , Factory(..)
  , Database(..)
  , Tx
  , ObjectStore(..)
  , Index(..)
  , Cursor(..)
  , KeyRange(..)
  , DbError(..)
  , StoreOptions(..)
  , TxMode(..)
  , Direction(..)
    -- * Factory
  , factory
  , open
  , deleteDatabase
  , databases
  , cmp
    -- * Database
  , objectStoreNames
  , hasObjectStore
  , createObjectStore
  , deleteObjectStore
  , close
  , dbName
  , dbVersion
    -- * Transaction
  , withTx
  , objectStore
  , abort
  , commit
    -- * Transaction completion
  , TransactionCallbacks

  , onTxComplete
  , onTxError
  , onTxAbort
    -- * Object store
  , put
  , add
  , get
  , getAll

  , getAllKeys
  , delete
  , clear
  , count
  , createIndex
  , deleteIndex
  , index
  , openCursor
  , openKeyCursor
    -- * Index
  , indexGet
  , indexGetKey
  , indexGetAll
  , indexGetAllKeys
  , indexCount
  , indexOpenCursor
  , indexOpenKeyCursor
    -- * Cursor
  , cursorKey
  , cursorPrimaryKey
  , cursorValue
  , advance
  , continueCursor
  , continuePrimaryKey
  , deleteCursor
  , updateCursor
    -- * Key ranges
  , keyOnly
  , lowerBound
  , upperBound
  , boundRange
    -- * Errors
  , errorName
  , errorMessage
  ) where

import Miso.IndexDb.Cursor
import Miso.IndexDb.Index
import Miso.IndexDb.Internal
import Miso.IndexDb.KeyRange
import Miso.IndexDb.ObjectStore
import Miso.IndexDb.Tx
import Miso.Promise hiding (get, put, errorMessage)
import Control.Monad (void)
import Miso.IndexDb.Prelude

result :: (PromiseM m, FromJSVal a) => Request '[] a -> m a
result (Typed _ (RequestCore p _ _)) = fromJSValUnchecked =<< await p

onSuccess :: forall pending rest a m.
  ( PromiseM m
  , Elem "onsuccess" pending ~ 'True
  , Drop "onsuccess" pending ~ rest
  )
  => Request pending a -> m (Request rest a)
onSuccess = onSuccessWith (\_ -> pure ())

onSuccessWith :: forall pending rest a m.
  ( PromiseM m
  , Elem "onsuccess" pending ~ 'True
  , Drop "onsuccess" pending ~ rest
  )
  => (JSVal -> m ()) -> Request pending a -> m (Request rest a)
onSuccessWith cb req@(Typed _ core) =
  setCallback @"onsuccess" req $ \ev -> do
    v <- eventResult ev
    cb v
    resolveReq' core v

onError :: forall pending rest a m.
  ( PromiseM m
  , Elem "onerror" pending ~ 'True
  , Drop "onerror" pending ~ rest
  )
  => Request pending a -> m (Request rest a)
onError = onErrorWith (\_ -> pure ())

onErrorWith :: forall pending rest a m.
  ( PromiseM m
  , Elem "onerror" pending ~ 'True
  , Drop "onerror" pending ~ rest
  )
  => (DbError -> m ()) -> Request pending a -> m (Request rest a)
onErrorWith cb req@(Typed _ core) =
  setCallback @"onerror" req $ \ev -> do
    err <- DbError <$> eventError ev
    cb err
    rejectReq core (unDbError err)

onUpgradeNeeded :: forall pending rest a m.
  ( PromiseM m
  , Elem "onupgradeneeded" pending ~ 'True
  , Drop "onupgradeneeded" pending ~ rest
  )
  => (Database -> m ()) -> Request pending a -> m (Request rest a)
onUpgradeNeeded cb req@(Typed _ _) =
  setCallback @"onupgradeneeded" req $ \ev -> do
    v <- eventResult ev
    cb (Database v)

onBlocked :: forall pending rest a m.
  ( PromiseM m
  , Elem "onblocked" pending ~ 'True
  , Drop "onblocked" pending ~ rest
  )
  => ((Int, Int) -> m ()) -> Request pending a -> m (Request rest a)
onBlocked cb req@(Typed _ _) =
  setCallback @"onblocked" req $ \ev -> do
    oldV <- fromJSValUnchecked =<< (ev ! "oldVersion")
    newV <- fromJSValUnchecked =<< (ev ! "newVersion")
    cb (oldV, newV)

newtype Factory = Factory JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

factory :: PromiseM m => m Factory
factory = Factory <$> jsg "indexedDB"

open ::
  PromiseM m =>
  Factory ->
  MisoString ->
  Maybe Int ->
  m (Request OpenCallbacks Database)
open (Factory f) name ver = do
  raw <- f # "open" $ (name, ver)
  mkRequest raw

deleteDatabase ::
  PromiseM m =>
  Factory ->
  MisoString ->
  m (Request DeleteCallbacks ())
deleteDatabase (Factory f) name = do
  raw <- f # "deleteDatabase" $ name
  mkRequest raw

databases :: (PromiseM m, FromJSVal a) => Factory -> m a
databases (Factory f) = do
  p <- f # "databases" $ ()
  fromJSValUnchecked =<< await (Promise p)

cmp :: (MonadIO m, ToJSVal a) => Factory -> a -> a -> m Int
cmp (Factory f) a b = fromJSValUnchecked =<< (f # "cmp" $ (a, b))

objectStoreNames :: PromiseM m => Database -> m [MisoString]
objectStoreNames (Database db) = fromJSValUnchecked =<< (db ! "objectStoreNames")

hasObjectStore :: PromiseM m => Database -> MisoString -> m Bool
hasObjectStore (Database db) name = do
  names <- db ! "objectStoreNames"
  fromJSValUnchecked =<< (names # "contains" $ name)

close :: MonadIO m => Database -> m ()
close (Database db) = void $ db # "close" $ ()

dbName :: MonadIO m => Database -> m MisoString
dbName (Database db) = fromJSValUnchecked =<< (db ! "name")

dbVersion :: MonadIO m => Database -> m Int
dbVersion (Database db) = fromJSValUnchecked =<< (db ! "version")

errorName :: MonadIO m => DbError -> m MisoString
errorName (DbError e) = fromJSValUnchecked =<< (e ! "name")

errorMessage :: MonadIO m => DbError -> m MisoString
errorMessage (DbError e) = fromJSValUnchecked =<< (e ! "message")
