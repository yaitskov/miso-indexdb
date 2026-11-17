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
  , Miso.IndexDb.objectStore
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
  , getKey
  , getAllKeys
  , Miso.IndexDb.delete
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
import Miso.IndexDb.Tx
import Miso.Promise hiding (get, put, errorMessage)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Prelude

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
    resolveReq core v

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

data StoreOptions = StoreOptions
  { storeKeyPath :: Maybe MisoString
  , storeAutoIncrement :: Bool
  }

instance ToJSVal StoreOptions where
  toJSVal (StoreOptions kp ai) = do
    o <- create
    setProp "keyPath" kp o
    setProp "autoIncrement" ai o
    toJSVal o

factory :: PromiseM m => m Factory
factory = Factory <$> jsg "indexedDB"

open :: PromiseM m => Factory -> MisoString -> Maybe Int -> m (Request OpenCallbacks Database)
open (Factory f) name ver = do
  raw <- f # "open" $ (name, ver)
  mkRequest raw

deleteDatabase :: PromiseM m => Factory -> MisoString -> m (Request DeleteCallbacks ())
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

createObjectStore :: MonadIO m => Database -> MisoString -> StoreOptions -> m ObjectStore
createObjectStore (Database db) name opts =
  ObjectStore <$> (db # "createObjectStore" $ (name, opts))

deleteObjectStore :: MonadIO m => Database -> MisoString -> m ()
deleteObjectStore (Database db) name = void $ db # "deleteObjectStore" $ name

close :: MonadIO m => Database -> m ()
close (Database db) = void $ db # "close" $ ()

dbName :: MonadIO m => Database -> m MisoString
dbName (Database db) = fromJSValUnchecked =<< (db ! "name")

dbVersion :: MonadIO m => Database -> m Int
dbVersion (Database db) = fromJSValUnchecked =<< (db ! "version")

objectStore :: MonadIO m =>
  Tx s '[] r t ->
  MisoString ->
  m ObjectStore
objectStore tx name = ObjectStore <$> (tx.tx # "objectStore" $ name)

put :: (PromiseM m, ToJSVal k, ToJSVal v)
    => ObjectStore -> v -> Maybe k -> m (Request RequestCallbacks JSVal)
put (ObjectStore os) v mk = do
  raw <- os # "put" $ (v, mk)
  mkRequest raw

add :: (PromiseM m, ToJSVal k, ToJSVal v)
    => ObjectStore -> v -> Maybe k -> m (Request RequestCallbacks JSVal)
add (ObjectStore os) v mk = do
  raw <- os # "add" $ (v, mk)
  mkRequest raw

get :: forall a k m. (PromiseM m, ToArgs k, FromJSVal a)
   => ObjectStore -> k -> m (Request RequestCallbacks a)
get (ObjectStore os) k = do
  raw <- os # "get" $ k
  mkRequest raw

getAll :: forall a q m. (PromiseM m, ToJSVal q, FromJSVal a)
       => ObjectStore -> Maybe q -> Maybe Int -> m (Request RequestCallbacks [a])
getAll (ObjectStore os) q cnt = do
  raw <- os # "getAll" $ (q, cnt)
  mkRequest raw

getKey :: (PromiseM m, ToArgs k) => ObjectStore -> k -> m (Request RequestCallbacks JSVal)
getKey (ObjectStore os) k = do
  raw <- os # "getKey" $ k
  mkRequest raw

getAllKeys :: (PromiseM m, ToArgs q) =>
  ObjectStore -> Maybe q -> m (Request RequestCallbacks [JSVal])
getAllKeys (ObjectStore os) q = do
  raw <- os # "getAllKeys" $ q
  mkRequest raw

delete :: (PromiseM m, ToArgs k) => ObjectStore -> k -> m (Request RequestCallbacks ())
delete (ObjectStore os) k = do
  raw <- os # "delete" $ k
  mkRequest raw

clear :: PromiseM m => ObjectStore -> m (Request RequestCallbacks ())
clear (ObjectStore os) = do
  raw <- os # "clear" $ ()
  mkRequest raw

count :: (PromiseM m, ToArgs q) => ObjectStore -> Maybe q -> m (Request RequestCallbacks Int)
count (ObjectStore os) q = do
  raw <- os # "count" $ q
  mkRequest raw

errorName :: MonadIO m => DbError -> m MisoString
errorName (DbError e) = fromJSValUnchecked =<< (e ! "name")

errorMessage :: MonadIO m => DbError -> m MisoString
errorMessage (DbError e) = fromJSValUnchecked =<< (e ! "message")
