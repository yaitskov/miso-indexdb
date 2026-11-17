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
  , Transaction(..)
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
  , createObjectStore
  , deleteObjectStore
  , transaction
  , close
  , dbName
  , dbVersion
    -- * Transaction
  , Miso.IndexDb.objectStore
  , abort
  , commit
    -- * Transaction completion
  , TransactionCallbacks
  , TransactionAwait
  , transactionAwait
  , onTransactionComplete
  , onTransactionError
  , onTransactionAbort
  , awaitTransaction
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

import Miso.IndexDb.Internal
import Miso.IndexDb.Tx
import Miso.Promise hiding (get, put, errorMessage)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (Symbol)
import UnliftIO (newEmptyMVar, putMVar, readMVar)
import Prelude

data RequestCore a = RequestCore Promise JSVal JSVal

type Request (pending :: [Symbol]) a = Typed pending (RequestCore a)

type RequestCallbacks = '["onsuccess", "onerror"]

type OpenCallbacks = '["onupgradeneeded", "onblocked", "onsuccess", "onerror"]

type DeleteCallbacks = '["onblocked", "onsuccess", "onerror"]

mkRequest :: PromiseM m => JSVal -> m (Request pending a)
mkRequest raw = do
  resMv <- newEmptyMVar
  rejMv <- newEmptyMVar
  p <- newPromise $ \res rej -> putMVar resMv res >> putMVar rejMv rej
  res <- readMVar resMv
  rej <- readMVar rejMv
  pure $ Typed raw (RequestCore p res rej)

resolveReq :: PromiseM m => RequestCore a -> JSVal -> m ()
resolveReq (RequestCore _ _ res) v = void $ apply (Function res) v

rejectReq :: PromiseM m => RequestCore a -> JSVal -> m ()
rejectReq (RequestCore _ _ rej) v = void $ apply (Function rej) v

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

newtype Database = Database JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype ObjectStore = ObjectStore JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype Index = Index JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype Cursor = Cursor JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype KeyRange = KeyRange JSVal
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

data Direction = Next | Previous | NextUnique | PreviousUnique

instance ToJSVal Direction where
  toJSVal Next = toJSVal ("next" :: MisoString)
  toJSVal Previous = toJSVal ("prev" :: MisoString)
  toJSVal NextUnique = toJSVal ("nextunique" :: MisoString)
  toJSVal PreviousUnique = toJSVal ("prevunique" :: MisoString)

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

createObjectStore :: MonadIO m => Database -> MisoString -> StoreOptions -> m ObjectStore
createObjectStore (Database db) name opts =
  ObjectStore <$> (db # "createObjectStore" $ (name, opts))

deleteObjectStore :: MonadIO m => Database -> MisoString -> m ()
deleteObjectStore (Database db) name = void $ db # "deleteObjectStore" $ name

transaction :: MonadIO m => Database -> [MisoString] -> TxMode -> m Transaction
transaction (Database db) names mode =
  Transaction <$> (db # "transaction" $ (names, mode))

close :: MonadIO m => Database -> m ()
close (Database db) = void $ db # "close" $ ()

dbName :: MonadIO m => Database -> m MisoString
dbName (Database db) = fromJSValUnchecked =<< (db ! "name")

dbVersion :: MonadIO m => Database -> m Int
dbVersion (Database db) = fromJSValUnchecked =<< (db ! "version")

objectStore :: MonadIO m => Transaction -> MisoString -> m ObjectStore
objectStore (Transaction tx) name = ObjectStore <$> (tx # "objectStore" $ name)

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

getAllKeys :: (PromiseM m, ToArgs q) => ObjectStore -> Maybe q -> m (Request RequestCallbacks [JSVal])
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

createIndex :: MonadIO m => ObjectStore -> MisoString -> MisoString -> m Index
createIndex (ObjectStore os) name keyPath =
  Index <$> (os # "createIndex" $ (name, keyPath))

deleteIndex :: MonadIO m => ObjectStore -> MisoString -> m ()
deleteIndex (ObjectStore os) name = void $ os # "deleteIndex" $ name

index :: MonadIO m => ObjectStore -> MisoString -> m Index
index (ObjectStore os) name = Index <$> (os # "index" $ name)

openCursor :: (PromiseM m, ToJSVal q)
           => ObjectStore -> Maybe q -> Maybe Direction -> m (Request RequestCallbacks (Maybe Cursor))
openCursor (ObjectStore os) q dir = do
  raw <- os # "openCursor" $ (q, dir)
  mkRequest raw

openKeyCursor :: (PromiseM m, ToJSVal q)
              => ObjectStore -> Maybe q -> Maybe Direction -> m (Request RequestCallbacks (Maybe Cursor))
openKeyCursor (ObjectStore os) q dir = do
  raw <- os # "openKeyCursor" $ (q, dir)
  mkRequest raw

indexGet :: forall a k m. (PromiseM m, ToArgs k, FromJSVal a)
         => Index -> k -> m (Request RequestCallbacks a)
indexGet (Index ix) k = do
  raw <- ix # "get" $ k
  mkRequest raw

indexGetKey :: (PromiseM m, ToArgs k) => Index -> k -> m (Request RequestCallbacks JSVal)
indexGetKey (Index ix) k = do
  raw <- ix # "getKey" $ k
  mkRequest raw

indexGetAll :: forall a q m. (PromiseM m, ToJSVal q, FromJSVal a)
            => Index -> Maybe q -> Maybe Int -> m (Request RequestCallbacks [a])
indexGetAll (Index ix) q cnt = do
  raw <- ix # "getAll" $ (q, cnt)
  mkRequest raw

indexGetAllKeys :: (PromiseM m, ToArgs q) => Index -> Maybe q -> m (Request RequestCallbacks [JSVal])
indexGetAllKeys (Index ix) q = do
  raw <- ix # "getAllKeys" $ q
  mkRequest raw

indexCount :: (PromiseM m, ToArgs q) => Index -> Maybe q -> m (Request RequestCallbacks Int)
indexCount (Index ix) q = do
  raw <- ix # "count" $ q
  mkRequest raw

indexOpenCursor :: (PromiseM m, ToJSVal q)
                => Index -> Maybe q -> Maybe Direction -> m (Request RequestCallbacks (Maybe Cursor))
indexOpenCursor (Index ix) q dir = do
  raw <- ix # "openCursor" $ (q, dir)
  mkRequest raw

indexOpenKeyCursor :: (PromiseM m, ToJSVal q)
                   => Index -> Maybe q -> Maybe Direction -> m (Request RequestCallbacks (Maybe Cursor))
indexOpenKeyCursor (Index ix) q dir = do
  raw <- ix # "openKeyCursor" $ (q, dir)
  mkRequest raw

cursorKey :: (MonadIO m, FromJSVal a) => Cursor -> m a
cursorKey (Cursor c) = fromJSValUnchecked =<< (c ! "key")

cursorPrimaryKey :: (MonadIO m, FromJSVal a) => Cursor -> m a
cursorPrimaryKey (Cursor c) = fromJSValUnchecked =<< (c ! "primaryKey")

cursorValue :: (MonadIO m, FromJSVal a) => Cursor -> m a
cursorValue (Cursor c) = fromJSValUnchecked =<< (c ! "value")

advance :: MonadIO m => Cursor -> Int -> m ()
advance (Cursor c) n = void $ c # "advance" $ n

continueCursor :: (MonadIO m, ToArgs k) => Cursor -> Maybe k -> m ()
continueCursor (Cursor c) mk = void $ c # "continue" $ mk

continuePrimaryKey :: (MonadIO m, ToJSVal k, ToJSVal pk)
                   => Cursor -> k -> pk -> m ()
continuePrimaryKey (Cursor c) k pk = void $ c # "continuePrimaryKey" $ (k, pk)

deleteCursor :: PromiseM m => Cursor -> m (Request RequestCallbacks ())
deleteCursor (Cursor c) = do
  raw <- c # "delete" $ ()
  mkRequest raw

updateCursor :: (PromiseM m, ToArgs v) => Cursor -> v -> m (Request RequestCallbacks JSVal)
updateCursor (Cursor c) v = do
  raw <- c # "update" $ v
  mkRequest raw

keyRangeClass :: MonadIO m => m JSVal
keyRangeClass = jsg "IDBKeyRange"

keyOnly :: (MonadIO m, ToArgs a) => a -> m KeyRange
keyOnly v = do
  c <- keyRangeClass
  KeyRange <$> (c # "only" $ v)

lowerBound :: (MonadIO m, ToJSVal a) => a -> Maybe Bool -> m KeyRange
lowerBound v exclusive = do
  c <- keyRangeClass
  KeyRange <$> (c # "lowerBound" $ (v, exclusive))

upperBound :: (MonadIO m, ToJSVal a) => a -> Maybe Bool -> m KeyRange
upperBound v exclusive = do
  c <- keyRangeClass
  KeyRange <$> (c # "upperBound" $ (v, exclusive))

boundRange :: (MonadIO m, ToJSVal lo, ToJSVal hi)
           => lo -> hi -> Maybe Bool -> Maybe Bool -> m KeyRange
boundRange lo hi loOpen hiOpen = do
  c <- keyRangeClass
  KeyRange <$> (c # "bound" $ (lo, hi, loOpen, hiOpen))

errorName :: MonadIO m => DbError -> m MisoString
errorName (DbError e) = fromJSValUnchecked =<< (e ! "name")

errorMessage :: MonadIO m => DbError -> m MisoString
errorMessage (DbError e) = fromJSValUnchecked =<< (e ! "message")
