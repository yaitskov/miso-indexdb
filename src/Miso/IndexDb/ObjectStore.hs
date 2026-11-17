module Miso.IndexDb.ObjectStore
  ( ObjectStore(..)
  , StoreOptions(..)
  , createObjectStore
  , deleteObjectStore
  , objectStore
  , put
  , add
  , get
  , getAll
  , getKey
  , getAllKeys
  , delete
  , clear
  , count
  ) where

import Miso.IndexDb.Internal
import Miso.IndexDb.Tx
import Miso.Promise hiding (get, put, errorMessage)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Prelude

newtype ObjectStore = ObjectStore JSVal
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

createObjectStore :: MonadIO m =>
  Database ->
  MisoString ->
  StoreOptions ->
  m ObjectStore
createObjectStore (Database db) name opts =
  ObjectStore <$> (db # "createObjectStore" $ (name, opts))

deleteObjectStore :: MonadIO m => Database -> MisoString -> m ()
deleteObjectStore (Database db) name = void $ db # "deleteObjectStore" $ name

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

getKey :: (PromiseM m, ToArgs k) =>
  ObjectStore ->
  k ->
  m (Request RequestCallbacks JSVal)
getKey (ObjectStore os) k = do
  raw <- os # "getKey" $ k
  mkRequest raw

getAllKeys :: (PromiseM m, ToArgs q) =>
  ObjectStore ->
  Maybe q ->
  m (Request RequestCallbacks [JSVal])
getAllKeys (ObjectStore os) q = do
  raw <- os # "getAllKeys" $ q
  mkRequest raw

delete :: (PromiseM m, ToArgs k) =>
  ObjectStore ->
  k ->
  m (Request RequestCallbacks ())
delete (ObjectStore os) k = do
  raw <- os # "delete" $ k
  mkRequest raw

clear :: PromiseM m => ObjectStore -> m (Request RequestCallbacks ())
clear (ObjectStore os) = do
  raw <- os # "clear" $ ()
  mkRequest raw

count :: (PromiseM m, ToArgs q) =>
  ObjectStore ->
  Maybe q ->
  m (Request RequestCallbacks Int)
count (ObjectStore os) q = do
  raw <- os # "count" $ q
  mkRequest raw
