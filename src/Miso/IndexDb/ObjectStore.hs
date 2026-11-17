{-# LANGUAGE AllowAmbiguousTypes #-}
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
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude
import Control.Concurrent ( MVar)
import Data.Proxy
import UnliftIO (throwIO, putMVar, Exception(..), newEmptyMVar, readMVar)
import Miso.JSON (jsonStringify)
import System.IO.Unsafe (unsafePerformIO)

newtype DbReq (pending :: [Symbol]) r = DbReq { unDbReq :: JSVal }
type NewDbReq r = DbReq RequestCallbacks r

newDbReq :: JSVal -> NewDbReq r
newDbReq = DbReq

setCb :: forall name pending rest m r.
  ( PromiseM m
  , KnownSymbol name
  , Elem name pending ~ 'True
  , Drop name pending ~ rest
  )
  => DbReq pending r -> (JSVal -> m ()) -> m (DbReq rest r)
setCb (DbReq r) f = do
  setField r (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
  pure $ DbReq r

newtype IndexDbException = IndexDbException JSVal
instance Show IndexDbException where
  show (IndexDbException j) =
    "IndexDbException: " <>  show (unsafePerformIO (jsonStringify j))

instance Exception IndexDbException

resolveReq :: (FromJSVal r, PromiseM m) => NewDbReq r -> m r
resolveReq r = do
  x  :: MVar (Either JSVal JSVal) <- newEmptyMVar
  r' <- setCb @OnSuccess r $ \_ -> putMVar x . Right =<< r.unDbReq ! "result"
  _r'' <- setCb @OnError r' $ \_ -> putMVar x . Left =<< r.unDbReq ! "error"
  readMVar x >>= \case
    Left e -> throwIO $ IndexDbException e
    Right j -> fromJSValUnchecked j


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
    => ObjectStore -> v -> Maybe k -> m (NewDbReq JSVal)
put (ObjectStore os) v mk = newDbReq <$> (os # "put" $ (v, mk))

add :: (PromiseM m, ToJSVal k, ToJSVal v)
    => ObjectStore -> v -> Maybe k -> m (NewDbReq JSVal)
add (ObjectStore os) v mk =newDbReq <$> (os # "add" $ (v, mk))

get :: forall a k m. (PromiseM m, ToArgs k, FromJSVal a)
   => ObjectStore -> k -> m a
get (ObjectStore os) k =
  resolveReq . newDbReq =<< (os # "get" $ k)

getAll :: forall q m. (PromiseM m, ToJSVal q)
       => ObjectStore -> Maybe q -> Maybe Int -> m (NewDbReq [JSVal])
getAll (ObjectStore os) q cnt = newDbReq <$> (os # "getAll" $ (q, cnt))

getAllKeys :: (PromiseM m, ToArgs q) =>
  ObjectStore ->
  Maybe q ->
  m (NewDbReq [JSVal])
getAllKeys (ObjectStore os) q = newDbReq <$> (os # "getAllKeys" $ q)

delete :: (PromiseM m, ToArgs k) => ObjectStore -> k -> m (NewDbReq ())
delete (ObjectStore os) k = DbReq <$> (os # "delete" $ k)

clear :: PromiseM m => ObjectStore -> m (NewDbReq ())
clear (ObjectStore os) = DbReq <$> (os # "clear" $ ())

count :: (PromiseM m, ToArgs q) =>
  ObjectStore ->
  Maybe q -> m Int
count (ObjectStore os) q =
  resolveReq . newDbReq =<< (os # "count" $ q)
