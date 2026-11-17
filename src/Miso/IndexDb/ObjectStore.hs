{-# LANGUAGE AllowAmbiguousTypes #-}
module Miso.IndexDb.ObjectStore
  ( ObjectStore(..)
  , StoreOptions(..)
  , MetaObjectStore
  , DataObjectStore
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
import Data.Kind (Type)

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

newtype ObjectStore (ts :: Maybe (s :: Type)) = ObjectStore JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

type MetaObjectStore = ObjectStore (Nothing :: Maybe ())
type DataObjectStore x = ObjectStore (Just x)

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
  m MetaObjectStore
createObjectStore (Database db) name opts =
  ObjectStore <$> (db # "createObjectStore" $ (name, opts))

deleteObjectStore :: MonadIO m => Database -> MisoString -> m ()
deleteObjectStore (Database db) name = void $ db # "deleteObjectStore" $ name

objectStore :: MonadIO m =>
  Tx s '[] r t ->
  MisoString ->
  m (DataObjectStore s)
objectStore tx name = ObjectStore <$> (tx.tx # "objectStore" $ name)

put :: (PromiseM m, ToJSVal k, ToJSVal v) =>
  DataObjectStore s -> v -> Maybe k -> m (NewDbReq JSVal)
put (ObjectStore os) v mk = newDbReq <$> (os # "put" $ (v, mk))

add :: (PromiseM m, ToJSVal k, ToJSVal v) =>
  DataObjectStore s -> v -> Maybe k -> m (NewDbReq JSVal)
add (ObjectStore os) v mk =newDbReq <$> (os # "add" $ (v, mk))

get :: forall a k m s. (PromiseM m, ToArgs k, FromJSVal a)
   => DataObjectStore s -> k -> m a
get (ObjectStore os) k =
  resolveReq . newDbReq =<< (os # "get" $ k)

getAll :: forall q m s. (PromiseM m, ToJSVal q) =>
  DataObjectStore s -> Maybe q -> Maybe Int -> m (NewDbReq [JSVal])
getAll (ObjectStore os) q cnt = newDbReq <$> (os # "getAll" $ (q, cnt))

getAllKeys :: (PromiseM m, ToArgs q) =>
  DataObjectStore s ->
  Maybe q ->
  m (NewDbReq [JSVal])
getAllKeys (ObjectStore os) q = newDbReq <$> (os # "getAllKeys" $ q)

delete :: (PromiseM m, ToArgs k) => DataObjectStore s -> k -> m (NewDbReq ())
delete (ObjectStore os) k = DbReq <$> (os # "delete" $ k)

clear :: PromiseM m => DataObjectStore s -> m (NewDbReq ())
clear (ObjectStore os) = DbReq <$> (os # "clear" $ ())

count :: (PromiseM m, ToArgs q) =>
  DataObjectStore s ->
  Maybe q -> m Int
count (ObjectStore os) q =
  resolveReq . newDbReq =<< (os # "count" $ q)
