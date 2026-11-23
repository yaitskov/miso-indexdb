{-# LANGUAGE AllowAmbiguousTypes #-}
module Miso.IndexDb.ObjectStore where


import Miso.Cb hiding (get, put, errorMessage)
import Miso.IndexDb.Prelude
-- import Miso.IndexDb.Tx
-- import Miso.JSON (jsonStringify)
-- import System.IO.Unsafe (unsafePerformIO)

newtype DbReq (pending :: [Symbol]) r = DbReq { unDbReq :: JSVal }
-- type NewDbReq r = DbReq RequestCallbacks r

-- newDbReq :: JSVal -> NewDbReq r
-- newDbReq = DbReq

-- setCb :: forall name pending rest m r.
--   ( CbM m
--   , KnownSymbol name
--   , Elem name pending ~ 'True
--   , Drop name pending ~ rest
--   )
--   => DbReq pending r -> (JSVal -> m ()) -> m (DbReq rest r)
-- setCb (DbReq r) f = do
--   setField r (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
--   pure $ DbReq r

-- newtype IndexDbException = IndexDbException JSVal
-- instance Show IndexDbException where
--   show (IndexDbException j) =
--     "IndexDbException: " <>  show (unsafePerformIO (jsonStringify j))

-- instance Exception IndexDbException

-- resolveReq :: (FromJSVal r, CbM m) => NewDbReq r -> m r
-- resolveReq r = do
--   x  :: MVar (Either JSVal JSVal) <- newEmptyMVar
--   r' <- setCb @OnSuccess r $ \_ -> putMVar x . Right =<< r.unDbReq ! "result"
--   _r'' <- setCb @OnError r' $ \_ -> putMVar x . Left =<< r.unDbReq ! "error"
--   readMVar x >>= \case
--     Left e -> throwIO $ IndexDbException e
--     Right j -> fromJSValUnchecked j

-- newtype ObjectStore (ts :: Maybe (s :: Type)) = ObjectStore JSVal
--   deriving newtype (FromJSVal, ToObject, ToJSVal)

-- type MetaObjectStore = ObjectStore (Nothing :: Maybe ())
-- type DataObjectStore x = ObjectStore (Just x)

-- data StoreOptions = StoreOptions
--   { storeKeyPath :: Maybe MisoString
--   , storeAutoIncrement :: Bool
--   }

-- instance ToJSVal StoreOptions where
--   toJSVal (StoreOptions kp ai) = do
--     o <- create
--     setProp "keyPath" kp o
--     setProp "autoIncrement" ai o
--     toJSVal o

-- createObjectStore :: MonadIO m =>
--   Database ->
--   MisoString ->
--   StoreOptions ->
--   m MetaObjectStore
-- createObjectStore (Database db) name opts =
--   ObjectStore <$> (db # "createObjectStore" $ (name, opts))

-- deleteObjectStore :: MonadIO m => Database -> MisoString -> m ()
-- deleteObjectStore (Database db) name = void $ db # "deleteObjectStore" $ name

-- objectStore :: MonadIO m =>
--   Tx s '[] r t ->
--   MisoString ->
--   m (DataObjectStore s)
-- objectStore tx name = ObjectStore <$> (tx.tx # "objectStore" $ name)

-- put :: (CbM m, ToJSVal k, ToJSVal v) =>
--   DataObjectStore s -> v -> Maybe k -> m (NewDbReq JSVal)
-- put (ObjectStore os) v mk = newDbReq <$> (os # "put" $ (v, mk))

-- add :: (CbM m, ToJSVal k, ToJSVal v) =>
--   DataObjectStore s -> v -> Maybe k -> m (NewDbReq JSVal)
-- add (ObjectStore os) v mk =newDbReq <$> (os # "add" $ (v, mk))

-- get :: forall a k m s. (CbM m, ToArgs k, FromJSVal a)
--    => DataObjectStore s -> k -> m a
-- get (ObjectStore os) k =
--   resolveReq . newDbReq =<< (os # "get" $ k)

-- getAll :: forall q m s. (CbM m, ToJSVal q) =>
--   DataObjectStore s -> Maybe q -> Maybe Int -> m (NewDbReq [JSVal])
-- getAll (ObjectStore os) q cnt = newDbReq <$> (os # "getAll" $ (q, cnt))

-- getAllKeys :: (CbM m, ToArgs q) =>
--   DataObjectStore s ->
--   Maybe q ->
--   m (NewDbReq [JSVal])
-- getAllKeys (ObjectStore os) q = newDbReq <$> (os # "getAllKeys" $ q)

-- delete :: (CbM m, ToArgs k) => DataObjectStore s -> k -> m (NewDbReq ())
-- delete (ObjectStore os) k = DbReq <$> (os # "delete" $ k)

-- clear :: CbM m => DataObjectStore s -> m (NewDbReq ())
-- clear (ObjectStore os) = DbReq <$> (os # "clear" $ ())

-- count :: (CbM m, ToArgs q) =>
--   DataObjectStore s ->
--   Maybe q -> m Int
-- count (ObjectStore os) q =
--   resolveReq . newDbReq =<< (os # "count" $ q)
