module Miso.IndexDb.Index
  ( Index(..)
  , MetaIndex
  , DataIndex
  , createIndex
  , deleteIndex
  , index
  , indexGet
  , indexGetKey
  , indexGetAll
  , indexGetAllKeys
  , indexCount
  ) where

import Miso.IndexDb.Internal
import Miso.IndexDb.ObjectStore
import Miso.IndexDb.Prelude
import Miso.Promise

newtype Index (ts :: Maybe (s :: Type)) = Index JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)
type MetaIndex = Index (Nothing :: Maybe ())
type DataIndex s = Index (Just s)

createIndex ::
  MonadIO m =>
  MetaObjectStore ->
  MisoString ->
  MisoString ->
  m MetaIndex
createIndex (ObjectStore os) name keyPath =
  Index <$> (os # "createIndex" $ (name, keyPath))

deleteIndex :: MonadIO m => MetaObjectStore -> MisoString -> m ()
deleteIndex (ObjectStore os) name = void $ os # "deleteIndex" $ name

index :: MonadIO m => DataObjectStore s -> MisoString -> m (DataIndex s)
index (ObjectStore os) name = Index <$> (os # "index" $ name)

indexGet :: forall a k m s. (PromiseM m, ToArgs k, FromJSVal a)
         => DataIndex s -> k -> m (Request RequestCallbacks a)
indexGet (Index ix) k = do
  raw <- ix # "get" $ k
  mkRequest raw

indexGetKey :: (PromiseM m, ToArgs k) =>
  DataIndex s ->
  k ->
  m (Request RequestCallbacks JSVal)
indexGetKey (Index ix) k = do
  raw <- ix # "getKey" $ k
  mkRequest raw

indexGetAll ::
  forall a q m s.
  (PromiseM m, ToJSVal q, FromJSVal a) =>
  DataIndex s ->
  Maybe q ->
  Maybe Int ->
  m (Request RequestCallbacks [a])

indexGetAll (Index ix) q cnt = do
  raw <- ix # "getAll" $ (q, cnt)
  mkRequest raw

indexGetAllKeys :: (PromiseM m, ToArgs q) =>
  DataIndex s ->
  Maybe q ->
  m (Request RequestCallbacks [JSVal])
indexGetAllKeys (Index ix) q = do
  raw <- ix # "getAllKeys" $ q
  mkRequest raw

indexCount :: (PromiseM m, ToArgs q) =>
  DataIndex s ->
  Maybe q ->
  m (Request RequestCallbacks Int)
indexCount (Index ix) q = do
  raw <- ix # "count" $ q
  mkRequest raw
