module Miso.IndexDb.Index
  ( Index(..)
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
import Miso.Promise
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Prelude

newtype Index = Index JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

createIndex :: MonadIO m => ObjectStore -> MisoString -> MisoString -> m Index
createIndex (ObjectStore os) name keyPath =
  Index <$> (os # "createIndex" $ (name, keyPath))

deleteIndex :: MonadIO m => ObjectStore -> MisoString -> m ()
deleteIndex (ObjectStore os) name = void $ os # "deleteIndex" $ name

index :: MonadIO m => ObjectStore -> MisoString -> m Index
index (ObjectStore os) name = Index <$> (os # "index" $ name)

indexGet :: forall a k m. (PromiseM m, ToArgs k, FromJSVal a)
         => Index -> k -> m (Request RequestCallbacks a)
indexGet (Index ix) k = do
  raw <- ix # "get" $ k
  mkRequest raw

indexGetKey :: (PromiseM m, ToArgs k) =>
  Index ->
  k ->
  m (Request RequestCallbacks JSVal)
indexGetKey (Index ix) k = do
  raw <- ix # "getKey" $ k
  mkRequest raw

indexGetAll :: forall a q m. (PromiseM m, ToJSVal q, FromJSVal a)
            => Index -> Maybe q -> Maybe Int -> m (Request RequestCallbacks [a])
indexGetAll (Index ix) q cnt = do
  raw <- ix # "getAll" $ (q, cnt)
  mkRequest raw

indexGetAllKeys :: (PromiseM m, ToArgs q) =>
  Index ->
  Maybe q ->
  m (Request RequestCallbacks [JSVal])
indexGetAllKeys (Index ix) q = do
  raw <- ix # "getAllKeys" $ q
  mkRequest raw

indexCount :: (PromiseM m, ToArgs q) =>
  Index ->
  Maybe q ->
  m (Request RequestCallbacks Int)
indexCount (Index ix) q = do
  raw <- ix # "count" $ q
  mkRequest raw
