module Miso.IndexDb.Cursor
  ( Cursor(..)
  , Direction(..)
  , openCursor
  , openKeyCursor
  , indexOpenCursor
  , indexOpenKeyCursor
  , cursorKey
  , cursorPrimaryKey
  , cursorValue
  , advance
  , continueCursor
  , continuePrimaryKey
  , deleteCursor
  , updateCursor
  ) where

import Miso.IndexDb.Index
import Miso.IndexDb.ObjectStore
import Miso.Promise
import Miso.IndexDb.Prelude
import Control.Monad (void)

newtype Cursor (s :: Type) = Cursor JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

data Direction = Next | Previous | NextUnique | PreviousUnique

instance ToJSVal Direction where
  toJSVal Next = toJSVal ("next" :: MisoString)
  toJSVal Previous = toJSVal ("prev" :: MisoString)
  toJSVal NextUnique = toJSVal ("nextunique" :: MisoString)
  toJSVal PreviousUnique = toJSVal ("prevunique" :: MisoString)

openCursor :: (PromiseM m, ToJSVal q) =>
  DataObjectStore s ->
  Maybe q ->
  Maybe Direction ->
  m (Cursor s)
openCursor (ObjectStore os) q dir =
  Cursor <$> (os # "openCursor" $ (q, dir))

openKeyCursor :: (PromiseM m, ToJSVal q) =>
  DataObjectStore s ->
  Maybe q ->
  Maybe Direction ->
  m (Cursor s)
openKeyCursor (ObjectStore os) q dir =
  Cursor <$> (os # "openKeyCursor" $ (q, dir))

indexOpenCursor :: (PromiseM m, ToJSVal q) =>
  DataIndex s ->
  Maybe q ->
  Maybe Direction ->
  m (Cursor s)
indexOpenCursor (Index ix) q dir =
  Cursor <$> (ix # "openCursor" $ (q, dir))

indexOpenKeyCursor :: (PromiseM m, ToJSVal q) =>
  DataIndex s ->
  Maybe q ->
  Maybe Direction ->
  m (Cursor s)
indexOpenKeyCursor (Index ix) q dir =
  Cursor <$> (ix # "openKeyCursor" $ (q, dir))

cursorKey :: (MonadIO m, FromJSVal a) => Cursor s -> m a
cursorKey (Cursor c) = fromJSValUnchecked =<< (c ! "key")

cursorPrimaryKey :: (MonadIO m, FromJSVal a) => Cursor s -> m a
cursorPrimaryKey (Cursor c) = fromJSValUnchecked =<< (c ! "primaryKey")

cursorValue :: (MonadIO m, FromJSVal a) => Cursor s -> m a
cursorValue (Cursor c) = fromJSValUnchecked =<< (c ! "value")

advance :: MonadIO m => Cursor s -> Int -> m ()
advance (Cursor c) n = void $ c # "advance" $ n

continueCursor :: (MonadIO m, ToArgs k) => Cursor s -> Maybe k -> m ()
continueCursor (Cursor c) mk = void $ c # "continue" $ mk

continuePrimaryKey :: (MonadIO m, ToJSVal k, ToJSVal pk)
                   => Cursor s -> k -> pk -> m ()
continuePrimaryKey (Cursor c) k pk = void $ c # "continuePrimaryKey" $ (k, pk)

deleteCursor :: PromiseM m => Cursor s -> m ()
deleteCursor (Cursor c) =
  void $ c # "delete" $ ()

updateCursor :: (PromiseM m, ToArgs v) => Cursor s -> v -> m ()
updateCursor (Cursor c) v =
  void $ c # "update" $ v
