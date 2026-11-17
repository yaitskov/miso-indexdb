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

import Miso.IndexDb.Internal
import Miso.Promise
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Prelude

newtype Cursor = Cursor JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

data Direction = Next | Previous | NextUnique | PreviousUnique

instance ToJSVal Direction where
  toJSVal Next = toJSVal ("next" :: MisoString)
  toJSVal Previous = toJSVal ("prev" :: MisoString)
  toJSVal NextUnique = toJSVal ("nextunique" :: MisoString)
  toJSVal PreviousUnique = toJSVal ("prevunique" :: MisoString)

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
