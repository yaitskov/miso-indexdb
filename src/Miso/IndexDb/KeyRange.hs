module Miso.IndexDb.KeyRange
  ( KeyRange(..)
  , keyOnly
  , lowerBound
  , upperBound
  , boundRange
  ) where

import Miso.IndexDb.Cb
import Miso.IndexDb.Prelude

newtype KeyRange = KeyRange JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

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
