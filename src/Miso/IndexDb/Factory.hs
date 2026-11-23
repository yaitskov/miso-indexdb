module Miso.IndexDb.Factory where

import Miso.Cb
import Miso.IndexDb.JsVal (Awaitable(..))
import Miso.IndexDb.Prelude

newtype Factory = Factory JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

factory :: MonadIO m => m Factory
factory = Factory <$> jsg "indexedDB"

data DatabaseInfo
  = DatabaseInfo
  { name :: MisoString
  , version :: Int
  } deriving (Show, Eq, Generic)

instance FromJSVal DatabaseInfo

listDatabaseNames :: (CbM m) => Factory -> m [DatabaseInfo]
listDatabaseNames (Factory f) = do
  p <- f # "databases" $ ()
  fromJSValUnchecked =<< await p
