module Miso.IndexDb.Factory where

import Miso.IndexDb.Cb
import Miso.IndexDb.DbName (DbName)
import Miso.IndexDb.JsVal (Awaitable(..))
import Miso.IndexDb.Prelude

newtype Factory = Factory JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

factory :: MonadIO m => m Factory
factory = Factory <$> jsg "indexedDB"

data DatabaseInfo
  = DatabaseInfo
  { name :: DbName
  , version :: Int
  } deriving (Show, Eq, Generic)

instance FromJSVal DatabaseInfo

listDatabaseNames :: (CbM m) => Factory -> m [DatabaseInfo]
listDatabaseNames (Factory f) = do
  p <- f # "databases" $ ()
  fromJSValUnchecked =<< await p
