module Miso.IndexDb.Factory where

import Miso.IndexDb.Prelude
import Miso.Promise

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

listDatabaseNames :: (PromiseM m) => Factory -> m [DatabaseInfo]
listDatabaseNames (Factory f) = do
  p <- f # "databases" $ ()
  fromJSValUnchecked =<< await p
