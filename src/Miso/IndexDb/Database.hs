module Miso.IndexDb.Database where

import Miso.IndexDb.Factory
import Miso.IndexDb.Prelude
import Miso.IndexDb.Request
import Miso.Promise


type DbName = MisoString
type OnBlocked = "onblocked"
type OnUpgradeNeeded = "onupgradeneeded"
type OpenCallbacks = '[OnUpgradeNeeded, OnBlocked, OnSuccess, OnError]
type DeleteCallbacks = '[OnBlocked, OnSuccess, OnError]

newtype Database = Database JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype JsEx = JsEx { message :: MisoString } deriving (Show, Eq)
-- instance Show JsEx where
--   show j = "JsEx " <> fromMisoString j.message
instance Exception JsEx

open ::
  PromiseM m =>
  Factory ->
  DbName ->
  Maybe Int ->
  m (Request Database OpenCallbacks)
open (Factory f) name ver =
  reqOfJSVal =<< (f # "open" $ (name, ver))

deleteDatabase ::
  PromiseM m =>
  Factory ->
  DbName ->
  m (Request () '[])
deleteDatabase (Factory f) name = do
  raw :: Request () DeleteCallbacks <- reqOfJSVal =<< (f # "deleteDatabase" $ name)

  resolveOn OnBlocked _ _ (\_ -> throwIO . JsEx $ "Delete DB " <> name <> " is blocked") =<<
    resolveOn OnError _ _ (\_ -> throwIO . JsEx $ "Delete DB " <> name <> " failed") =<<
      resolveOn OnSuccess _ _ (\_ -> consoleLog $ "DB " <> name <> " deleted") raw

objectStoreNames :: PromiseM m => Database -> m [MisoString]
objectStoreNames (Database db) = fromJSValUnchecked =<< (db ! "objectStoreNames")

hasObjectStore :: PromiseM m => Database -> MisoString -> m Bool
hasObjectStore (Database db) name = do
  names <- db ! "objectStoreNames"
  fromJSValUnchecked =<< (names # "contains" $ name)

closeDb :: MonadIO m => Database -> m ()
closeDb (Database db) = void $ db # "close" $ ()

getDbName :: MonadIO m => Database -> m MisoString
getDbName (Database db) = fromJSValUnchecked =<< (db ! "name")

dbVersion :: MonadIO m => Database -> m Int
dbVersion (Database db) = fromJSValUnchecked =<< (db ! "version")

-- errorName :: MonadIO m => DbError -> m MisoString
-- errorName (DbError e) = fromJSValUnchecked =<< (e ! "name")

-- errorMessage :: MonadIO m => DbError -> m MisoString
-- errorMessage (DbError e) = fromJSValUnchecked =<< (e ! "message")
