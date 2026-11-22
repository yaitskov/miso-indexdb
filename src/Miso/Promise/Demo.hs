{-# LANGUAGE BlockArguments #-}
module Miso.Promise.Demo where

import Miso.IndexDb as IDB
import Miso.IndexDb.Prelude
import Miso.Promise
-- import Miso.IndexDb.Request
import Miso.IndexDb.JsVal

eventResult :: PromiseM m => JSVal -> m JSVal
eventResult ev = do
  target <- ev ! "target"
  target ! "result"

-- eventError :: PromiseM m => JSVal -> m JSVal
-- eventError ev = do
--   target <- ev ! "target"
--   target ! "error"

storeName :: MisoString
storeName = "blobs"

-- openDb :: IO Database
openDb :: PromiseM m => DbName -> m Database
openDb dbName = do -- runPromiseT do
  f <- factory
  consoleLog $ "Open DB " <> dbName
  await
    =<< resolveOn OnError _ _ (\_er -> throwIO . JsEx  $ "Failed to open DB " <> dbName)
    =<< resolveOn OnSuccess _ _ (\ev -> Database <$> eventResult ev )
    =<< resolveOn OnBlocked _ _ (\_er -> throwIO . JsEx  $ "DB " <> dbName <> " is blocked")
    =<< setCallback1 OnUpgradeNeeded _ _ (\ev -> upgradeStore . Database =<< eventResult ev)
    =<< IDB.open f dbName (Just 1)

upgradeStore :: PromiseM m => Database -> m ()
upgradeStore db = do
  dbn <- getDbName db
  consoleLog $ "Upgrade schema of " <> dbn
  storeExists <- hasObjectStore db storeName
  consoleLog $ "Store " <> storeName <> " exists: " <>  ms (show storeExists)
  -- unless storeExists do
  --   void $ createObjectStore db storeName (StoreOptions Nothing True)
  -- pure db

withDatabase :: DbName -> (Database -> PromiseT IO a) -> IO a
withDatabase dbName f = runPromiseT (bracket (openDb dbName) closeDb f)

-- remove :: MisoString -> MisoString -> IO ()
-- remove store key = withDatabase \db ->
--   IDB.withTx db [store] IDB.ReadWrite (
--     onTxComplete (const $ pure ()) >=>
--     onTxError (\_ -> pure ()) >=>
--     onTxAbort (\_ -> pure ()) >=>
--     \tx -> do
--       os <- objectStore tx store
--       void $ IDB.delete os key
--       commit tx
--   )
