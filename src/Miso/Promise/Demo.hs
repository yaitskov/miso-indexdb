{-# LANGUAGE BlockArguments #-}
module Miso.Promise.Demo (openDb, remove) where

import Miso.IndexDb as IDB
import Miso.IndexDb.Prelude
import Miso.Promise

storeName :: MisoString
storeName = "blobs"

openDb :: IO IDB.Database
openDb = runPromiseT do
  f <- IDB.factory
  IDB.result
    =<< IDB.onError
    =<< IDB.onSuccess
    =<< IDB.onBlocked (\_ -> pure ())
    =<< IDB.onUpgradeNeeded upgradeStore
    =<< IDB.open f "blob" (Just 1)

upgradeStore :: PromiseM m => Database -> m ()
upgradeStore db = do
  storeExists <- IDB.hasObjectStore db storeName
  unless storeExists do
    void $ IDB.createObjectStore db storeName (IDB.StoreOptions Nothing True)

withDatabase :: (Database -> PromiseT IO a) -> IO a
withDatabase f = bracket openDb IDB.close (runPromiseT . f)

remove :: MisoString -> MisoString -> IO ()
remove store key = withDatabase \db ->
  IDB.withTx db [store] IDB.ReadWrite (
    onTxComplete (const $ pure ()) >=>
    onTxError (\_ -> pure ()) >=>
    onTxAbort (\_ -> pure ()) >=>
    \tx -> do
      os <- objectStore tx store
      void $ IDB.delete os key
      commit tx
  )
