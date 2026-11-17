{-# LANGUAGE BlockArguments #-}
module Miso.Promise.Demo (openDb, remove) where

import Control.Monad (unless, void)
import Miso.IndexDb qualified as IDB
import Miso.Promise
import UnliftIO (bracket)
import Prelude

storeName :: MisoString
storeName = "blobs"

openDb :: IO IDB.Database
openDb = runPromiseT do
  f <- IDB.factory
  req <- IDB.open f "blob" (Just 1)
  upgraded <- IDB.onUpgradeNeeded upgradeStore req
  blocked <- IDB.onBlocked (\_ -> pure ()) upgraded
  success <- IDB.onSuccess blocked
  done <- IDB.onError success
  IDB.result done

upgradeStore :: PromiseM m => IDB.Database -> m ()
upgradeStore db = do
  storeExists <- IDB.hasObjectStore db storeName
  unless storeExists do
    void $ IDB.createObjectStore db storeName (IDB.StoreOptions Nothing True)

withDatabase :: (IDB.Database -> PromiseT IO a) -> IO a
withDatabase f = bracket openDb IDB.close (runPromiseT . f)

remove :: MisoString -> MisoString -> IO ()
remove store key = withDatabase \db -> do
  tx <- IDB.transaction db [storeName] IDB.ReadWrite
  os <- IDB.objectStore tx storeName
  void $ IDB.delete os [store, key]
  tx' <- IDB.transactionAwait tx
  complete <- IDB.onTransactionComplete (pure ()) tx'
  failed <- IDB.onTransactionError (\_ -> pure ()) complete
  aborted <- IDB.onTransactionAbort (\_ -> pure ()) failed
  IDB.awaitTransaction aborted
