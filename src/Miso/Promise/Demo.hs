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
  IDB.result
    =<< IDB.onError
    =<< IDB.onSuccess
    =<< IDB.onBlocked (\_ -> pure ())
    =<< IDB.onUpgradeNeeded upgradeStore
    =<< IDB.open f "blob" (Just 1)

upgradeStore :: PromiseM m => IDB.Database -> m ()
upgradeStore db = do
  storeExists <- IDB.hasObjectStore db storeName
  unless storeExists do
    void $ IDB.createObjectStore db storeName (IDB.StoreOptions Nothing True)

withDatabase :: (IDB.Database -> PromiseT IO a) -> IO a
withDatabase f = bracket openDb IDB.close (runPromiseT . f)

remove :: MisoString -> MisoString -> IO ()
remove store key = withDatabase \db ->
  IDB.withTransaction db [storeName] IDB.ReadWrite \tx -> do
    os <- IDB.objectStore tx storeName
    void $ IDB.delete os [store, key]
    IDB.onTransactionAbort (\_ -> pure ())
      =<< IDB.onTransactionError (\_ -> pure ())
      =<< IDB.onTransactionComplete (pure ())
      =<< pure tx
