{-# LANGUAGE BlockArguments #-}
module Miso.Promise.Demo (openDb, remove) where

import Control.Monad (void, when)
import Miso.IndexDb qualified as IDB
import Miso.Promise
import UnliftIO (bracket)
import Prelude

openDb :: IO JSVal
openDb = do
  db' <- runPromiseT $ do
    p <- newPromise (\resolve reject -> do
      indexDb <- jsg "indexDb"
      r <- indexDb # "open" $ ("blob" :: MisoString, 1 :: Int)
      setField r "onupgradeneeded" =<< mkAsyncCallback do
        db <- r ! "result"
        collectNames <- liftIO $ db ! "objectStoreNames"
        let storeName = "blobs" :: MisoString
        hasBlobsCollection :: Bool <- fromJSValUnchecked =<< (collectNames # "contains" $ (storeName))
        when hasBlobsCollection do
          void $ db # "createObjectStore" $ (storeName)
      setField r "onsuccess" =<< mkAsyncCallback do
        void $ (Function resolve `apply`) =<< r ! "result"
      setField r "onerror" =<< mkAsyncCallback do
        void $ (Function reject `apply`) =<< r ! "error"
      )
    await p
  pure db'

withDatabase :: (IDB.Database -> PromiseT IO a) -> IO a
withDatabase f = bracket (IDB.Database <$> openDb) IDB.close (runPromiseT . f)

remove :: MisoString -> MisoString -> IO ()
remove store key = withDatabase \db -> do
  tx <- IDB.transaction db ["blobs"] IDB.ReadWrite
  os <- IDB.objectStore tx "blobs"
  void $ IDB.delete os [store, key]
  tx' <- IDB.transactionAwait tx
  complete <- IDB.onTransactionComplete (pure ()) tx'
  failed <- IDB.onTransactionError (\_ -> pure ()) complete
  aborted <- IDB.onTransactionAbort (\_ -> pure ()) failed
  IDB.awaitTransaction aborted
{- function openDatabase() {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION);
      request.onupgradeneeded = () => {
        const db = request.result;
        if (!db.objectStoreNames.contains(STORE_NAME)) {
          db.createObjectStore(STORE_NAME, { autoIncrement: true });
        }
      };
      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });
  } -}


{-
class IDBDatabase a where
  objectStoreNames :: MonadIO m => a -> m [ String ]

newtype Idb = Idb { db :: JSVal }

data IDBOpenDBRequest
  = IDBOpenDBRequest
    {
      onupgradeneeded :: forall db. IDBDatabase db => db -> IO ()
    }

class IDBFactory f db where
  open :: MonadIO m => f -> String -> Maybe Int -> IDBOpenDBRequest -> m db

newtype IdbFactory = IdbFactory { indexDbV :: JSVal }

indexDb :: IdbFactory
indexDb = unsafePerformIO (IdbFactory <$> jsg "indexDb")

instance IDBFactory IdbFactory where
  open (IdbFactory i) dbName dbVersion req = do
    r <- i # "open" $ (dbName , fromMaybe 1 dbVersion)

    setField r "onupgradeneeded" =<< asyncCallback (req.onupgradeneeded
-}
