{-# LANGUAGE BlockArguments #-}
module Miso.Promise
  ( module Miso.Promise
  , module UnliftIO.Miso
  ) where

import Miso hiding (tell, (#), (!), jsg, setField, fromJSValUnchecked, new, apply, await)
import Miso qualified as M
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import UnliftIO
import UnliftIO.Miso
import Prelude


newtype CbsToFree = CbsToFree { unCbsToFree :: MVar [Function] }
newtype PromiseT m a = FfiT (ReaderT CbsToFree m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader CbsToFree)

type PromiseM m = (MonadUnliftIO m, MonadReader CbsToFree m)

mkAsyncCallbackX :: PromiseM m => ((forall a. m a -> IO a) -> IO JSVal) -> m JSVal
mkAsyncCallbackX f = do
  fsMv <- unCbsToFree <$> ask
  -- FfiT do
  fw <- withRunInIO f
  modifyMVar_ fsMv $ (pure . (Function fw :))
  pure fw

mkAsyncCallback :: PromiseM m => m () -> m JSVal
mkAsyncCallback f = mkAsyncCallbackX $ \riio -> asyncCallback (riio f)

mkAsyncCallback1 :: PromiseM m => (JSVal -> m ()) -> m JSVal
mkAsyncCallback1 f = mkAsyncCallbackX $ \riio -> asyncCallback1 $ \x -> riio (f x)

mkAsyncCallback2 :: PromiseM m => (JSVal -> JSVal -> m ()) -> m JSVal
mkAsyncCallback2 f = mkAsyncCallbackX $ \riio -> asyncCallback2 $ \x y -> riio (f x y)

runPromiseT :: MonadUnliftIO m => PromiseT m a -> m a
runPromiseT (FfiT f) = do
  fsMv <- newMVar []
  finally (runReaderT f (CbsToFree fsMv)) do
    mapM_ (liftF freeFunction ) =<< readMVar fsMv

newtype Promise = Promise { unPromise :: JSVal }

newPromise :: PromiseM m => (JSVal -> JSVal -> m ()) -> m Promise
newPromise f2 = do
  executor <- mkAsyncCallback2 f2
  Promise <$> ((`new` [executor]) =<< jsg "Promise")

await :: PromiseM m => Promise -> m JSVal
await (Promise p) = liftIO $ M.await p

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
