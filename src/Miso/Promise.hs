{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
module Miso.Promise
  ( module Miso.Promise
  , module UnliftIO.Miso
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Miso hiding (tell, (#), (!), jsg, setField, fromJSValUnchecked, new, apply, await)
import Miso qualified as M
import Prelude
import UnliftIO
import UnliftIO.Miso


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

class Awaitable t a | t -> a where
  await :: MonadIO m => t -> m a

instance Awaitable Promise JSVal where
  await :: MonadIO m => Promise -> m JSVal
  await (Promise p) = liftIO $ M.await p

instance Awaitable JSVal JSVal where
  await :: MonadIO m => JSVal -> m JSVal
  await j = liftIO $ M.await j
