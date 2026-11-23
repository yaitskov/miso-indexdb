{-# LANGUAGE BlockArguments #-}
module Miso.Promise
  ( module Miso.Promise
  , module UnliftIO.Miso
  ) where

import Control.Applicative
import Control.Monad.Reader
import Miso hiding (tell, (#), (!), jsg, setField, fromJSValUnchecked, new, apply)
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
