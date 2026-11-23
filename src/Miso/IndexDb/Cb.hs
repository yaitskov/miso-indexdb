module Miso.IndexDb.Cb
  ( module Miso.IndexDb.Cb
  , module UnliftIO.Miso
  ) where

import Control.Applicative
import Control.Monad.Reader
import Miso hiding (tell, (#), (!), jsg, setField, fromJSValUnchecked, new, apply)
import Prelude
import UnliftIO
import UnliftIO.Miso


newtype CbsToFree = CbsToFree { unCbsToFree :: MVar [Function] }
newtype CbT m a = CbT (ReaderT CbsToFree m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader CbsToFree)

type CbM m = (MonadUnliftIO m, MonadReader CbsToFree m)

mkAsyncCallbackX :: CbM m => ((forall a. m a -> IO a) -> IO JSVal) -> m JSVal
mkAsyncCallbackX f = do
  fsMv <- unCbsToFree <$> ask
  -- CbT do
  fw <- withRunInIO f
  modifyMVar_ fsMv $ (pure . (Function fw :))
  pure fw

mkAsyncCallback :: CbM m => m () -> m JSVal
mkAsyncCallback f = mkAsyncCallbackX $ \riio -> asyncCallback (riio f)

mkAsyncCallback1 :: CbM m => (JSVal -> m ()) -> m JSVal
mkAsyncCallback1 f = mkAsyncCallbackX $ \riio -> asyncCallback1 $ \x -> riio (f x)

mkAsyncCallback2 :: CbM m => (JSVal -> JSVal -> m ()) -> m JSVal
mkAsyncCallback2 f = mkAsyncCallbackX $ \riio -> asyncCallback2 $ \x y -> riio (f x y)

runCbT :: MonadUnliftIO m => CbT m a -> m a
runCbT (CbT f) = do
  fsMv <- newMVar []
  finally (runReaderT f (CbsToFree fsMv)) do
    mapM_ (liftF freeFunction ) =<< readMVar fsMv
