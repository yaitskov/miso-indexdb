module Miso.IndexDb (module X, Awaitable(..)) where

-- import Miso.IndexDb.Cursor
-- import Miso.IndexDb.Index
-- import Miso.IndexDb.Internal
-- import Miso.IndexDb.KeyRange
-- import Miso.IndexDb.ObjectStore
-- import Miso.IndexDb.Prelude
import Miso.IndexDb.Database as X
import Miso.IndexDb.Factory as X
import Miso.IndexDb.JsVal (Awaitable(..))
import Miso.IndexDb.Request as X
-- import Miso.IndexDb.Tx
-- import Miso.Cb hiding (get, put, errorMessage)

-- result :: (CbM m, FromJSVal a) => Request '[] a -> m a
-- result (Typed _ (RequestCore p _ _)) = fromJSValUnchecked =<< await p

-- onSuccess :: forall pending rest a m.
--   ( CbM m
--   , Elem "onsuccess" pending ~ 'True
--   , Drop "onsuccess" pending ~ rest
--   )
--   => Request pending a -> m (Request rest a)
-- onSuccess = onSuccessWith (\_ -> pure ())

-- onSuccessWith :: forall pending rest a m.
--   ( CbM m
--   , Elem "onsuccess" pending ~ 'True
--   , Drop "onsuccess" pending ~ rest
--   )
--   => (JSVal -> m ()) -> Request pending a -> m (Request rest a)
-- onSuccessWith cb req@(Typed _ core) =
--   setCallback @"onsuccess" req $ \ev -> do
--     v <- eventResult ev
--     cb v
--     resolveReq' core v

-- onError :: forall pending rest a m.
--   ( CbM m
--   , Elem "onerror" pending ~ 'True
--   , Drop "onerror" pending ~ rest
--   )
--   => Request pending a -> m (Request rest a)
-- onError = onErrorWith (\_ -> pure ())

-- onErrorWith :: forall pending rest a m.
--   ( CbM m
--   , Elem "onerror" pending ~ 'True
--   , Drop "onerror" pending ~ rest
--   )
--   => (DbError -> m ()) -> Request pending a -> m (Request rest a)
-- onErrorWith cb req@(Typed _ core) =
--   setCallback @"onerror" req $ \ev -> do
--     err <- DbError <$> eventError ev
--     cb err
--     rejectReq core (unDbError err)

-- onUpgradeNeeded :: forall pending rest a m.
--   ( CbM m
--   , Elem "onupgradeneeded" pending ~ 'True
--   , Drop "onupgradeneeded" pending ~ rest
--   )
--   => (Database -> m ()) -> Request pending a -> m (Request rest a)
-- onUpgradeNeeded cb req@(Typed _ _) =
--   setCallback @"onupgradeneeded" req $ \ev -> do
--     v <- eventResult ev
--     cb (Database v)

-- onBlocked :: forall pending rest a m.
--   ( CbM m
--   , Elem "onblocked" pending ~ 'True
--   , Drop "onblocked" pending ~ rest
--   )
--   => ((Int, Int) -> m ()) -> Request pending a -> m (Request rest a)
-- onBlocked cb req@(Typed _ _) =
--   setCallback @"onblocked" req $ \ev -> do
--     oldV <- fromJSValUnchecked =<< (ev ! "oldVersion")
--     newV <- fromJSValUnchecked =<< (ev ! "newVersion")
--     cb (oldV, newV)



-- cmp :: (MonadIO m, ToJSVal a) => Factory -> a -> a -> m Int
-- cmp (Factory f) a b = fromJSValUnchecked =<< (f # "cmp" $ (a, b))
