-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE UndecidableInstances #-}
module Miso.IndexDb.Internal
  ( -- Typed(..)
  -- , Drop
  -- , DropStep
  -- , Elem
  -- , ElemStep
  -- , setCallback
  -- , eventResult
  -- , eventError
  -- , DbError(..)
  -- , unDbError
  -- , RequestCore(..)
  -- , Request
    RequestCallbacks
  -- , OpenCallbacks
  -- , DeleteCallbacks
  , OnSuccess
  , OnError
  -- , mkRequest
  -- , resolveReq'
  -- , rejectReq
  -- , Database(..)
  ) where

-- import Data.Coerce (coerce)
-- import Miso.Cb
-- import Miso.IndexDb.JsVal
-- import Miso.IndexDb.Prelude

-- data Typed (pending :: [Symbol]) a = Typed JSVal a

-- setCallback :: forall name pending rest a m.
--   ( CbM m
--   , KnownSymbol name
--   , Elem name pending ~ 'True
--   , Drop name pending ~ rest
--   )
--   => Typed pending a -> (JSVal -> m ()) -> m (Typed rest a)
-- setCallback (Typed raw a) f = do
--   setField raw (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
--   pure $ coerce (Typed raw a :: Typed pending a)

-- data RequestCore a = RequestCore Cb JSVal JSVal

-- type Request (pending :: [Symbol]) a = Typed pending (RequestCore a)

type OnSuccess = "onsuccess"
type OnError = "onerror"
type RequestCallbacks = '[OnSuccess, OnError]


-- mkRequest :: CbM m => JSVal -> m (Request pending a)
-- mkRequest raw = do
--   resMv <- newEmptyMVar
--   rejMv <- newEmptyMVar
--   p <- newCb $ \res rej -> putMVar resMv res >> putMVar rejMv rej
--   res <- readMVar resMv
--   rej <- readMVar rejMv
--   pure $ Typed raw (RequestCore p res rej)

-- resolveReq' :: CbM m => RequestCore a -> JSVal -> m ()
-- resolveReq' (RequestCore _ _ res) v = void $ apply (Function res) v

-- rejectReq :: CbM m => RequestCore a -> JSVal -> m ()
-- rejectReq (RequestCore _ _ rej) v = void $ apply (Function rej) v

-- eventResult :: CbM m => JSVal -> m JSVal
-- eventResult ev = do
--   target <- ev ! "target"
--   target ! "result"

-- eventError :: CbM m => JSVal -> m JSVal
-- eventError ev = do
--   target <- ev ! "target"
--   target ! "error"

-- newtype DbError = DbError JSVal
--   deriving newtype (FromJSVal, ToObject, ToJSVal)

-- unDbError :: DbError -> JSVal
-- unDbError (DbError v) = v
