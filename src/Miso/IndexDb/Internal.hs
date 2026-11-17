{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.IndexDb.Internal
  ( Typed(..)
  , Drop
  , DropStep
  , Elem
  , ElemStep
  , setCallback
  , eventResult
  , eventError
  , DbError(..)
  , unDbError
  , RequestCore(..)
  , Request
  , RequestCallbacks
  , OpenCallbacks
  , DeleteCallbacks
  , mkRequest
  , resolveReq
  , rejectReq
  , ObjectStore(..)
  , Index(..)
  ) where

import Miso.Promise
import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Data.Type.Equality (type (==))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import UnliftIO (newEmptyMVar, putMVar, readMVar)
import Prelude

type family Drop (name :: Symbol) (xs :: [Symbol]) :: [Symbol] where
  Drop n '[]       = '[]
  Drop n (x ': xs) = DropStep (n == x) n x xs

type family DropStep (same :: Bool) (n :: Symbol) (x :: Symbol) (xs :: [Symbol]) :: [Symbol] where
  DropStep 'True  n x xs = Drop n xs
  DropStep 'False n x xs = x ': Drop n xs

type family Elem (name :: Symbol) (xs :: [Symbol]) :: Bool where
  Elem n '[]       = 'False
  Elem n (x ': xs) = ElemStep (n == x) n xs

type family ElemStep (same :: Bool) (n :: Symbol) (xs :: [Symbol]) :: Bool where
  ElemStep 'True  n xs = 'True
  ElemStep 'False n xs = Elem n xs

data Typed (pending :: [Symbol]) a = Typed JSVal a

setCallback :: forall name pending rest a m.
  ( PromiseM m
  , KnownSymbol name
  , Elem name pending ~ 'True
  , Drop name pending ~ rest
  )
  => Typed pending a -> (JSVal -> m ()) -> m (Typed rest a)
setCallback (Typed raw a) f = do
  setField raw (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
  pure $ coerce (Typed raw a :: Typed pending a)

data RequestCore a = RequestCore Promise JSVal JSVal

type Request (pending :: [Symbol]) a = Typed pending (RequestCore a)

type RequestCallbacks = '["onsuccess", "onerror"]

type OpenCallbacks = '["onupgradeneeded", "onblocked", "onsuccess", "onerror"]

type DeleteCallbacks = '["onblocked", "onsuccess", "onerror"]

mkRequest :: PromiseM m => JSVal -> m (Request pending a)
mkRequest raw = do
  resMv <- newEmptyMVar
  rejMv <- newEmptyMVar
  p <- newPromise $ \res rej -> putMVar resMv res >> putMVar rejMv rej
  res <- readMVar resMv
  rej <- readMVar rejMv
  pure $ Typed raw (RequestCore p res rej)

resolveReq :: PromiseM m => RequestCore a -> JSVal -> m ()
resolveReq (RequestCore _ _ res) v = void $ apply (Function res) v

rejectReq :: PromiseM m => RequestCore a -> JSVal -> m ()
rejectReq (RequestCore _ _ rej) v = void $ apply (Function rej) v

eventResult :: PromiseM m => JSVal -> m JSVal
eventResult ev = do
  target <- ev ! "target"
  target ! "result"

eventError :: PromiseM m => JSVal -> m JSVal
eventError ev = do
  target <- ev ! "target"
  target ! "error"

newtype DbError = DbError JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

unDbError :: DbError -> JSVal
unDbError (DbError v) = v

newtype ObjectStore = ObjectStore JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)

newtype Index = Index JSVal
  deriving newtype (FromJSVal, ToObject, ToJSVal)
