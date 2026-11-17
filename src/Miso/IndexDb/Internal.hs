{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  ) where

import Miso.Promise
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Data.Type.Equality (type (==))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
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
