{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.IndexDb.JsVal where

import Miso.IndexDb.Prelude
import Data.Type.Equality (type (==))
import Miso.Promise


type family Drop (name :: Symbol) (xs :: [Symbol]) :: [Symbol] where
  Drop n '[]       = '[]
  Drop n (x ': xs) = DropStep (n == x) n x xs

type family DropStep
  (same :: Bool)
  (n :: Symbol)
  (x :: Symbol)
  (xs :: [Symbol]) ::
  [Symbol] where
  DropStep 'True  n x xs = Drop n xs
  DropStep 'False n x xs = x ': Drop n xs

type family Elem (name :: Symbol) (xs :: [Symbol]) :: Bool where
  Elem n '[]       = 'False
  Elem n (x ': xs) = ElemStep (n == x) n xs

type family ElemStep (same :: Bool) (n :: Symbol) (xs :: [Symbol]) :: Bool where
  ElemStep 'True  n xs = 'True
  ElemStep 'False n xs = Elem n xs

newtype JsVal (callbacks :: [Symbol]) = JsVal { unJsVal :: JSVal }
  deriving newtype (FromJSVal, ToJSVal, ToObject)

setCallback1 :: forall name pending rest m.
  ( PromiseM m
  , KnownSymbol name
  , Elem name pending ~ 'True
  , Drop name pending ~ rest
  )
  => JsVal pending -> (JSVal -> m ()) -> m (JsVal rest)
setCallback1 (JsVal j) f = do
  setField j (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
  pure $ JsVal j
