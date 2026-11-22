{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Miso.IndexDb.JsVal where

import Miso.IndexDb.Prelude
import Miso.Promise
import Miso.IndexDb.List

newtype JsVal (callbacks :: [Symbol]) = JsVal { unJsVal :: JSVal }
  deriving newtype (FromJSVal, ToJSVal, ToObject)

type ElemDrop name pending rest = (Elem name pending ~ 'True, Drop name pending ~ rest)

type HasCallbacks1Con t name pending rest = forall m.
    ( PromiseM m
    , KnownSymbol name
    , ElemDrop name pending rest
    )
    => (JSVal -> m ()) -> t pending -> m (t rest)

class HasCallbacks t where
  setCallback1 :: forall name pending rest -> KnownSymbol name => HasCallbacks1Con t name pending rest

instance HasCallbacks JsVal where
  setCallback1 :: forall name pending rest -> KnownSymbol name => HasCallbacks1Con JsVal name pending rest
  setCallback1 name _ _ f (JsVal j) = do
    setField j (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
    pure $ JsVal j
