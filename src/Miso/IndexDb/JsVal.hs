{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Miso.IndexDb.JsVal where

import Miso hiding (await, setField)
import Miso qualified as M
import Miso.Cb
import Miso.IndexDb.List
import Miso.IndexDb.Prelude

newtype JsVal (callbacks :: [Symbol]) = JsVal { unJsVal :: JSVal }
  deriving newtype (FromJSVal, ToJSVal, ToObject)

type ElemDrop name pending rest = (Elem name pending ~ 'True, Drop name pending ~ rest)

type HasCallbacks1Con t name pending rest = forall m.
    ( CbM m
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

class Awaitable t a | t -> a where
  await :: MonadIO m => t -> m a


instance Awaitable JSVal JSVal where
  await :: MonadIO m => JSVal -> m JSVal
  await j = liftIO $ M.await j
