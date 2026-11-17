{-# LANGUAGE BlockArguments #-}
module UnliftIO.Miso
  ( module Miso
  , liftF
  , liftF2
  , (#)
  , (!)
  , jsg
  , new
  , apply
  , setField
  , fromJSValUnchecked
  ) where

import Miso hiding ((#), (!), jsg, setField, fromJSValUnchecked, new, apply, await)
import Miso qualified as M
import Miso.DSL qualified as MDSL
import Control.Monad.IO.Class
import Prelude

liftF :: MonadIO m => (a -> IO b) -> a -> m b
liftF f a = liftIO (f a)

liftF2 :: MonadIO m => (a -> c -> IO b) -> a -> c -> m b
liftF2 f a c = liftIO (f a c)

(#) :: (MonadIO m, ToObject object, ToArgs args) => object -> MisoString -> args -> m JSVal
(#) o = liftF2 (o MDSL.#)
infixr 2 #

(!) :: forall o m. (MonadIO m, ToObject o) => o -> MisoString -> m JSVal
(!) = liftF2 (MDSL.!)

jsg :: MonadIO m => MisoString -> m JSVal
jsg = liftF MDSL.jsg

new :: forall m a o. (ToObject o, ToArgs a, MonadIO m) => o -> a -> m JSVal
new = liftF2 MDSL.new

apply :: forall m a. (ToArgs a, MonadIO m) => Function -> a -> m JSVal
apply = liftF2 MDSL.apply

setField :: forall m o v . (MonadIO m, ToObject o, ToJSVal v) => o -> MisoString -> v -> m ()
setField o = liftF2 (MDSL.setField o)

fromJSValUnchecked :: (FromJSVal a, MonadIO m) => JSVal -> m a
fromJSValUnchecked = liftF M.fromJSValUnchecked
