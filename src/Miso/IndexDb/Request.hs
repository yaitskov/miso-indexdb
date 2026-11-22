{-# LANGUAGE RequiredTypeArguments #-}

module Miso.IndexDb.Request where

import Miso.IndexDb.JsVal
    ( JsVal(JsVal), HasCallbacks(..), HasCallbacks1Con, ElemDrop )
import Miso.IndexDb.Prelude
import Miso.Promise
    ( JSVal, ToObject(..), PromiseM, Awaitable(..) )

type OnSuccess = "onsuccess"
type OnError = "onerror"

data ReqResult r = ReqResolved r | forall e. Exception e => ReqEx e

data Request r (unsetCbs :: [Symbol])
  = Request
  { requestVal :: JsVal unsetCbs
  , requestMVar :: MVar (ReqResult r)
  }

instance ToObject (Request r ucbs) where
  toObject r = toObject r.requestVal

reqOfJSVal :: PromiseM m => JSVal -> m (Request r ucbs)
reqOfJSVal j = Request (JsVal j) <$> newEmptyMVar

instance HasCallbacks (Request r) where
  setCallback1 ::
    forall name pending rest ->
    KnownSymbol name =>
    HasCallbacks1Con (Request r) name pending rest
  setCallback1 name pending rest cb r = do
    rv' <- setCallback1 name pending rest cb r.requestVal
    pure r { requestVal = rv' }

resolveOn ::
  forall m r. PromiseM m =>
  forall name ucbs ucbs' ->
  KnownSymbol name =>
  ElemDrop name ucbs ucbs' =>
  (JSVal -> m r) ->
  Request r ucbs ->
  m (Request r ucbs')
resolveOn name ucbs ucbs' cb r = do
  r' <- setCallback1 name ucbs ucbs' go r
  pure $ Request r'.requestVal r'.requestMVar
  where
    go cba = do
      tryAny (cb cba) >>= \case
        Left e -> putMVar r.requestMVar $ ReqEx e
        Right a -> putMVar r.requestMVar $ ReqResolved a
-- resolveWith :: PromiseM m => Request r ucbs -> r -> m ()
-- resolveWith req = putMVar req.requestMVar . ReqResolved

instance Awaitable (Request r '[]) r where
  await r = do
    readMVar r.requestMVar >>= \case
      ReqResolved res -> pure res
      ReqEx e -> throwIO e
