{-# LANGUAGE AllowAmbiguousTypes #-}


module Miso.IndexDb.Tx
  ( Tx(..)
  , TxMode(..)
  , abort
  , commit
  , abortWith
  , commitWith
  , TransactionCallbacks
  , withTx
  , onTxComplete
  , onTxError
  , onTxAbort
  ) where

import GHC.Generics
import Miso.IndexDb.Internal
import Miso.IndexDb.JsVal
import Miso.IndexDb.Prelude
import Miso.Promise


data TxMode = ReadOnly | ReadWrite | VersionChange

instance ToJSVal TxMode where
  toJSVal ReadOnly = toJSVal ("readonly" :: MisoString)
  toJSVal ReadWrite = toJSVal ("readwrite" :: MisoString)
  toJSVal VersionChange = toJSVal ("versionchange" :: MisoString)

data TxState = TxOpen | TxAborted | TxCommitted deriving (Show, Eq)

type OnComplete = "oncomplete"
type OnAbort = "onabort"

type TransactionCallbacks = '[OnComplete, OnError, OnAbort]

data Tx (txScope :: Type) (pending :: [Symbol]) r (txState :: TxState)
  = Tx
    { tx :: JsVal pending
    , mFinished :: MVar ()
    , mRes :: MVar r
    } deriving (Generic)

abortWith :: MonadIO m =>
  Tx s '[] r TxOpen -> r -> m (Tx s '[] r TxAborted)
abortWith tx r = do
  putMVar tx.mRes r
  void $ tx.tx # "abort" $ ()
  pure (to (from tx))

abort :: MonadIO m => Tx s '[] () TxOpen -> m (Tx s '[] () TxAborted)
abort tx = abortWith tx ()

commitWith :: MonadIO m =>
  Tx s '[] r TxOpen -> r -> m (Tx s '[] r TxCommitted)
commitWith tx r = do
  putMVar tx.mRes r
  void $ tx.tx # "commit" $ ()
  pure (to (from tx))
commit :: MonadIO m => Tx s '[] () TxOpen -> m (Tx s '[] () TxCommitted)
commit tx = commitWith tx ()

type family TxIsComplete txs where
  TxIsComplete TxOpen = 'False
  TxIsComplete TxAborted = 'True
  TxIsComplete TxCommitted = 'True

withTx :: forall r m tsx.
  (TxIsComplete tsx ~ 'True, PromiseM m) =>
  Database ->
  [MisoString] ->
  TxMode ->
  (forall s. Tx s TransactionCallbacks r TxOpen -> m (Tx s '[] r tsx)) ->
  m r
withTx db names mode f = do
  tx <- Tx<$> (JsVal <$> (db # "transaction" $ (names, mode))) <*> newEmptyMVar <*> newEmptyMVar
  tx' <- f tx
  readMVar tx'.mRes

setCb :: forall name s pending rest m r t.
  ( PromiseM m
  , KnownSymbol name
  , Elem name pending ~ 'True
  , Drop name pending ~ rest
  )
  => Tx s pending r t -> (JSVal -> m ()) -> m (Tx s rest r t)
setCb tx f = do
  tx' <- setCallback1 @name tx.tx f
  -- setField tx.tx (ms $ symbolVal @name Proxy) =<< mkAsyncCallback1 f
  pure $ Tx tx' tx.mFinished tx.mRes

onTransactionX :: forall x s pending rest m r t.
  ( PromiseM m
  , KnownSymbol x
  , Elem x pending ~ 'True
  , Drop x pending ~ rest
  ) =>
  (JSVal -> m ()) ->
  Tx s pending r t ->
  m (Tx s rest r t)
onTransactionX cb tx =
  setCb @x tx $ \a -> do
    finally (cb a) (putMVar tx.mFinished ())

type OnTransX x s pending rest m r txState  =
  ( PromiseM m
  , Elem x pending ~ 'True
  , Drop x pending ~ rest
  ) =>
  (JSVal -> m ()) ->
  Tx s pending r txState ->
  m (Tx s rest r txState)

onTxComplete ::
  forall x s pending rest m r t. (x ~ OnComplete) => OnTransX x s pending rest m r t
onTxComplete = onTransactionX @x

onTxError ::
  forall x s pending rest m r t. (x ~ OnError) => OnTransX x s pending rest m r t
onTxError = onTransactionX @x

onTxAbort ::
  forall x s pending rest m r t. (x ~ OnAbort) => OnTransX x s pending rest m r t
onTxAbort = onTransactionX @x
