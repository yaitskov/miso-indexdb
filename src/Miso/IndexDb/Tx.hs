{-# LANGUAGE RequiredTypeArguments #-}

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
import Miso.IndexDb.Cb
import Miso.IndexDb.Database
import Miso.IndexDb.JsVal
import Miso.IndexDb.Prelude
import Miso.IndexDb.Request
-- import Miso.IndexDb.List


data TxMode = ReadOnly | ReadWrite | VersionChange

instance ToJSVal TxMode where
  toJSVal ReadOnly = toJSVal ("readonly" :: MisoString)
  toJSVal ReadWrite = toJSVal ("readwrite" :: MisoString)
  toJSVal VersionChange = toJSVal ("versionchange" :: MisoString)

data TxState = TxOpen | TxAborted | TxCommitted deriving (Show, Eq)

type family TxMethod a where
  TxMethod TxAborted = Just "abort"
  TxMethod TxCommitted = Just "commit"
  TxMethod TxOpen = Nothing

type OnComplete = "oncomplete"
type OnAbort = "onabort"

type TransactionCallbacks = '[OnComplete, OnError, OnAbort]

data Tx (txScope :: Type) (pending :: [Symbol]) r (txState :: TxState)
  = Tx
    { tx :: Request r pending
    , finishedWith :: MVar r
    } deriving Generic

instance ToObject (Tx s p r t) where
  toObject tx = toObject tx.tx

endTxWith ::
  forall t m mn r s.
  (MonadIO m, KnownSymbol mn, TxMethod t ~ Just mn) =>
  Tx s '[] r TxOpen -> r -> m (Tx s '[] r t)
endTxWith tx r = do
  putMVar tx.finishedWith r
  void $ tx # (ms $ symbolVal (Proxy @mn)) $ ()
  pure (to (from tx))

abortWith :: MonadIO m =>
  Tx s '[] r TxOpen -> r -> m (Tx s '[] r TxAborted)
abortWith = endTxWith

abort :: MonadIO m => Tx s '[] () TxOpen -> m (Tx s '[] () TxAborted)
abort tx = abortWith tx ()

commitWith :: MonadIO m =>
  Tx s '[] r TxOpen -> r -> m (Tx s '[] r TxCommitted)
commitWith  = endTxWith

commit :: MonadIO m => Tx s '[] () TxOpen -> m (Tx s '[] () TxCommitted)
commit tx = commitWith tx ()

type family TxIsComplete txs where
  TxIsComplete TxOpen = 'False
  TxIsComplete TxAborted = 'True
  TxIsComplete TxCommitted = 'True

instance (TxIsComplete tsx ~ 'True) => Awaitable (Tx s '[] r tsx) r where
  await tx = await tx.tx

withTx :: forall r m tsx.
  (TxIsComplete tsx ~ 'True, CbM m) =>
  Database ->
  [MisoString] ->
  TxMode ->
  (forall s. Tx s TransactionCallbacks r TxOpen -> m (Tx s '[] r tsx)) ->
  m r
withTx db names mode f = do
  r <- reqOfJSVal =<< (db # "transaction" $ (names, mode))
  -- need catch
  mv <- newEmptyMVar
  f (Tx r mv) >>= await

data EmtpyMVarException = EmtpyMVarException deriving (Show, Eq)
instance Exception EmtpyMVarException

onTransactionX ::
  forall x pending rest ->
  KnownSymbol x => forall s m r t.
  ( CbM m
  , ElemDrop x pending rest
  ) =>
  (JSVal -> r -> m r) ->
  Tx s pending r t ->
  m (Tx s rest r t)
onTransactionX x pending rest cb tx = do
  a <- resolveOn x pending rest go tx.tx
  pure $ Tx a tx.finishedWith
  where
    go j = do
      tryReadMVar tx.finishedWith >>= \case
        Nothing -> throwIO EmtpyMVarException
        Just r -> cb j r

type OnTransX x pending rest =
  forall s m r txState.
  ( CbM m
  , ElemDrop x pending rest
  ) =>
  (JSVal -> r -> m r) ->
  Tx s pending r txState ->
  m (Tx s rest r txState)

onTxComplete :: forall x pending rest -> (x ~ OnComplete) => OnTransX x pending rest
onTxComplete x pending rest = onTransactionX x pending rest

onTxError :: forall x pending rest -> (x ~ OnError) => OnTransX x pending rest
onTxError x pending rest = onTransactionX x pending rest

onTxAbort :: forall x pending rest -> (x ~ OnAbort) => OnTransX x pending rest
onTxAbort x pending rest = onTransactionX x pending rest
