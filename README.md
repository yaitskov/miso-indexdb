# miso-promise

## Motivation

More typed and structured way for handingi promises in Miso.

### Bare JS
An agent generates a decent JS code in case of IndexDB, for example, but it is JS...
If the project is written Haskell that looks weird.

``` javascript
function openDatabase() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);
    request.onupgradeneeded = () => {
      const db = request.result;
      if (!db.objectStoreNames.contains(STORE_NAME)) {
        db.createObjectStore(STORE_NAME);
      }
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}
```

### Haskell typed analog
``` haskell
openDb :: IO JSVal
openDb = do
  db' <- runPromiseT $ do
    p <- newPromise (\resolve reject -> do
      indexDb <- jsg "indexDb"
      r <- indexDb # "open" $ ("blob" :: MisoString, 1 :: Int)
      setField r "onupgradeneeded" =<< mkAsyncCallback do
        db <- r ! "result"
        collectNames <- liftIO $ db ! "objectStoreNames"
        let storeName = "blobs" :: MisoString
        hasBlobsCollection :: Bool <- fromJSValUnchecked =<<
          (collectNames # "contains" $ (storeName))
        when hasBlobsCollection do
          void $ db # "createObjectStore" $ (storeName)
      setField r "onsuccess" =<< mkAsyncCallback do
        void $ (Function resolve `apply`) =<< r ! "result"
      setField r "onerror" =<< mkAsyncCallback do
        void $ (Function reject `apply`) =<< r ! "error"
      )
    await p
  pure db'
```

## Development environment

HLS should be available inside the default dev shell.

```shell
$ nix develop
$ cabal build
```
