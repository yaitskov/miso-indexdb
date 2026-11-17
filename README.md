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
import Miso.IndexDb qualified as IDB

openDb :: IO IDB.Database
openDb = runPromiseT do
  f <- IDB.factory
  req <- IDB.open f "blob" (Just 1)
  upgraded <- IDB.onUpgradeNeeded upgradeStore req
  blocked <- IDB.onBlocked (\_ -> pure ()) upgraded
  success <- IDB.onSuccess blocked
  done <- IDB.onError success
  IDB.result done

upgradeStore :: PromiseM m => IDB.Database -> m ()
upgradeStore db = do
  storeExists <- IDB.hasObjectStore db "blobs"
  unless storeExists do
    void $ IDB.createObjectStore db "blobs" (IDB.StoreOptions Nothing True)
```

## Development environment

HLS should be available inside the default dev shell.

```shell
$ nix develop
$ cabal build
```
