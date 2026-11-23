# IndexDB bindings library

## Tmp dir
- use tmp in project root for testing
## Use miso-indexdb.cabal.template instead of miso-indexdb.cabal
miso-indexdb.cabal rendered by gen-cabal.sh so no need to look there, just
regenerate it if template changes.
## GHC extensions
- don't add pragmas to Haskell files if they are covered by pragams from cabal file
## Wrap lines longer than 85
- operator (`->` or `::`) at the end of line
## Add new imports keeping asc order
## don't import from root Prelude directly
