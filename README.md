# miso-promise

## Motivation

Safer Promise handling in Miso.
JS Promise involve lot of callbacks,
meanwhile Miso library does not track JS callbacks creation and
this can easily cause leaks.

## Development environment

HLS should be available inside the default dev shell.

```shell
$ nix develop
$ emacs src/*/*/Qq.hs &
$ cabal build
$ cabal test --test-option=--hide-successes
```
