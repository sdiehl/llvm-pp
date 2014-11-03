llvm-pp
-------

[![Build Status](https://travis-ci.org/sdiehl/llvm-pp.svg)](https://travis-ci.org/sdiehl/llvm-pp)

A pretty printer for ``llvm-general-pure``. Goal is to be able to pretty print a sufficiently large subset of
the LLVM AST from pure Haskell without having to go through the C++ API.

Pretty much no way this code is going to pretty, it's a giant string munging program. Be warned.

Usage
-----

```bash
$ nix-shell
$ ghc -no-user-package-db Main.hs
```

License
-------

Released under the MIT License.
Copyright (c) 2014, Stephen Diehl
