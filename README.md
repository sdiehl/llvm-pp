llvm-pp
-------

[![Build Status](https://travis-ci.org/sdiehl/llvm-pp.svg)](https://travis-ci.org/sdiehl/llvm-pp)

A pretty printer for ``llvm-general-pure``. Goal is to be able to pretty print a
sufficiently large subset of the LLVM AST from pure Haskell without having to go
through the C++ API.

Pretty much no way this code is going to pretty, it's a giant string munging
program. Be warned.

Usage
-----

There is a single function ``ppllvm`` that maps a LLVM.General.AST.Module to a
String.

```haskell
import LLVM.General.AST
import LLVM.General.Pretty (ppllvm)

ppllvm :: Module -> String
```

Tests
-----

The test suite currently consists of round tripping a LLVM IR from correct IR
outputted by the llc toolchain, parsing into llvm-general AST and then printing
it back out and comparing it with the original textual form to see if the pretty
printer faithfully preserves the structure. The sample modules are in
``tests/``.

Using stack:

```bash
$ stac build
$ stack test
```
Using cabal:

```bash
$ cabal run
$ cabal run -- tests/simple.ll
```


If you're using Nix then:

```bash
$ nix-shell
$ cabal run
```

License
-------

Released under the MIT License.

Copyright (c) 2014-2016, Stephen Diehl
Copyright (c) 2015 Cedric Shock

