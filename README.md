# Example showing how to use -ddump-splices to look at generated Template Haskell code

[![Build Status](https://travis-ci.org/arowM/heterocephalus.svg?branch=master)](https://travis-ci.org/arowM/heterocephalus)

## Using the example program to check the generated Haskell code

The example program can be used to easily check the generated Haskell code, for
instance when using heterocephalus functions like `compileText`,
`compileTextFile`, etc.

First, compile the example program with `-ddump-splices` in order to get
GHC/Stack to dump the generated Haskell code to a file.  The example program
will only build if we enable the `buildexample` flag.

```sh
$ stack clean
$ stack build --flag "heterocephalus:buildexample" --ghc-options="-ddump-splices"
```

GHC/Stack will generate a splice file somewhere under `.stack-work/`.  The
location of the splice file will change depending on the architecture and Cabal
version.  `find` can be used to easily figure out where the splice file is.

```sh
$ find .stack-work/ -name "*.dump-splices"
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/heterocephalus-example/heterocephalus-example-tmp/example/Example.dump-splices
```

This file will show the Haskell code is generated from each Template Haskell and
quasiquote expression.

For example, a quasiquote like this:

```haskell
[compileText|foo #{a}|]
```

will produce Haskell code that looks like this (slightly simplified):

```haskell
do
  preEscapedText $ pack "foo "
  preEscapedToMarkup a
```

