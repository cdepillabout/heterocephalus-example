# Using -ddump-splices

[![Build Status](https://travis-ci.org/cdepillabout/heterocephalus-example.svg?branch=master)](https://travis-ci.org/cdepillabout/heterocephalus-example)

When debugging Haskell code, it can be useful to look at the code generated
from Template Haskell and Quasiquote expressions.  This repository is a
tutorial explaining an easy way to view the generated Haskell code.

## The Intro

This repository contains a single Haskell file, [Example.hs](app/Example.hs).  It
is a small example of using
[heterocephalus](https://hackage.haskell.org/package/heterocephalus), a
type-safe template engine.

The Haskell code defining the the template looks like this:

```haskell
template :: Html
template =
  let a = "hello"
      b = 3 :: Int
      cs = ["foo", "bar", "baz"]
  in [compileText|
variable interpolation:
  #{a}

if control statement:
%{ if (b == 3) }
  b is 3
%{ else }
  b is some other number
%{ endif }

forall control statement:
%{ forall c <- cs }
  #{c}
%{ endforall }
   |]
```

Everything within the `compileText` quasiquote is expanded at compile-time into
Haskell code.  This tutorial explains how to see exactly what code is
generated.

## The Setup

This tutorial assumes you are using `stack`.  You need to [install
`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install) if you
haven't already done so.

If GHC is not installed, it can be installed with `stack setup`.

```sh
$ stack setup
```

Now that GHC is installed, the example program can be built with `stack build`.

```sh
$ stack build
```

The example program is built and placed somewhere under the `.stack-work/`
directory.  `stack exec`  can be used to execute it:

```sh
$ stack exec -- heterocephalus-example

variable interpolation:
  hello

if control statement:
  b is 3

forall control statement:
  foo
  bar
  baz
```

## `-ddump-splices`

The example program needs to be compiled with the GHC flag `-ddump-splices` in
order to get GHC/Stack to dump the generated Haskell code to a file.

If the example program has already been built (as above), then `stack clean`
needs to be run to make sure the executable gets rebuilt.  After that, `stack
build` can be run again, but this time with the `-ddump-splices` flag.


```sh
$ stack clean
$ stack build --ghc-options="-ddump-splices"
```

__NOTE__: _Not running `stack clean` first can cause `stack build` to appear to
succeed, but no splice file to be generated.  Make sure `stack clean` is run
before `stack build` whenever `-ddump-splices` is being used._

GHC/Stack will generate a splice file somewhere under `.stack-work/`.  The
location of the splice file will change depending on the architecture and Cabal
version. `find` can be used to figure out where the splice file is.

```sh
$ find .stack-work/ -name '*.dump-splices'
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/heterocephalus-example/heterocephalus-example-tmp/app/Example.dump-splices
```

This `.dump-splices` file will show what Haskell code is generated from each
Template Haskell and quasiquote expression.

For example, a quasiquote like this:

```haskell
[compileText|foo #{a}|]
```

would produce Haskell code that looks like this (slightly simplified):

```haskell
do
  preEscapedText "foo "
  preEscapedToMarkup a
```

## Generated Code for the Example Template

Lets go back to the example program.  Once again, the template is defined like
this:

```haskell
template :: Html
template =
  let a = "hello"
      b = 3 :: Int
      cs = ["foo", "bar", "baz"]
  in [compileText|
variable interpolation:
  #{a}

if control statement:
%{ if (b == 3) }
  b is 3
%{ else }
  b is some other number
%{ endif }

forall control statement:
%{ forall c <- cs }
  #{c}
%{ endforall }
   |]
```

If this is compiled with the `-ddump-splices` flag, the output
`Example.dump-splices` file will look like this (after being cleaned up a
little to make it more readable):

```haskell
app/Example.hs:(15,19)-(30,5): Splicing expression
    template-haskell-2.11.0.0:Language.Haskell.TH.Quote.quoteExp
      compileText
      "variable interpolation:
         #{a}

       if control statement:
       %{ if (b == 3) }
         b is 3
       %{ else }
         b is some other number
       %{ endif }

       forall control statement:
       %{ forall c <- cs }
         #{c}
       %{ endforall }
          "
  ======>
    do
      preEscapedText "variable interpolation:\n"
      preEscapedToMarkup a
      preEscapedText "\n\nif control statement:\n"
      condH
        [(b == 3, preEscapedText "  b is 3")]
        (Just (preEscapedText "  b is some other number"))
      preEscapedText "\n\nforall control statement:\n"
      forM_ cs $ \c -> do
        preEscapedText "  "
        preEscapedToMarkup c
        preEscapedText "\n"
```

Above the `=======>` line, you can see the quasiquote as it exists in the
Haskell file.  Below the line, you can see the Haskell code that GHC has
generated.

## Conclusion

Sometimes it is difficult to read the splice files, but it there is often no
other way to easily debug Template Haskell and quasiquotes.
