# sml-parse [![CI](https://github.com/diku-dk/sml-parse/workflows/CI/badge.svg)](https://github.com/diku-dk/sml-parse/actions)

Standard ML Parser Combinator Library.

## Overview of MLB files

- `lib/github.com/diku-dk/sml-parse/parse.mlb`:

  - **signature** [`REGION`](lib/github.com/diku-dk/sml-parse/REGION.sig)
  - **signature** [`PARSE`](lib/github.com/diku-dk/sml-parse/PARSE.sig)
  - **structure** `Region` :> `REGION`
  - **structure** `Parse` :> `PARSE`


- `lib/github.com/diku-dk/sml-parse/simple_token.mlb`:

  - **signature** [`SIMPLE_TOKEN`](lib/github.com/diku-dk/sml-parse/SIMPLE_TOKEN.sig)
  - **structure** `SimpleToken` :> `SIMPLE_TOKEN`

## Use of the package

This library is set up to work well with the SML package manager
[smlpkg](https://github.com/diku-dk/smlpkg).  To use the package, in
the root of your project directory, execute the command:

```
$ smlpkg add github.com/diku-dk/sml-parse
```

This command will add a _requirement_ (a line) to the `sml.pkg` file in your
project directory (and create the file, if there is no file `sml.pkg`
already).

To download the library into the directory
`lib/github.com/diku-dk/sml-parse` (along with other necessary
libraries), execute the command:

```
$ smlpkg sync
```

You can now reference the `mlb`-file using relative paths from within
your project's `mlb`-files.

Notice that you can choose either to treat the downloaded package as
part of your own project sources (vendoring) or you can add the
`sml.pkg` file to your project sources and make the `smlpkg sync`
command part of your build process.

## Try it!

The parser combinator library works with either
[MLton](http://mlton.org) or [MLKit](http://elsman.com/mlkit/).

Now write

    $ smlpkg sync

Then simply write `make test` in your shell.

To use the MLKit as a compiler, write instead:

    $ MLCOMP=mlkit make clean test

## Authors

Copyright (c) 2015-2021 Martin Elsman, Martin Dybdal, University of
Copenhagen.
