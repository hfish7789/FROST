# FROST

**Note.** Originally hosted on GitLab at XAIN Foundation.

Experimental implementation of the FROST access control policy language.

## Getting Started

To build the project from source, GHC (Glasgow Haskell Compiler) is required.
The recommended way to install it is via [Stack](https://www.haskellstack.org).

The command

```
$ stack build
```

compiles the project, including the `frostc` executable.

```
$ frostc --help
```

```
frostc - a compiler for FROST

Usage: frostc ((-i|--ifile INFILE) | (-x|--iexpr EXPRESSION)) [-p|--pretty]
              [-c|--oconsole] [-o|--ofile OUTFILE]
  Synthesize a circuit-pair from a policy EXPRESSION or INFILE

Available options:
  -i,--ifile INFILE        Input file
  -x,--iexpr EXPRESSION    FROST expression
  -p,--pretty              Whether to pretty-print output
  -c,--oconsole            Whether to only output to console
  -o,--ofile OUTFILE       Output file (default: "out.json")
  -h,--help                Show this help text
```

## Compiling Policies

The tools in the project allow two modes of operation for building FROST
policies: using `frostc`, or as an EDSL (embedded domain-specific language)
library. The former is easier to start with.

Take this simple example of a policy:

```
grant if subject == user
      && age > 18
      && action == "drink"
```

Here `subject`, `user`, `age` and `action` are named entities that may be
resolved to values later (e.g. `subject`, `object` and `action` typically
feature in access requests). `18` is an integer literal and `"drink"` a string
literal.

Assume these are the contents of a file `policy.fro`. Compile this
with `frostc` using the option `--ifile` / `-i`:

```
$ frostc -i policy.fro
```

This produces a file `out.json` in an intermediate format, essentially a pair of
Boolean circuits equivalent to the policy.

* To specify a different output file name use the `--ofile` / `-o` option.
* To pretty-print the JSON output, use the `--pretty` / `-p` flag.
* To just output to console rather than to file, use the `--oconsole` / `-c` flag.
* Policy expressions can also be passed directly on the command line (rather
  than by file) using the `--iexpr` / `-x` option

