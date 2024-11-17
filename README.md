# Evalyn
A lightweight Scheme interpreter implemented in Haskell. This project includes my solutions to [https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours][Write Yourself a Scheme in 48 Hours], as well as additional changes I've implemented. 

## Features

- Full support for core Scheme syntax and semantics
- REPL (Read-Eval-Print Loop) for interactive use
- Simple error handling and debugging
- Linux environment support - use instead of shell scripts using Haskell's `System` module


# Examples

```
> (+ 1 2 3)
6
> (define x 10)
> (* x 2)
20
```


## Installation 
- Just get a binary from releases (Tested on Linux, but should work with MacOS too)

### Compiling from source
- [https://www.haskell.org/ghc/][GHC] (Tested on 9.2.7)
- Cabal
- Clone the directory, then run `cabal` run inside the directory
