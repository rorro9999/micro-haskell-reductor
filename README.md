## This is small reductor that implements lazy evaluation for combinators using fuel in (subset of) Haskell 

Usage reads from file/stdin if shell argument is/isn't provided.

```
$ cabal run -- zadanie3 --help
Usage: zadanie3 [--help] [file]
  --help  - display this message
  file    - file with program to reduce
```
A combinator is any rule starting from lowercase letter that substitues
identifiers from left-hand side in right-hand side such as.

```
add (S n) m = S (add n m)
main = add two two
```

A constructor is any identifier that starts from an uppercase letter (such as
S).

Combinators have pattern matching on constructors.

Evaluation of expressions is lazy, starts from main and tries for at most 
fuel = 30 reductions. Entire history of reductions is printed to stdout. Order
is left-most, then inner-most.

Parser is also written by me, as a challenge :). Getting the parser to work was
a challenge :).


