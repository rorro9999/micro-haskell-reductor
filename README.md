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
is left-most outer-most.

Parser is also written from scratch by me, as a challenge :).
Getting the parser to work was a challenge :).

Running ./run.sh (glorified two echoes and cabal run -- zadanie3 "przyklady/Zadanie3 - przykłady-20260909/addinf.uhs")
yields this

```
$ ./run.sh
usage case
example file with comments
inf = S inf
add Z n = n
add (S m) n = S (add m n)
main = add inf Z

fromList [("add",add Z n = n
add (S m) n = S (add m n)),("inf",inf = S inf),("main",main = add inf Z)]
------------------------------------------------------------
{main}
{add inf Z}
add {inf} Z
add {S inf} Z
{add (S inf) Z}
{S (add inf Z)}
S (add {inf} Z)
S (add {S inf} Z)
S {add (S inf) Z}
S {S (add inf Z)}
S (S (add {inf} Z))
S (S (add {S inf} Z))
S (S {add (S inf) Z})
S (S {S (add inf Z)})
S (S (S (add {inf} Z)))
S (S (S (add {S inf} Z)))
S (S (S {add (S inf) Z}))
S (S (S {S (add inf Z)}))
S (S (S (S (add {inf} Z))))
S (S (S (S (add {S inf} Z))))
S (S (S (S {add (S inf) Z})))
S (S (S (S {S (add inf Z)})))
S (S (S (S (S (add {inf} Z)))))
S (S (S (S (S (add {S inf} Z)))))
S (S (S (S (S {add (S inf) Z}))))
S (S (S (S (S {S (add inf Z)}))))
S (S (S (S (S (S (add {inf} Z))))))
S (S (S (S (S (S (add {S inf} Z))))))
S (S (S (S (S (S {add (S inf) Z})))))
S (S (S (S (S (S {S (add inf Z)})))))
S (S (S (S (S (S (S (add {inf} Z)))))))
S (S (S (S (S (S (S (add {S inf} Z)))))))
S (S (S (S (S (S (S {add (S inf) Z}))))))
S (S (S (S (S (S (S {S (add inf Z)}))))))
S (S (S (S (S (S (S (S (add {inf} Z))))))))
S (S (S (S (S (S (S (S (add {S inf} Z))))))))
S (S (S (S (S (S (S (S {add (S inf) Z})))))))
S (S (S (S (S (S (S (S {S (add inf Z)})))))))
S (S (S (S (S (S (S (S (S (add {inf} Z)))))))))
S (S (S (S (S (S (S (S (S (add {S inf} Z)))))))))
S (S (S (S (S (S (S (S (S {add (S inf) Z}))))))))
S (S (S (S (S (S (S (S (S {S (add inf Z)}))))))))
S (S (S (S (S (S (S (S (S (S (add {inf} Z))))))))))
S (S (S (S (S (S (S (S (S (S (add {S inf} Z))))))))))
S (S (S (S (S (S (S (S (S (S {add (S inf) Z})))))))))
S (S (S (S (S (S (S (S (S (S {S (add inf Z)})))))))))
S (S (S (S (S (S (S (S (S (S (S (add {inf} Z)))))))))))
S (S (S (S (S (S (S (S (S (S (S (add {S inf} Z)))))))))))
S (S (S (S (S (S (S (S (S (S (S {add (S inf) Z}))))))))))
S (S (S (S (S (S (S (S (S (S (S {S (add inf Z)}))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (add {inf} Z))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (add {S inf} Z))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S {add (S inf) Z})))))))))))
S (S (S (S (S (S (S (S (S (S (S (S {S (add inf Z)})))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (S (add {inf} Z)))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (S (add {S inf} Z)))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (S {add (S inf) Z}))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (S {S (add inf Z)}))))))))))))
S (S (S (S (S (S (S (S (S (S (S (S (S (S (add {inf} Z))))))))))))))
run out of fuel!
```
