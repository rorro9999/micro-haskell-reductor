#!/usr/bin/bash

testdir="przyklady/Zadanie3 - przykłady-20260909"

echo "usage case"
#cabal run -- zadanie3
#cabal run -- zadanie3 --help
echo "example file with comments"
#cabal run -- zadanie3 "$testdir/addinf.uhs"
#cabal run -- zadanie3 "$testdir/add22.uhs"
cabal run --ghc-options="-O0" -- zadanie3 "$testdir/add22.uhs"
