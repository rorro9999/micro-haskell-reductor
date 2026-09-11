module Main where

import Types
import Parse
import Compute
import System.Environment
import System.Exit


snc = sncFromList [1..5] :: SnocList Int
e1 = (Con "lewe") :$ (Var "prawe")
e2 = (Con "ll" :$ Con "lr") :$ (Var "rl" :$ Var "rr" :$ Var "rR")

main :: IO ()
main = do
  args <- getArgs
  contents <- parseArgs args
  printPath contents
