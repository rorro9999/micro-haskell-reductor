module Main where

import Types
import Parse
import System.Environment
import System.Exit


snc = fromList [1..5] :: SnocList Int
e1 = (Con "lewe") :$ (Var "prawe")
e2 = (Con "ll" :$ Con "lr") :$ (Var "rl" :$ Var "rr" :$ Var "rR")

main :: IO ()
main = do
  args <- getArgs
  contents <- parseArgs args
  let defs = getDefsFromString contents
  case defs of
    Left err -> do
      putStrLn $ "error: " ++ show err
      exitWith (ExitFailure 1)
    Right x -> putStrLn $ niceShowList "" "\n" x ""
  
