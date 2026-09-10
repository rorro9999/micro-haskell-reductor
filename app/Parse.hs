module Parse
  ( usage
  , parseArgs
  , getDefsFromString
  ) where

import System.Exit
import System.IO
import Text.Trifecta
import Control.Applicative
import Control.Monad.State
-- choice of trifecta was influenced by
-- chapter 24 of haskell programming from first principle (later called hpffp)
-- code is heavily inspired by that chapter

usage = do
  putStrLn "Usage: zadanie3 [--help] [file]"
  putStrLn "  --help  - display this message"
  putStrLn "  file    - file with program to reduce"
  exitWith ExitSuccess

parseArgs args = case args of
  [] -> hGetContents stdin -- seems appropriate
  ["--help"] -> usage
  [x] -> readFile x
  _ -> usage



data Error =
    TrifectaError ErrInfo
  | MyError String deriving Show

-- string -> lines (without whitespace, comments) -> nested string
-- -> list of matches -> defs -> check stupid
-- stuff like has main
getDefsFromString contents = case parseString stringToLines mempty contents of
  Failure err -> Left (TrifectaError err)
  Success lines -> traverse linesToMatch lines

linesToMatch lines = halvesToMatch <$> linesToHalves lines

linesToHalves lines = case parseString splitOnEq mempty lines of
  Failure err -> Left (TrifectaError err)
  Success halves -> Right halves

--halvesToMatch = undefined
--halvesToMatch (lhs, rhs) = stringToNested lhs
halvesToMatch (lhs, rhs) = stringToNested lhs
  



skipLineComment = do
  string "--"
  skipMany (noneOf "\n")

skipBlockComment = do
  string "{-"
  manyTill anyChar (string "-}")
  return ()

separator = do
      skipSome (oneOf " \n")
  <|> skipLineComment
  <|> skipBlockComment

skipUnimportant = skipMany separator

-- removes whitespace
stringToLines :: Parser [String]
stringToLines = do
  skipUnimportant
  many $ do
    line <- some (noneOf "\n-{")
    skipUnimportant
    return line

splitOnEq = do
  lhs <- some (noneOf "=")
  char '='
  rhs <- some (noneOf "=")
  eof
  return (lhs, rhs)

-- this is an intermediate representation, between string and pat/expr
data Nested a =
   Value a
 | Group [Nested a] deriving Show

-- changes a line into a list of (some letter) "(" and ")"
tokenize :: Parser [String]
tokenize = some $ do
  skipMany (oneOf " ")
  c <- anyChar
  case c of
    '(' -> return "("
    ')' -> return ")"
    _ -> do
      rest <- many (noneOf " ()")
      skipMany (oneOf " ")
      return (c : rest)

-- turns a string into Nested String
stringToNested s = case parseString tokenize mempty s of
  Failure err -> Left (TrifectaError err)
  Success tokens -> fst <$> runStateT parseMany tokens
   where
    parseMany :: StateT [String] (Either Error) (Nested String)
    parseMany = StateT $ \s -> do
      (xs, s') <- helper s
      case xs of
        [x] -> return (x, s')
        _ -> return (Group xs, s')
      where
   -- helper exists to make it way easier to flatten the expression properly
        helper :: [String] -> Either Error ([Nested String], [String])
        helper [] = Right ([], [])
        helper s@(")" : _) = Right ([], s)
        helper s = do
          (x, s') <- runStateT parseSingle s
          (xs, ss') <- helper s'
          return (x : xs, ss')

  -- parsingle either parses something, or fails
    parseSingle :: StateT [String] (Either Error) (Nested String)
    parseSingle = StateT $ \s -> case s of
      [] -> Left (MyError "out of input!")
      (")" : _) -> Left (MyError ("unexpected closing )! rest: " ++ show s))
      ("(" : ")" : _) -> Left (MyError ("useless ()! s is: " ++ show s))
      ("(" : ss) -> do
        (xs, s') <- runStateT parseMany ss
        case s' of
          (")" : ss') -> return (xs, ss')
          _ -> Left (MyError ("unclosed parentheses! in: " ++ show s))
      (x : s') -> return (Value x, s')
