{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
-- from hpffp

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
import Types
-- choice of trifecta was influenced by
-- chapter 24 of haskell programming from first principle (short: hpffp)
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

-- this is an intermediate representation, between string and pat/expr
data Nested a =
   Value a
 | Group [Nested a] deriving (Show, Functor, Foldable, Traversable)

-- string -> lines (without whitespace, comments) -> nested string
-- -> list of matches -> defs -> check stupid
-- stuff like has main
getDefsFromString contents = case parseString stringToLines mempty contents of
  Failure err -> Left (TrifectaError err)
  Success lines -> do
    matches <- traverse linesToMatch lines
    defs <- matchesToDefs matches
    return defs

matchesToDefs :: [Match] -> Either Error [Def]
matchesToDefs [] = Left (MyError "no matches found in file")
matchesToDefs (m:ms) = pure (Def <$> helper [[m]] ms)
 where
  helper :: [[Match]] -> [Match] -> [[Match]]
  helper (def@(a:_) : rest) (x:xs)
   | matchName a == matchName x = helper ((x:def):rest) xs
   | otherwise = helper ([x]:def:rest) xs
  helper acc [] = acc

linesToMatch lines = do
  halves <- linesToHalves lines
  halvesToMatch halves

linesToHalves lines = case parseString splitOnEq mempty lines of
  Failure err -> Left (TrifectaError err)
  Success halves -> Right halves

halvesToMatch (lhs, rhs) = (transformLhs lhs) <*> (transformRhs rhs)

transformRhs :: String -> Either Error Expr
transformRhs rhs = rhsFlat >>= helper
 where
  rhsFlat = stringToNested rhs
  helper (Value x)
   | isCon x   = return (Con x)
   | otherwise = return (Var x)
  helper (Group [x]) = helper x
  helper (Group (x:xs)) = do
    ys <- mapM helper xs
    y <- helper x
    return (foldl (\acc e -> acc :$ e) y ys)
  helper g = Left (MyError ("unexpected input: " ++ show g))

transformLhs :: String -> Either Error (Expr -> Match)
transformLhs lhs = Match <$> checkedName <*> pats
 where
  lhsFlat = stringToNested lhs
  checkedName = assertVar =<< name
  name = do
    lhflat <- lhsFlat
    case lhflat of
      Value nm -> return nm
      Group (first:_) -> case first of
        Group _ -> Left (MyError ("no name found for left handside " ++ lhs))
        Value x -> return x
      _ -> Left (MyError "unexpected empty group for lhs")
  pats = do
    lhflat <- lhsFlat
    nestedToPats lhflat

-- if tail fails, it means name is invalid
nestedToPats :: Nested String -> Either Error [Pat]
nestedToPats (Value _) = return []
nestedToPats (Group ms) = traverse helper (tail ms)
 where
  helper :: Nested String -> Either Error Pat
  singleName str
   | isCon str = Right (PApp str [])
   | otherwise = Right (PVar str)
  helper (Value x) = singleName x
  helper (Group [Value x]) = singleName x
  helper g@(Group (Value x:xs))
   | isCon x = do
     ps <- mapM helper xs
     return (PApp x ps)
   | otherwise = Left (MyError ("expected constructor, received var in: " ++ show g))
  helper g = Left (MyError ("unexpected input when trying to create pats: " ++ show g))

isLowerCase c = 'a' <= c && c <= 'z'
isVar = isLowerCase . head
assertVar str
 | isVar str = Right str
 | otherwise = Left (MyError ("identifier " ++ str ++ " was expected to be variable (first letter lower case)!"))

isUpperCase c = 'A' <= c && c <= 'Z'
isCon = isUpperCase . head
assertCon str
 | isCon str = Right str
 | otherwise = Left (MyError ("identifier " ++ str ++ " was expected to be constructor (first letter upper case)!"))

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
  Success tokens -> case runStateT parseMany tokens of
    Right (nested, []) -> Right nested
    Right (_, ss) -> Left (MyError ("didn't consume whole input! likely wrong input: " ++ show ss))
    Left e -> Left e
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
