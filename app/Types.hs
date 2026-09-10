module Types where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as Map

-- prints list nicely without parentheses nor commas
niceShowList :: Show a => String -> String -> [a] -> ShowS
niceShowList before after =
 foldr (\x acc -> showString before . showsPrec 9 x . showString after . acc) id

-----------------
-- Match & Def --
-----------------

type Name = String
data Def = Def { defMatches :: [Match] }
data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  ::Expr
    }

-- prints list of definitions
instance Show Def where
  showsPrec _ df = case defMatches df of
    [] -> id
    [m] -> showsPrec 0 m
    (m:ms) -> showsPrec 0 m . showChar '\n' . showsPrec 0 (Def ms)

instance Show Match where
  showsPrec _ (Match name pats rhs) =
    showString name . (niceShowList " " "" pats) .
    showString " = " . showsPrec 0 rhs

----------------
-- Expr & Pat --
----------------

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
data Pat = PVar Name | PApp Name [Pat]

instance Show Expr where
  showsPrec _ e = case e of
    Var x -> showString x
    Con x -> showString x
    (a :$ (b :$ c)) -> showsPrec 0 a . showChar ' ' .
       showParen True (showsPrec 0 b . showChar ' ' . showsPrec 0 c)
    (a :$ b) -> showsPrec 0 a . showChar ' ' . showsPrec 0 b

instance Show Pat where
  showsPrec bind p = case p of
    PVar name -> showString name
    PApp name [] -> showString name
    PApp name ps -> showParen True (showString name . niceShowList " " "" ps)

------------
-- zipper --
------------
-- inspired by https://learnyouahaskell.github.io/zippers

-- ZipL means expr is the left child, ZipR means expr is the right child
data Zipper = Zipper Expr (SnocList Zip)
data Zip = ZipL Expr | ZipR Expr

-- i wanted to avoid rewriting, but ideas like Con "{" :$ focus :$ Con "}" for
-- rendering do not seem to work : (
-- so i reverse (for simplicity) the list and rewrite expr show + injection
instance Show Zipper where
  showsPrec _ (Zipper e snc) =
    inject (showChar '{' . showsPrec 0 e . showChar '}') (toList snc)
   where
    inject :: ShowS -> [Zip] -> ShowS
    inject inj snc = case snc of
      [] -> inj
      (ZipL el : zs)
       | null zs   -> showsPrec 0 el . showChar ' ' . inj
       | otherwise -> showsPrec 0 el . showChar ' ' . showParen True
         (inject inj zs)
      (ZipR er : zs) -> inject (inj . showChar ' ' . showsPrec 0 er) zs

---------------------
-- defmap & StateT --
---------------------

type DefMap = Map.Map Name Def

type TreeS = StateT
data TS = TS {
    fuel :: Int
  , hist :: SnocList Zipper }

--------------
-- SnocList --
--------------
-- derived eq is trivial
newtype SnocList a = SnocList {unSnocList :: [a]} deriving (Eq)

toList (SnocList xs) = reverse xs
fromList xs = SnocList (reverse xs)
snoc (SnocList xs) x = SnocList (x : xs)
infixr 9 -:
(-:) = snoc

instance Functor SnocList where
  fmap f (SnocList xs) = SnocList (fmap f xs)

instance Foldable SnocList where
  foldr f z snc = foldr f z (toList snc)

instance Show a => Show (SnocList a) where
  showsPrec _ (SnocList xs) = case xs of
    [] -> id
    (y:ys) -> foldr (\el acc -> el . showChar '\n' . acc) id
      (fmap (showsPrec 0) ys) . showsPrec 0 y

-- default semigroup of appending (++)
instance Semigroup (SnocList a) where
  (SnocList xs) <> (SnocList ys) = SnocList (ys ++ xs)

instance Monoid (SnocList a) where
  mempty = SnocList []

instance Applicative SnocList where
  pure x = SnocList [x]
  (SnocList fab) <*> (SnocList a) = SnocList (fab <*> a)

-- default alternative of appending (++)
instance Alternative SnocList where
  empty = SnocList []
  xs <|> ys = xs <> ys
