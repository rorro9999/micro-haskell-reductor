module Types where

-- state imports are made with ai, as documentation is bad
import Control.Applicative
import Control.Monad.State.Strict (State, runState, state, execState, put)
import Control.Monad
import qualified Data.Map as Map
import Text.Trifecta

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
    , matchRhs  :: Expr
    }
getDefName = matchName . head . defMatches

-- prints list of definitions
instance Show Def where
  showsPrec _ df = case defMatches df of
    [] -> id
    [m] -> showsPrec 0 m
    m:ms -> showsPrec 0 m . showChar '\n' . showsPrec 0 (Def ms)

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
    a :$ (b :$ c) -> showsPrec 0 a . showChar ' ' .
       showParen True (showsPrec 0 b . showChar ' ' . showsPrec 0 c)
    a :$ b -> showsPrec 0 a . showChar ' ' . showsPrec 0 b

instance Show Pat where
  showsPrec bind p = case p of
    PVar name -> showString name
    PApp name [] -> showString name
    PApp name ps -> showParen True (showString name . niceShowList " " "" ps)

------------
-- Zipper --
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
    inject (showChar '{' . showsPrec 0 e . showChar '}') (sncToList snc)
   where
    inject :: ShowS -> [Zip] -> ShowS
    inject inj snc = case snc of
      [] -> inj
      ZipL el : zs
       | null zs   -> showsPrec 0 el . showChar ' ' . inj
       | otherwise -> showsPrec 0 el . showChar ' ' . showParen True
         (inject inj zs)
      ZipR er : zs -> inject (inj . showChar ' ' . showsPrec 0 er) zs

-------------------------------------------------
-- DefMap & SubstMap & NameDef & Error & Nested --
-------------------------------------------------

type DefMap = Map.Map Name Def
type SubstMap = Map.Map Name Expr
type NameExpr = (Name, Expr)

data Error =
    TrifectaError ErrInfo
  | MyError String deriving Show

data Nested a =
   Value a
 | Group [Nested a] deriving Show

nstToList x = go [] x
 where
  go acc (Value y) = y:acc
  go acc (Group (y:ys)) = go (go acc y) (Group ys)
  go acc (Group []) = acc

--------------------------
-- TreeS & ReduceResult --
--------------------------

-- used to determine if spine reduced, does not convey recursive meaning
--    :$
--    / \
--  :$   b
--  / \
-- f   a
-- only reduction of f, f a, f a b into new expr should return success.
-- forced reductions inside a or b are not considered Success
-- BUT OutOfFuel is recursive
data ReduceResult =
    OutOfFuel
  | Success
  | None deriving Show

type TreeS = State TS
data TS = TS {
    fuel   :: Int
  , hist   :: SnocList Zipper
  , foc    :: Zipper
  , df     :: DefMap
  }

get = state $ \s -> (s, s)

curExpr s = let Zipper e _ = foc s in e
curUp s = let Zipper _ snc = foc s in snc

replaceExpr :: Expr -> TreeS ()
replaceExpr e = do
  s <- get
  let Zipper _ snc = foc s
  put (s { foc = Zipper e snc } )

writeFoc :: Zipper -> TreeS ()
writeFoc focus = state $ \s -> ((), s { hist = hist s -: focus } )

decreaseFuel :: TreeS ()
decreaseFuel = state $ \s -> do
  ((), s { fuel = (fuel s) - 1 } )

useFuel = do
  decreaseFuel
  s <- get
  if fuel s <= 0
  then return OutOfFuel
  else return None

goUp :: TreeS ()
goUp = state $ \s -> do
  case foc s of
    Zipper e (SnocList (ZipL el:zs)) ->
      ((), s { foc = Zipper (el :$ e) (SnocList zs) } )
    Zipper e (SnocList (ZipR er:zs)) ->
      ((), s { foc = Zipper (e :$ er) (SnocList zs) } )

goRight :: TreeS ()
goRight = state $ \s -> do
  let (Zipper (l :$ r) snc) = foc s
  ((), s { foc = Zipper r (snc -: ZipL l) } )

goLeft :: TreeS ()
goLeft = state $ \s -> do
  let (Zipper (l :$ r) snc) = foc s
  ((), s { foc = Zipper l (snc -: ZipR r) } )

goLeftMost = do
  s <- get
  case curExpr s of
    _ :$ _ -> do
      goLeft
      goLeftMost
    _ -> return ()

goRightMost = do
  s <- get
  if canGoRightUp s
  then goUp >> goRightMost
  else return ()

canGoUp s = case curUp s of
  SnocList (x:xs) -> True
  _ -> False

canGoRightUp s = case curUp s of
  SnocList (ZipR _ : _) -> True
  _ -> False

canGoLeftUp s = case curUp s of
  SnocList (ZipL _ : _) -> True
  _ -> False

canGoLeft s = case curExpr s of
  _ :$ _ -> True
  _ -> False

canGoRight = canGoLeft



--------------
-- SnocList --
--------------
-- derived eq is trivial
newtype SnocList a = SnocList {unSnocList :: [a]} deriving (Eq)

sncToList (SnocList xs) = reverse xs
sncFromList xs = SnocList (reverse xs)
snoc (SnocList xs) x = SnocList (x : xs)
infixr 9 -:
(-:) = snoc

instance Functor SnocList where
  fmap f (SnocList xs) = SnocList (fmap f xs)

instance Foldable SnocList where
  foldr f z snc = foldr f z (sncToList snc)

instance Show a => Show (SnocList a) where
  showsPrec _ (SnocList xs) = case xs of
    [] -> id
    y:ys -> foldr (\el acc -> el . showChar '\n' . acc) id
      (fmap (showsPrec 0) (reverse ys)) . showsPrec 0 y

-- default semigroup of appending (++)
instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList (ys ++ xs)

instance Monoid (SnocList a) where
  mempty = SnocList []

instance Applicative SnocList where
  pure x = SnocList [x]
  SnocList fab <*> SnocList a = SnocList (fab <*> a)

-- default alternative of appending (++)
instance Alternative SnocList where
  empty = SnocList []
  xs <|> ys = xs <> ys
