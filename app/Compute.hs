module Compute
  ( printPath
  ) where

import Types
import Parse
import qualified Data.Map as Map
import Control.Monad.State (State, runState, state, execState, get, put)
import Control.Monad.Loops (whileM_)
import System.Exit

buildDefMap defs = do
  defMap <- builder Map.empty defs
  if Map.notMember "main" defMap
    then Left (MyError "no main found!")
    else do
      let main = defMap Map.! "main"
      if 1 /= length (defMatches main)
        then Left (MyError "multiple definitions of main!")
        else do
          if 0 /= (length $ (matchPats . head . defMatches) main)
            then Left (MyError "main has arguments!")
            else return defMap
 where
  builder acc [] = pure acc
  builder acc (d:ds)
   | Map.member (getDefName d) acc = Left (MyError ("multiple definitions of " ++ show d))
   | otherwise = builder (Map.insert (getDefName d) d acc) ds

printPath contents = do
  let defs = getDefsFromString contents
  let defMap = defs >>= buildDefMap
  case defMap of
    Left err -> do
      putStrLn $ "error: " ++ show err
      exitWith (ExitFailure 1)
    Right niceDefMap -> do
      case defs of
        Left err -> do
          putStrLn $ "error: " ++ show err
          exitWith (ExitFailure 1)
        Right niceDefs -> do
          putStrLn $ niceShowList "" "\n" niceDefs ""
          putStrLn $ show niceDefMap
          putStrLn "------------------------------------------------------------"
          putStrLn $ printPath' TS { fuel    = 100
                                   , hist    = mempty
                                   , foc     = Zipper (Var "main") mempty
                                   , df      = niceDefMap } ""
  
printPath' :: TS -> ShowS
printPath' ts = case reduceResult of
  OutOfFuel -> showsPrec 0 hst . showString "\nrun out of fuel!"
  _ -> showsPrec 0 hst . showString "\nnothing left to reduce!"
 where
  (reduceResult, ts') = runState eval ts
  hst = hist ts'

-- all TreeS functions are confined to the spine they are called in.
-- caller manages entering/exiting spines

-- case for spine owner is roughly: try to reduce function,
-- if succeeded, call eval again
-- if not, eval args
-- (doesn't leave spine)

eval :: TreeS ReduceResult
eval = do
  s <- get
  case curExpr s of
    Con _ -> return None -- nothing to do
    _ -> do              -- both Var and :$ cases
      res <- tryReduction
      case res of
        OutOfFuel -> return OutOfFuel
        Success -> eval -- keep reducing spine while possible
        None -> do      -- spine is "dead", so reduce args
          evalRights
          return None

--     2:$
--     / \
--  1:$   b
--  / \
-- f   a

liftToState func = do
  s <- get
  return (func s)

evalRights = do
  goLeftMost
  s <- get
  if canGoRightUp s
  then do
    goFuncToArg -- f to a
    eval
    whileM_ canGoArgToArg (goArgToArg >> eval) -- a to b, etc
    exitSpine -- from b's spine, return to caller's spine
  else return ()

-- tryReduction tries to reduce the spine.
-- each tryMatch should enter current spine, never leave it
tryReduction = do
  goLeftMost
  s <- get
  case curExpr s of
   Con _ -> return None -- nothing to reduce
   Var v -> do
    case Map.lookup v (df s) of
     Nothing -> return None -- no definition for function
     Just (Def matches) -> do
     -- try tries matches, until first works
      let try [] = return None
          try (x:xs) = do
            res <- tryMatch x
            case res of
              None -> try xs
              y -> return y -- propagates both OutOfFuel and Success cases
      try matches


tryMatch (Match _ pats expr) = do
  goLeftMost
  pairs <- matchPatsBase False pats
  case pairs of
    Left x -> return x
    Right pairs' -> do
      let substMap = Map.fromList (nstToList pairs')
      res <- useFuel

      -- only capture arguments that were utilized
      goLeftMost
      goUpN (length pats)

      s <- get
      writeFoc (foc s)

      case res of
        OutOfFuel -> return OutOfFuel
        _ -> do
          replaceExpr $ substitute substMap expr
          s' <- get
          writeFoc (foc s')
          return Success

-- must not leave spine
-- exact is a flag for whether or not number of args/pats in spine is
-- exactly the number of pats
-- this (probably) should have been EitherT reduceResult TreeS (Nested NameExpr)
-- ...
-- matchPats matches right leaves (like a, b) to given pats
matchPatsBase :: Bool -> [Pat] -> TreeS (Either ReduceResult (Nested NameExpr))
-- if no pats, then exact and not exact are good
matchPatsBase _ [] = return $ Right (Group [])

--     2:$
--     / \
--  1:$   b
--  / \
-- f   a

matchPatsBase exact pats@(_:_) = do
  s <- get
  if canGoRightUp s
  then do
    goFuncToArg -- f to a
    res <- matchPatsRec exact pats
    exitSpine -- exit b's spine
    return res
  else return $ Left None -- has pats to match, but no actual args

-- recursive version only jumps from a to b etc. caller exits spine of final arg
matchPatsRec :: Bool -> [Pat] -> TreeS (Either ReduceResult (Nested NameExpr))
matchPatsRec False [] = return $ Right (Group []) 
matchPatsRec True [] = do
  s <- get
  if canGoRightUp s
  then return $ Left None
  else return $ Right (Group [])

matchPatsRec exact (p : ps) = do
  matchP <- matchPat p
  case matchP of
    Left _ -> return matchP
    Right val -> do
      hasNextArg <- canGoArgToArg
      if hasNextArg
      then do
        goArgToArg
        rest <- matchPatsRec exact ps
        case rest of
          Left _ -> return $ Left None
          Right rest' -> return $ Right (Group [rest', val])
      else case ps of
        [] -> return matchP     -- job done
        _ -> return $ Left None -- there is not next argument

matchPat :: Pat -> TreeS (Either ReduceResult (Nested NameExpr))
matchPat (PVar name) = do
  e <- curExpr <$> get
  return $ Right (Value (name, e))

matchPat p@(PApp name pats) = do
  goLeftMost
  e <- curExpr <$> get
  case e of
    Con x -> do
      if x == name
      then matchPatsBase True pats -- constructor matches, now need args to match
      else return (Left None)
    Var _ -> do -- try to eval until WHNF
      res <- eval
      case res of
        OutOfFuel -> return $ Left OutOfFuel
        _ -> matchPat p -- still on the same spine, so should be fine

substitute substMap e = case e of
  l :$ r -> substitute substMap l :$ substitute substMap r
  Con _ -> e
  Var x -> Map.findWithDefault e x substMap
