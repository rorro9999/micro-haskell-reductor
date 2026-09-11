module Compute
  ( printPath
  ) where

import Types
import Parse
import qualified Data.Map as Map
--import Control.Monad.State (State, runState, state, execState, get, put)
import Control.Monad.State.Strict (State, runState, state, execState, put)
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
  hst = hist ts

-- eval is the owner of the entire spine, no matter where is it called
-- case for spine owner is roughly: try to reduce function,
-- if succeeded, call eval again
-- if not, eval leafs
-- (doesn't leave spine)

eval :: TreeS ReduceResult
eval = do
  s <- get
  case curExpr s of
    Con _ -> return None -- nothing to do
    _ -> do              -- both Var and :$ cases
      goLeftMost
      res <- tryReduction
      case res of
        Success -> eval -- keep reducing spine while possible
        OutOfFuel -> return OutOfFuel
        None -> do      -- spine is "dead", so reduce args
          goLeftMost
          evalRights

--     2:$
--     / \
--  1:$   b
--  / \
-- f   a
evalRights = do
  s <- get
  if canGoRightUp s
  then do   -- spine is not in the final argument
    goUp    -- either take path f (1:$) a (1:$) evalRight
    goRight -- or (1:$) (2:$) b (2:$) evalRight
    res <- eval
    goUp
    case res of
      OutOfFuel -> goRightMost >> return OutOfFuel -- propagate fuel error
      _ -> evalRights -- reduce rest of args
  else return None


-- tryReduction tries to reduce the spine.
-- (doesn't leave spine)
tryReduction = do
  s <- get
  case curExpr s of
   Con _ -> return None
   Var v -> do
    case Map.lookup v (df s) of
     Nothing -> return None
     Just (Def matches) -> do
     -- try tries matches, until first works
       let try [] = return None
           try (x:xs) = do
             res <- tryMatch x
             case res of
               None -> try xs
               y -> return y -- propagates both OutOfFuel and Success cases
       try matches


-- (doesn't leave spine)
tryMatch (Match _ pats expr) = do
  goLeftMost
  pairs <- matchPatterns False pats
  case pairs of
    Left x -> return x
    Right pairs' -> do
      let substMap = Map.fromList (nstToList pairs')
      res <- useFuel
      s <- get
      writeFoc (foc s) -- matchPatterns leaves with expression in focus

      case res of
        OutOfFuel -> return OutOfFuel
        _ -> do
          replaceExpr $ substitute substMap expr
          return Success

--    2:$
--    / \
--  1:$  b
--  / \
-- f   a
-- matchPatterns recursive case is like moving from f to a, or (1:$) to b
-- matchPatterns leaves having focus on the entire substitued expression
-- (doesn't leave spine)

-- exact is a flag for whether or not number of args/pats in spine is
-- exactly the number of pats
matchPatterns :: Bool -> [Pat] -> TreeS (Either ReduceResult (Nested NameExpr))
matchPatterns exact [] =
  if exact
  then do
    s <- get
    if canGoRightUp s
    then return $ Left None -- there are still more args to match
    else return $ Right (Group [])
  else return $ Right (Group [])

matchPatterns exact (p:ps) = do
  s <- get
  if canGoRightUp s
  then do
    goUp
    -- the only possible Expr constructor that let's you go up is :$
    -- which also let's you go to the right
    goRight
    res <- matchPat p
    case res of
      Left x -> return res
      Right nst -> do
        rest <- matchPatterns exact ps
        case rest of
          Left _ -> return rest
          Right rest' -> return $ Right (Group [nst, rest'])
  else return $ Left None

-- leaving the spine, to return to recursive case in matchPatterns
-- (leaves spine!!!)
matchPat :: Pat -> TreeS (Either ReduceResult (Nested NameExpr))
matchPat (PVar name) = do
  e <- curExpr <$> get
  goUp
  return $ Right (Value (name, e))

matchPat p@(PApp name pats) = do
  goLeftMost
  s <- get
  case curExpr s of
    Con x -> do
      if x == name
      then matchPatterns True pats -- constructor matches, now need args to match
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
