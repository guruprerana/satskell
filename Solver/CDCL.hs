module Solver.CDCL
  ( solution
  ) where

import CNF

import Control.Monad.State

-- import CNF.Eval
-- import Solver.Utils
import Data.List
import Data.Maybe

-- the current assignement for a var can be either Zero (False), undefined, or One (True)
data VarVal
  = Zero
  | Undef
  | One
  deriving (Show, Eq)

-- a clause is either unsatified, satisfied, unit, or unresolved
data ClsCharac
  = Unsat
  | Sat
  | Unit
  | Unres
  deriving (Show, Eq)

data CLG =
  CLG
      -- just a heap of unassigned vars
    { unassigned :: [Var]
      -- function associating a value to each var
    , vals :: Var -> VarVal
      -- antecedent clause of each var
    , ante :: Var -> Maybe Cls
      -- current decision level in the state
    , declev :: Int
      -- declaration level of variables arranged in desc. order or level
    , declevs :: [(Var, Int)]
      -- number of clauses
    , ncls :: Int
      -- clauses of the CNF
    , clss :: [Cls]
      -- associates each clause with a characterization
      -- the input integer i is assumed 0 <= i < ncls
      -- UNUSED FOR THE MOMENT
    , clscharac :: Int -> ClsCharac
      -- when we perform unit propagation and encounter an unsatified clause
      -- we assign this record to the unsat clause
    , unsatCls :: Maybe Cls
      -- an available unit clause if there exists one
    , unitCls :: Maybe (Lit, Cls)
    }

-- initializes the CLG with the initial required state given a CNF
initialCLG :: CNF -> CLG
initialCLG cnf =
  CLG
    { unassigned = vars cnf
    , vals = \_ -> Undef
    , ante = \_ -> Nothing
    , declev = -1
    , declevs = []
    , ncls = length $ clauses cnf
    , clss = clauses cnf
    , clscharac = \_ -> Unres
    , unsatCls = Nothing
    , unitCls =
        case filter (\(BigOr ls) -> length ls == 1) $ clauses cnf of
          [] -> Nothing
          cls@(BigOr ls):_ -> Just (head ls, cls)
    }

-- evaluates a literal given a CLG which contains the valuation function vals
evalLit :: CLG -> Lit -> Bool
evalLit clg (Lit i pol) =
  case ((vals clg) i, pol) of
    (Zero, False) -> True
    (One, True) -> True
    otherwise -> False

-- evaluates a clause given a CLG which contains the valuation function vals
evalCls :: CLG -> Cls -> Bool
evalCls clg (BigOr ls) = or $ map (evalLit clg) ls

-- list of undefined lits in a clause
undefLitsCls :: CLG -> Cls -> [Lit]
undefLitsCls clg (BigOr ls) = filter (\(Lit i _) -> ((vals clg) i) == Undef) ls

-- characterizes a clause and also returns a literal if it is a unit clause
characCls :: CLG -> Cls -> (ClsCharac, Maybe Lit)
characCls clg cls =
  case evalCls clg cls of
    True -> (Sat, Nothing)
    False ->
      case undefLitsCls clg cls of
        [] -> (Unsat, Nothing)
        l:[] -> (Unit, Just l)
        otherwise -> (Unres, Nothing)

-- updates the CLG by conditioning with a literal which appears in a clause cls
updateVals :: (Lit, Maybe Cls) -> CLG -> CLG
updateVals ((Lit i pol), cls) clg =
  clg
    { unassigned = filter (/= i) $ unassigned clg
    , vals =
        \i' ->
          if i' == i
            then (if pol
                    then One
                    else Zero)
            else (vals clg) i'
    , ante =
        \i' ->
          if i' == i
            then cls
            else (ante clg) i'
    , declevs = (i, declev clg) : (declevs clg)
    , unitCls = Nothing
    }

-- given a clause, it updates the clg after checking the clause
-- if the clause is a unit clause, we update the unitCls record
-- if the clause is unsat wrt the current assignment, we update the unsatCls record
checkCls :: Cls -> State CLG ()
checkCls cls = do
  clg <- get
  case characCls clg cls of
    (Unit, Just l) -> do
      put (clg {unitCls = Just (l, cls)})
      return ()
    (Unsat, _) -> do
      put (clg {unsatCls = Just cls})
      return ()
    otherwise -> return ()

-- performs the conditioning of a given variable assignment and also checks for
-- unsatisfied/unit clauses
inferLit :: (Lit, Maybe Cls) -> State CLG ()
inferLit (l, cls) = do
  clg <- get
  put (updateVals (l, cls) clg)
  -- go through each clause and check it with checkCls
  mapM checkCls $ clss clg
  return ()

-- repeatedly looks for and resolves wrt unit clauses
unitProp :: State CLG ()
unitProp = do
  clg <- get
  -- if we have an unsatisfied clause, then we have a conflict!
  case (length $ unassigned clg, unsatCls clg) of
    (_, Just cls) -> return ()
    (0, Nothing) -> return ()
    (_, Nothing) -> do
      case unitCls clg of
        Nothing -> return ()
        Just (l, cls) -> do
          inferLit (l, Just cls)
          unitProp
          return ()

-- simple branching variable picked from the stack of unassigned variables
pickBranchingVariable :: State CLG (Maybe Int)
pickBranchingVariable = do
  clg <- get
  case unassigned clg of
    [] -> return Nothing
    i:_ -> return $ Just i

conflictAnalysis :: Cls -> State CLG Int
conflictAnalysis = undefined

backtrack :: Int -> State CLG ()
backtrack = undefined

-- loops until all the variables are assigned or there is a conflict which is unresolvable
assignVariables :: State CLG ()
assignVariables = do
  clg <- get
  -- can explore with different heuristics for picking the branching variable
  mi <- pickBranchingVariable
  case mi of
    Nothing -> return () -- no more variables left to assign
    Just i -> do
      put (clg {declev = (declev clg) + 1})
      -- we assign decision variables to be false by default
      -- can experiment with random assignment?
      inferLit (Lit i False, Nothing)
      unitProp
      case unsatCls clg of
        Nothing -> do
          -- no conflicts! go ahead and assign other variables
          assignVariables
        Just cls -> do
          backtrackLevel <- conflictAnalysis cls
          if backtrackLevel < 0
            then return () -- unsat
            else do
              -- else backtrack
              put (clg {declev = backtrackLevel})
              backtrack backtrackLevel
              unitProp
              assignVariables
              return ()

cdcl :: State CLG ()
cdcl = do
  unitProp
  clg <- get
  case unsatCls clg of
    Just _ -> return () -- unsat
    Nothing -> do
      put (clg {declev = 0})
      assignVariables
      return ()

solution :: CNF -> Maybe Subst
solution = undefined
