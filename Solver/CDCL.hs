module Solver.CDCL
  ( solution
  ) where

import CNF

import Control.Monad.State

import Data.Heap as Heap (MaxPrioHeap, empty, filter, fromList, insert, view)

-- import CNF.Eval
-- import Solver.Utils
import Data.List
import Data.Map as Map (Map, findWithDefault, fromAscList, insert)
import Data.Maybe

-- the current assignement for a var can be either Zero (False), undefined, or One (True)
data VarVal
  = Zero
  | Undef
  | One
  deriving (Show, Eq)

switchVarVal :: VarVal -> VarVal
switchVarVal Zero = One
switchVarVal Undef = Undef
switchVarVal One = Zero

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
    { varsHeap :: Heap.MaxPrioHeap Int Var
    , varScores :: Map.Map Var Int
      -- function associating a value to each var
    , vals :: Map.Map Var VarVal
      -- antecedent clause of each var
    , ante :: Map.Map Var (Maybe Cls)
      -- current decision level in the state
    , declev :: Int
      -- declaration level of variables arranged in desc. order or level
    , declevs :: [(Var, Int)]
      -- additional map storing same data as declevs for faster access to devlev of a var
    , declevVar :: Map.Map Var Int
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

-- for VSIDS, calculate initial score of a variable by counting number of occurences in clauses
initScore :: CNF -> Var -> Int
initScore cnf i =
  length $
  Prelude.filter
    (\(BigOr ls) -> (Lit i True) `elem` ls || (Lit i False) `elem` ls) $
  clauses cnf

-- initializes the CLG with the initial required state given a CNF
initialCLG :: CNF -> CLG
initialCLG cnf =
  CLG
    { varsHeap = Heap.fromList $ zip (map (initScore cnf) (vars cnf)) (vars cnf)
    , varScores =
        Map.fromAscList $ zip (vars cnf) (map (initScore cnf) (vars cnf))
    , vals = Map.fromAscList [(v, Undef) | v <- (vars cnf)]
    , ante = Map.fromAscList [(v, Nothing) | v <- (vars cnf)]
    , declev = 0
    , declevs = [(i, -1) | i <- (vars cnf)]
    , declevVar = Map.fromAscList [(v, -1) | v <- (vars cnf)]
    , ncls = length $ clauses cnf
    , clss = clauses cnf
    , clscharac = \_ -> Unres
    , unsatCls = Nothing
    , unitCls =
        case Prelude.filter (\(BigOr ls) -> length ls == 1) $ clauses cnf of
          [] -> Nothing
          cls@(BigOr ls):_ -> Just (head ls, cls)
    }

-- evaluates a literal given a CLG which contains the valuation function vals
evalLit :: CLG -> Lit -> Bool
evalLit clg (Lit i pol) =
  case (Map.findWithDefault Undef i (vals clg), pol) of
    (Zero, False) -> True
    (One, True) -> True
    otherwise -> False

-- evaluates a clause given a CLG which contains the valuation function vals
evalCls :: CLG -> Cls -> Bool
evalCls clg (BigOr ls) = or $ map (evalLit clg) ls

-- list of undefined lits in a clause
undefLitsCls :: CLG -> Cls -> [Lit]
undefLitsCls clg (BigOr ls) =
  Prelude.filter (\(Lit i _) -> (Map.findWithDefault Undef i (vals clg)) == Undef) ls

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
    { vals = Map.insert i (if pol then One else Zero) (vals clg)
    , ante = Map.insert i cls (ante clg)
    , declevs = (i, declev clg) : (declevs clg)
    , declevVar = Map.insert i (declev clg) (declevVar clg)
    , unitCls = Nothing
    , varsHeap = Heap.filter (\(_, i') -> i' /= i) (varsHeap clg)
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
  clg' <- get
  mapM checkCls $ clss clg'
  return ()

-- repeatedly looks for and resolves wrt unit clauses
unitProp :: State CLG ()
unitProp = do
  clg <- get
  -- if we have an unsatisfied clause, then we have a conflict!
  case unsatCls clg of
    Just cls -> return ()
    Nothing -> do
      case unitCls clg of
        Nothing -> return ()
        Just (l, cls) -> do
          inferLit (l, Just cls)
          unitProp
          return ()

-- simple branching variable picked from the priority heap of unassigned variables
pickBranchingVariable :: State CLG (Maybe Var)
pickBranchingVariable = do
  clg <- get
  case Heap.view $ varsHeap clg of
    Nothing -> return Nothing
    Just ((prio, var), tail)
      --put (clg {varsHeap = tail})
     -> do
      return $ Just var

-- For two clauses ω_j and ω_k, for which there is a unique variable x such that one clause has a literal
-- x and the other has literal ¬x, resolution ω_j ω_k contains all the literals of ω_j and ω_k with
-- the exception of x and ¬x.
resolution :: Cls -> Cls -> Cls
resolution (BigOr ls1) (BigOr ls2) = BigOr ((union ls1 ls2) \\ [l, opl l])
  where
    l :: Lit
    l = head $ Prelude.filter ((`elem` ls2) . opl) ls1
    opl :: Lit -> Lit
    opl (Lit i pol) = Lit i (not pol)

-- compute decision level of a variable
declevOf :: CLG -> Var -> Int
declevOf clg i = Map.findWithDefault (-1) i (declevVar clg)

-- compute the learned clause given the conflict clause by recursive resolution
learnedClause :: CLG -> Cls -> Cls
learnedClause clg cls =
  if nLitsDeclev > 0
    then (learnedClause clg (resolution cls anteCls))
    else cls
  where
    anteCls =
      case anteClsM of
        Nothing ->
          error
            ("ANTECLS NOTHING: not possible in the usual flow " ++
             (show $ map (declevOf clg) litsDeclev))
        Just cls -> cls
    anteClsM = Map.findWithDefault Nothing (head litsDeclev) (ante clg)
    litsDeclev =
      Prelude.filter
        (\i -> (declevOf clg i) == (declev clg) && (Map.findWithDefault Nothing i (ante clg)) /= Nothing) $
      map (\(Lit i _) -> i) (literals cls)
    nLitsDeclev = length litsDeclev

-- computes lowest decision level of the vars in a given clause
maxDecLevel :: CLG -> Cls -> Int
maxDecLevel clg cls =
  case Prelude.filter (\d -> d /= (declev clg)) $
       map (declevOf clg) $
       Prelude.filter (\i -> (Map.findWithDefault Nothing i (ante clg)) == Nothing) $
       map (\(Lit i _) -> i) (literals cls) of
    [] -> (declev clg) - 1
    ls -> maximum ls

-- compute and add the learned clause
conflictAnalysis :: Cls -> State CLG Int
conflictAnalysis cls = do
  clg <- get
  put (clg {clss = (learnedClause clg cls) : (clss clg)})
  clg' <- get
  return $ maxDecLevel clg' $ head $ clss clg'

removeVal :: Var -> CLG -> CLG
removeVal i clg =
  clg
    { varsHeap =
        Heap.insert (Map.findWithDefault 0 i (varScores clg), i) (varsHeap clg)
    , vals = Map.insert i Undef (vals clg)
    , ante = Map.insert i Nothing (ante clg)
    , declevs = tail $ declevs clg
    , declevVar = Map.insert i (-1) (declevVar clg)
    }

incrementScoresLits :: [Lit] -> Map.Map Var Int -> Map.Map Var Int
incrementScoresLits [] scores = scores
incrementScoresLits ((Lit i _):ls) scores =
  incrementScoresLits ls $
  Map.insert i ((Map.findWithDefault 0 i scores) + 1) scores

-- takes the conflict clause and increments the scores for variables in the conflict clause
-- for the VSIDS heuristic
incrementScores :: Cls -> State CLG ()
incrementScores cls = do
  clg <- get
  put (clg {varScores = incrementScoresLits (literals cls) (varScores clg)})

-- backtracks to decision level d without changing the decision assignment at level d
backtrack :: Int -> State CLG ()
backtrack d = do
  clg <- get
  case declevs clg of
    [] -> return ()
    (i, d'):_ ->
      if d' < d
        then return ()
        else if (d' == d && (Map.findWithDefault Nothing i (ante clg)) == Nothing)
               then return () -- we do not need to change anything at the decision assignment
               else do
                 put (removeVal i clg)
                 backtrack d
                 return ()

-- UNUSED : SWITCHES VALUE OF A VARIABLE used in normal backtracking
switchDec :: Var -> State CLG ()
switchDec i = do
  clg <- get
  put
    (clg
       { vals = Map.insert i One (vals clg)
       })

checkCLG :: State CLG ()
checkCLG = do
  unitProp
  clg <- get
  case unsatCls clg of
    Nothing
      -- no conflicts! go ahead and assign other variables
     -> do
      assignVariables
    Just cls
      -- we have a conflict clause so increment scores
     -> do
      incrementScores cls
      backtrackLevel <- conflictAnalysis cls
      if backtrackLevel <= 0
        then return () -- unsat
          -- else we want to backtrack
        else do
          clg1 <- get
          put
            (clg1
               {declev = backtrackLevel, unsatCls = Nothing, unitCls = Nothing})
          backtrack backtrackLevel
          -- check and update unsatCls/unitCls because new ones could be created
          -- after the added clause
          clg2 <- get
          mapM checkCls $ clss clg2
          checkCLG

-- loops until all the variables are assigned or there is a conflict which is unresolvable
assignVariables :: State CLG ()
assignVariables = do
  mi <- pickBranchingVariable
  -- can explore with different heuristics for picking the branching variable
  case mi of
    Nothing -> return () -- no more variables left to assign
    Just i -> do
      clg <- get
      put (clg {declev = (declev clg) + 1})
      -- we assign decision variables to be false by default
      -- can experiment with random assignment?
      inferLit (Lit i False, Nothing)
      checkCLG

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

constructSubst :: (Map.Map Var VarVal) -> [Var] -> Subst
constructSubst vals vars = zip vars $ map (convert . (\i -> Map.findWithDefault Zero i vals)) vars
  where
    convert :: VarVal -> Bool
    convert One = True
    convert Zero = False
    convert Undef = error ("UNDEF VAR while constructing substitution")

getSolution :: [Var] -> CLG -> Maybe Subst
getSolution vars clg =
  case unsatCls clg of
    Nothing -> Just $ constructSubst (vals clg) vars
    Just _ -> Nothing

solution :: CNF -> Maybe Subst
solution cnf = getSolution (vars cnf) $ snd $ runState cdcl $ initialCLG cnf
