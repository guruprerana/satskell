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
    , declevs = [(i, -1) | i <- (vars cnf)]
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

-- simple branching variable picked from the stack of unassigned variables
pickBranchingVariable :: State CLG (Maybe Int)
pickBranchingVariable = do
  clg <- get
  case unassigned clg of
    [] -> return Nothing
    i:_ -> return $ Just i

-- For two clauses ω_j and ω_k, for which there is a unique variable x such that one clause has a literal
-- x and the other has literal ¬x, resolution ω_j ω_k contains all the literals of ω_j and ω_k with
-- the exception of x and ¬x.
resolution :: Cls -> Cls -> Cls
resolution (BigOr ls1) (BigOr ls2) = BigOr ((ls1 ++ ls2) \\ [l, opl l])
  where
    l :: Lit
    l = head $ filter ((`elem` ls2) . opl) ls1
    opl :: Lit -> Lit
    opl (Lit i pol) = Lit i (not pol)

-- compute decision level of a variable
declevi :: CLG -> Var -> Int
declevi clg i = snd $ head $ filter (\(i', _) -> i == i') (declevs clg)

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
             (show $ map (declevi clg) litsDeclev))
        Just cls -> cls
    anteClsM = (ante clg) $ head litsDeclev
    litsDeclev =
      filter (\i -> (declevi clg i) == (declev clg) && (ante clg) i /= Nothing) $
      map (\(Lit i _) -> i) (literals cls)
    nLitsDeclev = length litsDeclev

-- computes lowest decision level of the vars in a given clause
lowestDecLevel :: CLG -> Cls -> Int
lowestDecLevel clg cls =
  minimum $ map (declevi clg) $ map (\(Lit i _) -> i) (literals cls)

-- compute and add the learned clause
conflictAnalysis :: Cls -> State CLG Int
conflictAnalysis cls = do
  clg <- get
  put (clg {clss = (learnedClause clg cls) : (clss clg)})
  clg' <- get
  return $ lowestDecLevel clg' $ head $ clss clg'

removeVal :: Var -> CLG -> CLG
removeVal i clg =
  clg
    { unassigned = i : (unassigned clg)
    , vals =
        \i' ->
          if i' == i
            then Undef
            else (vals clg) i'
    , ante =
        \i' ->
          if i' == i
            then Nothing
            else (ante clg) i'
    , declevs = tail $ declevs clg
    }

backtrack :: Int -> State CLG ()
backtrack d = do
  clg <- get
  case declevs clg of
    [] -> return ()
    (i, d'):_ ->
      if d' < d
        then return ()
        else case (d', (ante clg) i) of
               (d, Nothing) -> return () -- don't need to backtrack the last decision variable
               otherwise -> do
                 put (removeVal i clg)
                 backtrack d
                 return ()

-- loops until all the variables are assigned or there is a conflict which is unresolvable
assignVariables :: State CLG ()
assignVariables = do
  -- can explore with different heuristics for picking the branching variable
  mi <- pickBranchingVariable
  case mi of
    Nothing -> return () -- no more variables left to assign
    Just i -> do
      clg <- get
      put (clg {declev = (declev clg) + 1})
      -- we assign decision variables to be false by default
      -- can experiment with random assignment?
      inferLit (Lit i False, Nothing)
      unitProp
      clg1 <- get
      case unsatCls clg1 of
        Nothing
          -- no conflicts! go ahead and assign other variables
         -> do
          assignVariables
        Just cls -> do
          backtrackLevel <- conflictAnalysis cls
          if backtrackLevel < 0
            then return () -- unsat
              -- else we want to backtrack
            else do
              clg2 <- get
              put
                (clg2
                   { declev = backtrackLevel
                   , unsatCls = Nothing
                   , unitCls = Nothing
                   })
              backtrack backtrackLevel
              -- check and update unsatCls/unitCls because new ones could be created
              -- after the added clause
              clg3 <- get
              mapM checkCls $ clss clg3
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

constructSubst :: (Var -> VarVal) -> [Var] -> Subst
constructSubst vals vars = zip vars $ map (convert . vals) vars
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
