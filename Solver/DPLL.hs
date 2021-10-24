module Solver.DPLL
  ( solution
  ) where

import CNF
import CNF.Eval
import Solver.Utils

import Data.List
import Data.Maybe

-- performs unit propagation on the cnf
unitProp :: CNF -> (CNF, [Subst] -> [Subst])
unitProp cnf = (conditionsCNF ls cnf, addLits ls)
  where
    ls :: [Lit]
    ls = map firstLit $ filter (\(BigOr lits) -> length lits == 1) $ clauses cnf
    firstLit :: Cls -> Lit
    firstLit = head . literals

solutions :: CNF -> [Subst]
solutions cnf =
  case clauses $ fst $ unitProp cnf of
    [] -> [[]] -- trivially satisfiable
    clss ->
      case (BigOr []) `elem` clss of
        True -> [] -- empty conjunction unsatisfiable
        False ->
          (unitP $ addLit l $ solutions $ conditionCNF l cnf') ++
          (if needed
             then (unitP $ addLit opl $ solutions $ conditionCNF opl cnf')
             else [])
          where l = head $ literals $ head clss
                opl = Lit (var l) (not $ pol l)
                (cnf', unitP) = unitProp cnf
                needed :: Bool
                needed = isLitIn cnf' opl

-- we also add an extra filter to cross check we only have correct solutions (for testing)
solution :: CNF -> Maybe Subst
solution frm =
  case filter (\rho -> evalCNF rho frm) $
       completeSolutions (vars frm) $ solutions frm of
    [] -> Nothing
    (rho:_) -> Just rho
