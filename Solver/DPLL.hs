module Solver.DPLL
  ( solution
  ) where

import CNF
import CNF.Eval
import Solver.Utils

import Data.List
import Data.Maybe

unitCls :: CNF -> [Lit]
unitCls cnf =
  map firstLit $ filter (\(BigOr lits) -> length lits == 1) $ clauses cnf
  where
    firstLit = head . literals

solutions :: CNF -> [Subst]
solutions cnf =
  case clauses cnf of
    [] -> [[]] -- trivially satisfiable
    clss ->
      case (BigOr []) `elem` clss of
        True -> [] -- empty conjunction unsatisfiable
        False ->
          case unitCls cnf of
            [] ->
              (addLit l $ solutions $ conditionCNF l cnf) ++
              (if needed
                 then (addLit opl $ solutions $ conditionCNF opl cnf)
                 else [])
              where l = head $ literals $ head clss
                    opl = Lit (var l) (not $ pol l)
                    needed = isLitIn cnf opl
            uls -> addLits uls $ solutions $ conditionsCNF uls cnf

-- we also add an extra filter to cross check we only have correct solutions (for testing)
solution :: CNF -> Maybe Subst
solution frm =
  case completeSolutions (vars frm) $ solutions frm of
    [] -> Nothing
    (rho:_) -> Just rho
