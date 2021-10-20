module Solver.DPLL (solution) where

import CNF
import CNF.Eval
import Solver.Utils

import Data.List
import Data.Maybe

-- performs unit propagation on the cnf
UnitProp :: CNF -> (CNF, [Subst] -> [Subst])
UnitProp cnf = undefined

solutions :: CNF -> [Subst]
solutions cnf = case clauses cnf of
                  []    -> [[]] -- trivially satisfiable
                  clss  -> case (BigOr []) `elem` clss of
                            True  -> [] -- empty conjunction unsatisfiable
                            False -> (addLit l $ solutions $ conditionCNF l cnf) 
                                      ++ (addLit opl $ solutions $ conditionCNF opl cnf)
                                        where
                                          l   = head $ literals $ head clss
                                          opl  = Lit (var l) (not $ pol l)

-- we also add an extra filter to cross check we only have correct solutions (for testing)
solution :: CNF -> Maybe Subst
solution frm = case filter (\rho -> evalCNF rho frm) $ completeSolutions (vars frm) $ solutions frm of
                []      -> Nothing
                (rho:_) -> Just rho
