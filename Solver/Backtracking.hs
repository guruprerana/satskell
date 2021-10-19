module Solver.Backtracking (solution) where

import CNF
import CNF.Eval

import Data.List
import Data.Maybe

condition :: Lit -> [Cls] -> [Cls]
condition _ [] = []
condition l (cls:clss) = if l `elem` (literals cls) then 
                          condition l clss else (fcls:condition l clss)
                            where
                              fcls  = BigOr $ filter (/=opl) $ literals cls
                              opl   = Lit (var l) (not $ pol l)

conditionCNF :: Lit -> CNF -> CNF
conditionCNF l c = BigAnd (vars c) (condition l $ clauses c)

addLit :: Lit -> [Subst] -> [Subst]
addLit l sbsts = [(unLit l):sbst | sbst <- sbsts]

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

-- with the above backtracking implementation, some variables are left out of substitutions
-- because any subtitution for these variables would work. we complete the substitutions
-- with the following routines

completeSbst :: [Var] -> Subst -> Subst
completeSbst [] sbst = sbst
completeSbst (x:xs) sbst = if ((x, True) `elem` sbst) || ((x, False) `elem` sbst)
                          then completeSbst xs sbst
                          else completeSbst xs ((x, True):sbst)

completeSolutions :: CNF -> [Subst]
completeSolutions cnf = map (completeSbst $ vars cnf) $ solutions cnf

-- we also add an extra filter to cross check we only have correct solutions (for testing)
solution :: CNF -> Maybe Subst
solution frm = case filter (\rho -> evalCNF rho frm) $ completeSolutions frm of
                []      -> Nothing
                (rho:_) -> Just rho
