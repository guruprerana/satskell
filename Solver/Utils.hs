module Solver.Utils where

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

conditions :: [Lit] -> [Cls] -> [Cls]
conditions ls clss = foldr condition clss ls

conditionCNF :: Lit -> CNF -> CNF
conditionCNF l c = BigAnd (vars c) (condition l $ clauses c)

conditionsCNF :: [Lit] -> CNF -> CNF
conditionsCNF ls c = BigAnd (vars c) (conditions ls $ clauses c)

addLit :: Lit -> [Subst] -> [Subst]
addLit l sbsts = [(unLit l):sbst | sbst <- sbsts]

addLits :: [Lit] -> [Subst] -> [Subst]
addLits ls sbsts = foldr addLit sbsts ls

-- with the backtracking implementation, some variables are left out of substitutions
-- because any subtitution for these variables would work. we complete the substitutions
-- with the following routines
completeSbst :: [Var] -> Subst -> Subst
completeSbst [] sbst = sbst
completeSbst (x:xs) sbst = if ((x, True) `elem` sbst) || ((x, False) `elem` sbst)
                            then completeSbst xs sbst
                            else completeSbst xs ((x, True):sbst)

-- when we only have partial assignments, we can complete them
completeSolutions :: [Var] -> [Subst] -> [Subst]
completeSolutions vars sbsts = map (completeSbst vars) sbsts

-- checks if literal is present in any clause of frm
isLitIn :: CNF -> Lit -> Bool
isLitIn cnf l = or $ map ((l `elem`) . literals) $ clauses cnf
