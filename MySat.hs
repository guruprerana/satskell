import CNF
import CNF.DIMACS

import qualified Solver.Naive as Naive
import qualified Solver.Backtracking as Backtracking
import qualified Solver.DPLL as DPLL

import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  unless (length args == 2) $ do
    putStrLn ("Usage: " ++ name ++ " <cnf file>" ++ " <method>")
    putStrLn ("Available methods: naive, backtracking, dpll")
    exitFailure
  f <- readCNFfromDIMACS (args !! 0)
  case (args !! 1) of
    ("naive") -> (case Naive.solution f of
      Nothing  -> putStrLn "UNSAT"
      Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho))
    ("backtracking") -> (case Backtracking.solution f of
      Nothing  -> putStrLn "UNSAT"
      Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho))
    ("dpll") -> (case DPLL.solution f of
      Nothing  -> putStrLn "UNSAT"
      Just rho -> putStrLn ("SAT\n" ++ dimacsSubst rho))
    otherwise -> putStrLn ("invalid method")
