# satskell

SAT in Haskell! In this project, we implement multiple algorithms to solve the Boolean Satisfiability problem. We have a basic **backtracking** algorithm, followed by a **DPLL** algorithm, and lastly the **CDCL** algorithm used in almost all of the state-of-the-art production grade SAT solvers (with additional optimizations on top of course!). Each of these is built in purely functional style using Haskell.

## Build

GHC (Glasgow Haskell Compiler) is a prerequisite to compile the program. You will also need to have the `mtl` Cabal package installed for which you can use `cabal install --lib mtl`.

You can use the `Makefile` to compile by running `make` from within the project directory.

## Running satskell

Satskell can solve SAT problems in the form of DIMACS files which is a file encoding for problems in CNF (Conjunctive Normal Form).

You can use the following command to run satskell and check the satisfiability of your CNF.

```
./MySat <dimacs-file-name> <method>
```

where `<method>` is one of `backtracking`, `dpll`, or `cdcl`.

## Comparison of algoritms

We test our implementations against [Minisat](https://github.com/niklasso/minisat)!

## In the future?

1. Complete the implementation of the VSIDS branching heuristic
2. Quicker identification of unit clauses or unsatisfied conflict clauses by storing additional data within the state associated to each clause
3. Periodic search restarts and clause deletion

### References

1. For the implementation of the CDCL algorithm, the following resources were used
    - Chapter 4 of this [Handbook of Satisifiability](https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf)
    - Helped [visualize CDCL](https://cse442-17f.github.io/Conflict-Driven-Clause-Learning/).
    - [GRASP - A New Search Algorithm for Satisfiability](https://www.cs.cmu.edu/~emc/15-820A/reading/grasp_iccad96.pdf)
2. For the implementation of the VSIDS heuristic for branching these [lecture slides](https://baldur.iti.kit.edu/sat/files/2019/l08.pdf) were of great help
