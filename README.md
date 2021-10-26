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

where `<method>` is one of `naive`, `backtracking`, `dpll`, or `cdcl`.

## Implementation remarks

- Each algorithm is written in separate modules found within `Solver/`.
- A few common functions are shared between the backtracking and DPLL implementations which are in the `Solver.Utils` module
- *Basic Backtracking*: Simple to write functionally!
- *DPLL*: Still pretty easy to implement unit propagation and pure literal elimination
- *CDCL*: With the DPLL implementation, we see that we have to pass around updated clauses and variables between functions which gets cumbersome very quickly especially when we have to deal with non-synchronous backtracking in CDCL. This is why we switch to a monadic style implementation of CDCL with the `State` monad in Haskell which helps us keep a track of the current implication graph and assignments by mutating it at different points.
- It is very interesting to note that with the monadic style, we can closely match imperative programming style which you can observe if you look at the `cdcl` and `assignVariables` functions in `Solver.CDCL` which almost correspond line by line to the pseudocode of the CDCL algorithm presented [here](https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf)

## Comparison of algoritms

We test our implementations against [Minisat](https://github.com/niklasso/minisat)!

TODO: Add table comparing timings of different algorithms vs. Minisat

## In the future?

1. Complete the implementation of the VSIDS branching heuristic
2. Quicker identification of unit clauses or unsatisfied conflict clauses by storing additional data within the state associated to each clause
3. Periodic search restarts and clause deletion

### References

1. For the implementation of the CDCL algorithm, the following resources were used
    - Chapter 4 of this [Handbook of Satisifiability](https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf)
    - Helped [visualize CDCL](https://cse442-17f.github.io/Conflict-Driven-Clause-Learning/).
    - [GRASP - A New Search Algorithm for Satisfiability](https://www.cs.cmu.edu/~emc/15-820A/reading/grasp_iccad96.pdf)
2. For the implementation of the VSIDS heuristic for branching these [lecture slides](https://baldur.iti.kit.edu/sat/files/2019/l08.pdf) are of great help

### Code formatting

We are using `hindent` to format our Haskell source code with the configuration file `.hindent.yaml`.
