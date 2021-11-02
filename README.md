# satskell

SAT in Haskell! In this project, we implement multiple algorithms to solve the Boolean Satisfiability problem. We have a basic **backtracking** algorithm, followed by a **DPLL** algorithm, and lastly the **CDCL** algorithm with the **VSIDS** branching heuristic (which is also the standard algorithm used in almost all of the state-of-the-art SAT solvers). Each of these is built in purely functional style using Haskell.

Along with the *CDCL* implementation, we include a method to add **watched literal tracking** to clauses for quicker identification of unit clauses, however the implementation we have is still quite slow and in fact does not speed up our *CDCL* method.

## Build

GHC (Glasgow Haskell Compiler) is a prerequisite to compile the program. You will also need to have the `mtl` Cabal package installed for which you can use `cabal install --lib mtl`.

You can use the `Makefile` to compile by running `make` from within the project directory.

## Running satskell

Satskell can solve SAT problems in the form of DIMACS files which is a file encoding for problems in CNF (Conjunctive Normal Form).

You can use the following command to run satskell and check the satisfiability of your CNF.

```
./MySat <dimacs-file-name> <method>
```

where `<method>` is one of `naive`, `backtracking`, `dpll`, `cdcl`, `cdclwl`.

Note: `cdclwl` refers to the CDCL method along with watched literals. However with our current implementation, it is (unfortunately) much slower than the simple `cdcl` method.

## Implementation remarks

- Each algorithm is written in separate modules found within `Solver/`.
- A few common functions are shared between the backtracking and DPLL implementations which are in the [`Solver.Utils`](https://github.com/guruprerana/satskell/blob/master/Solver/Utils.hs) module
- [*Basic Backtracking*](https://github.com/guruprerana/satskell/blob/master/Solver/Backtracking.hs): Simple to write functionally!
- [*DPLL*](https://github.com/guruprerana/satskell/blob/master/Solver/DPLL.hs): Still pretty easy to implement unit propagation and pure literal elimination
- [*CDCL*](https://github.com/guruprerana/satskell/blob/master/Solver/CDCL.hs): With the DPLL implementation, we see that we have to pass around updated clauses and variables between functions which gets cumbersome very quickly especially when we have to deal with non-chronological backtracking in CDCL. This is why we switch to a monadic style implementation of CDCL with the `State` monad in Haskell which helps us keep a track of the current implication graph and assignments by mutating the global state at different points. 
- The `State` monad also facilitates us to store additional structures within the state in order to boost our performance. For example, we store scores associated to each variable to implement the VSIDS branching heuristic.
- It is very interesting to note that with the monadic style, we can closely match imperative programming style which you can observe if you look at the `cdcl` and `assignVariables` functions in `Solver.CDCL` which almost correspond line by line to the pseudocode of the CDCL algorithm presented [here](https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf)

The pseudocode presented looks like this:

```
CDCL(ϕ, ν)
    if (UnitPropagation(ϕ, ν) == CONFLICT)
        then return UNSAT
    dl <- 0 (Decision level)
    while (not AllVariablesAssigned(ϕ, ν))
        do (x, v) = PickBranchingVariable(ϕ, ν) (Decide stage)
            dl <- dl + 1 (Increment decision level due to new decision)
            ν <- ν ∪ {(x, v)}
            if (UnitPropagation(ϕ, ν) == CONFLICT) (Deduce stage)
                then β = ConflictAnalysis(ϕ, ν) (Diagnose stage)
                     if (β < 0)
                        then return UNSAT
                        else Backtrack(ϕ, ν, β)
                             dl <- β (Decrement decision level due to backtracking)
    return SAT
```

In comparison, our code looks like

```hs
cdcl :: State CLG ()
cdcl = do
  unitProp -- try unit propogation
  clg <- get
  case unsatCls clg of
    Just _ -> return () -- if we have an unsatisfiable clause, then it is UNSAT
    Nothing -> do -- if not, then we can go ahead and start assigning variables
      put (clg {declev = 0})
      assignVariables
      return ()

-- loops until all the variables are assigned or there is a conflict which is unresolvable
assignVariables :: State CLG ()
assignVariables = do
  mi <- pickBranchingVariable
  -- can explore with different heuristics for picking the branching variable - cuurently using VSIDS
  case mi of
    Nothing -> return () -- no more variables left to assign so exit
    Just i -> do
      clg <- get
      put (clg {declev = (declev clg) + 1})
      -- we assign chosen decision variables to be false by default
      -- can experiment with random assignment?
      inferLit (Lit i False, Nothing)
      checkCLG

checkCLG :: State CLG ()
checkCLG = do
  unitProp -- after choosing decision variable try unit propogation
  clg <- get
  case unsatCls clg of -- check if we have unsatisfiable (conflict) clauses
    Nothing
      -- no conflicts! go ahead and assign other variables
     -> do
      assignVariables
    Just cls
      -- we have a conflict clause so increment scores
     -> do
      incrementScores cls -- for the VSIDS heuristic, increment scores of variables in conflict clause
      backtrackLevel <- conflictAnalysis cls
      if backtrackLevel <= 0
        then return () -- UNSAT
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
```

Notice the almost imperative implementation here which mimics the pseudocode shown earlier in a very similar fashion.

### Other smaller optimizations to make CDCL (slightly) faster

- In order to implement the *VSIDS* branching heuristic, we need to pick the variable with the highest score at each iteration of the variable assignment loop. For this, we employ [max priority heaps](https://github.com/guruprerana/satskell/blob/e6cf3ae56ca05c33be184c760ad4aee99adb6914/Solver/CDCL.hs#L42) which are available to us with the `Data.Heap` module.
- We use [integer maps](https://github.com/guruprerana/satskell/blob/e6cf3ae56ca05c33be184c760ad4aee99adb6914/Solver/CDCL.hs#L43) from the `Data.IntMap` module wherever possible instead of `Data.List` or `Data.Map` since it is faster than both of the others as it makes use of binary tries. For `Data.List`, we have a linear complexity in accessing elements and a logarithmic complexity with `Data.Map` which uses binary trees.
- Note that it is hard to implement a purely functional constant-time read and write arrays unless perhaps using a hash map. 

## Comparison of algorithms

We test our implementations against [Minisat](https://github.com/niklasso/minisat) by running each of our different methods and timing them on our test files.

We use the following settings to run Minisat: `minisat <cnf-file> -no-luby -rinc=1.5 -phase-saving=0 -rnd-freq=0.02`.

| **N** |    **Filename**   | **backtracking** | **dpll** | **cdcl** + **VSIDS heuristic** | **minisat** |
|:-----:|:-----------------:|:----------------:|:--------:|:------------------------------:|:-----------:|
|   1   |  `colorK5_4.cnf`  |      0.039s      |  0.012s  |             0.036s             |    0.005s   |
|   2   |  `colorK8_7.cnf`  |      1.976s      |  0.548s  |             8.469s             |    0.034s   |
|   3   | `colorK15_15.cnf` |      0.142s      |  0.107s  |             0.228s             |    0.007s   |
|   4   |    `ninec.cnf`    |      0.015s      |  0.015s  |             0.015s             |    0.008s   |
|   5   | `notdichot20.cnf` |     Too long!    |  0.031s  |             0.074s             |    0.007s   |
|   6   |     `php4.cnf`    |      0.017s      |  0.010s  |             0.010s             |    0.009s   |
|   7   |     `php5.cnf`    |      0.047s      |  0.012s  |             0.014s             |    0.007s   |
|   8   |     `php7.cnf`    |      0.248s      |  0.044s  |             0.646s             |    0.015s   |
|   9   |     `php9.cnf`    |      14.517s     |**1.790s**|          **Too long!**         |    0.226s   |
|   10  |      `r.cnf`      |      0.006s      |  0.010s  |             0.010s             |    0.004s   |
|   11  |   `random5.cnf`   |      0.011s      |  0.010s  |             0.010s             |    0.007s   |
|   12  |  `random60a.cnf`  |     Too long!    |  0.119s  |             0.095s             |    0.007s   |
|   13  |  `random60b.cnf`  |     Too long!    |  0.137s  |             0.289s             |    0.009s   |
|   14  |   `dubois20.cnf`  |     Too long!    | 51.470s  |           **1.504s**           |    0.007s   |
|   15  |   `dubois22.cnf`  |     Too long!    | 12.158s  |           **2.896s**           |    0.005s   |
|   16  |  `aim-100....cnf` |     Too long!    |  3.181s  |             0.052s             |    0.005s   |
|   17  |  `bf0432-007.cnf` |     Too long!    | **>3m**  |          **1m1.368s**          |    0.017s   |

## In the future?

1. Improve the watched literals implementation by also storing a map from literals to the clauses in which it appears - by storing this additional map in the state. This is easy to do with the `State` monad that we have adopted! 
2. Periodic search restarts and clause deletion to prevent blowup of generated clauses in CDCL. You can observe that `php9.cnf` is very slow with CDCL. This is mainly due to the blowup in the number of clauses.

### References

1. For the implementation of the CDCL algorithm, the following resources were used
    - Chapter 4 of this [Handbook of Satisifiability](https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf)
    - Helped [visualize CDCL](https://cse442-17f.github.io/Conflict-Driven-Clause-Learning/).
    - [GRASP - A New Search Algorithm for Satisfiability](https://www.cs.cmu.edu/~emc/15-820A/reading/grasp_iccad96.pdf)
2. For the implementation of the VSIDS heuristic for branching these [lecture slides](https://baldur.iti.kit.edu/sat/files/2019/l08.pdf) could be of great help
3. Gives a great overview of everything one can have in a SAT Solver: [The Quest for Efficient Boolean Satisfiability Solvers](https://www.princeton.edu/~chaff/publication/cade_cav_2002.pdf)

### Code formatting

We are using `hindent` to format our Haskell source code with the configuration file `.hindent.yaml`.

### Github repo where you can find the most up-to date satskell

https://github.com/guruprerana/satskell
