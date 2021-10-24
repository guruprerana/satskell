# CSE301 Project: SAT solver #

## Summary ##

The goal of the project is to write a *relatively* efficient SAT solver in Haskell
that inputs satisfiability questions and outputs an answer.  The choice of what
precise algorithms and data structures to use is up to you, although there are
some suggestions below.  You can work either individually or with a partner, and
you will be expected to give a brief oral presentation (~5 minutes) describing
your experience writing the solver, explaining the design choices that you made
as well as what kind of evaluation you performed.

## Top-level interface ##

Your SAT solver should read in a file containing a CNF formula represented in the DIMACS format, 
which is a primitive file format supported by many SAT solvers.
Here is an example of a DIMACS CNF file:

```
c This is a random 3-CNF formula with 5 variables and 4 clauses.
c It is satisfiable.
c 
p cnf 5 4
4 -3 -5 0
-3 -1 -2 0
1 -4 -5 0
-5 3 4 0
```

The first three lines beginning with "c" are comments.  The next line beginning
with "p" is a header describing the format of the formula (cnf), the number of
variables (5) and the number of clauses (4).  Finally, the remaining lines give
the clauses, each indicated by a list of positive or negative literals terminated
by a 0.  Thus the example above corresponds to the following formula in 
standard logical notation:
```math
(x₄ ∨ ¬x₃ ∨ ¬x₅) ∧ (¬x₃ ∨ ¬x₁ ∨ ¬x₂) ∧ (x₁ ∨ ¬x₄ ∨ ¬x₅) ∧ (¬x₅ ∨ x₃ ∨ x₄)
```
The output of your SAT solver should be a single line "SAT" or "UNSAT" indicating whether or not the formula is satisfiable, followed by a line with a satisfying assignment in the former case.
For instance, running on the above example, one possible output is:
```
SAT
1 -2 -3 4 -5
```
corresponding to the satisfying assignment
```math
[ (x₁, ⊤), (x₂, ⊥), (x₃, ⊥), (x₄, ⊤), (x₅, ⊥) ]
```
or equivalently to the conjunction of literals:
```math
x₁ ∧ ¬x₂ ∧ ¬x₃ ∧ x₄ ∧ ¬x₅
```
A formula may have more than one satisfying assignment, and in that case it is okay for your SAT solver to return any of them.
For example, another possible output for the above input is:
```
SAT
-1 -2 3 -4 -5
```

## Template code ##

To simplify your task, the file [Project.zip](Project.zip) already contains some Haskell
code that you can use as a template:

* CNF.hs: defines types of CNF formulas and substitutions as well as a few basic utility functions
* CNF/Eval.hs: evaluation of CNF formulas
* CNF/DIMACS.hs: routines for reading and writing files in DIMACS format
* CNF/Problems.hs: routines for generating some simple test problems
* Solver/Naive.hs: a naive SAT solver via exhaustive search (like the one you wrote for [Lab 3](../Labs/Lab3.md))
* MySat.hs: defines a top-level `main` routine that reads in a formula in DIMACS CNF format from a file specified on the command line and returns the output of the naive solver.

Note that the DIMACS parsing routines make use of the [Parsec](https://hackage.haskell.org/package/parsec) monadic parser combinator library.
To install this library on your machine, run `cabal install --lib parsec`.

Additionally, the subdirectory `Tests` contains a few test formulas in DIMACS format:

* r.cnf: a small unsatisfiable formula
* random5.cnf: the example formula above
* random60[ab].cnf: two random 3-CNF formulas with 60 variables
* colorK5_4.cnf: an unsatisfiable formula expressing that the complete graph on 5 vertices has a 4-coloring
* colorK8_7.cnf: an unsatisfiable formula expressing that the complete graph on 8 vertices has a 7-coloring
* colorK15_15.cnf: a satisfiable formula expressing that the complete graph on 15 vertices has a 15-coloring
* php[4-9].cnf: unsatisfiable formulas expressing successively larger versions of the negation of the [pigeon-hole principle](https://en.wikipedia.org/wiki/Pigeonhole_principle), that if n pigeons are placed in (n-1) holes then two pigeons must share the same hole
* notdichot20.cnf: an unsatisfiable formula resulting from [Tseitin translation](https://en.wikipedia.org/wiki/Tseytin_transformation) of the negation of (x1 | -x1) & ... & (x20 | -x20)

Running `ghc -O2 MySat.hs` from within the directory where you unzipped the
archive (assuming you've installed the Parsec library) will produce an
executable called `MySat`. This is already a working SAT solver, albeit a very inefficient one!
You can try invoking it like so:

```
$ ./MySat Tests/random5.cnf
SAT
-1 -2 -3 -4 -5
```

You should feel free to modify any of the template code, and certainly you should
replace the call to the naive solver in `MySat.hs` by a call to your own solver.

You are also encouraged to create additional tests (see below for more advice about that). 

## Module system ##

The code in the template is distributed across different files, making use of
[Haskell's module system](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html#library.anatomy).  Be aware that Haskell's module system relies on the
underlying directory structure, and module names have to match their file name and
location.  For example, the module `CNF` is defined in the file `CNF.hs`, and the
module `Solver.Naive` is defined in the file `Solver/Naive.hs`.  A module declaration
can optionally give a list of definitions to export.
For example, the file `Solver/Naive.hs` begins with the line
```hs
module Solver.Naive (solution) where
```
which says to only export the function
```hs
solution :: CNF -> Maybe Subst
```
to the outside world (i.e., to somebody who does an `import Solver.Naive`).
On the other hand, the file `CNF.hs` begins with the line
```hs
module CNF where
```
which says to export all of the definitions in the module.

You are encouraged to write your code as modularly as possible.
For example, you can try implementing different SAT solving algorithms as
different modules under the `Solver` directory, which might make use of
utility functions defined in a separate module.  You can also write
separate modules for testing your SAT solver and for comparing different algorithms.

## Administrative and technical notes ##

* The project is worth 40% of your course grade.

* You are allowed to work with a partner.  In that case, both partners must be
  able to explain the code that you submit, and you will give a joint oral
  presentation.  If you want to work with a partner, you must let me know via
  email by Monday, 18 October.

* The deadline for submitting the project is **31 Oct at 23h59**.
  Submission should take the form of a single archive file `Project.zip` uploaded
  via Moodle.  (If you are working with a partner, it is enough for one of
  you to submit your archive file.)

* It goes without saying that you should submit working Haskell code.
  In particular, it should be possible to compile your code by running
  `ghc -O2 MySat.hs` on the lab machines.  You can feel free to import
  anything from [the standard library](http://hackage.haskell.org/package/base),
  but if you want to use other external libraries please write me
  first to check this is okay.
  
* The global structure of your code should be described in a
  `README.md` file included in your zip archive, and if you are using
  any libraries, the installation instructions should also be
  described.

## Advice on writing your SAT solver ##

### Basic backtracking ###

Most SAT solving algorithms are based on some form of
[backtracking](https://en.wikipedia.org/wiki/Backtracking).  That means that they
gradually build up a *partial* assignment for some of the
variables in a formula, until either the whole formula is satisfied or there is no
way to make progress, and hence the formula is unsatisfiable.  The classic [DPLL
algorithm](https://en.wikipedia.org/wiki/DPLL_algorithm) is based on backtracking,
as is the [CDCL
algorithm](https://en.wikipedia.org/wiki/Conflict-driven_clause_learning), both of
which form the foundation for many modern SAT solvers.

To understand how these approaches work, it is worth trying to first implement
the simple algorithm below, which is not as efficient as DPLL or CDCL but
illustrates the basic idea of SAT solving via backtracking.  It relies
on the operation of *conditioning* a formula by a literal: given a formula $`F`$
and a literal $`ℓ`$, the formula $`F|ℓ`$ (read "$`F`$ given $`ℓ`$") is defined
by removing all clauses that contain $`ℓ`$ from $`F`$, and eliminating the
negation of $`ℓ`$ from remaining clauses.  For example, if 
```math
F = (x_1 ∨ ¬x_2) ∧ (x_2 ∨ x_3) ∧ (¬x_1 ∨ ¬x_3) ∧ (¬x_1 ∨ ¬x_2 ∨ x_3)
```
then
```math
F∣x_1 = (x_2 ∨ x_3) ∧ ¬x_3 ∧ (¬x_2 ∨ x_3)
```
while
```math
F∣¬x_1 = ¬x_2 ∧ (x_2 ∨ x_3)
```

Now, for any CNF formula $`F`$, we proceed as follows to construct a
satisfying assignment $`ρ`$:

1. If $`F`$ is the empty conjunction then it is trivially satisfiable, so our
   search succeeds and we return the empty assignment $`ρ = []`$.

2. Otherwise $`F`$ must contain at least one clause.  If it contains an empty
   clause then it is trivially unsatisfiable, so we fail.

3. Otherwise $`F`$ must contain at least one clause with at least one literal
   $`ℓ`$.  We try to construct a satisfying assignment $`ρ`$ for $`F|ℓ`$, and if
   that succeeds then we return the extended assignment $`ℓ:ρ`$.  Otherwise, we
   try to find a satisfying assignment $`ρ`$ for $`F|¬ℓ`$, and if
   that succeeds then we return the extended assignment $`¬ℓ:ρ`$.
   If that fails, we fail.

The procedure may either fail or return a partial assignment $`ρ`$
of values for some of the variables of $`F`$.
In that case, by construction, $`F`$ is satisfied under any extension
of $`ρ`$ with arbitrary values for the remaining variables.

As a first step, you can implement the above algorithm in
Haskell, either as a recursive function
```hs
solve :: [Cls] -> Maybe Subst
```
that attempts to construct a single partial satisfying assignment for a list of clauses, or as a recursive function
```hs
solve :: [Cls] -> [Subst]
```
that lazily constructs all partial satisfying assignments for a list of clauses.
You may first want to write a helper function
```hs
condition :: Lit -> [Cls] -> [Cls]
```
that conditions a list of clauses by a literal.

Once you're done, package everything up into a module that exports a function
```hs
solution :: CNF -> Maybe Subst
```
invoked from `MySat.hs`, and compare it with the first version of the solver using `Solver.Naive`.
(Attention: make sure that `solution` returns a *full* substitution for all the variables of
the CNF formula, and not just a partial substitution. It should be an invariant maintained
by `solve` that any partial substitution it returns can be extended to a full substitution by
assigning the missing variables arbitrary values.  By the way, just to be clear I am using the
words "substitution" and "assignment" interchangeably.)

### Improvements ###

There are many potential ways of improving the performance of your basic
backtracking SAT solver:

* One optimization performed by the DPLL algorithm is called *unit propagation:*
  if $`F`$ has a clause $`C = ℓ`$ with just a single literal (called a *unit clause*), then 
  the formula may be reduced eagerly to $`F∣ℓ`$, with no need to consider $`F∣¬ℓ`$.
  This optimization is very important in practice.

* Another optimization performed by DPLL is called *pure literal elimination:* if
  the negation of a literal $`ℓ`$ does not occur in any clauses of $`F`$, then 
  the formula may be reduced eagerly to $`F∣ℓ`$, again with no need to consider $`F∣¬ℓ`$.
  This optimization seems to be less important in practice.

* Practical implementations of DPLL use special data structures for quickly
  identifying unit clauses.  A literal $`ℓ`$ is said to be *active* if 
  it is consistent with the current substitution $`ρ`$, i.e., the underlying
  variable of $`ℓ`$ is either unmentioned in $`ρ`$ or assigned a value
  equal to the polarity of $`ℓ`$.  One possible way of identifying unit clauses is to
  keep a count of the number of currently active literals in each clause.
  Another approach adopted by many practical SAT solvers is called
  *watched literals*: each clause has a "watch" on up to two active literals,
  and each literal is associated with a list of clauses that watch it.
  If a literal $`ℓ`$ is falsified during backtracking search, only the clauses
  $`C`$ that watch it have to be updated by finding another active literal
  in $`C`$ to replace it if possible &mdash; and if it is not possible, it
  means $`C`$ is now either a unit clause or false.

* You could try different heuristics for picking a literal to branch on,
  like those explained in the Wikipedia article 
  [Boolean satisfiability algorithm heuristics](https://en.wikipedia.org/wiki/Boolean_satisfiability_algorithm_heuristics).

* You could implement [clause learning](https://en.wikipedia.org/wiki/Conflict-driven_clause_learning).

* You could perform some kind of pre-processing on the input.  For example, the *subsumption* rule says
  that if every literal in a clause $`C`$ appears also in another clause $`C'`$, then $`C'`$ can be removed.
  (You can find more discussion in [this paper](http://fmv.jku.at/papers/EenBiere-SAT05.pdf) by Eén and Biere,
  as well as in the Knuth reference listed in the Bibliography section below.)  Another kind of pre-processing
  is conversion to 3-CNF, which you could potentially use to simplify the design of your solver.

* From the perspective of Haskell, you could try experimenting with impure functional
  programming features such as [IORef](https://en.wikibooks.org/wiki/Haskell/Mutable_objects), or you
  could try using efficient purely functional data structures implemented in the standard library
  such as [Data.Map](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map.html).

* You might get some more ideas by reading the Haskell wiki articles on
  [performance](https://wiki.haskell.org/Performance) and 
  [performance in GHC](https://wiki.haskell.org/Performance/GHC).
  Chapter 25 of [Real World Haskell](http://book.realworldhaskell.org/)
  also gives some advice on [profiling and optimization](http://book.realworldhaskell.org/read/profiling-and-optimization.html).

Be warned that improving the performance of your SAT solver is not easy,
and even harder while maintaining correctness!  Make sure to separate
different approaches into different modules so that you can
compare them, and keep them for reference even if they do not necessarily
improve the performance of your solver.  (A revision control system
such as git or svn could eventually save you from losing your work.)

**I am interested in seeing the different approaches you tried and hearing about
the evaluation you performed, even if in the end you do not succeed in vastly
improving your basic backtracking SAT solver.**

### Tests ###

You should test your solver(s) on both satisfiable and unsatisfiable formulas.
Some valuable optimizations may help your solver to more quickly find a
satisfying assignment, but will not necessarily be useful for
unsatisfiable formulas.  Of course you should only implement *sound*
optimizations, in the sense that your solver should only
return correct satisfying assignments, and should never say that
a formula is unsatisfiable when it has a satisfying assignment.

In addition to the .cnf files already provided in the Tests directory (and others
freely available online, for example on [John Burkardt's
website](https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html)), you can try
generating your own formulas and turning them into .cnf files using the routine
`writeCNFtoDIMACS` from `CNF.DIMACS`.  For example, you can generate natural CNF
formulas from different constraint satisfaction problems, such as graph-coloring
or [Sudoku](https://www.lri.fr/~conchon/mpri/weber.pdf).  To give yourself the
extra flexibility of arbitrary boolean logic formulas, you can try implementing
the [Tseitin translation](https://en.wikipedia.org/wiki/Tseytin_transformation) to
convert to CNF without incurring an exponential blowup.

Another way of generating hard satisfiability problems is by generating random CNF
formulas with just the right amount of variables and clauses.  For example,
experimentally, it seems that a 3-CNF formula with $`n`$ literals and $`m`$
clauses is likely to be satisfiable if $`m/n < α_3`$ and likely to be
unsatisfiable if $`m/n > α_3`$, where $`α_3 ≈ 4.25 ± 0.05`$.  Random 3-CNF formulas with
a $`m/n`$ ratio right around $`α_3`$ are typically hard to decide for a SAT
solver, whereas formulas with a ratio far from $`α_3`$ in either direction
are typically easy to decide. (More generally, from experimental evidence it seems
there is a *threshold constant* $`α_k`$ for k-CNF formulas, although this has not
been proved formally except in the case $`k = 2`$ where $`α_2 = 1`$.)

Make sure to include any .cnf files that you created yourself in your archive submission,
as well as any code you wrote to generate them.

### Comparison ###

You can try downloading one or more of the following high-quality, open source SAT solvers:

* [Minisat](https://github.com/niklasso/minisat)

* [zChaff](https://www.princeton.edu/~chaff/zchaff.html)

* [Z3](https://github.com/Z3Prover/z3) (technically this is a more general [SMT](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories) solver, but it also works for SAT)

All of these accept problems in DIMACS CNF format, so you can compare them to your
solver (and don't be discouraged when your solver is likely to be outperformed!).

## Bibliography ##

Various references that may be useful:

* Wikipedia: [Boolean satisfiability problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem)

* Wikipedia: [DPLL](https://en.wikipedia.org/wiki/DPLL_algorithm)

* Wikipedia: [CDCL](https://en.wikipedia.org/wiki/Conflict-driven_clause_learning)

* Volume 4, Fascicle 6, "Satisfiability" of Donald E. Knuth's _The Art of Computer Programming_ ([link](https://dl.acm.org/doi/book/10.5555/2898950))

* [Formalization and Implementation of Modern SAT Solvers](http://poincare.matf.bg.ac.rs/~filip/phd/sat-tutorial.pdf) by Filip Marić

* Section 2.5.7 of [Program = Proof](http://www.lix.polytechnique.fr/Labo/Samuel.Mimram/teaching/INF551/course.pdf) by Samuel Mimram
