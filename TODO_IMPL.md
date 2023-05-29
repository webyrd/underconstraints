What needs to be done, exactly, to implement general underconstraints,
based on thoughts in `TODO.md`.

!) Get general underconstraints working and tested *before* trying to
 optimize.  Make sure optimizations are guided by benchmarks.

!) Run all the tests after making each change to the `mk.scm` code.


*) Update the constraint record object to include a list of
 underconstraint goals (without duplicates) associated with each logic
 variable. [done and tested]

*) Add the `add-c` code from the staged miniKanren branches of
 `faster-miniKanren` to keep track of which variables have been
 updated due to non-unification constraint solving.

*) Update `==` definition to run the underconstraints on variables
 that have been extended due to unification (in `added`) or are in the
 list of variables that have had constraints updated (through `add-c`
 being called).  The code also needs to push down the underconstraints
 onto fresh variables in a similar manner to how `absento` constraints
 are pushed down.  All of this code should only run after the normal
 constraint solving has succeeded.

*) Abstract the underconstraint solving and update code used in `==`,
 and add the equivalent code to `=/=`, `absento`, `symbolo`,
 `numbero`, and `stringo` goal constructor definitions.

...

*) Test general underconstraints, including nested underconstraints.

*) Once the general underconstraints are implemented, tested, and
 working, add engines version and parameter to control tracing, like
 with the one-shot underconstraints.

*) Once everything is working and tested, revisit and implement the
 "choice-point" version of general underconstraints, that delays
 running any underconstraints until reaching a `conde`.  At that
 point, underconstraints associated with variables that have changed
 are run, and underconstraints are attributed to new fresh variables
 ("pushed down") as necessary.

*) Once everything is working and tested, revisit to ensure the
 implementation performs as little `walk`ing of variables as possible.