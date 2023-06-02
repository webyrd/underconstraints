What needs to be done, exactly, to implement general underconstraints,
based on thoughts in `TODO.md`.

!) Get general underconstraints working and tested *before* trying to
 optimize.  Make sure optimizations are guided by benchmarks.

!) Run all the tests after making each change to the `mk.scm` code.

!) Need to be able to support nested underconstraints correctly.  One
 way to do this would be to strip all underconstraints off all
 variables with constraints on them in the constraint store in the
 state passed to the underconstraint's goal.  


From a conversation with Michael Ballantyne on June 1, 2023, Michael
convinced me that I don't need anything fancy in order to reset the
underconstraints passing in the state to an underconstraint's goal.
Instead, just have a hash table of variable->underconstraints as part
of the state.  Don't mess with the variable constraint state at all.
Whenever I pass in a state to an underconstraint's goal, reconstruct
the state object to contain the *empty* hash table of
variable->underconstraints.

Michael also pointed out that there might be two meanings to the
notion of "nested underconstraints": `(conj* u1 u2 g3 u4 u5)`, and
also my original notion of nesting that comes up, for example, when an
underconstraint's goal is a recursive relation.  In both cases we want
to reset the underconstraint store to the empty hash table when
running the goal associated with the underconstraint.  Lexical scope
does the rest!  :)


*) Update the constraint record object to include a list of
 underconstraint goals (without duplicates) associated with each logic
 variable.

*) Add the `add-c` code from the staged miniKanren branches of
 `faster-miniKanren` to keep track of which variables have been
 updated due to non-unification constraint solving.

??? What is the right way to handle nested general underconstraints?
Use a depth counter?  Strip away all underconstraints on all the
variables in the state being passed into the underconstraint's goal?
Some other approach?  Need to be careful here.  I could write a
function that strips away all underconstraints on all variables in the
state being passed into the underconstraint's goal, alhough that might
be expensive.  Perhaps a counter or scope-style sentinel value would
be more efficient.

*) Implement general `underconstraino` constraint that takes a term
and a goal, runs the underconstraint (with `onceo` semantics) in the
current state, and upon success returns the original state + the new
underconstraint pushed down onto any fresh variables.

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