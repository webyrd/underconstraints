What needs to be done, exactly, to implement general underconstraints,
based on thoughts in `TODO.md`.

!) Get general underconstraints working and tested *before* trying to
 optimize.  Make sure optimizations are guided by benchmarks.


*) Extend the state representation to include underconstraints, along
 with the substition and normal constraints.

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