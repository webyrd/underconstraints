From conversation with Michael Ballantyne 3 June 02023.

Move all underconstraint extensions to a separate file to minimize the
diff between vanilla faster-miniKanren and the extended version.
Should work from a restored `mk.scm` to ensure minimum diffing.

The underconstraint info added to the store should be a sigle U/V pair
of an underconstraint store and a list of "touched" variables.  This
minimizes the number of changes to faster-miniKanren.

Create a `solve-underconstraints` function that can be called either
at the end of `==`, individual constraint calls, and/or immediately
within `conde`, so I can experiment with performance tradeoffs of
running the underconstraints more or less often.  MB points out that
we might want to call underconstraints less often, if they turn out to
be expensive: might want a counter in the state, so we can solve
underconstraints only after at least N operations have occurred (every
10 times operations that add touched variables, or N basic
block/`conde` entries, etc.).


What needs to be done, exactly, to implement general underconstraints,
based on thoughts in `TODO.md`.

?) Is it ever necessary or desirable to remove an underconstraint?
For example, in the case in which the term an underconstraint is
associated with becomes ground.  Or, will the faster-miniKanren
implementation of unification and constraint solving avoid extending
the substitution or constraint store with ground variables, avoiding
this problem?  Certaintly it isn't *necessary*, since running an
underconstraint a finite number of extra times is inefficient but
sound.

!) Get general underconstraints working and tested *before* trying to
 optimize.  Make sure optimizations are guided by benchmarks.

!) Run all the tests after making each change to the `mk.scm` code.

!) Need to be able to support nested underconstraints correctly.  One
 way to do this would be to strip all underconstraints off all
 variables with constraints on them in the constraint store in the
 state passed to the underconstraint's goal.  

!) To avoid duplicate work, when I am running underconstraints
 associated with multiple variables, keep track of which
 underconstraints have already been run.  (Is `eq?` on procedures
 going to be an issue?  Do I need a gensym/counter/name on each
 underconstrained procedure, for easier debugging and reliable
 comparison?  Probably!)  Ensure that no underconstrained goal is run
 more than once per set of variables that are triggering the
 underconstraints, since running the same underconstraint more than
 once cannot result in failure (since all substitution and constraint
 extensions are discarded upon underconstraint success).

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


*) Update the state object to include a list of underconstraint goals
 (without duplicates) associated with each logic variable. [done]

*) Add an `underconstraino` goal constructor function that takes a
 term and a goal:

 `(underconstraino <term> <goal>)`

  Alternatively, could make `underconstraino` a macro that takes a
  term-expression and a goal-expression:

  `(underconstraino <term-expr> <goal-expr>)`

  Should also have a variant that takes a timeout for an engine, and a
  tracing variant, similar to one-shot underconstraints.

  When `underconstraino` is called, the associated goal should be
  immediately run, in the current state *updated with an empty
  underconstraint store*.  Upon failure, fail as usual.  Upon success,
  return a *singleton stream* (for `onceo` semantics) containing the
  *original* state passed to the underconstraint, but with the new
  underconstraint's goal added to the underconstraint store by pushing
  it down on any fresh variables inside of the term.  The
  underconstriaint should be given a unique name, either user-provided
  (plus a gensym, perhaps), or auto-generated, that will be associated
  with the underconstrained goal to ensure that the list of
  underconstraints associated with a variable does not contain
  duplicates, to aid with tracing, and to help keep track of which
  underconstraints have already been run when running all the
  underconstraints associated with a set of variables.
  

*) Add the `add-c` code from the staged miniKanren branches of
 `faster-miniKanren` to keep track of which variables have been
 updated due to non-unification constraint solving.

*) 

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