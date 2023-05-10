Underconstraints TODO

* think throught whether general underconstraints must be top-level
* think through how nested underconstraints should work (for both one-shot and general underconstraints)
* think through the interface for general underconstraints
* think through how general underconstraints should be represented in the constraint store
* closely read through the existing `faster-miniKanren` implementation
* add general underconstraints to the `faster-miniKanren` implementation
* add tests for when the engine times out for an underconstraint (which should lead to success)

* update README in faster-miniKanren.scm to reflect the underconstraint changes:
** figure out differences between README in faster-miniKanren.scm and
  the comments in mk.scm:
** README says that in =/= a constraint must be added to both variables
in one of the disjunct pairs, if applicable, but the comments in mk.scm
say that the constraint is added to only one variable
** README should describe the new `stringo` type constraint

* make sure I completely understand the mk.scm code:
** how and why is `remove-c` used?
** how does the new intmap work?
