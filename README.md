# underconstraints

Underconstraints for miniKanren, inspired by a conversation with
Youyou Cong about the `presento` constraint (a dual to the `absento`
constraint) in miniKanren, from the mkTut tutorial I gave at
<Programming 2023> in Tokyo.

The `faster-minikanren-with-underconstraints` code is modified from
Michael Ballantyne's
`https://github.com/michaelballantyne/faster-miniKanren`.

An `underconstraint` is a constraint that is *necessary*, but not
*sufficient*.  An underconstaint may (or may not!) result in a
computation with no answer "failing fast".  Otherwise, a sound use of
underconstraints should not affect the semantics of a relational
program in any way.

Underconstraints can be safely checked independently of each other,
without worrying about constraint interaction.  This may be useful for
failing fast, but not for ensuring correctness.  Underconstraints
could give us *some* improved commutative conjunction behavior.

It is not necessary to reify underconstraints, since underconstraints
are supposed to be no more constraining than the constraints that are
already imposed by the "normal" miniKanren constraints.  They do not
add any information to the declarative meaning of the program, but can
allow miniKanren to fail faster in some cases.

Removing all underconstraints from a program should never result in
additional answers to a run* query, assuming that the query terminates
when the underconstraints are present.  The implementation does not
check, however, whether the use of underconstraints is sound---proper
use of underconstraints is the responsibility of the author of the
program.  It should be possible to automatically generate sound
underconstraints in many (all?) cases, though, from the "normal"
miniKanren code.

Consider a `numeralo` underconstraint that ensures a term is a legal
Oleg numeral: `(numeralo '(1))` and `(numeralo '(0 1))` would succeed,
while `(numeralo '(0))` and `(numeralo '(1 0))` would fail.
The following use of the `numeralo` underconstraint would be unsound:

```
(run* (n m o)
  (== '() n)
  (== 'this-definitely-isnt-a-legal-numeral m)
  (numeralo n)
  (numeralo m)
  (*o n m o))
```

since the `(numeralo m)` would fail, meaning the `run*` would
return `()`.  However, removing `(numeralo m)` would result in the
`run*` returning `(() this-definitely-isnt-a-legal-numeral ())`, since
the `*o` relation encodes the rule that 0 times any value is 0,
regardless of whether the second argument to `*o` is a legal numeral.

These underconstrained constraints are safe to check independently of
each other without extending the constraint store.  I use this trick
in Barliman, but it could be made available at the user level, and
would be much more useful.  Basically it's a constraint to try to fail
fast.  I think it could help with Barliman synthesis, maybe.

For a Barliman typed synthesis program, for example, might write the
following:

```
(fresh (prog)
  (underconstraino (!-exampleo prog inputN outputN))
  ...
  (underconstraino (eval-exampleo prog input1 output1))
  (underconstraino (eval-exampleo prog input2 output2))
  (underconstraino (eval-exampleo prog input3 output3))
  ...
  (underconstraino (eval-exampleo prog inputN outputN))
  <normal evalo call>
  <normal !-o call>
  )
```

The idea is that each underconstrained constraint is run: 1) when the
constraint is introduced; and 2) when the contraint store is extended.

Running the underconstraints at the end is harmless but useless.
