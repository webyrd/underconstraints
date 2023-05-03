# underconstraints

Underconstraints for miniKanren, inspired by a conversation with
Youyou Cong about the `presento` constraint (a dual to the `absento`
constraint) in miniKanren, from the mkTut tutorial I gave at
<Programming 2023> in Tokyo.

The `faster-minikanren-with-underconstraints` code is modified from
Michael Ballantyne's
`https://github.com/michaelballantyne/faster-miniKanren`.

An *underconstraint* (short for "underconstrained constraint") is a
constraint that is *necessary*, but not *sufficient*.  An
underconstaint may (or may not!) result in a computation with no
answer "failing fast".  Otherwise, a sound use of underconstraints
should not affect the semantics of a relational program in any way.

Underconstraints can be safely checked independently of each other,
without worrying about constraint interaction.  This may be useful for
failing fast, but not for ensuring correctness.  Underconstraints
might give us *some* improved commutative conjunction behavior.

It is not necessary to reify underconstraints, since underconstraints
are supposed to be no more constraining than the constraints that are
already imposed by the "normal" miniKanren
constraints. Underconstraints do not add any information to the
declarative meaning of the program, but can allow miniKanren to fail
faster in some cases.

Removing all underconstraints from a program should never result in
additional answers to a `run*` query, assuming that the query
terminates when the underconstraints are present.  The implementation
does not check, however, whether the use of underconstraints is
sound---proper use of underconstraints is the responsibility of the
author of the program.  It should be possible to automatically
generate sound underconstraints in many (all?) cases, though, from
the "normal" miniKanren code.

Consider a `numeralo` underconstraint that ensures a term is a legal
Oleg numeral: `(numeralo '(1))` and `(numeralo '(0 1))` would succeed,
while `(numeralo '(0))` and `(numeralo '(1 0))` would fail.
The following use of the `numeralo` underconstraint would be unsound:

```
(run* (n m o)
  (== '() n)
  (== 'cat m)
  (numeralo n)
  (numeralo m)
  (*o n m o))
```

With the `(numeralo m)` underconstraint present, the `run*`
returns `()`.  However, removing the `(numeralo m)` underconstraint
would result in the `run*` returning `(() cat ())`, since the `*o`
relation encodes the rule that 0 times any value is 0, regardless of
whether the second argument to `*o` is a legal numeral.

Underconstraints are safe to check independently of each other without
extending the constraint store.  I use this trick in Barliman, but it
could be made available at the user level, and would be much more
useful.  Basically it's a constraint to try to fail fast.  I think it
could help with Barliman synthesis, maybe.

For a Barliman typed synthesis program, for example, we might write
the following:

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

The idea is that each underconstraint is run: 1) when the constraint
is introduced; and 2) when the contraint store is extended.

Running underconstraints at the end of a `run` is harmless but
useless (assuming the use of underconstraints is sound).
