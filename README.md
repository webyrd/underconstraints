# underconstraints

Underconstraints for miniKanren, inspired by a conversation with Youyou Cong about the `presento` constraint (a dual to the `absento` constraint) in miniKanren, from the mkTut tutorial I gave at <Programming 2023> in Tokyo.

`underconstraint` -- a constraint that is necessary, but not
sufficient.

Underconstrained constraints can be safely checked independently of
each other, without worrying about constraint interaction.  This may
be useful for failing fast, but not for ensuring correctness.
Underconstraints could give us *some* improved commutative conjunction
behavior.

It is not necessary to reify underconstraints, since underconstraints
are supposed to be no more constraining than the constraints that are
already imposed by the "normal" miniKanren constraints.  They do not
add any information to the declarative meaning of the program, but can
allow miniKanren to fail faster in some cases.

Removing all underconstraints from a program should never result in
additional answers to a run* query, assuming that the query terminated
with the underconstraints present.  The implementation does not check,
however, whether the use of underconstraints is sound---proper use of
underconstraints is the responsibility of the author of the program.
It should be possible to automatically generate sound underconstraints
in many (all?) cases, though, from the "normal" miniKanren code.

An incorrect use of underconstraints would be to ensure that a term is
a legal Oleg numeral: (1) and (0 1) succeed, (0) and (1 0) fail.  This
use of underconstraints is incorrect, since a run* that returns ()
when a term is instantiated to (0) might return an answer if the
underconstraint were to be removed.

These underconstrained constraints are safe to check independently of
each other without extending the constraint store.  I use this trick
in Barliman, but it could be made available at the user level, and
would be much more useful.  Basically it's a constraint to try to fail
fast.  I think it could help with Barliman synthesis, maybe.

For a Barliman typed synthesis program, for example, might write the following:

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
