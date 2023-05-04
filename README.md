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

Removing all underconstraints from a program *must never* result in
additional answers to a `run*` query, assuming that the query
terminates when the underconstraints are present---otherwise, the use
of undersconstraints in that program is unsound.  The implementation
does not check, however, whether the use of underconstraints is
sound---proper use of underconstraints is the responsibility of the
author of the program.  It should be possible to automatically
generate sound underconstraints in many (all?) cases, though, from the
"normal" miniKanren code.

It is unsound to use underconstraints alone to constrain the possible answers from a computation.  This is because underconstraints are checked individually, and do not interact with each other.  For example, consider the `run*` expression:

normal miniKanren:

```
(run* (x)
  (== 3 x)
  (== 4 x))
=> ()
```

unsound use:

```
(run* (x)
  (underconstraino (== 3 x))
  (underconstraino (== 4 x)))
=> (_.0)
```

sound version:

```
(run* (x)
  (== 3 x)
  (== 4 x)
  (underconstraino (== 3 x))
  (underconstraino (== 4 x)))
=> ()
```

equivalent to:

```
(run* (x)
  (underconstraino (== 3 x))
  (== 3 x)
  (== 4 x)
  (underconstraino (== 4 x)))
=> ()
```

typical usage:

```
(run* (x)
  (underconstraino (== 3 x))
  (underconstraino (== 4 x))
  (== 3 x)
  (== 4 x))
=> ()
```


A slightly more complex example:

```
(define one-or-two-choiceo
  (lambda (x)
    (conde
      ((== 1 x))
      ((== 2 x)))))

(define three-or-four-choiceo
  (lambda (x)
    (conde
      ((== 3 x))
      ((== 4 x)))))
```

normal miniKanren:

```
(run* (x)
  (one-or-two-choiceo x)
  (three-or-four-choiceo x))
=> ()
```

unsound use:

```
(run* (x)
  (underconstraino (one-or-two-choiceo x))
  (underconstraino (three-or-four-choiceo x)))
=> (_.0)
```

sound use:

```
(run* (x)
  (one-or-two-choiceo x)
  (three-or-four-choiceo x)
  (underconstraino (one-or-two-choiceo x))
  (underconstraino (three-or-four-choiceo x)))
=> ()
```

typical usage:

```
(run* (x)
  (underconstraino (one-or-two-choiceo x))
  (underconstraino (three-or-four-choiceo x))
  (one-or-two-choiceo x)
  (three-or-four-choiceo x))
=> ()
```






A silly/useless example.

normal miniKanren:

```
(run* (x)
  (symbolo x)
  (numbero x))
=> ()
```

This `run*` expression returns `()` because no value can simultaneously be both a symbol and a number.  However, the pure underconstraint equivalent returns `(_.0)`:

```
(run* (x)
  (underconstraino (symbolo x))
  (underconstraino (numbero x)))
=>
(_.0)
```

Since both underconstraints succeed individually, and since underconstraints do not interact with each other, the `run*` produces an answer.  For this reason, underconstraints should only be used in conjunction with non-underconstrained predicates or constraints, such as:


or the equivalent:

```
(run* (x)
  (symbolo x)
  (underconstraino (symbolo x))
  (numbero x)
  (underconstraino (numbero x)))
=>
()
```

typical usage (although pointless in this case, since `symbolo` and `numbero` are already lazy constaints, so underconstraints are no use in this particular example):

```
(run* (x)
  (underconstraino (symbolo x))
  (underconstraino (numbero x))
  (symbolo x)
  (numbero x))
=>
()
```

[TODO give an example of underconstraints that make use of `symbolo`, `numbero`, and/or `absento`, but in an intellegent way as part of a more complex relation that might benefit from underconstraints]







The point of underconstraints is *not* to constrain the set of
possible answers; rather, the point of underconstraints is to have a
chance to fail fast without prematurely grounding logic variables.

Here is a more sophisticated example on the sound use of
underconstraints.  Consider a `numeralo` relation that ensures a term
is a legal "Oleg numeral" (a little-endian list of binary digits that
must end with a '1' if non-empty), used in the pure relational
arithmetic code in *The Reasoned Schemer*:

```
(define numeralo
  (lambda (n)
    (conde
      ((== '() n))
      ((fresh (n^)
         (== `(1 . ,n^) n)
         (numeralo n^)))
      ((fresh (n^)
         (== `(0 . ,n^) n)
         (positive-numeralo n^))))))

(define positive-numeralo
  (lambda (n)
    (conde
      ((fresh (n^)
         (== `(1 . ,n^) n)
         (numeralo n^)))
      ((fresh (n^)
         (== `(0 . ,n^) n)
         (positive-numeralo n^))))))
```

`(numeralo '())`, `(numeralo '(1))`, and `(numeralo '(0 1))` succeed,
while `(numeralo '(0))`, `(numeralo '(1 0))`, and `(numeralo 'cat)` fail.

```
(run* (n m o)
  (== '() n)
  (== 'cat m)
  (numeralo n)
  (numeralo m)
  (*o n m o))
```


The following use of `numeralo` as an underconstraint would be
unsound:

```
(run* (n m o)
  (== '() n)
  (== 'cat m)
  (underconstraino (numeralo n))
  (underconstraino (numeralo m))
  (*o n m o))
```

With the `(underconstraino (numeralo m))` underconstraint present, the
`run*` returns `()` (since `m` is associated with the symbol `cat`,
which is not a legal numeral).  However, removing
the `(underconstraino (numeralo m))` underconstraint would result in
the `run*` returning `(() cat ())`, since `(*o '() 'cat '())`
succeeds (the `*o` relation encodes the rule that 0 times any value is
0, regardless of whether the second argument to `*o` is a legal
numeral).

```
(run* (n m o)
  (underconstraino (numeralo n))
  (underconstraino (numeralo m))  
  (== '() n)
  (== 'cat m)  
  (*o n m o)
  (numeralo n)
  (numeralo m))
```

General rules for using underconstraints:

1. for each underconstraint used, there should be a corresponding relation that enforces the underconstraint, to ensure soundness;

2. while it is sound to use `symbolo`, `numbero`, and `absento` contraints within an underconstraint, keep in mind that these constraints are already lazy, and already fail fast;

3. underconstraints should come early in the conjunction, possibly at the very end;

4. the normal relations corresponding to underconstraints should come late in the conjuntion, possible at the very end.



Underconstraints are safe to check independently of each other without
extending the constraint store, similarly to the trick used in
Barliman to check that each input/output example pair is consistent
individually before trying the full synthesis problem, but made
available at the user level.

For a Barliman typed synthesis program, for example, we might write
the following:

```
(fresh (prog)
  (underconstraino (!-exampleo prog inputN outputN))
  ...
  (underconstraino (eval-exampleo prog input-expr1 output-val11))
  (underconstraino (eval-exampleo prog input-expr2 output-val12))
  (underconstraino (eval-exampleo prog input-expr3 output-val13))
  ...
  (underconstraino (eval-exampleo prog input-exprN output-val1N))
  <normal evalo call>
  )
```

The idea is that each underconstraint is run: 1) when the constraint
is introduced; and 2) when the contraint store is extended.

Running underconstraints at the end of a `run` is harmless but useless
(assuming the use of underconstraints is sound).  For example:

```
(fresh (prog)
  <normal evalo call>
  (underconstraino (eval-exampleo prog input-expr1 output-val1))
  (underconstraino (eval-exampleo prog input-expr2 output-val2))
  (underconstraino (eval-exampleo prog input-expr3 output-val3))
  ...
  (underconstraino (eval-exampleo prog input-exprN output-valN))
  )
```

has the same declarative meaning as the previous program above, as the
same declarative meaning as:

```
(fresh (prog)
  <normal evalo call>)
```


Adding a type inferencer for synthesis might work well now:

```
(fresh (prog)
  (underconstraino (!-exampleo prog input-expr1 output-type1))
  (underconstraino (!-exampleo prog input-expr2 output-type2))
  (underconstraino (!-exampleo prog input-expr3 output-type3))
  ...
  (underconstraino (!-exampleo prog input-exprN output-typeN))
  ...
  (underconstraino (eval-exampleo prog input-expr1 output-val1))
  (underconstraino (eval-exampleo prog input-expr2 output-val2))
  (underconstraino (eval-exampleo prog input-expr3 output-val3))
  ...
  (underconstraino (eval-exampleo prog input-exprN output-typeN))
  <normal evalo call>
  <normal !-o call>
  )
```
