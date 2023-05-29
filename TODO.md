Underconstraints TODO

------------------
<from an email to myself, May 23, 2023>

Implement a flat (no nesting allowed), top-level underconstraint that takes a single term and a goal as arguments.

The constraint store keeps track of the current under constraints, and the previous walk*ed value of the term.

After all the normal constraints succeed, if the constraint store/substitution has been extended, the for each underconstraint, walk* the term, and compare it to the old walk*ed term.  If the new walk*ed term differs, then run the underconstraint. If not, ignore.

If the term ever becomes ground, can throw away the underconstraint after running it.

If the term becomes partially ground, may be able to simplify the term to just be a list of fresh variables, to keep down the costs of walk*.

May be able to further optimize by lazily walking rather than doing a full walk*.

Make sure to run the constraints in a new constraint store with a global scope.

Make sure to throw away any constraint store extensions upon success.

Would this version automatically work recursively?

Add timeout and tracing versions as well.

--

If I strip out all the underconstraints from the constraint store passed to the goal of the underconstriant, nested underconstraints may just work.

Basically a type of stratification

--

Can collect a list or set of fresh variables when doing the walk to check if anything has changed.  Use that list as the new term to check.

Could also check the constraints on those variables, I suppose.

Might be able to be ultra lazy while checking term equality.  Not sure.

---------------------------

Thoughts on these emails:

```
(underconstraino <term expression> <goal expression>)
```

seems like a reasonable interface.  Can always combine whatever terms
or variables into a single term through `cons` or `list` or
`quasiquote` and `unquote`.

Simplifying the term during each solving step, reconstructing a list
of remaining fresh variables (for example), might make sense.

--

Michael Ballantyne joined me, and had several suggestions and
questions.  Michael thinks it could be more efficient to put the
underconstraints on attributed variables.  Sounds like this would
work, since even though a disequality constraint is only placed on one
of the variable/term pairs (or on a single variable/variable pair),
the disequlity constraint can't be violated unless all of the relevant
variable/term or variable/variable pairs become equal.  So it is
possible to use the current attributed variable scheme to trigger an
under constraint when (and only when) the relevant attributed
variables are updated in a way that might cause the underconstraint to
now fail.

Michael says that the staged miniKanren code has to pay attention to
constrained variables that have changed, and that I can probably use
or adapt this code for the underconstraints (from `mk.scm` in the
`staged-file-cleanup` branch of `faster-miniKanren`):

```
; Constraint store object.
; Mapping of representative variable to constraint record. Constraints
; are always on the representative element and must be moved / merged
; when that element changes.

;; For staged-mk, we track the variables whose constraint store
;; entries have been changed since the last entering a `later-scope`.
(define C-vars car)
(define C-map cdr)
(define empty-C (cons '() empty-intmap))

(define (set-c st x c)
  (state-with-C
    st
    (cons (cons x (C-vars (state-C st)))
          (intmap-set (C-map (state-C st)) (var-idx x) c))))

(define (lookup-c st x)
  (let ((res (intmap-ref (C-map (state-C st)) (var-idx x))))
    (if (unbound? res)
      empty-c
      res)))
```

More `C-map` related functions:

```
; t:unbind in mk-vicare.scm either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define (remove-c x st)
  (state-with-C st (cons (C-vars (state-C st)) (intmap-set (C-map (state-C st)) (var-idx x) empty-c))))

(define (C-new-later-scope C)
  (cons '() (C-map C)))
```

Hmmm---`C-new-later-scope` doesn't appear to be used!

Is the list of variables actually reset?  Or does it just keep growing?  Is this an oversight in the code, or am I missing something?  I would expect `C-new-later-scope` to be used in `fresh` and/or `conde`, or perhaps reset after each batch of constraint solving.  I don't understand this...

I suppose for the purposes of underconstraints, I could reset the list each time unification and/or constraint solving is performed, then use the list immediately after the normal constraint solving.

```
(define (== u v)
  (lambda (st)
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
        (and-foldl update-constraints (state-with-S st S^) added)
        #f))))
```

Constraint update:

```
; Not fully optimized. Could do absento update with fewer
; hash-refs / hash-sets.
(define (update-constraints a st)
  (let ((old-c (lookup-c st (lhs a))))
    (if (eq? old-c empty-c)
      st
      (let ((st (remove-c (lhs a) st)))
       (and-foldl (lambda (op st) (op st)) st
        (append
          (if (c-T old-c)
            (list ((apply-type-constraint (c-T old-c)) (rhs a)))
            '())
          (map (lambda (atom) (absento atom (rhs a))) (c-A old-c))
          (map =/=* (c-D old-c))))))))
```

An example use of `lookup-c` and `set-c`, when adding
a disequality constraint:

```
(define (add-to-D st v d)
  (let* ((c (lookup-c st v))
         (c^ (c-with-D c (cons d (c-D c)))))
    (set-c st v c^)))
```

Should the variables associated with `added` returned from a
successful unification be handled the same, or differently, than the
variables added from non-unification constraint solving?
Unification/the `added` list seems like the only way an
underconstraint can be removed.  And perhaps the only way an
underconstraint can be pushed to another variable.  So, I should
probably hndled `added` pairs differently than the list of
newly-constrained vars that are not from unification.  I might need to
do something like remove the set of `added` vars from the set of
newly-constrained vars to see which underconstraints should actually
be run.

Do I need to remove dups from the list of updated variables in C?

I need to both associate underconstraints with fresh variables,
and (presumably) update/remove/add/move these underconstraint/variable
associations when variables become ground/partially ground (remove
underconstraint after running, or push down underconstraint on two
variables, etc).

I should ensure an underconstraint is never run more than once after
constraints are solved, even if multiple variables associated with a
given underconstraint have become further constrained.

If a variable associated with an underconstraint becomes ground,
should still run the underconstraint.  Do I need to remove the
underconstraint from that var?  Or is it not an issue, since that
variable will never appear in the `added` list, or in the list of
variables that have been constrained by constraints other than
unifiation?

Think through the complete case analysis for pushing down an
underconstraint, removing an underconstraint (if necessary), etc.
Think through the cases for `added` pairs.  Think about `walk`,
representative elements, etc.

Seems like there are some assumptions/invariants I need to
maintain/ensure/document.  For example, for the `added` list, is this
a legal miniKanren substitution, with the normal mk substitution
invariants?  Are left-hand-side terms all logic variables?  Are logic
variables on the right-hand-side fresh (but not necessarilty logic
variables occurring in a pair in the rhs)?  What are the properties
related to `walk`ing/`walk*`ing the lhs and rhs terms?  Do I ever need
to `walk` variables in the lhs of the `added` list?  What about
variables in the rhs?

Delay running underconstraints until both unification and normal
constraint solving both succeeds.

Could delay updating which underconstraints are on which variables
until after all underconstraints succeed, to hopefully avoid this
work (if an underconstraint fails).  It may have better amortized
efficiency to update the constraints on variables while solving the
underconstraints.  Need to look at this closely.

Could solve all the underconstraints in parallel, using multiple
threads, since the underconstraints are independent.  Could have the
main thread updating the variable associations for the
underconstraints, while the underconstraints run in parallel.  This
would add complexity, and Chez with threads runs single-threaded code
slower than does vanilla Chez.  Not sure this would be a win.

---------------------------

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
** why does `mplus*` use suspend, but not `bind*`?

```
; (bind* e:SearchStream g:Goal ...) -> SearchStream
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

; (suspend e:SearchStream) -> SuspendedStream
; Used to clearly mark the locations where search is suspended in order to
; interleave with other branches.
(define-syntax suspend (syntax-rules () ((_ body) (lambda () body))))

; (mplus* e:SearchStream ...+) -> SearchStream
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (mplus e0 (suspend (mplus* e ...))))))
```

** What are the rules/intuition for using `suspend`?
** how exactly does this reification code in `run` work?

```
(lambda (st)
  (let ((st (state-with-scope st nonlocal-scope)))
    (let ((z ((reify q) st)))
      (cons z (lambda () (lambda () #f))))))
```

What is the purpose of `(cons z (lambda () (lambda () #f)))`?

** In the description of constraints in the comments, this comment seems odd to me:

; Predicate: Any -> (or #f Any)

Isn't '#f' an element of the 'Any' type?  If #f is treated specially,
doesn't that mean that #f is actually ambiguous?  Would it be better
to return the Any value in a list to avoid this problem, or in
practice is it okay?  Or, perhaps the 'Any' return type is really 'Any
- {#f}', or something like that.

Also, why isn't the type of Predicate

Predicate: Any -> Boolean

That's a real predicate (plus non-divergence/non-error behavior).

number?, string?, symbol? all have this type, and are "true" predicates.

** In this code:

; TypeConstraint -> (Term -> Goal)
(define (apply-type-constraint tc)

the notion of a goal constructor type might help:

; TypeConstraint -> GoalConstructor
(define (type-constraint->goal-constructor tc)

