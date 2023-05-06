(load "faster-minikanren-with-underconstraints/mk-vicare.scm")
(load "faster-minikanren-with-underconstraints/mk.scm")
(load "faster-minikanren-with-underconstraints/numbers.scm")
(load "faster-minikanren-with-underconstraints/test-check.scm")

;; ** These examples do not run yet, since I have not yet implemented
;; `underconstraino`! **

;; Start with a few very trivial examples showing sound/unsound uses
;; of underconstraints and basic goal ordering concerns.
;;
;; These trivial examples don't really capture why underconstraints
;; might be helpful, since there is no branching/`conde`/conditionals
;; in these relations.  More interesting examples later show the real
;; motivation.  These trivial examples are just for testing the
;; implementation, and to make it easy to think about basic properties
;; of soundness/unsoundess and goal ordering.

(test "underconstraino-trivial-classic"
  ;; classic miniKanren
  (run* (x)
    (== 3 x)
    (== 4 x))
  '())

(test "underconstraino-trivial-unsound"
  ;; unsound!
  (run* (x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-trivial-sound-but-useless"
  ;; sound but with a useless ordering of goals
  (run* (x)
    (== 3 x)
    (== 4 x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-trivial-sound-but-useless-2"
  ;; also sound but with a useless ordering of goals
  (run* (x)
    (underconstraino (== 3 x))
    (== 3 x)
    (== 4 x)
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-trivial-typical"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x))
    (== 3 x)
    (== 4 x))
  '(_.0))

(test "underconstraino-trivial-optimized"
  ;; sound, with a typical goal ordering for underconstraints, and
  ;; with the underconstraint for the first non-constrained goal
  ;; removed, since it is not helpful with this particular goal
  ;; ordering
  ;;
  ;; it may be desirable to keep both underconstraints, if the goals
  ;; might be reordered in the future, or if the goals might be
  ;; dynamically reordered at runtime
  (run* (x)
    (underconstraino (== 4 x))
    (== 3 x)
    (== 4 x))
  '(_.0))


;; Another silly and useless trivial example.  Useless since `symbolo`
;; and `numbero` are already lazy constraints that don't ground their
;; arguments.

(test "underconstraino-silly-classic"
  ;; classic miniKanren
  (run* (x)
    (symbolo x)
    (numbero x))
  '())

(test "underconstraino-silly-unsound"
  ;; unsound!
  (run* (x)
    (underconstraino (symbolo x))
    (underconstraino (numbero x)))
  '(_.0))

(test "underconstraino-silly-sound-but-useless"
  ;; sound but with a useless ordering of goals
  (run* (x)
    (symbolo x)
    (underconstraino (symbolo x))
    (numbero x)
    (underconstraino (numbero x)))
  '())

(test "underconstraino-silly-typical"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (underconstraino (symbolo x))
    (underconstraino (numbero x))
    (symbolo x)
    (numbero x))
  '())

(test "underconstraino-silly-optimized"
  ;; sound, with a typical goal ordering for underconstraints, and
  ;; with the underconstraint for the first non-constrained goal
  ;; removed, since it is not helpful with this particular goal
  ;; ordering
  ;;
  ;; it may be desirable to keep both underconstraints, if the goals
  ;; might be reordered in the future, or if the goals might be
  ;; dynamically reordered at runtime
  (run* (x)
    (underconstraino (numbero x))
    (symbolo x)
    (numbero x))
  '())



;; Now for a slightly more realistic example.

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

;; classic miniKanren
(test "underconstraino-number-choiceo-classic"
  (run* (x)
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())

;; unsound!
(test "underconstraino-number-choiceo-unsound"
  (run* (x)
    (underconstraino (one-or-two-choiceo x))
    (underconstraino (three-or-four-choiceo x)))
  '(_.0))

;; sound but with a useless ordering of goals
(test "underconstraino-number-choiceo-sound-but-useless"
  (run* (x)
    (one-or-two-choiceo x)
    (three-or-four-choiceo x)
    (underconstraino (one-or-two-choiceo x))
    (underconstraino (three-or-four-choiceo x)))
  '())

;; sound, with a typical goal ordering for underconstraints:
;; underconstraints should normally come first
(test "underconstraino-number-choiceo-typical"
  (run* (x)
    (underconstraino (one-or-two-choiceo x))
    (underconstraino (three-or-four-choiceo x))
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())

;; sound, with a typical goal ordering for underconstraints, and
;; with the underconstraint for the first non-constrained goal
;; removed, since it is not helpful with this particular goal
;; ordering
;;
;; it may be desirable to keep both underconstraints, if the goals
;; might be reordered in the future, or if the goals might be
;; dynamically reordered at runtime
(test "underconstraino-number-choiceo-optimized"
  (run* (x)
    (underconstraino (three-or-four-choiceo x))
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())


;; Here is a more sophisticated example on the sound use of
;; underconstraints.
;;
;; Consider a `numeralo` relation that ensures a term is a legal "Oleg
;; numeral" (a little-endian list of binary digits that must end with
;; a '1' if non-empty), used in the pure relational arithmetic code in
;; 'The Reasoned Schemer':

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

(test "numeralo-0"
  (run* (q) (numeralo '()))
  '(_.0))

(test "numeralo-1"
  (run* (q) (numeralo '(1)))
  '(_.0))

(test "numeralo-2"
  (run* (q) (numeralo '(0 1)))
  '(_.0))


(test "numeralo-illegal-a"
  (run* (q) (numeralo '(0)))
  '())

(test "numeralo-illegal-b"
  (run* (q) (numeralo '(1 0)))
  '())

(test "numeralo-illegal-c"
  (run* (q) (numeralo 'cat))
  '())


;; We can use `numeralo` to ensure all numerals are well-formed, but
;; at the risk of prematurely grounding `n`, `m`, and `o`, resulting
;; in generate-and-test behavior:

(test "factor-6-gen-and-test"
  (run 4 (n m o)
    (== (build-num 6) o)
    (numeralo n)
    (numeralo m)
    (*o n m o))
  '(((1) (0 1 1) (0 1 1))
    ((0 1) (1 1) (0 1 1))
    ((1 1) (0 1) (0 1 1))
    ((0 1 1) (1) (0 1 1))))

(test "factor-6-verify"
  (run 4 (n m o)
    (== (build-num 6) o)
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '(((1) (0 1 1) (0 1 1))
    ((0 1 1) (1) (0 1 1))
    ((0 1) (1 1) (0 1 1))
    ((1 1) (0 1) (0 1 1))))

(test "factor-6-underconstraino-typical"
  (run 4 (n m o)
    (== (build-num 12) o)
    (underconstraino (numeralo n))
    (underconstraino (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '(((1) (0 1 1) (0 1 1))
    ((0 1 1) (1) (0 1 1))
    ((0 1) (1 1) (0 1 1))
    ((1 1) (0 1) (0 1 1))))


;; Now for the interesting part!
;;
;; Failing is a numeral is ill-formed, without diverging.

;; Not only does `*o` not ensure that its numerals are well formed,
;; `*o` will accept any term as its second argument when the first and
;; third arguments as () (which represents 0).

(test "*o-illegal-cat-undetected"
  ;; apparently, 0 * cat = 0
  ;;
  ;; The real New Math...
  (run* (n m o)
    (== 'cat m)
    (*o n m o))
  '((() cat ())))

;; We can verify that the numerals are legal *after* the call to `*o`:
(test "*o-illegal-cat-verify"
  (run 1 (n m o)
    (== 'cat m)
    (*o n m o)
    (numeralo n)
    (numeralo m)
    (numeralo o))
  '())

;; However, if we try to verify that the numerals are legal before the
;; call to `*o`, the `run` diverges!
(test-diverges "*o-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)
    (numeralo o)
    (*o n m o)))

;; In fact, this `run` diverges, even without the multiplication, due
;; to the `numeralo` goal itself!
(test-diverges "no-mult-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)))

;; So, we can't safely check that the arguments to `*o` are legal
;; before calling `*o`.  But, of couse, it is possible to construct
;; goals using `*o` that diverge when given the symbol 'cat' as an
;; argument (TODO: come up with a simple example!), so we really do
;; want to fail fast if the arguments are ill-formed.  And, of course,
;; even if a goal doesn't diverge, it might take a long time to
;; converge.

;; Goals wrapped in an `underconstraino` display `onceo` semantics, to
;; prevent a conjunction of underconstraints entering a
;; generate-and-test divergence loop.
(test "onceo-behavior-of-underconstraino-1"
  (run 1 (n m o)
    (== 'cat m)
    (underconstraino (numeralo n))
    (underconstraino (numeralo m))
    (numeralo n)
    (numeralo m))
  '())

;; We can use undercontraints to ensure we have legal numerals:

(test "onceo-behavior-of-underconstraino-2"
  (run 1 (n m o)
    (== 'cat m)
    (underconstraino (numeralo n))
    (underconstraino (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '())

;; The more comprehensive version, in which we ensure `o` is also
;; well-formed:

(test "*o-illegal-cat-underconstraino"
  (run 1 (n m o)
    (underconstraino (numeralo n))
    (underconstraino (numeralo m))
    (underconstraino (numeralo o))
    (== 'cat m)
    (*o n m o)
    (numeralo n)
    (numeralo m)
    (numeralo o))
  '())
