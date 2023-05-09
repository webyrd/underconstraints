(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")
(load "test-check.scm")

(test "one-shot-underconstraino-trivial-classic"
  ;; classic miniKanren
  (run* (x)
    (== 3 x)
    (== 4 x))
  '())

(test "one-shot-underconstraino-trivial-sound-but-useless"
  ;; sound but with a useless ordering of goals
  (run* (x)
    (== 3 x)
    (== 4 x)
    (one-shot-underconstraino 'a (== 3 x))
    (one-shot-underconstraino 'b (== 4 x)))
  '())

(test "one-shot-underconstraino-trivial-sound-but-useless-2"
  ;; also sound but with a useless ordering of goals
  (run* (x)
    (one-shot-underconstraino 'a (== 3 x))
    (== 3 x)
    (== 4 x)
    (one-shot-underconstraino 'b (== 4 x)))
  '())

(test "one-shot-underconstraino-trivial-typical"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (one-shot-underconstraino 'a (== 3 x))
    (one-shot-underconstraino 'b (== 4 x))
    (== 3 x)
    (== 4 x))
  '())

(test "one-shot-underconstraino-trivial-optimized"
  ;; sound, with a typical goal ordering for underconstraints, and
  ;; with the underconstraint for the first non-constrained goal
  ;; removed, since it is not helpful with this particular goal
  ;; ordering
  ;;
  ;; it may be desirable to keep both underconstraints, if the goals
  ;; might be reordered in the future, or if the goals might be
  ;; dynamically reordered at runtime
  (run* (x)
    (one-shot-underconstraino 'b (== 4 x))
    (== 3 x)
    (== 4 x))
  '())


;; Another silly and useless trivial example.  Useless since `symbolo`
;; and `numbero` are already lazy constraints that don't ground their
;; arguments.

(test "one-shot-underconstraino-silly-classic"
  ;; classic miniKanren
  (run* (x)
    (symbolo x)
    (numbero x))
  '())

(test "one-shot-underconstraino-silly-unsound"
  ;; unsound!
  (run* (x)
    (one-shot-underconstraino 'a (symbolo x))
    (one-shot-underconstraino 'b (numbero x)))
  '(_.0))

(test "one-shot-underconstraino-silly-sound-but-useless"
  (run* (x)
    (symbolo x)
    (one-shot-underconstraino 'a (symbolo x))
    (numbero x)
    (one-shot-underconstraino 'b (numbero x)))
  '())

(test "one-shot-underconstraino-silly-typical"
  (run* (x)
    (one-shot-underconstraino 'a (symbolo x))
    (one-shot-underconstraino 'b (numbero x))
    (symbolo x)
    (numbero x))
  '())

(test "one-shot-underconstraino-silly-optimized"
  (run* (x)
    (one-shot-underconstraino 'a (numbero x))
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
(test "one-shot-underconstraino-number-choiceo-classic"
  (run* (x)
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())

;; unsound!
(test "one-shot-underconstraino-number-choiceo-unsound"
  (run* (x)
    (one-shot-underconstraino 'a (one-or-two-choiceo x))
    (one-shot-underconstraino 'b (three-or-four-choiceo x)))
  '(_.0))

(test "one-shot-underconstraino-number-choiceo-sound-but-useless"
  (run* (x)
    (one-or-two-choiceo x)
    (three-or-four-choiceo x)
    (one-shot-underconstraino 'a (one-or-two-choiceo x))
    (one-shot-underconstraino 'b (three-or-four-choiceo x)))
  '())

(test "one-shot-underconstraino-number-choiceo-typical"
  (run* (x)
    (one-shot-underconstraino 'a (one-or-two-choiceo x))
    (one-shot-underconstraino 'b (three-or-four-choiceo x))
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())


(test "one-shot-underconstraino-number-choiceo-optimized"
  (run* (x)
    (one-shot-underconstraino 'b (three-or-four-choiceo x))
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())

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

(test "factor-6-one-shot-underconstraino-typical"
  (run 4 (n m o)
    (== (build-num 12) o)
    (one-shot-underconstraino 'a (numeralo n))
    (one-shot-underconstraino 'b (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '(((1) (0 1 1) (0 1 1))
    ((0 1 1) (1) (0 1 1))
    ((0 1) (1 1) (0 1 1))
    ((1 1) (0 1) (0 1 1))))


(test "*o-illegal-cat-undetected"
  ;; apparently, 0 * cat = 0
  ;;
  ;; The real New Math...
  (run* (n m o)
    (== 'cat m)
    (*o n m o))
  '((() cat ())))

(test "*o-illegal-cat-verify"
  (run 1 (n m o)
    (== 'cat m)
    (*o n m o)
    (numeralo n)
    (numeralo m)
    (numeralo o))
  '())

#;(test-diverges "*o-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)
    (numeralo o)
    (*o n m o)))

#;(test-diverges "no-mult-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)))

(printf "testing onceo-behavior-of-one-shot-underconstraino-1...\n")
(printf "shouldn't even the one-shot version of this query terminate with failure?\n")
(test "onceo-behavior-of-one-shot-underconstraino-1"
  (run 1 (n m o)
    (== 'cat m)
    (trace-one-shot-underconstraino 'a (numeralo n))
    (trace-one-shot-underconstraino 'b (numeralo m))
    (numeralo n)
    (numeralo m))
  '())
(printf "finished testing onceo-behavior-of-one-shot-underconstraino-1\n")


(printf "testing onceo-behavior-of-one-shot-underconstraino-1...\n")
(printf "shouldn't even the one-shot version of this query terminate with failure?\n")
(test "onceo-behavior-of-one-shot-underconstraino-1"
  (run 1 (n m o)
    (== 'cat m)
    (one-shot-underconstraino 'a (numeralo n))
    (one-shot-underconstraino 'b (numeralo m))
    (numeralo n)
    (numeralo m))
  '())
(printf "finished testing onceo-behavior-of-one-shot-underconstraino-1\n")

(test "onceo-behavior-of-one-shot-underconstraino-2"
  (run 1 (n m o)
    (== 'cat m)
    (one-shot-underconstraino 'a (numeralo n))
    (one-shot-underconstraino 'b (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '())

(test "*o-illegal-cat-one-shot-underconstraino"
  (run 1 (n m o)
    (one-shot-underconstraino 'a (numeralo n))
    (one-shot-underconstraino 'b (numeralo m))
    (one-shot-underconstraino 'c (numeralo o))
    (== 'cat m)
    (*o n m o)
    (numeralo n)
    (numeralo m)
    (numeralo o))
  '())
