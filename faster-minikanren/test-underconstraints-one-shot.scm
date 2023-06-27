(load "load.scm")

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
    (one-shot-underconstraino 'a1 (== 3 x))
    (one-shot-underconstraino 'b1 (== 4 x)))
  '())

(test "one-shot-underconstraino-trivial-sound-but-useless-2"
  ;; also sound but with a useless ordering of goals
  (run* (x)
    (one-shot-underconstraino 'a2 (== 3 x))
    (== 3 x)
    (== 4 x)
    (one-shot-underconstraino 'b2 (== 4 x)))
  '())

(test "one-shot-underconstraino-trivial-typical"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (one-shot-underconstraino 'a3 (== 3 x))
    (one-shot-underconstraino 'b3 (== 4 x))
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
    (one-shot-underconstraino 'b4 (== 4 x))
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
    (one-shot-underconstraino 'a5 (symbolo x))
    (one-shot-underconstraino 'b5 (numbero x)))
  '(_.0))

(test "one-shot-underconstraino-silly-sound-but-useless"
  (run* (x)
    (symbolo x)
    (one-shot-underconstraino 'a6 (symbolo x))
    (numbero x)
    (one-shot-underconstraino 'b6 (numbero x)))
  '())

(test "one-shot-underconstraino-silly-typical"
  (run* (x)
    (one-shot-underconstraino 'a7 (symbolo x))
    (one-shot-underconstraino 'b7 (numbero x))
    (symbolo x)
    (numbero x))
  '())

(test "one-shot-underconstraino-silly-optimized"
  (run* (x)
    (one-shot-underconstraino 'b8 (numbero x))
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
(test "one-shot-underconstraino-number-choiceo-unsound-a"
  (run* (x)
    (one-shot-underconstraino 'a9 (one-or-two-choiceo x)))
  '(_.0))

;; unsound!
(test "one-shot-underconstraino-number-choiceo-unsound-b"
  (run* (x)
    (one-shot-underconstraino 'b10 (three-or-four-choiceo x)))
  '(_.0))

;; unsound!
(test "one-shot-underconstraino-number-choiceo-unsound-c"
  (run* (x)
    (one-shot-underconstraino 'a11 (one-or-two-choiceo x))
    (one-shot-underconstraino 'b11 (three-or-four-choiceo x)))
  '(_.0))

(test "one-shot-underconstraino-number-choiceo-sound-but-useless"
  (run* (x)
    (one-or-two-choiceo x)
    (three-or-four-choiceo x)
    (one-shot-underconstraino 'a12 (one-or-two-choiceo x))
    (one-shot-underconstraino 'b12 (three-or-four-choiceo x)))
  '())

(test "one-shot-underconstraino-number-choiceo-typical"
  (run* (x)
    (one-shot-underconstraino 'a13 (one-or-two-choiceo x))
    (one-shot-underconstraino 'b13 (three-or-four-choiceo x))
    (one-or-two-choiceo x)
    (three-or-four-choiceo x))
  '())


(test "one-shot-underconstraino-number-choiceo-optimized"
  (run* (x)
    (one-shot-underconstraino 'b14 (three-or-four-choiceo x))
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

(test "factor-12-one-shot-underconstraino-typical"
  (run* (n m o)
    (== (build-num 12) o)
    (one-shot-underconstraino 'a15 (numeralo n))
    (one-shot-underconstraino 'b15 (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '(((1) (0 0 1 1) (0 0 1 1))
    ((0 0 1 1) (1) (0 0 1 1))
    ((0 1) (0 1 1) (0 0 1 1))
    ((0 0 1) (1 1) (0 0 1 1))
    ((1 1) (0 0 1) (0 0 1 1))
    ((0 1 1) (0 1) (0 0 1 1))))


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

(test-divergence "*o-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)
    (numeralo o)
    (*o n m o)))

(test-divergence "no-mult-illegal-cat-generate-and-test"
  (run 1 (n m o)
    (== 'cat m)
    (numeralo n)
    (numeralo m)))

(test "onceo-behavior-of-one-shot-underconstraino-1"
  (run 1 (n m o)
    (== 'cat m)
    (one-shot-underconstraino 'a16 (numeralo n))
    (one-shot-underconstraino 'b16 (numeralo m))
    (numeralo n)
    (numeralo m))
  '())

(test "onceo-behavior-of-one-shot-underconstraino-1"
  (run 1 (n m o)
    (== 'cat m)
    (one-shot-underconstraino 'a17 (numeralo n))
    (one-shot-underconstraino 'b17 (numeralo m))
    (numeralo n)
    (numeralo m))
  '())

(test "onceo-behavior-of-one-shot-underconstraino-2"
  (run 1 (n m o)
    (== 'cat m)
    (one-shot-underconstraino 'a18 (numeralo n))
    (one-shot-underconstraino 'b18 (numeralo m))
    (*o n m o)
    (numeralo n)
    (numeralo m))
  '())

(test "*o-illegal-cat-one-shot-underconstraino"
  (run 1 (n m o)
    (one-shot-underconstraino 'a19 (numeralo n))
    (one-shot-underconstraino 'b19 (numeralo m))
    (one-shot-underconstraino 'c19 (numeralo o))
    (== 'cat m)
    (*o n m o)
    (numeralo n)
    (numeralo m)
    (numeralo o))
  '())

;; `nevero` will never terminate; we need the engine to cut off
;; underconstraint execution.
(define nevero
  (lambda ()
    (conde
      (fail)
      ((nevero)))))
(test "nevero-engine"
  (run 1 (q)
    (one-shot-underconstraino 'a20 (nevero) 1000))
  '(_.0))
