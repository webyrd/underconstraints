(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")
(load "test-check.scm")

(test "underconstraino-trivial-classic"
  ;; classic miniKanren
  (run* (x)
    (== 3 x)
    (== 4 x))
  '())

(test "underconstraino-trivial-sound-but-useless"
  ;; sound but with a useless ordering of goals
  (run* (x)
    (== 3 x)
    (== 4 x)
    (one-shot-underconstraino 'a (== 3 x))
    (one-shot-underconstraino 'b (== 4 x)))
  '())

#!eof

(test "underconstraino-trivial-sound-but-useless-2"
  ;; also sound but with a useless ordering of goals
  (run* (x)
    (one-shot-underconstraino 'a (== 3 x))
    (== 3 x)
    (== 4 x)
    (one-shot-underconstraino 'b (== 4 x)))
  '())

(test "underconstraino-trivial-typical"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (one-shot-underconstraino 'a (== 3 x))
    (one-shot-underconstraino 'b (== 4 x))
    (== 3 x)
    (== 4 x))
  '())

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
    (one-shot-underconstraino 'b (== 4 x))
    (== 3 x)
    (== 4 x))
  '())

