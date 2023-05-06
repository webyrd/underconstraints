(load "faster-minikanren-with-underconstraints/mk-vicare.scm")
(load "faster-minikanren-with-underconstraints/mk.scm")
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

(test "underconstraino-1a"
  ;; classic miniKanren
  (run* (x)
    (== 3 x)
    (== 4 x))
  '())

(test "underconstraino-1b"
  ;; unsound!
  (run* (x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-1c"
  ;; sound but with a useless ordering of goals
  (run* (x)
    (== 3 x)
    (== 4 x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-1d"
  ;; also sound but with a useless ordering of goals
  (run* (x)
    (underconstraino (== 3 x))
    (== 3 x)
    (== 4 x)
    (underconstraino (== 4 x)))
  '(_.0))

(test "underconstraino-1e"
  ;; sound, with a typical goal ordering for underconstraints:
  ;; underconstraints should normally come first
  (run* (x)
    (underconstraino (== 3 x))
    (underconstraino (== 4 x))
    (== 3 x)
    (== 4 x))
  '(_.0))

(test "underconstraino-1f"
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

