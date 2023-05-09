(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")
(load "test-check.scm")

#|
;; can set global underconstraint parameter values:

  (*underconstraint-default-timeout-param* #f)
  (*trace-underconstraint-param* #f)
|#

(load "test-underconstraints-one-shot.scm")
;; (load "test-underconstraints-general.scm")
