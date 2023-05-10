(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")
(load "test-check.scm")

#|
;; can set global underconstraint parameter values:

  (*underconstraint-default-timeout-param* #f)
  (*trace-underconstraint-param* #f)
|#

(define (test-under file timeout trace?)
  (printf
   "\n*** testing ~s: `timeout` = ~s  `trace?` = ~s ***\n"
   file
   timeout
   trace?)
  (*underconstraint-default-timeout-param* timeout)
  (*trace-underconstraint-param* trace?)
  (load file))

(printf "=== Testing one-shot underconstraints ===\n")
(test-under "test-underconstraints-one-shot.scm" 1 #t)
(test-under "test-underconstraints-one-shot.scm" 1000 #t)
(test-under "test-underconstraints-one-shot.scm" 1000000 #t)
;;
(test-under "test-underconstraints-one-shot.scm" #f #t)
;;
(test-under "test-underconstraints-one-shot.scm" 1 #f)
(test-under "test-underconstraints-one-shot.scm" 1000 #f)
(test-under "test-underconstraints-one-shot.scm" 1000000 #f)
;;
(test-under "test-underconstraints-one-shot.scm" #f #f)
(printf "=== Finished testing one-shot underconstraints ===\n")

#|
(printf "=== Testing general underconstraints ===\n")
(test-under "test-underconstraints-general.scm" 1 #t)
(test-under "test-underconstraints-general.scm" 1000 #t)
(test-under "test-underconstraints-general.scm" 1000000 #t)
;;
(test-under "test-underconstraints-general.scm" #f #t)
;;
(test-under "test-underconstraints-general.scm" 1 #f)
(test-under "test-underconstraints-general.scm" 1000 #f)
(test-under "test-underconstraints-general.scm" 1000000 #f)
;;
(test-under "test-underconstraints-general.scm" #f #f)
(printf "=== Finished testing general underconstraints ===\n")
|#
