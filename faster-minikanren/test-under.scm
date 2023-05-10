(load "mk-vicare.scm")
(load "mk.scm")
(load "numbers.scm")
(load "test-check.scm")

#|
;; can set global underconstraint parameter values:

  (*underconstraint-default-timeout-param* #f)
  (*trace-underconstraint-param* #f)
|#

(define *total-num-underconstraint-tests-failed* 0)

(define (test-under file timeout trace?)
  (set! *num-tests-failed* 0)
  (printf
   "\n*** testing ~s: `timeout` = ~s  `trace?` = ~s ***\n"
   file
   timeout
   trace?)
  (*underconstraint-default-timeout-param* timeout)
  (*trace-underconstraint-param* trace?)
  (load file)
  (set! *total-num-underconstraint-tests-failed*
        (+ *total-num-underconstraint-tests-failed* *num-tests-failed*))
  (printf
   "*** finished testing ~s ***\n timeout = ~s\n trace? = ~s\n"
   file
   timeout
   trace?)
  (if (zero? *num-tests-failed*)
      (printf "no tests failed in this underconstraint test run\n")
      (printf "!!! ~s test(s) failed in this underconstraint test run !!!\n"
              *num-tests-failed*))
  (if (zero? *total-num-underconstraint-tests-failed*)
      (printf "no underconstraint tests have failed across all test\n")
      (printf "!!! ~s underconstraint test(s) have failed across all test !!!\n"
              *total-num-underconstraint-tests-failed*)))

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
