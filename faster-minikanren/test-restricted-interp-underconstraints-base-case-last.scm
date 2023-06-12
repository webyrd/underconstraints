(load "load.scm")
(load "restricted-interp.scm")

;; (*underconstraint-default-timeout-param* #f)
(*trace-underconstraint-param* #f)

(time (test "synthesize rember no underconstraints"
  (run 1 (q)
       (absento 3 q)
       (absento 4 q)
       (absento 5 q)
       (absento 6 q)
       (absento 7 q)
       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 5 '())) '())

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 6 (cons 6 '()))) '())

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '())))))) '(3 4 6))

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7)))
  '(((match l
       ('() l)
       ((cons _.0 _.1) (if (= _.0 e) _.1 (cons _.0 (rember e _.1)))))
     (=/=
       ((_.0 _.1))
       ((_.0 cons))
       ((_.0 e))
       ((_.0 if))
       ((_.0 rember))
       ((_.1 cons))
       ((_.1 e))
       ((_.1 if))
       ((_.1 rember)))
     (sym _.0 _.1)))))

(time (test "synthesize rember no underconstraints base-case last"
  (run 1 (q)
       (absento 3 q)
       (absento 4 q)
       (absento 5 q)
       (absento 6 q)
       (absento 7 q)
       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 6 (cons 6 '()))) '())

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '())))))) '(3 4 6))

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7))

       (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 5 '())) '()))
  '(((match l
       ('() l)
       ((cons _.0 _.1) (if (= _.0 e) _.1 (cons _.0 (rember e _.1)))))
     (=/=
       ((_.0 _.1))
       ((_.0 cons))
       ((_.0 e))
       ((_.0 if))
       ((_.0 rember))
       ((_.1 cons))
       ((_.1 e))
       ((_.1 if))
       ((_.1 rember)))
     (sym _.0 _.1)))))


(*trace-underconstraint-param* #f)
(define max-ticks 1000000)

(time (test "synthesize rember with one top-level general underconstraint base-case last"
  (run 1 (q)
    (absento 3 q)
    (absento 4 q)
    (absento 5 q)
    (absento 6 q)
    (absento 7 q)
    
    (underconstraino
     'u1
     q
     (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                      ,q)))
               (rember 5 '())) '())
     max-ticks)

       
    (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                     ,q)))
              (rember 6 (cons 6 '()))) '())

    (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                     ,q)))
              (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '())))))) '(3 4 6))

    (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                     ,q)))
              (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7))

    (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                     ,q)))
              (rember 5 '())) '()))
  '(((match l
       ('() l)
       ((cons _.0 _.1) (if (= _.0 e) _.1 (cons _.0 (rember e _.1)))))
     (=/=
       ((_.0 _.1))
       ((_.0 cons))
       ((_.0 e))
       ((_.0 if))
       ((_.0 rember))
       ((_.1 cons))
       ((_.1 e))
       ((_.1 if))
       ((_.1 rember)))
     (sym _.0 _.1)))))

#!eof

max-ticks 1_000_000
Testing "synthesize rember with one top-level general underconstraint base-case last"
*engine-completed-counter*: 152_402
*engine-timedout-counter*: 13_899
*immature-stream-counter*: 41_984_048
*fail-counter*: 10_648
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 141_754
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    15278 collections
    146.604183149s elapsed cpu time, including 10.803580174s collecting
    146.801536000s elapsed real time, including 10.862592000s collecting
    128190182000 bytes allocated, including 128150566000 bytes reclaimed

max-ticks 100_000:
Testing "synthesize rember with one top-level general underconstraint base-case last"
*engine-completed-counter*: 147_832
*engine-timedout-counter*: 17_755
*immature-stream-counter*: 20_865_094
*fail-counter*: 9384
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 138_448
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    3628 collections
    49.926226171s elapsed cpu time, including 4.417145891s collecting
    50.003483000s elapsed real time, including 4.435720000s collecting
    30446082960 bytes allocated, including 30391046752 bytes reclaimed

max-ticks 10_000:
Testing "synthesize rember with one top-level general underconstraint base-case last"
*engine-completed-counter*: 27
*engine-timedout-counter*: 167_630
*immature-stream-counter*: 10_220_255
*fail-counter*: 23
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 4
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    1677 collections
    24.913886971s elapsed cpu time, including 2.797574107s collecting
    24.917545000s elapsed real time, including 2.801184000s collecting
    14067799504 bytes allocated, including 14033198816 bytes reclaimed


max-ticks 1000:
*engine-completed-counter*: 0
*engine-timedout-counter*: 167649
*immature-stream-counter*: 1290932
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    628 collections
    14.809458008s elapsed cpu time, including 2.317793243s collecting
    14.812109000s elapsed real time, including 2.319961000s collecting
    5269433904 bytes allocated, including 5232802368 bytes reclaimed

max-ticks 1:
Testing "synthesize rember with one top-level general underconstraint base-case last"
*engine-completed-counter*: 0
*engine-timedout-counter*: 167649
*immature-stream-counter*: 0
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint base-case last" ...))
    505 collections
    14.155355142s elapsed cpu time, including 2.237991112s collecting
    14.165343000s elapsed real time, including 2.240763000s collecting
    4233399328 bytes allocated, including 4162380528 bytes reclaimed
