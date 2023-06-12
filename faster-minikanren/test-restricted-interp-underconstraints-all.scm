(load "load.scm")
(load "restricted-interp.scm")

;; (*underconstraint-default-timeout-param* #f)
(*trace-underconstraint-param* #f)

; Synthesize rember no underconstraints
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


(*trace-underconstraint-param* #f)
(define max-ticks 100000)

(time (test "synthesize rember with 3 top-level general underconstraint"
  (run 1 (q)
       (absento 3 q)
       (absento 4 q)
       (absento 5 q)
       (absento 6 q)
       (absento 7 q)

       (underconstraino
        'u2
        q
        (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 6 (cons 6 '()))) '())
        max-ticks)

       (underconstraino
        'u3
        q
        (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                  ,q)))
                 (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '())))))) '(3 4 6))
        max-ticks)
              
       (underconstraino
        'u4
        q
        (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                         ,q)))
                  (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7))
        max-ticks)       

       
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

#!eof

max-ticks 300000:
Testing "synthesize rember with 3 top-level general underconstraints"
*engine-completed-counter*: 35_638
*engine-timedout-counter*: 32_231
*immature-stream-counter*: 36_219_862
*fail-counter*: 16_021
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 19_617
(time (test "synthesize rember with one top-level general underconstraint" ...))
    9484 collections
    100.509216132s elapsed cpu time, including 2.810983754s collecting
    100.547784000s elapsed real time, including 2.829864000s collecting
    79546106976 bytes allocated, including 79541754736 bytes reclaimed

max-ticks 200_000:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 28_531
*engine-timedout-counter*: 37_207
*immature-stream-counter*: 29_954_121
*fail-counter*: 14_788
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 13_743
(time (test "synthesize rember with one top-level general underconstraint" ...))
    6870 collections
    87.606173145s elapsed cpu time, including 2.627853908s collecting
    87.816104000s elapsed real time, including 2.657688000s collecting
    57617737584 bytes allocated, including 57622117632 bytes reclaimed

max-ticks 100_000:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 10479
*engine-timedout-counter*: 51072
*immature-stream-counter*: 20948101
*fail-counter*: 7893
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 2586
(time (test "synthesize rember with one top-level general underconstraint" ...))
    3667 collections
    47.261424664s elapsed cpu time, including 1.514597804s collecting
    47.339939000s elapsed real time, including 1.534679000s collecting
    30754646384 bytes allocated, including 30759225312 bytes reclaimed

max-ticks 10_000:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88937
*immature-stream-counter*: 6750794
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    991 collections
    12.244019016s elapsed cpu time, including 1.374843767s collecting
    12.280039000s elapsed real time, including 1.381867000s collecting
    8316831680 bytes allocated, including 8289373328 bytes reclaimed
