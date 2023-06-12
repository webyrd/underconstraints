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

(time (test "synthesize rember with one top-level general underconstraint"
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
                  (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7))
        1000000)
       
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

1_000_000 ticks
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 41_443
*engine-timedout-counter*: 28_556
*immature-stream-counter*: 67_709_357
*fail-counter*: 17_985
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 23_458
(time (test "synthesize rember with one top-level general underconstraint" ...))
    26317 collections
    292.911830595s elapsed cpu time, including 10.088755338s collecting
    293.385975000s elapsed real time, including 10.208497000s collecting
    220723582160 bytes allocated, including 220720522464 bytes reclaimed

100_000 ticks:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 10_513
*engine-timedout-counter*: 51_072
*immature-stream-counter*: 20_965_801
*fail-counter*: 7_909
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 2_604
(time (test "synthesize rember with one top-level general underconstraint" ...))
    3670 collections
    39.422875490s elapsed cpu time, including 1.144207272s collecting
    39.435118000s elapsed real time, including 1.150990000s collecting
    30784896320 bytes allocated, including 30785695568 bytes reclaimed

10_000 ticks:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88_935
*immature-stream-counter*: 6_752_935
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    988 collections
    10.416775672s elapsed cpu time, including 1.114216824s collecting
    10.429295000s elapsed real time, including 1.117715000s collecting
    8295479504 bytes allocated, including 8265190384 bytes reclaimed

1000 ticks:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88935
*immature-stream-counter*: 696362
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    261 collections
    4.206881667s elapsed cpu time, including 0.857496191s collecting
    4.209522000s elapsed real time, including 0.858939000s collecting
    2190057264 bytes allocated, including 2153956848 bytes reclaimed

100 ticks:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88935
*immature-stream-counter*: 177872
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    214 collections
    3.293842676s elapsed cpu time, including 0.746446416s collecting
    3.294067000s elapsed real time, including 0.747049000s collecting
    1792358272 bytes allocated, including 1755858144 bytes reclaimed

10 ticks:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88935
*immature-stream-counter*: 177870
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    201 collections
    3.174206398s elapsed cpu time, including 0.734988831s collecting
    3.174327000s elapsed real time, including 0.735518000s collecting
    1688634688 bytes allocated, including 1650099456 bytes reclaimed

1 tick:
Testing "synthesize rember with one top-level general underconstraint"
*engine-completed-counter*: 0
*engine-timedout-counter*: 88935
*immature-stream-counter*: 0
*fail-counter*: 0
*singleton-succeed-counter*: 0
*non-singleton-succeed-counter*: 0
(time (test "synthesize rember with one top-level general underconstraint" ...))
    197 collections
    3.144889603s elapsed cpu time, including 0.739814187s collecting
    3.145316000s elapsed real time, including 0.740445000s collecting
    1650109456 bytes allocated, including 1621238576 bytes reclaimed
