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

(time (test "synthesize rember with one top-level general underconstraint"
  (run 1 (q)
       (absento 3 q)
       (absento 4 q)
       (absento 5 q)
       (absento 6 q)
       (absento 7 q)

       (underconstraino
        'babbys-first-underconstraint
        q
        (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                         ,q)))
                  (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '(3 4 6 7))
        1)
       
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
