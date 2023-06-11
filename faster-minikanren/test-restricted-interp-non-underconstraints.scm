(load "load.scm")
(load "restricted-interp.scm")

(test "identity"
  (run 1 (q) (evalo '(letrec ((double (lambda (l) : ((list) -> list)
                                        l)))
                       (double (cons 1 (cons 2 (cons 3 '()))))) q))
  '((1 2 3)))


(time (test "synthesize double"
        (run 1 (q)
             (absento '4 q)
             (absento '5 q)
             (absento '6 q)
             (evalo `(letrec ((double (lambda (l) : ((list) -> list)
                                        ,q)))
                       (double '())) '())
             (evalo `(letrec ((double (lambda (l) : ((list) -> list)
                                        ,q)))
                       (double (cons 4 (cons 5 (cons 6 '()))))) '(4 4 5 5 6 6))
             )
        '(((match l
             ('() l)
             ((cons _.0 _.1) (cons _.0 (cons _.0 (double _.1)))))
           (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 double)) ((_.1 cons)) ((_.1 double))) (sym _.0 _.1)))))

(time (test "synthesize append"
        (run 1 (q)
             (absento '3 q)
             (absento '4 q)
             (absento '5 q)
             (absento '6 q)
             (absento '7 q)
             (evalo `(letrec ((append (lambda (l s) : ((list list) -> list)
                                        ,q)))
                       (append '() (cons 5 (cons 6 '())))) '(5 6))
             (evalo `(letrec ((append (lambda (l s) : ((list list) -> list)
                                        ,q)))
                       (append (cons 3 (cons 4 (cons 5 '()))) (cons 6 (cons 7 '())))) '(3 4 5 6 7))
             )
        '(((match l
             ('() s)
             ((cons _.0 _.1) (cons _.0 (append _.1 s))))
           (=/= ((_.0 _.1)) ((_.0 append)) ((_.0 cons)) ((_.0 s)) ((_.1 append)) ((_.1 cons)) ((_.1 s))) (sym _.0 _.1)))))


(test "member forwards"
  (run 1 (q)
       (evalo `(letrec ((member (lambda (e l) : ((number list) -> list)
                                   (match l
                                     ('() '())
                                     ((cons a d) (if (= a e)
                                                   (cons e '())
                                                   (member e d)))))))
                 (member 5 (cons 3 (cons 4 (cons 5 (cons 6 '())))))) q)
       )
  '((5)))



; Synthesize member
(time (test "synthesize member"
        (run 1 (q)
             (absento 5 q)
             (absento 6 q)
             (absento 7 q)
             (absento 5 q)
             (evalo `(letrec ((member? (lambda (e l) : ((number list) -> list)
                                         ,q)))
                       (member? 5 '())) '())

             (evalo `(letrec ((member? (lambda (e l) : ((number list) -> list)
                                         ,q)))
                       (member? 6 (cons 6 '()))) '(6))

             (evalo `(letrec ((member? (lambda (e l) : ((number list) -> list)
                                         ,q)))
                       (member? 5 (cons 6 '()))) '())

             (evalo `(letrec ((member? (lambda (e l) : ((number list) -> list)
                                         ,q)))
                       (member? 5 (cons 3 (cons 4 (cons 6 (cons 5 '())))))) '(5))

             (evalo `(letrec ((member? (lambda (e l) : ((number list) -> list)
                                         ,q)))
                       (member? 5 (cons 3 (cons 4 (cons 6 (cons 7 '())))))) '())
             )
        '(((match l
             ('() l)
             ((cons _.0 _.1)
              (if (= _.0 e)
                l
                (member? e _.1))))
           (=/= ((_.0 _.1)) ((_.0 e)) ((_.0 if)) ((_.0 l)) ((_.0 member?)) ((_.1 e)) ((_.1 if)) ((_.1 l)) ((_.1 member?))) (sym _.0 _.1)))))


; Synthesize rember
(time (test "synthesize rember"
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
