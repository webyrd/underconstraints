(load "load.scm")
(load "restricted-interp.scm")
(load "restricted-interp-condg.scm")

(*trace-underconstraint-param* #f)


(test "condg commit"
 (run* (q)
   (freshg (x)
     (== x 1)
     (condg
      ([] [(== x 1)] [(== q 1)])
      ([] [(== x 2)] [(== q 2)]))))
 '(1))


(test "condg nondet"
 (run* (q)
   (underconstraino
    'u1
    '()
    (freshg (x)
      (condg
       ([] [(== x 1)] [(== q 1)])
       ([] [(== x 2)] [(== q 2)])))))
 '(_.0))

(test "condg commit outer, nondet inner, nested"
      (run* (q)
        (underconstraino
         'u1
         '()
         (freshg (x y a b)
                 (== q (cons a b))
                 (== x 1)
                 (condg
                  ([]
                   [(== x 1)]
                   [(== a 1)
                    (condg
                     ([] [(== y 1)] [(== b 1)])
                     ([] [(== y 2)] [(== b 2)]))])
                  ([]
                   [(== x 2)]
                   [(== a 2)])))))
      '((1 . _.0)))


(test "condg commit first, nondet second, conjunction"
      (run* (q)
        (underconstraino
         'u1
         '()
         (freshg (x y a b)
                 (== q (cons a b))
                 (== x 1)
                 (condg
                  ([] [(== x 1)] [(== a 1)])
                  ([] [(== x 2)] [(== a 2)]))
                 (condg
                  ([] [(== y 1)] [(== b 1)])
                  ([] [(== y 2)] [(== b 2)])))))
      '((1 . _.0)))

(test "condg nondet first, det second; commits second"
      (run* (q)
        (underconstraino
         'u1
         '()
         (freshg (x y a b)
           (== q (cons a b))
           (== y 1)
           (condg
            ([] [(== x 1)] [(== a 1)])
            ([] [(== x 2)] [(== a 2)]))
           (condg
            ([] [(== y 1)] [(== b 1)])
            ([] [(== y 2)] [(== b 2)])))))
      '((_.0 . 1)))


(test "condg interp should eval ground program 1"
      (run 1 (q)
        (underconstraino
         'u1
         '()
         (evalo/g '1 q)))
      '(1))



(test "condg interp should eval ground program 2"
      (run 1 (q)
        (underconstraino
         'u1
         '()
         (evalo/g '(cons 1 '()) q)))
      '((1)))


(test "condg interp should eval ground program identity"
  (run 1 (q)
    (underconstraino
         'u1
         '()
         (evalo/g '(letrec ((double (lambda (l) : ((list) -> list)
                                          l)))
                         (double (cons 1 (cons 2 (cons 3 '()))))) q)))
  '((1 2 3)))


(test "condg interp should eval ground program cons-if-eq"
      (run 1 (q)
        (underconstraino
         'u1
         '()
         (evalo/g '(letrec ((cons-if-= (lambda (v1 v2 l) : ((number number list) -> list)
                                         (if (= v1 v2)
                                             (cons v1 l)
                                             l))))
                     (cons-if-= 1 1 (cons 1 (cons 2 (cons 3 '()))))) q)))
      '((1 1 2 3)))


(test "condg interp should eval ground program rember 1"
      (run 1 (q)
        (underconstraino
         'u1
         '()
         (evalo/g '(letrec ((rember (lambda (e l) : ((number list) -> list)
                                            (match l
                                              ('() l)
                                              ((cons _.0 _.1) (if (= _.0 e) _.1 (cons _.0 (rember e _.1))))))))
                           (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '()))))))
                        q)))
      '((3 4 6 7)))


(test "condg interp should eval ground program rember 2"
      (run 1 (q)
        (underconstraino
         'u1
         '()
         (evalo/g '(letrec ((rember (lambda (e l) : ((number list) -> list)
                                            (match l
                                              ('() l)
                                              ((cons _.0 _.1) (if (= _.0 e) _.1 (cons _.0 (rember e _.1))))))))
                           (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '()))))))
                        q)))
      '((3 4 6)))


(time (test "interp eval ground program rember all"
  (run 1 (v1 v2 v3 v4)
    (fresh (q)
      (== q '(match l
               ('() l)
               ((cons a d) (if (= a e) d (cons a (rember e d))))))
      (absento 3 q)
      (absento 4 q)
      (absento 5 q)
      (absento 6 q)
      (absento 7 q)
      (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                   (match l
                                     ('() l)
                                     ((cons a d) (if (= a e) d (cons a (rember e d))))))))
                (rember 5 '()))
             v1)
    
      (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                 ,q)))
                (rember 6 (cons 6 '())))
             v2)
    
      (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                 ,q)))
                (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '()))))))
             v3)
    
      (evalo `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                 ,q)))
                (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '()))))))
             v4)))
  '((() () (3 4 6) (3 4 6 7)))))


(time (test "condg interp eval ground program rember all"
  (run 1 (v1 v2 v3 v4)
    (fresh (q)
      (== q '(match l
               ('() l)
               ((cons a d) (if (= a e) d (cons a (rember e d))))))
      (absento 3 q)
      (absento 4 q)
      (absento 5 q)
      (absento 6 q)
      (absento 7 q)
      (underconstraino
       'u1
       '()
       (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                    (match l
                                      ('() l)
                                      ((cons a d) (if (= a e) d (cons a (rember e d))))))))
                   (rember 5 '()))
                v1))

      (underconstraino
       'u2
       '()
       (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                    ,q)))
                   (rember 6 (cons 6 '())))
                v2))

      (underconstraino
       'u3
       '()
       (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                    ,q)))
                   (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '()))))))
                v3))

      (underconstraino
       'u4
       '()
       (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                    ,q)))
                   (rember 5 (cons 3 (cons 4 (cons 6 (cons 7 '()))))))
                v4))))
  '((() () (3 4 6) (3 4 6 7)))))


(*trace-underconstraint-param* #f)
(define max-ticks 10000000)

(time (test "synthesize rember with 3 top-level general underconstraint"
            (run 1 (q)
              (absento 3 q)
              (absento 4 q)
              (absento 5 q)
              (absento 6 q)
              (absento 7 q)
       
              (underconstraino
               'u1
               q
               (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                         ,q)))
                        (rember 5 '())) '())
               max-ticks)
    
              (underconstraino
               'u2
               q
               (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                            ,q)))
                           (rember 6 (cons 6 '()))) '())
               max-ticks)

              (underconstraino
               'u3
               q
               (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
                                            ,q)))
                           (rember 7 (cons 3 (cons 4 (cons 7 (cons 6 '())))))) '(3 4 6))
               max-ticks)
              
              (underconstraino
               'u4
               q
               (evalo/g `(letrec ((rember (lambda (e l) : ((number list) -> list)
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