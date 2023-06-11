; Relational interpreter emulating ideas from "Type-and-example-directed program synthesis"
; by 	Peter-Michael Osera	and Steve Zdancewic (http://dl.acm.org/citation.cfm?id=2738007)
; From https://github.com/michaelballantyne/scheme-workshop-2015

(define empty-env '())


(define (lookupo x env t type)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val ,type . ,t) b))
         ((fresh (lam-expr)
            (== `(rec ,type . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t type)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(define (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(define (eval-listo expr env val type)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d t-a t-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (== `(,t-a . ,t-d) type)
       (eval-expo a env v-a 'I t-a)
       (eval-listo d env v-d t-d)))))

(define (ext-env*o x* a* t* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2 t dt*)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `(,t . ,dt*) t*)
       (== `((,x . (val ,t . ,a)) . ,env) env2)
       (symbolo x)
       (symbolo t)
       (ext-env*o dx* da* dt* env2 out)))))

(define (evalo expr val)
  (fresh (type)
    (eval-expo expr empty-env val 'I type)))

(define (eval-expo expr env val EI type)
  (conde
    ((symbolo expr)
     ; EI can be either E or I here
     (lookupo expr env val type))

    ((== EI 'I)
     (== type 'list)
     (== '(quote ()) expr)
     (== '() val)
     (not-in-envo 'quote env))

    ((== EI 'I)
     (== type 'list)
     (fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v1 . ,v2) val)
       (not-in-envo 'cons env)
       (eval-expo e1 env v1 'I 'number)
       (eval-expo e2 env v2 'I 'list)))

    ((fresh (rator x* rands body env^ a* at* res)
       (== `(,rator . ,rands) expr)
       ; EI can be either E or I here
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^) 'E `(,at* -> ,type))
       (eval-listo rands env a* at*)
       (ext-env*o x* a* at* env^ res)
       (eval-expo body res val 'I type)))

    ((== EI 'I)
     (fresh (p-name x body letrec-body ftype)
       (== `(letrec ((,p-name (lambda ,x : ,ftype ,body)))
              ,letrec-body)
           expr)
       (list-of-symbolso x)
       (not-in-envo 'letrec env)
       (eval-expo letrec-body
                  `((,p-name . (rec ,ftype . (lambda ,x ,body))) . ,env)
                  val 'I type)))

    ((== EI 'I)
     (fresh (e1 e2 e3 v1 s1 s2)
       (== `(match ,e1
              ('() ,e2)
              ((cons ,s1 ,s2) ,e3)) expr)
       (symbolo s1)
       (symbolo s2)
       (not-in-envo 'match env)
       (eval-expo e1 env v1 'E 'list)
       (conde
         [(== '() v1)
          (eval-expo e2 env val 'I type)]
         [(fresh (a d)
            (== `(,a . ,d) v1)
            (=/= a 'closure)
            (eval-expo e3 `((,s1 . (val number . ,a)) (,s2 . (val list . ,d)) . ,env) val 'I type))])))

    ((== EI 'I)
     (fresh (e1 e2 e3 e4 v1 v2)
       (== `(if (= ,e1 ,e2)
              ,e3
              ,e4) expr)
       (not-in-envo 'if env)
       (eval-expo e1 env v1 'E 'number)
       (eval-expo e2 env v2 'E 'number)
       (conde
         [(== v1 v2)
          (eval-expo e3 env val 'I type)]
         [(=/= v1 v2)
          (eval-expo e4 env val 'I type)])
       ))

    ((== EI 'I) (== type 'number) (numbero expr) (== expr val))))



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
