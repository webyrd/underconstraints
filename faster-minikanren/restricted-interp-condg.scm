; Relational interpreter emulating ideas from "Type-and-example-directed program synthesis"
; by 	Peter-Michael Osera	and Steve Zdancewic (http://dl.acm.org/citation.cfm?id=2738007)
; From https://github.com/michaelballantyne/scheme-workshop-2015

(define empty-env/g '())

(define (lookupo/g x env t type)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (condg
     ([]
      [(== x y)]
      [(condg
        ([] [(== `(val ,type . ,t) b)] [])
        ([lam-expr] [(== `(rec ,type . ,lam-expr) b) (== `(closure ,lam-expr ,env) t)] []))])
     ([] [(=/= x y)] [(lookupo/g x rest t type)]))))

;; TODO: is it good or bad that this stalls when it gets to ambiguity, despite being used in guard?
(define (not-in-envo/g x env)
  (condg
   ([] [(== empty-env/g env)] [])
   ([y b rest]
    [(== `((,y . ,b) . ,rest) env)
     (=/= y x)]
    [(not-in-envo/g x rest)])))

(define (list-of-symbolso/g los)
  (condg
   ([] [(== '() los)] [])
   ([a d]
    [(== `(,a . ,d) los)
     (symbolo a)]
    [(list-of-symbolso/g d)])))

(define (eval-listo/g expr env val type)
  (condg
   ([] [(== '() expr) (== '() val)] [])
   ([a d v-a v-d t-a t-d]
    [(== `(,a . ,d) expr)
     (== `(,v-a . ,v-d) val)
     (== `(,t-a . ,t-d) type)]
    [(eval-expo/g a env v-a 'I t-a)
     (eval-listo/g d env v-d t-d)])))

(define (ext-env*o/g x* a* t* env out)
  (condg
   ([] [(== '() x*) (== '() a*) (== env out)] [])
   ([x a dx* da* env2 t dt*]
    [(== `(,x . ,dx*) x*)
     (== `(,a . ,da*) a*)
     (== `(,t . ,dt*) t*)
     (== `((,x . (val ,t . ,a)) . ,env) env2)
     (symbolo x)
     (symbolo t)]
    [(ext-env*o/g dx* da* dt* env2 out)])))

(define (evalo/g expr val)
  (fresh (type)
    (eval-expo/g expr empty-env/g val 'I type)))

(define (eval-expo/g expr env val EI type)
  (condg
   ([] [(symbolo expr)] [(lookupo/g expr env val type)])
   ([] [(== EI 'I)
        (== type 'list)
        (== '(quote ()) expr)
        (== '() val)
        (not-in-envo/g 'quote env)] [])
   ([e1 e2 v1 v2]
    [(== EI 'I)
     (== type 'list)
     (== `(cons ,e1 ,e2) expr)
     (== `(,v1 . ,v2) val)
     (not-in-envo/g 'cons env)]
    [(eval-expo/g e1 env v1 'I 'number)
     (eval-expo/g e2 env v2 'I 'list)])
   ([rator x* rands body env^ a* at* res]
    [(== `(,rator . ,rands) expr)
     (condg ;; need to make nonoverlapping with syntactic forms
      ([cv ct] [(lookupo/g rator env cv ct)] []) ;; rator is var
      ([a d] [(== rator (cons a d))] []))]       ;; rator is pair
    [(eval-expo/g rator env `(closure (lambda ,x* ,body) ,env^) 'E `(,at* -> ,type))
     (eval-listo/g rands env a* at*)
     (ext-env*o/g x* a* at* env^ res)
     (eval-expo/g body res val 'I type)])
   ([p-name x body letrec-body ftype]
    [(== EI 'I)
     (== `(letrec ((,p-name (lambda ,x : ,ftype ,body)))
            ,letrec-body)
         expr)
     (not-in-envo/g 'letrec env)]
    [(list-of-symbolso/g x)
     (eval-expo/g letrec-body
                  `((,p-name . (rec ,ftype . (lambda ,x ,body))) . ,env)
                  val 'I type)])
   ([e1 e2 e3 v1 s1 s2]
    [(== EI 'I)
     (== `(match ,e1
            ('() ,e2)
            ((cons ,s1 ,s2) ,e3)) expr)
     (symbolo s1)
     (symbolo s2)
     (not-in-envo/g 'match env)]
    [(eval-expo/g e1 env v1 'E 'list)
     (condg
      ([] [(== '() v1)] [(eval-expo/g e2 env val 'I type)])
      ([a d]
       [(== `(,a . ,d) v1)
        (=/= a 'closure)]
       [(eval-expo/g e3 `((,s1 . (val number . ,a)) (,s2 . (val list . ,d)) . ,env) val 'I type)]))])
   ([e1 e2 e3 e4 v1 v2]
    [(== EI 'I)
     (== `(if (= ,e1 ,e2)
              ,e3
              ,e4) expr)
     (not-in-envo/g 'if env)]
    [(eval-expo/g e1 env v1 'E 'number)
     (eval-expo/g e2 env v2 'E 'number)
     (condg
      ([] [(== v1 v2)] [(eval-expo/g e3 env val 'I type)])
      ([] [(=/= v1 v2)] [(eval-expo/g e4 env val 'I type)]))])
      
   
   ([] [(== EI 'I) (== type 'number) (numbero expr) (== expr val)] [])

   ))
