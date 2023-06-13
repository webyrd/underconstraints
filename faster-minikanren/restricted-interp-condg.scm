; Relational interpreter emulating ideas from "Type-and-example-directed program synthesis"
; by 	Peter-Michael Osera	and Steve Zdancewic (http://dl.acm.org/citation.cfm?id=2738007)
; From https://github.com/michaelballantyne/scheme-workshop-2015

(define empty-env/g '())

(define (lookupo/g x env t type)
  (freshg (y b rest)
    (==g `((,y . ,b) . ,rest) env)
    (condg
     ([]
      [(==g x y)]
      [(condg
        ([] [(==g `(val ,type . ,t) b)] [])
        ([lam-expr] [(==g `(rec ,type . ,lam-expr) b) (==g `(closure ,lam-expr ,env) t)] []))])
     ([] [(=/=g x y)] [(lookupo/g x rest t type)]))))

;; TODO: is it good or bad that this stalls when it gets to ambiguity, despite being used in guard?
#;(define (not-in-envo/g x env)
  (condg
   ([] [(==g empty-env/g env)] [])
   ([y b rest]
    [(==g `((,y . ,b) . ,rest) env)
     (=/=g y x)]
    [(not-in-envo/g x rest)])))

(define (list-of-symbolso/g los)
  (condg
   ([] [(==g '() los)] [])
   ([a d]
    [(==g `(,a . ,d) los)
     (symbolog a)]
    [(list-of-symbolso/g d)])))

(define (eval-listo/g expr env val type)
  (condg
   ([] [(==g '() expr) (==g '() val)] [])
   ([a d v-a v-d t-a t-d]
    [(==g `(,a . ,d) expr)
     (==g `(,v-a . ,v-d) val)
     (==g `(,t-a . ,t-d) type)]
    [(eval-expo/g a env v-a 'I t-a)
     (eval-listo/g d env v-d t-d)])))

(define (ext-env*o/g x* a* t* env out)
  (condg
   ([] [(==g '() x*) (==g '() a*) (==g env out)] [])
   ([x a dx* da* env2 t dt*]
    [(==g `(,x . ,dx*) x*)
     (==g `(,a . ,da*) a*)
     (==g `(,t . ,dt*) t*)
     (==g `((,x . (val ,t . ,a)) . ,env2) out)
     (symbolog x)
     (symbolog t)]
    [(ext-env*o/g dx* da* dt* env env2)])))

(define (evalo/g expr val)
  (freshg (type)
    (eval-expo/g expr empty-env/g val 'I type)))

(define (eval-expo/g expr env val EI type)
  (condg
   ([] [(symbolog expr)] [(lookupo/g expr env val type)])
   ([] [(==g EI 'I)
        (==g type 'list)
        (==g '(quote ()) expr)
        (==g '() val)
        #;(not-in-envo/g 'quote env)] [])
   ([e1 e2 v1 v2]
    [(==g EI 'I)
     (==g type 'list)
     (==g `(cons ,e1 ,e2) expr)
     (==g `(,v1 . ,v2) val)
     #;(not-in-envo/g 'cons env)]
    [(eval-expo/g e1 env v1 'I 'number)
     (eval-expo/g e2 env v2 'I 'list)])
   ([rator x* rands body env^ a* at* res]
    [(==g `(,rator . ,rands) expr)
     (condg ;; need to make nonoverlapping with syntactic forms
      ([cv ct] [(symbolog rator)] [(absentog rator '(quote cons letrec match if))]) ;; rator is var
      ([a d] [(==g rator (cons a d))] []))]       ;; rator is pair
    [(eval-expo/g rator env `(closure (lambda ,x* ,body) ,env^) 'E `(,at* -> ,type))
     (eval-listo/g rands env a* at*)
     (ext-env*o/g x* a* at* env^ res)
     (eval-expo/g body res val 'I type)])
   ([p-name x body letrec-body ftype]
    [(==g EI 'I)
     (==g `(letrec ((,p-name (lambda ,x : ,ftype ,body)))
            ,letrec-body)
         expr)
     #;(not-in-envo/g 'letrec env)]
    [(list-of-symbolso/g x)
     (eval-expo/g letrec-body
                  `((,p-name . (rec ,ftype . (lambda ,x ,body))) . ,env)
                  val 'I type)])
   ([e1 e2 e3 v1 s1 s2]
    [(==g EI 'I)
     (==g `(match ,e1
            ('() ,e2)
            ((cons ,s1 ,s2) ,e3)) expr)
     (symbolog s1)
     (symbolog s2)
     #;(not-in-envo/g 'match env)]
    [(eval-expo/g e1 env v1 'E 'list)
     (condg
      ([] [(==g '() v1)] [(eval-expo/g e2 env val 'I type)])
      ([a d]
       [(==g `(,a . ,d) v1)
        (=/=g a 'closure)]
       [(eval-expo/g e3 `((,s1 . (val number . ,a)) (,s2 . (val list . ,d)) . ,env) val 'I type)]))])
   ([e1 e2 e3 e4 v1 v2]
    [(==g EI 'I)
     (==g `(if (= ,e1 ,e2)
              ,e3
              ,e4) expr)
     #;(not-in-envo/g 'if env)]
    [(eval-expo/g e1 env v1 'E 'number)
     (eval-expo/g e2 env v2 'E 'number)
     (condg
      ([] [(==g v1 v2)] [(eval-expo/g e3 env val 'I type)])
      ([] [(=/=g v1 v2)] [(eval-expo/g e4 env val 'I type)]))])
      
   
   ([] [(==g EI 'I) (==g type 'number) (numberog expr) (==g expr val)] [])

   ))
