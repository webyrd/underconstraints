(load "load.scm")

(*trace-underconstraint-param* #f)


(test "condg commit"
 (run* (q)
   (fresh (x)
     (== x 1)
     (condg
      ([] [(== x 1)] [(== q 1)])
      ([] [(== x 2)] [(== q 2)]))))
 '(1))

(test "condg nondet"
 (run* (q)
   (fresh (x)
     (condg
      ([] [(== x 1)] [(== q 1)])
      ([] [(== x 2)] [(== q 2)]))))
 '(_.0))

(test "condg commit outer, nondet inner, nested"
 (run* (q)
   (fresh (x y a b)
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
       [(== a 2)]))))
 '((1 . _.0)))

(test "condg commit first, nondet second, conjunction"
 (run* (q)
   (fresh (x y a b)
     (== q (cons a b))
     (== x 1)
     (condg
      ([] [(== x 1)] [(== a 1)])
      ([] [(== x 2)] [(== a 2)]))
     (condg
      ([] [(== y 1)] [(== b 1)])
      ([] [(== y 2)] [(== b 2)]))))
 '((1 . _.0)))

(test "condg nondet first, det second; commits second"
 (run* (q)
   (fresh (x y a b)
     (== q (cons a b))
     (== y 1)
     (condg
      ([] [(== x 1)] [(== a 1)])
      ([] [(== x 2)] [(== a 2)]))
     (condg
      ([] [(== y 1)] [(== b 1)])
      ([] [(== y 2)] [(== b 2)]))))
 '((_.0 . 1)))