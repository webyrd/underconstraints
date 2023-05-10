(define *num-tests-failed* 0)

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (begin
               (set! *num-tests-failed* (add1 *num-tests-failed*))
               (printf "Failed:\n")
               (pretty-print 'tested-expression)
               (newline)
               (printf "Expected:\n")
               (pretty-print expected)
               (newline)
               (printf "Computed:\n")
               (pretty-print produced)
               (newline))))))))
