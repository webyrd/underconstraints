(define *test-divergence-engine-timeout-param*
  (make-parameter 10000000))

(define *num-tests-failed* 0)

(define-syntax test
  (syntax-rules ()
    ((_ title test-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced test-expression))
         (or (equal? expected produced)
             (begin
               (set! *num-tests-failed* (add1 *num-tests-failed*))
               (printf "!! Failed:\n")
               (pretty-print 'test-expression)
               (newline)
               (printf "Expected:\n")
               (pretty-print expected)
               (newline)
               (printf "Computed:\n")
               (pretty-print produced)
               (newline))))))))

(define-syntax test-divergence
  (syntax-rules ()
    ((_ title test-expression)
     (let ((timeout-ticks (*test-divergence-engine-timeout-param*)))
       (printf "Testing divergence test ~s using engine with ~s ticks gas\n"
               title
               timeout-ticks)
       (let ((eng (make-engine (lambda () test-expression))))
         (time
          (eng timeout-ticks
               ;; engine "completed" procedure
               (lambda (ticks-left-over value)
                 (set! *num-tests-failed* (add1 *num-tests-failed*))
                 (printf "!! Divergence test failed:\n")
                 (pretty-print 'test-expression)
                 (newline)
                 (print "Engine unexpectedly completed after ~s of ~s ticks, returning\n"
                        (- timeout-ticks ticks-left-over)
                        timeout-ticks
                        value))
               ;; engine "expired" procedure
               (lambda (new-engine)
                 (printf "Engine timed out after ~s ticks, as expected\n"
                         timeout-ticks)))))))))
