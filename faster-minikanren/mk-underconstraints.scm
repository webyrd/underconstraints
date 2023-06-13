;; WEB 7 May 02023 -- 5 June 02023
;;
;; Extend faster-miniKanren's `mk.scm` with underconstraints.

(load "pmatch.scm")

#;(define-syntax check-type
  (syntax-rules ()
    [(_ v pred)
     (check-type-runtime v pred 'pred)]))

(define-syntax check-type
  (syntax-rules ()
    [(_ v pred)
     v]))

(define (check-type-runtime v pred pred-e)
  (if (pred v)
      v
      (error 'check-type (format "expected ~s, got ~s" pred-e v)))) 

(define *underconstraint-how-often-param*
  (make-parameter 1))

(define *underconstraint-depth-limit-1*
  (make-parameter 100000000000))

(define *underconstraint-depth-limit-2*
  (make-parameter 100))

;; Global default timeout parameter: number of ticks/gas for engine,
;; or `#f` if the default is no timeout.  Can be overriden by optional
;; timeout argument (number ticks or `#f`) to individual
;; underconstraint call.  Upon timeout being reached, the
;; underconstraint *succeeds* (necessary for soundness).
(define *underconstraint-default-timeout-param*
  (make-parameter #f))

;; Global Boolean parameter for whether to trace underconstraints,
;; including: the goal expression passed to the underconstraint,
;; whether the uderconstraint succeeded or failed, and timing/resource
;; usage information (including whether the underconstraint timed out,
;; and after how many ticks the timeout occurred).  Underconstraint
;; calls beginning with `trace-` (`trace-underconstraino`,
;; `trace-one-shot-underconstraino`) are always traced, regardless of
;; the value of this parameter.
(define *trace-underconstraint-param*
  (make-parameter #f))

(define *engine-completed-counter* 0)
(define *engine-timedout-counter* 0)
(define *depth-limit-1-cutoff-counter* 0)
(define *depth-limit-2-cutoff-counter* 0)

(define *==-counter* 0)
(define *==g-counter* 0)
(define *immature-stream-counter* 0)
(define *fail-counter* 0)
(define *singleton-succeed-counter* 0)
(define *non-singleton-succeed-counter* 0)
(define *user-counter* 0)

(define-syntax increment-counter!
  (syntax-rules ()
    [(_ c) (set! c (add1 c))]))

(define (reset-counters!)
  (set! *engine-completed-counter* 0)
  (set! *engine-timedout-counter* 0)
  (set! *depth-limit-1-cutoff-counter* 0)
  (set! *depth-limit-2-cutoff-counter* 0)
  ;;
  (set! *==-counter* 0)
  (set! *==g-counter* 0)
  (set! *immature-stream-counter* 0)
  (set! *fail-counter* 0)
  (set! *singleton-succeed-counter* 0)
  (set! *non-singleton-succeed-counter* 0)
  (set! *user-counter* 0))

(define (print-counters!)
  (printf "*engine-completed-counter*: ~s\n" *engine-completed-counter*)
  (printf "*engine-timedout-counter*: ~s\n" *engine-timedout-counter*)
  (printf "*depth-limit-1-cutoff-counter*: ~s\n" *depth-limit-1-cutoff-counter*)
  (printf "*depth-limit-2-cutoff-counter*: ~s\n" *depth-limit-2-cutoff-counter*)
  
  ;;
  (printf "*==-counter*: ~s\n" *==-counter*)
  (printf "*==g-counter*: ~s\n" *==g-counter*)
  (printf "*immature-stream-counter*: ~s\n" *immature-stream-counter*)
  (printf "*fail-counter*: ~s\n" *fail-counter*)
  (printf "*singleton-succeed-counter*: ~s\n" *singleton-succeed-counter*)
  (printf "*non-singleton-succeed-counter*: ~s\n" *non-singleton-succeed-counter*)
  (printf "*user-counter*: ~s\n" *user-counter*))

(define (user-count)
  (lambda (st)
    (increment-counter! *user-counter*)
    st))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (begin
       (reset-counters!)
       (let ((result
              (take n
                    (suspend
                     ((fresh (q) g0 g ...
                        (always-trigger-underconstraintso)
                        (lambda (st)
                          (let ((st (state-with-scope st nonlocal-scope)))
                            (let ((z ((reify q) st)))
                              (cons z (lambda () (lambda () #f)))))))
                      empty-state)))))
         (print-counters!)
         result)))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x)
       (fresh (q0 q1 q ...)
         g0 g ...
         (== (list q0 q1 q ...) x))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

(define (underconstraint? v)
  (or (not v) (procedure? v)))

;; Underconstraint store object.
;; #f or a goal that is the current underconstraint

(define empty-U #f)

;; counter; how long since we last run the underconstraint?
(define empty-V 0)

;; underconstraint/counter store

(define empty-U/V (cons empty-U empty-V))

; State object.
; The state is the value that is monadically passed through the search
; Contains:
;   S - the substitution
;   C - the constraint store
;   U/V - the underconstraint store/counter

(define (state? v)
  (and (list? v) (= (length v) 3)))

(define (state S C U/V) (list S C U/V))

(define (state-S st) (car st))
(define (state-C st) (cadr st))
;;
(define (state-U/V st) (caddr st))
(define (state-U st) (car (state-U/V st)))
(define (state-V st) (cdr (state-U/V st)))

(define empty-state (state empty-subst empty-C empty-U/V))

(define (state-with-C st C^)
  (state (state-S st) C^ (state-U/V st)))

(define (state-with-U/V st U/V^)
  (state (state-S st) (state-C st) U/V^))

(define (state-with-U st U)
  (state (state-S st) (state-C st) (cons U (state-V st))))

(define (state-with-V st V)
  (state (state-S st) (state-C st) (cons (state-U st) V)))

(define state-with-scope
  (lambda (st new-scope)
    (state (subst-with-scope (state-S st) new-scope) (state-C st) (state-U/V st))))


(define (== u v)
  (lambda (st)
    (increment-counter! *==-counter*)
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
        (and-foldl update-constraints (state S^ (state-C st) (state-U/V st)) added)
        #f))))

(define (wrap-for-depth-limit gc)
  (lambda args
    (let ([g (apply gc args)])
      (lambda (depth1)
        (check-type depth1 number?)
        (lambda (depth2)
          (check-type depth2 number?)
          #;(printf "depth1: ~s; depth2: ~s\n" depth1 depth2)
          g)))))

(define (==2 u v)
  (lambda (st)
    (increment-counter! *==g-counter*)
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
        (and-foldl update-constraints (state S^ (state-C st) (state-U/V st)) added)
        #f))))

(define ==g (wrap-for-depth-limit ==2))
(define =/=g (wrap-for-depth-limit =/=))
(define absentog (wrap-for-depth-limit absento))
(define symbolog (wrap-for-depth-limit symbolo))
(define numberog (wrap-for-depth-limit numbero))
(define stringog (wrap-for-depth-limit stringo))



(define succeed (== #f #f))
(define fail (== #f #t))

(define (always-trigger-underconstraintso)
  (lambda (st)
    (let ((U (state-U st)))
      (if U
          (run-and-set-underconstraint U st)
          st))))

(define (sometimes-trigger-underconstraintso)
  (lambda (st)
    (let ((U (state-U st))
          (V (state-V st)))
      (if U
          (begin
            (let ([t (cdr U)])
              (printf
               "~s\n"
               ((reify t) (state-with-scope st (new-scope)))))
            (if (>= V (*underconstraint-how-often-param*))
                (run-and-set-underconstraint U (state-with-V st 0))
                (state-with-V st (+ V 1))))
          st))))

(define (underconstraino-aux user-name te t ge g timeout-info trace?)
  (lambda (st)
    (run-and-set-underconstraint (cons (g 0) t) st)))

(define-syntax underconstraino
  (syntax-rules ()
    [(_ name te ge)
     ;; use global default timeout parameter
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g #f #f))]
    [(_ name te ge #f)
     ;; no timeout (overrides global timeout parameter)
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g `(timeout #f) #f))]
    [(_ name te ge timeout-ticks)
     ;; use timeout with `timeout-ticks` ticks (gas) (overrides global
     ;; timeout parameter)
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g `(timeout ,timeout-ticks) #f))]))

(define-syntax trace-underconstraino
  ;; same as `underconstraino`, but with the trace flag set to `#t`
  ;; rather than `#f`
  (syntax-rules ()
    [(_ name te ge)
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g #f #t))]
    [(_ name te ge #f)
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g `(timeout #f) #t))]
    [(_ name te ge timeout-ticks)
     (let ((t te)
           (g ge))
       (underconstraino-aux name 'te t 'ge g `(timeout ,timeout-ticks) #t))]))

(define (infg? v)
  (or (not v)
      (and (pair? v) (state? (car v)) (procedure? (cdr v)))
      (state? v)))

(define-syntax case-infg
  (syntax-rules ()
    ((_ e (() e0) ((c^) e2) ((c f) e3))
     (let ((stream e))
       (cond
         ((not stream) e0)
         ((not (and (pair? stream)
                 (procedure? (cdr stream))))
          (let ((c^ stream)) e2))
         (else (let ((c (car stream)) (f (cdr stream)))
                 e3)))))))

;; Unsoundly fail when reaching depth-1 limit
(define (check-depth-1 g)
  (lambda (depth1)
    (check-type depth1 number?)
    (lambda (depth2)
      (check-type depth2 number?)
      (lambda (st)
        (check-type st state?)
        (if (> depth1 (*underconstraint-depth-limit-1*))
            (begin (increment-counter! *depth-limit-1-cutoff-counter*)
                   #f) ;; UNSOUND!
            (((g (+ depth1 1)) depth2) st))))))

;; Suspend when reaching depth-2-limit
(define (check-depth-2 g-on-fallback-thunk g)
  (lambda (depth2)
    (check-type depth2 number?)
    (lambda (st)
      (check-type st state?)
      (if (> depth2 (*underconstraint-depth-limit-2*))
          (begin (increment-counter! *depth-limit-2-cutoff-counter*) (cons st (g-on-fallback-thunk)))
          ((g (+ depth2 1)) st)))))

(define-syntax (condg stx)
  (syntax-case stx ()
    ((_ ((x ...) (g ...) (b ...)) ...)
     #`(check-depth-1
        (lambda (depth1)
          (check-type depth1 number?)
          (letrec ([condg-g (condg-runtime
                             (list
                              (lambda (depth2)
                                (check-type depth2 number?)
                                (lambda (st)
                                  (check-type st state?)
                                  (let ([scope (subst-scope (state-S st))])
                                    (let ([x (var scope)] ...)
                                      (cons
                                       (bindg* st ((g depth1) depth2) ...)
                                       (lambda (depth2)
                                         (check-type depth2 number?)
                                         (lambda (st) (bindg* st ((b depth1) depth2) ...))))))))
                              ...)
                             (lambda () condg-g))])
            condg-g))))))

(define (condg-runtime clauses g-thunk)
   (check-depth-2 g-thunk
      (lambda (depth2)
        (check-type depth2 number?)
        (lambda (st)
          (define (nondeterministic) (check-type (cons st (g-thunk)) infg?))
          (check-type st state?)
          (let ((st (state-with-scope st (new-scope)))) ;; for set-var-val at choice point entry
            (let loop ([clauses clauses] [previously-found-clause #f])
              (if (null? clauses)
                  (and previously-found-clause
                       (let ([guard-result (car previously-found-clause)]
                             [body (cdr previously-found-clause)])
                         ;; commit, evaluate body
                         ((body depth2) guard-result)))
                  (let* ([clause-evaluated (((car clauses) depth2) st)]
                         [guard-stream (car clause-evaluated)]
                         [body-g (cdr clause-evaluated)])
                    (let ([guard-result (evaluate-guard guard-stream body-g)])
                      (cond
                        [(not guard-result) (loop (cdr clauses) previously-found-clause)]
                        [(eq? 'nondet guard-result) (nondeterministic)]
                        [else (if previously-found-clause
                                  (nondeterministic)
                                  (loop (cdr clauses) guard-result))]))))))))))

(define (evaluate-guard stream body-g)
   (case-infg stream
     (() #f)
     ((c) (cons c body-g))
     ;; TODO: nondet or not better here?
     ((c f) 'nondet) ;; not the declared type
     #;((c f) (cons c (lambda (depth2)
                        (check-type depth2 number?)
                        (lambda (st) (check-type st state?)
                          (bindg ((f depth2) st) (body-g depth2))))))))
  


(define (bindg stream g)
  (check-type stream infg?)
  (check-type
   (case-infg stream
     (() #f)
     ((c) (g c))   ;; committed and finished, so just g left to do
     ((c1 f1)      ;; committed but suspened...
      (let ([s2 (g c1)])
        (case-infg s2
          (() #f)              ;; g fails, so whole thing fails
          ((c2) (cons c2 f1))  ;; committed and finished, so just f1 to return to
          ;; when we return we need to do both f1 and f2
          ((c2 f2) (cons c2 (lambda (depth2) (lambda (st) (bindg ((f1 depth2) st) (f2 depth2))))))))))
   infg?))

(define-syntax bindg*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bindg* (bindg e g0) g ...))))

(define-syntax freshg
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambda (depth1)
       (check-type depth1 number?)
       (lambda (depth2)
         (check-type depth2 number?)
         (lambda (st)
           (let ((scope (subst-scope (state-S st))))
             (let ((x (var scope)) ...)
               (bindg* (((g0 depth1) depth2) st) ((g depth1) depth2) ...)))))))))



(define (run-committing-underconstraint-onceo g t)
  (define timeout-info #f)
  (define trace-arg? #f)
  (define name 'the-underconstraint)
  (define (trace?)
    (or trace-arg?
        (*trace-underconstraint-param*)))
  (define (get-timeout-ticks)
    (pmatch timeout-info
      [#f
       ;; no timeout argument passed to macro, so use the global
       ;; default parameter
       (*underconstraint-default-timeout-param*)]
      [(timeout #f)
       ;; argument passed to macro that overrides the global default
       ;; value---timeout disabled
       #f]
      [(timeout ,timeout-ticks)
       (guard (and (integer? timeout-ticks)
                   (positive? timeout-ticks)))
       ;; argument passed to macro that overrides the default global
       ;; parameter---timeout enabled, with `timeout-ticks` ticks
       ;; (gas) for the engine
       timeout-ticks]
      [else
       (error
        'run-underconstraint-onceo
        (printf
         "ticks argument must be #f or a positive integer: given ~s"
         rest))]))
  (define-syntax maybe-time
    (syntax-rules ()
      [(_ e) e #;(if (trace?) (time e) e)]))
  (define-syntax begin-when-trace
    (syntax-rules ()
      [(_ e* ... e)
       (begin
         (when (trace?)
           e*)
         ...
         e)]))
  (lambda (st)
    (let ((st (state-with-scope st (new-scope)))
          (timeout-ticks (get-timeout-ticks)))
      (define (do-run)
        (let ([$ ((g 0) st)])
          (case-infg $
            (()
             (begin-when-trace
                 (printf
                  "* underconstraint ~s failed\n"
                  name)
                 (begin
                   (increment-counter! *fail-counter*)
                   #f)))
            ((c)
             (begin-when-trace
               (printf
                "* underconstraint ~s succeeded with singleton result\n"
                name)
               (begin
                 (increment-counter! *singleton-succeed-counter*)
                 c)))
            ((c f^)
             (begin-when-trace
               (printf
                "* underconstraint ~s succeeded with non-singleton stream\n"
                name)
               (begin
                 (increment-counter! *non-singleton-succeed-counter*)
                 (cons c f^)))))))
      
      (when (trace?)
        (newline)
        (printf
         "* running underconstraint ~s with timeout ~s\n"
         name timeout-ticks))

      (if (not timeout-ticks)
          (do-run)
          ;; `timeout-ticks` is not #f:
          (let ((eng (make-engine do-run)))
            (maybe-time
             (eng timeout-ticks
                  ;; engine "completed" procedure
                  (lambda (ticks-left-over value)
                    (increment-counter! *engine-completed-counter*)
                    (begin-when-trace
                     (printf
                      "* underconstraint ~s engine completed after ~s of ~s ticks\n"
                      name
                      (- timeout-ticks ticks-left-over)
                      timeout-ticks)
                     (begin
                       #;(printf
                        "~s\n"
                        ((reify t) st))
                       value)))
                  ;; engine "expired" procedure
                  (lambda (new-engine)
                    (increment-counter! *engine-timedout-counter*)
                    (begin-when-trace
                     (printf
                      "* underconstraint ~s engine ran out of gas after ~s ticks (treating as success)\n"
                      name
                      timeout-ticks)
                     (begin
                       #;(printf
                        "~s\n"
                        ((reify t) st))
                     #;(error 'uc "done")
                     ;; to maintain soundness, we must treat
                     ;; engine timeout timeout as
                     ;; success---return the original state
                     (cons st g)))))))))))


(define (run-and-set-underconstraint U st)
  (let ([g (car U)]
        [t (cdr U)])
    (let ([$ ((run-committing-underconstraint-onceo g t) st)])
      (check-type $ infg?)
      (case-infg $
        (() #f)
        ((c^) (state-with-U c^ #f))
        ((c^ f^) (state-with-U c^ (cons f^ t)))))))

