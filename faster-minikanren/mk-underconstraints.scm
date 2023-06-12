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

(define *immature-stream-counter* 0)
(define *fail-counter* 0)
(define *singleton-succeed-counter* 0)
(define *non-singleton-succeed-counter* 0)

(define-syntax increment-counter!
  (syntax-rules ()
    [(_ c) (set! c (add1 c))]))

(define (reset-counters!)
  (set! *engine-completed-counter* 0)
  (set! *engine-timedout-counter* 0)
  ;;
  (set! *immature-stream-counter* 0)
  (set! *fail-counter* 0)
  (set! *singleton-succeed-counter* 0)
  (set! *non-singleton-succeed-counter* 0))

(define (print-counters!)
  (printf "*engine-completed-counter*: ~s\n" *engine-completed-counter*)
  (printf "*engine-timedout-counter*: ~s\n" *engine-timedout-counter*)
  ;;
  (printf "*immature-stream-counter*: ~s\n" *immature-stream-counter*)
  (printf "*fail-counter*: ~s\n" *fail-counter*)
  (printf "*singleton-succeed-counter*: ~s\n" *singleton-succeed-counter*)
  (printf "*non-singleton-succeed-counter*: ~s\n" *non-singleton-succeed-counter*))


(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (begin
       (reset-counters!)
       (let ((result
              (take n
                    (suspend
                     ((fresh (q) g0 g ...
                        (trigger-underconstraintso)
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


; Underconstraint record object.
;
; An underconstraint record object is a proper list.
;
; When an underconstraint is first created, it is given a unique name
; so that duplicate underconstraints on a variable can be avoided, to
; aid with tracing, and to help keep track of which underconstraints
; have already been run when running all the underconstraints
; associated with a set of variables.
;
; Contains:
;
;   user-name    - user-specified name for the underconstraint. Used for
;                    tracing.
;   unique-name  - generated globally-unique name for the
;                    underconstraint.  Used for `eq?`-based lookup of the
;                    underconstraint, to ensure no variable includes
;                    duplicate underconstraints, and to ensure no
;                    underconstraint is run more than once when running
;                    underconstraints on multiple variables.
;   te           - the original term expression specified by the user.
;                    Used for tracing.
;   t            - term with which that the underconstraint is currently
;                    associated.  The same underconstraint might currently
;                    be associated with other terms as well, since
;                    underconstraints are "pushed down" when a fresh
;                    variable is associated with a pair containing fresh
;                    variables.  `t` might not be a fresh variable; if `t`
;                    is a pair that contains fresh variables when the
;                    underconstaint is run, the underconstraint will need
;                    to be "pushed down" onto those fresh variables if the
;                    underconstraint succeeds.
;   ge           - the original goal expression specified by the user.
;                    Used for tracing.
;   g            - the underconstraint goal specified by the user.
;   timeout-info - the engine timeout value specified by the user.
;   trace?       - whether the user used a tracing version of the
;                    `underconstraino` macro.

(define (underconstraint? v)
  (and (list? v) (= (length v) 8)))

(define (underconstraint user-name unique-name te t ge g timeout-info trace?)
  `(,unique-name ,t ,g ,timeout-info ,trace? ,user-name ,te ,ge))

(define (underconstraint-unique-name u)
  (car u))

(define (underconstraint-t u)
  (cadr u))

(define (underconstraint-with-t u t)
  (cons (car u) (cons t (cddr u))))

(define (underconstraint-g u)
  (caddr u))

(define (underconstraint-with-g u g)
  (cons (car u) (cons (cadr u) (cons g (cdddr u)))))

(define (underconstraint-timeout-info u)
  (cadddr u))

(define (underconstraint-trace? u)
  (list-ref u 4))

(define (underconstraint-user-name u)
  (list-ref u 5))

(define (underconstraint-te u)
  (list-ref u 6))

(define (underconstraint-ge u)
  (list-ref u 7))


; `u` is an association list (without duplicates) of
;
;   (<unique-name> . <underconstraint record object>)
;
; pairs associated with a variable.

(define empty-u '())

(define (u-unique-name u unique-name)
  (cond
    ((assq unique-name u) => cdr)
    (else #f)))

(define (u-with-underconstraint u underconstraint)
  (let ((unique-name (underconstraint-unique-name u)))
    (if (u-unique-name u unique-name)
        u
        `((,unique-name . ,underconstraint) . ,u))))

; Underconstraint store object.

; Mapping of a representative variable `x` to an association list `u`
; (without duplicates) of
;
;   (<unique-name> . <underconstraint record object>)
;
; pairs. The association list of underconstraint record objects is
; always on the representative element and must be moved / merged when
; that element changes.

(define empty-U empty-intmap)

(define (set-u st x u)
  (state-with-U
    st
    (intmap-set (state-U st) (var-idx x) u)))

(define (lookup-u st x)
  (let ((res (intmap-ref (state-U st) (var-idx x))))
    (if (unbound? res)
      empty-u
      res)))

; t:unbind in mk-vicare.scm either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define (remove-u x st)
  (state-with-U st (intmap-set (state-U st) (var-idx x) empty-u)))

; "touched" variables store
;
; The store is a list of variables that have been updated, either
; through unification or through "normal" constraint solving, since
; the last time that underconstraints were run.
;
; The list of "touched" variables does not contain duplicates.

(define empty-V '())

(define (set-v st v)
  (let ((V (state-V st)))
    (if (memq v V)
        st
        (state-with-V st (cons v V)))))

; underconstraint/"touched" variable store

(define empty-U/V (cons empty-U empty-V))

; State object.
; The state is the value that is monadically passed through the search
; Contains:
;   S - the substitution
;   C - the constraint store
;   U/V - the underconstraint store/list of "touched" variables (without duplicates)

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
    (let-values (((S^ added) (unify u v (state-S st))))
      (if S^
        (and-foldl update-constraints (state S^ (state-C st) (state-U/V st)) added)
        #f))))

(define succeed (== #f #f))
(define fail (== #f #t))

(define (remove-duplicate-underconstraints u*)
  (cond
    ((null? u*) '())
    ((assq (underconstraint-unique-name (car u*)) (cdr u*))
     (remove-duplicate-underconstraints (cdr u*)))
    (else
     (cons (car u*) (remove-duplicate-underconstraints (cdr u*))))))

(define (trigger-underconstraintso)
  (lambda (st)
    (let ((V (state-V st)))
      (if (null? V)
          st
          (let ((st (state-with-V st empty-V)))
            (let ((unders (apply append (map (lambda (x) (lookup-u st x)) V))))
              (let ((unders (remove-duplicate-underconstraints unders)))
                (let loop ((unders unders)
                           (st st))
                  (cond
                    ((null? unders) st)
                    (else (bind (run-and-add-committing-underconstraint (car unders) st)
                                (lambda (st) (loop (cdr unders) st)))))))))))))



;; if under is not already in the list of underconstraint records for the
;; variable `v`, add it and return the updated state.
(define (set-u/nonduplicate st v under)
  (let ((current-u (lookup-u st v)))
    (if (assq (underconstraint-unique-name under) current-u)
        st
        (set-u st v (cons under current-u)))))

(define (add-underconstraint-to-store underconstraint)
  (lambda (st)
    (let ((t (underconstraint-t underconstraint)))
      (let ((vars-to-attribute (vars (walk* t (state-S st)))))
        (let ((underconstraint^ (underconstraint-with-t underconstraint vars-to-attribute)))
          (fold-left
           (lambda (st v)
             (set-u/nonduplicate st v underconstraint^))
           st vars-to-attribute))))))

(define (underconstraino-aux user-name te t ge g timeout-info trace?)
  (lambda (st)
    (let* ((unique-name (parameterize ([gensym-prefix user-name]) (gensym)))
           (under (underconstraint user-name unique-name te t ge g timeout-info trace?)))
      (run-and-add-committing-underconstraint under st))))


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

(define-syntax (condg stx)
  (syntax-case stx ()
   ((_ ((x ...) (g ...) (b ...)) ...)
    #`(letrec ([condg-g (condg-runtime
                          (list
                            (lambda (st)
                              (let ([scope (subst-scope (state-S st))])
                                (let ([x (var scope)] ...)
                                  (cons
                                    (bindg* st g ...)
                                    (lambda (st) (bindg* st b ...))))))
                            ...)
                            (lambda () condg-g))])
         condg-g))))

(define (condg-runtime clauses g-thunk)
  (lambda (st)
    (define (nondeterministic) (check-type (cons st (g-thunk)) infg?))
    (let ((st (state-with-scope st (new-scope)))) ;; for set-var-val at choice point entry
      (let loop ([clauses clauses] [previously-found-clause #f])
        (if (null? clauses)
            (and previously-found-clause
                 (let ([guard-result (car previously-found-clause)]
                       [body (cdr previously-found-clause)])
                   ;; commit, evaluate body
                   (body guard-result)))
            (let* ([clause-evaluated ((car clauses) st)]
                   [guard-stream (car clause-evaluated)]
                   [body-g (cdr clause-evaluated)])
              (let ([guard-result (evaluate-guard guard-stream body-g)])
                (cond
                  [(not guard-result) (loop (cdr clauses) previously-found-clause)]
                  [else (if previously-found-clause
                            (nondeterministic)
                            (loop (cdr clauses) guard-result))]))))))))

(define (evaluate-guard stream body-g)
  (check-type
   (case-infg stream
              (() #f)
              ((c) (cons c body-g))
              ((c f) (cons c (lambda (st) (bindg (f st) body-g)))))
   infg?))
  


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
                            ((c2 f2) (cons c2 (lambda (st) (bindg (f1 st) f2))))))))
   infg?))

(define-syntax bindg*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bindg* (bindg e g0) g ...))))

(define-syntax freshg
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambda (st)
       (let ((scope (subst-scope (state-S st))))
         (let ((x (var scope)) ...)
           (bindg* (g0 st) g ...)))))))



(define (run-committing-underconstraint-onceo name ge g timeout-info trace-arg? general?)  
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
        (let ([$ (g st)])
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
         "* underconstraint ~s with timeout ~s trying goal expression:\n~s\n"
         name timeout-ticks ge))

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
                     value))
                  ;; engine "expired" procedure
                  (lambda (new-engine)
                    (increment-counter! *engine-timedout-counter*)
                    (begin-when-trace
                     (printf
                      "* underconstraint ~s engine ran out of gas after ~s ticks (treating as success)\n"
                      name
                      timeout-ticks)
                     ;; to maintain soundness, we must treat
                     ;; engine timeout timeout as
                     ;; success---return the original state
                     (cons st g))))))))))


(define (run-and-add-committing-underconstraint under st)
  (let ((user-name (underconstraint-user-name under))
        (unique-name (underconstraint-unique-name under))
        (t (underconstraint-t under))
        (ge (underconstraint-ge under))
        (g (underconstraint-g under))
        (timeout-info (underconstraint-timeout-info under))
        (trace? (underconstraint-trace? under)))
  
    (let ([$ ((run-committing-underconstraint-onceo `(,user-name . ,unique-name) ge g timeout-info trace? #t)
              (state-with-U/V st empty-U/V))])
      (check-type $ infg?)
      (case-infg $
        (() #f)
        ((c^) c^) ;; could remove underconstraint, here
        ((c^ f^)
         (let ((vars-to-attribute (vars (walk* t (state-S c^)))))
           #;(printf "~s\n" vars-to-attribute)
           (let* ((under^ (underconstraint-with-t under vars-to-attribute))
                  (under^ (underconstraint-with-g under^ f^)))
             (fold-left
              (lambda (st v)
                (set-u/nonduplicate st v under^))
              c^ vars-to-attribute))))))))

