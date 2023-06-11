;; WEB 7 May 02023 -- 5 June 02023
;;
;; Extend faster-miniKanren's `mk.scm` with underconstraints.

(load "pmatch.scm")

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

(define (underconstraint user-name unique-name te t ge g timeout-info trace?)
  `(,unique-name ,g ,t ,timeout-info ,trace? ,user-name ,te ,ge))

(define (underconstraint-unique-name u)
  (car u))

(define (underconstraint-g u)
  (cadr u))

(define (underconstraint-t u)
  (caddr u))

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
  (let ((U/V (state-U/V st)))
    (let ((U (car U/V))
          (V (cdr U/V)))
      (if (memq v V)
          st
          (cons U (cons v V))))))

; underconstraint/"touched" variable store

(define empty-U/V (cons empty-U empty-V))

; State object.
; The state is the value that is monadically passed through the search
; Contains:
;   S - the substitution
;   C - the constraint store
;   U/V - the underconstraint store/list of "touched" variables (without duplicates)

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


(define (run-underconstraint-onceo name ge g timeout-info trace? general?)
  (define (trace?)
    (or trace?
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
      [(_ e) (if (trace?) (time e) e)]))
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
      (when (trace?)
        (newline)
        (printf
         "* underconstraint ~s with timeout ~s trying goal expression:\n~s\n"
         name timeout-ticks ge))
      (if (not timeout-ticks)
          (let loop (($ (g st)))
             (case-inf $
               (()
                (begin-when-trace
                 (printf
                  "* underconstraint ~s failed\n"
                  name)
                 #f))
               ((f)
                (begin-when-trace
                 (printf
                  "* underconstraint ~s encountered `(f)` case of case-inf\n"
                  name)
                 (lambda ()
                   ;; thunkify forcing of `f` to allow interleaving,
                   ;; since we don't have timeout protection
                   (loop (f)))))
               ((c)
                (begin-when-trace
                 (printf
                  "* underconstraint ~s succeeded with singleton result\n"
                  name)
                 ;; TODO should consider returning `c` here; since the
                 ;; singleton state is being returned, it is safe to
                 ;; commit to the new state returned (updated with
                 ;; underconstraints, perhaps).
                 st))
               ((c f^)
                (begin-when-trace
                 (printf
                  "* underconstraint ~s succeeded with non-singleton stream\n"
                  name)
                 st))))
          ;; `timeout-ticks` is not #f:
          (let ((eng (make-engine
                       (lambda ()
                         (let loop (($ (g st)))
                            (case-inf $
                              (()
                               (begin-when-trace
                                (printf
                                 "* underconstraint ~s failed\n"
                                 name)
                                #f))
                              ((f)
                               (begin-when-trace
                                (printf
                                 "* underconstraint ~s encountered `(f)` case of case-inf\n"
                                 name)
                                ;; force `f` immediately, since we have
                                ;; a timeout to protect us
                                (loop (f))))
                              ((c)
                               (begin-when-trace
                                (printf
                                 "* underconstraint ~s succeeded with singleton result\n"
                                 name)
                                ;; TODO should consider returning `c` here; since the
                                ;; singleton state is being returned, it is safe to
                                ;; commit to the new state returned (updated with
                                ;; underconstraints, perhaps).
                                st))
                              ((c f^)
                               (begin-when-trace
                                (printf
                                 "* underconstraint ~s succeeded with non-singleton stream\n"
                                 name)
                                st))))))))
            (maybe-time
             (eng timeout-ticks
                  ;; engine "completed" procedure
                  (lambda (ticks-left-over value)
                    (begin-when-trace
                     (printf
                      "* underconstraint ~s engine completed after ~s of ~s ticks\n"
                      name
                      (- timeout-ticks ticks-left-over)
                      timeout-ticks)
                     value))
                  ;; engine "expired" procedure
                  (lambda (new-engine)
                    (begin-when-trace
                     (printf
                      "* underconstraint ~s engine ran out of gas after ~s ticks (treating as success)\n"
                      name
                      timeout-ticks)
                     ;; to maintain soundness, we must treat
                     ;; engine timeout timeout as
                     ;; success---return the original state
                     st)))))))))

;; One-shot underconstraints:

(define (one-shot-underconstraino-aux name ge g timeout-info trace?)
  (run-underconstraint-onceo name ge g timeout-info trace? #f))

(define-syntax one-shot-underconstraino
  (syntax-rules ()
    [(_ name ge)
     ;; use global default timeout parameter
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g #f #f))]
    [(_ name ge #f)
     ;; no timeout (overrides global timeout parameter)
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g `(timeout #f) #f))]
    [(_ name ge timeout-ticks)
     ;; use timeout with `timeout-ticks` ticks (gas) (overrides global
     ;; timeout parameter)
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g `(timeout ,timeout-ticks) #f))]))

(define-syntax trace-one-shot-underconstraino
  ;; same as `one-shot-underconstraino`, but with the trace flag set
  ;; to `#t` rather than `#f`
  (syntax-rules ()
    [(_ name ge)
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g #f #t))]
    [(_ name ge #f)
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g `(timeout #f) #t))]
    [(_ name ge timeout-ticks)
     (let ((g ge))
       (one-shot-underconstraino-aux name 'ge g `(timeout ,timeout-ticks) #t))]))


;; Full (multi-shot) underconstraints:

(define (run-general-underconstraint underconstraint st)
  
  ;; Run the underconstraint's goal `g` immediately, in the current
  ;; state `st`, but with the underconstraint mapping `U` and the list
  ;; of newly "touched" variables `V` replaced with the empty `U/V`.

  (let ((user-name (underconstraint-user-name underconstraint))
        (unique-name (underconstraint-unique-name underconstraint))
        (te (underconstraint-te underconstraint))
        (t (underconstraint-t underconstraint))
        (ge (underconstraint-ge underconstraint))
        (g (underconstraint-g underconstraint))
        (timeout-info (underconstraint-timeout-info underconstraint))
        (trace? (underconstraint-trace? underconstraint)))
    (let (($ ((run-underconstraint-onceo `(,user-name . ,unique-name) ge g timeout-info trace? #t)
              (state-with-U/V st empty-U/V))))
  
      ;; TODO Upon failure, fail.  Upon success:
      ;;
      ;; * "forget"/discard the new stream of states returned from
      ;; running `g`, since we are only running underconstraints for
      ;; fail-fast behavior.  (Also, we need to eventually return a
      ;; single stream, to get the desired `onceo` semantics.)
      ;;
      ;; * (as an optimization, if the new stream of states returned from
      ;; running `g` is a singleton stream, it is safe to commit the
      ;; returned singleton state.  Should probably restart solving the
      ;; remaining underconstraints in that case, using the newly updated
      ;; state extension (along with the new list of unique "touched"
      ;; variables).)
      ;;
      ;; * `walk` the term `t` in the substitution from the original
      ;; state `st`, finding the set of fresh variables at the leaves,
      ;; and add those variables, associated with the unique name and
      ;; underconstrained goal `g`, to the underconstraint mapping U.
      ;; Return a singleton stream with the original state `st`,
      ;; updated with the new U.

      'TODO
      
      )))

(define (add-general-underconstraino user-name te t ge g timeout-info trace?)
  (lambda (st)
    (let* ((unique-name (parameterize ([gensym-prefix user-name]) (gensym)))
           (under (underconstraint user-name unique-name te t ge g timeout-info trace?)))
      (run-general-underconstraint under st))))

(define-syntax underconstraino
  (syntax-rules ()
    [(_ name te ge)
     ;; use global default timeout parameter
     (let ((t te)
           (g ge))
       (add-general-underconstraino name 'te t 'ge g #f #f))]
    [(_ name te ge #f)
     ;; no timeout (overrides global timeout parameter)
     (let ((t te)
           (g ge))
       (add-general-underconstraino name 'te t 'ge g `(timeout #f) #f))]
    [(_ name te ge timeout-ticks)
     ;; use timeout with `timeout-ticks` ticks (gas) (overrides global
     ;; timeout parameter)
     (let ((t te)
           (g ge))
       (add-general-underconstraino name 'te t 'ge g `(timeout ,timeout-ticks) #f))]))

(define-syntax trace-underconstraino
  ;; same as `underconstraino`, but with the trace flag set to `#t`
  ;; rather than `#f`
  (syntax-rules ()
    [(_ name te ge)
     (let ((t te)
           (g ge))
       (add-underconstraino name 'te t 'ge g #f #t))]
    [(_ name te ge #f)
     (let ((t te)
           (g ge))
       (add-underconstraino name 'te t 'ge g `(timeout #f) #t))]
    [(_ name te ge timeout-ticks)
     (let ((t te)
           (g ge))
       (add-underconstraino name 'te t 'ge g `(timeout ,timeout-ticks) #t))]))
