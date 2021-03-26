#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require (only-in racket/pretty
                  (pretty-print pp)))

(define-for-syntax arithmetic-operators
  '(- + * / = > < >= <= expt sin cos))

(define-for-syntax binding-operator-name car)
(define-for-syntax binding-procedure-name cadr)

(define-for-syntax implementation-prefix 'n:)
(define-for-syntax (implementation-operator-name operator)
  (string->symbol (string-append (symbol->string implementation-prefix) (symbol->string operator))))
(define-for-syntax (arithmetic-operator-binding operator)
  `(,operator ,(implementation-operator-name operator)))
(define-for-syntax arithmetic-operator-bindings
  (map arithmetic-operator-binding arithmetic-operators))
(define-for-syntax (arithmetic-operator-definition binding)
  `(define ,(binding-operator-name binding) ,(binding-procedure-name binding)))
(define-for-syntax arithmetic-operator-definitions
  (map arithmetic-operator-definition arithmetic-operator-bindings))

(define-for-syntax get-implementation-value-cases
  (map (λ (binding) `((,(binding-operator-name binding)) ,(binding-procedure-name binding))) arithmetic-operator-bindings))
(define-for-syntax get-implementation-value-definition
  `(define (get-implementation-value procedure-name)
     (case procedure-name ,@get-implementation-value-cases)))

(define-for-syntax (arithmetic-operator-installations package)
  (map (λ (operator)
         `(set! ,operator (package-operator ,package ',operator)))
       arithmetic-operators))
(define-for-syntax install-arithmetic-definition
  `(define (install-arithmetic! package)
     ,@(arithmetic-operator-installations 'package)))

(define-syntax (define-arithmetic stx)
  (datum->syntax stx `(begin (define arithmetic-operators ',arithmetic-operators)
                             ,@arithmetic-operator-definitions
                             (require (only-in racket ,@arithmetic-operator-bindings))
                             ,get-implementation-value-definition
                             ,install-arithmetic-definition)))

(define-arithmetic)

(define (operator->procedure-name operator) operator)

(define (operator-arity operator)
  (procedure-arity
   (get-implementation-value (operator->procedure-name operator))))

(define (package-operator package operator) (cdr (assq operator package)))

;; Simple ODE

(define (%history ts xs)
  (λ args
    (cond ((null? args) `(%history ,ts ,xs))
          ((eq? (first args) 'x)
           (list-ref xs (second args)))
          ((eq? (first args) 't)
           (list-ref ts (second args)))
          ((eq? (first args) 'extend)
           (%history (cons (second args) ts)
                     (cons (third args) xs))))))

(define (make-initial-history t h xt xt1 xt2)
  (%history (list t (- t h) (- t (* 2 h)))
            (list xt xt1 xt2)))

(define (extend-history new-t new-x history) (history 'extend new-t new-x))    
(define (t step history) (history 't step))
(define (x step history) (history 'x step))

(define (stormer-2 F h)
  (λ (history)
    (+ (* 2 (x 0 history))
       (* -1 (x 1 history))
       (* (/ (expt h 2) 12)
          (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))))))

(define (stepper h integrator)
  (λ (history)
    (extend-history (+ (t 0 history) h)
                    (integrator history)
                    history)))

(define (evolver F h make-integrator)
  (let ((integrator (make-integrator F h)))
    (let ((step (stepper h integrator)))
      (define (evolve history n-steps)
        (if (n:> n-steps 0)
            (evolve (step history) (n:- n-steps 1))
            history))
      evolve)))

;; Simple example: D^2x(t) + x(t) = 0
;; 1 Soln: x(t) = sin(t)
(define (F t x) (- x))

;; Stormer requires 3 initial data points
(define (numeric-s0)
  (make-initial-history 0 0.1
                        (sin 0) (sin -0.01) (sin -0.02)))

(define (within? v expected tolerance)
  (< (- expected tolerance) v (+ expected tolerance)))

(define (test)
  (assert
   (within? (x 0 ((evolver F .01 stormer-2) (numeric-s0) 100))
            (sin 1) .0001)))

(define (make-arithmetic-1 name operator-modifier)
  (map (λ (operator) (cons operator (operator-modifier operator)))
       arithmetic-operators))

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
                     (λ (operator)
                       (λ args (cons operator args)))))

(install-arithmetic! symbolic-arithmetic-1)

(define (symbolic-evolution)
  (x 0 ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 'xt 'xt-n 'xt-2h)
        1)))

;; Combining arithmetics
(define (make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))
(define (operation-applicability operation)
  (third operation))

(define (all-args fixed-arity predicate)
  (assert (exact-nonnegative-integer? fixed-arity))
  (make-list fixed-arity predicate))

(define (enumerate-combined-applicabilities xs ys)
  (assert (= (length xs) (length ys)))
  (let loop ((results '(()))
             (xs xs)
             (ys ys))
    (if (null? xs)
        (map reverse results)
        (let ((x (first xs))
              (y (first ys)))
          (loop
           (append-map (λ (result) (list (cons x result) (cons y result))) results)
           (rest xs)
           (rest ys))))))

(define (any-arg fixed-arity predicate base-predicate)
  (assert (exact-nonnegative-integer? fixed-arity))
  (filter (λ (ps) (memq predicate ps))
          (enumerate-combined-applicabilities (make-list fixed-arity predicate) (make-list fixed-arity base-predicate))))

(define (simple-operation operator predicate procedure)
  (make-operation operator
                  (all-args (operator-arity operator)
                            predicate)
                  procedure))

(struct arithmetic
  (name
   domain-predicate
   base-arithmetic-packages
   map-of-constant-name-to-constant
   map-of-operator-name-to-operation)
  #:transparent)
(define make-arithmetic arithmetic)

(define default-object? (make-bundle-predicate 'default-object))
(define (default-object)
  (bundle default-object?))

(define numeric-arithmetic
  (make-arithmetic 'numeric number? '()
                   (λ (name)
                     (case name
                       ((additive-identity) 0)
                       ((multiplicative-identity) 1)
                       (else (default-object))))
                   (λ (operator)
                     (simple-operation operator number?
                                       (get-implementation-value
                                        (operator->procedure-name operator))))))

(define (symbolic? datum) (or (symbol? datum) (pair? datum)))

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic ;name
                   symbolic? ;domain-predicate
                   (list base-arithmetic) ;base-arithmetic-packages
                   (λ (name base-constant) ;constant-generator
                     base-constant)
                   (let ((base-predicate
                          (arithmetic-domain-predicate base-arithmetic)))
                     (λ (operator base-operation) ;operator-generator
                       (make-operation operator
                                       (any-arg (operator-arity operator)
                                                symbolic?
                                                base-predicate)
                                       (λ args cons operator args))))))