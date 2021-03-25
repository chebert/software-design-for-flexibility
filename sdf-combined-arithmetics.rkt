#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require (only-in racket/pretty
                  (pretty-print pp)))

(define-for-syntax arithmetic-operator-bindings
  '((- n:-)
    (+ n:+)
    (* n:*)
    (/ n:/)
    (= n:=)
    (> n:>)
    (< n:<)
    (>= n:>=)
    (<= n:<=)
    (expt n:expt)
    (sin n:sin)
    (cos n:cos)))

(define-syntax (require-arithmetic stx)
  (datum->syntax stx `(require (only-in racket ,@arithmetic-operator-bindings))))

(require-arithmetic)

(define (operator->procedure-name operator) operator)

(define (operator-arity operator)
  (procedure-arity
   (get-implementation-value (operator->procedure-name operator))))

(define-syntax (%get-implementation-value stx)
  (syntax-case stx ()
    [(_ procedure-name)
     (datum->syntax
      stx
      `(case procedure-name
         ,@(map (λ (binding) `((,(car binding)) ,(cadr binding))) arithmetic-operator-bindings)))]))

(define (get-implementation-value procedure-name)
  (%get-implementation-value procedure-name))

(define-for-syntax arithmetic-operators (map car arithmetic-operator-bindings))
(define-syntax (arithmetic-operators stx)
  (datum->syntax stx `',arithmetic-operators))

(define-syntax (define-arithmetic-operators stx)
  (datum->syntax stx
                 `(begin
                    ,@(map (λ (binding) `(define ,(car binding) ,(cadr binding))) arithmetic-operator-bindings))))
(define-arithmetic-operators)

(define-syntax (install-arithmetic-operators! stx)
  (syntax-case stx ()
    [(_ package)
     (datum->syntax
      stx
      `(begin
         ,@(map (λ (binding)
                  (let ((operator (car binding)))
                    `(set! ,operator (cdr (assq ',operator package)))))
                arithmetic-operator-bindings)))]))

(define (install-arithmetic! package)
  (install-arithmetic-operators! package))


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
  (n:< (n:- expected tolerance) v (n:+ expected tolerance)))

(define (test)
  (assert
   (within? (x 0 ((evolver F .01 stormer-2) (numeric-s0) 100))
            (sin 1) .0001)))

(define (make-arithmetic-1 name operator-modifier)
  (map (λ (operator) (cons operator (operator-modifier operator)))
       (arithmetic-operators)))

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

(define (all-args arity predicate) 'applicability-specification)
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
   map-of-operator-name-to-operation))
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