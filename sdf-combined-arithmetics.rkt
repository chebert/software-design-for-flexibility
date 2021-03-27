#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require (only-in racket/pretty
                  (pretty-print pp)))

(require (for-syntax "sdf-install-arithmetic.rkt"))
(require "sdf-install-arithmetic.rkt")


;; Prefix implemenation operators with n:
(define-for-syntax implementation-prefix 'n:)
(define-for-syntax (implementation-operator-name operator)
  (string->symbol (string-append (symbol->string implementation-prefix) (symbol->string operator))))


(define-for-syntax binding-operator-name car)
(define-for-syntax binding-procedure-name cadr)
;; Create bindings of '(operator . n:operator)
(define-for-syntax implementation-operator-bindings
  (map (λ (operator)
         `(,operator ,(implementation-operator-name operator)))
       arithmetic-operators))

;; Create local definitions of operator's so that we can set! them in install-arithmetic!
(define-for-syntax arithmetic-operator-definitions
  (map (λ (binding)
         `(define ,(binding-operator-name binding)
            ,(binding-procedure-name binding)))
       implementation-operator-bindings))


;; definition for get-implementation-value which looks up the base implemenation of procedure-name.
(define-for-syntax get-implementation-value-definition
  `(define (get-implementation-value procedure-name)
     (case procedure-name
       ((negate) n:-)
       ((invert) n:/)
       ,@(map
          (λ (binding)
            `((,(binding-operator-name binding)) ,(binding-procedure-name binding)))
          implementation-operator-bindings))))

;; Definition of unary operators introduced
(define negate n:-)
(define invert n:/)

(define (fix-arithmetic-procedure-arity operator procedure constant-map)
  (cond
    ((memq operator +-like-arithmetic-operators)
     (+-like-operator operator procedure constant-map))
    ((memq operator --like-arithmetic-operators)
     (--like-operator operator procedure))
    ((memq operator arithmetic-comparators)
     (arithmetic-comparator operator procedure))
    (else procedure)))
(define (+-like-operator operator binary-procedure constant-map)
  (λ args
    (let ((arity (length args)))
      (cond ((null? args) (0ary-operator-value operator constant-map))
            ((null? (rest args)) (first args))
            (else (foldl (swap-args binary-procedure) (first args) (rest args)))))))

(define (--like-operator operator binary-procedure)
  (λ args
    (let ((arity (length args)))
      (cond ((null? args) (error "Expected at least 1 argument to " operator))
            ((null? (rest args)) ((unary-operator-procedure operator) (first args)))
            (else (foldl (swap-args binary-procedure) (first args) (rest args)))))))

(define (arithmetic-comparator comparator binary-procedure)
  (λ args
     (let loop ((args args)
                (result #t))
       (if (or (not result) (null? args) (null? (rest args)))
           result
           (loop (rest args)
                 (and result (binary-procedure (first args) (second args))))))))
;; TODO: symbolic comparison is wack for +2 arguments
(define (0ary-operator-value operator constant-map)
  (cond ((eq? operator '+) (constant-map 'additive-identity))
        ((eq? operator '*) (constant-map 'multiplicative-identity))))
(define (unary-operator-procedure operator)
  (cond ((eq? operator '-) negate)
        ((eq? operator '/) invert)))


(define-syntax (define-arithmetic stx)
  (datum->syntax stx `(begin
                        ;; local definitions of all arithmetic operators so that they can be set!
                        ,@arithmetic-operator-definitions
                        ;; re-require the implementation operators prefixed with n:
                        (require (only-in racket ,@implementation-operator-bindings))
                        ;; define get-implementation-value
                        ,get-implementation-value-definition
                        ;; define install-arithmetic-package-bindings! from sdf-install-arithmetic
                        ,install-arithmetic-package-bindings-definition)))

(define-arithmetic)

;; Not sure the difference between operators and procedure-names.
(define (operator->procedure-name operator) operator)

;; Operators are fixed-arity because of applicability-specifications
(define (operator-arity operator)
  (case operator
    ((negate invert sin cos) 1)
    ((+ - * / < > = <= >= expt) 2)
    (else (error "arity not known for " operator))))

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

;(install-arithmetic! symbolic-arithmetic-1)

(define (symbolic-evolution)
  (x 0 ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 'xt 'xt-n 'xt-2h)
        1)))

;; Combining arithmetics
(define-record operation (operator applicability procedure))

(define (all-args fixed-arity predicate)
  (assert (exact-nonnegative-integer? fixed-arity))
  (list (make-list fixed-arity predicate)))

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

(define default-object? (make-bundle-predicate 'default-object))
(define (default-object)
  (bundle default-object?))

(define-record arithmetic
  (name
   domain-predicate
   base-arithmetic-packages
   map-of-constant-name-to-constant
   map-of-operator-name-to-operation))

(define (extended-constant-map arithmetic)
  (λ (operator)
    (apply (arithmetic-map-of-constant-name-to-constant arithmetic)
           operator
           (map (λ (base-arithmetic) ((extended-constant-map base-arithmetic) operator))
                (arithmetic-base-arithmetic-packages arithmetic)))))

(define (extended-operation-map arithmetic)
  (λ (operator)
    (apply (arithmetic-map-of-operator-name-to-operation arithmetic)
           operator
           (map (λ (base-arithmetic) ((extended-operation-map base-arithmetic) operator))
                (arithmetic-base-arithmetic-packages arithmetic)))))

(define (arithmetic-operator->installable-procedure-map arithmetic)
  (let ((operation-map (extended-operation-map arithmetic))
        (constant-map (extended-constant-map arithmetic)))
    (λ (operator)
      (let ((operation (operation-map operator)))
        (fix-arithmetic-procedure-arity operator
                                        (operation-procedure operation)
                                        constant-map)))))

(define (is-operation-applicable? operation args)
  (let ((applicability (operation-applicability operation)))
    (memf
     (λ (x) x)
     (map (λ (predicates)
            (not (memq #f (map (λ (p a) (p a)) predicates args))))
          applicability))))

(define (install-arithmetic! arithmetic)
  (install-arithmetic-package-bindings! (arithmetic-operator->installable-procedure-map arithmetic)))

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

(define symbolic-arithmetic
  (make-arithmetic 'symbolic symbolic? '()
                   (λ (name)
                     (case name
                       ((additive-identity) 'zero)
                       ((multiplicative-identity) 'one)
                       (else (default-object))))
                   (λ (operator)
                     (simple-operation operator symbolic?
                                       (λ args (cons operator args))))))

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
                                       (λ args (cons operator args)))))))


;; Combinator for arithmetics

(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))
(define (add-arithmetics* arithmetics)
  (if (null? (cdr arithmetics))
      (car arithmetics)
      (make-arithmetic 'add
                       (disjoin*
                        (map arithmetic-domain-predicate
                             arithmetics))
                       arithmetics
                       constant-union
                       operation-union)))

(define (disjoin* predicates)
  (λ (x)
    (let loop ((result #f)
               (predicates predicates))
      (if (or result (null? predicates))
          result
          (loop ((first predicates) x)
                (rest predicates))))))
(define (conjoin* predicates)
  (λ (x)
    (let loop ((result #t)
               (predicates predicates))
      (if (or (not result) (null? predicates))
          result
          (loop ((first predicates) x)
                (rest predicates))))))


(define (constant-union name . constants)
  (let ((unique
         (remove default-object? (remove-duplicates constants eqv?))))
    (if (pair? unique)
        (car unique) ; unreasonable choice
        (default-object))))

(define (operation-union operator . operations)
  (operation-union* operator operations))

(define (operation-union* operator operations)
  (make-operation operator
                  (applicability-union*
                   (map operation-applicability operations))
                  (λ args
                    (operation-union-dispatch operator
                                              operations
                                              args))))

(define (operation-union-dispatch operator operations args)
  (let ((operation
         (findf (λ (operation)
                  (is-operation-applicable? operation args))
                operations)))
    (if (not operation)
        (error "inapplicable operation:" operator args)
        (apply (operation-procedure operation) args))))

(define (applicability-union* applicabilities)
  (apply append applicabilities))

(define (extend-arithmetic extender base-arithmetic)
  (add-arithmetics base-arithmetic (extender base-arithmetic)))

(define combined-arithmetic (extend-arithmetic symbolic-extender numeric-arithmetic))