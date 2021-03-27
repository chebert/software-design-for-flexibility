#lang racket

(require "sdf-utils.rkt")

(define +-like-arithmetic-operators
  '(+ *))
(define --like-arithmetic-operators
  '(- /))
(define arithmetic-comparators
  '(= > < <= >=))
(define arithmetic-operators
  (append
   +-like-arithmetic-operators
   --like-arithmetic-operators
   arithmetic-comparators
   '(expt sin cos)))

(define arithmetic-operators-in-arithmetic-package
  (append
   arithmetic-operators
   '(negate invert)))

(define install-arithmetic-package-bindings-definition
  `(define (install-arithmetic-package-bindings! operator->procedure-map)
     ,@(map (λ (operator) `(set! ,operator (operator->procedure-map ',operator)))
            arithmetic-operators-in-arithmetic-package)))

(provide +-like-arithmetic-operators --like-arithmetic-operators arithmetic-comparators arithmetic-operators install-arithmetic-package-bindings-definition)