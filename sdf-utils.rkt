#lang racket


#|
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(struct point (x y))
(point? (point 3 5))
(point-x (point 3 5))
(point-y (point 3 5))

(define (factorial n)
  (let factlp ((count 1) (answer 1))
    (write-line (list count answer))
    (if (> count n)
        answer
        (factlp (+ count 1) (* count answer)))))

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c1 (make-counter))
(define c2 (make-counter))

(define radix (make-parameter 10))

(define (number->string-radix num)
  (number->string num (radix)))

(define point? (make-bundle-predicate 'point))
(define (make-point x y)
  (define (get-x) x)
  (define (get-y) y)
  (define (set-x! new-x) (set! x new-x))
  (define (set-y! new-y) (set! y new-y))
  (bundle point? get-x get-y set-x! set-y!))

|#

(define (write-line o)
  (displayln o))

;; Bundles
(define (make-bundle-predicate symbol)
  (define (uid) symbol)
  (define (me . args)
    (cond
      ((null? args) uid) ; (me) returns a uid
      ;; expects the bundle to return the uid
      ((eq? uid ((first args) me)) #t)
      (else #f)))
  me)
;; TODO: This always returns true for (λ (x) (x))

(define-syntax-rule (bundle type-predicate method ...)
  (λ (sym . args)
    (cond ((and type-predicate (eq? type-predicate sym))
           (type-predicate))
          ((eq? 'method sym) (apply method args))
          ...)))

(define-syntax-rule (assert form)
  (unless form
    (error "Assertion failed: " 'form form)))

(define-syntax (define-record stx)
  (let* ((datum (syntax->datum stx))
         (name (cadr datum))
         (fields (caddr datum))
         (constructor-name (string->symbol (string-append "make-" (symbol->string name)))))
    (datum->syntax stx `(struct ,name ,fields #:transparent #:constructor-name ,constructor-name))))

(provide assert bundle make-bundle-predicate write-line define-record)