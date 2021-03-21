#lang racket

(require "sdf-utils.rkt")

;; Function Combinators
(define (assert-arity f n)
  (assert (has-arity? (get-arity f) n)))

#|
(define (compose f g)
  (assert-arity f 1)
  (assert-arity g 1)
  (define (the-composition x)
    (f (g x)))
  the-composition)
|#

#|
(define (parallel-combine f g h)
  (assert-arity f 1)
  (assert-arity g 1)
  (assert-arity h 2)
  (define (the-composition x)
    (h (f x) (g x)))
  the-composition)
|#

(define arity-table (make-weak-hasheqv))

(define (restrict-arity! f n)
  (hash-set! arity-table f n)
  f)

(define (get-arity f)
  (or (hash-ref arity-table f #f)
      (procedure-arity f)))

(define (arity-list? arity) (pair? arity))

(define (has-arity? arity n)
  (cond
    ((arity-at-least? arity) (<= (arity-at-least-value arity) n))
    ((arity-list? arity) (if (member n arity) #t #f))
    (else (= n arity))))

(define (definite-arity? arity)
  (not (arity-at-least? arity)))

(define (arity-min arity)
  (cond
    ((arity-at-least? arity) (arity-at-least-value arity))
    ((arity-list? arity) (first arity))
    (else arity)))

(define (combine-arity-at-least at-least arity)
  (arity-at-least (+ (arity-at-least-value at-least)
                     (arity-min arity))))

(define (combine-arity-list arity-list definite-arity)
  (if (arity-list? definite-arity)
      (remove-duplicates
       (sort (append-map (λ (a) (map (λ (b) (+ a b)) definite-arity)) arity-list) <))
      (map (λ (x) (+ x definite-arity)) arity-list)))


(define (combine-arities arity-a arity-b)
  (cond
    ((arity-at-least? arity-a) (combine-arity-at-least arity-a arity-b))
    ((arity-at-least? arity-b) (combine-arity-at-least arity-b arity-a))
    ((arity-list? arity-a) (combine-arity-list arity-a arity-b))
    ((arity-list? arity-b) (combine-arity-list arity-b arity-a))
    (else (+ arity-a arity-b))))

#|
(define (spread-combine f g h)
  (let* ((n (get-arity f))
         (m (get-arity g))
         (t (combine-arities n m)))
    (assert (definite-arity? (get-arity f)))
    (assert-arity h 2)
    (define (the-combination . args)
      (assert (has-arity? t (length args)))
      (h (apply f (take args n))
         (apply g (list-tail args n))))
    (restrict-arity! the-combination t)))
|#

#|
(define (compose f g)
  (define (the-composition . args)
    (call-with-values (λ () (apply g args)) f))
  (restrict-arity! the-composition (get-arity g)))
|#

(define (spread-apply f g)
  (let* ((n (get-arity f))
         (m (get-arity g))
         (t (combine-arities n m)))
    (assert (definite-arity? (get-arity f)))
    (define (the-combination . args)
      (assert (has-arity? t (length args)))
      (call-with-values (λ () (apply g (list-tail args n)))
                        (λ gv (call-with-values (λ () (apply f (take args n)))
                                                (λ fv (apply values (append fv gv)))))))
    (restrict-arity! the-combination t)))

(define (spread-combine f g h)
  (compose h (spread-apply f g)))

(define (parallel-combine f g h)
  (compose h (λ args (values (apply f args) (apply g args)))))

(define (list-remove l index)
  (let loop ((i 0)
             (l l)
             (result '()))
    (if (null? l)
        (reverse result)
        (loop (+ 1 i)
              (rest l)
              (if (= i index)
                  result
                  (cons (first l) result))))))

#|
(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (define (the-procedure-augmentor f)
    (define (the-augmented-procedure . args)
      (assert-arity f (- (length args) 1))
      (apply f (list-remove args i)))
    (assert (has-arity? (get-arity f) i)))
    (restrict-arity! the-augmented-procedure
                     (combine-arities 1 (get-arity f))))
  the-procedure-augmentor)

(define ((curry-argument i) . args)
  (assert (exact-nonnegative-integer? i))
  (assert (<= i (length args)))
  (define (the-procedure-augmentor f)
    (assert-arity f (+ (length args) 1))
    (define (the-augmented-procedure arg)
      (apply f (list-insert args arg i)))
    the-augmented-procedure)
  the-procedure-augmentor)
|#

(define (list-insert xs x index)
  (let loop ((xs xs)
             (i 0)
             (result '()))
    (if (null? xs)
        (reverse
         (if (= i index)
             (cons x result)
             result))
        (loop (rest xs)
              (+ i 1)
              (if (= i index)
                  (cons (first xs) (cons x result))
                  (cons (first xs) result))))))

#|
(define (permute-arguments . indices)
  (define (the-permutor f)
    (define (the-permuted . args)
      (assert-arity f (length args))
      (assert (= (length args) (length indices)))
      (apply f (map (λ (i) (list-ref args i)) indices)))
    (restrict-arity! the-permuted (length indices)))
  the-permutor)
|#

(define (compose-args f arg-f)
  (compose f values* arg-f))

(define ((permute-arguments . permute-spec) f)
  (restrict-arity!
   (compose-args f (λ args (map (curry-right list-ref args) permute-spec)))
   (get-arity f)))

(define (((curry-argument i) . args) f)
  (compose f values* (λ (arg) (list-insert args arg i))))

(define ((discard-argument i) f)
  (restrict-arity!
   (compose-args f (curry-left* list-remove i))
   (combine-arities (get-arity f) 1)))

(define (((curry-arguments . curry-spec) . fixed-args) f)
  (assert (sorted? curry-spec <))
  (restrict-arity!
   (compose-args f
                 (λ unfixed-args
                   (foldl (λ (position arg fixed-args) (list-insert fixed-args arg position))
                          fixed-args
                          curry-spec
                          unfixed-args)))
   (length curry-spec)))

(define (sorted? xs compare)
  (if (or (null? xs) (null? (cdr xs)))
      #t
      (and (compare (first xs) (second xs))
           (sorted? (rest xs) compare))))

(define (values* args) (apply values args))

(define (((curry-arguments* position) . fixed-args) f)
  (assert (exact-nonnegative-integer? position))
  (compose-args f (λ args (list-insert fixed-args args position))))

(define ((discard-arguments . discard-spec) f)
  (let ((discard-spec (sort discard-spec >)))
    (restrict-arity!
     (compose-args f (λ args (foldl (swap-args list-remove) args discard-spec)))
     (combine-arities (get-arity f) (length discard-spec)))))

(define (compose2 f g)
  (restrict-arity!
   (λ args (call-with-values (λ () (apply g args)) f))
   (get-arity g)))

(define (compose . fs)
  (if (null? fs)
      values
      (let ((gs (reverse fs)))
        (foldl compose2 (first gs) (rest gs)))))

(define (rotate-right xs)
  (if (null? xs)
      '()
      (let ((rxs (reverse xs)))
        (cons (first rxs) (reverse (rest rxs))))))
(define (rotate-left xs)
  (if (null? xs)
      '()
      (let ((rxs (reverse (rest xs))))
        (reverse (cons (first xs) rxs)))))

(define (rotate-right-args f)
  (compose-args f (λ args (rotate-right args))))
(define (rotate-left-args f)
  (compose-args f (λ args (rotate-left args))))
(define swap-args
  (permute-arguments 1 0))
(define (curry-left f . args)
  ((apply (curry-arguments 0) args) f))
(define (curry-left* f . args)
  ((apply (curry-arguments* 0) args) f))
(define (curry-right f . args)
  ((apply (curry-arguments (length args)) args) f))
(define (curry-right* f . args)
  ((apply (curry-arguments* (length args)) args) f))

(define (join-args f)
  (compose-args f (λ (args) args)))
(define (splat-args f)
  (compose f (λ args args)))

(define (negate f)
  (compose not f))
(provide negate
         join-args
         splat-args
         curry-arguments*
         curry-arguments
         curry-left*
         curry-left
         curry-right*
         curry-right
         rotate-right-args
         rotate-left-args
         compose
         discard-arguments
         values*
         permute-arguments
         parallel-combine
         spread-combine
         spread-apply
         swap-args
         get-arity)