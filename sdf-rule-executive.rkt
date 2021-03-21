#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")


;; Coords
(struct coords (x y) #:transparent)
(define (coords= c1 c2) (and (= (coords-x c1) (coords-x c2))
                             (= (coords-y c1) (coords-y c2))))
(define (coords+ c offset) (coords (+ (coords-x c) (coords-x offset))
                                   (+ (coords-y c) (coords-y offset))))
(define (offset* direction distance)
  (coords (* (coords-x direction) distance)
          (* (coords-y direction) distance)))

;; Pieces
(struct piece (color coords type) #:transparent)

(define (piece-new-type piece type)
  (piece (piece-color piece)
         (piece-coords piece)
         type))

(define (piece-new-coords p coords)
  (piece (piece-color p)
         coords
         (piece-type p)))

;; Board
(struct board (pieces current-color) #:transparent)

(define (current-pieces board)
  (let ((current-color (board-current-color board)))
    (filter (λ (piece) (eq? (piece-color piece) current-color)) (board-pieces board))))

(define (position-info coords board)
  (let ((piece (board-get coords board)))
    (cond ((not piece) 'unoccupied)
          ((eq? (piece-color piece) (board-current-color board)) 'occupied-by-self)
          (else 'occupied-by-opponent))))
(define (is-position-on-board? coords board)
  (let ((x (coords-x coords))
        (y (coords-y coords)))
    (and (<= 0 x 7)
         (<= 0 y 7)
         (odd? (+ x y)))))
(define (is-position-unoccupied? coords board)
  (eq? 'unoccupied (position-info coords board)))
(define (is-position-occupied-by-opponent? coords board)
  (eq? 'occupied-by-opponent (position-info coords board)))

(define (board-get coords board)
  (findf (λ (piece) (coords= (piece-coords piece) coords)) (board-pieces board)))
(define (board-remove coords b)
  (board
   (filter (λ (piece) (not (coords= (piece-coords piece) coords))) (board-pieces b))
   (board-current-color b)))
(define (board-replace old-piece new-piece b)
  (board
   (cons new-piece (board-pieces (board-remove (piece-coords old-piece) b)))
   (board-current-color b)))

;; Change
(struct change (board piece flags) #:transparent)
(define get-board change-board)
(define get-piece change-piece)
(define get-flags change-flags)

;; Partial Moves
(define (initial-pmove board piece)
  (list (change board piece '())))
(define (is-pmove-empty? pmove)
  (null? (rest pmove)))
(define (is-pmove-finished? pmove)
  (memv 'finished (get-flags (first pmove))))
(define (current-board pmove)
  (get-board (first pmove)))
(define (current-piece pmove)
  (get-piece (first pmove)))

(define (combine-flags flags old-flags) (set-union flags old-flags))

(define (update-piece-and-append-flags procedure pmove flags)
  (let* ((old-piece (current-piece pmove))
         (new-piece (procedure old-piece)))
    (cons
     (change
      (board-replace old-piece new-piece (current-board pmove))
      new-piece
      (combine-flags flags (get-flags (first pmove))))
     pmove)))

(define (update-piece procedure pmove)
  (update-piece-and-append-flags procedure pmove '()))

(define (new-piece-position coords pmove)
  (update-piece-and-append-flags
   (λ (piece) (piece-new-coords piece coords))
   pmove
   '()))
(define (finish-move pmove)
  (cons (change (current-board pmove) (current-piece pmove) (combine-flags '(finished) (get-flags (first pmove))))
        pmove))
(define (captures-pieces? pmove)
  (memv 'capture-pieces (get-flags (first pmove))))
(define (capture-piece-at coords pmove)
  (cons
   (change
    (board-remove coords (current-board pmove))
    (current-piece pmove)
    (combine-flags '(capture-pieces) (get-flags (first pmove))))
   pmove))

(define (compute-new-position direction distance pmove)
  (coords+ (piece-coords (current-piece pmove))
           (offset* direction distance)))

;; Rule executive
(define (execute-rules initial-pmoves evolution-rules aggregate-rules)
  ((foldl compose (λ (x) x) aggregate-rules) ; aggregate-rules applied all at once at the end.
   (append-map (λ (pmove)
                 (evolve-pmove pmove evolution-rules))
               initial-pmoves)))

(define (evolve-pmove pmove evolution-rules)
  (append-map (λ (new-pmove)
                (if (is-pmove-finished? new-pmove)
                    (list new-pmove)
                    (evolve-pmove new-pmove evolution-rules)))
              (append-map (λ (evolution-rule)
                            (evolution-rule pmove))
                          evolution-rules)))

(define game? (make-bundle-predicate 'game))
(define (make-game symbol)
  (let ((evolution-rules (make-hasheq))
        (aggregate-rules (make-hasheq)))
    (λ args
      (cond ((null? args) symbol)
            ((eq? (first args) game?) (game?))
            ((eq? (first args) 'add-evolution-rule!)
             (hash-set! evolution-rules (second args) (third args)))
            ((eq? (first args) 'add-aggregate-rule!)
             (hash-set! aggregate-rules (second args) (third args)))
            ((eq? (first args) 'get-aggregate-rules)
             (hash-values aggregate-rules))
            ((eq? (first args) 'get-evolution-rules)
             (hash-values evolution-rules))))))

(define (define-evolution-rule name game behavior)
  (game 'add-evolution-rule! name behavior))

(define (define-aggregate-rule name game behavior)
  (game 'add-aggregate-rule! name behavior))
(define (get-evolution-rules game) (game 'get-evolution-rules))
(define (get-aggregate-rules game) (game 'get-aggregate-rules))

(provide (struct-out coords)
         coords=
         coords+
         offset*
         (struct-out piece)
         piece-new-type
         piece-new-coords
         (struct-out board)
         current-pieces
         position-info
         is-position-on-board?
         is-position-unoccupied?
         is-position-occupied-by-opponent?
         board-get
         board-remove
         board-replace
         (struct-out change)
         get-board
         get-piece
         get-flags
         initial-pmove
         is-pmove-empty?
         is-pmove-finished?
         current-board
         current-piece
         combine-flags
         update-piece-and-append-flags
         update-piece
         new-piece-position
         finish-move
         captures-pieces?
         capture-piece-at
         compute-new-position
         execute-rules
         game?
         make-game
         define-evolution-rule
         define-aggregate-rule
         get-evolution-rules
         get-aggregate-rules)