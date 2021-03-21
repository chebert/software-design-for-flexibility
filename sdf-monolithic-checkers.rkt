#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")

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

;; Coords
(struct coords (x y) #:transparent)
(define (coords= c1 c2) (and (= (coords-x c1) (coords-x c2))
                             (= (coords-y c1) (coords-y c2))))
(define (coords+ c1 c2) (coords (+ (coords-x c1) (coords-x c2))
                               (+ (coords-y c1) (coords-y c2))))

;; Pieces
(struct piece (color coords king?) #:transparent)

(define white-directions (list (coords -1 1)
                               (coords 1 1)))

(define black-directions (list (coords -1 -1)
                               (coords 1 -1)))

(define (possible-directions piece)
  (if (piece-king? piece)
      (append white-directions black-directions)
      (if (eq? (piece-color piece) 'black)
          black-directions
          white-directions)))

(define (should-be-crowned? piece)
  (if (piece-king? piece)
      #f
      (let ((y (coords-y (piece-coords piece))))
        (if (eq? 'black (piece-color piece))
            (= y 0)
            (= y 7)))))
(define (crown-piece p)
  (piece (piece-color p) (piece-coords p) #t))

;; Moves
(struct simple-move (coords piece board) #:transparent)
(struct jump (landing coords piece board) #:transparent)
(struct replace-piece (new-piece old-piece board) #:transparent)
(define make-simple-move simple-move)
(define make-jump jump)

;; Apply a move to a piece.
(define (step-to step)
  (cond ((simple-move? step)
         (let ((old-piece (simple-move-piece step)))
           ;; Update the coordinates of the piece.
           (piece (piece-color old-piece)
                  (simple-move-coords step)
                  (piece-king? old-piece))))
        ((jump? step)
         (let ((old-piece (jump-piece step)))
           ;; Update the coordinates of the piece.
           (piece (piece-color old-piece)
                  (jump-landing step)
                  (piece-king? old-piece))))
        ((replace-piece? step) (replace-piece-new-piece step))
        (else (error "Unknown step" step))))

;; Apply a move to a board.
(define (step-board step)
  (cond ((simple-move? step)
         ;; Replace the piece with the moved piece.
         (board-replace (simple-move-piece step) (step-to step) (simple-move-board step)))
        ((jump? step)
         ;; Remove the enemy piece, and replace the piece with the moved piece.
         (board-replace (jump-piece step) (step-to step) (board-remove (jump-coords step) (jump-board step))))
        ((replace-piece? step)
         (board-replace (replace-piece-old-piece step)
                        (replace-piece-new-piece step)
                        (replace-piece-board step)))
        (else (error "Unknown step" step))))

(define (path-contains-jumps? path)
  (memf jump? path))

(define (try-step piece board direction path)
  (let ((new-coords (coords+ (piece-coords piece) direction)))
    (and (is-position-on-board? new-coords board)
         (case (position-info new-coords board)
           ((unoccupied)
            (and (not (path-contains-jumps? path))
                 (cons (make-simple-move new-coords
                                         piece
                                         board)
                       path)))
           ((occupied-by-opponent)
            (let ((landing (coords+ new-coords direction)))
              (and (is-position-on-board? landing board)
                   (is-position-unoccupied? landing board)
                   (cons (make-jump landing
                                    new-coords
                                    piece
                                    board)
                         path))))
           ((occupied-by-self) #f)
           (else (error "Unknown position info" (position-info new-coords board)))))))

(define (compute-next-steps piece board path)
  (filter-map (λ (direction) (try-step piece board direction path))
              (possible-directions piece)))

(define (evolve-paths piece board)
  (let ((paths (compute-next-steps piece board '())))
    (let ((jumps (filter path-contains-jumps? paths)))
      (if (null? jumps)
          paths
          (evolve-jumps jumps)))))

(define (evolve-jumps paths)
  (append-map (λ (path)
                (let ((paths
                       (let ((step (car path)))
                         (compute-next-steps (step-to step)
                                             (step-board step)
                                             path))))
                  (if (null? paths)
                      (list path)
                      (evolve-jumps paths))))
              paths))

(define (generate-moves board)
  (crown-kings
   (mandate-jumps
    (append-map (λ (piece)
                  (evolve-paths piece board))
                (current-pieces board)))))

(define (mandate-jumps paths)
  (let ((jumps (filter path-contains-jumps? paths)))
    (if (null? jumps)
        paths
        jumps)))

(define (crown-kings paths)
  (map (λ (path)
         (let ((piece (step-to (car path))))
           (if (should-be-crowned? piece)
               (cons (replace-piece (crown-piece piece)
                                    piece
                                    (step-board (car path)))
                     path)
               path)))
       paths))


;; Pretty-output for seeing what's happening

(define (board->string board old-board)
  (string-append "Current: " (symbol->string (board-current-color board)) "\n"
                 "   0   1   2   3   4   5   6   7\n"
                 (apply string-append
                        (map (λ (row)
                               (string-append (board-row->string board row old-board) "\n"))
                             (range 8)))
                 " +---+---+---+---+---+---+---+---+"))
(define (piece->string piece old?)
  (string-append (if old? "(" " ")
                 (if (piece-king? piece)
                     (if (eq? 'black (piece-color piece)) "B" "W")
                     (if (eq? 'black (piece-color piece)) "b" "w"))
                 (if old? ")" " ")))
    
(define (board-row->string board row old-board)
  (apply string-append
         " +---+---+---+---+---+---+---+---+\n"
         (number->string row) "|"
         (map (λ (col)
                (let ((piece (board-get (coords col row) board))
                      (old-piece (and old-board (board-get (coords col row) old-board))))
                  (string-append
                   (cond
                    (piece (piece->string piece #f))
                    (old-piece (piece->string old-piece #t))
                    ((even? (+ col row)) " . ")
                    (else "   "))
                   "|")))
              (range 8))))

(define (coords->string coords)
  (string-append "<"
                 (number->string (coords-x coords))
                 ","
                 (number->string (coords-y coords))
                 ">"))
(define (step->string step)
  (string-append
   (cond
     ((simple-move? step)
      (let ((piece (simple-move-piece step)))
        (string-append "Move " (symbol->string (piece-color piece))
                       " piece at " (coords->string (piece-coords piece))
                       " to " (coords->string (simple-move-coords step))
                       "\n")))
     ((jump? step)
      (let ((piece (jump-piece step)))
        (string-append "Jump " (symbol->string (piece-color piece))
                       " piece at " (coords->string (piece-coords piece))
                       " to " (coords->string (jump-landing step))
                       "\n")))
     ((replace-piece? step)
      (let ((piece (replace-piece-new-piece step)))
        (string-append "King " (symbol->string (piece-color piece))
                       " piece at " (coords->string (piece-coords piece))
                       "\n"))))
   (board->string (step-board step)
                  (cond
                    ((simple-move? step) (simple-move-board step))
                    ((jump? step) (jump-board step))
                    ((replace-piece? step) (replace-piece-board step))))))

(define (path->string path path-n)
  (let ((n -1))
    (apply string-append (map (λ (step)
                                (set! n (+ 1 n))
                                (string-append "<Path " (number->string path-n) " Step " (number->string n) ">\n" (step->string step) "\n"))
                              path))))

(define (paths->string paths)
  (let ((n -1))
    (apply string-append
           "<<<Showing " (number->string (length paths)) " paths>>>\n"
           (map (λ (path)
                  (set! n (+ 1 n))
                  (string-append "<<New Path " (number->string n) ">>\n" (path->string (reverse path) n) "\n"))
                paths))))

;; Boards

(define start-board
  (board
   (list
    (piece 'white (coords 1 0) #f)
    (piece 'white (coords 3 0) #f)
    (piece 'white (coords 5 0) #f)
    (piece 'white (coords 7 0) #f)
    
    (piece 'white (coords 0 1) #f)
    (piece 'white (coords 2 1) #f)
    (piece 'white (coords 4 1) #f)
    (piece 'white (coords 6 1) #f)

    (piece 'white (coords 1 2) #f)
    (piece 'white (coords 3 2) #f)
    (piece 'white (coords 5 2) #f)
    (piece 'white (coords 7 2) #f)

    (piece 'black (coords 0 7) #f)
    (piece 'black (coords 2 7) #f)
    (piece 'black (coords 4 7) #f)
    (piece 'black (coords 6 7) #f)

    (piece 'black (coords 1 6) #f)
    (piece 'black (coords 3 6) #f)
    (piece 'black (coords 5 6) #f)
    (piece 'black (coords 7 6) #f)

    (piece 'black (coords 0 5) #f)
    (piece 'black (coords 2 5) #f)
    (piece 'black (coords 4 5) #f)
    (piece 'black (coords 6 5) #f))
   'white))

(define test-jump-board
  (board
   (list
    (piece 'white (coords 1 0) #f)
    (piece 'white (coords 3 0) #f)
    (piece 'white (coords 5 0) #f)
    (piece 'white (coords 7 0) #f)
    
    (piece 'white (coords 0 1) #f)
    (piece 'white (coords 2 1) #f)
    (piece 'white (coords 4 1) #f)
    ;(piece 'white (coords 6 1) #f)

    (piece 'white (coords 1 2) #f)
    (piece 'white (coords 3 2) #f)
    (piece 'white (coords 5 2) #f)
    (piece 'white (coords 5 4) #f)

    (piece 'black (coords 0 7) #f)
    (piece 'black (coords 2 7) #f)
    (piece 'black (coords 4 7) #f)
    (piece 'black (coords 6 7) #f)

    (piece 'black (coords 1 6) #f)
    (piece 'black (coords 3 6) #f)
    (piece 'black (coords 5 6) #f)
    (piece 'black (coords 7 6) #f)

    (piece 'black (coords 4 1) #f)
    (piece 'black (coords 2 5) #f)
    (piece 'black (coords 4 5) #f)
    (piece 'black (coords 6 5) #f))
   'black))


(define test-king-piece-board
  (board
   (list
    (piece 'white (coords 1 0) #f)
    (piece 'white (coords 3 0) #f)
    (piece 'white (coords 5 0) #f)
    ;(piece 'white (coords 7 0) #f)
    
    (piece 'white (coords 0 1) #f)
    (piece 'white (coords 2 1) #f)
    (piece 'white (coords 4 1) #f)
    ;(piece 'white (coords 6 1) #f)

    (piece 'white (coords 1 2) #f)
    (piece 'white (coords 3 2) #f)
    (piece 'white (coords 5 2) #f)
    ;(piece 'white (coords 5 4) #f)

    (piece 'black (coords 0 7) #f)
    (piece 'black (coords 2 7) #f)
    (piece 'black (coords 4 7) #f)
    (piece 'black (coords 6 7) #f)

    (piece 'black (coords 1 6) #f)
    (piece 'black (coords 3 6) #f)
    (piece 'black (coords 5 6) #f)
    (piece 'black (coords 7 6) #f)

    (piece 'black (coords 4 1) #f)
    (piece 'black (coords 2 5) #f)
    (piece 'black (coords 4 5) #f)
    (piece 'black (coords 6 1) #f))
   'black))


(displayln 'test-jump-board)
(displayln (board->string test-jump-board #f))

(displayln 'test-king-piece-board)
(displayln (board->string test-king-piece-board #f))

(displayln (list 'possible-moves-for 'test-jump-board))
(displayln (paths->string (generate-moves test-jump-board)))