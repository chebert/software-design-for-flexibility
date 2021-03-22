#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require "sdf-rule-executive.rkt")


(define chess (make-game 'chess))

(define (queen? piece) (eq? (piece-type piece) 'queen))
(define (king? piece) (eq? (piece-type piece) 'king))
(define (bishop? piece) (eq? (piece-type piece) 'bishop))
(define (rook? piece) (eq? (piece-type piece) 'rook))
(define (pawn? piece) (eq? (piece-type piece) 'pawn))

(define west (coords -1 0))
(define east (coords 1 0))
(define north (coords 0 -1))
(define south (coords 0 1))

(define northwest (coords+ north west))
(define northeast (coords+ north east))
(define southwest (coords+ south west))
(define southeast (coords+ south east))

(define diagonal-directions
  (list northwest northeast southwest southeast))
(define orthogonal-directions
  (list north south east west))
(define all-directions
  (append diagonal-directions orthogonal-directions))

(define (piece-slides? piece)
  (or (queen? piece)
      (bishop? piece)
      (rook? piece)))
(define (sliding-piece-directions piece)
  (cond ((queen? piece) all-directions)
        ((bishop? piece) diagonal-directions)
        ((rook? piece) orthogonal-directions)
        (else '())))

(define (make-slide-direction-flag direction) (list 'slide-direction direction))
(define (slide-direction-flag? flag) (and (pair? flag) (eq? (first flag) 'slide-direction)))
(define (slide-direction-from-flag slide-direction-flag) (second slide-direction-flag))

(define (sliding-move? pmove) (findf slide-direction-flag? (get-flags (first pmove))))
(define (slide-direction pmove)
  (let* ((flags  (get-flags (first pmove)))
         (flag (findf slide-direction-flag? flags)))
    (if (not flag)
        (error "Could not find slide-direction flag in " flags)
        (slide-direction-from-flag flag))))

(define (finish-capture-move new-coords pmove)
  (finish-move (new-piece-position new-coords (capture-piece-at new-coords pmove))))

;; Slide piece in direction by 1 square.
;; Capture an opposing piece, or moves piece to the unoccupied square.
;; returns #f if square is occupied by play piece.
(define (make-capturing-directional-move pmove direction)
  (let* ((piece (current-piece pmove))
         (board (current-board pmove))
         (new-position (coords+ (piece-coords piece) direction)))
    (and
     (is-position-on-board? new-position board)
     (cond
       ((is-position-occupied-by-opponent? new-position board)
        (finish-capture-move new-position pmove))
       ((is-position-unoccupied? new-position board)
        (new-piece-position new-position pmove))
       (else #f)))))

(define (make-non-capturing-directional-move pmove direction)
  (let* ((piece (current-piece pmove))
         (board (current-board pmove))
         (new-position (coords+ (piece-coords piece) direction)))
    (and
     (is-position-on-board? new-position board)
     (is-position-unoccupied? new-position board)
     (new-piece-position new-position pmove))))


(define (start-sliding-move pmove direction)
  (make-capturing-directional-move
   (update-piece-and-append-flags (λ (x) x) pmove (list (make-slide-direction-flag direction)))
   direction))

(define (continue-sliding-move pmove)
  (make-capturing-directional-move pmove (slide-direction pmove)))

(define (get-sliding-moves pmove)
  (if (is-pmove-empty? pmove)
      (filter-map (λ (direction) (start-sliding-move pmove direction))
                  (sliding-piece-directions (current-piece pmove)))
      (let ((move (continue-sliding-move pmove)))
        (if move
            (list (finish-move pmove) move)
            (list (finish-move pmove))))))

(define-evolution-rule 'sliding-move chess
  (λ (pmove)
    (let ((piece (current-piece pmove)))
      (if (piece-slides? piece)
          (get-sliding-moves pmove)
          '()))))

(define-evolution-rule 'king-move chess
  (λ (pmove)
    (let ((piece (current-piece pmove)))
      (if (king? piece)
          (map finish-move
               (filter-map (λ (direction) (make-capturing-directional-move pmove direction))
                           all-directions))
          '()))))

(define (black? piece) (eq? (piece-color piece) 'black))
(define (pawn-direction piece) (if (black? piece) south north))
(define (pawn-in-initial-position? piece)
  (if (black? piece)
      (= (coords-y (piece-coords piece)) 1)
      (= (coords-y (piece-coords piece)) 6)))

(define (get-pawn-extended-move pmove piece)
  (and (pawn-in-initial-position? piece)
       (make-non-capturing-directional-move pmove (pawn-direction piece))))

(define (get-pawn-simple-moves pmove piece)
  (let ((single-move (make-non-capturing-directional-move pmove (pawn-direction piece))))
    (if single-move
        (map finish-move
             (filter (λ (x) x) (list single-move (get-pawn-extended-move single-move piece))))
        '())))

(define (get-pawn-capture-directions piece)
  (if (black? piece)
      (list southeast southwest)
      (list northeast northwest)))
      

(define (get-pawn-capture-moves pmove piece)
  (let ((directions (get-pawn-capture-directions piece)))
    (filter-map (λ (direction)
                  (let ((new-coords (coords+ (piece-coords piece) direction))
                        (board (current-board pmove)))
                    (and
                     (is-position-on-board? new-coords board)
                     (is-position-occupied-by-opponent? new-coords board)
                     (finish-capture-move new-coords pmove))))
                directions)))

(define-evolution-rule 'pawn-simple-move chess
  (λ (pmove)
    (let ((piece (current-piece pmove)))
      (if (pawn? piece)
          (append (get-pawn-simple-moves pmove piece)
                  (get-pawn-capture-moves pmove piece))
          '()))))

;; Draw the board.
(define (stringify datum)
  (let ((o (open-output-string)))
    (write datum o)
    (get-output-string o)))

(define (coords->string coords)
  (string-append "<" (number->string (coords-x coords)) ", " (number->string (coords-y coords)) ">"))

(define (piece->string piece)
  (string-append "the " (stringify (piece-color piece)) " " (stringify (piece-type piece)) " at " (coords->string (piece-coords piece))))

(define (piece->board-string piece)
  (string-append
   (if (eq? 'black (piece-color piece))
       "b"
       "w")
   (cond
     ((queen? piece) "Q")
     ((bishop? piece) "B")
     ((rook? piece) "R")
     ((king? piece) "K")
     ((pawn? piece) "P"))))

(define (board-coords->string board old-board coords current-piece)
  (let ((piece (board-get coords board))
        (old-piece (board-get coords old-board)))
    (cond
      ((equal? current-piece piece)
       (string-append "<" (piece->board-string piece) ">"))
      (piece
       (string-append " " (piece->board-string piece) " "))
      (old-piece
       (string-append "(" (piece->board-string old-piece) ")"))
      ("    "))))

(define (piece-changed-type? old-piece new-piece)
  (not (eq? (piece-type old-piece) (piece-type new-piece))))

(define (board->string board initial-board current-piece)
  (string-append
   "    0    1     2    3    4   5    6     7\n"
   (let loop-rows ((row 0)
                   (result ""))
     (cond ((= row 8) result)
           (else
            (loop-rows
             (+ 1 row)
             (string-append
              result
              "  +----+----+----+----+----+----+----+----+\n"
              (number->string row)
              (let loop-cols ((col 0)
                              (result " |"))
                (cond ((= col 8)
                       (string-append result "\n"))
                      (else
                       (loop-cols
                        (+ col 1)
                        (string-append result (board-coords->string board initial-board (coords col row) current-piece) "|"))))))))))
     
   "  +----+----+----+----+----+----+----+----+\n"))

(define (direction->string direction)
  (cond
    ((equal? direction north) "north")
    ((equal? direction south) "south")
    ((equal? direction east) "east")
    ((equal? direction west) "west")
    ((equal? direction northeast) "northeast")
    ((equal? direction northwest) "northwest")
    ((equal? direction southeast) "southeast")
    ((equal? direction southwest) "southwest")))

(define (move->string pmove)
  (let* ((initial-pmove (last pmove))
         (initial-board (get-board initial-pmove))
         (initial-piece (get-piece initial-pmove))
         (board (current-board pmove))
         (piece (current-piece pmove))
         (current-coords (piece-coords piece))
         (flags (get-flags (first pmove))))
    (string-append
     (piece->string initial-piece) " moves to " (coords->string current-coords) "\n"
     (if (piece-changed-type? initial-piece piece)
         (string-append " and changes into a " (stringify (piece-type piece)) "\n")
         "")   
     (if (captures-pieces? pmove)
         (string-append "  capturing " (piece->string (board-get current-coords initial-board)) "\n")
         "")
     (if (sliding-move? pmove)
         (string-append "  by sliding " (direction->string (slide-direction pmove)) "\n")
         "")
     (board->string board initial-board piece))))

(define test-board
  (board
   (list (piece 'white (coords 4 4) 'queen)
         (piece 'black (coords 4 7) 'queen)
         (piece 'white (coords 2 2) 'bishop)
         (piece 'white (coords 2 6) 'rook)
         (piece 'white (coords 5 2) 'king)
         (piece 'white (coords 5 6) 'pawn)
         (piece 'black (coords 4 5) 'pawn))
   'white))

(define (initial-pmoves board)
  (map (λ (piece) (initial-pmove board piece))
        (current-pieces board)))

(define (generate-moves board game)
  (execute-rules
   (initial-pmoves board)
   (get-evolution-rules game) (get-aggregate-rules game)))
