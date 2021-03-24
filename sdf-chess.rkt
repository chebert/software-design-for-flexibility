#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require "sdf-rule-executive.rkt")


(define chess (make-game 'chess))

(define (queen? piece) (eq? (piece-type piece) 'queen))
(define (king? piece) (memv (piece-type piece) '(king initial-king)))
(define (bishop? piece) (eq? (piece-type piece) 'bishop))
(define (rook? piece) (memv (piece-type piece) '(rook initial-rook)))

(define (en-passant-pawn? piece) (eq? (piece-type piece) 'en-passant-pawn))
(define (pawn? piece) (memv (piece-type piece) '(pawn en-passant-pawn)))
(define (knight? piece) (eq? (piece-type piece) 'knight))

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

(define (sliding-move? pmove) (findf slide-direction-flag? (current-flags pmove)))
(define (slide-direction pmove)
  (let* ((flags  (current-flags pmove))
         (flag (findf slide-direction-flag? flags)))
    (if (not flag)
        (error "Could not find slide-direction flag in " flags)
        (slide-direction-from-flag flag))))

(define (make-finished-capture-move new-coords pmove)
  (let ((board (current-board pmove)))
    (and
     (is-position-on-board? new-coords board)
     (is-position-occupied-by-opponent? new-coords board)
     (finish-move (new-piece-position new-coords (capture-piece-at new-coords pmove))))))

(define (make-non-capture-move new-coords pmove)
  (let ((board (current-board pmove)))
    (and 
     (is-position-on-board? new-coords board)
     (is-position-unoccupied? new-coords board)
     (new-piece-position new-coords pmove))))

;; Slide piece in direction by 1 square.
;; Capture an opposing piece, or moves piece to the unoccupied square.
;; returns #f if square is occupied by play piece.
(define (make-capturing-directional-move pmove direction)
  (let* ((piece (current-piece pmove))
         (board (current-board pmove))
         (new-position (coords+ (piece-coords piece) direction)))
    (or
     (make-finished-capture-move new-position pmove)
     (make-non-capture-move new-position pmove))))

(define (make-non-capturing-directional-move pmove direction)
  (let* ((piece (current-piece pmove))
         (board (current-board pmove))
         (new-position (coords+ (piece-coords piece) direction)))
    (make-non-capture-move new-position pmove)))

(define (start-sliding-move pmove direction)
  (make-capturing-directional-move
   (append-flags pmove (list (make-slide-direction-flag direction)))
   direction))

(define (continue-sliding-move pmove)
  (make-capturing-directional-move pmove (slide-direction pmove)))

(define (get-sliding-moves pmove)
  (if (is-pmove-empty? pmove)
      (let* ((piece (current-piece pmove))
             ;; Remove initial status of piece since we are moving it.
             (updated-pmove (remove-piece-initial-status pmove)))
        (filter-map (λ (direction)
                      (start-sliding-move updated-pmove direction))
                    (sliding-piece-directions piece)))
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
    (let ((king (current-piece pmove)))
      (if (king? king)
          (map finish-move
               (filter-map
                (λ (direction)
                  (make-capturing-directional-move
                   ;; king is no longer in the initial position
                   (remove-piece-initial-status pmove)
                   direction))
                all-directions))
          '()))))

(define (black? piece) (eq? (piece-color piece) 'black))
(define (pawn-direction piece) (if (black? piece) south north))
(define (pawn-in-initial-position? piece)
  (if (black? piece)
      (= (coords-y (piece-coords piece)) 1)
      (= (coords-y (piece-coords piece)) 6)))

(define (get-pawn-extended-move pmove piece)
  (let ((move (and (pawn-in-initial-position? piece)
                   (make-non-capturing-directional-move pmove (pawn-direction piece)))))
    (and move (set-pawn-en-passant-status move))))

(define (get-pawn-non-capture-moves pmove piece)
  (let ((single-move (make-non-capturing-directional-move pmove (pawn-direction piece))))
    (if single-move
        (map finish-move
             (filter (λ (x) x) (list single-move (get-pawn-extended-move single-move piece))))
        '())))

(define (get-pawn-capture-directions piece)
  (if (black? piece)
      (list southeast southwest)
      (list northeast northwest)))

(define (make-finished-non-capture-move new-coords pmove)
  (let ((move (make-non-capture-move new-coords pmove)))
    (and move (finish-move move))))

(define (make-en-passant-capture-move new-coords pmove)
  (let* ((board (current-board pmove))
         (piece (current-piece pmove))
         (old-coords (piece-coords piece))
         (offset (coords (- (coords-x new-coords) (coords-x old-coords)) 0))
         (capture-coords (coords+ old-coords offset)))
    (and (is-position-on-board? new-coords board)
         (is-position-on-board? capture-coords board)
         (is-position-occupied-by-opponent? capture-coords board)
         (en-passant-pawn? (board-get capture-coords board))
         (capture-piece-at capture-coords (make-non-capture-move new-coords pmove)))))

(define (get-pawn-capture-moves pmove piece)
  (let ((directions (get-pawn-capture-directions piece)))
    (filter-map (λ (direction)
                  (let* ((new-coords (coords+ (piece-coords piece) direction))
                         (en-passant-move (make-en-passant-capture-move new-coords pmove))
                         (capture-move (or (make-finished-capture-move new-coords pmove)
                                           (and en-passant-move (finish-move en-passant-move)))))
                    capture-move))
                directions)))

(define (pawn-reached-opposite-side? pmove)
  (let* ((piece (current-piece pmove))
         (y (coords-y (piece-coords piece))))
    (and (pawn? piece)
         (if (black? piece)
             (= y 7)
             (= y 0)))))

(define (promotion-moves pmove)
  (map (λ (type)
         (finish-move
          (update-piece (λ (x) (piece-new-type x type)) pmove)))
       '(queen bishop rook knight)))

(define-aggregate-rule 'promote-pawns chess
  (λ (pmoves)
    (append-map
     (λ (pmove)
       (if (pawn-reached-opposite-side? pmove)
           (promotion-moves pmove)
           (list pmove)))
     pmoves)))

(define (remove-piece-initial-status pmove)
  (let ((piece (current-piece pmove)))
    (cond ((initial-king? piece) (update-piece (λ (p) (piece-new-type p 'king)) pmove))
          ((initial-rook? piece) (update-piece (λ (p) (piece-new-type p 'rook)) pmove))
          (else pmove))))
(define (remove-pawn-en-passant-status pmove)
  (let ((pawn (current-piece pmove)))
    (cond ((en-passant-pawn? pawn) (update-piece (λ (p) (piece-new-type p 'pawn)) pmove))
          (else pmove))))
(define (set-pawn-en-passant-status pmove)
  (let ((pawn (current-piece pmove)))
    (cond ((pawn? pawn) (update-piece (λ (p) (piece-new-type p 'en-passant-pawn)) pmove))
          (else pmove))))

(define-evolution-rule 'pawn-non-capture-move chess
  (λ (pmove)
    (let ((pawn (current-piece pmove)))
      (if (and (is-pmove-empty? pmove) (pawn? pawn))
          (get-pawn-non-capture-moves pmove pawn)
          '()))))
(define-evolution-rule 'pawn-capture-move chess
  (λ (pmove)
    (let ((pawn (current-piece pmove)))
      (if (and (is-pmove-empty? pmove) (pawn? pawn))
          (get-pawn-capture-moves pmove pawn)
          '()))))

(define (initial-rook? piece) (eq? (piece-type piece) 'initial-rook))
(define (initial-king? piece) (eq? (piece-type piece) 'initial-king))
(define (every? bools) (foldl (λ (x y) (and x y)) #t bools))
(define (none? bools) (not (every? bools)))
(define (some? bools) (foldl (λ (x y) (or x y)) #f bools))

(define (all-coords-between-x c1 c2)
  (let ((c1x (coords-x c1)) (c2x (coords-x c2)))
    (map (λ (x) (coords x (coords-y c1)))
         (if (< c1x c2x)
             (range (+ 1 c1x) c2x)
             (range (+ 1 c2x) c1x)))))
(define (all-coords-between-x-inclusive c1 c2)
  (let ((c1x (coords-x c1)) (c2x (coords-x c2)))
    (map (λ (x) (coords x (coords-y c1)))
         (if (< c1x c2x)
             (range c1x (+ 1 c2x))
             (range c2x (+ 1 c1x))))))

(define (positions-between-coords-are-unoccupied? board c1 c2)
  (every? (map (λ (c) (is-position-unoccupied? c board))
               (all-coords-between-x c1 c2))))

(define (would-be-in-check? board piece new-coords)
  (in-check? (board-replace piece (piece-new-coords piece new-coords) board)))

(define south-end 7)
(define north-end 0)
(define west-end 0)
(define east-end 7)

(define (west-rook? initial-rook)
  (= west-end (coords-x (piece-coords initial-rook))))

(define (king-castle-offset rook)
  (offset*
   (if (west-rook? rook) west east)
   2))

(define (king-intermediate-castle-positions rook king)
  (all-coords-between-x (coords+ (piece-coords king) (king-castle-offset rook))
                        (piece-coords king)))

(define (does-castling-apply? pmove)
  (let* ((rook (current-piece pmove))
         (board (current-board pmove))
         (king (findf initial-king? (current-pieces board))))
    (and (is-pmove-empty? pmove)
         ;; King and rook are in their initial positions and have not moved.
         (initial-rook? rook)
         king
         ;; all of the spaces the king and rook pass through are unoccupied
         (positions-between-coords-are-unoccupied? board (piece-coords rook) (piece-coords king))
         ;; the king is not currently in check
         (not (in-check? board))
         ;; None of the positions the king passes through would be in check
         (none? (map (λ (coords) (would-be-in-check? board king coords))
                     (king-intermediate-castle-positions rook king))))))

(define (rook-castle-position castled-king rook)
  (if (west-rook? rook)
      (coords+ east (piece-coords castled-king))
      (coords+ west (piece-coords castled-king))))

(define-evolution-rule 'castling chess
  (λ (pmove)
    (if (does-castling-apply? pmove)
        (let* ((board (current-board pmove))
               (king (findf king? (current-pieces board)))
               (rook (current-piece pmove))
               (new-king (piece-new-coords king (coords+ (piece-coords king) (king-castle-offset rook))))
               (new-rook-pos (rook-castle-position new-king rook))
               (pmove2 (new-piece-position new-rook-pos pmove)))
          (list (finish-move
                 (cons (change
                        (board-replace king new-king (current-board pmove2))
                        (current-piece pmove2)
                        (combine-flags '(castle) (current-flags pmove2)))
                       pmove2))))
        '())))

(define knight-offsets
  (list (coords 1 2)
        (coords 2 1)
        (coords -1 2)
        (coords 2 -1)
        (coords 1 -2)
        (coords -2 1)
        (coords -1 -2)
        (coords -2 -1)))

(define-evolution-rule 'knight-move chess
  (λ (pmove)
    (let ((piece (current-piece pmove)))
      (if (knight? piece)
          (filter-map 
           (λ (offset)
             (let ((new-coords (coords+ (piece-coords piece) offset)))
               (or (make-finished-capture-move new-coords pmove)
                   (make-finished-non-capture-move new-coords pmove))))
           knight-offsets)
          '()))))

(define (other-color color)
  (if (eq? color 'black)
      'white
      'black))
(define (board-swap-color b)
  (board (board-pieces b) (other-color (board-current-color b))))
(define (other-pieces board)
  (filter (λ (piece) (eq? (piece-color piece) (other-color (board-current-color board))))
          (board-pieces board)))

(define (captures-king? pmove)
  (and (captures-pieces? pmove)
       (not (findf king? (other-pieces (current-board pmove))))))

(define (in-check? board)
  (findf captures-king? (execute-rules (initial-pmoves (board-swap-color board)) (get-evolution-rules chess) '())))

(define (results-in-check? pmove) (in-check? (current-board pmove)))
(define-aggregate-rule 'remove-check-moves chess
  (λ (pmoves)
    (filter (λ (pmove) (not (results-in-check? pmove)))
            pmoves)))

(define (board-replace-en-passant-pawns b)
  (let ((current-color (board-current-color b)))
    (board
     (map (λ (piece)
            (if (and (not (eq? (piece-color piece) current-color))
                     (en-passant-pawn? piece))
                (piece-new-type piece 'pawn)
                piece))
          (board-pieces b))
     current-color)))

;; at the end of the turn, remove the en-passant status from all pawns for the next color.
(define-aggregate-rule 'remove-en-passant-status chess
  (λ (pmoves)
    (map (λ (pmove)
           (cons (change
                  (board-replace-en-passant-pawns (current-board pmove))
                  (current-piece pmove)
                  (current-flags pmove))
                 pmove))
         pmoves)))

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
   (if (black? piece)
       "b"
       "w")
   (cond
     ((queen? piece) "Q")
     ((bishop? piece) "B")
     ((initial-rook? piece) "r")
     ((rook? piece) "R")
     ((initial-king? piece) "k")
     ((king? piece) "K")
     ((en-passant-pawn? piece) "p")
     ((pawn? piece) "P")
     ((knight? piece) "N"))))

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

(define (captured-piece old-board current-board old-piece current-piece)
  (let ((diff (set-subtract (remove old-piece (board-pieces old-board))
                            (remove current-piece (board-pieces current-board)))))
    (if (null? diff) #f (first diff))))

(define (castling-move? pmove) (memv 'castle (current-flags pmove)))

(define (move->string pmove)
  (let* ((initial-pmove (last pmove))
         (initial-board (get-board initial-pmove))
         (initial-piece (get-piece initial-pmove))
         (board (current-board pmove))
         (piece (current-piece pmove))
         (current-coords (piece-coords piece))
         (flags (current-flags pmove)))
    (string-append
     (piece->string initial-piece) " moves to " (coords->string current-coords) "\n"
     (if (piece-changed-type? initial-piece piece)
         (string-append " and changes into a " (stringify (piece-type piece)) "\n")
         "")   
     (if (captures-pieces? pmove)
         (string-append "  capturing " (piece->string (captured-piece initial-board board initial-piece piece)) "\n")
         "")
     (if (sliding-move? pmove)
         (string-append "  by sliding " (direction->string (slide-direction pmove)) "\n")
         "")
     (if (castling-move? pmove)
         "  by castling\n"
         "")
     (board->string board initial-board piece))))

(define test-board
  (board
   (list (piece 'white (coords 4 4) 'queen)
         ;(piece 'black (coords 4 6) 'queen)
         (piece 'white (coords 2 2) 'bishop)
         ;(piece 'white (coords 2 6) 'rook)
         (piece 'white (coords 5 6) 'pawn)
         (piece 'black (coords 4 5) 'pawn)
         (piece 'white (coords 3 4) 'knight)
         
         ;(piece 'black (coords 4 1) 'bishop)
         (piece 'black (coords 7 3) 'en-passant-pawn)
         (piece 'white (coords 6 3) 'pawn)
         (piece 'white (coords 1 1) 'pawn)

         (piece 'white (coords 4 7) 'initial-king)
         (piece 'white (coords 0 7) 'initial-rook)
         (piece 'white (coords 7 7) 'initial-rook))
   'white))

(define (initial-pmoves board)
  (map (λ (piece) (initial-pmove board piece))
        (current-pieces board)))

(define (generate-moves board game)
  (execute-rules
   (initial-pmoves board)
   (get-evolution-rules game) (get-aggregate-rules game)))

(define start-board
  (board
   (append
    (list
     (piece 'black (coords 0 0) 'initial-rook)
     (piece 'black (coords 1 0) 'knight)
     (piece 'black (coords 2 0) 'bishop)
     (piece 'black (coords 3 0) 'queen)
     (piece 'black (coords 4 0) 'king)
     (piece 'black (coords 5 0) 'bishop)
     (piece 'black (coords 6 0) 'knight)
     (piece 'black (coords 7 0) 'initial-rook))
    (map (λ (x) (piece 'black (coords x 1) 'pawn)) (range 8))
    (list
     (piece 'white (coords 0 7) 'initial-rook)
     (piece 'white (coords 1 7) 'knight)
     (piece 'white (coords 2 7) 'bishop)
     (piece 'white (coords 3 7) 'queen)
     (piece 'white (coords 4 7) 'king)
     (piece 'white (coords 5 7) 'bishop)
     (piece 'white (coords 6 7) 'knight)
     (piece 'white (coords 7 7) 'initial-rook))
    (map (λ (x) (piece 'white (coords x 6) 'pawn)) (range 8)))
   'white))