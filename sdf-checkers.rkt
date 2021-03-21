#lang racket


(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require "sdf-rule-executive.rkt")


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
  (piece (piece-color p) (piece-coords p) 'king))


(define (piece-king? piece)
  (eq? 'king (piece-type piece)))

;; Checkers rules

(define checkers (make-game 'checkers))

(define-evolution-rule 'simple-move checkers
  (λ (pmove)
    (if (is-pmove-empty? pmove)
        (get-simple-moves pmove)
        '())))

(define (get-simple-moves pmove)
  (filter-map
   (λ (direction)
     (let ((landing (compute-new-position direction 1 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (finish-move (new-piece-position landing pmove)))))
   (possible-directions (current-piece pmove))))

(define-evolution-rule 'jump checkers
  (λ (pmove)
    (let ((jumps (get-jumps pmove)))
      (cond ((not (null? jumps)) jumps)
            ((is-pmove-empty? pmove) '())
            (else
             (list (finish-move pmove)))))))
(define (get-jumps pmove)
  (filter-map
   (λ (direction)
     (let ((possible-jump (compute-new-position direction 1 pmove))
           (landing (compute-new-position direction 2 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (is-position-occupied-by-opponent? possible-jump board)
            (capture-piece-at possible-jump
                              (new-piece-position landing pmove)))))
   (possible-directions (current-piece pmove))))

(define-aggregate-rule 'coronation checkers
  (λ (pmoves)
    (map (λ (pmove)
           (let ((piece (current-piece pmove)))
             (if (should-be-crowned? piece)
                 (update-piece crown-piece pmove)
                 pmove)))
         pmoves)))

(define-aggregate-rule 'require-jumps checkers
  (λ (pmoves)
    (let ((jumps (filter captures-pieces? pmoves)))
      (if (null? jumps)
          pmoves
          jumps))))

;; Pretty-output for seeing what's happening

(define (board->string board old-board)
  (string-append "Current: " (symbol->string (board-current-color board)) "\n"
                 "   0   1   2   3   4   5   6   7\n"
                 (apply string-append
                        (map (λ (row)
                               (string-append (board-row->string board row old-board) "\n"))
                             (range 8)))
                 " +---+---+---+---+---+---+---+---+"))
(define (board-piece->string piece old?)
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
                    (piece (board-piece->string piece #f))
                    (old-piece (board-piece->string old-piece #t))
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
(define (write-string datum)
  (let ((o (open-output-string)))
    (write datum o)
    (get-output-string o)))

(define (flags->string flags)
  (string-append "Change flags: "
                 (write-string flags)))
(define (piece->string piece)
  (string-append "Piece "
                 (write-string (piece-color piece))
                 " at "
                 (write-string (piece-coords piece))))
(define (change->string change old-change)
  (string-append
   (flags->string (get-flags change)) "\n"
   (piece->string (get-piece change)) "\n"
   (board->string (get-board change) (if old-change (get-board old-change) #f)) "\n"))

(define (move->string move move-string)
  (apply string-append
         (let loop ((result '())
                    (changes move)
                    (num (- (length move) 1)))
           (cond
             ((null? changes) result)
             ((null? (cdr changes))
              (loop (cons (string-append "<<" move-string ", Change " (number->string num) ">>\n"
                                         (change->string (first changes) #f)) result)
                    (rest changes)
                    (- num 1)))
             (else
              (loop (cons (string-append "<<" move-string ", Change " (number->string num) ">>\n"
                                         (change->string (first changes) (second changes))) result)
                    (rest changes)
                    (- num 1)))))))

(define (moves->string moves)
  (apply string-append
         (let loop ((result '())
                    (moves moves)
                    (num 0))
           (cond
             ((null? moves) (reverse result))
             (else
              (let ((move-string (string-append "Move " (number->string num))))
                (loop (cons (string-append "<<<" move-string ">>>\n"
                                           (move->string (first moves) move-string))
                            result)
                      (rest moves)
                      (+ num 1))))))))

;; Boards

(define start-board
  (board
   (list
    (piece 'white (coords 1 0) '())
    (piece 'white (coords 3 0) '())
    (piece 'white (coords 5 0) '())
    (piece 'white (coords 7 0) '())
    
    (piece 'white (coords 0 1) '())
    (piece 'white (coords 2 1) '())
    (piece 'white (coords 4 1) '())
    (piece 'white (coords 6 1) '())

    (piece 'white (coords 1 2) '())
    (piece 'white (coords 3 2) '())
    (piece 'white (coords 5 2) '())
    (piece 'white (coords 7 2) '())

    (piece 'black (coords 0 7) '())
    (piece 'black (coords 2 7) '())
    (piece 'black (coords 4 7) '())
    (piece 'black (coords 6 7) '())

    (piece 'black (coords 1 6) '())
    (piece 'black (coords 3 6) '())
    (piece 'black (coords 5 6) '())
    (piece 'black (coords 7 6) '())

    (piece 'black (coords 0 5) '())
    (piece 'black (coords 2 5) '())
    (piece 'black (coords 4 5) '())
    (piece 'black (coords 6 5) '()))
   'white))

(define test-jump-board
  (board
   (list
    (piece 'white (coords 1 0) '())
    (piece 'white (coords 3 0) '())
    (piece 'white (coords 5 0) '())
    (piece 'white (coords 7 0) '())
    
    (piece 'white (coords 0 1) '())
    (piece 'white (coords 2 1) '())
    (piece 'white (coords 4 1) '())
    ;(piece 'white (coords 6 1) '())

    (piece 'white (coords 1 2) '())
    (piece 'white (coords 3 2) '())
    (piece 'white (coords 5 2) '())
    (piece 'white (coords 5 4) '())

    (piece 'black (coords 0 7) '())
    (piece 'black (coords 2 7) '())
    (piece 'black (coords 4 7) '())
    (piece 'black (coords 6 7) '())

    (piece 'black (coords 1 6) '())
    (piece 'black (coords 3 6) '())
    (piece 'black (coords 5 6) '())
    (piece 'black (coords 7 6) '())

    (piece 'black (coords 4 1) '())
    (piece 'black (coords 2 5) '())
    (piece 'black (coords 4 5) '())
    (piece 'black (coords 6 5) '()))
   'black))


(define test-king-piece-board
  (board
   (list
    (piece 'white (coords 1 0) '())
    (piece 'white (coords 3 0) '())
    (piece 'white (coords 5 0) '())
    ;(piece 'white (coords 7 0) '())
    
    (piece 'white (coords 0 1) '())
    (piece 'white (coords 2 1) '())
    (piece 'white (coords 4 1) '())
    ;(piece 'white (coords 6 1) '())

    (piece 'white (coords 1 2) '())
    (piece 'white (coords 3 2) '())
    (piece 'white (coords 5 2) '())
    ;(piece 'white (coords 5 4) '())

    (piece 'black (coords 0 7) '())
    (piece 'black (coords 2 7) '())
    (piece 'black (coords 4 7) '())
    (piece 'black (coords 6 7) '())

    (piece 'black (coords 1 6) '())
    (piece 'black (coords 3 6) '())
    (piece 'black (coords 5 6) '())
    (piece 'black (coords 7 6) '())

    (piece 'black (coords 4 1) '())
    (piece 'black (coords 2 5) '())
    (piece 'black (coords 4 5) '())
    (piece 'black (coords 6 1) '()))
   'black))


(define (initial-pmoves board)
  (map (λ (piece) (initial-pmove board piece))
        (current-pieces board)))

(define (generate-moves board game)
  (execute-rules
   (initial-pmoves board)
   (get-evolution-rules game) (get-aggregate-rules game)))

(displayln 'test-jump-board)
(displayln (board->string test-jump-board #f))

(displayln 'test-king-piece-board)
(displayln (board->string test-king-piece-board #f))

