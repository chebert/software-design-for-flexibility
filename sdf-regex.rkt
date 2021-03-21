#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")

;; Regular expression wrapper

#|
(define (r:dot) (λ (extended?) "."))
(define (r:bol) (λ (extended?) "^"))
(define (r:eol) (λ (extended?) "$"))

#|
(define (r:seq . exprs)
  (if (null? exprs)
      ""
      (string-append "\\(" (apply string-append exprs) "\\)")))

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (λ (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))
|#

(define chars-needing-quoting '(#\. #\[ #\\ #\^ #\$ #\*))

#|
(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq (cons (car exprs)
                         (append-map (curry-right list "\\|") (rest exprs))))
      (r:seq)))
|#

#|
(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list expr "*"))
                       ((= max min) '())
                       (else
                        (make-list (- max min)
                                   (r:alt expr "")))))))
|#

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members)
        (list char)
        '()))
  (append (optional #\])
          (filter (negate (curry-left memv chars-needing-quoting-in-brackets)) members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets '(#\] #\^ #\-))

(define (r:* expr) (r:repeat 0 #f expr))
(define (r:+ expr) (r:repeat 1 #f expr))

#|
(define (r:repeat min max expr)
  (r:seq expr "\\{" "," (if max (number->string max) "") "\\}")
|#

(define (r:quote string)
  (λ (extended?)
    (list->string (append-map (λ (char) ((r:quote-char char) extended?)) (string->list string)))))
  
(define (r:single-char? expr)
  (λ (extended?)
    (if (string? (expr extended?))
        (let ((cs (string->list (expr extended?))))
          (let ((len (length cs)))
            (or (= len 1)
                (and (= len 2) (eq? (first cs) #\\)))))
        #f)))

(define (r:ensure-group expr)
  (λ (extended?)
    (if (or (r:single-char? (expr extended?)) (r:group? (expr extended?)))
        (expr extended?)
        (cons 'group (expr extended?)))))
(define (r:group expr)
  (λ (extended?)
    (if (r:group? (expr extended?))
        (expr extended?)
        (cons 'group (expr extended?)))))
(define (r:group? expr) (and (pair? expr) (eq? (car expr) 'group)))

(define (r:concatenate . exprs)
  (λ (extended?)
    (apply string-append (map (λ (expr) (r:string (expr extended?) extended?)) exprs))))
(define (r:alternation . exprs)
  (λ (extended?)
    (r:string (apply r:alt extended? (map (λ (expr) (r:string (r:ensure-group expr) extended?)) exprs)) extended?)))

(define (r:repeat expr min max)
  (λ (extended?)
    ((r:concatenate (r:ensure-group expr)
                    (r:special-char #\{)
                    (r:quote (number->string min))
                    (r:quote
                     (cond ((and max (= max min)) "")
                           (max (string-append "," (number->string max)))
                           (else ",")))
                    (r:special-char #\}))
     extended?)))

(define (r:string expr extended?)
  (if (r:group? expr)
      (string-append (r:special-char #\() (cdr expr) (r:special-char #\) extended?))
      expr))

(define (r:alt extended? . exprs)
  (if (pair? exprs)
      ((apply r:concatenate
             (cons (car exprs)
                   (append-map (λ (expr) (list (r:special-char #\| extended?) expr)) (rest exprs))))
       extended?)
      ((r:concatenate) extended?)))

(define (r:regex expr extended? anchor-bol? anchor-eol?)
  ((r:concatenate (if anchor-bol? (r:bol) "") expr (if anchor-eol? (r:eol) "")) extended?))

(define (r:back-reference n)
  (assert (<= 1 n 9))
  (λ (extended?)
    ((r:concatenate (string-append "\\" (number->string n))) extended?)))
      

(define extended-chars-needing-quoting '(#\. #\[ #\\ #\( #\) #\* #\+ #\? #\} #\| #\^ #\$))

(define test
  (let ((extended? #f))
    (r:regex
     (r:alternation
      (r:concatenate (r:group (r:quote "The[end of the]world")) (r:quote "\\"))
      (r:concatenate (r:repeat (r:quote "dog") 2 #f) (r:repeat (r:quote "[") 3 5))
      (r:quote "a")
      (r:group (r:quote "a"))
      (r:back-reference 2))
     extended?
     #f
     #t)))
|#

;; ERE & BRE combined

; Design options:
; Each expr is a
;   structure/list (e.g. `(r:quote ,string)) which is passed to an (ere expr) or (bre expr)
;     - multiple references to each expr structure (1 in ere, 1 in bre, and 1 in each expr)
;   bundle that takes a regex type. e.g. (expr 'ere) or (expr 'bre). (expr) could return a structure/list for further extension.

(define bre-chars-needing-quoting '(#\. #\[ #\\ #\^ #\$ #\*))
(define bre-special-chars '(#\| #\( #\) #\{ #\}))
(define ere-chars-needing-quoting '(#\. #\[ #\\ #\( #\) #\* #\+ #\? #\} #\| #\^ #\$))
(define chars-needing-quoting-in-brackets '(#\] #\^ #\-))

(define (r:special-char char)
  (λ args
    (if (null? args)
        `(r:special-char ,char)
        (list->string
         (case (first args)
           ((ere) (list char))
           ((bre) (if (memv char bre-special-chars)
                      (list #\\ char)
                      (list char))))))))

(define (r:quote-char char)
  (λ args
    (if (null? args)
        `(r:quote-char ,char)
        (list->string
         (case (first args)
           ((ere) (if (memv char ere-chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
           ((bre) (if (memv char bre-chars-needing-quoting)
                      (list #\\ char)
                      (list char))))))))

(define (r:dot)
  (λ args
    (if (null? args)
        '(r:dot)
        (case (first args)
          ((bre ere) ".")))))
(define (r:bol)
  (λ args
    (if (null? args)
        '(r:bol)
        (case (first args)
          ((bre ere) "^")))))
(define (r:eol)
  (λ args
    (if (null? args)
        '(r:eol)
        (case (first args)
          ((bre ere) "$")))))
(define (r:quote string)
  (λ args
    (if (null? args)
        `(r:quote ,string)
        (case (first args)
          ((bre ere) 
           (apply string-append (map (λ (char) ((r:quote-char char) (first args))) (string->list string))))))))


(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members)
        (list char)
        '()))
  (append (optional #\])
          (filter (negate (curry-left memv chars-needing-quoting-in-brackets)) members)
          (optional #\^)
          (optional #\-)))

(define (single-char? str)
  (let ((cs (string->list str)))
    (let ((len (length cs)))
      (or (= len 1)
          (and (= len 2) (eq? (first cs) #\\))))))

(define group? (make-bundle-predicate 'group))

(define (r:group expr)
  (λ args
    (cond
      ((null? args) `(r:group ,(expr)))
      ((eq? group? (first args)) (group?))
      (else
       (case (first args)
         ((bre ere)
          (let ((str (expr (first args))))
            (if (or (group? expr)
                    (single-char? str))
                str
                ((r:seq (r:special-char #\()
                        expr
                        (r:special-char #\)))
                 (first args))))))))))

(define (r:char-from string)
  (λ args
    (if (null? args)
        `(r:char-from ,string)
        (case (first args)
          ((bre ere)
           (case (string-length string)
             ((0 1) ((r:quote string) (first args)))
             (else
              (bracket string
                       (λ (members)
                         (if (set=? '(#\- #\^) members)
                             '(#\- #\^)
                             (quote-bracketed-contents members)))))))))))

(define (r:char-not-from string)
  (λ args
    (if (null? args)
        `(r:char-not-from ,string)
        (case (first args)
          ((bre ere)
           (bracket string
                    (λ (members)
                      (cons #\^ (quote-bracketed-contents members)))))))))

(define (r:seq . exprs)
  (λ args
    (if (null? args)
        `(r:seq ,@(map (λ (expr) (expr)) exprs))
        (case (first args)
          ((bre ere)
           (apply string-append
                  (map (λ (expr) (expr (first args))) exprs)))))))

(define (r:alt . exprs)
  (r:group
   (λ args
     (if (null? args)
         `(r:alt ,@(map (λ (expr) (expr)) exprs))
         (case (first args)
           ((bre ere)
            (if (null? exprs)
                ""
                (apply string-append
                       ((r:group (car exprs)) (first args))
                       (map (λ (expr)
                              (string-append ((r:special-char #\|) (first args))
                                             ((r:group expr) (first args))))
                            (rest exprs))))))))))

(define (r:repeat min max expr)
  (λ args
    (if (null? args)
        `(r:repeat ,min ,max ,(expr))
        (case (first args)
          ((bre ere)
           ((r:seq expr
                   (r:special-char #\{)
                   (r:quote (number->string min))
                   (r:quote
                    (cond ((and max (= max min)) "")
                          (max (string-append "," (number->string max)))
                          (else ",")))
                   (r:special-char #\}))
            (first args)))))))

(define (r:back-reference n)
  (assert (<= 1 n 9))
  (λ args
    (if (null? args)
        `(r:back-reference ,n)
        (case (first args)
          ((bre) (string-append "\\" (number->string n)))
          ((ere) (error "Extended Regex does not support back-references"))))))

(define test-expr
  (r:alt
   (r:seq (r:quote "The[end of the]world")) (r:quote "\\")
   (r:seq (r:repeat 2 #f (r:quote "dog")) (r:repeat 3 5 (r:quote "[")))
   (r:quote "a")
   (r:quote "a")
   (r:back-reference 1)))

(define (regex-string expr regex-type) (expr regex-type))

(define (write-bourne-shell-grep-command expr regex-type filename)
  (display (bourne-shell-grep-command-string expr regex-type filename)))
(define (bourne-shell-grep-command-string expr regex-type filename)
  (string-append "grep "
                 (if (eq? regex-type 'ere) "-Ee " "-e ")
                 (bourne-shell-quote-string (regex-string expr regex-type))
                 " "
                 filename))

(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))

(provide r:dot
         r:bol
         r:eol
         r:quote
         r:group
         r:char-from
         r:char-not-from
         r:seq
         r:alt
         r:repeat
         r:back-reference
         regex-string
         bourne-shell-grep-command-string)
