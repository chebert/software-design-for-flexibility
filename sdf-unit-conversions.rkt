#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")

;; Wrappers

(define (make-unit-conversion x->y y->x)
  (λ (x)
    (if (eq? x 'invert)
        (make-unit-conversion y->x x->y)
        (x->y x))))

(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ 32 (* 9/5 c)))))
(define celsius-to-kelvin
  (let ((zero-celsius 273.15)) ; kelvin
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))

;((compose celsius-to-kelvin fahrenheit-to-celsius) 80)

(define (unit-specializer procedure implicit-output-unit . implicit-input-units)
  (define (specializer specific-output-unit . specific-input-units)
    (let ((output-converter (make-conversion implicit-output-unit specific-output-unit))
          (input-converters (map make-conversion specific-input-units implicit-input-units)))
      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (λ (converter argument) (converter argument))
                     input-converters
                     arguments))))
      specialized-procedure))
  specializer)

(define (compound-unit? u)
  (and (pair? u) (memv (first u) '(/ * expt))))

(define (derive-conversion start end)
  (if (eq? (car start) (car end))
      (case (car start)
        ((/) (apply unit:/ (make-conversion (second start) (second end))
                    (map make-conversion (list-tail start 2) (list-tail end 2))))
        ((*) (apply unit:* (make-conversion (second start) (second end))
                    (map make-conversion (list-tail start 2) (list-tail end 2))))
        ((expt)
         (assert (= (third start) (third end)))
         (unit:expt (make-conversion (second start) (second end)) (third start)))
        (else "Unknown compound conversion operation " (car start)))
      (error "Cannot derive conversion for " start 'to end)))

; Issue: Path could go from compound-unit<->single-unit at any point in the path.

(define (make-conversion start end)
  (if (equal? start end)
      unit-to-unit
      (let ((path (unit-path end start)))
        (cond (path (apply make-unit-conversion-from-path path))
              ((and (compound-unit? end) (compound-unit? start))
               (derive-conversion start end))
              (else
               (error "No known conversion for path from " start 'to end))))))


(define (all-unit-conversions-from name)
  (map cdr (filter (λ (key) (equal? (car key) name)) (hash-keys unit-conversion-table))))

(define (unit-path start end)
  (let loop ((paths (list (list start)))
             (visited (set)))
    (if (null? paths)
        #f
        (let loop-paths ((paths paths)
                         (visited visited)
                         (new-paths '()))
          (if (null? paths)
              (loop new-paths visited)
              (let* ((path (first paths))
                     (v (first path))
                     (new-neighbors (filter (λ (u) (not (set-member? visited u)))
                                            (all-unit-conversions-from v))))
                (if (member end new-neighbors)
                    (cons end path)
                    (loop-paths (rest paths)
                                (set-union (list->set new-neighbors) visited)
                                (append (map (λ (neighbor) (cons neighbor path))
                                             new-neighbors)
                                        new-paths)))))))))

(define (unit:*bin u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))

(define unit-to-unit 
  (make-unit-conversion (lambda (v) v) (lambda (v) v)))

(define (unit:* . us)
  (foldl (swap-args unit:*bin) unit-to-unit us))

(define unit-conversion-table (make-hash))

(define (get-unit-conversion unit1 unit2)
  (if (eq? unit1 unit2)
      unit-to-unit
      (hash-ref unit-conversion-table (cons unit1 unit2))))

(define (register-unit-conversion unit1 unit2 conversion)
  (hash-set! unit-conversion-table (cons unit1 unit2) conversion)
  (hash-set! unit-conversion-table (cons unit2 unit1) (unit:invert conversion))
  #t)

(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))

(define (unit:expt u n)
  (if (> n 1)
      (unit:* u (unit:expt u (- n 1)))
      u))
(define (unit:invert u)
  (u 'invert))

(define (make-scalar-unit-conversion scale)
  (make-unit-conversion (lambda (x) (* x scale))
                        (lambda (x) (/ x scale))))

(define (make-unit-conversion-from-path . unit-names)
  (let loop ((unit-names unit-names)
             (unit unit-to-unit))
    (if (and (pair? unit-names) (not (null? (rest unit-names))))
        (loop (rest unit-names)
              (unit:* unit (get-unit-conversion (first unit-names) (second unit-names))))
        unit)))

(register-unit-conversion 'fahrenheit 'celsius fahrenheit-to-celsius)
(register-unit-conversion 'celsius 'kelvin celsius-to-kelvin)

(register-unit-conversion 'mile 'foot (make-scalar-unit-conversion 5280))
(register-unit-conversion 'foot 'inch (make-scalar-unit-conversion 12))
(register-unit-conversion 'yard 'foot (make-scalar-unit-conversion 3))
(register-unit-conversion 'meter 'foot (make-scalar-unit-conversion 3.28084))
(register-unit-conversion 'meter 'centimeter (make-scalar-unit-conversion 100))
(register-unit-conversion 'meter 'millimeter (make-scalar-unit-conversion 1000))

(register-unit-conversion 'cup 'fluid-ounce (make-scalar-unit-conversion 8))
(register-unit-conversion 'jigger 'fluid-ounce (make-scalar-unit-conversion 3/2))
(register-unit-conversion 'tablespoon 'teaspoon (make-scalar-unit-conversion 3))
(register-unit-conversion 'cup 'tablespoon (make-scalar-unit-conversion 16))
(register-unit-conversion 'gallon 'cup (make-scalar-unit-conversion 16))
(register-unit-conversion 'gallon '(expt inch 3) (make-scalar-unit-conversion 231))

(register-unit-conversion 'fahrenheit 'kelvin (unit:* fahrenheit-to-celsius celsius-to-kelvin))
(register-unit-conversion 'pound 'newton (make-scalar-unit-conversion 4.44822))

(register-unit-conversion 'minute 'second (make-scalar-unit-conversion 60))
(register-unit-conversion 'hour 'minute (make-scalar-unit-conversion 60))


(define (register-expt-conversion n u1 u2)
  (let ((c (make-conversion u1 u2)))
    (register-unit-conversion (list 'expt u1 n) (list 'expt u2 n)
                              (unit:expt c n))))

(register-expt-conversion 2 'inch 'meter)
(register-expt-conversion 3 'inch 'meter)

(register-unit-conversion '(/ meter second) '(/ mile hour) (unit:/ (make-conversion 'meter 'mile) (make-conversion 'second 'hour)))
(register-unit-conversion '(/ meter (expt second 2)) '(/ mile (* hour second))
                          (make-conversion '(/ meter second) '(/ mile hour)))


(provide make-unit-conversion
         unit-specializer
         make-conversion
         unit-to-unit
         unit:*
         unit:/
         unit:expt
         unit:invert
         get-unit-conversion
         register-unit-conversion
         make-scalar-unit-conversion
         make-unit-conversion-from-path)