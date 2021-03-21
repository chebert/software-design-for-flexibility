#lang racket

(require "sdf-utils.rkt")
(require "sdf-function-combinators.rkt")
(require "sdf-regex.rkt")
(require "sdf-unit-conversions.rkt")
(require "sdf-monolithic-checkers.rkt")

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define gas-constant 8.3144621) ; J/(K*mol)
         
(define make-specialized-gas-law-volume
  (unit-specializer
   gas-law-volume
   '(expt meter 3) ;output
   '(/ newton (expt meter 2)) ; pressure
   'kelvin ; temperature
   'mole)) ; amount

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume
   '(expt inch 3) ;output
   '(/ pound (expt inch 2)) ;pressure
   'fahrenheit ;temperature
   'mole)) ;amount

(sphere-radius (conventional-gas-law-volume 14.7 68 1)) ; 7.050 inches

(define jigger-gas-law-volume
  (make-specialized-gas-law-volume
   'jigger ;output
   '(/ pound (expt inch 2)) ;pressure
   'fahrenheit ;temperature
   'mole)) ;amount

(jigger-gas-law-volume 14.7 68 1) ; 542.118 jiggers

