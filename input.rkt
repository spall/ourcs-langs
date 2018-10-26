#lang racket

;; Input language for specifying flight data and flight queries...
;; - What will it look like?
;; - Want to catch input errors / invalid inputs
;;   - Parser should print out error message
;;   - To ask questions, have different input fields?

(define graph
  '(;(from to miles price)
    (amsterdam newyork 3641 311)
    (london newyork 3449 328)
    (istanbul newyork 5013 497)
    (syndey amsterdam 10348 778)
    (london sydney 10573 838)))

;; '(amsterdam newyork 3641 311) -> 'amsterdam
(define (from flight)
  (car flight))
;; '(amsterdam newyork 3641 311) -> 'newyork
(define (to flight)
  (cadr flight))
;; '(amsterdam newyork 3641 311) -> 3641
(define (miles flight)
  (caddr flight))
;; '(amsterdam newyork 3641 311) -> 311
(define (price flight)
  (cadddr flight))

;; '(amsterdam . newyork)
(define other-graph
  (map (lambda (flight) (cons (from flight) (to flight))) graph))