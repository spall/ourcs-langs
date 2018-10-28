#lang racket

(require "div-graph")

(require "graphviz.rkt")
(require "comprehension.rkt" "flightpaths.rkt")

(graphviz divisibility50)

(graphviz
  (subgraph divisibility50 (lambda (w f t) (< (string->number f) 5))))
 
(match-graph divisibility50
  [(-> w "20" to) (string->number to)]
  [(-> w "10" to) (list w (string->number to))])

(match-graph divisibility50
  [(-> w from to) #:when (equal? from "20") (string->number to)])


;; divisible?: String String -> Boolean
;; the strings are numbers between 1 and 50
(define (divisible? n m)
  (define edges (match-graph divisibility50
                             [(-> w from to) #:when (and (equal? from n)
                                                         (equal? to m)) #t]))
  (not (null? edges)))


;; flight-path: Symbol Symbol
(define (flight-path init dest)
  (define edges (match-graph flights
                             [(-> w from to) #:when (and (equal? from init)
                                                         (equal? to dest)) #t]
                             [(-> w from to) #:when (and (equal? from init)) (flight-path to dest)]))
  (ormap identity (flatten edges)))




