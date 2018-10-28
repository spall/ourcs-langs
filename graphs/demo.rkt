#lang racket

(require "graphviz.rkt")
(require "comprehension.rkt")

(require "div-graph" "flightpaths.rkt")

; Display entire graph
(graphviz divisibility50)

; Display subgraph of nodes <= 20
(graphviz
  (subgraph divisibility50 (lambda (w f t) (<= (string->number f) 20))))

(define (composite? n)
  (null?
    (match-graph divisibility50
      [(-> from "1") #:when (equal? n (string->number from)) #t])))

(define (composite?-no-dsl n)
  (null?
   (filter
    (lambda (edge)
      (and (equal? (second edge) n) (equal? (third edge) "1")))
    (cadddr divisibility50))))

(composite?-no-dsl "10")

(define (prime-factors n)
  (match-graph divisibility50
    [(-> prime from _) #:when (equal? from n) prime]))

(prime-factors "20") ; '(2 5)

(define (factors n)
  (let
    ([immediate-factors
      (match-graph divisibility50
        [(-> number factor) #:when (equal? number n) factor])])
   (remove-duplicates
     (flatten
      (append immediate-factors (map factors immediate-factors))))))

(factors "20") ; '("10" "4" "5" "2" "1")

;; divisible?: String String -> Boolean
;; the strings are numbers between 1 and 50
(define (divisible? n m)
  (if (member m (factors n)) #t #f))

;; flight-path: Symbol Symbol
(define (flight-path init dest)
  (define edges (match-graph flights
                             [(-> w from to) #:when (and (equal? from init)
                                                         (equal? to dest)) #t]
                             [(-> w from to) #:when (and (equal? from init)) (flight-path to dest)]))
  (ormap identity (flatten edges)))
