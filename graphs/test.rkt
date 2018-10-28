#lang racket

(require "div-graph")

(require "graphviz.rkt")
(require "comprehension.rkt")

(graphviz divisibility50)

(graphviz
  (subgraph divisibility50 (lambda (w f t) (< (string->number f) 5))))
 
(match-graph divisibility50
  [(-> w "20" to) (string->number to)]
  [(-> w "10" to) (list w (string->number to))])

(match-graph divisibility50
  [(-> w from to) #:when (equal? from "20") (string->number to)])
