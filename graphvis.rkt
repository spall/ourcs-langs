#lang racket


(define simple-graph
  '((istanbul . newyork)
    (amsterdam . newyork)
    (sydney . amsterdam)
    (london . sydney)))

;; graphviz: Graph -> DotFileFormat
(define (graphviz g)
  (define graph-init "graph graphname {")
  (define graph-inner (extract-edges g))
  (define graph-after "}")
  (string-append graph-init graph-inner graph-after))

;; extract-edges: Graph -> String
;; ((a . b) (c . d)) -> "a -- b; c -- d"
(define (extract-edges g)
  (cond
    [(empty? g) ""]
    [else (string-append (generate-string (car g)) (extract-edges (cdr g)))]))

;; generate-string: Pair -> String
;; (a . b) == "a -- b;"
(define (generate-string p)
  (define first (car p))
  (define second (cdr p))
  (define first-string (symbol->string first))
  (define second-string (symbol->string second))
  (string-append first-string " -- " second-string "; "))
 

(string-append "hi " "world")