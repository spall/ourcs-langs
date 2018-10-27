#lang racket

(provide graphviz graphviz-string)

(define (graphviz g output)
  (with-input-from-string
    (graphviz-string g)
    (lambda () (system (string-append "dot -Tpng > " output)))))

;; graphviz-string: Graph -> DotFileFormat
(define (graphviz-string g)
  (define graph-init "graph graphname {")
  (define graph-inner (extract-edges g))
  (define graph-after "}")
  (string-append graph-init graph-inner graph-after))

;; extract-edges: Graph -> String
;; ((w a b) (w c d)) -> "a -- b; c -- d"
(define (extract-edges g)
  (cond
    [(empty? g) ""]
    [else (string-append (generate-string (car g)) (extract-edges (cdr g)))]))

;; generate-string: Pair -> String
;; (w a b) == "a -> b;"
(define (generate-string p)
  (string-append (second p) " -> " (third p) "; "))

