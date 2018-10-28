#lang racket

(require 2htdp/image)
(provide graphviz graphviz-to-file graphviz-string)

; bitmap/file

(define (graphviz g)
  (let ([tmp (make-temporary-file)])
    (graphviz-to-file g (path->string tmp))
    (bitmap/file tmp)))

(define (graphviz-to-file g output)
  (with-input-from-string
    (graphviz-string g)
    (lambda () (system (string-append "dot -Tpng > " output)))))

;; graphviz-string: Graph -> DotFileFormat
(define (graphviz-string g)
  (define graph-init (format "digraph ~a {" (cadr g)))
  (define graph-inner (extract-edges (cadddr g)))
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
  (if (first p)
    (format "~s -> ~s [label=\"~s\"]; " (second p) (third p) (first p))
    (format "~s -> ~s; " (second p) (third p))))