#lang racket

(require racket/match)
(provide -> match-graph subgraph)

(define-match-expander ->
  (lambda (stx)
    (syntax-case stx ()
      [(_ weight from to) #'(list weight from to)]
      [(_ from to) #'(list _ from to)])))

(define-syntax-rule (match-graph g [lhs ... rhs] ...)
  (append*
   (map
    (lambda (edge)
      (match edge
        [lhs ... (list rhs)] ...
        [_ '()]))
    (cadddr g))))

(define (subgraph g f)
  (match g
    [(list type name nodes (list edges ...))
     `(,type ,name ,nodes ,(filter (lambda (ls) (apply f ls)) edges))]))
