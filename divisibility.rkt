#lang racket

(require math/number-theory)

; Take in a graph representing information about divisibility...

; V = 1 .. n (all of them)
; E = {(n,m,w) | n,m in V, w is prime (not 1),
;                m * w = n}

(define (write-divisibility-graph n)
  (define (list->csv f ls)
     (apply string-append (add-between (map f ls) ", ")))
  (let*
      ([o (open-output-file "div-graph" #:exists 'replace)]
       [numbers (range 1 n)]
       [edges (append-map
               (lambda (n) (map (lambda (p) (list n (/ n p) p)) (prime-divisors n)))
               numbers)])
    (fprintf o "digraph divisibility~s { nodes : {" n)
    (fprintf o (list->csv number->string numbers))
    (fprintf o "}\n edges : {")
    (fprintf o (list->csv
               (lambda (l) (format "[ ~s ; ~s -> ~s ]" (third l) (first l) (second l)))
               edges))
    (fprintf o "}}\n")
    (close-output-port o)))

(write-divisibility-graph 50)

; property: w < n

; check if n is divisible by m:
;   is there a path from n to m? (doesn't matter about shortest)

; shortest path: smallest number of jumps to get to your destination

; prime factorization of n:
;   given the path from n to 1, return all the weights along that path,
;   separately

; is there a number you can multiply n by to get m?
;   is there a path from m to n?

; prime factors of n?
;   weighs of the edges out of n