#lang racket/base

#|
+ 5
- 7
+ 10

+ 10 * 2
|#

(define total 0)

(define (plus n)
  (printf "~a + ~a\n" total n)
  (set! total (+ total n)) ;; total + n
  (printf "~a\n" total))

(define (minus n)
  (printf "~a + ~a\n" total n)
  (set! total (- total n)) ;; total - n
  (printf "~a\n" total))

(define (final-total)
  (printf "Total: ~a\n" total))

(plus 5)
(minus 7)
(plus 10)
(plus (* 10 2))
(final-total)


