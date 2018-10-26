#lang racket/base

#|
+ 5
- 7
+ 10

+ 10 * 2
|#

(define (print-total)
  (printf "~a\n" total))

(define total 0)                 ;; total = 0
(set! total (+ total 5))         ;; total = total + 5
(print-total)
(set! total (- total 7))         ;; total = total - 7
(print-total)
(set! total (+ total 10))        ;; total = total + 10
(print-total)
(set! total (+ total (* 10 2)))  ;; total = total + 2 * 10

;; no more additions or subtractions so say Total is: 
(printf "Total: ~a\n" total)




