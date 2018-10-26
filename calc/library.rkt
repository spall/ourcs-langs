#lang racket/base

(provide plus minus final-total)

(define total 0)

(define (plus n)
  (printf "~a + ~a\n" total n)
  (set! total (+ total n)) ;; total + n
  (printf "~a\n" total))

(define (minus n)
  (printf "~a - ~a\n" total n)
  (set! total (- total n)) ;; total - n
  (printf "~a\n" total))

(define (final-total)
  (printf "Total: ~a\n" total))