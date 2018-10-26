#lang racket

#| atom      := number
   op        := + | - | / | *

   exp := atom | exp op exp
|#

;; n op n
;; n op n op n

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(provide
 #%datum #%app provide
 (rename-out [readsyntax read-syntax]
             [customread read]
             [custom-module #%module-begin]))

(define-tokens data ( OP NUM))

(define-empty-tokens delim (EOF PL MN PLUS MINUS))

(define-lex-abbrevs
  [space (:or #\tab #\space)]
  [digit (:/ #\0 #\9)])

(define infix-lexer
  (lexer
   [#\newline null]
   [(eof) null]
   [space (infix-lexer input-port)]
   [#\+ (cons 'PL (infix-lexer input-port))]
   [#\- (cons 'MN (infix-lexer input-port))]
   [(:+ digit) (cons (token-NUM (string->number lexeme))
                     (infix-lexer input-port))]))

(define calc-lexer
  (lexer
   [#\newline (calc-lexer input-port)]
   [(eof) 'EOF]
   [space (calc-lexer input-port)]
   [#\+ 'PLUS #;(cons 'PLUS (infix-lexer input-port))]
   [#\- 'MINUS #;(cons 'MINUS (infix-lexer input-port))]
   [(:+ digit) (cons (token-NUM (string->number lexeme))
                     (infix-lexer input-port))]))


(define calc-parser
  (parser
   (start s)
   (end EOF)
   (error (lambda (a b c)
          (error 'calc-parser "Error parsing")))

   (tokens data delim)

   (grammar

    (s [(ln-list) (reverse $1)])

    (ln [(PLUS infix) `(plus ,$2) #;(datum->syntax #f `(plus ,$1))]
        [(MINUS infix) `(minus ,$2) #;(datum->syntax #f `(minus ,$1))])

    (infix [(NUM) $1]
           [(infix PL infix) `(+ ,$1 ,$3)]
           [(infix MN infix) `(- ,$1 ,$3)])

   #|(higher [(infix) $1]
            [(    TM ) `(
            [(    DV ) ]) |#
              
    (ln-list [() null]
             [(ln-list ln) (cons $2 $1)]))))

(define (customread ip)
  (syntax->datum
   (readsyntax #f ip)))

(define (readsyntax _ ip)
  (define calc-datums (calc-parser (lambda () (calc-lexer ip))))
  (define module-datum `(module calc "calc-reader.rkt"
                          ;(provide plus minus)
                          ,@calc-datums))
  (datum->syntax #f module-datum))


;; expander stuff

(define total 0)

(define (plus-helper n)
  (printf "~a + ~a\n" total n)
  (set! total (+ total n)) ;; total + n
  (printf "~a\n" total))

(define (minus-helper n)
  (printf "~a - ~a\n" total n)
  (set! total (- total n)) ;; total - n
  (printf "~a\n" total))

(define (final-total)
  (printf "Total: ~a\n" total))

(define-syntax-rule (custom-module expr ...)
  (#%module-begin expr ...))

(define (plus formula)
  (plus-helper formula))

(define (minus formula)
  (minus-helper formula))

(provide total plus minus)