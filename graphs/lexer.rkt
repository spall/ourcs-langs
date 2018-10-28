#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (for-syntax syntax/parse)
         (prefix-in : parser-tools/lex-sre))

(provide #%app #%datum begin all-defined-out graph graphs edge define quote 
         (rename-out [read-syntax_ read-syntax]
                     [read_ read]
                     [custom-module #%module-begin]))

(define (read_ ip)
  (syntax->datum
   (read-syntax_ #f ip)))

(define (read-syntax_ path ip)
  (define parsed-graph (graph-parser (lambda () (graph-lexer ip))))
  (define module-datum `(module graph "lexer.rkt"
                          ,parsed-graph))
  (datum->syntax #f module-datum))

(define-syntax-rule (custom-module graph ...)
  (#%module-begin
                  graph ...))

(define-syntax (graphs stx)
  (syntax-case stx ()
    [(_ graph ...)
     #'(begin graph ...)]))

(define-syntax (graph stx)
  (syntax-case stx ()
    [(_ type name (node ...) (edge ...))
     (syntax-case (datum->syntax #'help (string->symbol (syntax->datum #'name))) ()
       [var
        #'(begin (define var (list type name (list node ...) (list edge ...)))
                 (provide var))
        ])]))

(define-syntax (edge stx)
  (syntax-case stx ()
    [(_ w n1 n2)
     #'(list w n1 n2)]))
  

#|  `(,weight ,n1 ,n2)  ;; edge

    `(,graph-type ,name ,listofnodes ,listofedges)
|#
(define-tokens data (WEDGE EDGE GTYPE NAME))
(define-empty-tokens delim (OB CB EOF))

(define-lex-abbrevs
  [spaces (:* (:or #\space #\tab))]
  [weight (:+ numeric)]
  [name (:+ (:or alphabetic numeric))])

(define edge-lexer
  (lexer
   [(:: spaces name spaces "->" spaces name spaces #\])
    (let ([ls (string-split (string-trim lexeme "]" #:left? #f))])
      `(,(first ls) ;; node1
        ,(third ls)))])) ;; node2

(define wedge-lexer
  (lexer
   [(:: spaces weight spaces #\;) (token-WEDGE `(,(let ([rs (string->number (string-trim (string-trim lexeme ";" #:left? #f)))])
                                                    rs)
                                    ,@(edge-lexer input-port)))]  ;; weight 
   [(:: spaces #\;) (token-EDGE `(#f
                            ,@(edge-lexer input-port)))]))

#|

graph-types := bigraph | digraph | graph

node := sequence of letters

edge := [  ; node -> node ]
wedge := [ weight ; node -> node]

list-of-nodes := { node, node, node ... }

list-of-edges := { edge , ..1 }  ;; delimited by commas 

graph := graph-type letters+numbers { nodes : list-of-nodes
                                      edges : list-of-edges
                                    }
|#

(define graph-lexer
  (lexer
   [(eof) 'EOF]
   [#\newline (graph-lexer input-port)]
   [spaces (graph-lexer input-port)]
   [#\, (graph-lexer input-port)]
   [(:or "bigraph" "digraph" "graph") (token-GTYPE lexeme)]
   [name (token-NAME lexeme)]
   [#\{ (graph-lexer input-port)] 
   [#\} (graph-lexer input-port)]
   [(:: "nodes" spaces #\:) (graph-lexer input-port)]
   [(:: "edges" spaces #\:) (graph-lexer input-port)]
   [#\[ (wedge-lexer input-port)]))
   
#|
graph := graph-type letters+numbers { nodes : list-of-nodes
                                      edges : list-of-edges
                                    }
|#

(define graph-parser
  (parser

   (start g)
   (end EOF)
   (error (lambda (a b c)
            (error 'graph-parser "Error on token ~a with value ~a" b c)))
   (tokens data delim)

   (grammar

    (g [(graphs) `(graphs ,@(reverse $1))])
    
    (graphs [() null]
            [(graphs graph) (cons $2 $1)])
   
    (graph [(GTYPE NAME nodes edges)
            `(graph ,$1 ,$2 ,(reverse $3) ,(reverse $4))]
           [(GTYPE NAME nodes wedges)
            `(graph ,$1 ,$2 ,(reverse $3) ,(reverse $4))])
    
    (nodes [(NAME) (list $1)]
           [(nodes NAME) (cons $2 $1)])
    
    (edges [() null]
           [(edges EDGE) (cons `(edge ,@$2) $1)])
    
    (wedges [() null]
            [(wedges WEDGE) (cons `(edge ,@$2) $1)]))))

#|
(define ip (open-input-string "digraph graph1 { nodes : {a, b, c}
                                                edges : { [ ; a -> b], [ ; b -> c] }
                                              }"))
(graph-parser (lambda () (graph-lexer ip)))

(define ip2 (open-input-string "digraph graph1 { nodes : {a, b, c}
                                                edges : { [5 ; a -> b], [7 ; b -> c] }
                                              }"))

(define ip3 (open-input-string "digraph divisibility50 { nodes : {1, 2, 3}
                                                         edges : {[ 5 ; 7 -> 1 ], [ 3 ; 3 -> 1 ]}}"))

(graph-parser (lambda () (graph-lexer ip3)))

(define fp (open-input-file "../div-graph" #:mode 'text))
(graph-parser (lambda () (graph-lexer fp)))
|#

(define ip (open-input-string "digraph example1 { nodes : {a ,b, c}
                   edges: { [ ; a -> b ] , [ ; b -> c ] }}"))

;(graph-parser (lambda () (graph-lexer ip)))
   
   
   
   