#lang racket


(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#|  `(,weight ,n1 ,n2)  ;; edge

    `(,graph-type ,name ,listofnodes ,listofedges)
|#
(define-tokens data (WEDGE EDGE NODE GTYPE NAME))
(define-empty-tokens delim (OB CB EOF))

(define-lex-abbrevs
  [spaces (:* (:or #\space #\tab))]
  [node (:+ alphabetic)]
  [weight (:+ numeric)]
  [name (:+ (:or alphabetic numeric))])

(define edge-lexer
  (lexer
   [spaces (edge-lexer input-port)]
   [(:: node spaces "-->" spaces node) (let ([ls (string->list lexeme)])
                                         `(,(first ls) ;; node1
                                           ,(third ls)))])) ;; node2

(define wedge-lexer
  (lexer
   [(:: weight spaces #\;) (token-WEDGE `(,(string->number (string-trim lexeme))
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
 ;  [node (token-NODE lexeme)]
   [#\[ (wedge-lexer input-port)]))
   
#|
graph := graph-type letters+numbers { nodes : list-of-nodes
                                      edges : list-of-edges
                                    }
|#

(define graph-parser
  (parser

   (start graph)
   (end EOF)
   (error (lambda (a b c)
            (error 'graph-parser "Error on token ~a with value ~a" b c)))
   (tokens data delim)

   (grammar
   
   (graph [(GTYPE NAME nodes edges)
           `(,$1 ,$2 ,(reverse $3) ,(reverse $4))]
          [(GTYPE NAME nodes wedges)
           `(,$1 ,$2 ,(reverse $3) ,(reverse $4))])

   (nodes [(NAME) (list $1)]
          [(nodes NAME) (cons $2 $1)])

   (edges [() null]
          [(edges EDGE) (cons $2 $1)])

   (wedges [() null]
           [(wedges WEDGE) (cons $2 $1)]))))


(define ip (open-input-string "digraph graph1 { nodes : {a, b, c}
                                                edges : { [ ; a --> b], [ ; b --> c] }
                                              }"))
(graph-parser (lambda () (graph-lexer ip)))
   
   
   

   
   
   
   