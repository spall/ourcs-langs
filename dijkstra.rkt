#lang racket

;; Incomplete work from Friday

(define graph ;; (from . to)
  '((istanbul . newyork)
    (amsterdam . newyork)
    (sydney . amsterdam)
    (amsterdam . sydney)
    (london . sydney)
    (london . amsterdam)))

;; node -> node2
;; node2 -> node3
;; node4 -> node3
;; node2 -> node4

;; what are paths from node to node3

;; node -> node2 -> node3
;; node -> node2 -> node4 -> node3

;; node -> node2 -> node3

;; 
(define (path? g from to)
  (if (eq? from to)
      #t
      ; for each possible flight from -> from',
      ; check whether there's a path from' -> to
      (let* ([possible-flights (filter (lambda (flight) (eq? (car flight) from)) g)]
             [did-it-work (ormap (lambda (flight) (path? g (cdr flight) to)) possible-flights)])
        did-it-work)))

(define graph-w-dist ;; (from . to)
  '((5 istanbul newyork)
    (4 amsterdam  newyork)
    (10 sydney amsterdam)
    (10 amsterdam sydney)
    (12 london sydney)
    (3 london amsterdam)))


(define destination third)
(define dist first)
(define initial second)

;; node -> node2
;; node2 -> node3
;; node4 -> node3
;; node2 -> node4

;; what are paths from node to node3

;; node -> node2 -> node3
;; node -> node2 -> node4 -> node3

(define (all-from g from)
  (filter (lambda (flight) (eq? (initial flight) from)) g))


;; path is a (listof cities)
;; graph, city, city  ->  (listof paths)
(define (paths g from to)
  (cond
    [(eq? from to)
      '()]
    [else
     (define possible-flights (all-from g from))   ;; list of flights that leave from
     (printf "pf ~a\n" possible-flights)
     ;; (istnabul -> newyork)  (istanbul -> sydney) 
     
     ;; any of the (cdr possible-flights) has a path -> to
     ;; run distance on each __ in possible-flights with same to
     (define possible-paths (map (lambda (flight)
                                   (let loop ([paths_ (paths g (destination flight) to)])
                                     (cond
                                       [(empty? paths_)
                                        flight]
                                       [else

                                        ;; something is a list of (lists of paths)
     ;; ( all flights from new york that go to newyork)  and (all flights from sydney that go to new york)
     (printf "pp ~a\n" possible-paths)]))

(paths graph-w-dist 'istanbul 'newyork)

     
     
     
     
      

