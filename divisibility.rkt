; Take in a graph representing information about divisibility...

; V = 1 .. n (all of them)
; E = {(n,m,w) | n,m in V, w is prime (not 1),
;                m * w = n}

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
