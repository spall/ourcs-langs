#lang reader "lexer.rkt"

digraph flights {
                 nodes: {istanbul, sydney, newyork, moscow, hongkong}
                 edges: { [1000 ; istanbul -> newyork] , [5000 ; newyork -> sydney] , [3000 ; honkkong -> newyork] , [1400 ; sydney -> hongkong] , [1500 ; moscow -> istanbul]
                          }
                 }