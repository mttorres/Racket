#lang racket

(require graph)
 
(define simb (list ";" "?" "*" "U"))
(define grafo (weighted-graph/directed '((A a b) (B b c))))
(define grafovaz (weighted-graph/directed '(  )))
(define grafoun (weighted-graph/directed '(  )))
(add-vertex! grafoun 'a)
(define gg (weighted-graph/directed '((A a b) (B b c) (C a d) (D d e) (E a s)     )))
(define gp (weighted-graph/directed '((A a b) (B b c) (C c d) (D c e)      )))                   
(define gs (weighted-graph/directed '((A a b) (A b c) (B b d) (A c e)  (A d f)      )))   ; A;(A U B);A
(require "leitorpdl.rkt" )


 (define (leitor  in grafoin)
   (let* ( [linha (read-line in) ] [grafo (graph-copy grafoin) ] ) 
    
   (if (eof-object? linha )
       ((lambda (l)(fprintf (current-output-port) "ACABOU (EOF)\n" ) (close-input-port in) )linha)
       ((lambda (l) (fprintf (current-output-port) "PDL: ~s \n" (string-replace l "\r" "") ) (leitorpdl (string-replace l "\r" "") 'a grafo ) (leitor in grafoin)  )linha)
   )  
     
   (close-input-port in)  
)

)   


(define in (open-input-file "entradagrafounitario.txt"))
(define in2 (open-input-file "entrada2.txt"))
(define in3 (open-input-file "entrada3.txt"))
(define in4 (open-input-file "entrada4.txt"))
(define in5 (open-input-file "entrada5.txt"))
(leitor in grafoun)
(leitor in2 grafo)
(leitor in3 gg)
(leitor in4 gp)
(leitor in5 gs)




