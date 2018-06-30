#lang racket

(require graph)


(define (leitorpdl frame vert grafo)
 (when (> (string-length frame) 0)
   (let*( [inic (if (equal? (string-length frame) 0 ) frame      
                      (substring frame 0 1)
                      )   ]
           [prox  (cond [ ( < (string-length frame ) 2 ) ""  ]                 
                        [else (substring frame 1 2) ] ) ]
                      
            [viz (get-neighbors grafo vert)]
            [lisaux (if (string-contains? frame "U") (string-split (string-replace frame "U" "/" #:all? #f) "/" )  '() ) ] ) 
  
  ;   (fprintf (current-output-port) "inic:~s\n" inic)
  ;   (fprintf (current-output-port) "prox:~s\n" prox)
  ;   (fprintf (current-output-port) "vert:~s\n" vert)
  ;   (fprintf (current-output-port) "viz:~s\n" viz)
  ;   (fprintf (current-output-port) "frame:~s\n" frame)
    ; (print (edge-weight grafo vert (car viz)))
    ; Coisas que ainda faltam:
    ;                                                                                                                          ;tirar?
     (unless (or (string=? inic "(") (string=? inic ")" ) (string=? inic ";") (string=? inic "?") (string=? inic "*") (and (not(string-contains? frame "(") ) (string-contains? frame "U")  )) ;Se for um programa ele vai executar
      (if (and (and ( >(length viz) 0) (equal? (edge-weight grafo vert (car viz)) (string->symbol inic))) (or (string=? prox "" ) (string=? prox ";") (string=? prox "*") (string=? prox ")") ) )                      ;Possui vizinho com a aresta?
          (if ( string=? prox "*")                                                               ;O proximo é um *?
              (let* ( [proxviz (car viz) ] ) (remove-directed-edge! grafo vert proxviz)  (leitorpdl frame  proxviz grafo)) ;Se for ele só consumirá a aresta
              (if ( string=? prox "?") ; o proximo é um teste?
                  (let* ( [proxviz (car viz) ] ) (remove-directed-edge! grafo vert proxviz)  (leitorpdl (substring frame 2 (string-length frame)) proxviz grafo)) ;Se for um A? ele consome os dois
                  (let* ( [proxviz (car viz) ] ) (remove-directed-edge! grafo vert proxviz)  (leitorpdl (substring frame 1 (string-length frame)) proxviz grafo)) ;se não ele consome só o A
              )
          )
              ;(possivelmente falhou no teste dos vizihos ou aresta igual a frame)                       ; obs talvez tenha que por nesse not o "" e ";"                                          
          (if (or (and(string=? prox ";")(string=? prox "" ) (string=? prox ")" ) (and (=(length viz) 0 ) ) ) (string=? prox ";")(string=? prox "" )  )                                            ;Proxima execução é ; ou não existe mais frame?
              (fprintf (current-output-port) "O programa pdl nao existe no grafo, a aresta:~s nao foi encontrada \n" inic)     ;falha definitiva                                   ;Se sim, Existe uma operação onde é A;B e nao existe A, grafo ta incorreto.
              ;grafo ainda por ser correto ? ou *  ou ainda ter outros vizinhos que são corretos
              (if (or (string=? prox "*")(string=? prox "?" ))                                      ;Se não for ; então o grafo ainda pode ta correto se for um ? ou *
                  (if  (string=? prox "*")                                                           
                    (leitorpdl (substring frame 2 (string-length frame)) vert grafo)                 ; Se for um *(0 ou mais vezes)como ele já consumiu o A(antes) ele consome  o frame e não consumirá o grafo
                    (leitorpdl (substring frame 2 (string-length frame)) vert grafo)                 ; Caso onde o frame é A? ou seja para ser valido o vertice atual nao necessita ter arestas(mas pode ter se tiver um programa depois)
                                                                                               ; consome A? e nao segue para outro vertice
                                                                                 
                        
                        
                    ) ;falhou no seu 1o vizinho nao ser o programa ou sintaxe invalida
                    (if (not(or (string=? prox "" ) (string=? prox ";") (string=? prox "*") (string=? prox ")") ) )                                                                                                       
                         (fprintf (current-output-port) "Erro de sintaxe \n")
                         (map (lambda (vertice)
                                (if (not (equal?(edge-weight grafo vert vertice) (string->symbol inic)  )  )
                                    (fprintf (current-output-port) "A aresta:~s ainda nao foi encontrada \n" inic)
                                    (let* ( [proxviz vertice ] ) (remove-directed-edge! grafo vert proxviz)  (fprintf (current-output-port) "A aresta:~s foi encontrada! \n" inic) (leitorpdl (substring frame 1) proxviz grafo))
                                     )
                                  ) viz )  
                  )
              )
          )
        )
      )
     (if (and(or  (not(empty? lisaux )) ) (or  (not (string-contains? frame "(") ) (string=? inic "("  ) ) )
         (when (and(or  (not(empty? lisaux )) ) (or  (not (string-contains? frame "(") ) (string=? inic "("  ) ) )  
      ;  (fprintf (current-output-port) "nova entrada: :~s\n" lisaux)
         (if (string-contains?(list-ref lisaux 1) ")" )
               (when ( > (length (string-split (list-ref lisaux 1) ")") ) 1 )
                    ((lambda(nada)
                       (leitorpdl (string-append (string-replace (car lisaux) " " "") (list-ref (string-split (list-ref lisaux 1) ")") 1 )   ) vert grafo)
                       (leitorpdl (string-replace (car (cdr lisaux)) " " "") vert grafo)
                       )#t)
                               
               )    
           ((lambda(nada)
                    (leitorpdl (string-replace (car lisaux) " " "") vert grafo)  ; faz uma chamada para o 1o cara tratando espaços em branco e etc...
                    (leitorpdl (string-replace (car (cdr lisaux)) " " "") vert grafo)
         
              )#t)
         ) 
         
       )
         (when (or ( string=? inic ";") ( string=? inic "(") ( string=? inic ")") )   ;(string=? inic "?") NUNCA VAI ACONTECER sempre tem uma formula antes ;Inicial é o ;?(o proximo tem que ser um programa) inicial é ?(o proximo tem que ser ; e consequentemente um programa)
      ;    (print"yay")                                                                                            ;simplesmente consome o ; e DEPOIS verifica-se se o programa lido é correto (if la de cima)      
          (leitorpdl (substring frame 1 (string-length frame)) vert grafo)                                  ;Se sim, executa na proxima chamada        
                                                                                                      ;Se não, Ta errado
          
           )

      )
       
      
    


      
  )
   
)  
    
   (when (= (string-length frame) 0)                         ; O frame é nulo ( precisa ser modificado para verificar se o grafo tambem é nulo)     
     (if ( =(length(get-edges grafo)) 0)                                   ;Verifica se existem arestas 
         (fprintf (current-output-port) "O PDL lido existe no grafo \n")
         ;((lambda (nada)(print "O PDL lido existe no grafo") (exit #t) ) #t )                                 ; por mais um if aqui para ele dizer que que o atual é valido 
         (fprintf (current-output-port) "O grafo não foi totalmente inspecionado ou tem arestas a mais. Arestas: ~s \n" (get-edges grafo) )             ; esse print ve se TODAS as arestas estão no grafo mas no caso( A;B U C;D) ele ainda tem que remover as arestas  C e D
         
     )
    )
 
)  
  

(provide leitorpdl) 