; ------------------------------
; Matriz principal y funciones
; ------------------------------
(define matriz '())

; Crea una fila de valores en la matriz
(define (crearMatrizAux x y total)
  (cond ((= y 0) '())
        (else
         ; Buscar las posiciones iniciales de las fichas en el centro
         (cons (cond [(or (and (= (/ total 2) (- total x))
                               (= (/ total 2) (- total y)))
                          (and (= (- (/ total 2) 1) (- total x))
                               (= (- (/ total 2) 1) (- total y))))
                      1]
                     [(or (and (= (- (/ total 2) 1) (- total x))
                               (= (/ total 2) (- total y)))
                          (and (= (/ total 2) (- total x))
                               (= (- (/ total 2) 1) (- total y))))
                      2]
                     [else 0]
                     )
               (crearMatrizAux x (- y 1) total)))
        ))

; Devuelve una matriz de juego
(define (crearMatriz x y total)
  (cond ((= x 0) (list 1))
        (else (cons (crearMatrizAux x y total)
                    (crearMatriz (- x 1) y total)))
        ))

; Funcion para obtener el tamano de la matriz
(define (tamanoMatriz)
  (- (length matriz) 1))

; Funcion para obtener un elemento que esta en la posicion y de la lista
(define (buscarElemtAux y l)
  (cond ((= y 0)(car l))
        (else (buscarElemtAux (- y 1) (cdr l)))
        )
  )

; Funcion para obtener un elemento que esta en la posicion x de la lista
(define (buscarElemento x y mat)
  (cond ((= x 0)(buscarElemtAux y (car mat)))
        (else (buscarElemento (- x 1) y (cdr mat)))
        )
  )

; Funcion para sustituir un elemento de una lista.
(define (sustElemtAux y ele l)
  (cond ((= y 0)(cons ele (cdr l)))
        (else (cons (car l) (sustElemtAux (- y 1) ele (cdr l))))
        )
  )

; Funcion para sustituir un elemento en una lista de sublistas.
(define (sustElemento x y elemento mat)
  (cond ((not (list? (car mat)))(car mat))
        ((= x 0)(cons (sustElemtAux y elemento (car mat)) (cdr mat)))
        (else (cons (car mat)(sustElemento (- x 1) y elemento (cdr mat))))
        )
  )

; Obtener el cambiar el jugador activo
(define (cambiarJugador jugador mat)
  (cond ((list? (car mat))(cons (car mat)(cambiarJugador jugador (cdr mat))))
        (else (list jugador))))

; Cambiar el jugador directamente en la matriz principal
(define (cambiar-jugador)
  (set! matriz
        (cambiarJugador
         (cond [(= (obtenerJugador matriz) 1) 2]
               [else 1]) matriz))
  )

; Obtener el jugador activo
(define (obtenerJugador mat)
  (cond ((list? (car mat))
         (obtenerJugador (cdr mat)))
        (else (car mat))
        )
  )

;-------------------------------
; Jugador-Jugador
;-------------------------------
; Comprueba si una jugada es valida dada una direccion
(define (comprobar movX movY x y fichasRecorridas jugador n)
  (cond ((or (> 0 (+ x movX))
             (> (+ x movX) (- n 1)))
         #f)
        ((or (> 0 (+ y movY))
             (> (+ y movY) (- n 1)))
         #f)
        ((zero? (buscarElemento (+ x movX) (+ y movY) matriz))
         #f)
        ((and (equal? (buscarElemento (+ x movX) (+ y movY) matriz) jugador)
              (= fichasRecorridas 0))
         #f)
        ((and (equal? (buscarElemento (+ x movX) (+ y movY) matriz) jugador)
              (> fichasRecorridas 0))
         #t)
        ((not (equal? (buscarElemento (+ x movX) (+ y movY) matriz) jugador))
         (comprobar movX movY (+ x movX) (+ y movY) 
                    (+ fichasRecorridas 1) jugador n))
        (else #f)))

; Devuelve una matriz de juego con el resultado
; de una jugada para una direccion dada
(define (cambioFichas movX movY x y jugador n mat)
  (cond ((equal? (buscarElemento (+ x movX) (+ y movY) mat) jugador)
         (sustElemento x y jugador mat))
        (else
         (cambioFichas movX movY (+ x movX) (+ y movY) jugador
                       n (sustElemento x y jugador mat)))
        )
  )

; Comprueba si una jugada es posible en una direccion
(define (ExisteRuta movX movY x y n jugador mod mat)
  (cond ((comprobar movX movY x y 0 jugador n)
         (cond
           [mod
            (set! matriz (cambioFichas movX movY x y jugador n matriz))
            #t]
           [else
            #t
            ]
           ))
        (else #f)))

; Determina si un jugador puede colocar una ficha en una posicion
(define (posible-jugada? x y jugador mod)
  (and
   (zero? (buscarElemento x y matriz))
   (list? (member #t (list 
                      (ExisteRuta -1  0 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta -1  1 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta  0  1 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta  1  1 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta  1  0 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta  1 -1 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta  0 -1 x y (tamanoMatriz) jugador mod matriz)
                      (ExisteRuta -1 -1 x y (tamanoMatriz) jugador mod matriz)
                      )))
   ))

; Devuelve una lista con todas las jugadas validas
; que puede realizar un jugador.
; Esta funcion es la representacion de Vecinos
(define (posibles-jugadas jugador)
  (posibles-jugadas-aux (tamanoMatriz)
                        (tamanoMatriz)
                        jugador))
(define (posibles-jugadas-aux x y jugador)
  (cond [(zero? x) null]
        [(zero? y) (posibles-jugadas-aux (- x 1) (tamanoMatriz) jugador)]
        [else
         (if (posible-jugada? (- (tamanoMatriz) x)
                              (- (tamanoMatriz) y)
                              jugador #f)
             (cons (list (- (tamanoMatriz) x)
                         (- (tamanoMatriz) y))
                   (posibles-jugadas-aux x (- y 1) jugador))
             (posibles-jugadas-aux x (- y 1) jugador))]
        ))

; Realiza una jugada en la posicion dada
(define (Jugada jugada)
  (cond [(posible-jugada? (car jugada) (cadr jugada) (obtenerJugador matriz) #t)
         (cambiar-jugador)
         (dibujar-matriz)
         ; Comprobar si el tablero esta lleno y hay ganador
         (cond [(or (= 1 (fin?))
                    (= 2 (fin?)))
                ; Mostrar el ganador
                (message-box "Juego terminado"
                             (string-append
                              "Ganador: Jugador "
                              (number->string
                               (fin?)))
                             frame
                             (list 'ok))
                (reiniciar?)
                ]
               ; El tablero esta lleno y es empate
               [(= 3 (fin?))
                (message-box "Juego terminado"
                             "El juego es un empate."
                             frame (list 'ok))
                (reiniciar?)
                ]
               ; El tablero no esta lleno aun
               [else
                ; Mostrar posibles jugadas
                (dibujar-jugador)
                ; Si no hay posibles jugadas
                (cond [(null? (posibles-jugadas (obtenerJugador matriz)))
                       (cambiar-jugador)
                       (dibujar-matriz)
                       ; Mostrar mensaje
                       (send frame set-status-text
                             "Sin jugadas. El turno pasa al otro jugador")
                       ; Comprueba si el otro jugador tampoco tiene jugadas
                       ; lo que provoca que el juego se termine (ninguno tienen
                       ; jugadas)
                       (cond [(null? (posibles-jugadas (obtenerJugador matriz)))
                              (message-box "Juego terminado"
                                           (string-append
                                            "No hay mas jugadas posibles. Gana el jugador "
                                            (number->string (obtenerJugador matriz))
                                            )
                                           frame (list 'ok))
                              (dibujar-jugador)
                              ; Preguntar si se quiere iniciar un nuevo juego
                              (reiniciar?)]
                             )
                       ])
                ; Comprobar si el AI esta activado
                (cond [(and (send check-ai get-value)
                            (= 2 (obtenerJugador matriz)))
                       ; En el caso de que lo este, se busca la mejor jugada posible
                       ; y se ejecuta
                       (Jugada (mejor-jugada (posibles-jugadas 2) 2 matriz))
                       ])
                ])
         ]
        [else
         (dibujar-matriz)
         (send frame set-status-text "No válido: No hay fichas enemigas por tomar")
         ]
        )
  
  )

; Devuelve un numero indicando la diferencia de fichas
; de jug con respecto al otro jugador
(define (diferencia-fichas jug mat)
  (- (contar-fichas jug mat)
     (contar-fichas (if (= jug 1) 2 1) mat)
     ))

; Indica si es mejor mat1 que mat2 para jug
(define (mejor? mat1 mat2 jug)
  (cond [(> (diferencia-fichas jug mat1)
            (diferencia-fichas jug mat2))
         #t]
        [else #f]
        ))

; Indica la mejor jugada para un jugador en una matriz
; Esta funcion es una busqueda en Profundidad
(define (mejor-jugada posibles jugador mat)
  (mejor-jugada-aux posibles jugador mat (car posibles))
  )
(define (mejor-jugada-aux posibles jugador mat mejor-jugada)
  (cond [(null? posibles) mejor-jugada]
        [(mejor? (simular-jugada (car posibles) jugador mat)
                 (simular-jugada mejor-jugada jugador mat)
                 jugador)
         (mejor-jugada-aux (cdr posibles) jugador mat (car posibles))]
        [else (mejor-jugada-aux (cdr posibles) jugador mat mejor-jugada)]
        ))

; Devuelve una matriz con los cambios para una direccion
(define (simular movX movY x y jugador matrix)
  (cond ((> movX 1)matrix)
        (else (simular (+ movX 1) movY x y jugador 
                       (simular-aux movX movY x y jugador matrix)))        
        )
  )
(define (simular-aux movX movY x y jugador matrix)
  (cond ((and (= movX 0)
              (= movY 0))
         (simular-aux movX (+ movY 1)
                      x y jugador matrix))
        ((> movY 1)matrix)
        ((equal? (ExisteRuta movX movY x y (tamanoMatriz) jugador #f matrix) #t)
         (simular-aux movX (+ movY 1) x y jugador 
                      (cambioFichas movX movY x y jugador (tamanoMatriz) matrix)))
        (else (simular-aux movX (+ movY 1) x y jugador matrix))
        )
  )

; Muestra el estado del tablero si se hace una jugada
(define (simular-jugada jugada jugador mat)
  (cond [(null? jugada) null]
        [else
         (simular -1 -1 (car jugada) (cadr jugada) jugador mat)]
        ))

; ------------------------------
; Fin del juego
; ------------------------------
; Comprueba la cantidad de fichas asignadas a un jugador
(define (contar-fichas jugador mat)
  (contar-fichas-aux jugador mat))
(define (contar-fichas-aux jugador mat)
  (cond [(or (null? mat) (not (list? (car mat)))) 0]
        [else (+ (contar-fichas-vector-aux jugador (car mat))
                 (contar-fichas-aux jugador (cdr mat)))]
        ))
(define (contar-fichas-vector-aux jugador vector)
  (cond [(null? vector) 0]
        [(= jugador (car vector))
         (+ 1 (contar-fichas-vector-aux jugador (cdr vector)))]
        [else (contar-fichas-vector-aux jugador (cdr vector))]
        ))

; Revisa si el juego termino, y devuelve el jugador ganador
; cero en caso de que no haya terminado, y 3 si es empate
(define (fin?)
  (cond [(zero? (contar-fichas 0 matriz))
         (cond [(> (contar-fichas 1 matriz) (contar-fichas 2 matriz)) 1]
               [(> (contar-fichas 2 matriz) (contar-fichas 1 matriz)) 2]
               [else 3])]
        [else 0]
        ))

; ------------------------------
; Imagenes
; ------------------------------

(define img-fondo   (make-object bitmap% "res/fondo.png"   'png))
(define img-negra   (make-object bitmap% "res/negra.png"   'png))
(define img-blanca  (make-object bitmap% "res/blanca.png"  'png))
(define img-posible (make-object bitmap% "res/posible.png" 'png))
(define img-icono   (make-object bitmap% "res/icono.png"   'png))

; ------------------------------
; Interfaz Grafica de Usuario
; ------------------------------

; Funcion de cada boton
(define (accionBoton x y)
  ; La casilla seleccionada esta vacia
  (cond [(= 0 (buscarElemento y x matriz))
         ; Realizar jugada en la posicion indicada
         (Jugada (list y x))
         ]
        ; Mostrar error por jugada no valida
        [else
         (send frame set-status-text "No valido: Casilla no vacía.")
         ]))

; Preguntar si se desea reiniciar el juego
(define (reiniciar?)
  (cond [(equal? 'yes
                 (message-box
                  "Reiniciar"
                  "¿Desea iniciar un nuevo juego?"
                  frame (list 'yes-no))
                 )
         (reiniciar (tamanoMatriz))]
        ))

; Dibujar matriz de juego en la interfaz
(define (dibujar-matriz)
  (dibujar-matriz-aux matriz button-matrix)
  (dibujar-posibles (posibles-jugadas (obtenerJugador matriz)))
  (dibujar-jugador)
  )
(define (dibujar-matriz-aux mJuego mBotones)
  (cond [(null? mBotones) null]
        [else
         (dibujar-vector-aux (car mJuego) (car mBotones))
         (dibujar-matriz-aux (cdr mJuego) (cdr mBotones))]
        ))
(define (dibujar-vector-aux vJuego vBotones)
  (cond [(null? vBotones) null]
        [else
         (send (car vBotones)
               set-label
               (cond
                 [(= 0 (car vJuego)) img-fondo]
                 [(= 1 (car vJuego)) img-blanca]
                 [(= 2 (car vJuego)) img-negra]
                 ))
         (dibujar-vector-aux (cdr vJuego) (cdr vBotones))]
        ))
; Muestra graficamente las posibles jugadas (validas) para un jugador
(define (dibujar-posibles lista)
  (cond [(not (null? lista))
         (send (buscarElemento (caar lista) (cadar lista) button-matrix)
               set-label img-posible)
         (dibujar-posibles (cdr lista))
         ]))

; Mostrar siguiente jugador
(define (dibujar-jugador)
  (send msg set-label
        (string-append "Turno del jugador "
                       (number->string 
                        (obtenerJugador matriz)))
        )
  (send frame set-status-text
        (string-append
         "Jugador 1: "
         (number->string
          (contar-fichas 1 matriz))
         ". Jugador 2: "
         (number->string
          (contar-fichas 2 matriz))
         "."
         ))
  (send (send status-canvas get-dc) set-brush "black" 'solid)
  (send (send status-canvas get-dc) set-pen "white" 2 'solid)
  (send (send status-canvas get-dc)
        draw-rectangle 0 0 (send status-canvas get-width) 20)
  (send (send status-canvas get-dc) set-brush "white" 'solid)
  (send (send status-canvas get-dc) set-pen "white" 2 'solid)
  (send (send status-canvas get-dc)
        draw-rectangle 0 0
        (/ (* (send status-canvas get-width)
              (contar-fichas 1 matriz))
           (+ (contar-fichas 1 matriz)
               (contar-fichas 2 matriz)))
        20)
  )

; Ventana inicial
(define frame (new frame%
                   [label "Othelo"]
                   [x 400]
                   [y 200]
                   [alignment (list 'center 'center)]
                   [min-width 200]
                   ))
; Mensaje que indica el jugador actual
(define msg (new message% [parent frame]
                 [label "Turno del jugador 1"]
                 [auto-resize #t]
                 ))
(define pane-controles
  (new horizontal-pane%
       [parent frame]))
; Check-box para el AI
(define check-ai
  (new check-box%
       [parent pane-controles]
       [label "Jugar contra máquina"]
       [value #f]))
(define button-reiniciar
  (new button%
       [parent pane-controles]
       [label "Reiniciar"]
       [callback
        (lambda (b e)
          (reiniciar?))]
       ))

; Agregar los botones a la interfaz
(define (addButtons total filas columnas panel)
  (cond [(> columnas 0)
         (cons (new button%
                    [parent panel]
                    [vert-margin 0]
                    [horiz-margin 0]
                    [label img-fondo]
                    [callback (lambda (button event)
                                (accionBoton (- total columnas)
                                             (- total filas))
                                )])
               (addButtons total filas (- columnas 1) panel))]
        [else null]
        ))
(define (addButtonMatrix total filas columnas parentPanel)
  (cond [(> filas 0)
         (cons 
          (addButtons total filas
                      columnas (new horizontal-panel%
                                    [parent parentPanel]
                                    [alignment (list 'center 'center)]
                                    ))
          (addButtonMatrix total (- filas 1)
                           columnas parentPanel))]
        [else null]
        ))

(define button-matrix null)
(define contenedor-botones
  (new vertical-pane%
       [parent frame]
       ))
(define status-canvas
  (new canvas%
       [parent frame]
       [min-height 20]
       ))

; Limpia la matriz de juego y la dibuja en pantalla
(define (reiniciar tam)
  (set! matriz (crearMatriz tam tam tam))
  (dibujar-matriz)
  )

; Inicializar el programa
(define (othelo tam)
  ; Comprobar el tamano de matriz
  (cond [(and (> tam 2) (= 0 (remainder tam 2)))
         ; Dibujar icono y status-line
         (send frame set-icon img-icono)
         (send frame create-status-line)
         ; Agregar los botones
         (set! button-matrix (addButtonMatrix tam tam tam contenedor-botones))
         ; Limpiar matriz y mostrarla
         (reiniciar tam)
         ; Mostrar ventana
         (send frame show #t)
         ]
        [else
         (display "Error: El tamaño debe ser un par mayor a 2")
         ]
        )
  )

(display "Inicie el juego llamando la función (othelo TAM), siendo TAM la dimensión del tablero de juego")