#lang racket/gui

(require (lib "graphics.ss" "graphics"))
(open-graphics)

#|--------------------------------------------------------------------------------------------------------------------------------------------|#
#|                                               Ventana para definir tamano de la matriz                                                     |#
#|--------------------------------------------------------------------------------------------------------------------------------------------|#

(define setMatrix (new dialog% [label "Set Matrix Dimensions"]))

;define el eje i de la matriz
;por medio de la expresion regular solo permite ingresar digitos al casillero
(define inputI (new text-field% [label "columna"][parent setMatrix][init-value "3"]
                    [callback
        (lambda(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #px"[^(0-9)]" v ""))))]))

(define inputJ (new text-field% [label "fila"][parent setMatrix][init-value "5"]
                    [callback
        (lambda(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #rx"[^(0-9)]" v ""))))]))

(define confirmButton (new button% [parent setMatrix][label "Confirm"]
                           [callback (lambda (b e) (when (message-box "Confirm" "Are you sure?"
                                                                     setMatrix '(yes-no))
                                                     (let* ([v (string->number (send inputI get-value))])  (set! cantidadX (limite v)))
                                                     (let* ([u (string->number (send inputJ get-value))])  (set! cantidadY (limite u)))
                                                     (send setMatrix show #f)))]))
; limite de los de los ejes de la matriz (rechaza numeros menor a 3 o mayores a 10)
(define (limite numero)
  (cond
    ((>= 3 numero) 3)
    ((<= 10 numero) 10)
    (else
     numero)))

#|--------------------------------------------------------------------------------------------------------------------------------------------|#
#|                                                           LOGICA DEL JUEGO                                                                 |#
#|--------------------------------------------------------------------------------------------------------------------------------------------|#

; crea una lista llena de n 0s
(define (Crear_row cols result) 
    (cond 
        ((zero? cols) result)
        (else (Crear_row (- cols 1) (append result (list 0))))
        )
)

; crea una matriz llena de ceros
(define (Crear_matriz row cols result)
    (cond 
        ((zero? row) result)
        (else 
            (Crear_matriz (- row 1) cols (append result (list (Crear_row cols '())))
                )
            )
        )
)

; compara el valor en la posicion x y con el valor requerido
(define (compararMatrix_val row col matrix value)
    (cond
        ((null? matrix) matrix) 
        ((and (zero? row) (zero? col))
            (cond 
                ((not (list? (car matrix))) 
                    (equal? (car matrix) value)
                    )
                (else 
                    (equal? (caar matrix) value)
                    )
                )
            )
        ((not (zero? row)) 
            (compararMatrix_val (- row 1) col (cdr matrix) value)
            )
        ((zero? row) 
            (cond 
                ((list? (car matrix)) 
                    (compararMatrix_val row col (car matrix) value)
                    )
                ((not (zero? col)) 
                    (compararMatrix_val row (- col 1) (cdr matrix) value)
                    )
                )
            )
        )
)

; inserta un valor en la posicion de la lista dada y devuelve la lista modificada
(define (insertar_lis lista pos elemt) 
    (cond 
        ((null? lista) lista)
        ((zero? pos) 
            (append (list elemt) (cdr lista))
            )
        (else 
            (append 
                (list (car lista)) (insertar_lis (cdr lista) (- pos 1) elemt)
                )
            )
        )
)

; inserta el valor en la posicion i j en una matriz
(define (insertar col row matrix value) 
    (cond 
        ((null? matrix) matrix)
        ((zero? row) 
            (append (list (insertar_lis (car matrix) col value)) (cdr matrix))
            )
        (else 
            (append 
                (list (car matrix)) (insertar col (- row 1) (cdr matrix) value)
                )
            )
        )
)

;obtiene el valor en determinada posicion en una lista
(define (get_val lista pos) 
    (cond 
        ((null? lista) lista)
        ((zero? pos) (car lista))
        (else 
            (get_val (cdr lista) (- pos 1))
            )
        )
)

;obtiene el valor en determinada posicion en una matriz
(define (get_val_mat matrix row col)
    (get_val (get_val matrix row) col)
)

;funcion auxiliar 
(define (lar_list_aux lista largo)
    (cond
        ((null? lista) largo)
        (else (lar_list_aux (cdr lista) (+ largo 1)))
    )
)

;funcion para obtener el largo de una lista
(define (lar_list lista)
    (cond
        ((null? lista) 0)
        (else (lar_list_aux (cdr lista) 1))
    )
)

;funcion que devuelve la fila de una matriz. Recibe la matriz y el indice de la fila
(define (get_row matrix nrow) 
    (cond 
        ((zero? nrow) (car matrix))
        (else 
            (get_row (cdr matrix) (- nrow 1))
            )
        )
)

;funcion auxiliar que recibe la fila, el valor a comprobar y el indice para recorrer la fila
(define (check_row_aux row value pos) 
    (cond 
        ((zero? pos) #t)
        ((equal? (get_val row pos) value) 
            (check_row_aux row value (- pos 1))
            )
        (else 
            #f
            )
        )
)

;verifica que en toda la fila exista el mismo valor
(define (check_row matrix row) 
    (cond 
        ((equal? row (lar_list (car matrix))) #f)
        (else 
            (check_row_aux (get_row matrix row) (get_val (get_row matrix row) 0) 
                (- (lar_list (get_row matrix row)) 1) 
                    )
            )
        )
)

;funcion que itera sobre las filas de la matris para verificar si existe algun 
;ganador. Recibe la matriz y el indice de la fila
(define (check_winner_row_aux matrix row)   
    (cond 
        ((equal? row (lar_list matrix)) #f)
        ((not (check_row matrix row)) 
            (check_winner_row_aux matrix (+ row 1))
            )
        (else #t)
        )
)

;funcion que elimina la primer columna de la matris, recibe la matriz original y una lista vacia
(define (get_rest mat result)
    (cond 
        ((null? mat) result)
        ((null? (cdar mat)) '())
        (else
            (get_rest (cdr mat) (append result (list (cdar mat))))
        )
    )
)

;funcion auxiliar para obtener la columna, recibe la matriz y una lista vacia
(define (get_col_aux matrix result) 
    (cond 
        ((null? matrix) result)
        (else 
            (get_col_aux (cdr matrix) (append result (list (caar matrix))))
            )
        )
)

;funcion para obtener alguna columna de la matris. Recibe la matris y el indice de la columna
(define (get_col matrix ncol) 
    (cond 
        ((zero? ncol) (get_col_aux matrix '()))
        (else 
            (get_col (get_rest matrix '()) (- ncol 1))
            )
        )
)

;funcion auxiliar que recibe la columna, el valor para compara y el indice
(define (check_col_aux col value pos) 
    (cond 
        ((zero? pos) #t)
        ((equal? (get_val col pos) value) 
            (check_col_aux col value (- pos 1))
            )
        (else 
            #f
            )
        )
)

;funcion para verificar si toda la columna posee el mismo valor, recibe la matris
;y el indice de la columna
(define (check_col matrix col) 
    (cond 
        ((equal? col (lar_list matrix)) #f)
        (else 
            (check_col_aux (get_col matrix col) (get_val (get_col matrix col) 0) 
                (- (lar_list (get_col matrix col)) 1)
                )
            )
        )
)

;funcion auxiliar para iterar sobre las columnas de la matris para buscar si hay
;un gandor, recibe la matris y el numero de la columna
(define (check_winner_col_aux matrix col) 
    (cond 
        ((equal? col (lar_list matrix)) #f)
        ((not (check_col matrix col)) 
            (check_winner_col_aux matrix (+ col 1))
            )
        (else #t)
        )
)

;funcion auxiliar para obtener la diagonal desde arriba bajando hacia la derecha
(define (get_diag_top_aux matrix row col result) 
    (cond 
        ((or 
            (equal? row (lar_list matrix)) (equal? col (lar_list (car matrix)))) 
                result)
        (else 
            (get_diag_top_aux matrix (+ row 1) (+ col 1) (append result (list (get_val_mat matrix row col))))
            )
        )
)

;funcion para obtener la diagonal, recibe la matris y el indice de columna para luego
;ir dezplazando
(define (get_diag_top matrix col) 
    (cond 
        ((> col (- (lar_list (car matrix)) 3)) '())
        (else 
            (get_diag_top_aux matrix 0 col '())
            )
        )
)

;funcion auxiliar para obtener la diagonal desde abajo subiendo hacia la derecha
(define (get_diag_bot_aux matrix row col result) 
    (cond 
        ((or 
            (equal? row -1) (equal? col (lar_list (car matrix)))) 
                result)
        (else 
            (get_diag_bot_aux matrix (- row 1) (+ col 1) (append result (list (get_val_mat matrix row col))))
            )
        )
)

;funcion para obtener la diagonal desde abajo hacia la derecha, recibe la matris y
;la fila desde donde empieza la diagonal
(define (get_diag_bot matrix row) 
    (cond 
        ((< row (- (lar_list matrix) 3)) '())
        (else 
            (get_diag_bot_aux matrix row 0 '())
            )
        )
)

;funcion auxiliar para verificar si existe
;algun ganador
(define (check_diag_top_aux diag value pos) 
    (cond 
        ((zero? pos) #t)
        ((equal? (get_val diag pos) value) 
            (check_diag_top_aux diag value (- pos 1))
            )
        (else #f)
    )
)

;funcion para verificar si toda la diagonal contiene el mismo symgolo
(define (check_diag_top matrix diag) 
    (cond 
        ((equal? diag (lar_list matrix)) #f)
        (else 
            (check_diag_top_aux (get_diag_top matrix diag) (get_val (get_diag_top matrix diag) 0) 
                (- (lar_list (get_diag_top matrix diag)) 1)
                )
            )
        )
)  

;funcion auxiliar para iterar sobre las diagonales de la matriz
(define (check_winner_diag_top_aux matrix diag) 
    (cond 
        ((> diag (- (lar_list (car matrix)) 3)) #f)
        ((not (check_diag_top matrix diag)) 
            (check_winner_diag_top_aux matrix (+ diag 1))
            )
        (else #t)
        )
)

;funcion auxiliar para verificar si existe
;algun ganador
(define (check_diag_bot_aux diag value pos) 
    (cond 
        ((zero? pos) #t)
        ((equal? (get_val diag pos) value) 
            (check_diag_bot_aux diag value (- pos 1))
            )
        (else #f)
    )
)

;funcion para verificar el valor de la diagonal
(define (check_diag_bot matrix diag) 
    (cond 
        ((equal? diag (lar_list matrix)) #f)
        (else 
            (check_diag_bot_aux (get_diag_bot matrix diag) (get_val (get_diag_bot matrix diag) 0) 
                (- (lar_list (get_diag_bot matrix diag)) 1)
                )
            )
        )
)

;funcion para iterar sobre las diagonales para verificar el valor de la diagonal
;vista desde abajo hacia arriba 
(define (check_winner_diag_bot_aux matrix diag) 
    (cond 
        ((equal? diag -1) #f)
        ((not (check_diag_bot matrix diag)) 
            (check_winner_diag_bot_aux matrix (- diag 1))
            )
        (else #t)
        )
)

;funcion para ver si existe algun jugador contemplando ultima jugada
(define (check_winner matrix)
    (cond 
        ((check_winner_row_aux matrix 0) #t)
        ((check_winner_col_aux matrix 0) #t)
        ((check_winner_diag_top_aux matrix 0) #t)
        ((check_winner_diag_bot_aux matrix (lar_list matrix)) #t)
        (else #f)
        )
)

(define (verificar matrix turno)
  (cond
    ((and (check_winner matrix) (eq? turno 1)) (msj WX)) ;falta funcion para parar el juego
    ((and (check_winner matrix) (eq? turno 2)) (msj WO)) ;falta funcion para parar el juego
    )
  )



#|--------------------------------------------------------------------------------------------------------------------------------------------|#
#|                                                           ESTRUCTURA DEL JUEGO                                                             |#
#|--------------------------------------------------------------------------------------------------------------------------------------------|#

(define cantidadX 3) ; cantidad de cuadros en el eje FILAS
(define cantidadY 3) ; cantidad de cuadros en el eje COLUMNAS


(send setMatrix show #t) ; despliega la ventana de setMatrix

(define dimensionI (* 80 cantidadX)) ; define el tamano de las lineas y el tablero en el eje de las FILAS
(define dimensionJ (* 80 cantidadY)) ; define el tamano de las lineas y el tablero en el eje de las COLUMNAS

(define cuadroX (* 80 (+ 1 cantidadX))) ; rango el cual se utiliza para las lineas horizontales del tablerO (FILAS)
(define cuadroY (* 80 (+ 1 cantidadY))) ; rango el cual se utiliza para las lineas verticales del tablero (COLUMNAS)

(define z (open-viewport "TIC-TAC-TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define las dimensiones de la ventana dejando 10 pxl entre los bordes en ambos ejes
(define p (open-pixmap "TIC/TAC/TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define una ventana oculta para poder actualizar el tablero

(define h 0) ; inicial eje FILAS
(define v 0) ; inicial eje COLUMNAS
(define margen 10)
(define FT "Fuera del Tablero") ; variable que contiene el mensaje de Fuera del Tablero
(define NP "No se puede poner ahi") ; variable que contiene el mensaje de Ya el campo esta ocupado
(define WX "JUGADOR X GANO") ; variable que contiene el mensaje de gane para X.
(define WO "JUGADOR X GANO") ; variable que contiene el mensaje de gane para O.

; lineas verticales
(for ([h (in-range 10 cuadroX 80)])
  ((draw-line z)(make-posn h 10) (make-posn h (+ dimensionJ 10)) "black")
  )

; lineas horizontales
(for ([v (in-range 10 cuadroY 80)])
  ((draw-line z)(make-posn 10 v) (make-posn (+ dimensionI 10) v) "black")
  )

;dibujar
(define (dibujarMarca i j turno mInicial)
  (cond
    ((eq? turno 2) (dibujarO i j turno mInicial))
    ((eq? turno 1) (dibujarX i j turno mInicial))
    )
  (copy-viewport p z) ;aqui posiblemente va haber problemas de graficos
  (lines) ; al igual que aqui
  )

;dibujar X
(define (dibujarX i j turno mInicial)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Juno/Desktop/REPOS/TIC-TAC-TOE/visuals/X.png" (make-posn (+ a 22) (+ b 15)))
    (copy-viewport p z)
    (lines)
    (sleep 1)
    )
  (juego (insertar (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))) mInicial 1) (+ turno 1))
  )

;dibujar O
(define (dibujarO i j turno mInicial)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Juno/Desktop/REPOS/TIC-TAC-TOE/visuals/O.png" (make-posn (+ a 22) (+ b 15)))
    (copy-viewport p z)
    (lines)
    (sleep 1)
    )
  (juego (insertar (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))) mInicial 2) (- turno 1))
  )

(define (lines)
  ; lineas horizontales
  (for ([v (in-range 10 cuadroY 80)])
    ((draw-line z)(make-posn 10 v) (make-posn (+ dimensionI 10) v) "black")
    )
  ; lineas verticales
  (for ([h (in-range 10 cuadroX 80)])
    ((draw-line z)(make-posn h 10) (make-posn h (+ dimensionJ 10)) "black")
    )
  )

; coodenadas del click truncadas para que se muestren de forma correcta
(define (pos n)
  (truncate (/ (- n margen) 80))
  )

; despliega alerta
(define (msj text)
  (define m (open-viewport "Alerta" 300 50))
  ((draw-string m) (make-posn 50 20) text "red")
  (sleep 2)
  (close-viewport m)
  )

; comienza a jugar despues de un click
(define (juego mInicial turno)
  ;(print mInicial)
  (cond
    ((equal? (left-mouse-click? (get-mouse-click z)) #f) (juego mInicial turno))
    (else
     (verificar mInicial turno)
     (juego_aux mInicial turno (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))))
     )
    )

  )

;funcion auxiliar para comenzar el juego
(define (juego_aux mInicial turno mouseX mouseY)
  (reglas mInicial turno mouseX mouseY)
  )

; verifica las reglas
(define (reglas mInicial turno mouseX mouseY)
  (cond
    ((or (or (> (posn-x (query-mouse-posn z)) dimensionI) (< (posn-x (query-mouse-posn z)) margen)) (or (> (posn-y (query-mouse-posn z)) dimensionJ) (< (posn-y (query-mouse-posn z)) margen)))(msj FT) (juego mInicial turno))
    ((not(compararMatrix_val mouseY mouseX mInicial 0))
        (msj NP)
        (print (pos (posn-x (query-mouse-posn z))))
        (print (pos (posn-y (query-mouse-posn z))))
        (juego mInicial turno)
        )
    (else
     (dibujarMarca mouseX mouseY turno mInicial)
     )
    )
  )


; corre el juego
(juego (Crear_matriz cantidadY cantidadX '()) 2)

