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

    ((draw-pixmap p) "C:/Users/Oska/Desktop/REPOS/TIC TAC TOE/visuals/X.png" (make-posn (+ a 22) (+ b 15)))
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

    ((draw-pixmap p) "C:/Users/Oska/Desktop/REPOS/TIC TAC TOE/visuals/O.png" (make-posn (+ a 22) (+ b 15)))
    (copy-viewport p z)
    (lines)
    (sleep 1)
    )
  (juego (insertar (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))) mInicial 2) (- turno 1))
  )

(define (lines)
  ; lineas verticales
  (for ([h (in-range 10 cuadroX 80)])
  ((draw-line z)(make-posn h 10) (make-posn h (+ dimensionJ 10)) "black")
  )

; lineas horizontales
(for ([v (in-range 10 cuadroY 80)])
  ((draw-line z)(make-posn 10 v) (make-posn (+ dimensionI 10) v) "black")
  )
  )

; coodenadas del click
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
  (print mInicial)
  (cond
    ((equal? (left-mouse-click? (get-mouse-click z)) #f) (juego mInicial turno))
    (else
     (juego_aux mInicial turno)
     )
    )

  )

;funcion auxiliar para comenzar el juego
(define (juego_aux mInicial turno)
  (reglas mInicial turno)
  )

; verifica las reglas
(define (reglas mInicial turno)
  (cond
    ((or (or (> (posn-x (query-mouse-posn z)) dimensionI) (< (posn-x (query-mouse-posn z)) margen)) (or (> (posn-y (query-mouse-posn z)) dimensionJ) (< (posn-y (query-mouse-posn z)) margen)))(msj FT) (juego mInicial turno))
    ((or(compararMatrix_val (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))) mInicial 1)(compararMatrix_val (pos (posn-x (query-mouse-posn z))) (pos (posn-y (query-mouse-posn z))) mInicial 2)) (msj NP) (juego mInicial turno))
    (else
     (dibujarMarca (pos (posn-x (query-mouse-posn z)))(pos (posn-y (query-mouse-posn z))) turno mInicial)
     )
    )
  )


; corre el juego
(juego (Crear_matriz cantidadY cantidadX '()) 2)







