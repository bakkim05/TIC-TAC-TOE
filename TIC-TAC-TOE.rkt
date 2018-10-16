#lang racket/gui

(require (lib "graphics.ss" "graphics"))
(open-graphics)

#|--------------------------------------------------------------------------------------------------------------------------------------------|#
#|                                               Ventana para definir tamano de la matriz                                                     |#
#|--------------------------------------------------------------------------------------------------------------------------------------------|#

(define setMatrix (new dialog% [label "Set Matrix Dimensions"]))

;define el eje i de la matriz
;por medio de la expresion regular solo permite ingresar digitos al casillero
(define inputI (new text-field% [label "i"][parent setMatrix][init-value "3"]
                    [callback
        (lambda(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #px"[^(3-9)]" v ""))))]))

(define inputJ (new text-field% [label "j"][parent setMatrix][init-value "3"]
                    [callback
        (lambda(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #rx"[^(3-9)]" v ""))))]))

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
#|                                                           ESTRUCTURA DEL JUEGO                                                             |#
#|--------------------------------------------------------------------------------------------------------------------------------------------|#

(define cantidadX 3) ; cantidad de cuadros en el eje X
(define cantidadY 3) ; cantidad de cuadros en el eje Y

(send setMatrix show #t) ; despliega la ventana de setMatrix

(define dimensionI (* 80 cantidadX)) ; define el tamano de las lineas y el tablero en el eje X
(define dimensionJ (* 80 cantidadY)) ; define el tamano de las lineas y el tablero en el eje Y

(define cuadroX (* 80 (+ 1 cantidadX))) ; rango X el cual se utiliza para las lineas verticales del tablero
(define cuadroY (* 80 (+ 1 cantidadY))) ; rango Y el cual se utiliza para las lineas horizontales del tablerO

(define z (open-viewport "TIC-TAC-TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define las dimensiones de la ventana dejando 10 pxl entre los bordes en ambos ejes
(define p (open-pixmap "TIC/TAC/TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define una ventana oculta para poder actualizar el tablero

(define u 1) ; 0 turno O ; 1 turno X
(define h 0) ; inicial eje x
(define v 0) ; inicial eje y
(define margen 10)
(define FT "Fuera del Tablero") ; variable que contiene el mensaje de Fuera del Tablero
(define NP "No se puede poner ahi") ; variable que contiene el mensaje de Ya el campo esta ocupado

; cambia de turno
(define (cambiarTurno)
 (cond
   ((eq? u 2) (set! u 1))
   ((eq? u 1) (set! u 2))
   )
  )

; lineas verticales
(for ([h (in-range 10 cuadroX 80)])
  ((draw-line z)(make-posn h 10) (make-posn h (+ dimensionJ 10)) "black")
  )

; lineas horizontales
(for ([v (in-range 10 cuadroY 80)])
  ((draw-line z)(make-posn 10 v) (make-posn (+ dimensionI 10) v) "black")
  )

;dibujar
(define (dibujarMarca i j turno)
  (cond
    ((eq? turno 2) (dibujarO i j))
    ((eq? turno 1) (dibujarX i j))
    )
  (copy-viewport p z)
  (lines)
  (cambiarTurno)
  )

;dibujar X
(define (dibujarX i j)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Oska/Desktop/Repos/TIC TAC TOE/visuals/X.png" (make-posn (+ a 22) (+ b 15)))
    )
  )

;dibujar O
(define (dibujarO i j)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Oska/Desktop/Repos/TIC TAC TOE/visuals/O.png" (make-posn (+ a 22) (+ b 15)))
    )
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

;evalua posicion donde se da click y determina la accion que debe tomar
(define (juego)
  (if (equal? (left-mouse-click? (get-mouse-click z)) #f)
      (juego)
      (begin
        (cond
          ((or (or (> (posn-x (query-mouse-posn z)) dimensionI) (< (posn-x (query-mouse-posn z)) margen)) (or (> (posn-y (query-mouse-posn z)) dimensionJ) (< (posn-y (query-mouse-posn z)) margen)))(msj FT))
          (else
           (dibujarMarca (pos (posn-x (query-mouse-posn z)))(pos (posn-y (query-mouse-posn z))) u)
           )
          )
        (juego)
        )
      )
  )


; corre el juego
(juego)













