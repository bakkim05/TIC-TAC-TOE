#lang racket/gui

(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define setMatrix (new dialog% [label "Set Matrix Dimensions"]))

(define inputI (new text-field% [label "i"][parent setMatrix][init-value "3"]
                    [callback
                     (lambda (f ev)
                       (define v (send f get-value))
                       (unless (string->number v)
                         (send f set-value (regexp-replace* #rx"1[0]|[3-9]" v ""))))]))

(define inputJ (new text-field% [label "j"][parent setMatrix][init-value "3"]
                    [callback
                     (lambda (f ev)
                       (define v (send f get-value))
                       (unless (string->number v)
                         (send f set-value (regexp-replace* #rx"1[0]|[3-9]" v ""))))]))

(define button (new horizontal-pane% [parent setMatrix]))
(define confirmButton (new button% [parent button][label "Confirm"]
                           [callback (lambda (b e) (when (message-box "Confirm" "Are you sure?"
                                                                     setMatrix '(yes-no))
                                                     (let* ([v (string->number (send inputI get-value))])  (set! cantidadX v))
                                                     (let* ([u (string->number (send inputJ get-value))])  (set! cantidadY u))))]))

(define cantidadX 3) ; cantidad de cuadros en el eje X
(define cantidadY 3) ; cantidad de cuadros en el eje Y

(send setMatrix show #t)

(define dimensionI (* 80 cantidadX)) ; define el tamano de las lineas y el tablero en el eje X
(define dimensionJ (* 80 cantidadY)) ; define el tamano de las lineas y el tablero en el eje Y

(define cuadroX (* 80 (+ 1 cantidadX))) ; rango X el cual se utiliza para las lineas verticales del tablero
(define cuadroY (* 80 (+ 1 cantidadY))) ; rango Y el cual se utiliza para las lineas horizontales del tablerO

(define z (open-viewport "TIC-TAC-TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define las dimensiones de la ventana dejando 10 pxl entre los bordes en ambos ejes
(define p (open-pixmap "TIC/TAC/TOE" (+ dimensionI 20) (+ dimensionJ 20))) ; define una ventana oculta para poder actualizar el tablero

(define u 0) ; alternar turno
(define h 0) ; inicial eje x
(define v 0) ; inicial eje y
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
(define (dibujarMarca i j)
  (cond
    ((eq? u 0) (dibujarX i j))
    ((eq? u 1) (dibujarO i j))
    )
  )

;dibujar X
(define (dibujarX i j)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Oska/Desktop/Repos/TIC TAC TOE/X.png" (make-posn (+ a 22) (+ b 15)))
    (copy-viewport p z)
      
    )
  )

;dibujar O
(define (dibujarO i j)
    (begin
    (define a (+ (* i 80) margen))
    (define b (+ (* j 80) margen))

    ((draw-pixmap p) "C:/Users/Oska/Desktop/Repos/TIC TAC TOE/O.png" (make-posn (+ a 22) (+ b 15)))
    (copy-viewport p z)
      
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

#|
;evalua posicion donde se da click y determina la accion que debe tomar
(define (juego)
  (if (equal? (left-mouse-click? (get-mouse-click z)) #f)
      (juego)
      (begin
        (cond
          ((or (or (> (posn-x (query-mouse-posn z)) dimensionI) (< (posn-x (query-mouse-posn z)) margen)) (or (> (posn-y (query-mouse-posn z)) dimensionJ) (< (posn-y (query-mouse-posn z)) margen)))(msj FT))
          )
        (else
         (dibujarMarca )
         )
        (juego)
        )
      )
  )

|#

;(juego)













