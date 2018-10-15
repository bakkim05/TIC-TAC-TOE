#lang racket

(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define cantidadX 3) ; cantidad de cuadros en el eje X
(define cantidadY 3) ; cantidad de cuadros en el eje Y

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

; lineas verticales
(for ([h (in-range 10 cuadroX 80)])
  ((draw-line z)(make-posn h 10) (make-posn h (+ dimensionJ 10)) "black")
  )

; lineas horizontales
(for ([v (in-range 10 cuadroY 80)])
  ((draw-line z)(make-posn 10 v) (make-posn (+ dimensionI 10) v) "black")
  )



; funcion para X

; funcion para O

; dibujar
;(define (dibujar i j)
 
;  )

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

