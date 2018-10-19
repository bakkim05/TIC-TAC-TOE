#lang racket

;verificar victoria
(define (win? matriz num)
  (or (horizontal matriz num) (vertical matriz num)
       (diagonales matriz num))
  )

;transponer la matriz
(define (transpose matriz)
  (cond((null? matriz) '())
       ((null? (car matriz)) '())
       (else( cons (get_column matriz) (transpose (remove_col matriz))))
   ))

;elimina primera columna
(define (remove_col matriz)
  (cond((null? matriz) '())
       (else(cons (cdar matriz) (remove_col (cdr matriz))))
  ))

;verifica si hay una fila llena de num
(define (horizontal matriz num)
  (cond((null? matriz)#f)
        ((linea_h (car matriz) num) #t)
        (else(horizontal (cdr matriz) num))
   ))

;verifica si hay una columna llena de num
(define (vertical matriz num)
  (horizontal (transpose matriz) num)
  )

;obtener la primera columna de la matriz
(define (get_column matriz)
  (cond((null? matriz) '())
       (else(cons (caar matriz) (get_column (cdr matriz))))
   ))

;verifica si todos los elementos en la lista son iguales a num
(define (linea_h lista num)
  (cond((null? lista)#t)
       ((equal? (car lista) num) (linea_h (cdr lista) num))
       (else #f)
  ))

;invertir lista/ voltear matriz horizontalmente
(define (invertir lista )
  (cond((null? lista) '())
       (else( append (invertir (cdr lista))  (list(car lista)) ))
   ))

;verificar diagonal: \
(define (diagonal matriz num)
  (cond((null? matriz)#t)
       ((null? (car matriz)) #t)
       ((and (equal? (caar matriz) num) (not(null? (cdar matriz))) (null? (cdr matriz)) ) #f)
       ((equal? (caar matriz) num) (diagonal (cdr(remove_col matriz)) num) )
       (else #f)
   ))

;verificar diagonales: \
(define (diagonales1 matriz num)
  (cond ((null? matriz)#f)
        ((diagonal matriz num) #t)
        (else (diagonales1 (cdr matriz) num))
  ))

;verificar diagonales /
(define (diagonales2 matriz num)
  (diagonales1 (invertir matriz) num)
)

(define (diagonales matriz num)
  (or (diagonales1 matriz num) (diagonales2 matriz num)
      (diagonales1 (transpose matriz) num) (diagonales2 (transpose matriz) num)))

(provide (all-defined-out))