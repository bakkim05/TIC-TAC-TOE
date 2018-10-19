#lang racket

;; Devuelve una matriz de 0 de la cantidad de filas y columnas que se ingresa.
(define (make_matrix m n)
 (make-matrix_aux m n '()) 
)

;; Crea una lista por cada fila que se ingreso y a esta lista le agrega una lista del tamaño de las columnas.
(define (make-matrix_aux m n result)
  (cond
    ((zero? m)
     result
    )     
    (else
     (make-matrix_aux (- m 1) n (append result (list (make-list n 0))))
    ) 
  )  
)

;; Toma un elemento y los ingresa en la posición de la matriz que se especifíca. Recorre las filas hasta que encuentra la
;; que está buscando.
(define (replace_matrix matrix i j element)
  (cond
    ((zero? i)     
     (cons (replace_list (car matrix) j element) (cdr matrix))
    )
    (else
     ( cons (car matrix) (replace_matrix (cdr matrix) (- i 1) j element ))
    )
  )
)

;; Recorre la fila en la cual está el elemento a modificar y lo cambia.
(define (replace_list list j element)
  (cond
    ((zero? j)
     (cons element (cdr list))
    )
    (else
     (cons(car list) (replace_list (cdr list) (- j 1) element))
    )
 )
)

;; Devuelve un elemento de la matriz. Recorre las filas hasta que encuentra la
;; que está buscando.
(define (get_element matrix i j)
  (cond
    ((zero? i)
     (get_aux (car matrix) j)
    )
    (else
     (get_element (cdr matrix) (- i 1) j  )
    )
  )
)

;; Recorre la fila en la cual está el elemento a encontar y lo retorna.
(define (get_aux list j)
  (cond
    ((zero? j)
     (car list)
    )
    (else
     (get_aux (cdr list) (- j 1) )
    )
  )
)

;; Devueleve los elementos de una matriz en una lista.
(define (matrix-to-list matrix)
  (to-list-aux matrix '())
)

;; Recorre la matriz y va agregando sus elementos a una lista.
(define (to-list-aux matrix result)
  (cond
    ((empty? matrix)
     result
    )
    ((empty? (car matrix))
     (to-list-aux (cdr matrix) result)
    )
    (else
     (to-list-aux (append (list (cdar matrix)) (cdr matrix)) (append result (list (caar matrix))))
    ) 
  )  
)

;; Devuelve el tamaño de la lista.
(define (len lista)
  (cond ((null? lista) 0)
        (else
         (+ 1 (len (cdr lista))))))


;Conjunto de Funciones que devuelve el numero de posibilidades de ganar futuras 
;Devolver posibilidades de gane futuras
(define (posibilidades matriz num)
  (+ (horizontalx matriz num 0) (verticalx matriz num 0)
       (diagonalesx matriz num 0)
  )
)

;transponer la matriz
(define (transposex matriz)
  (cond((null? matriz) '())
       ((null? (car matriz)) '())
       (else( cons (get_columnx matriz) (transposex (remove_colx matriz))))
   ))

;elimina primera columna
(define (remove_colx matriz)
  (cond((null? matriz) '())
       (else(cons (cdar matriz) (remove_colx (cdr matriz))))
  ))

;verifica si hay una fila llena de num
(define (horizontalx matriz num posibilidad)
  (cond((null? matriz) posibilidad)
        ((linea_hx (car matriz) num) (horizontalx (cdr matriz) num (+ posibilidad 1)))
        (else(horizontalx (cdr matriz) num posibilidad))
   ))

;verifica si hay una columna llena de num
(define (verticalx matriz num posibilidad)
  (horizontalx (transposex matriz) num posibilidad)
  )

;obtener la primera columna de la matriz
(define (get_columnx matriz)
  (cond((null? matriz) '())
       (else(cons (caar matriz) (get_columnx (cdr matriz))))
   ))

;verifica si todos los elementos en la lista son iguales a num
(define (linea_hx lista num)
  (cond((null? lista)#t)
       ((or (equal? (car lista) num) (equal? (car lista) 0)) (linea_hx (cdr lista) num))
       (else #f)
  ))

;invertir lista/ voltear matriz horizontalmente
(define (invertirx lista )
  (cond((null? lista) '())
       (else( append (invertirx (cdr lista))  (list(car lista)) ))
   ))

;verificar diagonal: \
(define (diagonalx matriz num)
  (cond((null? matriz)#t)
       ((null? (car matriz)) #t)
       ((and (or (equal? (caar matriz) num) (equal? (caar matriz) 0)) (not(null? (cdar matriz))) (null? (cdr matriz)) ) #f)
       ((or (equal? (caar matriz) num) (equal? (caar matriz) 0)) (diagonalx (cdr(remove_colx matriz)) num) )
       (else #f)
   ))

;verificar diagonalesx: \
(define (diagonalesx1 matriz num posibilidad)
  (cond ((null? matriz) posibilidad)
        ((diagonalx matriz num) (diagonalesx1 (cdr matriz) num (+ posibilidad 1)))
        (else (diagonalesx1 (cdr matriz) num posibilidad))
  ))

;verificar diagonalesx /
(define (diagonalesx2 matriz num posibilidad)
  (diagonalesx1 (invertirx matriz) num posibilidad)
  )

;Main verificar diagonalesx
(define (diagonalesx matriz num posibilidad)
  (cond ((equal? (len matriz) (len (car matriz))) (/ (+ (diagonalesx1 matriz num posibilidad) (diagonalesx2 matriz num posibilidad)
      (diagonalesx1 (transposex matriz) num posibilidad) (diagonalesx2 (transposex matriz) num posibilidad)) 2))
  (else
    (+ (diagonalesx1 matriz num posibilidad) (diagonalesx2 matriz num posibilidad)
       (diagonalesx1 (transposex matriz) num posibilidad) (diagonalesx2 (transposex matriz) num posibilidad)))))

(provide (all-defined-out))