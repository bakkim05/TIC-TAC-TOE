#lang racket

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
(define (get_val_mat matriz row col)
    (get_val (get_val matriz row) col)
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

;in progress
(define (check_winner matrix) 
    (cond 
        ((null? matrix) matrix)
        )
)

;in progress
(define (check_row matrix row) 
    (cond 
        ((equal? row (lar_list (car matrix))) #f)
        (else 
            ;(check_row_aux matrix)
            )
        )
)

(define listin (Crear_matriz 3 3 '()))
(define listin2 '((1 2 3) (4 5 6)))

