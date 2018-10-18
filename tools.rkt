#lang racket

; crea una lista llena de n 0s
(define (Crear_row cols result) 
    (cond 
        ((zero? cols) result)
        (else (Crear_row (- cols 1) (append result (list 0))))
        )
)

; crea una matriz llena de ceros
(define (Crear_matrix row cols result)
    (cond 
        ((zero? row) result)
        (else 
            (Crear_matrix (- row 1) cols (append result (list (Crear_row cols '())))
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

(define (get_row matrix nrow) 
    (cond 
        ((zero? nrow) (car matrix))
        (else 
            (get_row (cdr matrix) (- nrow 1))
            )
        )
)

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

(define (check_winner_row_aux matrix row)   
    (cond 
        ((equal? row (lar_list matrix)) #f)
        ((not (check_row matrix row)) 
            (check_winner_row_aux matrix (+ row 1))
            )
        (else #t)
        )
)

(define (get_rest mat result)
    (cond 
        ((null? mat) result)
        ((null? (cdar mat)) '())
        (else
            (get_rest (cdr mat) (append result (list (cdar mat))))
        )
    )
)

(define (get_col_aux matrix result) 
    (cond 
        ((null? matrix) result)
        (else 
            (get_col_aux (cdr matrix) (append result (list (caar matrix))))
            )
        )
)

(define (get_col matrix ncol) 
    (cond 
        ((zero? ncol) (get_col_aux matrix '()))
        (else 
            (get_col (get_rest matrix '()) (- ncol 1))
            )
        )
)

(define (check_col_aux row value pos) 
    (cond 
        ((zero? pos) #t)
        ((equal? (get_val row pos) value) 
            (check_col_aux row value (- pos 1))
            )
        (else 
            #f
            )
        )
)

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

(define (check_winner_col_aux matrix col) 
    (cond 
        ((equal? col (lar_list matrix)) #f)
        ((not (check_col matrix col)) 
            (check_winner_col_aux matrix (+ col 1))
            )
        (else #t)
        )
)

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

(define (get_diag_top matrix col) 
    (cond 
        ((> col (- (lar_list (car matrix)) 3)) '())
        (else 
            (get_diag_top_aux matrix 0 col '())
            )
        )
)

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

(define (get_diag_bot matrix row) 
    (cond 
        ((< row (- (lar_list matrix) 3)) '())
        (else 
            (get_diag_bot_aux matrix row 0 '())
            )
        )
)



(define (check_winner matrix)
    (cond 
        ((check_winner_row_aux matrix 0) #t)
        ((check_winner_col_aux matrix 0) #t)
        ;((check_winner_diag_aux matrix 0) #t)
        (else #f)
        )
)


(define mat '((1 0 4) (0 3 0) (2 0 5) (6 7 8)))

(get_diag_bot mat (- (lar_list mat) 2))