#lang racket

(define (Crear_row cols result) 
    (cond 
        ((zero? cols) result)
        (else (Crear_row (- cols 1) (append result (list 0))))
        )
)

(define (Crear_matriz row cols result)
    (cond 
        ((zero? row) result)
        (else 
            (Crear_matriz (- row 1) cols (append result (list (Crear_row cols '())))
                )
            )
        )
)


(define (compararMatrix_val row col matrix value)
    (cond 
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

(define mat '((3 2 1) (2 3 1) (1 2 3)))

(compararMatrix_val 2 2 mat 3)
