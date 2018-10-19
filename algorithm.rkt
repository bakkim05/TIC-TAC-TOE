#lang racket
(require "tools.rkt")

(define (mejorPosicion matriz)
    (cond ((not(null? (ganador matriz 2))) (cadr(ganador matriz 2)))
        ((not(null? (ganador matriz 1))) (cadr(ganador matriz 1)))
        (else matriz)
        )
)

(define (ganador) #t)

(define (matSize elem num)
    (cond ((null? elem) num)
        (else (matSize (cdr elem) (+ num 1)))
    )
)

(define CERO 0)
(define ONE 1)

(define (insertAtPos xPos yPos element matriz)
    (cond 
        (
            (equal? yPos CERO) 
            (cond 
                ((equal? xPos CERO) (append (list (cons element (cdar matriz))) (cdr matriz)))

                (else 
                    (append
                        (list
                            (cons
                                (caar matriz)
                                (car 
                                    (insertAtPos
                                        (- xPos ONE)
                                        yPos
                                        element
                                        (list (cdar matriz))
                                    )
                                )
                            )
                        )
                            (cdr
                                matriz
                            )
                    )
                )
            )
        )
            (else (append (list (car matriz)) (insertAtPos xPos (- yPos ONE) element (cdr matriz)))
            )
    )
)

(define (aplicarAtPos col  row element matriz VALOR)
    (cond
        ((equal? 0 row) (append (list(aplicarAtCol col element (car matriz) VALOR)) (cdr matriz)))
        (else (append (list (car matriz)) (aplicarAtPos col  (- row 1) element (cdr matriz) VALOR)))
    )
)

(define (aplicarAtCol col element line val)
    (cond 
        ((equal? 0 col) (cons (element val (car line)) (cdr line)))
        (else (cons (car line) (aplicarAtCol (- col 1) element (cdr line) val))))
)

#|(define (algorithm matriz)
    (algorithm_aux matriz (Crear_matrix (lar_list matriz) (lar_list (car matriz)) '()) )
)|#

(define (algorithm_aux matriz mat_score)
    ;(composedScore matriz 
    (checkAndScore matriz mat_score matriz 0 0)
)

#|(define (composedScore matriz score)
    body)|#

(define (checkAndScore matrix score mToChunk row col)
    (cond 
        ((null? mToChunk) score)
        (else 
            (checkAndScore matrix (checkAndScore_2 matrix score (car mToChunk) row col) (cdr mToChunk) (+ row 1) col))
    )
)

(define (checkAndScore_2 mat score lToChunk row col)
    (cond 
        ((null? lToChunk) score)
        ((equal? (car lToChunk) 1) (checkAndScore_2 mat (simpleScore - 15 mat score row col 0) (cdr lToChunk) row (+ col 1)))
        ((equal? (car lToChunk) 2) (checkAndScore_2 mat (simpleScore + 10 mat score row col 0) (cdr lToChunk) row (+ col 1)))
        (else (checkAndScore_2 mat score (cdr lToChunk) row (+ col 1)))
    )
)

(define (simpleScore fn VAL matriz mat_score row col pos)
    (cond 
        ((equal? pos 0) 
            (simpleScore fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (- row ONE) (- col ONE)) row col (+ pos ONE)))
        ((equal? pos 1) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (- row ONE) col ) row col (+ pos ONE)))
        ((equal? pos 2) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (- row ONE) (+ col ONE) ) row col (+ pos ONE)))
        ((equal? pos 3) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score row (+ col ONE) ) row col (+ pos ONE)))
        ((equal? pos 4) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (+ row ONE) (+ col ONE) ) row col (+ pos ONE)))
        ((equal? pos 5) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (+ row ONE) col ) row col (+ pos ONE)))
        ((equal? pos 6) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score (+ row ONE) (- col ONE) ) row col (+ pos ONE)))
        ((equal? pos 7) 
            (simpleScore  fn VAL matriz (simpleScore_aux fn VAL matriz mat_score row (- col ONE) ) row col (+ pos ONE)))
        (else mat_score)
        )) 

(define (simpleScore_aux fn VALUE mat scores row col)
    (cond 
        ((or (< row 0) (< col 0)) scores)
        ((equal? (get_val_mat mat row col ) 1) scores)
        ((equal? (get_val_mat mat row col ) 2) scores )
        ((or (> (+ 1 row) (lar_list mat)) (> (+ 1 col) (lar_list (car mat)) )) scores)
        (else (aplicarAtPos row col fn scores VALUE)) 
    )
)

(algorithm_aux '((0 0 0) (0 1 0) (0 0 0)) (Crear_matrix 3 3))