#lang racket
(require racket/include)
(include "tools.rkt")
(define (mejorPosicion matriz)
    (cond ((not(null? (ganador matriz 2))) (cadr(ganador matriz 2)))
        ((not(null? (ganador matriz 1))) (cadr(ganador matriz 1)))
        (else (rand matriz))
        )
)

(define (ganador) #t)

(define (rand matriz)
    (insertAtPos (rand (+ 1 (matSize (car matriz) 0)
                    )
            ) 
            (rand (+ 1 (matSize matriz 0
                        )
                    )
            )
    )
)

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
            (= yPos CERO) 
            (cond 
                ((= xPos CERO) (append (list (cons element (cdar matriz))) (cdr matriz)))

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

(define VALOR 10)
(define (aplicarAtPos xPos yPos element matriz)
    (cond 
        (
            (= yPos CERO) 
            (cond 
                ((= xPos CERO) (append (list (cons (element (caar matriz) VALOR) (cdar matriz))) (cdr matriz)))

                (else 
                    (append
                        (list
                            (cons
                                (caar matriz)
                                (car 
                                    (aplicarAtPos
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
            (else (append (list (car matriz)) (aplicarAtPos xPos (- yPos ONE) element (cdr matriz)))
            )
    )
)

(insertAtPos 1 2 5 '((1 1 1) (2 2 2) (3 3 3)))
