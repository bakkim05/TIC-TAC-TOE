#lang racket

(require "MatrixManagment.rkt")
(require "Game.rkt")

;Lo que hace es sustituir temporalmente una posición vacía con el fin de contar las filas, columnas y diagonales que tiene disponibles a futuro como posibilidad de gane, en ese instante.
(define (listaProbAux matrix i j lista)
  (cond ((equal? i (len (car matrix))) (listaProbAux matrix 0 (+ j 1) lista))
        ((equal? j (len matrix)) (reverse lista))
        ((not (or (equal? (get_element matrix j i) 1) (equal? (get_element matrix j i) 2)))
         (listaProbAux matrix (+ i 1) j (cons (list (- (posibilidades (replace_matrix matrix j i 2) 1) (posibilidades (replace_matrix matrix j i 2) 2)) i j) lista)))
        (else
         (listaProbAux matrix (+ i 1) j lista))))

;Crea una lista con tripletas que contienen (probabilidad i j)
(define (listaProb matrix)
  (listaProbAux matrix 0 0 '()))

;Auxiliar que hace todo el brete, lo que hace es buscar cual de las posibilidades calculadas es más negativa.
(define (mejorPosAux ele lista)
  (cond ((null? lista) ele)
        ((< (caar lista) (car ele))
         (mejorPosAux (car lista) (cdr lista)))
        (else
         (mejorPosAux ele (cdr lista)))))

;Busca en la lista de probabilidad y posiciones, la tripleta más conveniente para la pc.
(define (mejorPos lista)
  (cond ((null? lista) #f)
        (else
         (mejorPosAux (car lista) (cdr lista)))))



;verifica si en alguna posición gana el Usuario, esto sustituyendo temporalmente un espacio y preguntando si ganó el usuario o pc respectivamente, luego devuelve esa posición, si alguno gana en ella.
(define (someoneWinAux i j matrix num)
  (cond ((equal? i (len (car matrix))) (someoneWinAux 0 (+ j 1) matrix num))
        ((equal? j (len matrix)) '())
        ((not (or (equal? (get_element matrix j i) 1) (equal? (get_element matrix j i) 2)))
         (cond ((win? (replace_matrix matrix j i num) num)
                (list i j))
               (else
                (someoneWinAux (+ i 1) j matrix num))))
        (else
          (someoneWinAux (+ i 1) j matrix num))))


(define (someoneWin? matrix num)
  (someoneWinAux 0 0 matrix num))


(define (greedyAlgorithmnAux matrix lista)
  (replace_matrix matrix (cadr lista) (car lista) 2))

;algoritmo principal donde primero se busca el espacio donde puede ganar la pc para colocar la ficha, sino, pregunta lo mismo para el usuario, para matar una jugada
;si no, calcula la mayor probabilidad de movimientos posibles en el instante para colocar la ficha en ese lugar.
(define (greedyAlgorithmn matrix)
  (cond((not(null? (someoneWin? matrix 2)))
        (greedyAlgorithmnAux matrix (someoneWin? matrix 2)))
        ((not(null? (someoneWin? matrix 1)))
        (greedyAlgorithmnAux matrix (someoneWin? matrix 1)))
       (else
        (greedyAlgorithmnAux matrix (cdr(mejorPos (listaProb matrix)))))))

(define (greedy matrix)
  (greedyIA matrix (greedyAlgorithmn matrix)))

(define (greedyIA matrix result)
  matrix
  result
  (append (list (findDiff matrix result 0 0)) (list result)))

(define (findDiff mat1 mat2 row col)
  (cond ((null? (findDiffList (car mat1) (car mat2) row col )) (findDiff (cdr mat1)( cdr mat2) (+ row 1) col))
  (else (findDiffList (car mat1) (car mat2) row col))))

(define (findDiffList list1 list2 row col)
  (cond ((null? list1) '())
    ((equal? (car list1) (car list2)) (findDiffList (cdr list1) (cdr list2) row (+ 1 col)))
    (else (list row col))))



(greedy  '((0 0 0) (0 0 1)))
(provide (all-defined-out))