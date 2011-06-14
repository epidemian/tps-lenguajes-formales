;;;; Created on 2011-06-14 01:47:55

; Calcula todas las permutaciones de una lista. La lista no debe tener 
; elementos repetidos.
; Ejemplo:
; > (permutaciones '(a b c)))
; ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
(defun permutaciones (l)
  (if (null l) 
      '(())
      ; Por cada elemento de l, genero una lsita de todas las permutaciones de l
      ; con ese elemnto en la cabeza.
      (mapcan (lambda (elem)
                (mapcar (lambda (perm) (cons elem perm)) 
                        (permutaciones (remove elem l))))
              l)))

; Genera una secuencia de enteros consecutivos de tamaño n comenzando por el 
; valor inicio.
; Ejemplo:
; > (iota 5 1)
; (1 2 3 4 5)
(defun iota (n &optional (inicio 0)) 
  (if (zerop n) nil 
      (cons inicio (iota (- n 1) (+ inicio 1)))))

#|(defun reina-no-choca (reina otras-reinas &optional (fila 1))
   (or (null otras-reinas)
       (and (not (equal reina (+ (car otras-reinas) fila)))
            (not (equal reina (- (car otras-reinas) fila))) 
            (reina-no-choca reina (cdr otras-reinas) (+ fila 1)))))

(defun es-solucion (tablero)
  (or (null tablero) 
      (and (reina-no-choca (car tablero) (cdr tablero)) 
           (es-solucion (cdr tablero)))))|#

(defun hay-repetidos (l)
  (and (not (null l))
       (or (find (car l) (cdr l))
           (hay-repetidos (cdr l)))))

(defun es-solucion (tablero)
  (and (not (hay-repetidos (mapcar '+ tablero (iota (length tablero)))))
       (not (hay-repetidos (mapcar '- tablero (iota (length tablero)))))))

(defun n-reinas (n)
  (remove-if-not 'es-solucion (permutaciones (iota n))))
