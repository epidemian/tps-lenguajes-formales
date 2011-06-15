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

; Verifica si una lista contiene elementos repetidos.
(defun tiene-repetidos (l)
  (and (not (null l))
       (or (find (car l) (cdr l))
           (tiene-repetidos (cdr l)))))

; Verifica si un tablero es una solución del problema de N Reinas. Un tablero
; se representa mediante una lista de longitud n donde cada elemento es la 
; columna de una reina y su índice es la fila.
(defun es-solucion (tablero)
  ; Un tablero es solución si no hay reinas en las mismas diagonales.
  (and (not (tiene-repetidos (mapcar '+ tablero (iota (length tablero)))))
       (not (tiene-repetidos (mapcar '- tablero (iota (length tablero)))))))

; Encuentra todas las soluciones al problema de N Reinas para un N dado.
(defun n-reinas (n) 
  (remove-if-not 'es-solucion (permutaciones (iota n))))