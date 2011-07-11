
; Genera una secuencia de enteros consecutivos de tamaño n comenzando por el 
; valor inicio. Por ejemplo (iota 5 1) -> (1 2 3 4 5)
(defun iota (n &optional (inicio 0)) 
  (if (zerop n) nil 
      (cons inicio (iota (- n 1) (+ inicio 1)))))

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