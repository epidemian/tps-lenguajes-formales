; Devuelve las permutaciones de l que cumplan con el predicado dado. 
(defun filtrar-permutaciones (predicado l &optional (inicial nil))
  (if (null l)
      (if (funcall predicado inicial) (list inicial) nil)
      (mapcan (lambda (elem)
                (filtrar-permutaciones predicado (remove elem l) 
                                       (cons elem inicial)))
              l)))

; Encuentra todas las soluciones al problema de N Reinas para un N dado.
(defun n-reinas (n) 
  (filtrar-permutaciones 'es-solucion (iota n)))