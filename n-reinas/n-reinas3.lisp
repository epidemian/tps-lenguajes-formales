; Devuelve las permutaciones de l que cumplan con el predicado dado. 
(defun filtrar-permutaciones-parciales (predicado l &optional (inicial nil))
  (if (funcall predicado inicial)
      (if (null l)
          (list inicial)
          (mapcan (lambda (elem)
                    (filtrar-permutaciones-parciales 
                     predicado (remove elem l) (cons elem inicial)))
                  l))
      nil))

; Encuentra todas las soluciones al problema de N Reinas para un N dado.
(defun n-reinas (n) 
  (filtrar-permutaciones-parciales 'es-solucion (iota n)))