; Calcula las permutaciones de una lista. La lista no debe tener elementos 
; repetidos. Por ejemplo (permutaciones '(a b c))) ->
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

; Encuentra todas las soluciones al problema de N Reinas para un N dado.
(defun n-reinas (n) 
  (remove-if-not 'es-solucion (permutaciones (iota n))))