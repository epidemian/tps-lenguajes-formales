; Devuelve los vecinos de un nodo en un grafo dado.
(defun vecinos (nodo grafo)
  (cadr (find nodo grafo :key 'car :test 'equal)))

(defun expandir-trayectoria (trayectoria grafo)
  (mapcar (lambda (vecino) (cons vecino trayectoria))
          (set-difference (vecinos (car trayectoria) grafo) trayectoria)))

; Devuelve un camino que conecta los nodos inicio y fin en un grado dado, o nil 
; en caso de no existir camino alguno.
(defun gps (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (if (null trayectorias) nil
      (if (equal (caar trayectorias) fin)
          (reverse (car trayectorias))
          (gps inicio fin grafo
               (append (expandir-trayectoria (car trayectorias) grafo)
                       (cdr trayectorias))))))

; Devuelve un camino mínimo que conecta los nodos inicio y fin en un grado dado,
; o nil en caso de no existir camino alguno.
(defun gps-min (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (if (null trayectorias) nil
      (or (reverse (find fin trayectorias :key 'car :test 'equal))
          (gps-min inicio fin grafo
                   (mapcan (lambda (tr) (expandir-trayectoria tr grafo))
                           trayectorias)))))

; La representación del grafo:
; 	a--c--f     h--i
;	\    / \    \
;	b---d--g    j
;	\     /
;       `-e--´
(defparameter *grafo-simple* 
  '((a (b c)) (b (a d e)) (c (a f)) (d (b f g)) (e (b g)) (f (c d g)) 
  (g (d e f)) (h (i j)) (i (h)) (j (h)))) 