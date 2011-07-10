; Verifica si dos nodos a y b son iguales.
(defun nodos-iguales (a b)
  (or (equalp a b) 
      (and (listp a) (listp b) (equalp a (reverse b)))))

; Devuelve los nodos vecinos de un nodo en un grafo dado.
(defun vecinos (nodo grafo)
  (cadr (find nodo grafo :key 'car :test 'nodos-iguales)))

; Devuelve todas las posibles "expansiones" de una trayectoria dada agregando 
; los vecinos del primer nodo que aún no formen parte de la misma.
(defun expandir-trayectoria (trayectoria grafo)
  (mapcar (lambda (vecino) (cons vecino trayectoria))
          (set-difference (vecinos (car trayectoria) grafo) trayectoria)))

; Devuelve un camino que conecta los nodos inicio y fin en un grado dado, o nil 
; en caso de no existir camino alguno.
(defun gps (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (if (null trayectorias) nil
      (if (nodos-iguales (caar trayectorias) fin)
          (reverse (car trayectorias))
          (gps inicio fin grafo 
               (append (expandir-trayectoria (car trayectorias) grafo)
                       (cdr trayectorias))))))

; Devuelve un camino mínimo que conecta los nodos inicio y fin en un grado dado,
; o nil en caso de no existir camino alguno.
(defun gps-min (inicio fin grafo &optional (trayectorias (list (list inicio))))
  (if (null trayectorias) nil
      (or (reverse (find fin trayectorias :key 'car :test 'nodos-iguales))
          (gps-min inicio fin grafo
                   (mapcan (lambda (tr) (expandir-trayectoria tr grafo))
                           trayectorias)))))

; La representación del grafo no conexo:
; 	a--c--f     
;	|    / \    h--i
;	b---d---g    \
;	 \     /      j
;         `-e-´
(defparameter *grafo-simple* 
  '((a (b c)) (b (a d e)) (c (a f)) (d (b f g)) (e (b g)) (f (c d g)) 
    (g (d e f)) (h (i j)) (i (h)) (j (h))))

(defun agregar-arista (arista grafo)
  (if (member (cadr arista) (vecinos (car arista) grafo) :test 'nodos-iguales) 
      grafo
      (cons (list (car arista) 
                  (cons (cadr arista) (vecinos (car arista) grafo)))
            (remove-if (lambda (x) (nodos-iguales (car arista) (car x)))
                       grafo))))

(defun crear-grafo (aristas &optional (grafo-inicial nil))
  (if (null aristas) grafo-inicial
      (crear-grafo (cdr aristas) 
                   (agregar-arista (car aristas) grafo-inicial))))

(defun calle (esquinas) 
  (if (< (length esquinas) 2) nil
      (cons (list (car esquinas) (cadr esquinas))
            (calle (cdr esquinas)))))

(defun mapa-facu () 
  (let* (
         ; Calles.
         (mx "Mexico") (ch "Chile") (in "Independencia") (eu "Estados Unidos") 
         (pe "Peru") (bo "Bolivar") (de "Defensa") (ba "Balcarce") 
         (pc "Paseo Colon") (az "Azopardo")
         ; Esquinas.
         (n1 (list mx pe))  (n2 (list mx bo))  (n3 (list mx de))
         (n4 (list mx ba))  (n5 (list mx pc))  (n6 (list mx az))
         (n7 (list ch pe))  (n8 (list ch bo))  (n9 (list ch de))
         (n10 (list ch ba)) (n11 (list ch pc)) (n12 (list ch az))
         (n13 (list in pe)) (n14 (list in bo)) (n15 (list in de))
         (n16 (list in pc)) (n17 (list in az)) (n18 (list eu pe))
         (n19 (list eu bo)) (n20 (list eu de)) (n21 (list eu pc))
         (n22 (list eu az)))
    (crear-grafo (append (calle (list n1 n2 n3 n4 n5 n6))
                         (calle (list n7 n8 n9 n10 n11 n12))
                         (calle (list n17 n16 n15 n14 n13))
                         (calle (list n18 n19 n20 n21 n22))
                         (calle (list n18 n13 n7 n1))
                         (calle (list n2 n8 n14 n19))
                         (calle (list n20 n15 n9 n3))
                         (calle (list n4 n10))
                         (calle (list n5 n11 n16 n21))
                         (calle (list n21 n16 n11 n5))
                         (calle (list n22 n17 n12 n6)))))) 
