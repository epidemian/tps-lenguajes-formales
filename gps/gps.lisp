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
;    a--c--f     
;    |    / \    h--i
;    b---d---g    \
;     \     /      j
;      `-e-´
(defparameter *grafo-simple* 
  '((a (b c)) (b (a d e)) (c (a f)) (d (b f g)) (e (b g)) (f (c d g)) 
    (g (d e f)) (h (i j)) (i (h)) (j (h))))

; Devuelve el grafo que resulta de agregar una arista a un grafo dado. Una 
; arista es una lista (n1 n2) y representa una conexión desde el nodo n1 al nodo
; n2. Ejemplo: (agregar-arista '(a c) '((a (b)) (b (c)))) -> ((a (c b)) (b (c)))
(defun agregar-arista (arista grafo)
  (if (member (cadr arista) (vecinos (car arista) grafo) :test 'nodos-iguales) 
      grafo
      (cons (list (car arista) 
                  (cons (cadr arista) (vecinos (car arista) grafo)))
            (remove-if (lambda (x) (nodos-iguales (car arista) (car x)))
                       grafo))))

; Crea un grafo a partir de una lista de aristas. Ejemplo:
; (crear-grafo '((a b) (b c))) -> ((b (c)) (a (b)))
(defun crear-grafo (aristas &optional (grafo-inicial nil))
  (if (null aristas) grafo-inicial
      (crear-grafo (cdr aristas) 
                   (agregar-arista (car aristas) grafo-inicial))))

; Crea una lista de aristas a partir del nombre de una calle y sus 
; intersecciones. Ejemplo (crear-calle "A" '("B" "C" "D")) ->
; ((("A" "B") ("A" "C")) (("A" "C") ("A" "D")))
(defun crear-calle (calle intersecciones) 
    (if (< (length intersecciones) 2) nil
        (cons (list (list calle (car intersecciones)) 
                    (list calle (cadr intersecciones)))
              (crear-calle calle (cdr intersecciones))))) 

(defparameter *mapa-facu*
  (let* ((mx "Mexico") (ch "Chile") (in "Independencia") (eu "Estados Unidos") 
         (pe "Peru") (bo "Bolivar") (de "Defensa") (ba "Balcarce") 
         (pc "Paseo Colon") (az "Azopardo"))
    (crear-grafo (append (crear-calle mx (list pe bo de ba pc az))
                         (crear-calle ch (list pe bo de ba pc az))
                         (crear-calle in (list az pc de bo pe))
                         (crear-calle eu (list pe bo de pc az))
                         (crear-calle pe (list eu in ch mx))
                         (crear-calle bo (list mx ch in eu))
                         (crear-calle de (list eu in ch mx))
                         (crear-calle ba (list mx ch))
                         (crear-calle pc (list mx ch in eu))
                         (crear-calle pc (list eu in ch mx))
                         (crear-calle az (list eu in ch mx)))))) 

; Dada una calle y una esquina (una lista con dos calles) devuelve la calle de
; la esquina que no es la calle dada. Ejemplo: 
; (otra-calle-en-equina "A" ("B" "A")) -> "B"
(defun otra-calle (calle esquina)
  (find-if (lambda (x) (not (equalp calle x))) esquina))

; Imprime un camino en un formato legible.
(defun imprimir-camino (camino)
  (if (> (length camino) 1)
      (imprimir-camino-desde camino 
                             (car (intersection (car camino) (cadr camino) 
                                                :test 'equalp)))))

(defun imprimir-camino-desde (camino calle-anterior)
  (if camino 
      (if (member calle-anterior (cadr camino) :test 'equalp)
          ; Sigue por la misma calle.
          (imprimir-camino-desde (cdr camino) calle-anterior)
          ; Agarra una calle nueva.
          (progn  
            (format t "Tomar ~s hasta ~s~%" calle-anterior 
                    (otra-calle calle-anterior (car camino)))
            (imprimir-camino-desde (cdr camino) 
                                   (otra-calle calle-anterior (car camino)))))))

