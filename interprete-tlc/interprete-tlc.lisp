; Devuelve el valor de una variable en un ambiente dado.
(defun valor (nombre ambiente)
  (cadr (find nombre ambiente :key 'car)))

; Verifica si una función de TLC Lisp es directamente aplicable en Common Lisp 
; sin tener que hacer ninguna traducción rara.
(defun es-funcion-aplicable (funcion)
  (member funcion '(car cdr cons list append + - * / < > eq atom null listp 
                    numberp length)))

; Evalúa una expresión TLC Lisp en un ambiente dado. Por ejemplo:
; (evaluar '(cons x y) '((x 1) (y (2 3)))) -> (1 2 3)
(defun evaluar (expresion ambiente)
  (if (atom expresion)
      (if (numberp expresion)
          expresion
          (valor expresion ambiente))
      (cond
        ; Funciones que evalúan solo algunos de sus argumentos.
        ((eq (car expresion) 'quote) (cadr expresion))
        ((eq (car expresion) 'and) (and (evaluar (nth 1 expresion) ambiente)
                                        (evaluar (nth 2 expresion) ambiente)))
        ((eq (car expresion) 'or) (or (evaluar (nth 1 expresion) ambiente)
                                      (evaluar (nth 2 expresion) ambiente)))
        ((eq (car expresion) 'if) (if (evaluar (nth 1 expresion) ambiente)
                                      (evaluar (nth 2 expresion) ambiente)
                                      (evaluar (nth 3 expresion) ambiente)))
        ; Funciones que evalúan todos sus argumentos.
        (t (aplicar (car expresion) 
                    (mapcar (lambda (x) (evaluar x ambiente)) (cdr expresion)) 
                    ambiente)))))

(defun aplicar (funcion argumentos ambiente)
  (if (atom funcion)
      (cond
        ; nth se comporta distinto en TLC.
        ((eq funcion 'nth) (nth (- (cadr argumentos) 1) (car argumentos)))
        ; Si es una función que se comporta igual en CL, la aplica directamente.
        ((es-funcion-aplicable funcion) (apply funcion argumentos))
        ; Es una función definida en el ambiente.
        (t (aplicar (valor funcion ambiente) argumentos ambiente)))
      ; Si no es un átomo es una función lambda. Evalúa su cuerpo en el ambiente 
      ; que resulta de asociar sus parámetros a los argumentos mas el ambiente
      ; anterior.
      (evaluar (nth 2 funcion) 
               (append (mapcar 'list (nth 1 funcion) argumentos) ambiente))))
