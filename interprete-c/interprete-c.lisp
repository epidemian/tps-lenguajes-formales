(defparameter *mapa-operadores* 
  '((+= +) (-= -) (*= *) (/= /) (%= %) (++ +) (-- -)))

(defparameter *mapa-pesos*
  '((== 1) (!= 1) (< 2) (> 2) (<= 2) (>= 2) (+ 3) (- 3) (* 4) (/ 5) (% 6)))

; Retorna el operador binario asociado a un operador de asignación.
; Por ejemplo (buscar-operador '*=) -> *
(defun buscar-operador (op)
  (cadr (find op *mapa-operadores* :key 'car)))

; Verifica si un símbolo es un operador en C.
(defun es-operador (op)
  (member op *mapa-pesos* :key 'car))

; Retorna el peso de un operador en C. Mientras más grande su peso, mayor su 
; precedencia.
(defun peso-operador (op)
  (cadr (find op *mapa-pesos* :key 'car)))

; Verifica si un símbolo es una variable en una memoria dada.
(defun es-variable (var mem)
  (member var mem :key 'car))

; Una condición de error para las variables no declaradas.
(define-condition variable-no-declarada (error)
  (var :initarg :text :reader var))

; Devuelve la memoria que resulta de asignar una variable a una memoria dada.
(defun asignar (var valor mem)
  (if (not (es-variable var mem))
      (error 'variable-no-declarada :var var))
  (cons (list var valor) (remove var mem :key 'car)))

; Agrega una nueva variable a una memoria dada.
(defun agregar-variable (var mem)
  (cons (list var) mem))

; Ejecuta un programa en pseudo-C
(defun run(prg ent &optional(mem nil))
  (if (null prg) nil
      (if (eq (caar prg) 'int) 
          (run (cdr prg) ent (agregar-variable (nth 1 (car prg)) mem))
          (if (eq (caar prg) 'main) 
              (ejec (nth 1 (car prg)) ent mem ) 
              'no-hay-main))))

(defun ejec (prg ent mem &optional (sal nil))
  (if (null prg) sal
      (cond
        ; printf: Ejecuta el resto del programa modificando la salida
        ((eq (caar prg) 'printf) 
         (ejec (cdr prg) ent mem (append sal (list (valor (cdar prg) mem)))))
        ; scanf: Ejecuta el rest del programa sacando el primer valor de la 
        ; entrada y asociandolo a una variable en memoria.
        ((eq (caar prg) 'scanf) 
         (ejec (cdr prg) (cdr ent) 
               (asignar (nth 1 (car prg)) (car ent) mem) sal))
        ; Asignaciones.
        ((es-variable (caar prg) mem)
         (if (eq (nth 1 (car prg)) '=)
             ; Ejecuta el resto del programa modificando la memoria.
             (ejec (cdr prg) ent 
                   (asignar (caar prg) (valor (cddar prg) mem) mem) sal)
             ; Transforma las sentencias a += b en a = a + b 
             ; Y a++ en a = a + 1
             (ejec (cons (list (caar prg) '= (caar prg) 
                               (buscar-operador (nth 1 (car prg)))
                               (if (member (nth 1 (car prg)) '(+= -= *= /= %=)) 
                                   (cddar prg) 1)) 
                         (cdr prg)) 
                   ent mem sal)))
        ; Operadores pre-inrecmento/decremento. Los tranforma en post-inc/dec.
        ((member (caar prg) '(++ --))
         (ejec (cons (reverse (car prg))(cdr prg)) ent mem sal))
        ; If: agrega las sentencias correspondientes al comienzo del resto del 
        ; programa.
        ((eq (caar prg) 'if)
         (ejec (append
                (if (eq (valor (nth 1 (car prg)) mem ) 0)
                    (if (eq (length (car prg)) 5) (nth 4 (car prg)) nil)
                    (nth 2 (car prg)))
                (cdr prg))
               ent mem sal))
        ; While: si la condición se cumple vuelve a ejecutar todo el programa 
        ; más las sentencias en el while. Si no, ejecuta el resto del programa.
        ((eq (caar prg) 'while) 
         (if (eq (valor (nth 1 (car prg)) mem) 0)
             (ejec (cdr prg) ent mem sal)
             (ejec (append (nth 2 (car prg)) prg) ent mem sal))))))

; Devuelve el valor de una variable en una memoria dada.
(defun valor-memoria (var mem)
  (if (not (es-variable var mem))
      (error 'variable-no-declarada :var var))
  (cadr (find var mem :key 'car)))

; Retorna una función booleana estilo C a partir de un operador booleano de 
; Lisp.
(defun funcion-booleana (op)
  (lambda (x y) (if (funcall op x y) 1 0)))

; Devuelve una función válida de Lisp a partir de un operador binario de C.
(defun operador-a-funcion (op)
  (cond
    ((member op '(< > <= >=)) (funcion-booleana op))
    ((eq op '==) (funcion-booleana 'eq))
    ((eq op '!=) (funcion-booleana (lambda (x y) (not (eq x y)))))
    ((eq op '%) 'mod)
    (t op)))

; Devuelve los operandos que resultan de ejecutar el primer operador de 
; operadores sobre los dos primeros operandos de operandos.
(defun operar (mem operadores operandos)
  (cons (apply (operador-a-funcion (car operadores)) 
               (list (valor (nth 1 operandos) mem) 
               (valor (nth 0 operandos) mem)))
        (cddr operandos)))

; Devuelve el valor de una expresión para una memoria dada.
(defun valor (expresion mem &optional(operadores nil) (operandos nil))
  (if (atom expresion)
      (if (not (null expresion))
          (if (numberp expresion) expresion (valor-memoria expresion mem))
          (if (null operadores)
              (valor (car operandos) mem)
              (valor expresion mem (cdr operadores)
                     (operar mem operadores operandos))))
      (if (es-operador (car expresion)) 
          (if (null operadores)
              (valor (cdr expresion) mem (cons (car expresion) operadores) 
                     operandos)
              (if (< (peso-operador (car operadores)) 
                     (peso-operador (car expresion)))
                  (valor (cdr expresion) mem 
                         (cons (car expresion) operadores) operandos)
                  (valor (cdr expresion) mem 
                         (cons (car expresion) (cdr operadores))
                         (operar mem operadores operandos))))
          (valor (cdr expresion) mem operadores 
                 (cons (car expresion) operandos)))))