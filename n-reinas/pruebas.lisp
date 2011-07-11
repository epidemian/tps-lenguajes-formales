(load "n-reinas")
(defparameter *valores-iterativos* nil)
(defparameter *valores-recursivos* nil)

(progn
  (dotimes (i 5)
    (format t "i: ~a ~%" i)
    (push (list (+ i 4) (- (- (get-internal-real-time) 
                              (progn (n-reinas (+ i 4)) (get-internal-real-time)))))
 *valores-iterativos* ))

  (dotimes (i 5)
    (format t "i: ~a ~%" i)
    (push (list (+ i 4) (- (- (get-internal-real-time) 
                              (progn (n-reinas (+ i 4)) (get-internal-real-time)))))
 *valores-recursivos*))

  (setf *valores-iterativos* (nreverse *valores-iterativos*))
  (setf *valores-recursivos* (nreverse *valores-recursivos*)))

(adw-charting:with-chart (:bar 800 600)
  (adw-charting:add-series "Reinas Iterativo" *valores-iterativos*)
  (adw-charting:add-series "Reinas Recursivo" *valores-recursivos*)
  (adw-charting:set-axis :y "Tiempo (ms)")
  (adw-charting:set-axis :x "Cantidad de reinas" :data-interval 1)
  (adw-charting:save-file "vecto-reinas.png"))

(adw-charting:with-chart (:bar 800 600)
  (adw-charting:add-series "Reinas Iterativo"
  (mapcar (lambda (x) (list (car x) (log (if (= 0 (cadr x))
     1
     (cadr x))))) *valores-iterativos*))
  (adw-charting:add-series "Reinas Recursivo"
  (mapcar (lambda (x) (list (car x) (log (if (= 0 (cadr x))
     1
     (cadr x))))) *valores-recursivos*))
  (adw-charting:set-axis :y "Log(Tiempo (ms))")
  (adw-charting:set-axis :x "Cantidad de reinas" :data-interval 1)
  (adw-charting:save-file "vecto-reinas-log.png"))