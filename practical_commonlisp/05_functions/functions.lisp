(defun fn (params)
    "Optional documentation string"
    (format t "Hey ~a! This is the body of the function.~%" params))

(fn "sebi")

(defun op (a &optional b)
  "Function with optional param b"
  ())

(defun op-default (a &optional (b 10))
  "Function with optional param b and default 10"
  ())

(defun make-rectangle (width &optional (height width))
  "The default of optional param height is width"
  (list width height))

(format t "~a~%"(make-rectangle 10))
(format t "~a~%"(make-rectangle 10 20))



(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

(format t "is c supplied? ~a no!~%" (foo 1 2))
(format t "is c supplied? ~a yeah!~%" (foo 1 2 5))


;;;; &rest parameter - variadic arguments 
;; (defun format (stream string &rest values) ...)
;; (defun + (&rest numbers) ...)



(defun foo-key (&key a b c) (list a b c))
(format t "~%~%===== key params =====~%")
(format t "(foo-key :a 1)      => ~a~%" (foo-key :a 1))
(format t "(foo-key :b 1)      => ~a~%" (foo-key :b 1))
(format t "(foo-key :c 1)      => ~a~%" (foo-key :c 1))
(format t "(foo-key :b 1 :c 2) => ~a~%" (foo-key :b 1 :c 2))
(format t "~a~%" (foo-key :a 1 :b 2 :c 4))

(defun foo-return-from (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo-return-from (list i j))))))
(format t "~a~%" (foo-return-from 10))

(function foo)
#'foo

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(plot #'exp 0 4 1/2)

(let ((plot-data (list 0 3 1/2))) 
  (apply #'plot #'exp plot-data))

(plot #'(lambda (x) (* 2 x)) 0 10 1)

;; Lexical Variable and Closures
(let ((count 0)) #'(lambda () (setf count (1+ count))))
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))

(format t "fn: ~a~%" (funcall *fn*))
(format t "fn: ~a~%" (funcall *fn*))
(format t "fn: ~a~%" (funcall *fn*))

;;;; list of functions that increments, decrements and returns count
;;;; count is within a clousure 
(list
  #'(lambda () (incf count))
  #'(lambda () (decf count))
  #'(lambda () count))

(format t "~a~%" 
  (let ((count 10 ))
    (dolist 
        (x (list
          #'(lambda () (incf count))
          #'(lambda () (decf count))
          #'(lambda () count)))
        (format t "~a~%" (funcall x)))))

;;;; Global variables
;;;; defvar
;; defvar only assigns init value if the variable is undefined.
;; it also can be defined without giving initial value.
(defvar *count-var* 0
  "Count of widgets made so far")
(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

(with-open-file 
    (logfile "lisp.log" 
         :direction :output 
         :if-exists :supersede)
  ;; here global var stdout is rebind to logfile
  ;; but only within that let bind
  (let ((*standard-output* logfile))
    (format t "~a%~%" "[INFO] Logging into file.")))

(format t "~a~%" "[INFO] This log goes to stdout.")

;; see also logging.lisp
