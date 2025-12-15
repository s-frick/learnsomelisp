(defmacro with-logging ((file) &body body)
  `(with-open-file (logfile ,file
                           :direction :output
                           :if-exists :supersede)
    (let ((*standard-output* logfile))
      ,@body)))

(defmacro with-timing (&body body)
  `(let ((start (get-internal-real-time)))
     (unwind-protect
          (progn ,@body)
       (let* ((end   (get-internal-real-time))
              (diff  (- end start))
              (ms    (* 1000.0 (/ diff internal-time-units-per-second))))
         (format t "[TIME] ~,3f ms~%" ms)))))

(defun program ()
  (format t "~a~%" "[INFO] Program started.")
  (sleep 1)
  (format t "~a~%" "[INFO] Log some useful stuff."))

(defun main () 
  (with-logging ("lisp.log") 
    (with-timing (program))))

(main)

