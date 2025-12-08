(defun main () 
  "Main"
  (read-cmd))

; (defmacro case-str (&body body)
;   `(let ((value v))
;      (cond ((equal value ,@body)))))
;
; (case-str)
;
; (let ((value value))
;   (cond ((equal value '"greet"))))

(defun log-err ()
  (format t "Usage: ./hello greet~%"))

(defun greet-cmd ()
  (format t "Hello, ~a!~%" (prompt-cmd "Your name? :> ")))

(defun bye-cmd ()
  (format t "Bye bye, ~a.~%" (prompt-cmd "Your name? :> ")))

(defun which-cmd (s)
  "Gives you back which command was selected"
  (cond 
    ((string-equal "greet" s) (greet-cmd) t)
    ((string-equal "bye" s)   (bye-cmd)   t)
    ((string-equal "exit" s)  nil)
    (t                        (log-err)   t))
  )

(defun prompt-cmd (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-cmd () 
  "read a command"
  (loop while (which-cmd (prompt-cmd "> "))))

(main)
