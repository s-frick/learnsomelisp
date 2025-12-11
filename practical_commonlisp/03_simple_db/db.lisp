(defvar *db* nil)

(defun db-insert (cd) (push cd *db*))

(defun db-dump ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%}" cd)))

(defun db-save (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun db-load (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun db-commit (filename)
  (db-save)
  (db-load))

(defun db-select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun db-update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar 
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title (setf (getf row :title) title))
                (if artist (setf (getf row :artist) artist))
                (if rating (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

(defun mk-set-expr (field value)
  `(if ,value (setf (getf row ,field) ,value)))

(defun mk-cmp-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun mk-expr-list (fn &rest fields)
  (loop while fields
        collecting (funcall fn (pop fields) (pop fields))))

(mk-expr-list #'mk-set-expr :rating 10 :title "Title")

(defmacro db-where (&rest clauses)
  `#'(lambda (cd) (and ,@(mk-expr-list #'mk-cmp-expr clauses))))

'(db-where :title "Give Us a Break" :ripped t)
(db-select (db-where :title "Give Us a Break" :ripped t))

