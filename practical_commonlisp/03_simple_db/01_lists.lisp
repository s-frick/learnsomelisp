;; property list with keyword symbols
(list :a 1 :b 2 :c 3)

;; get props from list by name
(getf (list :a 1 :b 2 :c 3) :c)


;;; DATABASE
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; global variable *db*
(defvar *db* nil)

(defun add-record (cd) (push cd *db*))
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; prompt
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")
  ))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist) 
  (remove-if-not 
    #'(lambda (cd) (equal (getf cd :artist) artist))
  *db*))

(defun select(selector-fn)
    (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; abstract it with macro?
; (defun where (&key title artist rating (ripped nil ripped-p))
;   #'(lambda (cd)
;       (and
;         (if title    (equal (getf cd :title) title)   t)
;         (if artist   (equal (getf cd :artist) artist) t)
;         (if rating   (equal (getf cd :rating) rating) t)
;         (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; update with where clause
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar 
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title (setf (getf row :title) title))
                (if artist (setf (getf row :artist) artist))
                (if rating (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

;; remove
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; macro - 01 - reversing code
(defmacro backwards (expr) (reverse expr))
;; (backwards ("hello, world" t format))
;; expands to:
;; (format t "hello, world")
;; outputs when evaluated: "hello, world"


;; (equal (getf cd field) value)
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; (make-comparison-expr :rating 10) => (equal (getf cd :rating) 10)

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

;; (make-comparisons-list (list :rating 10 :title "Title")) 
;;    => ((EQUAL (GETF CD :RATING) 10) (EQUAL (GETF CD :TITLE) "Title"))

;; variant of , is ,@
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

'(where :title "Give Us a Break" :ripped t)
;; ,@ kinda unwraps the following list
;; `(and ,(list 1 2 3)) => (AND (1 2 3))
;; `(and ,@(list 1 2 3)) => (AND 1 2 3)
;; `(and ,@(list 1 2 3) 4)



;; keyword parameters - bind call args to corresponding values
; (defun foo (&key a b c) (list a b c))
; (foo :a 1 :c 3) ;; ==> (1 NIL 3) | NIL is bound to b, because no value was given for b

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
(foo :a 1 :c 3) ;; ==> (1 20 3 T) | now b is 20 (default value)
(foo) ;; ==> (1 20 30 NIL) | now b is 20 (default value), c is default to 30 and c-p (supplied-p parameter) is nil=false


;;;; Operate on DB

;; fill in some cds
; (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
; (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
; (add-record (make-cd "Home" "Dixie Chicks" 9 t))

;; dump the db
; (dump-db)
; (save-db "./cds.db")
; (load-db "./cds.db")



;;;; Main
