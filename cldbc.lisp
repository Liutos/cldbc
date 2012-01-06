(defpackage :cldbc
  (:use :cl :cl-mysql)
  (:export :get-connection
	   :select :select-fn
	   :map-row
	   :next-row :insert :insert-fn
	   :update-fn :delete-fn))

(in-package :cldbc)

(defclass result ()
  ((schema :initarg :schema)
   (content :initarg :content
	    :reader result-content)
   (pointer :initarg :pointer :initform 0
	    :accessor result-pointer))
  (:documentation "A result of a query action. Query includes select, insert, update and delete."))

(defun get-connection (user password database)
  "Connect to the database by means of the USER name, PASSWORD and access DATABASE."
  (declare (string user password database))
  (connect :user user :password password :database database))

(defun mapend (fn list &optional not-end-action)
  "Call function FN with each element in proper LIST. If the element is not the last one, call the NOT-END-ACTION on it every time before calling the next one."
  (declare (function fn)
	   (list list))
  (when list
    (funcall fn (car list))
    (if (cdr list)
	(funcall not-end-action (car list)))
    (mapend fn (cdr list) not-end-action)))

(defun gen-cond-expr (requirements &optional (cond-type "WHERE"))
  "Generate a AND statement that each sub-clauses is the form like `left-value operation right-value'."
  (with-output-to-string (stream)
    (format stream " ~A " cond-type)
    (mapend #'(lambda (triple)
		(destructuring-bind (l op r) triple
		  (format stream "~A ~A '~A'" l op r)))
	    requirements
	    #'(lambda (x)
		(declare (ignore x))
		(format stream " AND ")))))

(defun gen-select-expr (fields table &optional requirements group-by order-by)
  (declare (list requirements)
	   (string table))
  (with-output-to-string (stream)
    (format stream "SELECT ")
    (if (eql '* fields)
	(format stream "*")
	(format stream "~{~A~^, ~}" fields))
    (format stream " FROM ~A" table)
    (if requirements
	(format stream "~A" (gen-cond-expr requirements)))
    (when group-by
      (format stream " GROUP BY ")
      (if (consp group-by)
	  (destructuring-bind (col reqs) group-by
	    (format stream "~A~A" col (gen-cond-expr reqs "HAVING")))
	  (format stream "~A" group-by)))
    (when order-by
      (format stream " ORDER BY ")
      (if (consp order-by)
	  (destructuring-bind (col order) order-by
	    (if (not (or (eql 'ASC order) (eql 'DESC order)))
		(error "Unknown type of order ~A" order)
		(format stream "~A ~A" col order)))
	  (format stream "~A" order-by)))))

(defmacro salet (key-values list &body body)
  `(let ,(mapcar #'(lambda (pair)
		     (destructuring-bind (k v) pair
		       `(,v (second (assoc ',k ,list)))))
		 key-values)
     ,@body))

(defun select-fn (fields table &rest clauses)
  "Select the FIELDS of rows in TABLE which meets the requirements in CLAUSES and format the output as described in CLAUSES."
  (salet ((where requirements)
	  (group-by group-by)
	  (order-by order-by))
      clauses
    (query (gen-select-expr fields table requirements group-by order-by))))

(defmacro select (fields table &body clauses)
  "The equivaient macro of function SELECT-FN."
  (salet ((where requirements)
	  (group-by group-by)
	  (order-by order-by))
      clauses
    `(query ,(gen-select-expr fields table requirements group-by order-by))))

(defun make-result (original-result)
  "Encapsulate the result of raw query into a defined class."
  (make-instance 'result
		 :schema (cadar original-result)
		 :content (caar original-result)))

(defun next-row (result)
  "Get the next row which hasn't been processed."
  (with-slots (content pointer) result
    (prog1
	(nth pointer content)
      (incf pointer))))

(defun map-row (fn result)
  "Act as function like mapcar, but for rows in instace of class result."
  (with-slots (content) result
    (mapcar fn content)))

(defun gen-insert-expr (table values &optional columns)
  (declare (string table)
	   (list columns values))
  (with-output-to-string (stream)
    (format stream "INSERT INTO ~A" table)
    (when columns
      (format stream " (~{~A~^, ~})" columns))
    (format stream " VALUES (~{'~A'~^, ~})" values)))

(defun insert-fn (table values &optional columns)
  "Insert the VALUES into TABLE according to the COLUMNS."
  (query (gen-insert-expr table values columns)))

(defmacro insert ((table &optional columns) values)
  "The equivaient macro of function INSERT-FN."
  `(query ,(gen-insert-expr table values columns)))

(defun gen-update-expr (table set-forms &optional requirements)
  (declare (string table)
	   (list set-forms requirements))
  (with-output-to-string (stream)
    (format stream "UPDATE ~A" table)
    (format stream " SET ~{~{~A = '~A'~}~^, ~}" set-forms)
    (when requirements
      (format stream "~A" (gen-cond-expr requirements)))))

(defun update-fn (table set-forms &optional requirements)
  "Set the fields of rows which meets the REQUIREMENTS in TABLE according to the SET-FORMS."
  (query (gen-update-expr table set-forms requirements)))

(defun gen-delete-expr (table &optional requirements)
  (declare (string table)
	   (list requirements))
  (with-output-to-string (stream)
    (format stream "DELETE FROM ~A" table)
    (when requirements
      (format stream "~A" (gen-cond-expr requirements)))))

(defun delete-fn (table &optional requirements)
  "Delete the row in TABLE which meets the REQUIREMENTS."
  (query (gen-delete-expr table requirements)))