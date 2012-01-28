(defpackage :cldbc
  (:use :cl :cl-mysql)
  (:export :sql-select			;Functions
	   :sql-insert
	   :sql-update
	   :sql-delete
	   :make-result
	   :get-connection
	   :maprow
	   :result-content
	   :dorow			;Macros
	   :with-select
	   :with-slots-insert
	   ))

(in-package :cldbc)

(defclass result ()
  ((fields :initarg :fields
	   :reader result-fields)
   (content :initarg :content :initform nil
	    :reader result-content)
   (pointer :initform 0
	    :accessor result-pointer
	    :documentation "The value of this pointer indicates the position of the processing row in the content of the result. The value of this slot should just be modified from inner of some pre-defined functions."))
  (:documentation "The result of query. This class is used for shielding the details when manipulating the query result returned from the database."))

(defun get-connection (user-name plain-password database-name)
  "Connect to a existing database named DATABASE-NAME by means of the USER-NAME and PLAIN-PASSWORD and set the encoding appropriately."
  (connect :user user-name
	   :password plain-password
	   :database database-name)
  (query "set names 'utf8'"))

(defun gen-cond-expr (cond-spec &optional (cond-type 'where))
  "Generate a string of AND expression that each sub-expression is constructed with a left-value, a operator and a right-value according to each element in the COND-SPEC list."
  (with-output-to-string (*standard-output*)
    (format t "~A " cond-type)
    (loop for triple on cond-spec
       do (destructuring-bind (l op r) (car triple)
	    (format t "~A ~A '~A'" l op r))
       when (cdr triple) do (format t " AND "))))

(defun gen-group-by-expr (group-by-spec)
  "Generate the string of GROUP BY clause."
  (with-output-to-string (*standard-output*)
    (format t "GROUP BY ")
    (if (consp group-by-spec)
	(destructuring-bind (col . conds) group-by-spec
	  (format t "~A ~A" col (gen-cond-expr conds 'having)))
	(format t "~A" group-by-spec))))

(defun gen-order-by-expr (order-by-spec)
  "Generate the string of ORDER BY clause."
  (with-output-to-string (*standard-output*)
    (format t "ORDER BY ")
    (if (consp order-by-spec)
	(destructuring-bind (col order) order-by-spec
	  (format t "~A ~A" col order))
	(format t "~A" order-by-spec))))

(defun gen-fields-expr (fields-spec)
  (with-output-to-string (*standard-output*)
    (if (eq '* fields-spec)
	(format t "*")
	(loop for fields on fields-spec
	   do (let ((field (car fields)))
		(if (consp field)
		    (format t "~{~A~^ ~}" field)
		    (format t "~A" field)))
	   when (cdr fields) do (format t ", ")))))

(defun gen-sql-select-expr (fields-spec table-name &key where-spec group-by-spec order-by-spec)
  "Generate the statement string represents selecting the fields specified in FIELDS-SPEC from table named TABLE-NAME and meets the requirements listed in WHERE-SPEC. If GROUP-BY-SPEC is non-nil, concatenate the corresponding string at the end of the previous generated string. So is the ORDER-BY-SPEC argument. See the manual for examples and details about this function."
  (with-output-to-string (*standard-output*)
    (format t "SELECT ~A" (gen-fields-expr fields-spec))
    (format t " FROM ~A" table-name)
    (if where-spec
	(format t " ~A" (gen-cond-expr where-spec)))
    (when group-by-spec
      (format t " ~A" (gen-group-by-expr group-by-spec)))
    (when order-by-spec
      (format t " ~A" (gen-order-by-expr order-by-spec)))))

(defun sql-select (fields-spec table-name &key where-spec group-by-spec order-by-spec)
  "Excute the statement generated by the function GEN-SQL-SELECT-EXPR."
  (query (gen-sql-select-expr fields-spec table-name
			      :where-spec where-spec
			      :group-by-spec group-by-spec
			      :order-by-spec order-by-spec)))

(defun gen-sql-insert-expr (table-name values &optional fields)
  "Generate the statement for inserting a new value into a table named TABLE-NAME with VALUES. Columns would be specified if FIELDS is non-nil."
  (with-output-to-string (*standard-output*)
    (format t "INSERT INTO ~A" table-name)
    (if fields
	(format t " (~{~A~^, ~})" fields))
    (format t " VALUES (~{'~A'~^, ~})" (mapcar #'escape-string values))))

(defun sql-insert (table-name values &optional fields)
  "Insert a new entry into the given table in database."
  (query (gen-sql-insert-expr table-name values fields)))

(defmacro with-slots-insert (table-name instance &rest slots)
  "Insert the specified slots into database by specifying the corresponding fields in table named TABLE-NAME. The name of fields used in table must be the same as the symbol used in argument SLOTS."
  `(with-slots ,slots ,instance
     (sql-insert ,table-name (list ,@slots) ',slots)))

(defun gen-sql-update-expr (table-name assignment-spec &key where-spec)
  "Generate the statement for updating the existing entries. ASSIGNMENT-SPEC is a alist. Each of its elements specifies the field to be set and the right-value for assignment."
  (with-output-to-string (*standard-output*)
    (format t "UPDATE ~A SET ~{~{~A = '~A'~}~^, ~}"
	    table-name assignment-spec)
    (if where-spec
	(format t " ~A" (gen-cond-expr where-spec)))))

(defun sql-update (table-name assignment-spec &key where-spec)
  "Update the content of the entries meets the given requirements in WHERE-SPEC."
  (query (gen-sql-update-expr table-name assignment-spec
			      :where-spec where-spec)))

(defun gen-sql-delete-expr (table-name &key where-spec)
  "Generate the statement string for deleting the specified entries in the table named TABLE-NAME."
  (with-output-to-string (*standard-output*)
    (format t "DELETE FROM ~A" table-name)
    (if where-spec
	(format t " ~A" (gen-cond-expr where-spec)))))

(defun sql-delete (table-name &key where-spec)
  (query (gen-sql-delete-expr table-name :where-spec where-spec)))

(defun make-result (query-result)
  "Convert the QUERY-RESULT returned by the function SQL-SELECT into a instance of class RESULT."
  (make-instance 'result
		 :fields (cadar query-result)
		 :content (caar query-result)))

(defun maprow (fn result)
  "Apply the function FN to each rows in the content of the RESULT."
  (mapcar #'(lambda (row)
	      (apply fn row))
	  (result-content result)))

(defmacro dorow ((var result) &body body)
  `(dolist (,var (result-content ,result))
     ,@body))

(defmacro with-select ((field-vars table-name &key where-spec group-by-spec order-by-spec) &body body)
  "Mix the functionanities of the function SQL-SELECT and the macro DOROW. The FIELD-VARS must be a list of symbols. If the car of FIELD-VARS is an asterisk, it means selecting all the fields in the table named TABLE-NAME, and the second element of FIELD-VARS would be the symbol indicates a row in the result set. Otherwise, they are both all the objective fields to be selected and the symbol bind to the value of the corresponding field. See file `exwith-select.lisp' for details."
  (let ((row (gensym)))
    `(dorow (,row (make-result (sql-select ',(if (eql '* (car field-vars))
						 '*
						 field-vars) ,table-name
					   :where-spec ,where-spec
					   :group-by-spec ,group-by-spec
					   :order-by-spec ,order-by-spec)))
       ,(if (eql '* (car field-vars))
	    `(let ((,(cadr field-vars) ,row))
	       ,@body)
	    `(destructuring-bind ,field-vars ,row
	       ,@body)))))

(defun nthrow (n result)
  "Get the Nth row in the content of the argument RESULT of kind class result."
  (nth n (result-content result)))

(defun field-position (field-name result)
  "Return the position of the field owned the name FIELD-NAME in the FIELDS slot of RESULT with type of class result. If the field named FIELD-NAME doesn't exist, NIL would be returned."
  (let ((fields (result-fields result)))
    (position (format nil "~A" field-name)
	      fields :key #'car :test #'string=)))

;;; In fact, I don't know what's the point of using this function but not the 
;;; macro WITH-SELECT when processing with some fields after a selection 
;;; operation. So this function isn't recommended.
(defun get-field-in-row-by-name (field-name result)
  "Return the value stored in the same position as the field named FIELD-NAME in the fields slot of RESULT. The caller must ensure the field with name FIELD-NAME is existed."
  (let ((n (result-pointer result))
	(content (result-content result))
	(pos (field-position field-name result)))
    (nth pos (nth n content))))