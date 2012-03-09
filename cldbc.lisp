(defpackage :cldbc
  (:use :cl :cl-mysql)
  (:shadowing-import-from :cl-mysql
			  :escape-string)
  (:export :sql-select			;Functions
	   :sql-insert
	   :sql-update
	   :sql-delete
	   ;; The functions above is the basic operations of a SQL database
	   :create-database
	   :delete-from-all-tables
	   :escape-string
	   :get-connection
	   :make-result
	   :maprow
	   :mapselect
	   :result-content
	   :row-exist-p
	   :sget-connection
	   :sql-sole-select
	   :dorow			;Macros
	   :with-select
	   :with-slots-insert
	   :with-sole-select
	   :with-sole-found
	   :with-sole-found-let
	   )
  (:documentation "The function MAPROW and macro WITH-SELECT is used for applying the same operation on every elements in the result set returned from the database."))

(in-package :cldbc)

(proclaim '(inline create-database use-database))
(declaim (optimize (debug 3)))

(defclass result ()
  ((fields :initarg :fields
	   :reader result-fields)
   (content :initarg :content :initform nil
	    :reader result-content)
   (pointer :initform 0			;It seems that this varible is useless
	    :accessor result-pointer
	    :documentation "The value of this pointer indicates the position of the processing row in the content of the result. The value of this slot should just be modified from inner of some pre-defined functions."))
  (:documentation "The result of query. This class is used for shielding the details when manipulating the query result returned from the database."))

(defun get-connection (user-name plain-password database-name)
  "Connect to a existing database named DATABASE-NAME by means of the USER-NAME and PLAIN-PASSWORD and set the encoding appropriately."
  (connect :user user-name
	   :password plain-password
	   :database database-name)
  (query "set names 'utf8'"))

(defun create-database (database-name)
  (query (format nil "CREATE DATABASE ~A character set 'utf8'" database-name)))

(defun use-database (database-name)
  (query (format nil "USE ~A" database-name)))

(defun sget-connection (user password database)
  "Acts like the function GET-CONNECTION except that this function would create the objective database if it doesn't exist yet."
  (connect :user user :password password)
  (handler-case (use-database database)
    (cl-mysql-system:mysql-error ()
      (create-database database)
      (use-database database)))
  (query "set names 'utf8'"))

;;; The old version of function GEN-COND-EXPR.
;; (defun gen-cond-expr (cond-spec &optional (cond-type 'where))
;;   "Generate a string of AND expression that each sub-expression is constructed with a left-value, a operator and a right-value according to each element in the COND-SPEC list."
;;   (with-output-to-string (*standard-output*)
;;     (format t "~A " cond-type)
;;     (loop for triple on cond-spec
;;        do (destructuring-bind (l op r) (car triple)
;;   	    (format t "~A ~A '~A'" l op r))
;;        when (cdr triple) do (format t " AND "))))

(defun gen-cond-rec (cond-set)
  (let ((lg 'and) (st cond-set))
    (when (member (car cond-set) '(and or))
      (setf lg (car cond-set)
	    st (cdr cond-set)))
    (with-output-to-string (*standard-output*)
      (loop
	 :for triples :on st
	 :do (let ((triple (car triples)))
	       (if (member (car triple) '(and or))
		   (format t "(~A)" (gen-cond-rec triple))
		   (destructuring-bind (l op r) triple
		     (format t "~A ~A '~A'" l op r))))
	 :when (cdr triples) :do (format t " ~S " lg)))))

(defun gen-cond-expr (cond-spec &optional (cond-type 'where))
  "Generate a string of AND expression that each sub-expression is constructed with a left-value, a operator and a right-value according to each element in the COND-SPEC list. This new version function can process its first argument recursively. See file `exgen-cond.lisp' for detail."
  (with-output-to-string (*standard-output*)
    (format t "~A " cond-type)
    (format t "~A" (gen-cond-rec cond-spec))))

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
		    (if (member (car field) '(max))
			(progn
			  (format t "~A" (car field))
			  (format t "(~{~A~^, ~})" (cdr field)))
			(format t "~{~A~^ ~}" field))
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
    (format t " VALUES (~{'~A'~^, ~})" values)))

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

(defun mapselect (fn fields-spec table-name &key where-spec group-by-spec order-by-spec)
  "Query a result set from the database depends on the argument FIELDS-SPEC, TABLE-NAME and the three keyword argument and after this, map the function FN to each of the elements in the result set."
  (maprow fn (make-result (sql-select fields-spec table-name
				      :where-spec where-spec
				      :group-by-spec group-by-spec
				      :order-by-spec order-by-spec))))

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

(defun gen-field-entry-expr (field-name type &key not-null auto-increment primary-key)
  "Generate a field specification according to the arguments used when creating a table in a database. The parameter TYPE can be a symbol or a proper list with two elements. If it's a symbol, it must represent a legal type in database. If it is a cons, the first and the second element would be the type and the specification of the type in order. The field with non-nil value of argument AUTO-INCREMENT would be set the primary key flag."
  (with-output-to-string (*standard-output*)
    (format t "~A" field-name)
    (if (consp type)
	(destructuring-bind (attr n) type
	  (format t " ~A(~D)" attr n))
	(format t " ~A" type))
    (and not-null (format t " NOT NULL"))
    (and auto-increment (format t " AUTO_INCREMENT"))
    (and primary-key (format t " PRIMARY KEY"))))

(defun gen-field-spec-expr (fields-spec)
  (with-output-to-string (*standard-output*)
    (loop
       :for fs on fields-spec
       :do (format t "~A" (apply #'gen-field-entry-expr (car fs)))
       :when (cdr fs) :do (format t ", "))))

(defun gen-create-table-expr (table-name fields-spec &key table-primary-key)
  "Generate the string represents the statement for creating a table according to the arguments. Each element in the list FIELDS-SPEC must be the structure of the parameter list of function GEN-FIELD-ENTRY-EXPR."
  (with-output-to-string (*standard-output*)
    (format t "CREATE TABLE ~A (~A"
	    table-name (gen-field-spec-expr fields-spec))
    (if table-primary-key
	(format t ", PRIMARY KEY (~{~A~^, ~})" table-primary-key))
    (format t ")")))

(defun create-table (table-name fields-spec &key table-primary-key)
  "Start creating a table named TABLE-NAME with fields specified by the arguments FIELDS-SPEC and the others."
  (query (gen-create-table-expr table-name fields-spec
				:table-primary-key table-primary-key)))

(defmacro define-simple-class-table (name slots-spec &key table-primary-key)
  "Define a class and a table in the database both named NAME. The SLOTS-SPEC contains both the specification of slots in class definitions and informations of fields. The name of each slot is the same as the name of each field in table. The class defined through this macro is unable to inherit from other classes. This is why this function is `simple'. For details, see example in file `exdsct.lisp' in example/ directory."
  `(progn
     (defun ,name ()
       ,(mapcar #'(lambda (spec)
		    (cons (car spec)
			  (cadr spec)))
		slots-spec))
     (create-table ,(format nil "~A" name)
		   ',(mapcar #'(lambda (spec)
				 (cons (car spec)
				       (caddr spec)))
			     slots-spec)
		   :table-primary-key ,table-primary-key)))

(defmacro with-sole-select ((fields-spec table-name &key where-spec) &body body)
  "Acts like the macro WITH-SELECT but this macro would not loop among the result set. Instead, the caller of this macro must ensure that the result set returned by the inner SQL-SELECT function just contains one element. That's what `sole' means. For more information see file `exsole-select.lisp'."
  (let ((query-result (gensym))
	(result (gensym))
	(content (gensym)))
    `(let ((,query-result (sql-select ',fields-spec ,table-name
				      :where-spec ,where-spec)))
       (let ((,result (make-result ,query-result)))
	 (let ((,content (result-content ,result)))
	   (destructuring-bind ,fields-spec (car ,content)
	     ,@body))))))

(defun sql-sole-select (fields-spec table-name &key where-spec)
  "Acts like the function SQL-SELECT but this function returns the first element in the result set. The caller must ensure that the object result set just contains one element."
  (car (result-content (make-result (sql-select fields-spec
						table-name
						:where-spec where-spec)))))

(defmacro with-sole-found ((fields-spec table-name &key where-spec) &body body)
  "Act like the macro WITH-SOLE-SELECT above. This macro will return nil when the result set returned from the query of selecting in database contains no elements. In the other case, the expression in the argument BODY will be evaluted. See `exsole-select.lisp' for details."
  (let ((query-result (gensym))
	(content (gensym)))
    `(let ((,query-result (sql-select ',fields-spec
				      ,table-name
				      :where-spec ,where-spec)))
       (let ((,content (result-content (make-result ,query-result))))
	 (cond (,content
		(destructuring-bind ,fields-spec (car ,content)
		  ,@body))
	       (t nil))))))

(defmacro with-sole-found-let ((fields-spec table-name &key where-spec) &body body)
  "Thsi macro is similar with the previous version named WITH-SOLE-FOUND. The FIELDS-SPEC argument of this macro is special because each element in this argument can both be a symbol of a list contains two sub elements. If it's a symbol, it would be used both as column name in query statement and variable name in a LET form. If it's a list, the car of it would be used as the variable name and the cadr, the second one would be used as the column name for constructing the query statement."
  (let ((query-result (gensym))
	(content (gensym))
	(fields-spec (mapcar #'(lambda (x)
				 (if (consp x) (cadr x) x))
			     fields-spec))
	(vars (mapcar #'(lambda (x)
			  (if (consp x) (car x) x))
		      fields-spec)))
    `(let ((,query-result (sql-select ',fields-spec ,table-name
				      :where-spec ,where-spec)))
       (let ((,content (result-content (make-result ,query-result))))
	 (cond (,content
		(destructuring-bind ,vars (car ,content)
		  ,@body))
	       (t nil))))))

(defun escape-string (string &key database)
  "The re-exporting symbol of the function named `escape-string' in package CL-MYSQL."
  (cl-mysql::escape-string string :database database))

(defun row-exist-p (table-name where-spec)
  "Return T if the table in database named TABLE-NAME contains a row meets the requirement specified by argument WHERE-SPEC."
  (with-select ((* args) table-name
		:where-spec where-spec)
    (return args)))

(defun delete-from-all-tables ()
  (let ((table-names (mapcan #'identity
			     (caar (list-tables)))))
    (dolist (table table-names)
      (sql-delete table))))