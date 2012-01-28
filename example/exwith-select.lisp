;;; The example of using the macro exported from package CLDBC. This macro is 
;;; used for simplying the burden of mixing the code from selecting fields in a 
;;; table to destructuring-bind the values in a row to the corresponding symbol 
;;; which have the same name as the fields specifiers. Here is an example, and 
;;; the table `Student' is the same as the one used in file `exselect.lisp'

(with-select ((sname) "Student")
  (print sname))

;;; The code above selects the name of each students in table `Student' and print 
;;; them to the standard output. The symbol SNAME is used both as the field 
;;; specifier and the symbol that held the value of the corresponding field. You 
;;; can use the asterisk symbol as the caar of the first element of this macro, 
;;; if you do so, the second element of the FIELD-VARS will be the symbol stored 
;;; the whole row returned from the query statement. For example

(with-select ((* args) "Student")
  (print args))

;;; Evaluate this expression, then each row contains all the information of the 
;;; entries returned from database will be print. The expression in the three 
;;; keyword parameters' place would be evaluated for the convenience of 
;;; constructing the code as you need. For example

(defparameter *sno* 5)

(with-select ((sno sname) "Student" :where-spec `((sno < ,*sno*)))
  (format t "~D -> ~A~%" sno sname))

;;; Because the value of keyword argument :where-spec would be evaluated, it's 
;;; very usefull for using the backquote to construct the code like the code 
;;; above. However, this definition of macro WITH-SELECT isn't very well because 
;;; it depends on the inner structure of class result------each row in the content 
;;; of the query result must be a proper list in order to use the macro 
;;; DESTRUCTURING-BIND for assignment.

;;; At first, I was worried that the WITH-SELECT macro would accept the 
;;; not-existed field and becomes a bug when using the DESTRUCTURING-BIND macro 
;;; for assignment. However, if the caller pass an unexisted field to the macro 
;;; WITH-SELECT in the place of parameter FIELD-VARS, it would signal a error 
;;; when pass the statement to the MySQL for execution.

;;; When we're processing with the result from selecting operation, there is a 
;;; pattern in this operation. So what we actually need is the pattern for 
;;; accepting the arguments for querying from the database, then traversal through 
;;; the selecting result and execute some code on each row of the result set. In 
;;; a word, the WITH-SELECT macro is usefull enough.