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
;;; entries returned from database will be print.