;;; The examples of using the SQL-UPDATE function exported from CLDBC package for 
;;; updating the specified columns' value of specified entries in given table. A 
;;; simple example looks like the following code

(sql-update "Student" '((sname "Foobaz")) :where-spec '((sno = 6)))

;;; The value's requirements of the keyword argument WHERE-SPEC is the same as 
;;; the one in function SQL-SELECT. The second argument ASSIGNMENT-SPEC is a alist
;;; whose elements have the form of `(field new-value)'. The field is the column 
;;; to be assigned and the new-value is used to assigning, of course.