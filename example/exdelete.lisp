;;; The example of deleting entries in the given table by means of the function 
;;; SQL-DELETE. For this operation, you just need the name of the table you 
;;; operated on and the optional requirements of the rows to be deleted. For 
;;; example, the expression below show how to delete the row inserted in the 
;;; tutorial of using SQL-INSERT function.

(sql-delete "Student" :where-spec '((sno = 6)))

;;; You can delete all the elements in table named by the first argument without 
;;; the keyword argument WHERE-SPEC in the expression above.