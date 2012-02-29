;;; The function sql-sole-select and macro with-sole-select is designed specially 
;;; for the purpose of operating on one element return from the database query. 
;;; Generally, SQL query is set-oriented and most of the functions provided by 
;;; this package is this type. However, we sometimes need to operate on only one 
;;; element, the only one row in the result set returned from the database 
;;; manager. This function was created when I used this package for building the 
;;; lower-layout of another project named ideashare. I found that I need to 
;;; process on one row elegantly. Therefore, these function and macro comes.

(with-sole-select ((Sno Sname) "Student"
		   :where-spec '((Sno = 1)))
  (print Sno)
  (print Sname))

;;; If you evaluate the expression above, string "1" and "Apple" would be printed 
;;; to standard output. And because this macro is one-element-oriented, the key 
;;; word argument offered is :where-spec. :group-by-spec and :order-by-spec is 
;;; useless when processing on the table contains only one row. The code below use 
;;; the function SQL-SOLE-SELECT can done the same work

(destructuring-bind (Sno Sname)
    (sql-sole-select '(Sno Sname) "Student" :where-spec '((Sno = 1)))
  (print Sno)
  (print Sname))

;;; However, the macro WITH-SOLE-SELECT does not use the function SQL-SOLE-SELECT 
;;; because the macro is designed ealier than the function. Sometimes the function 
;;; call of SQL-SOLE-SELECT is more elegant than the calling of macro 
;;; WITH-SOLE-SELECT.

;;; The macro WITH-SOLE-FOUND is better than macro WITH-SOLE-SELECT because it 
;;; will return NIL automatically when the result of query in the database is 
;;; empty. Otherwise, the expression in BODY argument would be evaluted and the 
;;; return value in controled by them.

(with-sole-found ((Sno Sname) "Student"
		  :where-spec '((Sno = 0)))
  (list Sno Sname))

;;; The code above will return NIL because the default table Student does not 
;;; have a row with Sno equals zero.