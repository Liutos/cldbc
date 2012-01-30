;;; This example show tha usage of the function CREATE-TABLE exported from 
;;; the CLDBC package. This function is used for creating a table in the current 
;;; database. The calling form seems like the SQL statement entered in the command 
;;; line of a RDBMS. For example, assume that we'd like to create a table named 
;;; `Foo' with two fields. The first field named `Id' is type of INT and would be 
;;; increment automatically; the second one named `Baz' is type of CHAR(20) and 
;;; should be not null. The function call for creating such a table is

(create-table "FooBar"
	      '((Id INT :auto-increment t :primary-key t)
		(Baz (CHAR 20) :not-null t)))

;;; You can also specify the primary key of the table at the position after all 
;;; the specifications of all fields by using the keyword argument 
;;; TABLE-PRIMARY-KEY. For example

(create-table "FooBaz"
	      '((Id INT :auto-increment t)
		(Baz (CHAR 20) :not-null t))
	      :table-primary-key '(Id))