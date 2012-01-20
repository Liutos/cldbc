;;; The example of using the function SQL-SELECT from package CLDBC
;;; First, please ensure you have a database named `test' in MySQL database 
;;; management system and a table named `Student' in `test' database. The content 
;;; of the `Student' table listed following
;;; Sno	Sname	Sex	Age	Dept
;;; 1	Apple	mal	19	CS
;;; 2	Pear	mal	20	CS
;;; 3	Orange	fem	19	CS
;;; 4	Peach	mal	18	CS
;;; 5	Banana	fem	19	EN
;;; You can evaluate the following expression for selecting all the rows in the 
;;; table. The result is the same as using the QUERY function from CL-MYSQL 
;;; package.

(sql-select '(sno sname sex age dept) "Student")

;;; You can use the symbol to indicate the fields to be selected because the fields
;;; in a table in MySQL database is case-insensitive. And you can use a list of 
;;; strings as well. However, you must use a string value to indicate the table 
;;; you operated on because tha table name is case-sensitive. You can also use 
;;; the asterisk symbol '*' for indicating all the fields, like the following 
;;; expression

(sql-select '* "Student")

;;; You can also specified the requirements of the rows selected. Specify the 
;;; keyword argument WHERE-SPEC to do this. The argument should have the following 
;;; structure, it's a list of triples. And each triple contains the symbol as 
;;; left-value, a operator for comparing and a constant left-value. For example

(sql-select '* "Student" :where-spec '((sno < 4)))

;;; The expression above will get the rows whose SNO field is less than 4. If you 
;;; want to use the GROUP BY and ORDER BY clause, specified the keyword argument 
;;; GROUP-BY and ORDER-BY. For instance

(sql-select '((2012 - age)) "Student" :group-by-spec 'age)

;;; The value of GROUP-BY-SPEC keyword argument can be a list. If it is, the car 
;;; is the column name used by the GROUP BY clause, the cdr is used by the HAVING 
;;; clause. The structure of the cdr is the same as the WHERE-SPEC keyword 
;;; argument. Try the following code

(sql-select '(sname) "Student" :group-by-spec '(dept (dept = "EN")))

;;; And here's the example of ORDER-BY-SPEC keyword argument usage. Order the 
;;; result rows by their values in SNO column descreasingly. If order the rows 
;;; increasingly, you can just use the symbol SNO as argument.

(sql-select '(sno sname) "Student" :order-by-spec '(sno desc))