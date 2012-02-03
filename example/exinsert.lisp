;;; The examples of using the SQL-INSERT for inserting new entries in the given 
;;; table in MySQL database. For describing the insertion functionanity, the table 
;;; used as example in the exselect.lisp is essential. If you don't have, create 
;;; it at first, please. Now here comes the first example of using the SQL-INSERT 
;;; function to insert a new student into the `Student' table. The new student's 
;;; number, name, sex, age and dept is 6, Foobar, unknown, 19 and ZH.

(sql-insert "Student" '("6" "Foobar" "unk" 19 "ZH"))

;;; You can also specified the columns you'd like to fill through the optional 
;;; argument FIELDS. This parameter is a list of symbols. You can use a symbol to 
;;; to indicate the filed you'd like to fill because the DBMS is case-insensitive 
;;; when processing the column names. The example equivaient to the code above by 
;;; specifying the columns used is here

(sql-insert "Student" '("6" "Foobar" "unk" 19 "ZH") '(sno sname sex age dept))

;;; If you have defined a new class as the following code described, and you'd 
;;; like to insert a new entry into the database just by specifying only a part of 
;;; a instance of this kind of class into table, you can use the macro 
;;; WITH-SLOTS-INSERT. For example

(defclass post ()
  ((title)
   (text)))

(with-slots-insert "Posts" a-object title text)

;;; The code above would be expanded to the form like the following expression

(with-slots (title text) a-object
  (sql-insert "Posts" (list title text) '(title text)))