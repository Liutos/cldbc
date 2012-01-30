;;; Actually, this macro is not my like. However, it's still important to write 
;;; the example and usage document for it. This macro, 
;;; DEFINE-SIMPLE-CLASS-TABLE, is used for defining a new class and create a 
;;; corresponding table in database according to its slots' attributes. Assume 
;;; that we'd like to define a class as described below

(defclass post ()
  ((title :initarg :title)
   (text :initarg :text)))

;;; After defining the class, we should define a table for storing the data of 
;;; this kind of class. The SQL statement may be the following thing

CREATE TABLE POST (
	Title CHAR(20),
	Text CHAR(20)
)

;;; All the code above could be mixed into this macro as the following code 
;;; described. The second argument is a list of triplets. Each triplet in this 
;;; list specifies the information of slots in the class and fields in the table.
;;; The first element of a triplet is the name of the slot and field. The second 
;;; element is a list, it would be consed with the first element as a slot 
;;; specification of the class. The third element is also a list, consed with the 
;;; first element and used as a field specification in table used as the component 
;;; of the argument FIELDS-SPEC of function CREATE-TABLE.

(define-simple-class-table post
    ((title (:initarg :title)
	    ((char 20)))
     (text (:initarg :text)
	   ((char 20)))))