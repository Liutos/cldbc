;;; The function GEN-COND-EXPR is used for generating the SQL expression placed in 
;;; the where clauses in a SQL query statement. The old version of this function 
;;; can just generate the predicates connected by the AND logical connective. In 
;;; the newer version, the function can also generate a statement contains the 
;;; predicates connected by the OR logical connective. In addition, it can process 
;;; its argument recursively. For example, the old GEN-COND-EXPR could just 
;;; process the code below

(gen-cond-expr '((sno != 1) (sname != "foobar")))

;;; The code above generates the string "WHERE SNO != '1' AND SNAME != 'foobar'". 
;;; The default logical connective is AND and the caller has no idea about 
;;; changing it. In the new version, if you want to change the logical connective 
;;; to OR, do it like this,

(gen-cond-expr '(or (sno != 1) (sname != "foobar")))

;;; And the elements in the OR expression can also be this form so that this 
;;; function is able to process its argument recursively. For example

(gen-cond-expr '((sno != 1) (or (sno = 1) (sname != "foobar"))))

;;; The expression above generates the string 
;;; "WHERE SNO != '1' AND (SNO = '1' OR SNAME != 'foobar')". Because the function 
;;; GEN-SQL-SELECT-EXPR, GEN-SQL-UPDATE-EXPR, GEN-SQL-DELETE-EXPR and 
;;; GEN-GROUP-BY-EXPR are using this function for generating the where clauses, 
;;; the caller also get the ability of constructing statement like above when they 
;;; are selecting, updating and deleting.