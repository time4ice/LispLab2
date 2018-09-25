(defun len (list1 l)
    (if (null list1) l
        (len (cdr list1) (+ l 1))
    )
)

(defun delelem (list1 list2 elem)
(if(eq (len list2 0) (- elem 1)) (append list2 (cdr list1) )
                    (delelem (cdr list1) (append list2 (list (car list1)) ) elem))
)
)

(defun deleting (list1 list2 depth elem)
(if 
 (null list1) list2 
 (if (atom (car list1)) (deleting (cdr list1) (append list2 (list (car list1))) depth elem)
                        (if (not (eq (len (car list1) 0) depth)) (deleting (cdr list1)  (append list2 (list(deleting (car list1) '() depth elem))) depth elem)
                                                               (deleting (cdr list1) (append list2 (list(deleting (delelem(car list1) '() elem) '() depth elem))) depth elem)                   
                        )
 )
)
) 


(princ "5.Deleting elements from sublists:")
(princ (deleting '((1 2 (3 4 5 6) 7) (8 9)) '() 4 2))










