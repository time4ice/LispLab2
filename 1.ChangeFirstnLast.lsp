(defun middle (list1 list2)
(if (atom(cdr list1)) list2
(middle (cdr list1) (append list2 (list(car list1)) ))
)
)

(defun change (list1)
(append (last list1) (middle (cdr list1) '( )) (list (car list1))))

(princ "1.Change 1st and last:")
(princ (change '(1 2 3 4 5 6 7 8 9)))
(terpri)


