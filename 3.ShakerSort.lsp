(defun eonesort (list1 list2)
(if 
 (atom (cdr list1)) (append (last list1) list2 )
 (if (> (car (last list1)) (car (last (butlast list1)))) 
  (eonesort (butlast list1) (append (last list1) list2 ))
  (eonesort (append (butlast (butlast list1)) (last list1)) (append (last (butlast list1)) list2 ))
 )
)
) 

(defun ebubblesort (list1 list2 list3)
(if
 (atom (cdr list1)) (append list3 (list(car list1)) list2 )
 (bbubblesort (cdr (eonesort list1 '())) list2 (append list3 (list (car(eonesort list1 '())))))
)
)


(defun bonesort (list1 list2)
(if 
 (atom (cdr list1)) (append list2 (last list1))
 (if (< (car list1) (car (cdr list1))) 
  (bonesort (cdr list1) (append list2 (list (car list1))))
  (bonesort (append (list(car list1)) (cddr list1)) (append list2 (list(car (cdr list1)))))
 )
)
) 

(defun bbubblesort (list1 list2 list3)
(if
 (atom (cdr list1)) (append list3 (list(car list1)) list2)
 (ebubblesort (butlast(bonesort list1 '())) (append (last(bonesort list1 '())) list2) list3)
)
)


(princ "3.Shakersort:")
(princ (bbubblesort '(3 5 3 8 9 1 3 4) '() '()))
(terpri)
