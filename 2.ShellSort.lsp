(defun findh (paramt h)
(if (or (eq paramt 1) (eq paramt 0)) h
             (findh (- paramt 1) (+ 1 (* 3 h)))
)
)

(defun len (list1 l)
    (if (null list1) l
        (len (cdr list1) (+ l 1))
    )
)

(defun paste (a list1 list2)
(if (null list1) (append list2 (list a) list1)
                 (if (> (car list1) a) (append list2 (list a) list1)
                                       (paste a (cdr list1) (append list2 (list (car list1) )))
                 )
)
)

(defun simplesort (list1 list2)
(if (null list1) list2
                 (simplesort (cdr list1) (paste (car list1) list2 '()) ))
)
)

(defun mymerge (list1 list2 list3 h position reqpos)
(if (null list2) (append list3 list1)
                 (if (not (eq (rem position h) reqpos)) (mymerge (cdr list1) list2 (append list3 (list(car list1))) h (+ position 1) reqpos)
                                       (mymerge list1 (cdr list2) (append list3 (list(car list2))) h (+ position 1) reqpos)
                 )
)
)

(defun sortwithh (list1 list2 list3 h reqposition position)
(if (null list1) (mymerge list3 (simplesort list2 '())  '() h 0 reqposition)
                  (if (eq (rem position h) reqposition) (sortwithh (cdr list1) (append list2 (list (car list1))) list3 h reqposition (+ position 1))
                                  (sortwithh (cdr list1) list2  (append list3 (list (car list1))) h reqposition (+ position 1))
                   )
)
)

(defun globalsortwithh (list1 h step reqposition)
(if (eq step 0) list1
                (globalsortwithh (sortwithh list1 '() '() h reqposition 0) h (- step 1) (+ reqposition 1)) 
)
)

(defun shellsort2 (list1 h )
(if (eq h 1) (simplesort list1 '())
             (shellsort2 (globalsortwithh list1 h h 0) (floor(/ (- h 1) 3)))
)
)


(defun shellsort (list1)
(shellsort2 list1 (findh (floor (- (log (len list1 0) 3) 1)) 1))
)

(print "ShellSort: ")
(print (shellsort '(27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))
(terpri)



