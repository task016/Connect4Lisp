(defun dodaj(el lista)
    (cond
        ((null lista) nil)
        ((equalp (car lista) #\-) (cons el (cdr lista)))
        (t (append (list (car lista)) (dodaj el (cdr lista))))
    )
)