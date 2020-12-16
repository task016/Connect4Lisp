(setf n (read))
(setf pom n)

(defun pravilistu (n) (cond ((= n 1) (list #\-)) (t(cons #\- (pravilistu(1- n))))))

(defun pravimatricu (n) (cond ((= n 1) (list (pravilistu pom))) (t(cons (pravilistu pom) (pravimatricu(1- n))))
                              ))

(defun pravi3Dmatricu (n) (cond ((= n 1) (list (pravimatricu pom))) (t(cons (pravimatricu pom) (pravi3Dmatricu(1- n))))  
                           ))
(setf tabla (pravi3Dmatricu n))

tabla

(setf x (read))
(setf y (read))

(defun dodaj(el lista)
    (if (member #\- lista) 
        (cond
            ((null lista) nil)
            ((equalp (car lista) #\-) (cons el (cdr lista)))
            (t (append (list (car lista)) (dodaj el (cdr lista))))
        )
        nil
    )
)


(defun postavired (x y tabla el) (cond ((= x 0) (cons (postavikolona y (car tabla) el) (cdr tabla))) (t(cons (car tabla) (postavired (1- x) y (cdr tabla) el)) 
                                                                                                  )))

  
(defun postavikolona (y lista el) (cond ((= y 0) (cons (dodaj el (car lista)) (cdr lista))) (t(cons (car lista) (postavikolona (1- y) (cdr lista) el)))
                                      ))

(setf tabla (postavired x y tabla #\X))