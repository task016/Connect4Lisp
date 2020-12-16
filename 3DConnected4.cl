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

(if (>= x n) nil '(1))
(if (>= y n) nil '(1))

(defun dodaj(el lista)
     
        (cond
            ((null lista) nil)
            ((equalp (car lista) #\-) (cons el (cdr lista)))
            (t (append (list (car lista)) (dodaj el (cdr lista))))
        )
)


(defun odigrajpotez (x y tabla el) (cond ((= x 0) (cons (postavikolona y (car tabla) el) (cdr tabla))) (t(cons (car tabla) (odigrajpotez (1- x) y (cdr tabla) el)) 
                                                                                                  )))

  
(defun postavikolona (y lista el) (cond ((= y 0) (cons (dodaj el (car lista)) (cdr lista))) (t(cons (car lista) (postavikolona (1- y) (cdr lista) el)))
                                      ))


(defun odigraj (x y el)
  (if (member #\- (nth y (nth x tabla)))
      (setf tabla (odigrajpotez x y tabla el))
    ))

(odigraj x y #\X)