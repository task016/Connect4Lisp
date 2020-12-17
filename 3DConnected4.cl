(setf n (read))

(defun pravilistu (x) (cond ((= x 1) (list #\-)) (t(cons #\- (pravilistu(1- x))))))

(defun pravimatricu (x) (let ((pom n)) (cond ((= x 1) (list (pravilistu pom))) (t(cons (pravilistu pom) (pravimatricu(1- x))))
                              )))

(defun pravi3Dmatricu (x) (let ((pom n)) (cond ((= x 1) (list (pravimatricu pom))) (t(cons (pravimatricu pom) (pravi3Dmatricu(1- x))))  
                           )))
(setf tabla (pravi3Dmatricu n))

tabla

(setf x (1- (read)))
(setf y (1- (read)))


(if (>= x n) nil '(1))
(if (>= y n) nil '(1))

(defun dodaj(el lista)
     
        (cond
            ((null lista) nil)
            ((equalp (car lista) #\-) (cons el (cdr lista)))
            (t (append (list (car lista)) (dodaj el (cdr lista))))
        )
)


(defun odigrajpotez (x y tabla el) (cond ((= y 0) (cons (postavikolona x (car tabla) el) (cdr tabla))) (t(cons (car tabla) (odigrajpotez x (1- y) (cdr tabla) el)) 
                                                                                                  )))

  
(defun postavikolona (x lista el) (cond ((= x 0) (cons (dodaj el (car lista)) (cdr lista))) (t(cons (car lista) (postavikolona (1- x) (cdr lista) el)))
                                      ))


(defun odigraj (pomtabla x y el)
  (if (member #\- (nth x (nth y pomtabla)))
      (setf tabla (odigrajpotez x y pomtabla el))
    ))

(odigraj tabla x y #\X)