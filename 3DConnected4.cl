(setf n (read))
(setf pom n)

(defun pravilistu (n) (cond ((= n 1) (list '-)) (t(cons '- (pravilistu(1- n))))))

(defun pravimatricu (n) (cond ((= n 1) (list (pravilistu pom))) (t(cons (pravilistu pom) (pravimatricu(1- n))))
                              ))

(defun pravi3Dmatricu (n) (cond ((= n 1) (list (pravimatricu pom))) (t(cons (pravimatricu pom) (pravi3Dmatricu(1- n))))  
                           ))
(setf tabla (pravi3Dmatricu n))



(setf x (read))
(setf y (read))

(defun postavi)