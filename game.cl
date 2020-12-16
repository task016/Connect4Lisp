(setf tabla nil)
(setf dim nil)
(setf isFirstPlayer nil)
(setf isTwoPlayers nil)
(setf movesToGo nil)
(setf nowPlaying #\X)

(setf pom nil)

;FUNKCIJE ZA KREIRANJE TABLE (pravi3Dmatricu dim)
(defun pravilistu (n) 
    (cond 
        ((= n 1) (list #\-)) 
        (t (cons #\- (pravilistu(1- n))))
    )
)

(defun pravimatricu (n) 
    (cond 
        ((= n 1) (list (pravilistu pom))) 
        (t(cons (pravilistu pom) (pravimatricu(1- n))))
    )
)

(defun pravi3Dmatricu (n) 
    (cond 
        ((= n 1) (list (pravimatricu pom))) 
        (t(cons (pravimatricu pom) (pravi3Dmatricu(1- n))))
    )
)

;FUNKCIJE ZA STAMPANJE TABLE (drawTable tabla dim)
(defun drawFirstLine(n) 
    (cond 
        ((= n 4) (format t "0 1 2 3 4 5 6 7 8 9 A B C D E F~%"))
        ((= n 6) (format t "0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z~%"))
    )
)

(defun drawTable(lista n)
    (format t "~%")
    (drawFirstLine n)
    (cond
        ((= n 4)
            (loop for i from 1 to 7
            do (cond
                    ((= i 1)
                        (loop for j from 0 to (- n 1) 
                        do (format t "      ~C " (nth 3 (nth 3 (nth j lista))))
                        )
                        (format t "~%")
                    )
                    ((= i 2)
                        (loop for j from 0 to (- n 1)
                        do (format t "    ~C ~C " (nth 3 (nth 2 (nth j lista))) (nth 2 (nth 3 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 3)
                        (loop for j from 0 to (- n 1)
                        do (format t "  ~C ~C ~C " (nth 3 (nth 1 (nth j lista))) (nth 2 (nth 2 (nth j lista))) (nth 1 (nth 3 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 4)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C ~C " (nth 3 (nth 0 (nth j lista))) (nth 2 (nth 1 (nth j lista))) (nth 1 (nth 2 (nth j lista))) (nth 0 (nth 3 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 5)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C   " (nth 2 (nth 0 (nth j lista))) (nth 1 (nth 1 (nth j lista))) (nth 0 (nth 2 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 6)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C     " (nth 1 (nth 0 (nth j lista))) (nth 0 (nth 1 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 7)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C       " (nth 0 (nth 0 (nth j lista))))  
                        )
                        (format t "~%")
                    )
            )  
            )
        )
        ((= n 6)
            (loop for i from 1 to 11
            do (cond
                    ((= i 1)
                        (loop for j from 0 to (- n 1) 
                        do (format t "          ~C " (nth 5 (nth 5 (nth j lista))))
                        )
                        (format t "~%")
                    )
                    ((= i 2)
                        (loop for j from 0 to (- n 1)
                        do (format t "        ~C ~C " (nth 5 (nth 4 (nth j lista))) (nth 4 (nth 5 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 3)
                        (loop for j from 0 to (- n 1)
                        do (format t "      ~C ~C ~C " (nth 5 (nth 3 (nth j lista))) (nth 4 (nth 4 (nth j lista))) (nth 3 (nth 5 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 4)
                        (loop for j from 0 to (- n 1)
                        do (format t "    ~C ~C ~C ~C " (nth 5 (nth 2 (nth j lista))) (nth 4 (nth 3 (nth j lista))) (nth 3 (nth 4 (nth j lista))) (nth 2 (nth 5 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 5)
                        (loop for j from 0 to (- n 1)
                        do (format t "  ~C ~C ~C ~C ~C " (nth 5 (nth 1 (nth j lista))) (nth 4 (nth 2 (nth j lista))) (nth 3 (nth 3 (nth j lista))) (nth 2 (nth 4 (nth j lista))) (nth 1 (nth 5 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 6)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C ~C ~C ~C " (nth 5 (nth 0 (nth j lista))) (nth 4 (nth 1 (nth j lista))) (nth 3 (nth 2 (nth j lista))) (nth 2 (nth 3 (nth j lista))) (nth 1 (nth 4 (nth j lista))) (nth 0 (nth 5 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 7)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C ~C ~C   " (nth 4 (nth 0 (nth j lista))) (nth 3 (nth 1 (nth j lista))) (nth 2 (nth 2 (nth j lista))) (nth 1 (nth 3 (nth j lista))) (nth 0 (nth 4 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 8)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C ~C     " (nth 3 (nth 0 (nth j lista))) (nth 2 (nth 1 (nth j lista))) (nth 1 (nth 2 (nth j lista))) (nth 0 (nth 3 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 9)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C ~C       " (nth 2 (nth 0 (nth j lista))) (nth 1 (nth 1 (nth j lista))) (nth 0 (nth 2 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 10)
                        (loop for j from 0 to (- n 1)
                        do (format t "~C ~C         " (nth 1 (nth 0 (nth j lista))) (nth 0 (nth 1 (nth j lista))))  
                        )
                        (format t "~%")
                    )
                    ((= i 11)
                        (loop for j from 0 to (- n 1) 
                        do (format t "~C           " (nth 0 (nth 0 (nth j lista))))
                        )
                        (format t "~%")
                    )
            )  
            )
        )
    )
    (drawFirstLine n)
)

;FUNKCIJE ZA IGRANJE POTEZA
(defun dodaj(el lista)
    (cond
        ((null lista) nil)
        ((equalp (car lista) #\-) (cons el (cdr lista)))
        (t (append (list (car lista)) (dodaj el (cdr lista))))
    )
)

(defun postavikolona (y lista el) (cond ((= y 0) (cons (dodaj el (car lista)) (cdr lista))) (t(cons (car lista) (postavikolona (1- y) (cdr lista) el)))))

(defun odigrajpotez (x y tabla el) (cond ((= x 0) (cons (postavikolona y (car tabla) el) (cdr tabla))) (t(cons (car tabla) (odigrajpotez (1- x) y (cdr tabla) el)))))

(defun odigraj (x y el)
  (if (member #\- (nth y (nth x tabla)))
      (setf tabla (odigrajpotez x y tabla el))
    )
)

;INICIJALIZACIJA IGRE
(defun gameInit ()
    (print "Unesite dimenzije table")
    (format t "~%a) 4~%b) 6~%")
    (setf input (read ))
    (cond 
        ((equal input 'a) (setf dim 4))
        ((equal input 'b) (setf dim 6))
        (t (progn (print "Greska pri unosu dimenzija") (gameInit)))
    )
    ;linija ispod se brise kad se sredi crtanje
    (setf pom dim)
    (print "Igrac-Igrac ili Igrac-BOT?")
    (format t "~%a) Igrac-Igrac~%b) Igrac-BOT~%")
    (setf input (read ))
    (cond 
        ((equal input 'a) (setf isTwoPlayers t))
        ((equal input 'b) (setf isTwoPlayers nil))
        (t (progn (print "Greska pri unosu broja igraca") (gameInit)))
    )
    (print "Zelite li da igrate prvi?")
    (format t "~%a) Da~%b) Ne~%")
    (setf input (read ))
    (cond 
        ((equal input 'a) (setf isFirstPlayer t))
        ((equal input 'b) (setf isFirstPlayer nil))
        (t (progn (print "Greska pri unosu") (gameInit)))
    )
    (setf tabla (pravi3Dmatricu dim))
    (drawTable tabla dim)

    ;TREBA DA SE NASTAVI OVA FUNKCIJA
)

(gameInit)