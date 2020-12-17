;uf

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

(drawTable '(((#\A #\B #\C #\D)(#\E #\F #\G #\H)(#\I #\J #\K #\L)(#\M #\N #\O #\P))
             ((#\1 #\2 #\3 #\4)(#\5 #\6 #\7 #\8)(#\9 #\0 #\1 #\2)(#\3 #\4 #\5 #\6))
             ((#\- #\- #\- #\-)(#\- #\- #\- #\-)(#\- #\- #\- #\-)(#\- #\- #\- #\-))
             ((#\- #\- #\- #\-)(#\- #\- #\- #\-)(#\- #\- #\- #\-)(#\- #\- #\- #\-))) 4)

(drawTable '(((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)) 
             ((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)) 
             ((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)) 
             ((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)) 
             ((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)) 
             ((#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F)(#\A #\B #\C #\D #\E #\F))) 6)
