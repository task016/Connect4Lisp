
;brojac istih, poenix, poenio, lista
(defun prebroj(n px po lista)
    (cond
        ((= n 4)
            (if (equal (car lista) #\X) (prebroj (1- n) (1+ px) po lista) (prebroj (1- n) px (1+ po) lista)) 
        )  
        ((= 1 (length lista)) (list px po))
        ((equal (car lista) (cadr lista)) (prebroj (1+ n) px po (cdr lista)))
        (t (prebroj '1 px po (cdr lista)))
    )
)

;vraca listu (poeniX, poeniO)
(defun countPoints(lista)
    (prebroj 1 0 0 lista)
)

(countPoints '(#\X #\O #\X #\X #\X #\X))
