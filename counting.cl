
;brojac istih, poenix, poenio, lista
(defun countPillar(n px po lista)
    (cond
        ((= n 4)
            (if (equal (car lista) #\X) (countPillar (1- n) (1+ px) po lista) (countPillar (1- n) px (1+ po) lista)) 
        )  
        ((= 1 (length lista)) (list px po))
        ((equal (car lista) (cadr lista)) (countPillar (1+ n) px po (cdr lista)))
        (t (countPillar '1 px po (cdr lista)))
    )
)

;vraca listu (poeniX, poeniO)
(defun racunajStub(lista)
    (countPillar 1 0 0 lista)
)

(racunajStub '(#\X #\O #\X #\X #\X #\X))
