
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

;racuna poene u svim stubovima u jednoj koloni
(defun racunajStubove(px po lista)
    (cond
        ((null lista) (list px po))
        (t 
            (let ((poeni (countPoints (car lista))))
                (racunajStubove (+ px (car poeni)) (+ po (cadr poeni)) (cdr lista))
            ) 
        )        
    )
)

(racunajStubove 0 0 '((#\X #\X #\X #\X) (#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X)))

;racuna poene u stubovima na celoj tabli i dodaje ih na pocetne poene px i po //TREBA DA BUDE DEO FJE "RACUNAJ SVE"
(defun countAllPillars(px po lista)
    (cond
        ((null lista) (list px po))
        (t (let ((poeni (racunajStubove px po (car lista))))
                (countAllPillars (car poeni) (cadr poeni) (cdr lista))
            )
        )        
    )
)

(countAllPillars 0 0 '(((#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\X #\X #\X #\X))))

;STVARI ISPOD SU ZA KOLONE
;izdvaja sve prve elemente ugnjezdenih listi i vraca ih kao listu
(defun listaPrvih(lista)
    (cond
        ((null lista) NIL)
        (t (cons (car (car lista)) (listaPrvih (cdr lista))))        
    )
)

(listaPrvih '((#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\X #\X #\X #\X) (#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\X #\X #\X #\X)))

;vraca pocetnu listu bez prvih elemenata uignjezdenih listi
(defun listaBezPrvih(lista)
    (cond
        ((null lista) NIL)
        (t (cons (cdr (car lista)) (listaBezPrvih (cdr lista))))
    )
)

(listaBezPrvih '((#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\X #\X #\X #\X) (#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\X #\X #\X #\X)))

;vraca poene jedne kolone bez racunanja dijagonale i zasebnih stubova
(defun kolonaBezD(px po lista)
    (cond
        ((null (car lista)) (list px po))
        (t 
            (let ((poeni (countPoints (listaPrvih lista))))
                (kolonaBezD (+ px (car poeni)) (+ po (cadr poeni)) (listaBezPrvih lista))  
            )
        )
    )   
)

(kolonaBezD 0 0 '((#\X #\X #\X #\X) (#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X)))

;racuna ukupne poene svih kolona
(defun racunajSveKolone(px po lista)
    (cond
        ((null lista) (list px po))
        (t (let ((poeni (kolonaBezD px po (car lista))))
                (racunajSveKolone (car poeni) (cadr poeni) (cdr lista))
            )
        )
    )
)

(racunajSveKolone 0 0 '(((#\O #\X #\O #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\X #\X #\O)(#\X #\X #\X #\X)(#\O #\X #\X #\O)(#\X #\X #\X #\X))))

;vraca poene jedne kolone (poeni stubova + poeni kolone bez dijagonale), treba da se doda i racunanje poena na dijagonali unutar kolone
;; (defun racunajKolonu(lista) 
;;     (let ((poeni (racunajStubove 0 0 lista)))
;;         (kolonaBezD (car poeni) (cadr poeni) lista)
;;     )
;; )

;; (racunajKolonu '((#\X #\X #\X #\X) (#\X #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X) (#\O #\X #\X #\X)))

;funkcija sa interneta za transponovanje matrice
(defun transpose (m)
  (apply #'mapcar #'list m))

(transpose '(((#\O #\X #\O #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
             ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
             ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
             ((#\O #\X #\X #\O)(#\X #\X #\X #\X)(#\O #\X #\X #\O)(#\X #\X #\X #\X))))

;racunanje svih redova, transponovanjem matrice i koriscenjem funkcije za racunanje kolona
(defun racunajSveRedove(px po lista)
    (racunajSveKolone px po (transpose lista))
)

(racunajSveRedove 0 0 '(((#\O #\X #\O #\X)(#\O #\X #\O #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\X #\X #\O)(#\X #\X #\X #\X)(#\O #\X #\X #\O)(#\X #\X #\X #\X))))

;konacna funkcija za racunanje  poena bez dijagonala, koristeci sve funkcije iznad
(defun racunajPoeneBezD(lista)
    (let ((poeni (countAllPillars 0 0 lista)))
        (setf poeni (racunajSveKolone (car poeni) (cadr poeni) lista))
        (setf poeni (racunajSveRedove (car poeni) (cadr poeni) lista))
    )
)

(racunajPoeneBezD     '(((#\O #\X #\O #\X)(#\O #\X #\O #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\O #\O #\O)(#\X #\X #\X #\X)(#\O #\O #\O #\O)(#\O #\O #\O #\O))
                        ((#\O #\X #\X #\O)(#\X #\X #\X #\X)(#\O #\X #\X #\O)(#\X #\X #\X #\X))))