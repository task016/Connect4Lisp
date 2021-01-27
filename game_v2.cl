(require "Inference_engine.cl")
(setf tabla nil)
(setf dim nil)
(setf isFirstPlayer nil)
(setf isTwoPlayers nil)
(setf movesToGo nil)
(setf nowPlaying #\X)

;FUNKCIJE ZA KREIRANJE TABLE (pravi3Dmatricu dim)
(defun pravilistu (x) (cond ((= x 1) (list #\-)) (t(cons #\- (pravilistu(1- x))))))

(defun pravimatricu (x) (let ((pom dim)) (cond ((= x 1) (list (pravilistu pom))) (t(cons (pravilistu pom) (pravimatricu(1- x)))))))

(defun pravi3Dmatricu (x) (let ((pom dim)) (cond ((= x 1) (list (pravimatricu pom))) (t(cons (pravimatricu pom) (pravi3Dmatricu(1- x)))))))

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

(defun odigrajpotez (x y tabla el) (cond ((= y 0) (cons (postavikolona x (car tabla) el) (cdr tabla))) (t(cons (car tabla) (odigrajpotez x (1- y) (cdr tabla) el)))))

  
(defun postavikolona (x lista el) (cond ((= x 0) (cons (dodaj el (car lista)) (cdr lista))) (t(cons (car lista) (postavikolona (1- x) (cdr lista) el)))))


(defun odigraj (pomtabla x y el)
  (if (member #\- (nth x (nth y pomtabla)))
      (setf tabla (odigrajpotez x y pomtabla el))
))

(defun humanPlay()
   (cond
    ((= 0 movesToGo) (checkForWinner tabla)) 
    (t 
        (format t "~%Prvi igrac na potezu~%Unesite potez u formatu red kolona~%")
        (if (not (odigraj tabla (1- (read )) (1- (read )) nowPlaying)) (progn (format t "~%Los potez, igrate ponovo~%") (humanPlay)))
        (setf movesToGo (1- movesToGo))
        (if (equal nowPlaying #\X) (setf nowPlaying #\O) (setf nowPlaying #\X))
        (drawTable tabla dim)
        (botPlay) 
    )
    )
)

(defun botPlay()
  (cond
    ((= 0 movesToGo) (checkForWinner tabla))
    (t
        (cond
            ((equal t isTwoPlayers)
                (format t "~%Drugi igrac na potezu~%Unesite potez u formatu red kolona~%")
                (if (not (odigraj tabla (1- (read )) (1- (read )) nowPlaying)) (progn (format t "~%Los potez, igrate ponovo~%") (botPlay)))
                (setf movesToGo (1- movesToGo))
                (if (equal nowPlaying #\X) (setf nowPlaying #\O) (setf nowPlaying #\X))
                (drawTable tabla dim)
                (humanPlay)  
            )
            (t 
                (format t "~%BOT na potezu~%")
                (setf tabla (cadr (minimax tabla 3 -1000 1000 t)))
                (setf movesToGo (1- movesToGo))
                (if (equal nowPlaying #\X) (setf nowPlaying #\O) (setf nowPlaying #\X))
                (drawTable tabla dim)
                (humanPlay)  
            )            
        )
    )
)
)

;FUNKCIJE ZA SVA MOGUCA STANJA

(defun odigrajstanja (pomtabla x y el)
  (if (member #\- (nth x (nth y pomtabla)))
      (odigrajpotez x y pomtabla el)
    ))

(defun mogucastanja (pom potez stanje) 
  (let ((i dim)) (cond ((< pom 0) '())
        ((null (mogucastanja_kol i pom potez stanje)) (mogucastanja (1- pom) potez stanje))
        (t(cons (mogucastanja_kol i pom potez stanje) (mogucastanja (1- pom) potez stanje)))                   
        )))

(defun mogucastanja_kol (i pom potez stanje) 
  (cond ((< i 0) '())
        ((null (odigrajstanja stanje i pom potez)) (mogucastanja_kol (1- i) pom potez stanje ))
        (t(cons (odigrajstanja stanje i pom potez) (mogucastanja_kol (1- i) pom potez stanje )))
        ))

(defun mogstanja (pom potez stanje) (apply 'append (mogucastanja pom potez stanje)))

;FUNCKIJE ZA RACUNANJE POENA
;brojac istih, poenix, poenio, lista
(defun prebroj(n px po lista)
    (cond
        ((= n 4)
            (if (equal (car lista) #\X) (prebroj (1- n) (1+ px) po lista) (if (equal (car lista) #\O) (prebroj (1- n) px (1+ po) lista) (list px po))) 
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

;racuna poene u stubovima na celoj tabli i dodaje ih na pocetne poene px i po
(defun countAllPillars(px po lista)
    (cond
        ((null lista) (list px po))
        (t (let ((poeni (racunajStubove px po (car lista))))
                (countAllPillars (car poeni) (cadr poeni) (cdr lista))
            )
        )        
    )
)

;izdvaja sve prve elemente ugnjezdenih listi i vraca ih kao listu
(defun listaPrvih(lista)
    (cond
        ((null lista) NIL)
        (t (cons (car (car lista)) (listaPrvih (cdr lista))))        
    )
)

;vraca pocetnu listu bez prvih elemenata uignjezdenih listi
(defun listaBezPrvih(lista)
    (cond
        ((null lista) NIL)
        (t (cons (cdr (car lista)) (listaBezPrvih (cdr lista))))
    )
)

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

;vraca dijagonalu kao listu, sa pocetkom u (0,i), u zavisnosti od parametra glavna vraca glavnu ili sporednu
(defun getDiag(i glavna lista)
    (cond
        ((null lista) NIL)
        ((and glavna (= i (length (car lista)))) NIL)
        ((and (not glavna) (< i 0)) NIL)
        ((>= i (length (car lista))) (getDiag (1- i) glavna (cdr lista)))
        ((< i 0) (getDiag (1+ i) glavna (cdr lista)))
        (glavna (cons (nth i (car lista)) (getDiag (1+ i) glavna (cdr lista))))
        (t (cons (nth i (car lista)) (getDiag (1- i) glavna (cdr lista))))        
    )
)

;funkcija za transponovanje matrice
(defun transpose (m)
  (apply #'mapcar #'list m)
)

;vraca poene dijagonala jedne kolone, uvek se poziva sa i=-2
(defun kolonaD(px po i lista)
    (cond 
        ((= 4 (length (car lista))) 
            (cond
                ((= i -2) 
                    (let ((poeni (countPoints (getDiag 0 t (transpose lista)))))
                        (kolonaD (+ px (car poeni)) (+ po (cadr poeni)) 2 lista)
                    ) 
                )
                ((= i 2)
                    (let ((poeni (countPoints (getDiag 3 NIL (transpose lista)))))
                        (list (+ px (car poeni)) (+ po (cadr poeni)))  
                    ) 
                )                
            )
        )
        ((= 5 (length (car lista)))
             (cond
                ((= i 6) (list px po))
                ((< i -1) (kolonaD px po (1+ i) lista))
                ((= 2 i) (kolonaD px po (1+ i) lista))
                ((< i 2)
                    (let ((poeni (countPoints (getDiag i t (transpose lista)))))
                        (kolonaD (+ px (car poeni)) (+ po (cadr poeni)) (1+ i) lista)  
                    ) 
                )
                (t (let ((poeni (countPoints (getDiag i NIL (transpose lista)))))
                        (kolonaD (+ px (car poeni)) (+ po (cadr poeni)) (1+ i) lista)  
                    ) 
                )                
            )
        )
        ((= 6 (length (car lista)))
            (cond
                ((= i 8) (list px po))
                ((<= i 2)
                    (let ((poeni (countPoints (getDiag i t (transpose lista)))))
                        (kolonaD (+ px (car poeni)) (+ po (cadr poeni)) (1+ i) lista)  
                    ) 
                )
                (t (let ((poeni (countPoints (getDiag i NIL (transpose lista)))))
                        (kolonaD (+ px (car poeni)) (+ po (cadr poeni)) (1+ i) lista)  
                    ) 
                )                
            )
        )
    )
)

;racuna ukupne poene svih kolona
(defun racunajSveKolone(px po lista)
    (cond
        ((null lista) (list px po))
        (t (let ((poeni (kolonaBezD 0 0 (car lista)))
                 (poeniD (kolonaD 0 0 -2 (car lista))))
                (racunajSveKolone (+ px (car poeni) (car poeniD)) (+ po (cadr poeni) (cadr poeniD)) (cdr lista))
            )
        )
    )
)

;racunanje svih redova, transponovanjem matrice i koriscenjem funkcije za racunanje kolona
(defun racunajSveRedove(px po lista)
    (racunajSveKolone px po (transpose lista))
)

;konacna funkcija za racunanje  poena, koristeci sve funkcije iznad (racuna dijagonale u ravni, ali ne dijagonale u prostoru)
(defun racunajPoeneBezD(lista)
    (let ((poeni (countAllPillars 0 0 lista)))
        (setf poeni (racunajSveKolone (car poeni) (cadr poeni) lista))
        (setf poeni (racunajSveRedove (car poeni) (cadr poeni) lista))
    )
)

;racuna poene dijagonala u prostoru, start: i=-2
(defun racunajDijagonale(px po i lista)
     (cond 
        ((= 4 (length (car lista))) 
            (cond
                ((= i -2) 
                    (let ((poeni (kolonaD 0 0 -2 (getDiag 0 t lista)))
                          (poeniBezD (kolonaBezD 0 0 (getDiag 0 t lista))))
                        (racunajDijagonale (+ px (car poeni) (car poeniBezD)) (+ po (cadr poeni) (cadr poeniBezD)) 2 lista)
                    ) 
                )
                ((= i 2)
                    (let ((poeni (kolonaD 0 0 -2 (getDiag 3 NIL lista)))
                          (poeniBezD (kolonaBezD 0 0 (getDiag 3 NIL lista))))
                        (list (+ px (car poeni) (car poeniBezD)) (+ po (cadr poeni) (cadr poeniBezD)))
                    ) 
                )                
            )
        )
        ((= 6 (length (car lista)))
            (cond
                ((= i 8) (list px po))
                ((<= i 2)
                    (let ((poeni (kolonaD 0 0 -2 (getDiag i t lista)))
                            (poeniBezD (kolonaBezD 0 0 (getDiag i t lista))))
                        (racunajDijagonale (+ px (car poeni) (car poeniBezD)) (+ po (cadr poeni) (cadr poeniBezD)) (1+ i) lista)  
                    ) 
                )
                (t (let ((poeni (kolonaD 0 0 -2 (getDiag i NIL lista)))
                         (poeniBezD (kolonaBezD 0 0 (getDiag i NIL lista))))
                        (racunajDijagonale (+ px (car poeni) (car poeniBezD)) (+ po (cadr poeni) (cadr poeniBezD)) (1+ i) lista)
                    ) 
                )                
            )
        )
    )
)

;konacna funkcija za racunanje poena, vraca poene u obliku (X O)
(defun countFinalPoints(lista)
    (let ((poeni (racunajPoeneBezD lista))
          (poeniD (racunajDijagonale 0 0 -2 lista)))
        (list (+ (car poeni) (car poeniD)) (+ (cadr poeni) (cadr poeniD)))
    )
)

(defun checkForWinner(tabla)
  (format t "~%Checking for winner...")
  (let ((poeni (countFinalPoints tabla)))
        (format t "~%X: ~a~%O: ~a~%" (car poeni) (cadr poeni))
    )
  (exit)
)

;PROCENI STANJE - BEZ MASINE ZA ZAKLJUCIVANJE

(defun proceniStanje(stanje) 
    (let ((poeni (countFinalPoints stanje)))
       (if isFirstPlayer 
           (- (* 10 (cadr poeni)) (* 10 (car poeni)))
           (- (* 10 (car poeni)) (* 10 (cadr poeni)))
       )         
    )
    
)

;MINMAX SA ALPHABETA ODSECANJEM
(defun minimax (stanje dubina a b maxpl)
   (when (or (= dubina 0) (null (mogstanja (1- dim) #\O stanje) ))
     (return-from minimax (list (proceniStanje stanje) stanje)))
    (if maxpl
        (let ((value '-1000) (st '()))
          (dolist (temp (mogstanja (1- dim) (if isFirstPlayer #\O #\X) stanje))
            (setf tmpVal (minimax temp (1- dubina) a b '()))
            (if (> (car tmpVal) value) (setf st temp))
            (setf value (max value (car tmpVal)))
            (setf a (max a (car tmpVal)))
            (if (>= a b)
              (return))) (list value st))
      
      (let ((value 1000) (st '()))
        (dolist (temp (mogstanja (1- dim) (if isFirstPlayer #\X #\O) stanje))
          (setf tmpVal2 (minimax temp (1- dubina) a b t))
          (if (< (car tmpVal2) value) (setf st temp))
          (setf value (min value (car tmpVal2)))
          (setf b (min b (car tmpVal2)))
          (if (<= b a)
            (return))) (list value st))
                            ))
;MASINA ZAKLJUCIVANJA

(defun =dec (x) (1- x))
(defun =inc (x) (1+ x))
(defun !eq (sign el) (equalp sign el))


(defun =proveri-stub (lista i j z) 
                               (cond ((null lista) '()) 
                               ((equalp (car lista) #\X)(append (list 'On #\X i j z) (=proveri-stub (cdr lista) i j (1+ z)))) 
                               ((equalp (car lista) #\O)(append (list '(On #\O i j z)) (=proveri-stub (cdr lista) i j (1+ z))))
                               ((equalp (car lista) #\-)(=proveri-stub (cdr lista) i j (1+ z)))      
                               (t(=proveri-stub (cdr lista) i j (1+ z))) 
                                     ))
(defun =proveri-kolona (matrix i j z) 
                                     (cond ((null matrix) '())
                                     ((null (=proveri-stub (car matrix) i j z)) (=proveri-kolona (cdr matrix) i (1+ j) z))
                                     (t (cons  (=proveri-stub (car matrix) i j z) (=proveri-kolona (cdr matrix) i (1+ j) z)))
                                          ))
(defun =proveri-red (stanje i j z) 
                                     (cond ((null stanje) '())
                                     ((null (=proveri-kolona (car stanje) i j z)) (=proveri-red (cdr stanje) (1+ i) j z))
                                     (t (append  (=proveri-kolona (car stanje) i j z) (=proveri-red (cdr stanje) (1+ i) j z)))
                                           ))







;RULES

(setq *T1-RULES* '(
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el (=dec ?x) ?y ?z) (On ?el (=dec (=dec ?x)) ?y ?z) (On ?el (=dec (=dec (=dec ?x))) ?y ?z))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Dole)) 
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x (=dec ?y) ?z) (On ?el ?x (=dec (=dec ?y)) ?z) (On ?el ?x (=dec (=dec (=dec ?y))) ?z))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Dole))
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=dec ?z)) (On ?el ?x ?y (=dec (=dec ?z))) (On ?el ?x ?y (=dec (=dec (=dec ?z)))))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Dole))
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el (=inc ?x) ?y ?z) (On ?el (=inc (=inc ?x)) ?y ?z) (On ?el (=inc (=inc (=inc ?x))) ?y ?z))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Gore)) 
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x (=inc ?y) ?z) (On ?el ?x (=inc (=inc ?y)) ?z) (On ?el ?x (=inc (=inc (=inc ?y))) ?z))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Gore))
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=inc ?z)) (On ?el ?x ?y (=inc (=inc ?z))) (On ?el ?x ?y (=inc (=inc (=inc ?z)))))
                   THEN (Cetri-u-nizux ?el ?x ?y ?z 'Gore))           
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el (=dec ?x) ?y ?z) (On ?el (=dec (=dec ?x)) ?y ?z) (On ?el (=dec (=dec (=dec ?x))) ?y ?z))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Dole)) 
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x (=dec ?y) ?z) (On ?el ?x (=dec (=dec ?y)) ?z) (On ?el ?x (=dec (=dec (=dec ?y))) ?z))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Dole))
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=dec ?z)) (On ?el ?x ?y (=dec (=dec ?z))) (On ?el ?x ?y (=dec (=dec (=dec ?z)))))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Dole))
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el (=inc ?x) ?y ?z) (On ?el (=inc (=inc ?x)) ?y ?z) (On ?el (=inc (=inc (=inc ?x))) ?y ?z))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Gore)) 
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x (=inc ?y) ?z) (On ?el ?x (=inc (=inc ?y)) ?z) (On ?el ?x (=inc (=inc (=inc ?y))) ?z))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Gore))
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=inc ?z)) (On ?el ?x ?y (=inc (=inc ?z))) (On ?el ?x ?y (=inc (=inc (=inc ?z)))))
                   THEN (Cetri-u-nizuo ?el ?x ?y ?z 'Gore))                                            
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el (=dec ?x) ?y ?z) (On ?el (=dec (=dec ?x)) ?y ?z)) THEN (Tri-u-nizux ?el ?x ?y ?z 'Dole))
	(IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x (=dec ?y) ?z) (On ?el ?x (=dec (=dec ?y)) ?z)) THEN (Tri-u-nizux ?el ?x ?y ?z 'Dole)) 
	(IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=dec ?z)) (On ?el ?x ?y (=dec (=dec ?z)))) THEN (Tri-u-nizux ?el ?x ?y ?z 'Dole))	
	(IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el (=inc ?x) ?y ?z) (On ?el (=inc (=inc ?x)) ?y ?z)) THEN (Tri-u-nizux ?el ?x ?y ?z 'Gore))
	(IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x (=inc ?y) ?z) (On ?el ?x (=inc (=inc ?y)) ?z)) THEN (Tri-u-nizux ?el ?x ?y ?z 'Gore)) 
        (IF (AND (!eq #\X ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=inc ?z)) (On ?el ?x ?y (=inc (=inc ?z)))) THEN (Tri-u-nizux ?el ?x ?y ?z 'Gore))
        (IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el (=dec ?x) ?y ?z) (On ?el (=dec (=dec ?x)) ?y ?z)) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Dole))
	(IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x (=dec ?y) ?z) (On ?el ?x (=dec (=dec ?y)) ?z)) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Dole)) 
	(IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=dec ?z)) (On ?el ?x ?y (=dec (=dec ?z)))) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Dole))	
	(IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el (=inc ?x) ?y ?z) (On ?el (=inc (=inc ?x)) ?y ?z)) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Gore))
	(IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x (=inc ?y) ?z) (On ?el ?x (=inc (=inc ?y)) ?z)) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Gore)) 
	(IF (AND (!eq #\O ?el) (On ?el ?x ?y ?z) (On ?el ?x ?y (=inc ?z)) (On ?el ?x ?y (=inc (=inc ?z)))) THEN (Tri-u-nizuo ?el ?x ?y ?z 'Gore))           
                   ))
;FACTS
(defun genFacts(tabla)
    (=proveri-red tabla 0 0 0)
  )

;MASINA STANJA GLAVNA FUNKCIJA

(defun heuristicsConclusionMachine (tabla)
    (let* 
        (
            (*T1-FACTS* (genFacts tabla))  
        )  
        (progn
            (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
            
            (let*
                (
                    (acc1x (* (count-results '(Cetri-u-nizux ?el ?x ?y ?z 'Gore)) 10))
                    (acc1o (* (count-results '(Cetri-u-nizuo ?el ?x ?y ?z 'Gore)) -10))                   
                    (acc2x (* (count-results '(Tri-u-nizux ?el ?x ?y ?z 'Dole)) 5))                   
                    (acc2o (* (count-results '(Tri-u-nizuo ?el ?x ?y ?z 'Dole)) -5))
                    (acc3x (* (count-results '(Tri-u-nizux ?el ?x ?y ?z 'Gore)) 5 ))                   
                    (acc3o (* (count-results '(Tri-u-nizuo ?el ?x ?y ?z 'Gore)) -5 ))
                    (acc4x (* (count-results '(Cetri-u-nizux ?el ?x ?y ?z 'Dole)) 10))
                    (acc4o (* (count-results '(Cetri-u-nizuo ?el ?x ?y ?z 'Dole)) -10))
                    (acc (+ acc2x acc2o acc1x acc1o acc3x acc3o acc4x acc4o ))  
                )         
             acc
            )
        )
    )
  )


;INICIJALIZACIJA IGRE
(defun gameInit ()
    (setf nowPlaying #\X)
    (print "Unesite dimenzije table")
    (format t "~%a) 4~%b) 6~%")
    (setf input (read ))
    (cond 
        ((equal input 'a) (setf dim 4))
        ((equal input 'b) (setf dim 6))
        (t (progn (print "Greska pri unosu dimenzija") (gameInit)))
    )
    (setf movesToGo (expt dim 3))
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
    (if (equal t isFirstPlayer) (humanPlay) (botPlay))
)

(gameInit)
