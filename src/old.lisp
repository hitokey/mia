(defvar *base*
  '((((b5 m)(c#4 sm)(c#4 sm)(A + 7 1)))
    (((b5 smpp)(d4 sc)(d4 m)(D - () 1)))
    (((p sm)(e4 sm)(f#4 sm)(g4 sm)(A + 7 2)))
    (((a5 m)(F# dim () 1))((a5 m)(B + 7 1)))
    (((a5 sm)(g4 sm)(f#4 sm)(e4 sm)(E - () 1)))
    (((d4 sm)(c#4 sm)(c4 sm)(b4 sm)(D + () 1)))
    (((bb4 m)(G - 6 1))((a4 m)(A + 7 3)))
    (((p sb)(D + () 2)) )))


(defun size(l)
  (if (null l) 0
      (+ (size (cdr l)) 1)))

(defun lst(l)
  (cond ((null l) '())
	((atom l) '())
	((null (cdr l)) (car l))
	(t (lst (cdr l)) )))

(defun naccmp(l)
  (size l))

(defun qtdes(l)
  (if (null l) '()
      (cons (naccmp (car l)) (qtdes (cdr l))) ))


(defun sepacc(n c)
  (if (null c) '()
      (cons (list (lst (car c)) n)
	    (sepacc n (cdr c)) )))

(defun getacc(l)
  (if (null l) '()
      (append (sepacc (naccmp (car l)) (car l))
	      (getacc (cdr l)) )))

(defun mc(n &optional (l *bmc*))
  (nth (- n 1) l)) 

(defun uac (n)
  (lst (lst (mc n))) )

(defun accmp(c)
  (if (null c) '()
      (cons (lst (car c)) (accmp (cdr c))) ))

(defun tc (n &optional (l *btc*))
  (nth (- n 1) l))


(defun tipo(c &optional (n 1))
  (cond ((null (tc n)) '())
	((equal c (car (tc n))) n)
	(t (tipo c (+ n 1)) )))



(defun tac(ac)
  (if (null (cdr ac)) nil
      (cons (cadar ac)
	    (tac (cdr ac)) )))

(defun ctc(n)
  (nth (- n 1) *bctc*))


(defun tcmp(c)
  (if (null c) '()
      (append (tac (car c))
	      (tcmp (cdr c)) )))
