;;#+AUTHOR: Pedro Fernandes
;;#+FILE: base.lisp

(defparameter *bmc* nil) ;; base de compassos; beats of base
(defparameter *btc* nil) ;; tipos de compassos; beats of type
(defparameter *bctc* nil) ;; base de tipo compassos Consequentes; 

(defun chord-amount(xl)
  "Input um lista do tipo de *bmc*, retorna um lista
   de inteiro com a quantidade de acorde."
  (if (null xl) nil
      (cons (length (car xl)) (chord-amount (cdr xl))) ))

(defun split-chord(n beats)
  "Input number e lista compassos, inteiro e compassos *bmc*, retorna 
  uma lista com os compasso marcados com n."
  (if (null beats) nil
      (cons (list (car (last (car beats)))  n)
	    (split-chord n (cdr beats)))) )


(defun split-chords(beats)
  "Input compasso *bmc*, retorna um lista com somentes os acordes
   do lista de compassos em ordem e classficados pela quantidade de cada compasso" 
  (if (null beats) nil
      (append (split-chord (length (car beats)) (car beats))
	      (split-chords (cdr beats)) )))

(defun select-the-nth(n &optional (l *bmc*))
  "Input n and opcional lista de compassos, retorne o campasso n-essimo"
  (nth (- n 1) l))


(defun chord-last-select-n(n &optional (l *bmc*))
  "Input n e opcional lista de compassos, retorna o ultimo acorde do 
   compasso n"
  (car (last (car (select-the-nth n l)) )))

(defun chord-chords(beats)
  "Input compasso, retorna uma lista com os acorde do compasso"
  (if (null beats) '()
      (cons (car (last (car beats)))
	    (car (chord-chords (cdr beats)) ))))

(defun type-of-beats(beats &optional (n 1))
  "Input um compasso, busca de um compasso semente, retorna o
   indice indicando sua localization"
  (cond ((null (select-the-nth n *btc*)) nil)
	((equal beats (car (select-the-nth n *btc*))) n)
	(t (type-of-beats beats (+ n 1)) )))


(defun time-of-chord(chord)
  "Input acorde, ira retorna um lista de todos os tempos,
  para cada nota do acorde."
  (if (null (cdr chord)) nil
      (cons (cadar chord)
	    (time-of-chord (cdr chord))) ))


(defun time-of-beats(beats)
  "Input um compasso, retorna uma lista com os
   tempo do compasso."
  (if (null beats) nil
      (append (time-of-chord (car beats))
	      (time-of-beats (cdr beats))) ))


(defun exist-beats(beats &optional (ls *btc*))
  "Input time do compasso, verifica se o campasso de entrada
   existe na lista de entrada, por default a lista e btc"
  (cond ((null ls) nil)
	((equal beats (caar ls)) t)
	(t (exist-beats beats (cdr ls)) ))) 


(defun types-of-beats(type &optional (n 1))
  (cond ((null (select-the-nth n)) nil)
	((equal type (time-of-beats (select-the-nth n)))
	 (cons n (types-of-beats type (+ n 1)) ))
	(t (types-of-beats type (+ n 1)) )))


(defun make-btc(ls)
  (let ((aux (time-of-beats (car ls)) ))
    (cond ((null ls) t)
	  ((exist-beats aux *btc*) (make-btc (cdr ls)))
	  (t (setq *btc* (cons (list aux (types-of-beats aux 1)) *btc*))
	     (make-btc (cdr ls)) ))))

(defun make-bctc(ls)
  (cond ((null ls) nil)
	((equal (car ls) (s
