(load "base.lisp")
(load "old.lisp")


(equal (chord-amount *base*)
       (qtdes *base*))

(equal (split-chord 1 *base*)
       (sepacc 1 *base*))

(equal (split-chords *base*)
       (getacc *base*))



(equal (time-of-chord (caar *base*))
       (tac (caar *base*)))

