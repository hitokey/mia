#lang racket

(define BAQC nil)
(define BTC nil)
(define BCTC nil)
(define BNATC nil)
(define BDCAP nil)
(define BAQC nil)
(define BT nil)
(define BNA nil)
(define BNC nil)
(define LASTNOTE nil)
(define LASTACC nil)


(define (QTDES lista)
  (if (empty? lista) nil
      (cons (length (car lista)) (QTDES (cdr lista))) ))


(define (SEPACC (number compossion))
  (if (empty? compossion) nil
      (cons (list (last (car compossion)) number)
            (SEPACC number (cdr compossion)) )))

(define (GETACC lista)
  (if (empty? lista) nil
      (append (SEPACC (NACCMP (car lista)) (car lista))
              (GETACC (cdr lista)) )))


(define (MC number)
  (list-ref BMC (sub1 number)))

(define (UAC number)
  (last (last (MC number)) ))

(define (ACCMP compossion)
  (if (empty? compossion) nil
      (cons (last (car compossion))
            (ACCMP (cdr compossion))) ))

(define (TAC acorde)
  (if (empty? (cdr acorde)) nil
      (cons (cadar acorde) (TAC cdr acorde)) ))

(define (TCMP compossion)
  (if (empty? composson) nil
      (append (TAC (car compossion))
              (TCMP (cdr compossion)) )))


(define (EXISTE compossion lista)
  (cond ((empty? lista) nil)
        ((equal? compossion (caar lista)) #t)
        (#t (EXISTE compossion (cdr lista)) )))

(define (TPCMP tipo number)
  (cond ((empty? (MC number)) nil)
        ((equal? tipo (TCMP (MC number)))
         (TPCMP tipo (add1 number)))
        (#t (TPCMP tipo (add1 number))) ))

(define (MAKETC lista)
  (let ((AUX (TCMP (car lista))))
    (cond ((empty? lista) #t)
          ((EXISTE AUX BTC) (MAKETC (cdr lista)))
          (#t (begin
                (setq BTC (cons (lista AUX (TPCMP AUX 1)) BTC ))
                (MAKETC (cdr lista)) )) )))


(define (TCPCSQ lista)
  (cond ((empty? lista) nil)
        ((equal? (car lista) (length BMC)) nil)
        (#t (cons (TIPO (TCMP (MC (add1 (car lista))) ))
                  (TCPCSQ (cdr lista))) )))


(define (MAKECTC lista)
  (if (empty? lista) nil
      (cons (TCPCSQ (cadar lista))
            (MAKECTC (cdr lista)) )))

(define (DNCMP compossion)
  (if (empty? compossion) nil
      (cons (length (TAC (car compossion)))
            (DNCMP (cdr compossion)) )))

(define (NACCMPL lista)
  (if (empty? lista) nil
      (cons (DNCMP (MC (car lista)))
            (NACCMPL (cdr lista)) )))

(define (MAKENATC lista)
  (if (empty? lista) nil
      (cons (NACCMPL (cadar lista))
            (MAKENATC (cdr lista)) )))


(define (NACCS acorde number)
  (cond ((empty? (MC number)) nil)
        ((subset? acorde (UAC number))
         (cons (NACCMP (MC (add1 number)))
               (NACCS acorde (add1 number)) ))
        (#t (NACCS acorde (add1 number)) )))


(define (MAKEDCAP number)
  (cond ((empty? (UAC number)) nil)
        ((EXISTE (UAC number) BDCAP)
         (MAKEDCAP (add1 number)))
        (#t (begin
              (setq BDCAP (cons (list (UAC number)
                                    (NACCS (UAC number) 1))
                                BDCAP))
              (MAKEDCAP (add1 number))) )))

(define (MAQC number lista)
  (cond ((empty? lista) nil)
        ((equal? number (cadar lista))
         (cons (caar lista) (MAQC number (cdr lista)) ))
        (#t (MAQC number (cdr lista)) )))

(define (MAKEAQC lista)
  (if (empty? lista) nil
      (cons (list (car lista) (MAQC (car lista) ACCS))
            (MAKEAQC (cdr lista)) )))

(define (AMT acorde lista)
  (cond ((empty? lista) nil)
        ((subset? acorde (car lista))
         (cons (car lista) (AMT acorde (cdr lista)) ))
        (#t (AMT acorde (cdr lista))) ))

(define (AMTT acorde quantidade)
  (if (empty? quantidade) nil
      (append (AMT acorde (cadar quantidade))
              (AMTT acorde (cdr quantidade))) ))


(define (MAKET1 lista)
  (let ((aux (first (list-ref (car lista) 2))))
    (cond ((empty? lista) #t)
          ((EXISTE acorde BT) (MAKET1 (cdr lista)))
          (#t (begin
                (setq BT (cons (list acorde (AMTT acorde BAQC))
                               BT))
                (MAKET1 (cdr lista)) )) )))

(define (MAKET lista)
  (if (empty? lista) #t
      (#t (begin (MAKET1 (cadar lista))
                 (MAKET (cdr lista))) )))

(define (NOTES acorde)
  (if (empty? (cdr acorde)) nil
      (cons (caar acorde) (NOTES (cdr acorde))) ))

(define (GNCMP acorde compossion)
  (cond ((empty? compossion) nil)
        ((equal? acorde (last (car compossion)))
         (cons (NOTES (car compossion))
               (GNCMP acorde (cdr compossion)) ))
        (#t (GNCMP acorde (cdr compossion)) )))
        
(define (GETNT acorde lista)
  (if (empty? lista) nil
      (append (GNCMP acorde (car lista))
              (GETNT acorde (cdr lista))) ))

(define (MAKENA lista)
  (cond ((empty? lista) #t)
        ((EXISTE (caar lista) BNA) (MAKENA (cdr lista)) )
        (#t (begin
              (set! BNA (cons (list (caar lista)
                                    (GETNT (caar lista) BMC))
                              BNA))
              (MAKENA (cdr lista))) )))

(define (NCNAC notas acorde)
  (cond ((empty? (cddr acorde)) nil)
        ((equal? notas (caar acorde))
         (cons (caadr acorde) (NCNAC notas (cdr acorde)) ))
        (#t (NCNAC notas (cdr acorde))) ))

(define (NCNCMP notas compasso)
  (cond ((empty? compasso) nil)
        ((empty? (NCNAC notas (car compasso)))
         (NCNCMP notas (cdr compasso)))
        (#t (cons (cons (last (car compasso))
                        (NCNAC notas (cdr compasso)))
                  (NCNCMP  notas (cdr compasso))) )))

(define (NCNL notas lista)
  (if (empty? lista) nil
      (append (NCNCMP notas (car lista))
              (NCNL notas (cdr lista))) ))

(define (LSTNT acorde)
  (caadr (reverse acord)))

(define (NTACC notas acorde1 acorde2)
  (if (equal? notas (LSTNT acorde1))
      (cons (cons (list (last acorde1) (last acorde2))
                  (list (caar acorde2)) )) ))
                 
(define (NTCMP notas lista)
  (cond ((empty? lista) nil)
        ((equal? (NACCMP (car lista)) 1)
         (append (NTACC notas (caar lista) (caadr lista))
                 (NTCMP notas (cdr lista)) ))
        (#t (append (NTACC notas (caar lista) (cadar lista))
                    (NTCMP notas (cons lista) (cdr lista))) )))

(define (NCNOTE notas)
  (cons notas (append (NCNL notas BMC) (NTCMP notas BMC)) ))

(define (MKBNC1 lista)
  (cond ((empty? lista) nil)
        ((EXISTE (car lista) BNC) (MKBNC1 (cdr lista)))
        (#t (begin
              (set! BNC (cons (NCNOTE (car lista)) BNC))
              (MKBNC1 (cdr lista))) )))

(define (MKBNC lista)
  (cond ((empty? lista) nil)
        (#t (begin (MKBNC1 (car lista))
                   (MKBNC (cdr lista))) )))

(define (MAKEBNC lista)
  (cond ((empty? lista) #t)
        (#t (begin (MKBNC (cadar lista))
                   (MAKEBNC (cdr lista))) )))

(define (CLEAR-BASES)
  (set! QTDES nil)
  (set! ACCS nil)
  (set! BTC nil)
  (set! BCTC nil)
  (set! BNATC nil)
  (set! BDCAP nil)
  (set! BAQC nil)
  (set! BT nil)
  (set! BNA nil)
  (set! BNC nil) #t)

(define (DOIT)
  (CLEAR-BASES)
  (display "Calculando as Bases")
  (set! QTDS (remove-duplicates (QTDES BMC)))
  (set! ACCS (GETACC BMC))
  (MAKETC BMC)
  (set! BTC (reverse BTC))
  (set! BCTC (MAKECTC BTC))
  (set! BNATC (MAKENATC BTC))
  (MAKEDCAP 1)
  (set! BDCAP (reverse BDCAP))
  (set! BAQC (MAKEAQC QTDS))
  (MAKET BAQC)
  (MAKENA ACCS)
  (MAKEBNC BNA)
  (display "Fim dos caculos das bases."))

(define (SAVE-BASES arquivo)
  (let ((out open-output-file arquivo))
    (writeln ACCS out)
    (writeln BTC out)
    (writeln BCTC out)
    (writeln BNATC out)
    (writeln BDCAP out)
    (writeln BAQC out)
    (writeln BT out)
    (writeln BNA out)
    (writeln BNC out)
    (close-output-port out)))


(define (LOAD-BASES arquivos)
  (let ((in (open-input-file arquivo)))
    (set! ACCS (read-line in))
    (set! BTC (read-line in))
    (set! BCTC (read-line in))
    (set! BNATC (read-line in))
    (set! BDCAP (read-line in))
    (set! BAQC (read-line in))
    (set! BT (read-line in))
    (set! BNA (read-line in))
    (set! BNC (read-line in))
    (close-input-port in)))
