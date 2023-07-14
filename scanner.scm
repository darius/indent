(define (scanning chars)
  (tokenize (columnize chars)))

(define make-token list)
(define token.tag car)
(define token.column cadr)
(define token.value caddr)

(define (tokenize inputs)
  (if (null? inputs)
      (list (make-token 'indent -1 #f))
      (let ((column (input.column (car inputs))))
	   
	(define (scan-indentation inputs)
	  (if (null? inputs)
	      (list (make-token 'indent -1 #f))
	      (case (input.char (car inputs))
		((#\space #\tab #\newline)
		 (scan-indentation (cdr inputs)))
		(else
		 (cons (make-token 'indent (input.column (car inputs)) #f)
		       (tokenize inputs))))))

	; Pre: INPUTS is nonempty
	(define (scan-atom inputs)
	  (do ((inputs inputs (cdr inputs))
	       (chars '() (cons (input.char (car inputs))
				chars)))
	      ((or (null? inputs)
		   (not (constituent? (input.char (car inputs)))))
	       (cons (make-token 'atom column
				 (cook-atom (reverse chars)))
		     (tokenize inputs)))))

	(define (constituent? char)
	  (not (memv char '(#\space #\tab #\newline #\:))))

	(define (cook-atom chars)
	  (let ((str (list->string chars)))
	    (or (string->number str)
		(string->symbol str))))

	(case (input.char (car inputs))
	  ((#\space #\tab)
	   (tokenize (cdr inputs)))
	  ((#\newline)
	   (scan-indentation (cdr inputs)))
	  ((#\:)
	   (cons (make-token ': column #f)
		 (tokenize (cdr inputs))))
	  (else
	   (scan-atom inputs))))))

(define (columnize chars)
  (do ((column 0 (next-column column (car chars)))
       (chars chars (cdr chars))
       (result '() (cons (make-input (car chars) column)
			 result)))
      ((null? chars)
       (reverse result))))

(define make-input cons)
(define input.char car)
(define input.column cdr)

(define (next-column column char)
  (case char
    ((#\newline) 0)
    ((#\tab)	 (+ column (- 8 (modulo column 8))))
    (else        (+ column 1))))
