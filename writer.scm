(define comfortable-width 32)

(define (print-indented x)
  (let ((column 0))

    (define (nl)
      (newline)
      (set! column 0))

    (define (show str)
      (display str)
      (set! column (+ column (string-length str))))

    (define (output atom)
      (show (coerce-string atom)))

    (define (indent c)
      (do ()
	  ((<= c column))
	(show " ")))

    (define (print x)
      (cond ((not (pair? x))
	     (output x)
	     (nl))
	    (else
	     (let ((x (normalize-list x)))
	       (output (car x))
	       (let ((rest (cdr x)))
		 (cond ((and (not (= (length rest) 1))
			     (one-liner? (+ column 1) rest))
			(show ":")
			(print-one-line rest))
		       (else
			(print-each rest))))))))

    (define (normalize-list ls)
      (if (pair? (car ls))
	  (cons (string->symbol "(") ls)
	  ls))

    (define (one-liner? column ls)
      (and (all atom? ls)
	   (<= (+ column (one-liner-length ls)) 
	       comfortable-width)))

    (define (one-liner-length ls)
      (+ (length ls)
	 (sum (map (compose string-length coerce-string) ls))))
			     
    (define (print-one-line ls)
      (for-each (lambda (x)
		  (show " ")
		  (output x))
		ls)
      (nl))

    ; Pre: LS is nonempty
    (define (print-each ls)
      (show " ")
      (for-each (let ((c column))
		  (lambda (arg) 
		    (indent c)
		    (print arg)))
		ls))

    (print x)))

; FIXME: rename this.
(define coerce-string
  (lambda (x)
    (cond ((symbol? x) (symbol->string x))
	  ((string? x) (string-append "\"" x "\"")) ;FIXME: escaping
	  ((number? x) (number->string x))
	  ((null? x)   "()")
	  ((boolean? x) (if x "#t" "#f"))
	  (else (impossible)))))

(define (atom? x)
  (or (symbol? x)
      (string? x)
      (number? x)
      (boolean? x)))

(define (all f ls)
  (or (null? ls)
      (and (f (car ls))
	   (all f (cdr ls)))))

(define (sum ls)
  (if (null? ls)
      0
      (+ (car ls) (sum (cdr ls)))))

(define (compose f g)
  (lambda (x) (f (g x))))
