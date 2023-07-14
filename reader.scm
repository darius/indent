(define (try filename)
  (for-each (compose print post-read)
	    (read-file filename)))

; The reader's abstract grammar is a bit different from s-expressions:
; an expression for it is an atom with a list of arguments, which is
; represented in an s-expression as (atom . arguments).  Thus, what in
; Lisp is an atom, here is an atom with no arguments, represented as 
; (atom).  Post-read goes over the result represented that way and
; converts it into the sort of s-expression you'd expect in Lisp.
;
; (We could have had the reader just return things that way in the 
; first place, but it seemed like if we aren't trying to imitate Lisp
; directly, then what read-file returns may be a simpler way of 
; representing things.)
(define (post-read se)
  (cond ((pair? se)
	 (if (null? (cdr se))
	     (post-read (car se))
	     (map post-read se)))
	(else se)))

(define (read-file filename)
  (read-indented (snarf-chars filename)))

(define (read-indented chars)
  (reading (scanning chars)))

; Pre: TOKENS ends with an indent token to column -1
; Consume the whole stream and return a parsed s-expression.
(define (reading tokens)
  (read* #f #t 0 tokens '() 
	 (lambda (x tokens) x)))

; Pre: TOKENS is nonempty
; Read through TOKENS up to where its margin is < MARGIN, and return
; (K a t), where t is the remaining tail of TOKENS, and a is the 
; list of s-expressions parsed from the head and appended to the 
; reverse of ACCUM.  FIRST-LINE? and FIXED? affect the parse.
(define (read* first-line? fixed? margin tokens accum k)
  (let ((t (car tokens)))
    (if (< (token.column t) margin)
	(k (reverse accum) tokens)
	(case (token.tag t)
	  ((:) (panic "Unexpected colon") #f)
	  ((indent atom) 
	   (let ((new-margin (if fixed? margin (token.column t))))
	     (read1 new-margin tokens
		    (lambda (x tokens)
		      (read* (and first-line? 
				  (not (eq? 'indent (token.tag t))))
			     (not first-line?)
			     (if first-line? margin new-margin)
			     tokens
			     (cons x accum)
			     k)))))
	  (else (impossible))))))

; Like READ*, except tokens on the current line are just added to
; ACCUM.
(define (read-flat margin tokens accum k)
  (let ((t (car tokens)))
    (case (token.tag t)
      ((:) (panic "Unexpected colon") #f)
      ((indent) (read* #t #f margin tokens accum k))
      ((atom) (read-flat margin (cdr tokens)
			 (cons (list (token.value t)) accum) 
			 k))
      (else (impossible)))))

; Read one s-expression s from TOKENS, and return (K s t), where t is
; the remaining tail of TOKENS.
(define (read1 margin tokens k)
  (if (null? tokens)
      (panic "Unexpected EOF"))
  (let ((t (car tokens))
	(tokens (cdr tokens)))
    (case (token.tag t)
      ((:)      (panic "Unexpected colon") #f)
      ((indent) (if (not (= margin (token.column t)))
		    (panic "Bad indentation"))
		(read1 margin tokens k))
      ((atom)   (let ((v (token.value t)))
		  (if (there-follows? ': tokens)
		      (read-flat (+ margin 1) (cdr tokens) (list v) k)
		      (read* #t #f (+ margin 1) tokens (list v) k))))
      (else (impossible)))))

; Return true iff TOKENS starts with TAG.
(define (there-follows? tag tokens)
  (and (not (null? tokens))
       (eq? tag (token.tag (car tokens)))))
