(load "scanner.scm")
(load "reader.scm")
(load "writer.scm")

(define (panic message)
  (error message))


; Helpers

(define (print x)
  (write x)
  (newline))

(define (snarf-chars filename)
  (snarf read-char filename))

(define (snarf reader filename)
  (call-with-input-file filename
    (lambda (port)
      (let snarfing ((xs '()))
        (let ((x (reader port)))
          (if (eof-object? x)
              (reverse xs)
	      (snarfing (cons x xs))))))))
