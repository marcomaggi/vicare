;; demo-debugging-compiler.sps --
;;

#!vicare
(import (vicare)
  (vicare debugging compiler))

(define (%print x)
  (pretty-print x (current-error-port)))

(define (doit form)
  (%print
   ($unparse-recordized-code/pretty
    (compile-up-to $optimize-letrec
      (let-values
	  (((code unused)
	    (expand-form-to-core-language form (interaction-environment))))
	code)))))

(doit '(letrec ((a 1)
		(b 2))
	 (list a b)))

(doit '(letrec* ((a (lambda (x)
		      (when x
			(a #f))))
		 (b 123)
		 (c 456)
		 (d (begin
		      (set! c 789)
		      9)))
	 a))

;;; end of file
;; Local Variables:
;; eval: (put 'compile-up-to 'scheme-indent-function 1)
;; End:
