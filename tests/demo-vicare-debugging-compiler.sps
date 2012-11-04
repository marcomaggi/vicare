;; demo-debugging-compiler.sps --
;;

#!vicare
(import (vicare)
  (vicare debugging compiler))

(module (doit)

  (define (doit form)
    (%print form)
    (%display "==> ")
    (%print* ($unparse-recordized-code/pretty
	      (compile-up-to
		  $optimize-letrec
		  #;$specify-representation
		  #;$source-optimize
		(expand form)))
	     3)
    (%display "\n\n"))

  (define (expand form)
    (let-values
	(((code unused)
	  (expand-form-to-core-language form (environment '(vicare)))))
      code))

  (define (%print x)
    (pretty-print x (current-error-port)))

  (define (%print* x start)
    (pretty-print* x (current-error-port) start #t))

  (define (%display x)
    (let ((port (current-error-port)))
      (display x port)
      (flush-output-port port)))

  #| end of module: doit |# )


;; (doit '(let ((a 1))
;; 	 (let ((a a))
;; 	   a)))

;; (doit '(let ((a 1))
;; 	 (let ((a 2))
;; 	   (let ((a 3))
;; 	     a))))

;; (doit '(letrec ((a 1)
;; 		(b 2))
;; 	 (list a b)))

;; (doit '(letrec* ((a (lambda (x)
;; 		      (when x
;; 			(a #f))))
;; 		 (b 123)
;; 		 (c 456)
;; 		 (d (begin
;; 		      (set! c 789)
;; 		      9)))
;; 	 a))

;; (doit '(letrec* ((a 123)
;; 		 (b 2)
;; 		 (c b)
;; 		 (d (lambda () 123)))
;; 	 b))

;; (doit '(letrec* ((a 123)
;; 		 (b 2)
;; 		 (c b)
;; 		 (d (lambda () 123)))
;; 	 (set! d 123)
;; 	 b))

;; (parametrise ((optimize-level 2))
;;   (doit '(letrec* ((f (lambda () (g)))
;; 		   (g (lambda () (f))))
;; 	   (list f g))))

;; (parametrise ((optimize-level 2))
;;   (doit '(letrec* ((f (lambda () (g)))
;; 		   (g (lambda () (f))))
;; 	   123)))

;; (parametrise ((optimize-level 2))
;;   (doit '(letrec* ((f (lambda () 1))
;; 		   (g (lambda () 2)))
;; 	   123)))

(parametrise ((optimize-level 2))
  (doit '(letrec ((a (display "ciao"))
		  (b (lambda (x) a (list x))))
	   (set! a 123)
	   123)))

;; (parametrise ((optimize-level 2))
;;   (doit '((lambda (a)
;; 	    (set! a 123)
;; 	    (display "ciao"))
;; 	  123)))

;; (parametrise ((optimize-level 2))
;;   (doit '((lambda (x y . rest) (list x y rest))
;; 	  123 456 7 8 9)))



#;(doit '(list (display "ciao\n")))

;;; end of file
;; Local Variables:
;; eval: (put 'compile-up-to 'scheme-indent-function 1)
;; End:
