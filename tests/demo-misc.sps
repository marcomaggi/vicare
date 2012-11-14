;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!vicare
(import (vicare)
  (vicare debugging compiler))

#;(module (doit library-doit)

  (define-syntax doit
    (syntax-rules ()
      ((_ ?pass ?form)
       (let ((original-form ?form))
	 (%unparse-and-print original-form
			     (compile-up-to ?pass (%expand ?form)))))))

  (define-syntax library-doit
    (syntax-rules ()
      ((_ ?pass ?form)
       (let ((original-form ?form))
	 (%unparse-and-print original-form
			     (compile-up-to ?pass (%library-expand ?form)))))))

;;; --------------------------------------------------------------------

  (define (%expand form)
    (let-values
	(((code unused)
	  (expand-form-to-core-language form (environment '(vicare)))))
      code))

  (define (%library-expand form)
    (let-values (((id name ver imp* vis* inv*
		      invoke-code visit-code
		      export-subst export-env
		      guard-code guard-req*)
		  (expand-library form)))
      invoke-code))

;;; --------------------------------------------------------------------

  (define (%unparse-and-print original-form core-form)
    (%print original-form)
    (%display "==> ")
    (%print* ($unparse-recordized-code/pretty core-form) 3)
    (%display "\n\n"))

  (define (%print x)
    (pretty-print x (current-error-port)))

  (define (%print* x start)
    (pretty-print* x (current-error-port) start #t))

  (define (%display x)
    (let ((port (current-error-port)))
      (display x port)
      (flush-output-port port)))

  #| end of module: doit |# )


#;(library-doit $introduce-tags
 '(library (alpha)
    (export ciao)
    (import (vicare))
    (define (ciao)
      (hello))
    (define (hello)
      123)))

(define (ciao)
  123)

(display (ciao))

;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
