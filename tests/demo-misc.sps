;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!vicare
(import (vicare))

(define sexp
  (expand-library->sexp '(library (demo)
			   (export)
			   (import (vicare))
			   (define* (ciao . {x fixnum?})
			     x))))

(debug-print (assq 'invoke-code sexp))

;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
