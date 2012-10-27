;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!vicare
(import (vicare)
  (ikarus system $compiler))

(define (print arg)
  (pretty-print arg (current-error-port)))

(let-values (((id name ver imp* vis* inv*
		  invoke-code visit-code
		  export-subst export-env
		  guard-code guard-req*)
	      (expand-library
	       '(library (doit)
		  (export doit)
		  (import (vicare))
		  (define (doit x)
		    (if (null? x)
			#f
		      (doit (cdr x))))))))
  (print invoke-code))

#!eof
#;(define source
  '(let loop ((i 0))
     (when (zero? (mod i #e1e6))
       (fprintf (current-error-port) "~a " i)
       (flush-output-port (current-error-port))
       (void))
     (when (< i #e1e9)
       (cons i (loop (+ 1 i))))))

(define source
  '(library (this)
     (export a)
     (import (vicare))
     (define (a)
       (let loop ((i 0))
	 (when (zero? (mod i #e1e6))
	   (fprintf (current-error-port) "~a " i)
	   (flush-output-port (current-error-port))
	   (void))
	 (when (< i #e1e9)
	   (cons i (loop (+ 1 i))))))
     ))

(define records
  (let-values
      #;(((code libs) (core-expand source (interaction-environment))))
      (((id name ver imp* vis* inv*
	    code #;invoke-code visit-code
	    export-subst export-env
	    guard-code guard-req*)
	(library-expander source)))
    (print inv*)
    (let* ((R ($compiler.recordize code))
	   (R ($compiler.optimize-direct-calls R))
	   (R ($compiler.optimize-letrec R))
	   (R ($compiler.source-optimize R))
	   (R ($compiler.rewrite-references-and-assignments R))
	   (R ($compiler.introduce-tags R))
	   (R ($compiler.introduce-vars R))
	   (R ($compiler.sanitize-bindings R))
	   (R ($compiler.optimize-for-direct-jumps R))
	   (R ($compiler.insert-global-assignments R))
	   (R ($compiler.convert-closures R))
	   (R ($compiler.optimize-closures/lift-codes R))
	   #;(R ($compiler.alt-cogen R))
	   )
      R)))

(print ($unparse-recordized-code records))
#;(print ($unparse-recordized-code/pretty records))


;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
