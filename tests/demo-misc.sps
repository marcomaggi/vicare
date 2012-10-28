;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!vicare
(import (vicare)
  (ikarus system $compiler))

(define (print arg)
  (pretty-print arg (current-error-port)))

(define source
  '(let loop ((i 0))
     (when (zero? (mod i #e1e6))
       (fprintf (current-error-port) "~a " i)
       (flush-output-port (current-error-port))
       (void))
     (when (< i #e1e9)
       (cons i (loop (+ 1 i))))))

(define records
  (let-values
      (((code libs) (expand-core-language-form source (interaction-environment))))
    (print code)
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
