;;; demo-misc.sps --
;;
;;Demo miscellaneous stuff.
;;

#!r6rs
(import (vicare))

(let loop ((i 0))
  (when (zero? (mod i #e1e1))
    (fprintf (current-error-port) "~a " i)
    (flush-output-port (current-error-port))
    (void))
  (when (< i 100)
    (pretty-print (cons i (loop (+ 1 i)))
		  (current-error-port))))


;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
