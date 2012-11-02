;;; demo-issue-35.sps --
;;
;;Run this script with debugging mode turned on.
;;

#!r6rs
(import (vicare))

(let loop ((i 0))
  (when (zero? (mod i #e1e6))
    (fprintf (current-error-port) "~a " i)
    (flush-output-port (current-error-port))
    (void))
  (loop (+ 1 i)))

;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
