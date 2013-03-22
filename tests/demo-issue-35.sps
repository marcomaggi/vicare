;;; demo-issue-35.sps --
;;
;;Run this script with debugging mode turned on.
;;

#!r6rs
(import (ikarus))

(let loop ((i 0))
  (when (zero? (mod i #e1e6))
    (let ((bv (string->utf8 (string-append (number->string i) " "))))
      (foreign-call "ikrt_write_fd" 2 bv 0 (bytevector-length bv)))
    (void))
  (loop (+ 1 i)))

;; (let loop ((i 0))
;;   (when (zero? (mod i #e1e3))
;;     (display (string-append (number->string i) " ")
;; 	     (current-error-port))
;;     (flush-output-port (current-error-port))
;;     (void))
;;   (loop (+ 1 i)))

;; (let loop ((i 0))
;;   (when (zero? (mod i #e1e3))
;;     (fprintf (current-error-port) "~a " i)
;;     (flush-output-port (current-error-port))
;;     (void))
;;   (loop (+ 1 i)))

;;; end of file
;;Local Variables:
;;coding: utf-8
;;End:
