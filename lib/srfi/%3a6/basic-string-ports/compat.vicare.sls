#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi :6 basic-string-ports compat)
  (export open-output-string get-output-string)
  (import (vicare))
  (define (open-output-string)
    (let-values (((port getter)
		  (open-string-output-port)))
      port)))

;;; end of file
