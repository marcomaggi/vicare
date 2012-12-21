;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :6 basic-string-ports)
  (export
    (rename (open-string-input-port open-input-string))
    open-output-string
    get-output-string)
  (import (vicare))
  (define (open-output-string)
    (let-values (((port getter)
		  (open-string-output-port)))
      port)))

;;; end of file
