;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :8 receive)
  (export receive)
  (import (rnrs))
  (define-syntax receive
    (syntax-rules ()
      ((_ formals expression b b* ...)
       (call-with-values
	   (lambda () expression)
         (lambda formals b b* ...))))))

;;; end of file
