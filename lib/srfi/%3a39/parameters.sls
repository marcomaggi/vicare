;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named  LICENSE.srfi  from  the   original  collection  this  file  is
;;;distributed with.

#!r6rs
(library (srfi :39 parameters)
  (export make-parameter parameterize)
  (import (only (vicare)
		make-parameter
		parameterize)))

;;; end of file
