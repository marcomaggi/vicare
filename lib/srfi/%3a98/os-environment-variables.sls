;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named  LICENSE.srfi  from  the   original  collection  this  file  is
;;;distributed with.

#!r6rs
(library (srfi :98 os-environment-variables)
  (export (rename (getenv  get-environment-variable)
		  (environ get-environment-variables)))
  (import (only (vicare)
		getenv
		environ)))

;;; end of file
