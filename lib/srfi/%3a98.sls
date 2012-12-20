;;;Copyright 2008-2010 Derick Eddington.  My MIT-style license is in the
;;;file named  LICENSE.srfi from  the original  collection this  file is
;;;distributed with.

#!r6rs
(library (srfi :98)
  (export get-environment-variable
	  get-environment-variables)
  (import (srfi :98 os-environment-variables)))

;;; end of file
