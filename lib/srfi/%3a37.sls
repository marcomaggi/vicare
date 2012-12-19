;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named  LICENSE.srfi  from  the   original  collection  this  file  is
;;;distributed with.

#!r6rs
(library (srfi :37)
  (export
    args-fold
    option
    option-names
    option-optional-arg?
    option-processor
    option-required-arg?
    option?)
  (import (srfi :37 args-fold)))

;;; end of file
