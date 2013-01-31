;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :25)
  (export
    array
    array-end
    array-rank
    array-ref
    array-set!
    array-start
    array?
    make-array
    shape
    share-array)
  (import (srfi :25 multi-dimensional-arrays)))

;;; end of file
