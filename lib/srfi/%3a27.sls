;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :27)
  (export
    default-random-source
    make-random-source
    random-integer
    random-real
    random-source-make-integers
    random-source-make-reals
    random-source-pseudo-randomize!
    random-source-randomize!
    random-source-state-ref
    random-source-state-set!
    random-source?)
  (import (srfi :27 random-bits)))

;;; end of file
