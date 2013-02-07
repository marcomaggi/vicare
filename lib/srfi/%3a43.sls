;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :43)
  (export
    list->vector
    make-vector
    reverse-list->vector
    reverse-vector->list
    vector
    vector->list
    vector-any
    vector-append
    vector-binary-search
    vector-concatenate
    vector-copy
    vector-copy!
    vector-count
    vector-empty?
    vector-every
    vector-fill!
    vector-fold
    vector-fold-right
    vector-for-each
    vector-index
    vector-index-right
    vector-length
    vector-map
    vector-map!
    vector-ref
    vector-reverse!
    vector-reverse-copy
    vector-reverse-copy!
    vector-set!
    vector-skip
    vector-skip-right
    vector-swap!
    vector-unfold
    vector-unfold-right
    vector=
    vector?)
  (import (srfi :43 vectors)))

;;; end of file
