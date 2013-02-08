;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :42)
  (export
    :
    :-dispatch-ref
    :-dispatch-set!
    :char-range
    :dispatched
    :do
    :generator-proc
    :integers
    :let
    :list
    :parallel
    :port
    :range
    :real-range
    :string
    :until
    :vector
    :while
    any?-ec
    append-ec
    dispatch-union
    do-ec
    every?-ec
    first-ec
    fold-ec
    fold3-ec
    last-ec
    list-ec
    make-initial-:-dispatch
    max-ec
    min-ec
    product-ec
    string-append-ec
    string-ec
    sum-ec
    vector-ec
    vector-of-length-ec)
  (import (srfi :42 eager-comprehensions)))

;;; end of file
