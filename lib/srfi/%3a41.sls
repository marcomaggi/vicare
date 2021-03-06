;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (srfi :41)
  (export
    define-stream
    list->stream
    port->stream
    stream
    stream->list
    stream-append
    stream-car
    stream-cdr
    stream-concat
    stream-cons
    stream-constant
    stream-drop
    stream-drop-while
    stream-filter
    stream-fold
    stream-for-each
    stream-from
    stream-iterate
    stream-lambda
    stream-length
    stream-let
    stream-map
    stream-match
    stream-null
    stream-null?
    stream-of
    stream-pair?
    stream-range
    stream-ref
    stream-reverse
    stream-scan
    stream-take
    stream-take-while
    stream-unfold
    stream-unfolds
    stream-zip
    stream?)
  (import (srfi :41 streams)))

;;; end of file
