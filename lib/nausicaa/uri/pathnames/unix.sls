;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: Unix file pathnames
;;;Date: Sat Apr  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa uri pathnames unix)
  (export
    <pathname>
    <relative-pathname>
    <absolute-pathname>
    <relative-unix-pathname>
    <absolute-unix-pathname>
    pathname
    &unix-pathname-parser-error
    &unix-pathname-normalisation-error)
  (import (nausicaa)
    (vicare unsafe operations)
    (nausicaa uri pathnames abstract)
    (prefix (vicare parser-tools unix-pathnames) parser.))


;;;; labels shadowing condition objects

(define-label &unix-pathname-parser-error
  (parent &error)
  (shadows parser.&unix-pathname-parser-error)
  (protocol (lambda () parser.make-unix-pathname-parser-error))
  (predicate parser.unix-pathname-parser-error?))

(define-label &unix-pathname-normalisation-error
  (parent &error)
  (shadows parser.&unix-pathname-normalisation-error)
  (protocol (lambda () parser.make-unix-pathname-normalisation-error))
  (predicate parser.unix-pathname-normalisation-error?))


(define-class <absolute-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<absolute-unix-pathname>)
  (parent <absolute-pathname>)

  (protocol
   (lambda (make-absolute-pathname)
     (lambda* ((ptn uxptn.pathname?))
       (define who 'make-<relative-unix-pathname>)
       ((make-absolute-pathname)
	(uxptn.$bytevector-normalise (uxptn.string/bytevector->pathname-bytevector ptn who))))))

  (fields (immutable (pathname <bytevector>)))

  #| end of class |# )


(define-class <relative-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<relative-unix-pathname>)
  (parent <relative-pathname>)

  (protocol
   (lambda (make-relative-pathname)
     (lambda* ((ptn uxptn.pathname?))
       (define who 'make-<relative-unix-pathname>)
       ((make-relative-pathname)
	(uxptn.$bytevector-normalise (uxptn.string/bytevector->pathname-bytevector ptn who))))))

  (fields (immutable (pathname <bytevector>)))

  #| end of class |# )


(define (pathname obj)
  (let (((bv <bytevector-u8>) (parser.string/bytevector->pathname-bytevector obj __who__)))
    (cond ((and ($bytevector-not-empty? bv)
		($ascii-chi-slash? (bv[0])))
	   (<absolute-unix-pathname> (bv)))
	  (else
	   (<relative-unix-pathname> (bv))))))


;;;; done

)

;;; end of file
