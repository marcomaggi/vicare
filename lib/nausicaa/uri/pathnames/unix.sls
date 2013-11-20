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


;;; helpers

(define-inline-constant CHI-SLASH	47)
(define-inline-constant BV-SINGLE-DOT	'#vu8(46))
(define-inline-constant PAIR-SINGLE-DOT	'(#vu8(46)))

(define-inline ($ascii-chi-slash? chi)
  ($fx= CHI-SLASH chi))

(define (%drop-last-pair ell)
  (if (null? (cdr ell))
      '()
    (cons (car ell) (%drop-last-pair (cdr ell)))))

(define (%replace-last-pair ell replacement)
  (if (null? (cdr ell))
      replacement
    (cons (car ell) (%replace-last-pair (cdr ell) replacement))))


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


(module (<absolute-unix-pathname>)

  (define-class <absolute-unix-pathname>
    (nongenerative nausicaa:uri:pathnames:unix:<absolute-unix-pathname>)
    (parent <absolute-pathname>)

    (protocol
     (lambda (make-absolute-pathname)
       (lambda (obj)
	 (%make-absolute-unix-pathname obj make-absolute-pathname))))

    #| end of class |# )

  (define (%make-absolute-unix-pathname obj make-absolute-pathname)
    ;;Recursive function.
    ;;
    (define __who__ 'make-<absolute-unix-pathname>)
    (cond ((string? obj)
	   (%make-absolute-unix-pathname ($string->ascii obj)))
	  ((bytevector? obj)
	   (let ((port (open-bytevector-input-port obj)))
	     (receive (absolute? original-segments)
		 (parser.parse-pathname port)
	       (unless absolute?
		 (assertion-violation __who__ "expected absolute pathname" obj))
	       (receive (changed? normalised-segments)
		   (parser.normalise-pathname absolute? original-segments)
		 ((make-absolute-pathname (if changed?
					      (parser.serialise-segments absolute? normalised-segments)
					    (bytevector-copy obj))
					  normalised-segments))))))

	  ((parser.list-of-segments? obj)
	   ((make-absolute-pathname (parser.serialise-segments #t obj) obj)))

	  (else
	   (procedure-argument-violation __who__ "invalid argument type" obj))))

    #| end of module |# )


(module (<relative-unix-pathname>)

  (define-class <relative-unix-pathname>
    (nongenerative nausicaa:uri:pathnames:unix:<relative-unix-pathname>)
    (parent <relative-pathname>)

    (protocol
     (lambda (make-relative-pathname)
       (lambda (obj)
	 (%make-relative-unix-pathname obj make-relative-pathname))))

    #| end of class |# )

  (define (%make-relative-unix-pathname obj make-relative-pathname)
    ;;Recursive function.
    ;;
    (cond ((string? obj)
	   (%make-relative-unix-pathname ($string->ascii obj)))

	  ((bytevector? obj)
	   (let ((port (open-bytevector-input-port obj)))
	     (receive (absolute? original-segments)
		 (parser.parse-pathname port)
	       (when absolute?
		 (assertion-violation __who__ "expected relative pathname" obj))
	       (receive (changed? normalised-segments)
		   (parser.normalise-pathname absolute? original-segments)
		 ((make-relative-pathname (if changed?
					      (parser.serialise-segments absolute? normalised-segments)
					    (bytevector-copy obj))
					  normalised-segments))))))

	  ((parser.list-of-segments? obj)
	   ((make-relative-pathname (parser.serialise-segments #t obj) obj)))

	  (else
	   (procedure-argument-violation __who__ "invalid argument type" obj))))

  #| end of module |# )


(define (pathname obj)
  (let (((bv <bytevector-u8>) (parser.string/bytevector->pathname-bytevector __who__ obj)))
    (cond ((and ($bytevector-not-empty? bv)
		($ascii-chi-slash? (bv[0])))
	   (<absolute-unix-pathname> (bv)))
	  (else
	   (<relative-unix-pathname> (bv))))))


(define-method (pathname-absolute (o <absolute-unix-pathname>) (absolute <absolute-unix-pathname>))
  o)

(define-method (pathname-absolute (o <relative-unix-pathname>) (absolute <absolute-unix-pathname>))
  (let ((original-segments (append (absolute segments) (o segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-pathname #t original-segments)
      (<absolute-unix-pathname> (normalised-segments)))))


(define-method (pathname-string (O <relative-unix-pathname>))
  ($ascii->string (O bytevector)))

(define-method (pathname-string (O <absolute-unix-pathname>))
  ($ascii->string (O bytevector)))


(define-method (pathname-prepend (suffix <relative-unix-pathname>) (prefix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-pathname #f original-segments)
      (<relative-unix-pathname> (normalised-segments)))))

(define-method (pathname-prepend (suffix <relative-unix-pathname>) (prefix <absolute-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-pathname #t original-segments)
      (<absolute-unix-pathname> (normalised-segments)))))


(define-method (pathname-append (prefix <relative-unix-pathname>) (suffix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-pathname #f original-segments)
      (<relative-unix-pathname> (normalised-segments)))))

(define-method (pathname-append (prefix <absolute-unix-pathname>) (suffix <relative-unix-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-pathname #f original-segments)
      (<absolute-unix-pathname> ((parser.serialise-segments #t normalised-segments))))))


(define-method (pathname-tail (o <absolute-unix-pathname>))
  (<relative-unix-pathname> ((or (o last-pair) PAIR-SINGLE-DOT))))

(define-method (pathname-dirname (o <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((if (null? (o segments))
				 (o segments)
			       (%drop-last-pair (o segments))))))

(define-method (pathname-rootname (o <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((if (or (null? (o segments))
				     (not (o last-dot-index-in-last-segment)))
				 (o segments)
			       (let ((tail (car (o last-pair)))
				     (name (make-bytevector (o last-dot-index-in-last-segment))))
				 (bytevector-copy! tail 0 name 0 (o last-dot-index-in-last-segment))
				 (%replace-last-pair (o segments) (list name)))))))


(module ()

  (define (%replace-extension who (o <absolute-unix-pathname>) (extension <bytevector>))
    (let ((name (o name)))
      (if (bytevector=? name BV-SINGLE-DOT)
	  o
	(let ((segments (%replace-last-pair (o segments) (list (bytevector-append name extension)))))
	  (receive (changed? normalised-segments)
	      (parser.normalise-pathname #t segments)
	    (<absolute-unix-pathname> (normalised-segments)))))))

  (define-method (pathname-replace-extension (o <absolute-unix-pathname>) (extension <string>))
    (%replace-extension __who__ o (parser.string/bytevector->pathname-bytevector extension)))

  (define-method (pathname-replace-extension (o <absolute-unix-pathname>) (source <absolute-unix-pathname>))
    (%replace-extension __who__ o (or (source extension)
				      '#vu8())))

  (add-method pathname-replace-extension (<absolute-unix-pathname> <bytevector>) %replace-extension)

  #| end of module |# )


;;;; done

)

;;; end of file
