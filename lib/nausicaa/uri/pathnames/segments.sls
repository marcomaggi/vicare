;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: pathname objects with segments as internal representation
;;;Date: Sat Nov 23, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa uri pathnames segments)
  (export
    <pathname>
    <absolute-pathname>
    <relative-pathname>
    <absolute-segments-pathname>
    <relative-segments-pathname>

    ;; multimethods
    pathname-absolute
    pathname-prepend		pathname-append
    pathname-string

    pathname-tail		pathname-dirname
    pathname-rootname		pathname-name
    pathname-extension		pathname-replace-extension)
  (import (nausicaa)
    (nausicaa uri pathname abstract))


;;; helpers

(define-constant SINGLE-DOT-BV		'#vu8(46))
(define-constant DOUBLE-DOT-BV		'#vu8(46 46))

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


(define (%bytevector-find-last-dot-index (segment <bytevector-u8>))
  ;;Given  a   bytevector,  find  the   index  of  the   rightmost  byte
  ;;representing a dot  in ASCII coding, according to  the convention of
  ;;dots separating extensions.
  ;;
  (let previous-byte ((i ($fx+ -1 (segment $length))))
    (cond (($fxzero? i)
	   #f)
	  ;; (char->integer #\.) => 46
	  (($fx= 46 (segment[i]))
	   i)
	  (else
	   (previous-byte ($fxsub1 i))))))


(define-class <segments-pathname-clauses>
  (abstract)

  (fields
   (immutable (segments		<list>))
		;The   list  of   bytevector   segments,  verified   and
		;normalised.
   (immutable (last-pair	<pair>))
		;False of the last pair in SEGMENTS.
   (mutable memoized-last-dot-index-in-last-segment)

   #| end of fields |# )

  (virtual-fields

   (immutable last-dot-index-in-last-segment
	      (lambda ((O <segments-pathname>))
		(or (O $memoized-last-dot-index-in-last-segment)
		    (and (O $last-pair)
			 (receive-and-return (dot-index)
			     (%bytevector-find-last-dot-index (O $last-pair $car))
			   (set! (O $memoized-last-dot-index-in-last-segment) dot-index))))))

   #| end of virtual-vields |# )

  #| end of class |# )


(define-class <absolute-segments-pathname>
  (nongenerative nausicaa:uri:pathnames:segments:<absolute-segments-pathname>)
  (parent <absolute-pathname>)
  (mixins (<segments-pathname-clauses>
	   (<segments-pathname>		<absolute-segments-pathname>)))
  (super-protocol
   (lambda (make-top)
     (lambda (segments)
       ((make-top) segments (last-pair segments) #f))))

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
		 (parser.normalise-segments absolute? original-segments)
	       ((make-absolute-pathname (if changed?
					    (parser.serialise-segments absolute? normalised-segments)
					  (bytevector-copy obj))
					normalised-segments))))))

	((parser.list-of-segments? obj)
	 ((make-absolute-pathname (parser.serialise-segments #t obj) obj)))

	(else
	 (procedure-argument-violation __who__ "invalid argument type" obj))))


(define-class <relative-segments-pathname>
  (nongenerative nausicaa:uri:pathnames:segments:<relative-segments-pathname>)
  (parent <relative-pathname>)
  (mixins (<segments-pathname-clauses>
	   (<segments-pathname>		<relative-segments-pathname>)))
  (super-protocol
   (lambda (make-top)
     (lambda (segments)
       ((make-top) segments (last-pair segments) #f))))

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
		 (parser.normalise-segments absolute? original-segments)
	       ((make-relative-pathname (if changed?
					    (parser.serialise-segments absolute? normalised-segments)
					  (bytevector-copy obj))
					normalised-segments))))))

	((parser.list-of-segments? obj)
	 ((make-relative-pathname (parser.serialise-segments #t obj) obj)))

	(else
	 (procedure-argument-violation __who__ "invalid argument type" obj))))


(define-method (pathname-extension (O <absolute-pathname>))
  (and (O $last-pair)
       (let (((last-segment <bytevector>) (O $last-pair $car))
	     (dot-index (O last-dot-index-in-last-segment)))
	 (and dot-index
	      (receive-and-return ((extension <bytevector>))
		  (make-bytevector ($fx- (last-segment $length) dot-index))
		(bytevector-copy! last-segment dot-index extension 0 (extension length)))))))

(define-method (pathname-name (O <absolute-pathname>))
  (if (O $last-pair)
      (let* (((last-segment <bytevector>) (O $last-pair $car))
	     (dot-index		(O last-dot-index-in-last-segment))
	     (name.len		(or dot-index (last-segment length))))
	(receive-and-return ((name <bytevector>))
	    (make-bytevector name.len)
	  (bytevector-copy! last-segment 0 name 0 name.len)))
    SINGLE-DOT-BV))


(define-method (pathname-absolute (o <absolute-segments-pathname>) (absolute <absolute-segments-pathname>))
  o)

(define-method (pathname-absolute (o <relative-segments-pathname>) (absolute <absolute-segments-pathname>))
  (let ((original-segments (append (absolute segments) (o segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-segments #t original-segments)
      (<absolute-segments-pathname> (normalised-segments)))))


(define-method (pathname-string (O <relative-segments-pathname>))
  ($ascii->string (O bytevector)))

(define-method (pathname-string (O <absolute-segments-pathname>))
  ($ascii->string (O bytevector)))


(define-method (pathname-prepend (suffix <relative-segments-pathname>) (prefix <relative-segments-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-segments #f original-segments)
      (<relative-segments-pathname> (normalised-segments)))))

(define-method (pathname-prepend (suffix <relative-segments-pathname>) (prefix <absolute-segments-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-segments #t original-segments)
      (<absolute-segments-pathname> (normalised-segments)))))


(define-method (pathname-append (prefix <relative-segments-pathname>) (suffix <relative-segments-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-segments #f original-segments)
      (<relative-segments-pathname> (normalised-segments)))))

(define-method (pathname-append (prefix <absolute-segments-pathname>) (suffix <relative-segments-pathname>))
  (let ((original-segments (append (prefix segments) (suffix segments))))
    (receive (changed? normalised-segments)
	(parser.normalise-segments #f original-segments)
      (<absolute-segments-pathname> ((parser.serialise-segments #t normalised-segments))))))


(define-method (pathname-tail (o <absolute-segments-pathname>))
  (<relative-segments-pathname> ((or (o last-pair) PAIR-SINGLE-DOT))))

(define-method (pathname-dirname (o <absolute-segments-pathname>))
  (<absolute-segments-pathname> ((if (null? (o segments))
				 (o segments)
			       (%drop-last-pair (o segments))))))

(define-method (pathname-rootname (o <absolute-segments-pathname>))
  (<absolute-segments-pathname> ((if (or (null? (o segments))
				     (not (o last-dot-index-in-last-segment)))
				 (o segments)
			       (let ((tail (car (o last-pair)))
				     (name (make-bytevector (o last-dot-index-in-last-segment))))
				 (bytevector-copy! tail 0 name 0 (o last-dot-index-in-last-segment))
				 (%replace-last-pair (o segments) (list name)))))))


(module ()

  (define (%replace-extension who (o <absolute-segments-pathname>) (extension <bytevector>))
    (let ((name (o name)))
      (if (bytevector=? name BV-SINGLE-DOT)
	  o
	(let ((segments (%replace-last-pair (o segments) (list (bytevector-append name extension)))))
	  (receive (changed? normalised-segments)
	      (parser.normalise-segments #t segments)
	    (<absolute-segments-pathname> (normalised-segments)))))))

  (define-method (pathname-replace-extension (o <absolute-segments-pathname>) (extension <string>))
    (%replace-extension __who__ o (parser.string/bytevector->pathname-bytevector extension)))

  (define-method (pathname-replace-extension (o <absolute-segments-pathname>) (source <absolute-segments-pathname>))
    (%replace-extension __who__ o (or (source extension)
				      '#vu8())))

  (add-method pathname-replace-extension (<absolute-segments-pathname> <bytevector>) %replace-extension)

  #| end of module |# )


;;;; done

)

;;; end of file
