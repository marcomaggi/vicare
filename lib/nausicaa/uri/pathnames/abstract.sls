;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: file system pathname handling
;;;Date: Fri Mar 26, 2010
;;;
;;;Abstract
;;;
;;;	List format:
;;;
;;;		(car name-as-list) -> "." or "/"
;;;		(car (reverse name-as-list)) -> file name+extension
;;;
;;;	examples:
;;;
;;;		("." "alpha" "beta" "file.ext")
;;;		("/" "alpha" "beta" "file.ext")
;;;		("/" "alpha" ".." "file.ext")
;;;		("/" "alpha" "." "file.ext")
;;;		("." "alpha" "." ".." "..")
;;;
;;;
;;;Copyright (c) 2006-2007, 2010-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa uri pathnames abstract)
  (export
    <pathname>
    <absolute-pathname>
    <relative-pathname>

    ;; multimethods
    pathname-absolute
    pathname-prepend		pathname-append
    pathname-string

    pathname-tail		pathname-dirname
    pathname-rootname		pathname-name
    pathname-extension		pathname-replace-extension

    ;; condition object types
    &pathname
    &byte			raise-byte-error
    &parser			raise-parser-error
    &normalisation		raise-normalisation-error

    ;; auxiliary syntaxes
    string:			bytevector:
    segments:)
  (import (nausicaa)
    (vicare unsafe operations))


;;; helpers

(define-constant SINGLE-DOT-BV		'#vu8(46))
(define-constant DOUBLE-DOT-BV		'#vu8(46 46))

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

(define-auxiliary-syntaxes
  string:
  bytevector:
  segments:)


;;;; multimethods

(define-generic pathname-absolute		(self absolute-pathname))
(define-generic pathname-prepend		(relative-self relative-pathname))
(define-generic pathname-append			(relative-self relative-pathname))
(define-generic pathname-string			(self))

(define-generic pathname-tail			(absolute-self))
(define-generic pathname-dirname		(absolute-self))
(define-generic pathname-rootname		(absolute-self))
(define-generic pathname-name			(absolute-self))
(define-generic pathname-extension		(absolute-self))
(define-generic pathname-replace-extension	(absolute-self source))


;;;; condition object types

(define-condition-type &pathname
    (parent &error))

(define-condition-type &byte
    (parent &pathname)
  (fields sequence offset))

(define (raise-byte-error who message sequence offset)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-byte-condition sequence offset))))

(define-condition-type &parser
    (parent &pathname)
  (fields port offset))

(define (raise-parser-error who message port offset)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-parser-condition port offset))))

(define-condition-type &normalisation
    (parent &pathname)
  (fields absolute original-segments))

(define (raise-normalisation-error who message absolute original-segments)
  (raise
   (condition (make-who-condition who)
	      (make-message-condition message)
	      (make-normalisation-condition absolute original-segments))))


(define-class <pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<pathname>)
  (abstract)

  (super-protocol
   (lambda (make-top)
     (lambda (bytevector segments)
       ((make-top) bytevector segments (last-pair segments)))))

  (fields
   (immutable (bytevector	<bytevector>))
		;Holds the pathname, verified and normalised.
   (immutable (segments		<list>))
		;The   list  of   bytevector   segments,  verified   and
		;normalised.
   (immutable (last-pair	<pair>))
		;False of the last pair in SEGMENTS.
   #| end of fields |# )

  (virtual-fields
   (immutable (string <string>) pathname.string))

  (methods (absolute	pathname-absolute)
	   (append	pathname-append))

  #| end of class |# )


(define-class <absolute-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<absolute-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda (bytevector segments)
       ((make-pathname bytevector segments) #f))))

  (fields (mutable memoized-last-dot-index-in-last-segment))

  (virtual-fields
   (immutable last-dot-index-in-last-segment
	      (lambda ((O <absolute-pathname>))
		(or (O $memoized-last-dot-index-in-last-segment)
		    (and (O $last-pair)
			 (receive-and-return (dot-index)
			     (%bytevector-find-last-dot-index (O $last-pair $car))
			   (set! (O $memoized-last-dot-index-in-last-segment) dot-index))))))
   #| end of virtual-vields |# )

  (methods (dirname		pathname-dirname)
	   (rootname		pathname-rootname)
	   (tail		pathname-tail)
	   (extension		pathname-extension)
	   (name		pathname-name)
	   (replace-extension	pathname-replace-extension))

  #| end of class |# )


(define-class <relative-pathname>
  (nongenerative nausicaa:uri:pathnames:abstract:<relative-pathname>)
  (abstract)
  (parent <pathname>)

  (super-protocol
   (lambda (make-pathname)
     (lambda (bytevector segments)
       ((make-pathname bytevector segments)))))

  (methods (prepend	pathname-prepend))

  #| end of class |# )


;;The  following  method  implementations   are  here,  implemented  for
;;abstract classes, because they do not  need to create a new <PATHNAME>
;;instance.
;;

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


;;;; done

)

;;; end of file
