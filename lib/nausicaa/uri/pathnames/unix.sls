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


#!vicare
(library (nausicaa uri pathnames unix)
  (export
    <pathname>
    <relative-pathname>
    <absolute-pathname>
    <relative-unix-pathname>
    <absolute-unix-pathname>
    pathname
    pathname=?
    &unix-pathname-parser-error
    &unix-pathname-normalisation-error)
  (import (nausicaa)
    (vicare unsafe operations)
    (nausicaa uri pathnames abstract)
    (prefix (vicare parser-tools unix-pathnames)
	    uxptn.))


;;;; labels shadowing condition objects

(define-label &unix-pathname-parser-error
  (parent &error)
  (shadows uxptn.&unix-pathname-parser-error)
  (protocol (lambda () uxptn.make-unix-pathname-parser-error))
  (predicate uxptn.unix-pathname-parser-error?))

(define-label &unix-pathname-normalisation-error
  (parent &error)
  (shadows uxptn.&unix-pathname-normalisation-error)
  (protocol (lambda () uxptn.make-unix-pathname-normalisation-error))
  (predicate uxptn.unix-pathname-normalisation-error?))


;;;; auxiliary labels

(define-label <bytevector-unix-segment>
  (nongenerative nausicaa:uri:pathnames:unix:<bytevector-unix-segment>)
  (parent <bytevector-u8>)
  (predicate (lambda (obj)
	       (uxptn.$bytevector-segment? obj))))

(define-label <string-unix-segment>
  (nongenerative nausicaa:uri:pathnames:unix:<string-unix-segment>)
  (parent <string>)
  (predicate (lambda (obj)
	       (uxptn.$string-segment? obj))))

;;; --------------------------------------------------------------------

(define-label <bytevector-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<bytevector-unix-pathname>)
  (parent <bytevector-u8>)
  (predicate (lambda (bv)
	       (uxptn.$bytevector-pathname? bv))))

(define-label <string-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<string-unix-pathname>)
  (parent <string>)
  (predicate (lambda (str)
	       (uxptn.$string-pathname? str))))

;;; --------------------------------------------------------------------

(define-label <absolute-bytevector-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<absolute-bytevector-unix-pathname>)
  (parent <bytevector-unix-pathname>)
  (predicate (lambda (bv)
	       (uxptn.$bytevector-absolute? bv))))

(define-label <absolute-string-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<absolute-string-unix-pathname>)
  (parent <string-unix-pathname>)
  (predicate (lambda (str)
	       (uxptn.$string-absolute? str))))

;;; --------------------------------------------------------------------

(define-label <relative-bytevector-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<relative-bytevector-unix-pathname>)
  (parent <bytevector-unix-pathname>)
  (predicate (lambda (bv)
	       (uxptn.$bytevector-relative? bv))))

(define-label <relative-string-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<relative-string-unix-pathname>)
  (parent <string-unix-pathname>)
  (predicate (lambda (str)
	       (uxptn.$string-relative? str))))


;;;; pathname classes

(define-class <absolute-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<absolute-unix-pathname>)
  (parent <absolute-pathname>)

  (protocol
   (lambda (make-absolute-pathname)
     (lambda (ptn)
       (define who 'make-<absolute-unix-pathname>)
       (if (and (uxptn.pathname? ptn)
		(uxptn.absolute? ptn))
	   ((make-absolute-pathname)
	    (uxptn.$bytevector-normalise (uxptn.string/bytevector->pathname-bytevector ptn who)))
	 (procedure-argument-violation who
	   "expected absolute Unix pathname as argument" ptn)))))

  (fields (immutable (pathname <bytevector>)))

  #| end of class |# )

(define-class <relative-unix-pathname>
  (nongenerative nausicaa:uri:pathnames:unix:<relative-unix-pathname>)
  (parent <relative-pathname>)

  (protocol
   (lambda (make-relative-pathname)
     (lambda (ptn)
       (define who 'make-<relative-unix-pathname>)
       (if (and (uxptn.pathname? ptn)
		(uxptn.relative? ptn))
	   ((make-relative-pathname)
	    (uxptn.$bytevector-normalise (uxptn.string/bytevector->pathname-bytevector ptn who)))
	 (procedure-argument-violation who
	   "expected absolute Unix pathname as argument" ptn)))))

  (fields (immutable (pathname <bytevector>)))

  #| end of class |# )


;;;; generic constructor

(module (pathname)

  (define-generic-definer define-generic
    (argument-type-inspector extended-tag-unique-identifier-of))

  (define (extended-tag-unique-identifier-of obj)
    (cond ((bytevector? obj)
	   (tag-case obj
	     ((<absolute-bytevector-unix-pathname>)
	      (tag-unique-identifiers <absolute-bytevector-unix-pathname>))
	     ((<relative-bytevector-unix-pathname>)
	      (tag-unique-identifiers <relative-bytevector-unix-pathname>))
	     ((<bytevector-unix-segment>)
	      (tag-unique-identifiers <bytevector-unix-segment>))
	     (else
	      (tag-unique-identifiers-of obj))))

	  ((string? obj)
	   (tag-case obj
	     ((<absolute-string-unix-pathname>)
	      (tag-unique-identifiers <absolute-string-unix-pathname>))
	     ((<relative-string-unix-pathname>)
	      (tag-unique-identifiers <relative-string-unix-pathname>))
	     ((<string-unix-segment>)
	      (tag-unique-identifiers <string-unix-segment>))
	     (else
	      (tag-unique-identifiers-of obj))))

	  (else
	   (tag-unique-identifiers-of obj))))

;;; --------------------------------------------------------------------

  (define-generic pathname (representation))

;;; bytevector argument implementations

  (define-method ((pathname <absolute-unix-pathname>) (O <absolute-bytevector-unix-pathname>))
    (<absolute-unix-pathname> (O)))

  (define-method ((pathname <relative-unix-pathname>) (O <relative-bytevector-unix-pathname>))
    (<relative-unix-pathname> (O)))

;;; bytevector argument implementations

  (define-method ((pathname <absolute-unix-pathname>) (O <absolute-string-unix-pathname>))
    (<absolute-unix-pathname> ((uxptn.string/bytevector->pathname-bytevector O))))

  (define-method ((pathname <relative-unix-pathname>) (O <relative-string-unix-pathname>))
    (<relative-unix-pathname> ((uxptn.string/bytevector->pathname-bytevector O))))

  #| end of module |# )


;;;; some multimethods implementations

(define-method (pathname=? (A <absolute-unix-pathname>) (B <absolute-unix-pathname>))
  (or (eq? A B)
      ($bytevector= (A $pathname) (B $pathname))))

(define-method (pathname=? (A <relative-unix-pathname>) (B <relative-unix-pathname>))
  (or (eq? A B)
      ($bytevector= (A $pathname) (B $pathname))))


;;;; absolute pathname multimethods

(define-method (pathname-bytevector (O <absolute-unix-pathname>))
  (O $pathname))

(define-method (pathname-string (O <absolute-unix-pathname>))
  (uxptn.pathname-bytevector->string (O $pathname)))

(define-method (pathname-uri-representation (O <absolute-unix-pathname>))
  (uxptn.$bytevector-uri-representation (O $pathname)))

;;; --------------------------------------------------------------------

(define-method (pathname-extension (O <absolute-unix-pathname>))
  (uxptn.$bytevector-extension (O $pathname)))

(define-method (pathname-dirname (O <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((uxptn.$bytevector-dirname (O $pathname)))))

(define-method (pathname-tailname (O <absolute-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-tailname (O $pathname)))))

(define-method (pathname-rootname (O <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((uxptn.$bytevector-rootname (O $pathname)))))

(define-method (pathname-split (O <absolute-unix-pathname>))
  (uxptn.$bytevector-split (O $pathname)))

;;;NOTE Remember that the class method "replace-extension" is defined as
;;;method   of  "<pathname>"   and   implemented   by  the   multimethod
;;;"pathname-replace-extension"  defined  in  the  library  of  abstract
;;;classes.    So   it  is   useles   to   define  a   new   multimethod
;;;"pathname-replace-extension"  with extended  identification function,
;;;because "<pathname>" cannot reference it.

(define-method (pathname-replace-extension (O <absolute-unix-pathname>) (E <bytevector>))
  (with-tagged-arguments-validation (__who__)
      ((<bytevector-unix-segment>	E))
    (<absolute-unix-pathname> ((uxptn.$bytevector-replace-extension (O $pathname) E)))))

(define-method (pathname-replace-extension (O <absolute-unix-pathname>) (E <string>))
  (with-tagged-arguments-validation (__who__)
      ((<string-unix-segment>	E))
    (<absolute-unix-pathname>
     ((uxptn.$bytevector-replace-extension (O $pathname)
					   (uxptn.string/bytevector->pathname-bytevector E))))))

;;; --------------------------------------------------------------------

(define-method (pathname-prefix? (O <absolute-unix-pathname>) (R <absolute-unix-pathname>))
  (uxptn.$bytevector-prefix? (O $pathname) (R bytevector)))

(define-method (pathname-prefix? (O <absolute-unix-pathname>) (R <relative-unix-pathname>))
  #f)

;;; --------------------------------------------------------------------

(define-method (pathname-suffix? (O <absolute-unix-pathname>) (R <absolute-unix-pathname>))
  (uxptn.$bytevector-suffix? (O $pathname) (R $pathname)))

(define-method (pathname-suffix? (O <absolute-unix-pathname>) (R <relative-unix-pathname>))
  (uxptn.$bytevector-suffix? (O $pathname) (R $pathname)))

;;; --------------------------------------------------------------------

(define-method (pathname-prepend (O <absolute-unix-pathname>) (R <relative-unix-pathname>))
  (<absolute-unix-pathname> ((uxptn.$bytevector-prepend (O $pathname) (R bytevector)))))

(define-method (pathname-prepend (O <absolute-unix-pathname>) (R <absolute-unix-pathname>))
  (assertion-violation __who__
    "cannot prepend absolute pathname to absolute pathname" O R))

;;; --------------------------------------------------------------------

(define-method (pathname-append (O <absolute-unix-pathname>) (R <pathname>))
  (assertion-violation __who__
    "cannot append absolute pathname" O R))


;;;; relative pathname multimethods

(define-method (pathname-bytevector (O <relative-unix-pathname>))
  (O $pathname))

(define-method (pathname-string (O <relative-unix-pathname>))
  (uxptn.pathname-bytevector->string (O $pathname)))

(define-method (pathname-uri-representation (O <relative-unix-pathname>))
  (uxptn.$bytevector-uri-representation (O $pathname)))

;;; --------------------------------------------------------------------

(define-method (pathname-extension (O <relative-unix-pathname>))
  (uxptn.$bytevector-extension (O $pathname)))

(define-method (pathname-dirname (O <relative-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-dirname (O $pathname)))))

(define-method (pathname-tailname (O <relative-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-tailname (O $pathname)))))

(define-method (pathname-rootname (O <relative-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-rootname (O $pathname)))))

(define-method (pathname-split (O <relative-unix-pathname>))
  (uxptn.$bytevector-split (O $pathname)))

;;;NOTE Remember that the class method "replace-extension" is defined as
;;;method   of  "<pathname>"   and   implemented   by  the   multimethod
;;;"pathname-replace-extension"  defined  in  the  library  of  abstract
;;;classes.    So   it  is   useles   to   define  a   new   multimethod
;;;"pathname-replace-extension"  with extended  identification function,
;;;because "<pathname>" cannot reference it.

(define-method (pathname-replace-extension (O <relative-unix-pathname>) (E <bytevector>))
  (with-tagged-arguments-validation (__who__)
      ((<bytevector-unix-segment>	E))
    (<relative-unix-pathname> ((uxptn.$bytevector-replace-extension (O $pathname) E)))))

(define-method (pathname-replace-extension (O <relative-unix-pathname>) (E <string>))
  (with-tagged-arguments-validation (__who__)
      ((<string-unix-segment>	E))
    (<relative-unix-pathname>
     ((uxptn.$bytevector-replace-extension (O $pathname)
					   (uxptn.string/bytevector->pathname-bytevector E))))))

;;; --------------------------------------------------------------------

(define-method (pathname-prefix? (O <relative-unix-pathname>) (R <absolute-unix-pathname>))
  #f)

(define-method (pathname-prefix? (O <relative-unix-pathname>) (R <relative-unix-pathname>))
  (uxptn.$bytevector-suffix? (O $pathname) (R bytevector)))

;;; --------------------------------------------------------------------

(define-method (pathname-suffix? (O <relative-unix-pathname>) (R <relative-unix-pathname>))
  (uxptn.$bytevector-suffix? (O $pathname) (R $pathname)))

(define-method (pathname-suffix? (O <relative-unix-pathname>) (R <absolute-unix-pathname>))
  (uxptn.$bytevector-suffix? (O $pathname) (R $pathname)))

;;; --------------------------------------------------------------------

(define-method (pathname-prepend (O <relative-unix-pathname>) (R <relative-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-prepend (O $pathname) (R bytevector)))))

(define-method (pathname-prepend (O <relative-unix-pathname>) (R <absolute-unix-pathname>))
  (assertion-violation __who__
    "cannot prepend relative pathname to absolute pathname" O R))

;;; --------------------------------------------------------------------

(define-method (pathname-append (O <relative-unix-pathname>) (R <relative-unix-pathname>))
  (<relative-unix-pathname> ((uxptn.$bytevector-append (O $pathname) (R bytevector)))))

(define-method (pathname-append (O <relative-unix-pathname>) (R <absolute-unix-pathname>))
  (<absolute-unix-pathname> ((uxptn.$bytevector-append (O $pathname) (R bytevector)))))


;;;; done

)

;;; end of file
