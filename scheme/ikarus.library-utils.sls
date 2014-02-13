;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: library utilities
;;;Date: Thu Feb 13, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ikarus library-utils)
  (export
    ;; library names and version numbers
    library-name?
    library-version-numbers?		library-version-number?
    library-name-decompose
    library-name->identifiers		library-name->version
    library-name-identifiers=?		library-name=?
    library-name<?			library-name<=?
    library-version=?
    library-version<?			library-version<=?

    ;; library references and conformity
    library-reference?			library-version-reference?
    library-sub-version-reference?	library-sub-version?
    library-reference-decompose
    library-reference->identifiers
    library-reference->version-reference
    library-reference-identifiers=?
    conforming-sub-version-and-sub-version-reference?
    conforming-version-and-version-reference?
    conforming-library-name-and-library-reference?)
  (import (except (vicare)
		  ;; library names and version numbers
		  library-name?
		  library-version-numbers?		library-version-number?
		  library-name-decompose
		  library-name->identifiers		library-name->version
		  library-name-identifiers=?		library-name=?
		  library-name<?			library-name<=?
		  library-version=?
		  library-version<?			library-version<=?

		  ;; library references and conformity
		  library-reference?			library-version-reference?
		  library-sub-version-reference?	library-sub-version?
		  library-reference-decompose
		  library-reference->identifiers
		  library-reference->version-reference
		  library-reference-identifiers=?
		  conforming-sub-version-and-sub-version-reference?
		  conforming-version-and-version-reference?
		  conforming-library-name-and-library-reference?)
    (vicare unsafe operations)
    (vicare language-extensions simple-match))


;;;; R6RS library name and version utilities

(define (library-version-numbers? obj)
  ;;Return #t if  OBJ is a list of library  version numbers according to
  ;;R6RS, this includes OBJ being null.
  ;;
  ;;NOTE According to R6RS: OBJ should  be an exact integer, which means
  ;;a finxum or bignum for Vicare.   We accept only fixnums because they
  ;;are faster  to handle and "big  enough".  (Marco Maggi; Tue  Apr 23,
  ;;2013)
  ;;
  (or (null? obj)
      (and (list? obj)
	   (for-all library-version-number? obj))))

(define (library-version-number? obj)
  ;;Return #t if OBJ is a version number according to R6RS.
  ;;
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

(define (library-name? sexp)
  ;;Return  #t if  SEXP is  a  symbolic expressions  compliant with  the
  ;;definition of <LIBRARY NAME> according to R6RS.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    (if identifiers #t #f)))

;;; --------------------------------------------------------------------

(define (library-name-decompose obj)
  ;;Scan OBJ  validating it as  a <LIBRARY  NAME> as specified  by R6RS.
  ;;Return   two  values:   the   list  of   identifiers,  the   version
  ;;specification.  The  version can  be null.   If OBJ  is not  a valid
  ;;<LIBRARY NAME>:  return #f  and #f.  The  returned values  may share
  ;;structure with OBJ.
  ;;
  (if (or (null? obj) (not (list? obj)))
      (values #f #f)
    (let next-identifier ((next (car obj))
			  (tail (cdr obj))
			  (ids  '()))
      (cond ((symbol? next) ;identifier
	     (if (null? tail)
		 ;;There is  no version number, so we  return OBJ itself
		 ;;as list of identifiers.
		 (values obj '())
	       (next-identifier (car tail) (cdr tail) (cons next ids))))
	    ((and (list? next) (null? tail)) ;version spec
	     (if (library-version-numbers? next)
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

(define (library-name->identifiers sexp)
  ;;Given  a  symbolic  expressions  compliant with  the  definition  of
  ;;<LIBRARY NAME>  according to R6RS:  return the list  of identifiers.
  ;;If SEXP is not compliant return #f.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    identifiers))

(define (library-name->version sexp)
  ;;Given  a  symbolic  expressions  compliant with  the  definition  of
  ;;<LIBRARY  NAME>  according  to  R6RS:  return the  list  of  version
  ;;numbers.  If SEXP is not compliant return #f.
  ;;
  (receive (identifiers version)
      (library-name-decompose sexp)
    version))

;;; --------------------------------------------------------------------

(define* (library-name-identifiers=? (sexp1 library-name?) (sexp2 library-name?))
  ;;Given  two symbolic  expressions  compliant with  the definition  of
  ;;<LIBRARY NAME>  according to R6RS: return  #t if they  have the same
  ;;list of identifiers.
  ;;
  (for-all eq?
	   (library-name->identifiers sexp1)
	   (library-name->identifiers sexp2)))

(module (library-name=?
	 library-name<?
	 library-name<=?)

  (define (library-name=? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of identifiers and the same version numbers.
    ;;
    (%library-name-comparison library-version=? sexp1 sexp2))

  (define (library-name<? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of  identifiers and  the version  of SEXP1  is less  than the
    ;;version of SEXP2.
    ;;
    (%library-name-comparison library-version<? sexp1 sexp2))

  (define (library-name<=? sexp1 sexp2)
    ;;Given two  symbolic expressions  compliant with the  definition of
    ;;<LIBRARY NAME> according to R6RS: return  #t if they have the same
    ;;list of identifiers and the version of SEXP1 is less than or equal
    ;;to the version of SEXP2.
    ;;
    (%library-name-comparison library-version<=? sexp1 sexp2))

  (define* (%library-name-comparison version-predicate (sexp1 library-name?) (sexp2 library-name?))
    (let-values
	(((ids1 vrs1) (library-name-decompose sexp1))
	 ((ids2 vrs2) (library-name-decompose sexp2)))
      (and (= (length ids1)
	      (length ids2))
	   (for-all eq? ids1 ids2)
	   (version-predicate vrs1 vrs2))))

  #|end of module |# )

;;; --------------------------------------------------------------------

(define* (library-version=? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS: return  #t if they  have the same
  ;;numbers.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) == (1 2 3)
  ;;	(1 2 3) != (1 2 3 4)
  ;;	(1 2 3) == (1 2 3 0 0 0)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (or (null? vrs2)
	       (for-all (lambda (fx)
			  ($fxzero? fx))
		 vrs2)))
	  ((null? vrs2)
	   (for-all (lambda (fx)
		      ($fxzero? fx))
	     vrs1)) ;it cannot be (null? vrs1) here
	  (else
	   (and ($fx= ($car vrs1) ($car vrs2))
		(loop ($cdr vrs1) ($cdr vrs2)))))))

(define* (library-version<? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS:  return #t if the  version number
  ;;represented by VRS1  is less than the version  number represented by
  ;;VRS2.
  ;;
  ;;Comparison  of digits  stops at  the first  digit for  which <  or >
  ;;return true.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) <  (4 2 3)
  ;;	(1 2 3) <  (1 4 3)
  ;;	(1 2 3) <  (1 2 4)
  ;;	(1 2 3) <  (1 2 3 4)
  ;;	(1 2 3) !< (1 2 3 0 0 0)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (cond ((null? vrs2)		#f)
		 ((find (lambda (fx)
			  ($fxpositive? fx))
		    vrs2)		#t)
		 (else			#f)))
	  ((null? vrs2)
	   #f)
	  (($fx< ($car vrs1) ($car vrs2))
	   #t)
	  (($fx> ($car vrs1) ($car vrs2))
	   #f)
	  (else ;;(= (car vrs1) (car vrs2))
	   (loop ($cdr vrs1) ($cdr vrs2))))))

(define* (library-version<=? (vrs1 library-version-numbers?) (vrs2 library-version-numbers?))
  ;;Given two lists of version  numbers compliant with the definition of
  ;;<LIBRARY NAME>  according to R6RS:  return #t if the  version number
  ;;represented  by VRS1 is  less than  or equal  to the  version number
  ;;represented by VRS2.
  ;;
  ;;Comparison of digits  stops at the first digit  for which <= returns
  ;;false.
  ;;
  ;;If one of the lists is longer  and the elements up to the end of the
  ;;shortest are equal: the lists are "equal" if the tail of the longest
  ;;is made of zeros.
  ;;
  ;;Examples:
  ;;
  ;;	(1 2 3) <= (1 2 3)
  ;;	(1 2 3) <= (4 2 3)
  ;;	(1 2 3) <= (1 4 3)
  ;;	(1 2 3) <= (1 2 4)
  ;;	(1 2 3) <= (1 2 3 4)
  ;;	(1 2 3 0) <= (1 2 3)
  ;;
  (let loop ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   #t)
	  ((null? vrs2)
	   (for-all (lambda (fx)
		      ($fxzero? fx))
	     vrs1))
	  (else
	   (and ($fx<= ($car vrs1) ($car vrs2))
		(loop  ($cdr vrs1) ($cdr vrs2)))))))


;;;; R6RS library references and conformity

(define-syntax %normalise-to-boolean
  (syntax-rules ()
    ((_ ?expr)
     (if ?expr #t #f))))

;;; --------------------------------------------------------------------
;;; predicates

(define (library-reference? sexp)
  ;;Return true  if SEXP is  a valid  library reference as  specified by
  ;;R6RS.
  ;;
  (receive (identifiers version)
      (library-reference-decompose sexp)
    (%normalise-to-boolean identifiers)))

(define (library-version-reference? obj)
  ;;Return true if OBJ is a valid library version reference as specified
  ;;by R6RS.
  ;;
  (match obj
    ;;We decide to  accept empty AND clauses, which  will always match a
    ;;version specification.
    (('and (let ?version-reference ...))
     (for-all library-version-reference? ?version-reference))

    ;;We decide  to accept  empty OR clauses,  which will never  match a
    ;;version specification.
    (('or  (let ?version-reference ...))
     (for-all library-version-reference? ?version-reference))

    (('not (let ?version-reference))
     (library-version-reference? ?version-reference))

    ;;Notice that null is a valid version reference as specified by R6RS
    ;;(see  the table  at  the  end of  the  documentation node  "scheme
    ;;library form version" in Nausicaa).  Null always matches.
    (((let ?sub-version-reference ...))
     (for-all library-sub-version-reference? ?sub-version-reference))))

(define (library-sub-version-reference? sub-version)
  ;;Return  true if  OBJ is  a  valid library  sub-version reference  as
  ;;specified by R6RS.
  ;;
  (match sub-version
    (('and (let ?sub-version ...))
     (for-all library-sub-version-reference? ?sub-version))
    (('or  (let ?sub-version ...))
     (for-all library-sub-version-reference? ?sub-version))
    (('not (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    (('<= (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    (('>= (let ?sub-version))
     (library-sub-version-reference? ?sub-version))
    ((apply library-sub-version?)
     #t)
    (_ #f)))

(define (library-sub-version? obj)
  ;;Return #t if OBJ is a sub-version number.
  ;;
  ;;NOTE According to R6RS: OBJ should be an exact non-negative integer,
  ;;which means a  non-negative finxum or bignum for  Vicare.  We accept
  ;;only fixnums  because they  are faster to  handle and  "big enough".
  ;;(Marco Maggi; Tue Apr 23, 2013)
  ;;
  (and (fixnum? obj)
       ($fxnonnegative? obj)))

;;; --------------------------------------------------------------------
;;; decomposition

(define (library-reference-decompose obj)
  ;;Scan  OBJ validating  it as  a <library  reference> as  specified by
  ;;R6RS.   Return two  values:  the list  of  identifiers, the  version
  ;;reference.  The version can be null.  If OBJ is not a valid <library
  ;;reference>: return #f and #f.
  ;;
  (if (or (null? obj)
	  (not (list? obj)))
      (values #f #f)
    (let next-identifier ((next ($car obj))
			  (rest ($cdr obj))
			  (ids  '()))
      (cond ((symbol? next) ;identifier
	     (if (null? rest)
		 ;;No  version   reference,  so  OBJ  is   the  list  of
		 ;;identifiers.
		 (values obj '()) ; == (values (reverse (cons next ids)) '())
	       (next-identifier ($car rest) ($cdr rest) (cons next ids))))
	    ((and (list? next) (null? rest)) ;version spec
	     (if (library-version-reference? next)
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

;;; --------------------------------------------------------------------

(define (library-reference->identifiers sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    identifiers))

(define (library-reference->version-reference sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    version))

;;; --------------------------------------------------------------------

(define* (library-reference-identifiers=? (ref1 library-reference?) (ref2 library-reference?))
  (let ((ids1 (library-reference->identifiers ref1))
	(ids2 (library-reference->identifiers ref2)))
    (and (= (length ids1)
	    (length ids2))
	 (for-all eq? ids1 ids2))))

;;; --------------------------------------------------------------------

(define* (conforming-sub-version-and-sub-version-reference? (sub-version library-sub-version?)
							    (sub-version-reference library-sub-version-reference?))
  ;;SUB-VERSION must  be a fixnum  representing a single  version number
  ;;from a library name, as defined by R6RS.
  ;;
  ;;SUB-VERSION-REFERENCE  must be  a  single  sub-version reference  as
  ;;specified by R6RS:
  ;;
  ;;   ?sub-version-reference
  ;;     == ?sub-version ...
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  (define (%recurse sub-ver-ref)
    (conforming-sub-version-and-sub-version-reference? sub-version sub-ver-ref))
  (match sub-version-reference
    ((apply library-sub-version?)
     ($fx= sub-version sub-version-reference))

    (('>= (let ?sub-version-ref))
     ($fx>= sub-version ?sub-version-ref))

    (('<= (let ?sub-version-ref))
     ($fx<= sub-version ?sub-version-ref))

    (('and)
     #t)

    (('and (let ?sub-version-ref ...))
     (%normalise-to-boolean
      (for-all %recurse ?sub-version-ref)))

    (('or)
     #f)

    (('or (let ?sub-version-reference ...))
     (%normalise-to-boolean
      (find %recurse ?sub-version-reference)))

    (('not (let ?sub-version-ref))
     (not (%recurse ?sub-version-ref)))

    (else
     (assertion-violation __who__
       "invalid library sub-version reference" sub-version-reference))))

;;; --------------------------------------------------------------------

(define* (conforming-version-and-version-reference? (version library-version-numbers?)
						    (version-reference library-version-reference?))
  ;;VERSION must be a list of version numbers as specified by R6RS.
  ;;
  ;;VERSION-REFERENCE must be a version reference as specified by R6RS:
  ;;
  ;;  ?version-reference
  ;;     == (?sub-version-reference ...)
  ;;     == (and ?version-reference ...)
  ;;     == (or  ?version-reference ...)
  ;;     == (not ?version-reference)
  ;;
  ;;  ?sub-version-reference
  ;;     == ?sub-version
  ;;     == (>=  ?sub-version)
  ;;     == (<=  ?sub-version)
  ;;     == (and ?sub-version-reference ...)
  ;;     == (or  ?sub-version-reference ...)
  ;;     == (not ?sub-version-reference)
  ;;
  ;;  ?sub-version
  ;;     == #<non-negative fixnum>
  ;;
  (match version-reference
    (()
     #t)

    (('and (let ?version-reference ...))
     (%normalise-to-boolean
      (for-all (lambda (reference)
		 (conforming-version-and-version-reference? version reference))
	?version-reference)))

    (('or (let ?version-reference ...))
     (%normalise-to-boolean
      (find (lambda (reference)
	      (conforming-version-and-version-reference? version reference))
	?version-reference)))

    (('not (let ?version-reference))
     (not (conforming-version-and-version-reference? version ?version-reference)))

    (_
     (let next-sub-version ((version		version)
			    (version-reference	version-reference))
       (cond ((null? version-reference)
	      ;;According  to R6RS:  if  the  version reference  is
	      ;;shorter than the version, it is a match.
	      #t)
	     ((null? version)
	      (null? version-reference))
	     ((conforming-sub-version-and-sub-version-reference?
	       ($car version) ($car version-reference))
	      (next-sub-version ($cdr version) ($cdr version-reference)))
	     (else
	      #f))))))

;;; --------------------------------------------------------------------

(define* (conforming-library-name-and-library-reference? (name library-name?)
							 (reference library-reference?))
  (let-values
      (((libnam.ids libnam.version)  (library-name-decompose name))
       ((libref.ids libref.version)  (library-reference-decompose reference)))
    (and (for-all eq? libnam.ids libref.ids)
	 (conforming-version-and-version-reference? libnam.version libref.version))))


;;;; done

)

;;; end of file
;; Local Variables:
;; End:
