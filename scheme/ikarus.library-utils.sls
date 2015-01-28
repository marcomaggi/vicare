;;;
;;;Part of: Vicare Scheme
;;;Contents: library utilities
;;;Date: Thu Feb 13, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus library-utils)
  (export
    init-search-paths-and-directories

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
    conforming-library-name-and-library-reference?

    ;; search paths and special directories
    library-source-search-path
    library-binary-search-path
    compiled-libraries-store-directory
    library-extensions

    ;; library pathnames
    library-name->filename-stem
    library-reference->filename-stem
    directory+library-stem->library-binary-pathname
    directory+library-stem->library-source-pathname
    library-name->library-binary-pathname-in-store-directory
    library-reference->library-binary-pathname-in-store-directory
    library-source-pathname->library-stem-pathname
    library-source-pathname->library-binary-tail-pathname

    ;; program pathnames construction
    program-source-pathname->program-binary-pathname)
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
    (prefix (ikarus.posix) posix.)
    (vicare unsafe operations)
    (vicare language-extensions simple-match))


;;;; helpers

(define (%list-of-strings? obj)
  (and (list? obj)
       (for-all string? obj)))

(define (%get-existent-directory-real-pathname-from-env system-environment-variable)
  (let ((X (posix.getenv system-environment-variable)))
    (if (and X
	     (posix.file-string-pathname? X)
	     (posix.directory-exists?     X))
	(posix.real-pathname X)
      #f)))

(define (%get-directory-real-pathname-from-env system-environment-variable)
  (let ((X (posix.getenv system-environment-variable)))
    (if (and X (posix.file-string-pathname? X))
	(if (posix.directory-exists? X)
	    (posix.real-pathname X)
	  X)
      #f)))

;;; --------------------------------------------------------------------

(define (%string-suffix? str suffix)
  ;;Return true if the string SUFFIX is a suffix for the string STR; otherwise return
  ;;false.
  ;;
  ;;  (%string-suffix? "ciao mamma" "ciao")	=> #t
  ;;  (%string-suffix? "ciao mamma" "hello")	=> #f
  ;;
  (let ((str.len    (string-length str))
	(suffix.len (string-length suffix)))
    (and (fx< suffix.len str.len)
	 (string=? suffix (substring str (fx- str.len suffix.len) str.len)))))

(define (%string-desuffix str suffix)
  ;;Assume that the  string SUFFIX is a  suffix for the string STR;  strip the suffix
  ;;from STR and return the tail string.
  ;;
  ;;  (%string-desuffix "ciao mamma" "ciao ")	=> "mamma"
  ;;
  (substring str 0 (fx- (string-length str) (string-length suffix))))


;;;; library and program file extensions

(define-constant LIBRARY-SOURCE-EXTENSION
  ;;The file extension of source library files.
  ;;
  ".sls")

(define-constant LIBRARY-BINARY-EXTENSION
  ;;The file extension of serialised FASL files.
  ;;
  ;;NOTE  In previous  versions  there were  2  extensions: ".vicare-32bit-fasl"  for
  ;;32-bit  platforms  and  ".vicare-64bit-fasl"  for 64-bit  platforms.   But  since
  ;;version 0.4 there is a single extension.  (Marco Maggi; Thu Feb 20, 2014)
  ;;
  ".fasl")

;;; --------------------------------------------------------------------

(define-constant PROGRAM-SOURCE-EXTENSION
  ;;The file extension of source program files.
  ;;
  ".sps")

(define-constant PROGRAM-BINARY-EXTENSION
  ;;The file extension of serialised FASL programs.
  ;;
  LIBRARY-BINARY-EXTENSION)

;;; --------------------------------------------------------------------

(define library-extensions
  ;;Hold a list of strings representing file name extensions, leading dot included.
  ;;
  (make-parameter '()
    (lambda* ({obj %list-of-strings?})
      obj)))


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

(define* (library-name-identifiers=? {sexp1 library-name?} {sexp2 library-name?})
  ;;Given  two symbolic  expressions  compliant with  the definition  of
  ;;<LIBRARY NAME>  according to R6RS: return  #t if they  have the same
  ;;list of identifiers.
  ;;
  (let ((ids1 (library-name->identifiers sexp1))
	(ids2 (library-name->identifiers sexp2)))
    (and (= (length ids1)
	    (length ids2))
	 (for-all eq? ids1 ids2))))

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

  (define* (%library-name-comparison version-predicate {sexp1 library-name?} {sexp2 library-name?})
    (let-values
	(((ids1 vrs1) (library-name-decompose sexp1))
	 ((ids2 vrs2) (library-name-decompose sexp2)))
      (and (= (length ids1)
	      (length ids2))
	   (for-all eq? ids1 ids2)
	   (version-predicate vrs1 vrs2))))

  #|end of module |# )

;;; --------------------------------------------------------------------

(define* (library-version=? {vrs1 library-version-numbers?} {vrs2 library-version-numbers?})
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

(define* (library-version<? {vrs1 library-version-numbers?} {vrs2 library-version-numbers?})
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

(define* (library-version<=? {vrs1 library-version-numbers?} {vrs2 library-version-numbers?})
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

(define* (library-reference-identifiers=? {ref1 library-reference?} {ref2 library-reference?})
  (let ((ids1 (library-reference->identifiers ref1))
	(ids2 (library-reference->identifiers ref2)))
    (and (= (length ids1)
	    (length ids2))
	 (for-all eq? ids1 ids2))))

;;; --------------------------------------------------------------------

(define* (conforming-sub-version-and-sub-version-reference? {sub-version library-sub-version?}
							    {sub-version-reference library-sub-version-reference?})
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

(define* (conforming-version-and-version-reference? {version library-version-numbers?}
						    {version-reference library-version-reference?})
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

(define* (conforming-library-name-and-library-reference? {name library-name?}
							 {reference library-reference?})
  (let-values
      (((libnam.ids libnam.version)  (library-name-decompose name))
       ((libref.ids libref.version)  (library-reference-decompose reference)))
    (and (= (length libnam.ids)
	    (length libref.ids))
	 (for-all eq? libnam.ids libref.ids)
	 (conforming-version-and-version-reference? libnam.version libref.version))))


;;;; source libraries search path
;;
;;The search path in  which to look for source library files.   The parameter must be
;;set to  a, possibly empty, list  of non-empty strings representing  valid directory
;;pathnames.
;;
;;Notice that we must  not test for directories existence: a  directory may not exist
;;at the time this search path is initialised, but be created later.
;;
(define library-source-search-path
  (make-parameter '()
    (lambda* ({P posix.list-of-string-pathnames?})
      P)))


;;;; compiled libraries search path
;;
;;The search  path in  which to look  for compiled library  files (FASL  files).  The
;;parameter must be set to a,  possibly empty, list of non-empty strings representing
;;valid directory pathnames.
;;
;;Notice that we must  not test for directories existence: a  directory may not exist
;;at the time this search path is initialised, but be created later.
;;
(define library-binary-search-path
  (make-parameter '()
    (lambda* ({P posix.list-of-string-pathnames?})
      P)))


;;;; compiled libraries store directory
;;
;;The  parameter  COMPILED-LIBRARIES-STORE-DIRECTORY  contains   false  or  a  string
;;representing the absolute pathname of a directory.
;;
;;When the selected library locator is "compile-time": the store directory is used to
;;search  for  compiled  libraries.   It  is  an error  if  the  library  locator  is
;;"compile-time" and no store directory is selected.
;;
;;Under the store  directory: FASL files are stored with  pathnames composed from the
;;library stem, which is generated from the  library name.  For example, if the store
;;directory is "$(builddir)/lib" the source library:
;;
;;   $(top_srcdir)/lib/vicare/posix.sls
;;
;;is stored as:
;;
;;   $(builddir)/lib/vicare/posix.fasl
;;
;;Notice  that the  selected store  directory may  not exist.   If it  exists: it  is
;;normalised to  its real pathname, otherwise  it is left  untouched and it may  be a
;;relative pathname.
;;
(module (compiled-libraries-store-directory)

  (define-constant DEFAULT-COMPILED-LIBRARIES-STORE-DIRECTORY
    ;;Default  value  for   the  COMPILED-LIBRARIES-STORE-DIRECTORY  parameter.   The
    ;;default value is built as follows:
    ;;
    ;;1.  If  the environment variable  VICARE_STORE_DIRECTORY is set and  holding an
    ;;existent directory pathname: select its value.
    ;;
    ;;2. Otherwise no store directory is selected and the parameter is set to false.
    ;;
    (%get-directory-real-pathname-from-env "VICARE_STORE_DIRECTORY"))

  (define compiled-libraries-store-directory
    (make-parameter
	DEFAULT-COMPILED-LIBRARIES-STORE-DIRECTORY
      (lambda* ({pathname posix.file-string-pathname?})
	pathname)))

  #| end of module |# )

(define* (call-with-compiled-libraries-store-directory proc)
  (cond ((compiled-libraries-store-directory)
	 => proc)
	(else
	 (error __who__ "compiled libraries store directory is not set"))))


;;;; initialisation of search paths and library directories

(define* (init-search-paths-and-directories library-source-search-path-directory*
					    library-binary-search-path-directory*
					    store-directory
					    more-file-extensions?)
  ;;Initialise the search path for source libraries.
  ;;
  ;;LIBRARY-SOURCE-SEARCH-PATH-DIRECTORY*  must be  a  list  of strings  representing
  ;;directory pathnames gathered  from the command line.  The order  of the pathnames
  ;;must be  the same as  the one in  which the arguments  were given on  the command
  ;;line.
  ;;
  ;;If a directory does not exist: it is  just left there in the search path.  It may
  ;;not exist at process start time, but maybe it will be created later.
  ;;
  (library-source-search-path
   (append library-source-search-path-directory*
	   (cond ((posix.getenv "VICARE_SOURCE_PATH")
		  => posix.split-search-path-string)
		 (else '()))
	   (library-source-search-path)))

  ;;Initialise the search path for compiled libraries (FASL files).
  ;;
  ;;LIBRARY-BINARY-SEARCH-PATH-DIRECTORY*  must be  a  list  of strings  representing
  ;;directory pathnames gathered  from the command line.  The order  of the pathnames
  ;;must be  the same as  the one in  which the arguments  were given on  the command
  ;;line.
  ;;
  ;;If a directory does not exist: it is  just left there in the search path.  It may
  ;;not exist at process start time, but maybe it will be created later.
  ;;
  (library-binary-search-path
   (append library-binary-search-path-directory*
	   (cond ((posix.getenv "VICARE_LIBRARY_PATH")
		  => posix.split-search-path-string)
		 (else '()))
	   (let ()
	     (module (target-os-uid vicare-lib-dir)
	       (include "ikarus.config.ss"))
	     (case target-os-uid
	       ((linux bsd darwin cygwin)
		(list vicare-lib-dir))
	       (else
		(error 'library-binary-search-path
		  "internal error: invalid target OS UID"
		  target-os-uid))))
	   (library-binary-search-path)))

;;; --------------------------------------------------------------------

  (when store-directory
    ;;A  pathname was  selected from  the command  line.  STORE-DIRECTORY  must be  a
    ;;string representing the pathname of an existent directory.
    (if (posix.file-string-pathname? store-directory)
	(compiled-libraries-store-directory (if (posix.directory-exists? store-directory)
						(posix.real-pathname store-directory)
					      store-directory))
      (raise
       (condition (make-i/o-file-does-not-exist-error store-directory)
		  (make-who-condition __who__)
		  (make-message-condition "invalid compiled libraries store directory pathname")
		  (make-irritants-condition (list store-directory))))))

  ;;Initialise the list of source library file extensions.
  ;;
  (library-extensions
   (if more-file-extensions?
       (let ((%prefix (lambda (ext ls)
			(append (map (lambda (x)
				       (string-append ext x))
				  ls)
				ls))))
	 (%prefix "/main" (%prefix ".vicare" '(".sls" ".ss" ".scm"))))
     '(".vicare.sls" ".sls")))

  (void))


;;;; file name stems construction

(module (library-name->filename-stem
	 library-reference->filename-stem)

  (define* (library-reference->filename-stem {libref library-reference?})
    ;;Convert the non-empty list of identifiers  from a R6RS library reference into a
    ;;string representing the corresponding relative file pathname, without extension
    ;;but including a leading #\/ character.  Examples:
    ;;
    ;;   (library-reference->filename-stem '(alpha beta gamma ((>= 3))))
    ;;   => "/alpha/beta/gamma"
    ;;
    ;;   (library-reference->filename-stem '(alpha beta main ((>= 3))))
    ;;   => "/alpha/beta/main_"
    ;;
    ;;notice how the component "main", when  appearing last, is "quoted" by appending
    ;;an underscore.
    ;;
    ;;The returned value can be used as:
    ;;
    ;;* Source library  name, by appending an extension like  ".sls".
    ;;
    ;;* Compiled library name, by appending an extension like ".fasl".
    ;;
    (%compose-stem (library-reference->identifiers libref)))

  (define* (library-name->filename-stem {libref library-reference?})
    ;;Convert  the non-empty  list of  identifiers from  a R6RS  library name  into a
    ;;string representing the corresponding relative file pathname, without extension
    ;;but including a leading #\/ character.  Examples:
    ;;
    ;;   (library-name->filename-stem '(alpha beta gamma (1 2 3)))
    ;;   => "/alpha/beta/gamma"
    ;;
    ;;   (library-name->filename-stem '(alpha beta main (1 2 3)))
    ;;   => "/alpha/beta/main_"
    ;;
    ;;notice how the component "main", when  appearing last, is "quoted" by appending
    ;;an underscore.
    ;;
    ;;The returned value can be used as:
    ;;
    ;;* Source library  name, by appending an extension like  ".sls".
    ;;
    ;;* Compiled library name, by appending an extension like ".fasl".
    ;;
    (%compose-stem (library-name->identifiers libref)))

  (define (%compose-stem id*)
    (receive (port extract)
	(open-string-output-port)
      (let next-component ((component		(car id*))
			   (ls			(cdr id*))
			   (first-component?	#t))
	(write-char #\/ port)
	(let ((component-name (symbol->string component)))
	  (for-each (lambda (N)
		      (let ((ch (integer->char N)))
			(if (or (char<=? #\a ch #\z)
				(char<=? #\A ch #\Z)
				(char<=? #\0 ch #\9)
				(char=?  ch #\.)
				(char=?  ch #\-)
				(char=?  ch #\+)
				(char=?  ch #\_))
			    (write-char ch port)
			  (receive (D M)
			      (div-and-mod N 16)
			    (write-char #\% port)
			    (%display-hex D port)
			    (%display-hex M port)))))
	    (bytevector->u8-list (string->utf8 component-name)))
	  (if (pair? ls)
	      (next-component (car ls) (cdr ls) #f)
	    (when (and (not first-component?)
		       (%main*? component-name))
	      (write-char #\_ port)))))
      (extract)))

  (define (%display-hex N port)
    (if (fx<= 0 N 9)
	(display N port)
      (write-char (integer->char (fx+ (char->integer #\a) (fx- N 10))) port)))

  (define (%main*? component-name)
    (let ((component-name.len (string-length component-name)))
      (and (fx>= component-name.len 4)
	   (string=? (substring component-name 0 4) "main")
	   (for-all (lambda (ch)
		      (char=? ch #\_))
	     (string->list (substring component-name 4 component-name.len))))))

  #| end of module |# )


;;;; library pathnames construction

(module (directory+library-stem->library-binary-pathname
	 directory+library-stem->library-source-pathname)

  (define* (directory+library-stem->library-binary-pathname directory stem)
    (%build-pathname __who__ directory stem LIBRARY-BINARY-EXTENSION))

  (define* (directory+library-stem->library-source-pathname directory stem)
    (%build-pathname __who__ directory stem LIBRARY-SOURCE-EXTENSION))

  (define (%build-pathname who directory stem extension)
    (receive-and-return (pathname)
	(string-append directory stem extension)
      (unless (posix.file-string-pathname? pathname)
	(raise
	 (condition (make-who-condition who)
		    (make-message-condition "invalid string as pathname from given arguments")
		    (make-i/o-filename-error pathname)
		    (make-irritants-condition (list directory stem extension)))))))

  #| end of module |# )

;;; --------------------------------------------------------------------

(define* (library-name->library-binary-pathname-in-store-directory {libname library-name?})
  ;;Given an R6RS  compliant library name build and return  a string representing the
  ;;pathname of a binary library in the current store directory.
  ;;
  (call-with-compiled-libraries-store-directory
   (lambda (store-directory)
     (directory+library-stem->library-binary-pathname store-directory (library-name->filename-stem libname)))))

(define* (library-reference->library-binary-pathname-in-store-directory {libref library-reference?})
  ;;Given an R6RS compliant library reference  build and return a string representing
  ;;the pathname of a binary library in the current store directory.
  ;;
  (call-with-compiled-libraries-store-directory
   (lambda (store-directory)
     (directory+library-stem->library-binary-pathname store-directory (library-reference->filename-stem libref)))))

;;; --------------------------------------------------------------------

(define* (library-source-pathname->library-stem-pathname {source-pathname posix.file-string-pathname?})
  (cond ((%string-suffix?  source-pathname ".vicare.sls")
	 (%string-desuffix source-pathname ".vicare.sls"))
	((%string-suffix?  source-pathname ".sls")
	 (%string-desuffix source-pathname ".sls"))
	((%string-suffix?  source-pathname ".vicare.ss")
	 (%string-desuffix source-pathname ".vicare.ss"))
	((%string-suffix?  source-pathname ".ss")
	 (%string-desuffix source-pathname ".ss"))
	((%string-suffix?  source-pathname ".vicare.scm")
	 (%string-desuffix source-pathname ".vicare.scm"))
	((%string-suffix?  source-pathname ".scm")
	 (%string-desuffix source-pathname ".scm"))
	(else
	 source-pathname)))

(define (library-source-pathname->library-binary-tail-pathname source-pathname)
  (string-append (library-source-pathname->library-stem-pathname source-pathname) LIBRARY-BINARY-EXTENSION))


;;;; program pathnames construction

(define* (program-source-pathname->program-binary-pathname {source-pathname posix.file-string-pathname?})
  (define (%error ptn)
    (assertion-violation __who__
      "unable to build valid FASL file pathname from program source pathname"
      source-pathname ptn))
  (let ((ptn (cond ((%string-suffix?  source-pathname PROGRAM-SOURCE-EXTENSION)
		    (%string-desuffix source-pathname PROGRAM-SOURCE-EXTENSION))
		   (else
		    (string-append source-pathname PROGRAM-BINARY-EXTENSION)))))
    (if (posix.file-string-pathname? ptn)
	(let ((binary-pathname ptn))
	  (if (posix.file-string-pathname? binary-pathname)
	      binary-pathname
	    (%error binary-pathname)))
      (%error ptn))))


;;;; done

;; #!vicare
;; (define dummy
;;   (foreign-call "ikrt_print_emergency" #ve(ascii "library-utils")))

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
