;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;;; syntax definition
;;
;;A quick summary of R6RS syntax definitions along with Vicare extensions:
;;
;;  (import ?import-spec ...)
;;
;;  ?import-spec
;;     == ?import-set
;;     == (for ?import-set ?import-level)
;;
;;  ?import-set
;;     == ?library-reference
;;     == (library ?library-reference)
;;     == (only ?import-set ?identifier ...)
;;     == (except ?import-set ?identifier)
;;     == (rename ?import-set (?identifier1 ?identifier2) ...)
;;
;;  ?library-reference
;;     == (?identifier0 ?identifier ...)
;;     == (?identifier0 ?identifier ... ?version-reference)
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
;;Vicare extends ?IMPORT-SET with:
;;
;;     == (prefix ?import-set ?identifier)
;;     == (deprefix ?import-set ?identifier)
;;     == (suffix ?import-set ?identifier)
;;     == (desuffix ?import-set ?identifier)
;;
;;Example, given:
;;
;;  ((rename (only (foo)
;;                 x z)
;;           (x y))
;;   (only (bar)
;;         q))
;;
;;this function returns the names and labels:
;;
;;   #(z y q)		#(lab.z lab.x lab.q)
;;
;;Externally  visible imported  bindings are  selected by  an EXPORT-SUBST:  an alist
;;whose keys are the  external symbol names of the bindings and  whose values are the
;;associated label gensyms.
;;


(library (psyntax.import-spec-parser)
  (export parse-import-spec*)
  (import (rnrs)
    (psyntax.compat)
    (only (psyntax.lexical-environment)
	  PSYNTAX-SYNTAX-MATCH)
    (psyntax.library-collectors)
    (psyntax.library-utils)
    (prefix (psyntax.library-manager) libman.))

;; module interfaces
(import PSYNTAX-SYNTAX-MATCH)


;;;; helpers

(define-constant __module_who__ 'import)

(define (find* sym* subst import-spec-stx)
  ;;Find all the entries in SUBST having as name the symbols in SYM*; return the list
  ;;of labels  from the selected entries.   It is an error  if a name in  SYM* is not
  ;;present in the SUBST.
  ;;
  ;;IMPORT-SPEC-STX must be a syntax object  representing the import spec in which we
  ;;search for the SYM*; it is used for descriptive error reporting.
  ;;
  ;;This function is the  one that raises an error if we try  to import an unexistent
  ;;binding, as in:
  ;;
  ;;   (import (only (vicare) this-does-not-exist))
  ;;
  (map (lambda (sym)
	 (cond ((assq sym subst)
		=> cdr)
	       (else
		(%synner "cannot find identifier in export list of import spec"
			 import-spec-stx sym))))
    sym*))

(define (rem* sym* subst)
  ;;Remove  from SUBST  all the  entries having  name in  the list  of symbols  SYM*.
  ;;Return the new subst with the entries removed.   It is fine if some names in SYM*
  ;;are not present in SUBST.
  ;;
  (let recur ((subst subst))
    (cond ((null? subst)
	   '())
	  ((memq (caar subst) sym*)
	   (recur (cdr subst)))
	  (else
	   (cons (car subst) (recur (cdr subst)))))))

(define (remove-dups ls)
  ;;Recursive function.  Remove duplicate items from the list LS.  Compare items with
  ;;EQ?.
  ;;
  (cond ((null? ls)
	 '())
	((memq (car ls) (cdr ls))
	 (remove-dups (cdr ls)))
	(else
	 (cons (car ls) (remove-dups (cdr ls))))))

;;; --------------------------------------------------------------------

(define (symbol-syntax? obj)
  (symbol? (syntax->datum obj)))

(define (%error-two-import-with-different-bindings name)
  (%synner "two imports with different bindings" name))

(case-define %synner
  ((message form)
   (syntax-violation __module_who__ message form))
  ((message form subform)
   (syntax-violation __module_who__ message form subform)))


(module (parse-import-spec*)
  ;;Given   a  list   of  SYNTAX-MATCH   expression  arguments   representing  import
  ;;specifications from  a LIBRARY form,  as defined  by R6RS plus  Vicare extensions
  ;;(which can simply be the raw sexp argument to the ENVIRONMENT function):
  ;;
  ;;1. Parse and validate the import specs.
  ;;
  ;;2. For libraries not yet loaded: load  the selected library files and add them to
  ;;   the current collector function referenced by IMP-COLLECTOR.
  ;;
  ;;3. Apply to  library-exported binding names the transformations  described by the
  ;;   import spec, obtaining the external names.
  ;;
  ;;4. Check for name conflicts between imported bindings.
  ;;
  ;;Return 2 values which can be used to build a new top level RIB record:
  ;;
  ;;1. NAME-VEC, a vector of symbols  representing the external names of the imported
  ;;   bindings.  This vector has no duplicates.
  ;;
  ;;2. LABEL-VEC  is a vector  of label gensyms  uniquely associated to  the imported
  ;;   bindings.  This vector has no duplicates.
  ;;
  (define (parse-import-spec* import-spec*)
    (let loop ((import-spec*  import-spec*)
	       (export-table  (make-eq-hashtable)))
      ;;EXPORT-TABLE  has  EXPORT-SUBST names  as  keys  and EXPORT-SUBST  labels  as
      ;;values.  It is used to check for duplicate names with different labels, which
      ;;is an error.  Example:
      ;;
      ;;   (import (rename (french)
      ;;                   (salut	ciao))	;ERROR!
      ;;           (rename (british)
      ;;                   (hello	ciao)))	;ERROR!
      ;;
      (if (pair? import-spec*)
	  (begin
	    (for-each (lambda (name.label)
			(%add-subst-entry! export-table name.label))
	      (%import-spec->export-subst ($car import-spec*)))
	    (loop ($cdr import-spec*) export-table))
	(hashtable-entries export-table))))

  (define (%add-subst-entry! export-table name.label)
    ;;Add  the   given  NAME.LABEL   entry  to   EXPORT-TABLE;  return
    ;;unspecified values.  Raise a  syntax violation if NAME.LABEL has
    ;;the same name of an entry in EXPORT-TABLE, but different label.
    ;;
    (let ((name  ($car name.label))
	  (label ($cdr name.label)))
      (cond ((hashtable-ref export-table name #f)
	     => (lambda (already-existent-label)
		  (unless (eq? already-existent-label label)
		    (%error-two-import-with-different-bindings name))))
	    (else
	     (hashtable-set! export-table name label)))))

  #| end of module |# )


(module (%import-spec->export-subst)

  (define (%import-spec->export-subst import-spec)
    ;;Process the IMPORT-SPEC and return the corresponding subst.
    ;;
    ;;The IMPORT-SPEC is parsed; the specified library is loaded and interned, if not
    ;;already in the library collection; the raw subst from the library definition is
    ;;processed according to the rules in IMPORT-SPEC.
    ;;
    ;;If an error  is found, including library version non-conforming  to the library
    ;;reference, an exception is raised.
    ;;
    (syntax-match import-spec ()
      ((?for ?import-set . ?import-levels)
       ;;FIXME  Here we  should validate  ?IMPORT-LEVELS even  if it  is not  used by
       ;;Vicare.  (Marco Maggi; Tue Apr 23, 2013)
       (eq? (syntax->datum ?for) 'for)
       (%import-set->export-subst ?import-set import-spec))

      (?import-set
       (%import-set->export-subst ?import-set import-spec))))

  (define (%import-set->export-subst import-set import-spec)
    ;;Recursive  function.   Process  the  IMPORT-SET and  return  the  corresponding
    ;;EXPORT-SUBST.  IMPORT-SPEC  is the  full import  specification from  the IMPORT
    ;;clause: it is used for descriptive error reporting.
    ;;
    (define (%recurse import-set)
      (%import-set->export-subst import-set import-spec))
    (define (%local-synner message)
      (%synner message import-spec import-set))
    (syntax-match import-set ()
      ((?spec ?spec* ...)
       ;;According  to R6RS,  the  symbol LIBRARY  can  be used  to  quote a  library
       ;;reference whose first identifier is "for", "rename", etc.
       (not (memq (syntax->datum ?spec)
		  '(rename except only prefix deprefix suffix desuffix library)))
       (%import-library (cons ?spec ?spec*)))

      ((?rename ?import-set (?old-name* ?new-name*) ...)
       (and (eq? (syntax->datum ?rename) 'rename)
	    (for-all symbol-syntax? ?old-name*)
	    (for-all symbol-syntax? ?new-name*))
       (let ((subst       (%recurse ?import-set))
	     (?old-name*  (map syntax->datum ?old-name*))
	     (?new-name*  (map syntax->datum ?new-name*)))
	 ;;FIXME Rewrite this  to eliminate find* and  rem* and merge.
	 ;;(Abdulaziz Ghuloum)
	 (let ((old-label* (find* ?old-name* subst ?import-set)))
	   (let ((subst (rem* ?old-name* subst)))
	     ;;FIXME Make sure map is valid. (Abdulaziz Ghuloum)
	     (%merge-export-subst* (map cons ?new-name* old-label*) subst)))))

      ((?except ?import-set ?sym* ...)
       (and (eq? (syntax->datum ?except) 'except)
	    (for-all symbol-syntax? ?sym*))
       (let ((subst (%recurse ?import-set)))
	 (rem* (map syntax->datum ?sym*) subst)))

      ((?only ?import-set ?name* ...)
       (and (eq? (syntax->datum ?only) 'only)
	    (for-all symbol-syntax? ?name*))
       (let* ((subst  (%recurse ?import-set))
	      (name*  (map syntax->datum ?name*))
	      (name*  (remove-dups name*))
	      (lab*   (find* name* subst ?import-set)))
	 (map cons name* lab*)))

      ((?prefix ?import-set ?the-prefix)
       (and (eq? (syntax->datum ?prefix) 'prefix)
	    (symbol-syntax? ?prefix))
       (let ((subst   (%recurse ?import-set))
	     (prefix  (symbol->string (syntax->datum ?the-prefix))))
	 (map (lambda (x)
		(cons (string->symbol
		       (string-append prefix (symbol->string (car x))))
		      (cdr x)))
	   subst)))

      ((?deprefix ?import-set ?the-prefix)
       (and (eq? (syntax->datum ?deprefix) 'deprefix)
	    (symbol-syntax? ?the-prefix))
       (if (option.strict-r6rs)
	   (%local-synner "deprefix import specification forbidden in strict R6RS mode")
	 (let* ((subst       (%recurse ?import-set))
		(prefix.str  (symbol->string (syntax->datum ?the-prefix)))
		(prefix.len  (string-length prefix.str)))
	   ;;This should never happen.
	   (when (zero? prefix.len)
	     (%local-synner "null deprefix prefix"))
	   (map (lambda (subst.entry)
		  (let* ((orig.str  (symbol->string (car subst.entry)))
			 (orig.len  (string-length orig.str)))
		    (if (and (< prefix.len orig.len)
			     (string=? prefix.str (substring orig.str 0 prefix.len)))
			(cons (string->symbol (substring orig.str prefix.len orig.len))
			      (cdr subst.entry))
		      (%local-synner
		       (string-append "binding name \"" orig.str
				      "\" cannot be deprefixed of \"" prefix.str "\"")))))
	     subst))))

      ((?suffix ?import-set ?the-suffix)
       (and (eq? (syntax->datum ?suffix) 'suffix)
	    (symbol-syntax? ?suffix))
       (if (option.strict-r6rs)
	   (%local-synner "suffix import specification forbidden in strict R6RS mode")
	 (let ((subst   (%recurse ?import-set))
	       (suffix  (symbol->string (syntax->datum ?the-suffix))))
	   (map (lambda (x)
		  (cons (string->symbol
			 (string-append (symbol->string (car x)) suffix))
			(cdr x)))
	     subst))))

      ((?desuffix ?import-set ?the-suffix)
       (and (eq? (syntax->datum ?desuffix) 'desuffix)
	    (symbol-syntax? ?the-suffix))
       (if (option.strict-r6rs)
	   (%local-synner "desuffix import specification forbidden in strict R6RS mode")
	 (let* ((subst       (%recurse ?import-set))
		(suffix.str  (symbol->string (syntax->datum ?the-suffix)))
		(suffix.len  (string-length suffix.str)))
	   ;;This should never happen.
	   (when (zero? suffix.len)
	     (%local-synner "null desuffix suffix"))
	   (map (lambda (subst.entry)
		  (let* ((orig.str    (symbol->string (car subst.entry)))
			 (orig.len    (string-length orig.str))
			 (prefix.len  (fx- orig.len suffix.len)))
		    (if (and (< suffix.len orig.len)
			     (string=? suffix.str
				       (substring orig.str prefix.len orig.len)))
			(cons (string->symbol (substring orig.str 0 prefix.len))
			      (cdr subst.entry))
		      (%local-synner
		       (string-append "binding name \"" orig.str
				      "\" cannot be desuffixed of \"" suffix.str "\"")))))
	     subst))))

      ;;According  to  R6RS: the  symbol  LIBRARY  can be  used  to  quote a  library
      ;;reference whose first identifier is "for", "rename", etc.
      ((?library (?spec* ...))
       (eq? (syntax->datum ?library) 'library)
       (%import-library ?spec*))

      (_
       (%synner "invalid import set" import-spec import-set))))

  (define (%import-library libref)
    (receive (name version-conforms-to-reference?)
	(%parse-library-reference libref)
      (when (null? name)
	(%synner "empty library name" libref))
      ;;Search for the library first in the collection of already interned libraires,
      ;;then  on the  file system.   If  successful: LIB  is an  instance of  LIBRARY
      ;;struct.
      (let ((lib (libman.find-library-by-reference (syntax->datum libref))))
	(unless (version-conforms-to-reference? (library-name->version (libman.library-name lib)))
	  (%synner "library does not satisfy version specification" libref lib))
	;;Here we know that we are loading  a library for future expansion of code so
	;;let's visit it right away.
	(libman.visit-library lib)
	((imp-collector) lib)
	(libman.library-export-subst lib))))

  #| end of module: %IMPORT-SPEC->EXPORT-SUBST |# )


(module (%parse-library-reference)

  (define (%parse-library-reference libref)
    ;;Given  a  SYNTAX-MATCH  expression   argument  LIBREF  representing  a  library
    ;;reference as defined by R6RS: parse and validate it.  Return 2 values:
    ;;
    ;;1. A list of symbols representing the library spec identifiers.
    ;;
    ;;2. A predicate function to be used  to check if a library version conforms with
    ;;   the requirements of this library specification.
    ;;
    (let recur ((spec libref))
      (syntax-match spec ()

	(((?version-spec* ...))
	 (values '() (%build-version-pred ?version-spec* libref)))

	((?id . ?rest*)
	 (symbol-syntax? ?id)
	 (receive (name pred)
	     (recur ?rest*)
	   (values (cons (syntax->datum ?id) name)
		   pred)))

	(()
	 (values '() (lambda (x) #t)))

	(_
	 (%synner "invalid library specification in import set" libref spec)))))

  (define (%build-version-pred version-reference libref)
    ;;Recursive  function.  Given  a version  reference:  validate it  and build  and
    ;;return a predicate function  that can be used to verify  if library versions do
    ;;conform.
    ;;
    ;;LIBREF must  be the  enclosing library  reference, it  is used  for descriptive
    ;;error reporting.
    ;;
    (define (%recurse X)
      (%build-version-pred X libref))
    (syntax-match version-reference ()
      (()
       (lambda (x) #t))

      ((?and ?version* ...)
       (eq? (syntax->datum ?and) 'and)
       (let ((predicate* (map %recurse ?version*)))
	 (lambda (x)
	   (for-all (lambda (pred)
		      (pred x))
	     predicate*))))

      ((?or ?version* ...)
       (eq? (syntax->datum ?or) 'or)
       (let ((predicate* (map %recurse ?version*)))
	 (lambda (x)
	   (exists (lambda (pred)
		     (pred x))
	     predicate*))))

      ((?not ?version)
       (eq? (syntax->datum ?not) 'not)
       (let ((pred (%recurse ?version)))
	 (lambda (x)
	   (not (pred x)))))

      ((?subversion* ...)
       (let ((predicate* (map (lambda (subversion)
				(%build-subversion-pred subversion libref))
			   ?subversion*)))
	 (lambda (x)
	   (let loop ((predicate* predicate*)
		      (x          x))
	     (cond ((null? predicate*)
		    #t)
		   ((null? x)
		    #f)
		   (else
		    (and ((car predicate*) (car x))
			 (loop (cdr predicate*) (cdr x)))))))))

      (_
       (%synner "invalid version reference" libref version-reference))))

  (define (%build-subversion-pred subversion* libref)
    ;;Recursive function.   Given a subversion  reference: validate it and  build and
    ;;return a predicate function  that can be used to verify  if library versions do
    ;;conform.
    ;;
    ;;LIBREF must  be the  enclosing library  reference, it  is used  for descriptive
    ;;error reporting.
    ;;
    (define (%recurse X)
      (%build-subversion-pred X libref))
    (syntax-match subversion* ()
      (?subversion-number
       (%subversion? ?subversion-number)
       (let ((N (syntax->datum ?subversion-number)))
	 (lambda (x)
	   (= x N))))

      ((?and ?subversion* ...)
       (eq? (syntax->datum ?and) 'and)
       (let ((predicate* (map %recurse ?subversion*)))
	 (lambda (x)
	   (for-all (lambda (pred)
		      (pred x))
	     predicate*))))

      ((?or ?subversion* ...)
       (eq? (syntax->datum ?or) 'or)
       (let ((predicate* (map %recurse ?subversion*)))
	 (lambda (x)
	   (exists (lambda (pred)
		     (pred x))
	     predicate*))))

      ((?not ?subversion)
       (eq? (syntax->datum ?not) 'not)
       (let ((pred (%recurse ?subversion)))
	 (lambda (x)
	   (not (pred x)))))

      ((?<= ?subversion-number)
       (and (eq? (syntax->datum ?<=) '<=)
	    (%subversion? ?subversion-number))
       (let ((N (syntax->datum ?subversion-number)))
	 (lambda (x)
	   (<= x N))))

      ((?>= ?subversion-number)
       (and (eq? (syntax->datum ?>=) '>=)
	    (%subversion? ?subversion-number))
       (let ((N (syntax->datum ?subversion-number)))
	 (lambda (x)
	   (>= x N))))

      (_
       (%synner "invalid sub-version specification in library reference"
		libref subversion*))))

  (define (%subversion? stx)
    (library-version-number? (syntax->datum stx)))

  #| end of module: %PARSE-LIBRARY-REFERENCE |# )


(module (%merge-export-subst*)

  (define (%merge-export-subst* subst1 subst2)
    ;;Recursive function.  Given two substs: merge them and return the result.
    ;;
    ;;Assume that SUBST1 has unique entries in itself and SUBST2 has unique entrie in
    ;;itself.  If an entry from SUBST1 has  the name name but different label from an
    ;;entry in SUBST2: raise a syntax error.
    ;;
    (if (pair? subst1)
	(%insert-to-subst ($car subst1)
			  (%merge-export-subst* ($cdr subst1) subst2))
      subst2))

  (define (%insert-to-subst entry subst)
    ;;Given a  subst ENTRY and a  SUBST: insert the entry  in the subst if  it is not
    ;;already present and return the result; else return SUBST.
    ;;
    (let ((name  ($car entry))
	  (label ($cdr entry)))
      (cond ((assq name subst)
	     ;;An entry for NAME already exists.
	     => (lambda (x)
		  (if (eq? (cdr x) label)
		      ;;Same name and same label: OK.
		      subst
		    ;;Same name but different label: ERROR.
		    (%error-two-import-with-different-bindings name))))
	    (else
	     ;;Prepend the new entry.
	     (cons entry subst)))))

  #| end of module: %MERGE-EXPORT-SUBST* |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
