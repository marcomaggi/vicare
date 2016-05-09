;;; -*- coding: utf-8-unix -*-
;;;
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


(library (psyntax.export-spec-parser)
  (export parse-export-spec*)
  (import (rnrs)
    (psyntax.compat)
    (prefix (only (psyntax.config)
		  strict-r6rs-enabled?)
	    options::)
    (only (psyntax.lexical-environment)
	  ~bound-identifier=?
	  valid-bound-ids?
	  PSYNTAX-SYNTAX-MATCH))

(import PSYNTAX-SYNTAX-MATCH)
(include "psyntax.helpers.scm" #t)


(define* (parse-export-spec* export-spec*)
  ;;Given  a  list of  SYNTAX-MATCH  expression  arguments representing  the  exports
  ;;specification from a LIBRARY form, return 2 values:
  ;;
  ;;1. A list of symbols representing the external names of the exported bindings.
  ;;
  ;;2.  A  list of  identifiers  (syntax  objects  holding  a symbol  as  expression)
  ;;   representing the internal names of the exported bindings.
  ;;
  ;;This function checks that none of  the identifiers is ~BOUND-IDENTIFIER=?  to another: the
  ;;library does not  export the same external *name* twice.   It is instead possible
  ;;to export  the same identifier  multiple times if  we give it  different external
  ;;names.
  ;;
  ;;According to R6RS, an export specification has the following syntax:
  ;;
  ;;   (export ?export-spec ...)
  ;;
  ;;   ?export-spec
  ;;     == ?identifier
  ;;     == (rename (?internal-identifier ?external-identifier) ...)
  ;;
  ;;Vicare adds the following:
  ;;
  ;;     == (prefix   (?internal-identifier ...) the-prefix)
  ;;     == (deprefix (?internal-identifier ...) the-prefix)
  ;;     == (suffix   (?internal-identifier ...) the-suffix)
  ;;     == (desuffix (?internal-identifier ...) the-suffix)
  ;;
  (define-synner %synner (quote parse-export-spec*) export-spec*)
  (let loop ((export-spec*          export-spec*)
	     (internal-identifier*  '())
	     (external-identifier*  '()))
    (if (null? export-spec*)
	(if (valid-bound-ids? external-identifier*)
	    (values (map syntax->datum external-identifier*)
		    internal-identifier*)
	  (%synner "invalid exports" (%find-dups external-identifier*)))
      (syntax-match (car export-spec*) ()
	(?identifier
	 (identifier? ?identifier)
	 (loop (cdr export-spec*)
	       (cons ?identifier internal-identifier*)
	       (cons ?identifier external-identifier*)))

	((?rename (?internal* ?external*) ...)
	 (and (eq? (syntax->datum ?rename) 'rename)
	      (for-all identifier? ?internal*)
	      (for-all identifier? ?external*))
	 (loop (cdr export-spec*)
	       (append ?internal* internal-identifier*)
	       (append ?external* external-identifier*)))

	((?prefix (?internal* ...) ?the-prefix)
	 (and (eq? (syntax->datum ?prefix) 'prefix)
	      (for-all identifier? ?internal*)
	      (identifier? ?the-prefix))
	 (if (options::strict-r6rs-enabled?)
	     (%synner "prefix export specification forbidden in strict R6RS mode")
	   (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		  (external*  (map (lambda (id)
				     (datum->syntax
				      id (string->symbol
					  (string-append
					   prefix.str
					   (symbol->string (syntax->datum id))))))
				?internal*)))
	     (loop (cdr export-spec*)
		   (append ?internal* internal-identifier*)
		   (append  external* external-identifier*)))))

	((?deprefix (?internal* ...) ?the-prefix)
	 (and (eq? (syntax->datum ?deprefix) 'deprefix)
	      (for-all identifier? ?internal*)
	      (identifier? ?the-prefix))
	 (if (options::strict-r6rs-enabled?)
	     (%synner "deprefix export specification forbidden in strict R6RS mode")
	   (let* ((prefix.str (symbol->string (syntax->datum ?the-prefix)))
		  (prefix.len (string-length prefix.str))
		  (external*  (map (lambda (id)
				     (let* ((id.str  (symbol->string (syntax->datum id)))
					    (id.len  (string-length id.str)))
				       (if (and (< prefix.len id.len)
						(string=? prefix.str
							  (substring id.str 0 prefix.len)))
					   (datum->syntax
					    id (string->symbol
						(substring id.str prefix.len id.len)))
					 (%synner
					  (string-append "binding name \"" id.str
							 "\" cannot be deprefixed of \""
							 prefix.str "\"")))))
				?internal*)))
	     (loop (cdr export-spec*)
		   (append ?internal* internal-identifier*)
		   (append  external* external-identifier*)))))

	((?suffix (?internal* ...) ?the-suffix)
	 (and (eq? (syntax->datum ?suffix) 'suffix)
	      (for-all identifier? ?internal*)
	      (identifier? ?the-suffix))
	 (if (options::strict-r6rs-enabled?)
	     (%synner "suffix export specification forbidden in strict R6RS mode")
	   (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		  (external*  (map (lambda (id)
				     (datum->syntax
				      id (string->symbol
					  (string-append
					   (symbol->string (syntax->datum id))
					   suffix.str))))
				?internal*)))
	     (loop (cdr export-spec*)
		   (append ?internal* internal-identifier*)
		   (append  external* external-identifier*)))))

	((?desuffix (?internal* ...) ?the-suffix)
	 (and (eq? (syntax->datum ?desuffix) 'desuffix)
	      (for-all identifier? ?internal*)
	      (identifier? ?the-suffix))
	 (if (options::strict-r6rs-enabled?)
	     (%synner "desuffix export specification forbidden in strict R6RS mode")
	   (let* ((suffix.str (symbol->string (syntax->datum ?the-suffix)))
		  (suffix.len (string-length suffix.str))
		  (external*  (map (lambda (id)
				     (define id.str
				       (symbol->string (syntax->datum id)))
				     (define id.len
				       (string-length id.str))
				     (define prefix.len
				       (fx- id.len suffix.len))
				     (if (and (< suffix.len id.len)
					      (string=? suffix.str
							(substring id.str prefix.len id.len)))
					 (datum->syntax
					  id (string->symbol
					      (substring id.str 0 prefix.len)))
				       (%synner
					(string-append "binding name \"" id.str
						       "\" cannot be desuffixed of \""
						       suffix.str "\""))))
				?internal*)))
	     (loop (cdr export-spec*)
		   (append ?internal* internal-identifier*)
		   (append  external* external-identifier*)))))

	(_
	 (%synner "invalid export specification" (car export-spec*)))))))


(module (%find-dups)

  (define (%find-dups ls)
    (let loop ((ls    ls)
	       (dups  '()))
      (cond ((null? ls)
	     dups)
	    ((%find-bound=? (car ls) (cdr ls) (cdr ls))
	     => (lambda (x)
		  (loop (cdr ls)
			(cons (list (car ls) x)
			      dups))))
	    (else
	     (loop (cdr ls) dups)))))

  (define (%find-bound=? x lhs* rhs*)
    (cond ((null? lhs*)
	   #f)
	  ((~bound-identifier=? x (car lhs*))
	   (car rhs*))
	  (else
	   (%find-bound=? x (cdr lhs*) (cdr rhs*)))))

  #| end of module: %FIND-DUPS |# )


;;;; done

#| end of library |# )

;;; end of file
