;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: definition of hard-coded built-in label types
;;;Date: Sat Oct  1, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;

#!vicare
(program (makefile.built-in-label-types)
  (import (vicare)
    (prefix (vicare expander) xp::))


;;;; syntaxes

(define-syntax (define-scheme-type input-form.stx)
  (case-define synner
    ((message)
     (syntax-violation (quote define-scheme-type) message input-form.stx #f))
    ((message subform)
     (syntax-violation (quote define-scheme-type) message input-form.stx subform)))
  (define (main stx)
    (syntax-case stx ()
      ((?kwd ?type-name ?parent-name . ?clauses)
       (let* ((type-name.str	(symbol->string (syntax->datum #'?type-name)))
	      (clause*.stx	(xp::syntax-clauses-unwrap #'?clauses synner))
	      (clause*.stx	(xp::syntax-clauses-collapse clause*.stx))
	      (parsed-specs	(%parse-clauses clause*.stx)))
	 (%validate-parent #'?parent-name)
	 (with-syntax
	     ((TYPE-DESCRIPTOR		(datum->syntax #'?kwd (string->symbol
							       (string-append type-name.str "-ctd"))))
	      (UID			(datum->syntax #'?kwd (string->symbol
							       (string-append "vicare:core-type:" type-name.str))))
	      (CONSTRUCTOR		(parsed-specs-constructor		parsed-specs))
	      (PREDICATE		(parsed-specs-predicate			parsed-specs))
	      (EQUALITY-PREDICATE	(parsed-specs-equality-predicate	parsed-specs))
	      (COMPARISON-PROCEDURE	(parsed-specs-comparison-procedure	parsed-specs))
	      (HASH-FUNCTION		(parsed-specs-hash-function		parsed-specs))
	      (METHODS			(parsed-specs-methods			parsed-specs)))
	   #'(begin
	       (pretty-print '(set-cons! VICARE-CORE-BUILT-IN-SCHEME-OBJECT-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
					 (quote (?type-name
						 ($core-type-name
						  . #(?type-name UID ?parent-name CONSTRUCTOR PREDICATE
								 EQUALITY-PREDICATE COMPARISON-PROCEDURE HASH-FUNCTION
								 TYPE-DESCRIPTOR METHODS)))))
			     (stdout))
	       (flush-output-port (stdout))))))
      ))

  (define (%validate-parent parent.stx)
    (when (let ((prnt #'?parent-name))
	    (and (identifier? prnt)
		 (or (free-identifier=? prnt #'<void>)
		     (free-identifier=? prnt #'<null>)
		     (free-identifier=? prnt #'<empty-vector>))))
      (synner "attempt to use a sealed object-type as parent of object-type specification"
	      #'?parent-name)))

  (define-constant LIST-OF-CLAUSES
    (xp::syntax-clauses-validate-specs
     (list (xp::make-syntax-clause-spec #'constructor		0 1 0 1      '() '())
	   (xp::make-syntax-clause-spec #'type-predicate		0 1 1 1      '() '())
	   (xp::make-syntax-clause-spec #'hash-function		0 1 1 1      '() '())
	   (xp::make-syntax-clause-spec #'equality-predicate	0 1 1 1      '() '())
	   (xp::make-syntax-clause-spec #'comparison-procedure	0 1 1 1      '() '())
	   (xp::make-syntax-clause-spec #'methods			0 1 1 +inf.0 '() '()))))

  (define-record-type parsed-specs
    (fields
      (mutable constructor)
		;A  boolean or  an  identifier representing  the object  constructor.
		;When #f: this object type has  no constructor.  When #t: this object
		;type has  no constructor, but  the syntax  NEW must verify  that its
		;single argument is already an instance of this type.
      (mutable predicate)
		;False or an identifier representing  the object predicate.  When #f:
		;this object type has no predicate.
      (mutable equality-predicate)
		;False or an identifier representing the equality predicate function.
		;When #f: this object type has no equality predicate.
      (mutable comparison-procedure)
		;False or an identifier  representing the comparison procedure.  When
		;#f: this object type has no comparison procedure.
      (mutable hash-function)
		;False or an identifier representing  the object hash function.  When
		;#f: this object type has no hash function.
      (mutable methods)
		;A possibly empty association vector of method specifications.
      #| end of FIELDS |# )
    (protocol
      (lambda (make-record)
	(lambda ()
	  (make-record #f #f #f #f #f '#()))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (%parse-clauses clause*.stx)
    (xp::syntax-clauses-fold-specs
     (lambda* ({parsed-specs parsed-specs?} {clause-spec xp::syntax-clause-spec?} args)
       ;;ARGS is  a vector of  vectors holding the  values from the  clauses matching
       ;;CLAUSE-SPEC.
       (assert (fx=? 1 (vector-length args)))
       (let ((arg (vector-ref args 0)))
	 (case-identifiers (xp::syntax-clause-spec-keyword clause-spec)
	   ((constructor)
	    (if (fxzero? (vector-length arg))
		(parsed-specs-constructor-set! parsed-specs #f)
	      (let ((id (vector-ref arg 0)))
		(unless (or (identifier? id) (boolean? id))
		  (synner "invalid constructor specification" id))
		(parsed-specs-constructor-set! parsed-specs id))))
	   ((type-predicate)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (boolean? id))
		(synner "invalid predicate specification" id))
	      (parsed-specs-predicate-set! parsed-specs id)))
	   ((equality-predicate)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid equality predicate specification" id))
	      (parsed-specs-equality-predicate-set! parsed-specs id)))
	   ((comparison-procedure)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid comparison procedure specification" id))
	      (parsed-specs-comparison-procedure-set! parsed-specs id)))
	   ((hash-function)
	    (let ((id (vector-ref arg 0)))
	      (unless (or (identifier? id) (not id))
		(synner "invalid hash function specification" id))
	      (parsed-specs-hash-function-set! parsed-specs id)))
	   ((methods)
	    (syntax-case arg ()
	      (#((?method-name ?method-implementation-procedure) ...)
	       (parsed-specs-methods-set! parsed-specs #'#((?method-name . ?method-implementation-procedure) ...)))
	      (_
	       (synner "invalid syntax in METHODS clause" arg))))))
       parsed-specs)
     (make-parsed-specs) LIST-OF-CLAUSES clause*.stx))

  (main input-form.stx))


;;;; definitions

;;NOTE  This actual  definitions must  stay  in a  separate file  because the  scheme
;;objects tables are  also sourced in the "ikarus.*" files,  where another definition
;;for DEFINE-SCHEME-TYPE is present.
;;
(include "scheme-object-types.scm" #t)


;;;; done

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
