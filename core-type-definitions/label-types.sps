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

(define-syntax (define-built-in-label-type input-form.stx)
  (define (main stx)
    (syntax-case stx ()
      ((?kwd ?type-name . ?clauses)
       (let* ((type-name.str	(symbol->string (syntax->datum #'?type-name)))
	      (clause*.stx	(xp::syntax-clauses-unwrap #'?clauses synner))
	      (clause*.stx	(xp::syntax-clauses-collapse clause*.stx))
	      (parsed-specs	(%parse-clauses clause*.stx)))
	 (with-syntax
	     ((UID			(xp::identifier-append #'?kwd "vicare:core-type:" type-name.str))
	      (PARENT			(parsed-specs-parent			parsed-specs))
	      (CONSTRUCTOR		(parsed-specs-constructor		parsed-specs))
	      (DESTRUCTOR		(parsed-specs-destructor		parsed-specs))
	      (TYPE-PREDICATE		(parsed-specs-type-predicate		parsed-specs))
	      (EQUALITY-PREDICATE	(parsed-specs-equality-predicate	parsed-specs))
	      (COMPARISON-PROCEDURE	(parsed-specs-comparison-procedure	parsed-specs))
	      (HASH-FUNCTION		(parsed-specs-hash-function		parsed-specs))
	      (METHODS			(parsed-specs-methods			parsed-specs)))
	   #'(begin
	       (pretty-print '(set-cons! VICARE-CORE-BUILT-IN-LABEL-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
					 (quote (?type-name
						 ($core-label-type-name
						  . #(?type-name UID PARENT CONSTRUCTOR DESTRUCTOR TYPE-PREDICATE
								 EQUALITY-PREDICATE COMPARISON-PROCEDURE HASH-FUNCTION
								 METHODS)))))
			     (stdout))
	       (flush-output-port (stdout))))))
      (_
       (synner "invalid syntax use"))))

;;; --------------------------------------------------------------------

  (define-constant LIST-OF-CLAUSES
    (xp::syntax-clauses-validate-specs
     (list
      ;; NAME MIN-OCCUR MAX-OCCUR MIN-ARGS MAX-ARGS MUTUALLY-INCLUSIVE MUTUALLY-EXCLUSIVE
      (xp::make-syntax-clause-spec #'parent			1 1 1 1      '() '())
      (xp::make-syntax-clause-spec #'constructor			0 1 0 1      '() '())
      (xp::make-syntax-clause-spec #'destructor			0 1 0 1      '() '())
      (xp::make-syntax-clause-spec #'type-predicate		0 1 1 1      '() '())
      (xp::make-syntax-clause-spec #'equality-predicate		0 1 1 1      '() '())
      (xp::make-syntax-clause-spec #'comparison-procedure		0 1 1 1      '() '())
      (xp::make-syntax-clause-spec #'hash-function		0 1 1 1      '() '())
      (xp::make-syntax-clause-spec #'methods			0 1 1 +inf.0 '() '()))))

  (define-record-type parsed-specs
    (fields
      (mutable parent)
		;A syntax object representing a type annotation.  It is the parent of
		;the label-type.
      (mutable constructor)
		;A  boolean or  an  identifier representing  the object  constructor.
		;When  #f:  this  object-type  has no  constructor.   When  #t:  this
		;object-type has no constructor, but  the syntax NEW must verify that
		;its single argument is already an instance of this type.
      (mutable destructor)
		;False or an identifier representing the object destructor.  When #f:
		;this object-type has no destructor.
      (mutable type-predicate)
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
	  (make-record #f	;parent
		       #f	;constructor
		       #f	;destructor
		       #f	;type-predicate
		       #f	;equality-predicate
		       #f	;comparison-procedure
		       #f	;hash-function
		       '#()	;methods
		       ))))
    #| end of DEFINE-RECORD-TYPE |# )

  (define (%parse-clauses clause*.stx)
    (xp::syntax-clauses-fold-specs combine (make-parsed-specs) LIST-OF-CLAUSES clause*.stx))

  (define* (combine {parsed-specs parsed-specs?} {clause-spec xp::syntax-clause-spec?} args)
    ;;ARGS  is a  vector of  vectors  holding the  values from  the clauses  matching
    ;;CLAUSE-SPEC.
    ;;
    (define arg (vector-ref args 0))
    (case-identifiers (xp::syntax-clause-spec-keyword clause-spec)
      ((parent)
       (parsed-specs-parent-set! parsed-specs (vector-ref arg 0)))

      ((constructor)
       (if (fxzero? (vector-length arg))
	   (parsed-specs-constructor-set! parsed-specs #f)
	 (let ((id (vector-ref arg 0)))
	   (unless (%boolean-or-id? id)
	     (synner "invalid constructor specification" id))
	   (parsed-specs-constructor-set! parsed-specs id))))

      ((destructor)
       (if (fxzero? (vector-length arg))
	   (parsed-specs-destructor-set! parsed-specs #f)
	 (let ((id (vector-ref arg 0)))
	   (unless (%false-or-id? id)
	     (synner "invalid destructor specification" id))
	   (parsed-specs-destructor-set! parsed-specs id))))

      ((type-predicate)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid predicate specification" id))
	 (parsed-specs-type-predicate-set! parsed-specs id)))

      ((equality-predicate)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid equality predicate specification" id))
	 (parsed-specs-equality-predicate-set! parsed-specs id)))

      ((comparison-procedure)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid comparison procedure specification" id))
	 (parsed-specs-comparison-procedure-set! parsed-specs id)))

      ((hash-function)
       (let ((id (vector-ref arg 0)))
	 (unless (%false-or-id? id)
	   (synner "invalid hash function specification" id))
	 (parsed-specs-hash-function-set! parsed-specs id)))

      ((methods)
       (syntax-case arg ()
	 (#((?method-name ?method-implementation-procedure) ...)
	  (if (and (xp::all-identifiers? #'(?method-name ...))
		   (xp::all-identifiers? #'(?method-implementation-procedure ...)))
	      (parsed-specs-methods-set! parsed-specs #'#((?method-name . ?method-implementation-procedure) ...))
	    (synner "expected identifiers as method names and implementation procedure names")))
	 (_
	  (synner "invalid syntax in METHODS clause" arg))))

      (else
       (synner "invalid syntax clause" (xp::syntax-clause-spec-keyword clause-spec))))
    parsed-specs)

;;; --------------------------------------------------------------------

  (define (%boolean-or-id? obj)
    (or (identifier? obj)
	(boolean?    obj)))

  (define (%false-or-id? obj)
    (or (identifier? obj)
	(not obj)))

  (case-define synner
    ((message)
     (synner message #f))
    ((message subform)
     (syntax-violation (quote define-built-in-label-type) message input-form.stx subform)))

;;; --------------------------------------------------------------------

  (main input-form.stx))


;;;; numerics

(define-built-in-label-type <byte>
  (parent <fixnum>)
  (constructor #t)
  (destructor #f)
  (type-predicate byte-fixnum?))

(define-built-in-label-type <zero-byte>
  (parent <byte>)
  (constructor #t)
  (destructor #f)
  (type-predicate zero-fixnum?))

(define-built-in-label-type <positive-byte>
  (parent <byte>)
  (constructor #t)
  (destructor #f)
  (type-predicate positive-byte-fixnum?))

(define-built-in-label-type <negative-byte>
  (parent <byte>)
  (constructor #t)
  (destructor #f)
  (type-predicate negative-byte-fixnum?))

;;; --------------------------------------------------------------------

(define-built-in-label-type <octet>
  (parent <non-negative-fixnum>)
  (constructor #t)
  (destructor #f)
  (type-predicate octet-fixnum?))

(define-built-in-label-type <zero-octet>
  (parent <octet>)
  (constructor #t)
  (destructor #f)
  (type-predicate zero-fixnum?))

(define-built-in-label-type <positive-octet>
  (parent <octet>)
  (constructor #t)
  (destructor #f)
  (type-predicate positive-octet-fixnum?))


;;;; input/output ports

(define-built-in-label-type <input-port>
  (parent (or <binary-input-only-port> <binary-input/output-port>
	      <textual-input-only-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of input ports, independent from textual/binary specialisation
   (eof?			port-eof?)
   #| end of METHODS |# ))

(define-built-in-label-type <output-port>
  (parent (or <binary-output-only-port> <binary-input/output-port>
	      <textual-output-only-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of output ports, independent from textual/binary specialisation
   (flush			flush-output-port)
   #| end of METHODS |# ))

(define-built-in-label-type <input/output-port>
  (parent (or <binary-input/output-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of input ports, independent from textual/binary specialisation
   (eof?			port-eof?)
   ;;methods of output ports, independent from textual/binary specialisation
   (flush			flush-output-port)
   #| end of METHODS |# ))

;;; --------------------------------------------------------------------

(define-built-in-label-type <textual-port>
  (parent (or <textual-input-only-port> <textual-output-only-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   #| end of METHODS |# ))

(define-built-in-label-type <binary-port>
  (parent (or <binary-input-only-port> <binary-output-only-port> <binary-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   #| end of METHODS |# ))

;;; --------------------------------------------------------------------

(define-built-in-label-type <binary-input-port>
  (parent (or <binary-input-only-port> <binary-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of input ports, independent from textual/binary specialisation
   (eof?			port-eof?)
   #| end of METHODS |# ))

(define-built-in-label-type <binary-output-port>
  (parent (or <binary-output-only-port> <binary-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of output ports, independent from textual/binary specialisation
   (flush			flush-output-port)
   #| end of METHODS |# ))

;;; --------------------------------------------------------------------

(define-built-in-label-type <textual-input-port>
  (parent (or <textual-input-only-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of input ports, independent from textual/binary specialisation
   (eof?			port-eof?)
   #| end of METHODS |# ))

(define-built-in-label-type <textual-output-port>
  (parent (or <textual-output-only-port> <textual-input/output-port>))
  (constructor #t)
  (equality-predicate eq?)
  (hash-function port-hash)
  (methods
   ;;methods of "<port>"
   (open?			open-port?)
   (closed?			closed-port?)
   ;;
   (set-non-blocking-mode	port-set-non-blocking-mode!)
   (unset-non-blocking-mode	port-unset-non-blocking-mode!)
   (non-blocking-mode?		port-in-non-blocking-mode?)
   (reset			<port>-reset)
   ;;
   (has-position?		port-has-port-position?)
   (has-set-position?		port-has-set-port-position!?)
   (position			<port>-position)
   ;;
   (id				port-id)
   (fd				port-fd)
   (uid				port-uid)
   (transcoder			port-transcoder)
   ;;
   (close			close-port)
   (dump-status			port-dump-status)
   ;;
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)
   ;;methods of output ports, independent from textual/binary specialisation
   (flush			flush-output-port)
   #| end of METHODS |# ))


;;;; weak pairs

(define-built-in-label-type <weak-pair>
  (parent <pair>)
  (constructor weak-cons)
  (destructor #f)
  (type-predicate weak-pair?))


;;;; done

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
