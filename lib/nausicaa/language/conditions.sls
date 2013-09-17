;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: predefined condition types
;;;Date:Thu Sep  3, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language conditions)
  (export

    define-condition-type
    parent			fields
    try				catch
    finally			else

    ;; redefined from (rnrs conditions (6))
    &condition
    &warning
    &serious
    &error
    &violation
    &assertion
    &irritants
    &who
    &message
    &non-continuable
    &implementation-restriction
    &lexical
    &syntax
    &undefined

    ;; redefined from (rnrs arithmetic flonums (6))
    &no-infinities
    &no-nans

    ;; redefined from (rnrs io ports (6))
    &i/o
    &i/o-read
    &i/o-write
    &i/o-port
    &i/o-encoding
    &i/o-decoding
    &i/o-invalid-position
    &i/o-filename
    &i/o-file-protection
    &i/o-file-is-read-only
    &i/o-file-already-exists
    &i/o-file-does-not-exist

    ;; redefined from (vicare)
    &errno
    &h_errno
    &i/o-eagain
    &out-of-memory-error

    ;; convenience labels
    <common-conditions>

    ;; mismatch
    &mismatch make-mismatch-condition mismatch-condition?

    ;; wrong num args
    &wrong-num-args make-wrong-num-args-condition wrong-num-args-condition?
    condition-wrong-num-args.procname
    condition-wrong-num-args.expected
    condition-wrong-num-args.given
    raise-wrong-num-args-error

    ;; unimplemented
    &unimplemented make-unimplemented-condition unimplemented-condition?
    raise-unimplemented-error)
  (import (except (vicare)
		  define-condition-type
		  ;; (rnrs conditions (6))
		  &condition
		  &warning
		  &serious
		  &error
		  &violation
		  &assertion
		  &irritants
		  &who
		  &message
		  &non-continuable
		  &implementation-restriction
		  &lexical
		  &syntax
		  &undefined
		  ;; (rnrs arithmetic flonums (6))
		  &no-infinities
		  &no-nans
		  ;; (rnrs io ports (6))
		  &i/o
		  &i/o-read
		  &i/o-write
		  &i/o-port
		  &i/o-encoding
		  &i/o-decoding
		  &i/o-invalid-position
		  &i/o-filename
		  &i/o-file-protection
		  &i/o-file-is-read-only
		  &i/o-file-already-exists
		  &i/o-file-does-not-exist
		  ;; (vicare)
		  &errno
		  &h_errno
		  &i/o-eagain
		  &out-of-memory-error)
    (prefix (only (vicare)
		  define-condition-type
		  ;; (rnrs conditions (6))
		  &condition
		  &warning
		  &serious
		  &error
		  &violation
		  &assertion
		  &irritants
		  &who
		  &message
		  &non-continuable
		  &implementation-restriction
		  &lexical
		  &syntax
		  &undefined
		  ;; (rnrs arithmetic flonums (6))
		  &no-infinities
		  &no-nans
		  ;; (rnrs io ports (6))
		  &i/o
		  &i/o-read
		  &i/o-write
		  &i/o-port
		  &i/o-encoding
		  &i/o-decoding
		  &i/o-invalid-position
		  &i/o-filename
		  &i/o-file-protection
		  &i/o-file-is-read-only
		  &i/o-file-already-exists
		  &i/o-file-does-not-exist
                  ;; (vicare)
		  &errno
		  &h_errno
		  &i/o-eagain
		  &out-of-memory-error)
	    rnrs.)
    (nausicaa language oopp)
    (only (nausicaa language builtins)
	  <condition>)
    (only (nausicaa language auxiliary-syntaxes)
	  parent
	  fields))


(define-syntax (define-condition-type stx)
  (define who 'define-condition-type)

  (define (main stx)
    (syntax-case stx (parent fields)

      ;;Replacement for  R6RS syntax  defining the associated  label and
      ;;shadowing the parent label.
      ;;
      ((_ ?type ?supertype ?constructor ?predicate (?field ?accessor) ...)
       (begin
	 (unless (identifier? #'?type)
	   (synner "expected identifier as condition type name" #'?type))
	 (unless (identifier? #'?supertype)
	   (synner "expected identifier as condition supertype name" #'?supertype))
	 (unless (identifier? #'?constructor)
	   (synner "expected identifier as condition type constructor name" #'?constructor))
	 (unless (identifier? #'?predicate)
	   (synner "expected identifier as condition type predicate name" #'?predicate))
	 (unless (all-identifiers? #'(?field ...))
	   (synner "expected identifiers as condition type field names" #'(?field ...)))
	 (unless (all-identifiers? #'(?accessor ...))
	   (synner "expected identifiers as condition type accessor names" #'(?accessor ...)))
	 #'(begin
	     (with-label-shadowing (?supertype)
	       (rnrs.define-condition-type the-type
		 ?supertype ?constructor ?predicate
		 (?field ?accessor) ...))
	     (define-label ?type
	       (parent ?supertype)
	       (shadows the-type)
	       (protocol (lambda () ?constructor))
	       (predicate ?predicate)
	       (virtual-fields (immutable ?field ?accessor) ...)))))

      ;;Compressed  syntax with  parent  and fields.   Notice that  this
      ;;syntax allows for tagged fields.
      ;;
      ((?kwd ?type (parent ?supertype) (fields ?field ...))
       (begin
	 (unless (identifier? #'?type)
	   (synner "expected identifier as condition type name" #'?type))
	 (unless (identifier? #'?supertype)
	   (synner "expected identifier as condition supertype name" #'(parent ?supertype)))
	 (let ((name-str (%name-id->name-str #'?type)))
	   (with-syntax
	       ((CONSTRUCTOR	(%name-str->constructor-name-id #'?type name-str))
		(PREDICATE	(%name-str->predicate-name-id   #'?type name-str))
		(((FIELD TAGGED-FIELD ACCESSOR) ...)
		 (%fields-stx #'?type name-str #'(?field ...))))
	     #'(begin
		 (with-label-shadowing (?supertype)
		   (rnrs.define-condition-type the-type
		     ?supertype CONSTRUCTOR PREDICATE
		     (FIELD ACCESSOR) ...))
		 (define-label ?type
		   (shadows the-type)
		   (parent ?supertype)
		   (protocol (lambda () CONSTRUCTOR))
		   (predicate PREDICATE)
		   (virtual-fields (immutable TAGGED-FIELD ACCESSOR) ...)))))))

      ;;Compressed syntax without fields.
      ;;
      ((?kwd ?type (parent ?supertype))
       #'(?kwd ?type (parent ?supertype) (fields)))

      ;;Compressed syntax without parent.
      ;;
      ((?kwd ?type (fields ?field ...))
       #'(?kwd ?type (parent &condition) (fields ?field ...)))

      ;;Compressed syntax with neither parent nor fields.
      ;;
      ((?kwd ?type)
       #'(?kwd ?type (parent &condition) (fields)))

      (_
       (synner "invalid syntax for condition type definition"))))

  (define (%name-id->name-str type-name-id)
    ;;Given an  identifier representing the condition  object type name,
    ;;check if its first character is  an ampersand: if it is return the
    ;;name itself,  as a string, with  the ampersand stripped; if  it is
    ;;not, raise a syntax violation.
    ;;
    (let ((string-name (symbol->string (syntax->datum type-name-id))))
      (if (char=? #\& (string-ref string-name 0))
	  (substring string-name 1 (string-length string-name))
	(synner "condition type name must begin with \"&\" character" type-name-id))))

  (define (%name-str->constructor-name-id lexical-context name-str)
    ;;Given the condition  type name as a string,  return an identifier,
    ;;in  LEXICAL-CONTEXT,  representing  the   name  of  the  condition
    ;;object's constructor.
    ;;
    (datum->syntax lexical-context (string->symbol (string-append "make-" name-str "-condition"))))

  (define (%name-str->predicate-name-id lexical-context name-str)
    ;;Given the condition  type name as a string,  return an identifier,
    ;;in  LEXICAL-CONTEXT,  representing  the   name  of  the  condition
    ;;object's predicate.
    ;;
    (datum->syntax lexical-context (string->symbol (string-append name-str "-condition?"))))

  (define (%field-id->accessor-name-id lexical-context name-str field-name-id)
    ;;Given  a  string  representing  a   condition  type  name  and  an
    ;;identifier representing a condition object's field name: return an
    ;;identifier, in LEXICAL-CONTEXT,  representing the associated field
    ;;accessor name.
    ;;
    (let* ((field-name.str     (symbol->string (syntax->datum field-name-id)))
	   (accessor-name.str  (string-append "condition-" name-str "." field-name.str)))
      (datum->syntax lexical-context (string->symbol accessor-name.str))))

  (define (%fields-stx lexical-context name-str fields-stx)
    ;;Given the  condition type  name as  a string  and a  syntax object
    ;;holding a  list of  field specifications, return  a list  of field
    ;;specifications, in LEXICAL-CONTEXT, in  which every field spec has
    ;;the format:
    ;;
    ;;	(?FIELD-NAME ?TAGGED-FIELD-NAME ?ACCESSOR)
    ;;
    ;;where ?TAGGED-FIELD-NAME has the format:
    ;;
    ;;	(?FIELD-NAME ?TAG)
    ;;
    ;;and ?TAG can be "<top>".
    ;;
    (define (%field-id->accessor-id field-id)
      (%field-id->accessor-name-id lexical-context name-str field-id))
    (let next-field ((output '())
		     (input  fields-stx))
      (syntax-case input ()
	(()
	 (reverse output))

	;;Tagged field with accessor.
	;;
	((((?field-name ?tag) ?field-accessor) . ?other-fields)
	 (all-identifiers? #'(?field-name ?field-accessor ?tag))
	 (next-field (cons #'(?field-name (?field-name ?tag) ?field-accessor) output)
		     #'?other-fields))

	;;Tagged field without accessor.
	;;
	((((?field-name ?tag)) . ?other-fields)
	 (all-identifiers? #'(?field-name ?tag))
	 (with-syntax
	     ((ACCESSOR (%field-id->accessor-id #'?field-name)))
	   (next-field (cons #'(?field-name (?field-name ?tag) ACCESSOR) output)
		       #'?other-fields)))

	;;Field with accessor (no tags).
	;;
	(((?field-name ?field-accessor) . ?other-fields)
	 (all-identifiers? #'(?field-name ?field-accessor))
	 (next-field (cons #'(?field-name (?field-name <top>) ?field-accessor) output)
		     #'?other-fields))

	;;Field name only (no accessor, no tags).
	;;
	((?field-name . ?other-fields)
	 (identifier? #'?field-name)
	 (with-syntax
	     ((ACCESSOR (%field-id->accessor-id #'?field-name)))
	   (next-field (cons #'(?field-name (?field-name <top>) ACCESSOR) output)
		       #'?other-fields)))

	((?field . ?other-fields)
	 (synner "invalid condition type field specification" #'?field)))))

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (main stx))


;;;; try ... catch ... finally ... syntax

(define-auxiliary-syntaxes catch finally)

(define-syntax (try stx)
  (define who 'try)

  (define (main stx)
    (syntax-case stx (catch finally)
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause ...) (finally ?finally-body0 ?finally-body ...))
       (let ((var-id             #'?var)
	     (catch-clauses-stx  #'(?catch-clause0 ?catch-clause ...)))
	 (validate-variable #'?var)
	 (with-syntax
	     (((GUARD-CLAUSE ...) (parse-multiple-catch-clauses var-id catch-clauses-stx)))
	   #'(with-compensations
	       (push-compensation ?finally-body0 ?finally-body ...)
	       (guard (?var GUARD-CLAUSE ...) ?body)))))
      ((_ ?body (catch ?var ?catch-clause0 ?catch-clause ...))
       (let ((var-id             #'?var)
	     (catch-clauses-stx  #'(?catch-clause0 ?catch-clause ...)))
	 (validate-variable #'?var)
	 (with-syntax
	     (((GUARD-CLAUSE ...) (parse-multiple-catch-clauses var-id catch-clauses-stx)))
	   #'(guard (?var GUARD-CLAUSE ...) ?body))))
      (_
       (synner "invalid try syntax"))))

  (define (parse-multiple-catch-clauses var-id clauses-stx)
    (syntax-case clauses-stx (else)
      (()
       '())
      ;;The one with the ELSE clause must come first!!!
      (((else ?else-body0 ?else-body ...))
       clauses-stx)
      (((?tag ?tag-body0 ?tag-body ...) . ?other-clauses)
       (identifier? #'?tag)
       (cons #`((is-a? #,var-id ?tag)
		(with-tags ((#,var-id ?tag))
		  ?tag-body0 ?tag-body ...))
	     (parse-multiple-catch-clauses var-id #'?other-clauses)))
      ((?clause . ?other-clauses)
       (synner "invalid catch clause in try syntax" #'?clause))))

  (define (validate-variable var-id)
    (unless (identifier? var-id)
      (synner "expected identifier as variable" var-id)))

  (define synner
    (case-lambda
     ((message)
      (synner message #f))
     ((message subform)
      (syntax-violation who message stx subform))))

  (main stx))

;; (define-syntax catch)
;; (define-syntax (try stx)
;;   (syntax-case stx (catch else)
;;     ;;The one with the ELSE clause must come first!!!
;;     ((_ ?body
;; 	(catch ?var
;; 	  (?tag ?tag-body0 ?tag-body ...)
;; 	  ...
;; 	  (else ?else-body0 ?else-body ...)))
;;      #'(guard (?var
;; 	       ((is-a? ?var ?tag) (with-tags ((?var ?tag)) ?tag-body0 ?tag-body ...))
;; 	       ...
;; 	       (else ?else-body0 ?else-body ...))
;; 	 ?body))
;;     ((_ ?body
;; 	(catch ?var
;; 	  (?tag ?tag-body0 ?tag-body ...)
;; 	  ...))
;;      #'(guard (?var
;; 	       ((is-a? ?var ?tag) (with-tags ((?var ?tag)) ?tag-body0 ?tag-body ...))
;; 	       ...)
;; 	 ?body))
;;     ))


;;;; (rnrs conditions (6))

(define-label &condition
  (predicate condition?)
  (shadows rnrs.&condition))

(define-label &warning
  (parent &condition)
  (protocol (lambda () make-warning))
  (predicate warning?)
  (shadows rnrs.&warning))

(define-label &serious
  (parent &condition)
  (protocol (lambda () make-serious-condition))
  (predicate serious-condition?)
  (shadows rnrs.&serious))

(define-label &error
  (parent &serious)
  (protocol (lambda () make-error))
  (predicate error?)
  (shadows rnrs.&error))

(define-label &violation
  (parent &serious)
  (protocol (lambda () make-violation))
  (predicate violation?)
  (shadows rnrs.&violation))

(define-label &assertion
  (protocol (lambda () make-assertion-violation))
  (parent &violation)
  (predicate assertion-violation?)
  (shadows rnrs.&assertion))

(define-label &irritants
  (parent &condition)
  (protocol (lambda () make-irritants-condition))
  (predicate irritants-condition?)
  (shadows rnrs.&irritants)
  (virtual-fields (immutable irritants condition-irritants)))

(define-label &who
  (protocol (lambda () make-who-condition))
  (predicate who-condition?)
  (shadows rnrs.&who)
  (virtual-fields (immutable who condition-who)))

(define-label &message
  (parent &condition)
  (protocol (lambda () make-message-condition))
  (predicate message-condition?)
  (shadows rnrs.&message)
  (virtual-fields (immutable message condition-message)))

(define-label &non-continuable
  (parent &violation)
  (protocol (lambda () make-non-continuable-violation))
  (predicate non-continuable-violation?)
  (shadows rnrs.&non-continuable))

(define-label &implementation-restriction
  (parent &violation)
  (protocol (lambda () make-implementation-restriction-violation))
  (predicate implementation-restriction-violation?)
  (shadows rnrs.&implementation-restriction))

(define-label &lexical
  (parent &violation)
  (protocol (lambda () make-lexical-violation))
  (predicate lexical-violation?)
  (shadows rnrs.&lexical))

(define-label &syntax
  (parent &violation)
  (protocol (lambda () make-syntax-violation))
  (predicate syntax-violation?)
  (shadows rnrs.&syntax)
  (virtual-fields (immutable form   syntax-violation-form)
		  (immutable suform syntax-violation-subform)))

(define-label &undefined
  (parent &violation)
  (protocol (lambda () make-undefined-violation))
  (predicate undefined-violation?)
  (shadows rnrs.&undefined))


;;;; (rnrs arithmetic flonums (6))

(define-label &no-infinities
  (parent &implementation-restriction)
  (protocol (lambda () make-no-infinities-violation))
  (predicate no-infinities-violation?)
  (shadows rnrs.&no-infinities))

(define-label &no-nans
  (parent &implementation-restriction)
  (protocol (lambda () make-no-nans-violation))
  (predicate no-nans-violation?)
  (shadows &no-nans))


;;;; (rnrs io ports (6))

(define-label &i/o
  (parent &error)
  (protocol (lambda () make-i/o-error))
  (predicate i/o-error?)
  (shadows rnrs.&i/o))

;;; --------------------------------------------------------------------

(define-label &i/o-read
  (parent &i/o)
  (protocol (lambda () make-i/o-read-error))
  (predicate i/o-read-error?)
  (shadows rnrs.&i/o-read))

(define-label &i/o-write
  (parent &i/o)
  (protocol (lambda () make-i/o-write-error))
  (predicate i/o-write-error?)
  (shadows rnrs.&i/o-write))

;;; --------------------------------------------------------------------

(define-label &i/o-port
  (parent &i/o)
  (protocol (lambda () make-i/o-port-error))
  (predicate i/o-port-error?)
  (shadows rnrs.&i/o-port)
  (virtual-fields (immutable port i/o-error-port)))

(define-label &i/o-encoding
  (parent &i/o-port)
  (protocol (lambda () make-i/o-encoding-error))
  (predicate i/o-encoding-error?)
  (shadows rnrs.&i/o-encoding)
  (virtual-fields (immutable error-char i/o-encoding-error-char)))

(define-label &i/o-decoding
  (parent &i/o-port)
  (protocol (lambda () make-i/o-decoding-error))
  (predicate i/o-decoding-error?)
  (shadows rnrs.&i/o-decoding))

;;; --------------------------------------------------------------------

(define-label &i/o-invalid-position
  (parent &i/o)
  (protocol (lambda () make-i/o-invalid-position-error))
  (predicate i/o-invalid-position-error?)
  (shadows rnrs.&i/o-invalid-position)
  (virtual-fields (immutable position i/o-error-position)))

(define-label &i/o-filename
  (parent &i/o)
  (protocol (lambda () make-i/o-filename-error))
  (predicate i/o-filename-error?)
  (shadows rnrs.&i/o-filename)
  (virtual-fields (immutable filename i/o-error-filename)))

(define-label &i/o-file-protection
  (parent &i/o-filename)
  (protocol (lambda () make-i/o-file-protection-error))
  (predicate i/o-file-protection-error?)
  (shadows rnrs.&i/o-file-protection))

(define-label &i/o-file-is-read-only
  (parent &i/o-file-protection)
  (protocol (lambda () make-i/o-file-is-read-only-error))
  (predicate i/o-file-is-read-only-error?)
  (shadows rnrs.&i/o-file-is-read-only))

(define-label &i/o-file-already-exists
  (parent &i/o-filename)
  (protocol (lambda () make-i/o-file-already-exists-error))
  (predicate i/o-file-already-exists-error?)
  (shadows rnrs.&i/o-file-already-exists))

(define-label &i/o-file-does-not-exist
  (parent &i/o-filename)
  (protocol (lambda () make-i/o-file-does-not-exist-error))
  (predicate i/o-file-does-not-exist-error?)
  (shadows rnrs.&i/o-file-does-not-exist))


;;;; (vicare)

(define-label &errno
  (parent &condition)
  (protocol (lambda () make-errno-condition))
  (predicate errno-condition?)
  (shadows rnrs.&errno))

(define-label &h_errno
  (parent &condition)
  (protocol (lambda () make-h_errno-condition))
  (predicate h_errno-condition?)
  (shadows rnrs.&h_errno))

(define-label &i/o-eagain
  (parent &i/o)
  (protocol (lambda () make-i/o-eagain))
  (predicate i/o-eagain-error?)
  (shadows rnrs.&i/o-eagain))

(define-label &out-of-memory-error
  (parent &error)
  (protocol (lambda () make-out-of-memory-error))
  (predicate out-of-memory-error?)
  (shadows rnrs.&out-of-memory-error))


;;;; custom condition types

(define-label <common-conditions>
  (parent <condition>)
  (protocol (lambda ()
	      (lambda (who message irritants)
		(condition
		 (make-who-condition who)
		 (make-message-condition message)
		 (make-irritants-condition irritants)))))
  (predicate (lambda (obj)
	       (and ((&who) obj)
		    ((&message) obj)
		    ((&irritants) obj))))
  (virtual-fields (immutable (who	&who)		condition-who)
		  (immutable (message	&message)	condition-message)
		  (immutable (irritants &irritants)	condition-irritants)))

(define-condition-type &mismatch
  (parent &assertion))

(define-condition-type &wrong-num-args
  (parent &assertion)
  (fields procname expected given))

(define-syntax raise-wrong-num-args-error
  (syntax-rules ()
    ((_ ?who ?message ?procname ?expected ?given)
     (raise (condition (make-who-condition ?who)
		       (make-message-condition ?message)
		       (make-wrong-num-args-condition ?procname ?expected ?given))))))

(define-condition-type &unimplemented
  (parent &error))

(define raise-unimplemented-error
  (case-lambda
   ((who)
    (raise-unimplemented-error who "feature not implemented or not available" #f))
   ((who message . irritants)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-unimplemented-condition)
		(make-irritants-condition irritants))))))

;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'rnrs.define-condition-type 'scheme-indent-function 1)
;; End:
