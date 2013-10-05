;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: utility functions for macro-writing
;;;Date: Thu Aug 29, 2013
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
(library (ikarus.syntax-utilities)
  (export

    ;; identifier processing: generic functions
    identifier-prefix		identifier-suffix
    identifier-append		identifier-format
    identifier->string		string->identifier

    duplicate-identifiers?	delete-duplicate-identifiers
    identifier-memq

    ;; identifier processing: records API
    identifier-record-constructor
    identifier-record-predicate
    identifier-record-field-accessor
    identifier-record-field-mutator

    ;; identifier processing: structs API
    identifier-struct-constructor
    identifier-struct-predicate
    identifier-struct-field-accessor
    identifier-struct-field-mutator

    ;; pairs processing
    syntax-car			syntax-cdr
    syntax->list		identifiers->list
    all-identifiers?

    ;; vectors processing
    syntax->vector

    ;; unwrapping
    syntax-unwrap

    ;; comparison
    syntax=?
    identifier=symbol?

    ;; inspection
    #;quoted-syntax-object?

    ;; clauses helpers
    syntax-clauses-unwrap
    syntax-clauses-filter
    syntax-clauses-remove
    syntax-clauses-partition
    syntax-clauses-collapse
    syntax-clauses-verify-at-least-once
    syntax-clauses-verify-at-most-once
    syntax-clauses-verify-exactly-once
    syntax-clauses-verify-mutually-inclusive
    syntax-clauses-verify-mutually-exclusive

    ;; clause specification structs
    make-syntax-clause-spec
    syntax-clause-spec?
    syntax-clause-spec-keyword
    syntax-clause-spec-min-number-of-occurrences
    syntax-clause-spec-max-number-of-occurrences
    syntax-clause-spec-min-number-of-arguments
    syntax-clause-spec-max-number-of-arguments
    syntax-clause-spec-mutually-inclusive
    syntax-clause-spec-mutually-exclusive
    syntax-clauses-single-spec
    syntax-clauses-fold-specs)
  (import (except (vicare)
		  ;; identifier processing: generic functions
		  identifier-prefix		identifier-suffix
		  identifier-append		identifier-format
		  identifier->string		string->identifier

		  duplicate-identifiers?	delete-duplicate-identifiers
		  identifier-memq

		  ;; identifier processing: records API
		  identifier-record-constructor
		  identifier-record-predicate
		  identifier-record-field-accessor
		  identifier-record-field-mutator

		  ;; identifier processing: structs API
		  identifier-struct-constructor
		  identifier-struct-predicate
		  identifier-struct-field-accessor
		  identifier-struct-field-mutator

		  ;; pairs processing
		  syntax-car			syntax-cdr
		  syntax->list			identifiers->list
		  all-identifiers?

		  ;; vectors processing
		  syntax->vector

		  ;; unwrapping
		  syntax-unwrap

		  ;; comparison
		  syntax=?
		  identifier=symbol?

		  ;; inspection
		  #;quoted-syntax-object?

		  ;; clauses helpers
		  syntax-clauses-unwrap
		  syntax-clauses-filter
		  syntax-clauses-remove
		  syntax-clauses-partition
		  syntax-clauses-collapse
		  syntax-clauses-verify-at-least-once
		  syntax-clauses-verify-at-most-once
		  syntax-clauses-verify-exactly-once
		  syntax-clauses-verify-mutually-inclusive
		  syntax-clauses-verify-mutually-exclusive

		  ;; clause specification structs
		  make-syntax-clause-spec
		  syntax-clause-spec?
		  syntax-clause-spec-keyword
		  syntax-clause-spec-min-number-of-occurrences
		  syntax-clause-spec-max-number-of-occurrences
		  syntax-clause-spec-min-number-of-arguments
		  syntax-clause-spec-max-number-of-arguments
		  syntax-clause-spec-mutually-inclusive
		  syntax-clause-spec-mutually-exclusive
		  syntax-clauses-single-spec
		  syntax-clauses-fold-specs)
    (vicare unsafe operations)
    (vicare arguments validation))


;;;; helpers

(define (%make-synner who)
  (lambda (message subform)
    (syntax-violation who message subform #f)))


;;;; identifiers processing: generic functions

(define (identifier-prefix prefix id)
  (define who 'identifier-prefix)
  (assert (identifier? id))
  (string->identifier id
		      (string-append
		       (cond ((string? prefix)
			      prefix)
			     ((symbol? prefix)
			      (symbol->string prefix))
			     ((identifier? prefix)
			      (symbol->string (syntax->datum prefix)))
			     (else
			      (assertion-violation who
				"expected string, symbol or identifier as prefix argument" prefix)))
		       (symbol->string (syntax->datum id)))))

(define (identifier-suffix id suffix)
  (define who 'identifier-suffix)
  (assert (identifier? id))
  (string->identifier id
		      (string-append
		       (symbol->string (syntax->datum id))
		       (cond ((string? suffix)
			      suffix)
			     ((symbol? suffix)
			      (symbol->string suffix))
			     ((identifier? suffix)
			      (symbol->string (syntax->datum suffix)))
			     (else
			      (assertion-violation who
				"expected string, symbol or identifier as suffix argument" suffix))))))

(define (identifier-append ctx . items)
  (define who 'identifier-append)
  (identifier? ctx)
  (receive (port getter)
      (open-string-output-port)
    (for-each (lambda (item)
		(display (cond ((string? item)
				item)
			       ((symbol? item)
				(symbol->string item))
			       ((identifier? item)
				(symbol->string (syntax->datum item)))
			       (else
				(assertion-violation who
				  "expected string, symbol or identifier as item argument" item)))
			 port))
      items)
    (string->identifier ctx (getter))))

(define (identifier-format ctx template . items)
  (define who 'identifier-format)
  (string->identifier ctx
		      (apply format template
			     (map (lambda (item)
				    (cond ((string? item)
					   item)
					  ((symbol? item)
					   (symbol->string item))
					  ((identifier? item)
					   (symbol->string (syntax->datum item)))
					  (else
					   (assertion-violation who
					     "expected string, symbol or identifier as item argument" item))))
			       items))))

;;; --------------------------------------------------------------------

(define (identifier->string id)
  (assert (identifier? id))
  (symbol->string (syntax->datum id)))

(define (string->identifier ctx str)
  (assert (identifier? ctx))
  (datum->syntax ctx (string->symbol str)))

;;; --------------------------------------------------------------------

(define duplicate-identifiers?
  ;;Recursive  function.   Search  the   list  of  identifiers  STX  for
  ;;duplicate  identifiers; at  the  first duplicate  found, return  it;
  ;;return false if no duplications are found.
  ;;
  (case-lambda
   ((stx)
    (duplicate-identifiers? stx free-identifier=?))
   ((stx identifier=)
    (syntax-case stx ()
      (() #f)
      ((?car . ?cdr)
       (let loop ((first #'?car)
		  (rest  #'?cdr))
	 (syntax-case rest ()
	   (()
	    (duplicate-identifiers? #'?cdr identifier=))
	   ((?car . ?cdr)
	    (if (identifier= first #'?car)
		first
	      (loop first #'?cdr))))))))))

(define delete-duplicate-identifiers
  (case-lambda
   ((ids)
    (delete-duplicate-identifiers ids free-identifier=?))
   ((ids identifier=)
    ;;Given the list  of identifiers IDS remove  the duplicate identifiers
    ;;and return a proper list of unique identifiers.
    ;;
    (assert (and (list? ids) (for-all identifier? ids)))
    (let clean-tail ((ids ids))
      (if (null? ids)
	  '()
	(let ((head (car ids)))
	  (cons head (clean-tail (remp (lambda (id)
					 (identifier= id head))
				   (cdr ids))))))))))

(define identifier-memq
  (case-lambda
   ((id ids)
    (identifier-memq id ids free-identifier=?))
   ((id ids identifier=)
    ;;Search the list of identifiers IDS for one which is IDENTIFIER= to
    ;;ID and return the sublist starting  with it; return false if ID is
    ;;not present.
    ;;
    (assert (identifier? id))
    (assert (and (list? ids) (for-all identifier? ids)))
    (let recur ((ids ids))
      (cond ((null? ids)
	     #f)
	    ((let ((stx (car ids)))
	       (and (identifier? stx)
		    (identifier= id stx)))
	     ids)
	    (else
	     (recur (cdr ids))))))))


;;;; identifiers processing: records API

(define (identifier-record-constructor type-id)
  (identifier-prefix "make-" type-id))

(define (identifier-record-predicate type-id)
  (identifier-suffix type-id "?"))

(define (identifier-record-field-accessor type-id field-name)
  (identifier-append type-id type-id "-" field-name))

(define (identifier-record-field-mutator type-id field-name)
  (identifier-append type-id type-id "-" field-name "-set!"))


;;;; identifiers processing: structs API

(define (identifier-struct-constructor type-id)
  (identifier-prefix "make-" type-id))

(define (identifier-struct-predicate type-id)
  (identifier-suffix type-id "?"))

(define (identifier-struct-field-accessor type-id field-name)
  (identifier-append type-id type-id "-" field-name))

(define (identifier-struct-field-mutator type-id field-name)
  (identifier-append type-id "set-" type-id "-" field-name "!"))


;;;; pairs processing

(define syntax-car
  (case-lambda
   ((stx)
    (syntax-car stx (%make-synner 'syntax-car)))
   ((stx synner)
    (assert (procedure? synner))
    (syntax-case stx ()
      ((?car . ?cdr)
       #'?car)
      (_
       (synner "expected syntax object holding pair as argument" stx))))))

(define syntax-cdr
  (case-lambda
   ((stx)
    (syntax-cdr stx (%make-synner 'syntax-cdr)))
   ((stx synner)
    (assert (procedure? synner))
    (syntax-case stx ()
      ((?car . ?cdr)
       #'?cdr)
      (_
       (synner "expected syntax object holding pair as argument" stx))))))

(define syntax->list
  (case-lambda
   ((stx)
    (syntax->list stx (%make-synner 'syntax->list)))
   ((stx synner)
    (assert (procedure? synner))
    (let recur ((stx stx))
      (syntax-case stx ()
	(() '())
	((?car . ?cdr)
	 (identifier? #'?car)
	 (cons #'?car (recur #'?cdr)))
	((?car . ?cdr)
	 (cons (syntax-unwrap #'?car) (recur #'?cdr)))
	(_
	 (synner "expected syntax object holding proper list as argument" stx)))))))

(define identifiers->list
  (case-lambda
   ((stx)
    (identifiers->list stx (%make-synner 'identifiers->list)))
   ((stx synner)
    (assert (procedure? synner))
    (let recur ((stx stx))
      (syntax-case stx ()
	(() '())
	((?car . ?cdr)
	 (identifier? #'?car)
	 (cons #'?car (recur #'?cdr)))
	(_
	 (synner "expected syntax object holding proper list of identifiers as argument" stx)))))))

(define (all-identifiers? stx)
  (syntax-case stx ()
    (() #t)
    ((?car . ?cdr)
     (identifier? #'?car)
     (all-identifiers? #'?cdr))
    (_ #f)))


;;;; vectors processing

(define syntax->vector
  (case-lambda
   ((stx)
    (syntax->vector stx (%make-synner 'syntax->vector)))
   ((stx synner)
    (syntax-case stx ()
      (() '())
      (#(?item ...)
       (list->vector (syntax->list #'(?item ...) synner)))
      (_
       (synner "expected syntax object holding vector as argument" stx))))))


;;;; unwrapping

(define (syntax-unwrap stx)
  ;;Given a syntax object STX  decompose it and return the corresponding
  ;;S-expression holding datums and identifiers.  Take care of returning
  ;;a proper  list when the  input is a  syntax object holding  a proper
  ;;list.
  ;;
  (syntax-case stx ()
    (()
     '())
    ((?car . ?cdr)
     (cons (syntax-unwrap (syntax ?car))
	   (syntax-unwrap (syntax ?cdr))))
    (#(?item ...)
     (list->vector (syntax-unwrap (syntax (?item ...)))))
    (?atom
     (identifier? (syntax ?atom))
     (syntax ?atom))
    (?atom
     (syntax->datum (syntax ?atom)))))


;;;; comparison

(define (syntax=? stx1 stx2)
  (define (%syntax=? stx1 stx2)
    (cond ((and (identifier? stx1)
		(identifier? stx2))
	   (free-identifier=? stx1 stx2))
	  ((and (pair? stx1)
		(pair? stx2))
	   (and (syntax=? (car stx1) (car stx2))
		(syntax=? (cdr stx1) (cdr stx2))))
	  ((and (vector? stx1)
		(vector? stx2))
	   (vector-for-all syntax=? stx1 stx2))
	  (else
	   (equal? stx1 stx2))))
  (%syntax=? (syntax-unwrap stx1) (syntax-unwrap stx2)))

(define (identifier=symbol? id sym)
  ;;Return true  if the symbol  SYM is equal to  the symbol name  of the
  ;;identifier ID.
  ;;
  (define who 'identifier=symbol?)
  (with-arguments-validation (who)
      ((identifier	id)
       (symbol		sym))
    (eq? sym (syntax->datum id))))


;;;; inspection

;;FIXME This is commented out for now because the C language FASL reader
;;cannot read R6RS records.  (Marco Maggi; Fri Aug 30, 2013)
;;
;; (define (quoted-syntax-object? stx)
;;   ;;Given a syntax object: return true if  it is a list whose car is one
;;   ;;among   QUOTE,  QUASIQUOTE,   SYNTAX,   QUASISYNTAX;  return   false
;;   ;;otherwise.
;;   ;;
;;   (syntax-case stx ()
;;     ((?car . ?cdr)
;;      (and (identifier? #'?car)
;; 	  (or (free-identifier=? #'?car #'quote)
;; 	      (free-identifier=? #'?car #'quasiquote)
;; 	      (free-identifier=? #'?car #'syntax)
;; 	      (free-identifier=? #'?car #'quasisyntax)))
;;      #t)
;;     (_ #f)))


;;;; syntax clauses utilities

(define syntax-clauses-unwrap
  (case-lambda
   ((clauses)
    (syntax-clauses-unwrap clauses (%make-synner 'syntax-clauses-unwrap)))
   ((clauses synner)
    ;;Scan the syntax object CLAUSES expecting a list with the format:
    ;;
    ;;    ((?identifier . ?things) ...)
    ;;
    ;;return a syntax object representing CLAUSES fully unwrapped.
    ;;
    ;;SYNNER must  be a closure  used to raise  a syntax violation  if a
    ;;parse  error occurs;  it must  accept two  arguments: the  message
    ;;string, the invalid subform.
    ;;
    (syntax-case clauses ()

      (() '())

      (((?identifier . ?things) . ?other-clauses)
       (identifier? #'?identifier)
       (cons (cons #'?identifier (syntax-unwrap #'?things))
	     (syntax-clauses-unwrap #'?other-clauses synner)))

      (((?wrong . ?things) . ?other-clauses)
       (synner "expected identifier as syntax clause first element" #'?wrong))

      ((?clause . ?other-clauses)
       (synner "invalid clause syntax" #'?clause))

      (_
       (synner "expected list of elements as syntax clauses" clauses))))))

(define (syntax-clauses-filter keyword-identifiers clauses)
  ;;Given a fully unwrapped syntax object holding a list of clauses with
  ;;the format:
  ;;
  ;;    ((?identifier ?thing ...) ...)
  ;;
  ;;select  the ones  having ?IDENTIFIER  being FREE-IDENTIFIER=?  to an
  ;;identifier in  the list KEYWORD-IDENTIFIERS and  return the selected
  ;;clauses in a fully unwrapped syntax object holding the list of them;
  ;;return null if no matching clause is found.
  ;;
  (assert (all-identifiers? keyword-identifiers))
  (filter (lambda (clause)
	    (exists (lambda (keyword)
		      (free-identifier=? keyword (car clause)))
	      keyword-identifiers))
    clauses))

(define (syntax-clauses-remove keyword-identifiers clauses)
  ;;Given a fully unwrapped syntax object holding a list of clauses with
  ;;the format:
  ;;
  ;;    ((?identifier ?thing ...) ...)
  ;;
  ;;discard the  ones having ?IDENTIFIER being  FREE-IDENTIFIER=?  to an
  ;;identifier in  the list KEYWORD-IDENTIFIERS and  return the selected
  ;;clauses in a fully unwrapped syntax object holding the list of them;
  ;;return null if no matching clause is found.
  ;;
  (assert (all-identifiers? keyword-identifiers))
  (remp (lambda (clause)
	  (exists (lambda (keyword)
		    (free-identifier=? keyword (car clause)))
	    keyword-identifiers))
    clauses))

(define (syntax-clauses-partition keyword-identifiers clauses)
  ;;Given a fully unwrapped syntax object holding a list of clauses with
  ;;the format:
  ;;
  ;;    ((?identifier ?thing ...) ...)
  ;;
  ;;partition   it    into   the    ones   having    ?IDENTIFIER   being
  ;;FREE-IDENTIFIER=?  to an identifier  in the list KEYWORD-IDENTIFIERS
  ;;and the others.  Return two values: the list of matching clauses and
  ;;the list of non-matching clauses.
  ;;
  (assert (all-identifiers? keyword-identifiers))
  (partition (lambda (clause)
	       (exists (lambda (keyword)
			 (free-identifier=? keyword (car clause)))
		 keyword-identifiers))
    clauses))

(define (syntax-clauses-collapse clauses)
  ;;Given a fully unwrapped syntax object holding a list of clauses with
  ;;the format:
  ;;
  ;;    ((?identifier ?thing ...) ...)
  ;;
  ;;collapse the clauses  having equal ?IDENTIFIER into  a single clause
  ;;and return the resulting unwrapped syntax object.  Example:
  ;;
  ;;    (syntax-clauses-collapse ((#'fields #'a #'b #'c)
  ;;                              (#'fields #'d #'e #'f)))
  ;;    => ((#'fields #'a #'b #'c #'d #'e #'f))
  ;;
  (if (null? clauses)
      '()
    (let* ((A     (car clauses))
	   (D     (cdr clauses))
	   (A-key (car A)))
      (receive (match no-match)
	  (syntax-clauses-partition (list A-key) D)
	(cons (cons A-key (apply append (cdr A) (map cdr match)))
	      (syntax-clauses-collapse no-match))))))


;;;; syntax clauses constraints

(define syntax-clauses-verify-at-least-once
  (case-lambda
   ((keywords clauses)
    (syntax-clauses-verify-at-least-once keywords clauses
					 (%make-synner 'syntax-clauses-verify-at-least-once)))
   ((keywords clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify that all  the identifiers in the list  KEYWORDS are present
    ;;at  least   once  as   clause  keywords.   If   successful  return
    ;;unspecified values, else call SYNNER.
    ;;
    (assert (all-identifiers? keywords))
    (assert (procedure? synner))
    (let loop ((keywords keywords))
      (unless (null? keywords)
	(let ((keyword (car keywords)))
	  (if (exists (lambda (clause)
			(free-identifier=? keyword (car clause)))
		clauses)
	      (loop (cdr keywords))
	    (synner "missing mandatory clause" keyword))))))
   ))

(define syntax-clauses-verify-at-most-once
  (case-lambda
   ((keywords clauses)
    (syntax-clauses-verify-at-most-once keywords clauses
					(%make-synner 'syntax-clauses-verify-at-most-once)))
   ((keywords clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify that  the identifiers in  the list KEYWORDS are  present at
    ;;most once  as clause  keywords.  If successful  return unspecified
    ;;values, else call SYNNER.
    ;;
    (assert (all-identifiers? keywords))
    (assert (procedure? synner))
    (let loop ((keywords keywords))
      (unless (null? keywords)
	(let* ((keyword (car keywords))
	       (present (syntax-clauses-filter (list keyword) clauses)))
	  (if (>= 1 (length present))
	      (loop (cdr keywords))
	    (synner "clause must be present at most once" present))))))
   ))

(define syntax-clauses-verify-exactly-once
  (case-lambda
   ((keywords clauses)
    (syntax-clauses-verify-exactly-once keywords clauses
					(%make-synner 'syntax-clauses-verify-exactly-once)))
   ((keywords clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify  that the  identifiers  in the  list  KEYWORDS are  present
    ;;exactly once as clause keywords.  If successful return unspecified
    ;;values, else call SYNNER.
    ;;
    (assert (all-identifiers? keywords))
    (assert (procedure? synner))
    (let loop ((keywords keywords))
      (unless (null? keywords)
	(let* ((keyword (car keywords))
	       (present (syntax-clauses-filter (list keyword) clauses))
	       (number  (length present)))
	  (if (= 1 number)
	      (loop (cdr keywords))
	    (synner "clause must be present exactly once" (if (< 1 number)
							      present
							    keyword)))))))
   ))

(define syntax-clauses-verify-mutually-inclusive
  (case-lambda
   ((keywords clauses)
    (syntax-clauses-verify-mutually-inclusive keywords clauses
					      (%make-synner 'syntax-clauses-verify-mutually-inclusive)))
   ((keywords clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify that the if one of  the identifiers in the list KEYWORDS is
    ;;present at  least once as  clause identifier, then all  the others
    ;;are present  too.  If  successful return unspecified  values, else
    ;;call SYNNER.
    ;;
    (module ()
      (assert (all-identifiers? keywords))
      (assert (procedure? synner)))
    (define-struct flag
      (id present?))
    (define flags
      (map (lambda (key)
	     (make-flag key #f))
	keywords))
    (let next-clause ((clauses clauses))
      (if (null? clauses)
	  ;;If at least one is present...
	  (when (exists flag-present? flags)
	    ;;... check that all are present.
	    (let ((missing (fold-left (lambda (knil flag)
					(if ($flag-present? flag)
					    knil
					  (cons ($flag-id flag) knil)))
			     '()
			     flags)))
	      (unless (null? missing)
		(synner "mutually inclusive clauses are missing" missing))))
	(let next-flag ((flags flags))
	  (cond ((null? flags)
		 (next-clause ($cdr clauses)))
		((free-identifier=? ($flag-id ($car flags))
				    ($caar clauses))
		 ($set-flag-present?! ($car flags) #t)
		 (next-clause ($cdr clauses)))
		(else
		 (next-flag ($cdr flags)))))
	)))
   ))

(define syntax-clauses-verify-mutually-exclusive
  (case-lambda
   ((keywords clauses)
    (syntax-clauses-verify-mutually-exclusive keywords clauses
					      (%make-synner 'syntax-clauses-verify-mutually-exclusive)))
   ((keywords clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify that the if one of  the identifiers in the list KEYWORDS is
    ;;present at  least once as  clause identifier, then all  the others
    ;;are NOT  present.  If  successful return unspecified  values, else
    ;;call SYNNER.
    ;;
    (module ()
      (assert (all-identifiers? keywords))
      (assert (procedure? synner)))
    (define-struct flag
      (id clauses))
    (define-inline (flag-cons-clause! flag clause)
      ($set-flag-clauses! flag (cons clause ($flag-clauses flag))))
    (define flags
      (map (lambda (key)
	     (make-flag key '()))
	keywords))
    (let next-clause ((clauses clauses))
      (if (null? clauses)
	  ;;If at least one is present...
	  (when (exists (lambda (flag)
			  (not (null? (flag-clauses flag))))
		  flags)
	    ;;... check that all are present.
	    (let ((present (fold-left (lambda (knil flag)
					(if (null? ($flag-clauses flag))
					    knil
					  (append ($flag-clauses flag) knil)))
			     '()
			     flags)))
	      (unless (<= (length present) 1)
		(synner "mutually exclusive clauses are present" (reverse present)))))
	(let next-flag ((flags flags))
	  (cond ((null? flags)
		 (next-clause ($cdr clauses)))
		((free-identifier=? ($flag-id ($car flags))
				    ($caar clauses))
		 (flag-cons-clause! ($car flags) ($car clauses))
		 (next-clause ($cdr clauses)))
		(else
		 (next-flag ($cdr flags)))))
	)))
   ))


;;;; clause specification structs

(define-record-type syntax-clause-spec
  (opaque #t)
  (nongenerative vicare:syntax-clause)
  (fields (immutable keyword)
		;An identifier representing the keyword for this clause.
	  (immutable min-number-of-occurrences)
		;A  non-negative real  number  representing the  allowed
		;minimum number of occurrences for this clause.  0 means
		;the  clause   is  optional;  1  means   the  clause  is
		;mandatory.
	  (immutable max-number-of-occurrences)
		;A  non-negative real  number  representing the  allowed
		;maximum number of occurrences for this clause.  0 means
		;the clause is forbidden; 1 means the clause must appear
		;at most  once; +inf.0 means  the clause can  appear any
		;number of times.
	  (immutable min-number-of-arguments)
		;A  non-negative real  number  representing the  allowed
		;minimum number  of arguments for this  clause.  0 means
		;the clause  can have no  arguments; 1 means  the clause
		;must have at least one argument.
	  (immutable max-number-of-arguments)
		;A  non-negative real  number  representing the  allowed
		;maximum number  of arguments for this  clause.  0 means
		;the clause  has no arguments;  1 means the  clause must
		;have at most one arguments; +inf.0 means the clause can
		;have any number of arguments.
	  (immutable mutually-inclusive)
		;A list  identifiers representing clauses  keywords that
		;must appear along with this one.
	  (immutable mutually-exclusive)
		;A list  identifiers representing clauses  keywords that
		;must not appear along with this one.
	  )
  (protocol
   (lambda (make-record)
     (lambda (keyword min-occur max-occur min-args max-args mutually-inclusive mutually-exclusive)
       (define (count? obj)
	 (and (real? obj) (<= 0 obj)))
       (assert (identifier? keyword))
       (assert (count? min-occur))
       (assert (count? max-occur))
       (assert (count? min-args))
       (assert (count? max-args))
       (assert (<= min-occur max-occur))
       (assert (<= min-args max-args))
       (assert (all-identifiers? mutually-inclusive))
       (assert (all-identifiers? mutually-exclusive))
       (make-record keyword
		    min-occur max-occur
		    min-args max-args
		    mutually-inclusive mutually-exclusive)))))

(define syntax-clauses-single-spec
  (case-lambda
   ((spec clauses)
    (syntax-clauses-single-spec spec clauses (%make-synner 'syntax-clauses-single-spec)))
   ((spec clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify that the clauses conform to the given clause specification.
    ;;If successful  return a  (possibly empty)  list of  syntax objects
    ;;representing  the cdrs  of the  clauses matching  SPEC; else  call
    ;;SYNNER.
    ;;
    (assert (syntax-clause-spec? spec))
    (assert (procedure? synner))
    (let* ((keyword (syntax-clause-spec-keyword spec))
	   (present (syntax-clauses-filter (list keyword) clauses))
	   (number  (length present)))
      (unless (<= (syntax-clause-spec-min-number-of-occurrences spec)
		  number
		  (syntax-clause-spec-max-number-of-occurrences spec))
	(synner (string-append "clause must be present at least "
			       (number->string (syntax-clause-spec-min-number-of-occurrences spec))
			       " times and at most "
			       (number->string (syntax-clause-spec-max-number-of-occurrences spec))
			       " times")
		(if (< 1 number)
		    present
		  keyword)))
      ;;Validate arguments
      (for-each (lambda (clause)
		  (define number
		    (length (cdr clause)))
		  (unless (<= (syntax-clause-spec-min-number-of-arguments spec)
			      number
			      (syntax-clause-spec-max-number-of-arguments spec))
		    (synner (string-append "clause must have at least "
					   (number->string (syntax-clause-spec-min-number-of-arguments spec))
					   " arguments and at most "
					   (number->string (syntax-clause-spec-max-number-of-arguments spec))
					   " arguments")
			    clause)))
	present)
      ;;Validate mutually inclusive.
      (unless (null? present)
	(for-each (lambda (id)
		    (unless (exists (lambda (clause)
				      (free-identifier=? id (car clause)))
			      clauses)
		      (synner "missing mutually inclusive clause" (list keyword id))))
	  (syntax-clause-spec-mutually-inclusive spec)))
      ;;Validate mutually exclusive.
      (unless (null? present)
	(let ((others (syntax-clauses-filter (syntax-clause-spec-mutually-exclusive spec) clauses)))
	  (unless (null? others)
	    (synner "mutually exclusive clauses are present" (append present others)))))
      ;;Return the list of arguments.
      (map cdr present)))
   ))

(define syntax-clauses-fold-specs
  (case-lambda
   ((combine knil specs clauses)
    (syntax-clauses-fold-specs combine knil specs clauses (%make-synner 'syntax-clauses-fold-specs)))
   ((combine knil specs clauses synner)
    ;;Given a  fully unwrapped syntax  object holding a list  of clauses
    ;;with the format:
    ;;
    ;;    ((?identifier ?thing ...) ...)
    ;;
    ;;verify   that   the   clauses   conform  to   the   given   clause
    ;;specifications.  Combine the clause  arguments with the given KNIL
    ;;in a FOLD-LEFT fashion.  If  successful return the resulting KNIL;
    ;;else call SYNNER.
    ;;
    (assert (procedure? combine))
    (assert (for-all syntax-clause-spec? specs))
    (assert (procedure? synner))
    (fold-left (lambda (knil spec)
		 (combine knil spec (syntax-clauses-single-spec spec clauses synner)))
      knil
      specs))
   ))


;;;; done

)

;;; end of file
