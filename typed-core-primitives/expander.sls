;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for expander core primitives
;;Date: Tue Dec 26, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;
;;This program is free  software: you can redistribute it and/or  modify it under the
;;terms  of  the  GNU General  Public  License  as  published  by the  Free  Software
;;Foundation, either version 3 of the License, or (at your option) any later version.
;;
;;This program  is distributed in the  hope that it  will be useful, but  WITHOUT ANY
;;WARRANTY; without  even the implied  warranty of  MERCHANTABILITY or FITNESS  FOR A
;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;;You should have received  a copy of the GNU General Public  License along with this
;;program.  If not, see <http://www.gnu.org/licenses/>.
;;

#!vicare
(library (typed-core-primitives expander)
  (export typed-core-primitives.expander)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.expander)


;;;; syntax-case, safe procedures

(section

(declare-type-predicate syntax-object?	<syntax-object>)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<stx>)			=> (<top>)))
		   (attributes
		    ((_)			effect-free))))
		)))
  (declare stx-expr)
  (declare stx-mark*)
  (declare stx-rib*)
  (declare stx-annotated-expr*)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-type-predicate identifier?	<syntactic-identifier>)

(declare-core-primitive identifier-bound?
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<boolean>)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive bound-identifier=?
    (safe)
  (signatures
   ((<syntactic-identifier> <syntactic-identifier>)	=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive free-identifier=?
    (safe)
  (signatures
   ((<syntactic-identifier> <syntactic-identifier>)	=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive generate-temporaries
    (safe)
  (signatures
   ((<top>)		=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive datum->syntax
    (safe)
  (signatures
   ((<syntactic-identifier> <top>)	=> (<stx>)))
  (attributes
   ((_ _)				effect-free result-true)))

(declare-core-primitive syntax->datum
    (safe)
  (signatures
   ((<top>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive make-variable-transformer
    (safe)
  (signatures
   ((<procedure>)	=> (<top>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate variable-transformer?)

(declare-core-primitive variable-transformer-procedure
    (safe)
  (signatures
   ((<top>)		=> (<procedure>)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive make-synonym-transformer
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<top>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate synonym-transformer?)

(declare-core-primitive synonym-transformer-identifier
    (safe)
  (signatures
   ((<top>)		=> (<syntactic-identifier>)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive make-expand-time-value
    (safe)
  (signatures
   ((<top>)		=> (<top>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-object-predicate expand-time-value?)

(declare-core-primitive expand-time-value-object
    (safe)
  (signatures
   ((<top>)		=> (<top>)))
  (attributes
   ((_)			effect-free)))

(declare-core-primitive retrieve-expand-time-value
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<top>)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive syntax-parameter-value
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<top>)))
  (attributes
   ((_)			effect-free)))

;;;

(declare-core-primitive syntactic-binding-putprop
    (safe)
  (signatures
   ((<syntactic-identifier> <symbol> <top>)	=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive syntactic-binding-getprop
    (safe)
  (signatures
   ((<syntactic-identifier> <symbol>)		=> (<top>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive syntactic-binding-remprop
    (safe)
  (signatures
   ((<syntactic-identifier> <symbol>)		=> (<void>)))
  (attributes
   ((_ _)		result-true)))

(declare-core-primitive syntactic-binding-property-list
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; syntax utilities

(declare-core-primitive identifier->string
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<string>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive string->identifier
    (safe)
  (signatures
   ((<syntactic-identifier> <string>)	=> (<syntactic-identifier>)))
  (attributes
   ((_ _)		effect-free result-true)))

;;;

(declare-core-primitive identifier-prefix
    (safe)
  (signatures
   ((<string> <syntactic-identifier>)		=> (<syntactic-identifier>))
   ((<symbol> <syntactic-identifier>)		=> (<syntactic-identifier>))
   ((<syntactic-identifier> <syntactic-identifier>)		=> (<syntactic-identifier>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive identifier-suffix
    (safe)
  (signatures
   ((<syntactic-identifier> <string>)		=> (<syntactic-identifier>))
   ((<syntactic-identifier> <symbol>)		=> (<syntactic-identifier>))
   ((<syntactic-identifier> <syntactic-identifier>)		=> (<syntactic-identifier>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive identifier-append
    (safe)
  (signatures
   #;((<syntactic-identifier> . [or <string> <symbol> <syntactic-identifier>])	=> (<syntactic-identifier>))
   ((<syntactic-identifier> . <list>)		=> (<syntactic-identifier>)))
  (attributes
   ((_ . _)		effect-free result-true)))

(declare-core-primitive identifier-format
    (safe)
  (signatures
   #;((<syntactic-identifier> <string> . [or <string> <symbol> <syntactic-identifier>])	=> (<syntactic-identifier>))
   ((<syntactic-identifier> <string> . <list>)	=> (<syntactic-identifier>)))
  (attributes
   ((_ _ . _)		effect-free result-true)))

;;;

(declare-core-primitive duplicate-identifiers?
    (safe)
  (signatures
   ;; ((<list>)			=> ([or <false> <syntactic-identifier>]))
   ;; ((<list> <procedure>)	=> ([or <false> <syntactic-identifier>]))
   ((<list>)			=> (<top>))
   ((<list> <procedure>)	=> (<top>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive delete-duplicate-identifiers
    (safe)
  (signatures
   ((<list>)			=> (<list>))
   ((<list> <procedure>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

;;;

(declare-core-primitive identifier-memq
    (safe)
  (signatures
   ;; ((<syntactic-identifier> <list>)		=> ([or <false> <list>]))
   ;; ((<syntactic-identifier> <list> <procedure>)	=> ([or <false> <list>]))
   ((<syntactic-identifier> <list>)		=> (<top>))
   ((<syntactic-identifier> <list> <procedure>)	=> (<top>)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

;;;

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<syntactic-identifier>)	=> (<syntactic-identifier>)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare identifier-record-constructor)
  (declare identifier-record-predicate)
  (declare identifier-struct-constructor)
  (declare identifier-struct-predicate)
  #| end of LET-SYNTAX |# )

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<syntactic-identifier> <string>)		=> (<syntactic-identifier>))
		    ((<syntactic-identifier> <symbol>)		=> (<syntactic-identifier>))
		    ((<syntactic-identifier> <syntactic-identifier>)	=> (<syntactic-identifier>)))
		   (attributes
		    ((_)		effect-free result-true))))
		)))
  (declare identifier-record-field-accessor)
  (declare identifier-record-field-mutator)
  (declare identifier-struct-field-accessor)
  (declare identifier-struct-field-mutator)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(declare-core-primitive syntax-car
    (safe)
  (signatures
   ((<syntax-object>)			=> (<syntax-object>))
   ((<syntax-object> <procedure>)	=> (<syntax-object>)))
  (attributes
   ((_)				effect-free)
   ((_ _)			effect-free)))

(declare-core-primitive syntax-cdr
    (safe)
  (signatures
   ((<syntax-object>)			=> (<syntax-object>))
   ((<syntax-object> <procedure>)	=> (<syntax-object>)))
  (attributes
   ((_)				effect-free)
   ((_ _)			effect-free)))

(declare-core-primitive syntax->list
    (safe)
  (signatures
   ((<syntax-object>)			=> (<list>))
   ((<syntax-object> <procedure>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax->vector
    (safe)
  (signatures
   ((<syntax-object>)			=> (<vector>))
   ((<syntax-object> <procedure>)	=> (<vector>)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive identifiers->list
    (safe)
  (signatures
   ((<syntax-object>)			=> (<list>))
   ((<syntax-object> <procedure>)	=> (<list>)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

(declare-core-primitive syntax-unwrap
    (safe)
  (signatures
   ((<syntax-object>)		=> (<top>)))
  (attributes
   ((_)				effect-free)))

;;;

(declare-core-primitive all-identifiers?
    (safe)
  (signatures
   ((<stx>)			=> (<boolean>))
   ((<list>)			=> (<boolean>)))
  (attributes
   ((_)				effect-free)))

(declare-core-primitive syntax=?
    (safe)
  (signatures
   ((<syntax-object> <syntax-object>)	=> (<boolean>)))
  (attributes
   ((_ _)			effect-free)))

(declare-core-primitive identifier=symbol?
    (safe)
  (signatures
   ((<syntactic-identifier> <symbol>)	=> (<boolean>)))
  (attributes
   ((_ _)		effect-free)))

;;; --------------------------------------------------------------------

(declare-core-primitive syntax-clauses-unwrap
    (safe)
  (signatures
   ((<syntax-object>)			=> (<list>))
   ((<syntax-object> <procedure>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-filter
    (safe)
  (signatures
   ((<list> <syntax-object>)		=> (<list>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-remove
    (safe)
  (signatures
   ((<list> <syntax-object>)		=> (<list>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-partition
    (safe)
  (signatures
   ((<list> <syntax-object>)		=> (<list> <list>)))
  (attributes
   ((_ _)		effect-free)))

(declare-core-primitive syntax-clauses-collapse
    (safe)
  (signatures
   ((<list>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))

;;;

(declare-core-primitive syntax-clauses-verify-at-least-once
    (safe)
  (signatures
   ((<list> <list>)		=> (<void>))
   ((<list> <list> <procedure>)	=> (<void>)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-at-most-once
    (safe)
  (signatures
   ((<list> <list>)		=> (<void>))
   ((<list> <list> <procedure>)	=> (<void>)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-exactly-once
    (safe)
  (signatures
   ((<list> <list>)		=> (<void>))
   ((<list> <list> <procedure>)	=> (<void>)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-mutually-inclusive
    (safe)
  (signatures
   ((<list> <list>)		=> (<void>))
   ((<list> <list> <procedure>)	=> (<void>)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

(declare-core-primitive syntax-clauses-verify-mutually-exclusive
    (safe)
  (signatures
   ((<list> <list>)		=> (<void>))
   ((<list> <list> <procedure>)	=> (<void>)))
  (attributes
   ;;Not  effect-free because  it  validates the  input and  raises  an exception  on
   ;;failure.
   ((_ _)		result-true)
   ((_ _ _)		result-true)))

;;; --------------------------------------------------------------------
;;; clause specification structs

(declare-core-primitive make-syntax-clause-spec
    (safe)
  (signatures
   ((<syntactic-identifier> <real> <real> <real> <real> <list> <list>)		=> (<syntax-clause-spec>))
   ((<syntactic-identifier> <real> <real> <real> <real> <list> <list> <top>)	=> (<syntax-clause-spec>)))
  (attributes
   ((_ _ _ _ _ _ _)			effect-free result-true)
   ((_ _ _ _ _ _ _ _)			effect-free result-true)))

(declare-type-predicate syntax-clause-spec?	<syntax-clause-spec>)

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<syntax-clause-spec>)	=> (?return-value-tag)))
		   (attributes
		    ((_)			effect-free))))
		)))
  (declare syntax-clause-spec-keyword			<syntactic-identifier>)
  (declare syntax-clause-spec-min-number-of-occurrences	<real>)
  (declare syntax-clause-spec-max-number-of-occurrences	<real>)
  (declare syntax-clause-spec-min-number-of-arguments	<real>)
  (declare syntax-clause-spec-max-number-of-arguments	<real>)
  (declare syntax-clause-spec-mutually-inclusive	<list>)
  (declare syntax-clause-spec-mutually-exclusive	<list>)
  (declare syntax-clause-spec-custom-data		<top>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive syntax-clauses-single-spec
    (safe)
  (signatures
   ((<syntax-clause-spec> <list>)			=> (<vector>))
   ((<syntax-clause-spec> <list> <procedure>)		=> (<vector>)))
  (attributes
   ((_ _)		effect-free result-true)
   ((_ _ _)		effect-free result-true)))

(declare-core-primitive syntax-clauses-fold-specs
    (safe)
  (signatures
   ((<procedure> <top> <list> <list>)			=> (<top>))
   ((<procedure> <top> <list> <list> <procedure>)	=> (<top>)))
  (attributes
   ((_ _ _)		effect-free)
   ((_ _ _ _)		effect-free)))

(declare-core-primitive syntax-clauses-validate-specs
    (safe)
  (signatures
   ((<list>)	=> (<list>)))
  (attributes
   ((_)			effect-free result-true)))



/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
