;; -*- coding: utf-8-unix -*-
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

  (define-object-binary/multi-comparison-declarer declare-type-signature-binary/multi-comparison <type-signature>)


;;;; configuration

(declare-core-primitive initialise-expander
    (safe)
  (signatures
   (()				=> (<void>))))

(declare-parameter generate-descriptive-gensyms?)
(declare-parameter generate-descriptive-marks?)

(declare-core-primitive typed-language-enabled?
    (safe)
  (signatures
   (()				=> (<boolean>))
   ((<top>)			=> (<boolean>))))

(declare-core-primitive strict-r6rs-enabled?
    (safe)
  (signatures
   (()				=> (<boolean>))
   ((<top>)			=> (<boolean>))))


;;;; main operations

(declare-core-primitive expand-form-to-core-language
    (safe)
  (signatures
   ((<top> <lexical-environment>)		=> (<top> (list-of <library>)))))

(declare-parameter current-inferior-lexenv)


;;;; record types

(declare-core-rtd <lexical-environment>-rtd)
(declare-core-rcd <lexical-environment>-rcd)

(declare-core-rtd <non-interaction-lexical-environment>-rtd)
(declare-core-rcd <non-interaction-lexical-environment>-rcd)

(declare-core-rtd <interaction-lexical-environment>-rtd)
(declare-core-rcd <interaction-lexical-environment>-rcd)

(declare-core-rtd <stx>-rtd)
(declare-core-rcd <stx>-rcd)

(declare-core-rtd <syntactic-identifier>-rtd)
(declare-core-rcd <syntactic-identifier>-rcd)

(declare-core-rtd <syntax-clause-spec>-rtd)
(declare-core-rcd <syntax-clause-spec>-rcd)

(declare-core-rtd <type-signature>-rtd)
(declare-core-rcd <type-signature>-rcd)


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

(declare-type-predicate syntactic-identifier?		<syntactic-identifier>)

(declare-core-primitive identifier?
    (safe)
  (signatures
   ((<syntactic-identifier>)				=> (<boolean>))
   #;(((ancestor-of <syntactic-identifier>))		=> (<boolean>))
   (((or <stx> <record>	<struct> <top>))		=> (<boolean>))
   ;; (((and (not (ancestor-of <syntactic-identifier>))
   ;;        (not <syntactic-identifier>)))                => (<boolean>))
   (((and (not <top>)
	  (not <record>)
	  (not <struct>)
	  (not <stx>)
	  (not <syntactic-identifier>)))		=> (<false>))))

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

/section)


;;;; special macro transformers

(section

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

(declare-core-primitive syntax-parameter-value
    (safe)
  (signatures
   ((<syntactic-identifier>)	=> (<top>)))
  (attributes
   ((_)			effect-free)))

/section)


;;;; syntactic bindings, utilities

(section

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

/section)


;;;; syntactic identifiers, utilities

(section

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
   ((<syntactic-identifier> <string> . (list-of (or <string> <symbol> <syntactic-identifier>)))
    => (<syntactic-identifier>)))
  (attributes
   ((_ _ . _)		effect-free result-true)))

;;;

(declare-core-primitive duplicate-identifiers?
    (safe)
  (signatures
   ((<list>)			=> ([or <false> <syntactic-identifier>]))
   ((<list> <procedure>)	=> ([or <false> <syntactic-identifier>])))
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
   ((<syntactic-identifier> <list>)		=> ([or <false> <list>]))
   ((<syntactic-identifier> <list> <procedure>)	=> ([or <false> <list>])))
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
  (declare identifier-method-procname)
  #| end of LET-SYNTAX |# )

/section)


;;;; syntax utilities

(section

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

(declare-core-primitive syntax-replace-id
    (safe)
  (signatures
   ((<syntax-object> <syntactic-identifier> <syntactic-identifier>)		=> (<top>)))
  (attributes
   ((_ _ _)			effect-free)))

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

/section)


;;; syntax clauses specification structs

(section

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


;;; syntax clauses operations

(section

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

/section)


;;;; syntax objects utilities, safe procedures

(section

(declare-core-primitive parse-logic-predicate-syntax
    (safe)
  (signatures
   ((<syntax-object>)			=> (<syntax-object>))
   ((<syntax-object> <procedure>)	=> (<syntax-object>))))

/section)


;;;; object type specifications

(section

(declare-core-rtd <object-type-spec>-rtd)
(declare-core-rcd <object-type-spec>-rcd)

(declare-type-predicate object-type-spec?		<object-type-spec>)

(declare-core-primitive make-type-annotation
    (safe)
  (signatures
   ((<syntax-object>)	=> (<object-type-spec>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-rtd <core-type-spec>-rtd)
(declare-core-rcd <core-type-spec>-rcd)

(declare-type-predicate core-type-spec?		<core-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <struct-type-spec>-rtd)
(declare-core-rcd <struct-type-spec>-rcd)

(declare-core-primitive make-struct-type-spec
    (safe)
  (signatures
   ((<syntactic-identifier> <struct-type-descriptor> <syntactic-identifier> <syntactic-identifier> <list>)
    => (<struct-type-spec>))))

(declare-type-predicate struct-type-spec?		<struct-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <record-type-spec>-rtd)
(declare-core-rcd <record-type-spec>-rcd)

(declare-core-primitive make-record-type-spec
    (safe)
  (signatures
   ((<syntactic-identifier>		     ;type-name
     <symbol>				     ;uid
     <syntactic-identifier>		     ;rcd-id
     <syntactic-identifier>		     ;rtd-id
     (or <false> <syntactic-identifier>)     ;super-protocol-id
     (or <false> <syntactic-identifier>)     ;parent-name.id
     (or <false> <syntactic-identifier>)     ;constructor.stx
     (or <false> <syntactic-identifier>)     ;destructor.stx
     <syntactic-identifier>		     ;predicate.stx
     (or <false> <syntactic-identifier>)     ;equality-predicate.id
     (or <false> <syntactic-identifier>)     ;comparison-procedure.id
     (or <false> <syntactic-identifier>)     ;hash-function.id
     (alist <symbol> <syntactic-identifier>) ;methods-table
     (list-of <syntactic-identifier>))	     ;implemented-interfaces
    => (<record-type-spec>))))

(declare-type-predicate record-type-spec?		<record-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <compound-condition-type-spec>-rtd)
(declare-core-rcd <compound-condition-type-spec>-rcd)

(declare-type-predicate compound-condition-type-spec?	<compound-condition-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <hashtable-type-spec>-rtd)
(declare-core-rcd <hashtable-type-spec>-rcd)

(declare-type-predicate hashtable-type-spec?	<hashtable-type-spec>)

;;; --------------------------------------------------------------------
;;; <enumeration-type-spec>

(declare-core-rtd <enumeration-type-spec>-rtd)
(declare-core-rcd <enumeration-type-spec>-rcd)

(declare-core-primitive make-enumeration-type-spec
    (safe)
  (signatures
   ((<enum-set>)			=> (<enumeration-type-spec>))
   ((<enum-set> <syntax-object>)	=> (<enumeration-type-spec>))))

(declare-type-predicate  enumeration-type-spec?	<enumeration-type-spec>)

(declare-core-primitive enumeration-type-spec.symbol*
    (safe)
  (signatures
   ((<enumeration-type-spec>)	=> ((list-of <symbol>)))))

(declare-core-primitive enumeration-type-spec.member?
    (safe)
  (signatures
   ((<enumeration-type-spec> <symbol>)	=> (<boolean>))))

;;; --------------------------------------------------------------------

(declare-core-rtd <pair-type-spec>-rtd)
(declare-core-rcd <pair-type-spec>-rcd)

(declare-type-predicate pair-type-spec?			<pair-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <pair-of-type-spec>-rtd)
(declare-core-rcd <pair-of-type-spec>-rcd)

(declare-type-predicate pair-of-type-spec?		<pair-of-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <list-type-spec>-rtd)
(declare-core-rcd <list-type-spec>-rcd)

(declare-type-predicate list-type-spec?			<list-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <list-of-type-spec>-rtd)
(declare-core-rcd <list-of-type-spec>-rcd)

(declare-type-predicate list-of-type-spec?		<list-of-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <vector-of-type-spec>-rtd)
(declare-core-rcd <vector-of-type-spec>-rcd)

(declare-type-predicate vector-of-type-spec?		<vector-of-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <vector-type-spec>-rtd)
(declare-core-rcd <vector-type-spec>-rcd)

(declare-type-predicate vector-type-spec?		<vector-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <union-type-spec>-rtd)
(declare-core-rcd <union-type-spec>-rcd)

(declare-type-predicate union-type-spec?		<union-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <intersection-type-spec>-rtd)
(declare-core-rcd <intersection-type-spec>-rcd)

(declare-type-predicate intersection-type-spec?		<intersection-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <complement-type-spec>-rtd)
(declare-core-rcd <complement-type-spec>-rcd)

(declare-type-predicate complement-type-spec?		<complement-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <ancestor-of-type-spec>-rtd)
(declare-core-rcd <ancestor-of-type-spec>-rcd)

(declare-type-predicate ancestor-of-type-spec?	<ancestor-of-type-spec>)

;;; --------------------------------------------------------------------
;;; <alist-type-spec>

(declare-core-rtd <alist-type-spec>-rtd)
(declare-core-rcd <alist-type-spec>-rcd)

(declare-core-primitive make-alist-type-spec
    (safe)
  (signatures
   ((<object-type-spec> <object-type-spec>)		=> (<alist-type-spec>))))

(declare-type-predicate alist-type-spec?		<alist-type-spec>)

(declare-core-primitive alist-type-spec.key-ots
    (safe)
  (signatures
   ((<alist-type-spec>)		=> (<alist-type-spec>))))

(declare-core-primitive alist-type-spec.val-ots
    (safe)
  (signatures
   ((<alist-type-spec>)		=> (<alist-type-spec>))))

;;; --------------------------------------------------------------------
;;; <label-type-spec>

(declare-core-rtd <label-type-spec>-rtd)
(declare-core-rcd <label-type-spec>-rcd)

(declare-core-primitive make-label-type-spec
    (safe)
  (signatures
   ((<syntactic-identifier>		      ;label-name-id
     <symbol>				      ;uid
     <syntax-object>			      ;parent-id
     (or <false> <syntactic-identifier>)      ;constructor
     (or <false> <syntactic-identifier>)      ;destructor
     (or <false> <syntactic-identifier>)      ;type-predicate
     (or <false> <syntactic-identifier>)      ;equality-predicate
     (or <false> <syntactic-identifier>)      ;comparison-procedure
     (or <false> <syntactic-identifier>)      ;hash-function
     (alist <symbol> <syntactic-identifier>)) ;methods-table
    => (<label-type-spec>))))

(declare-type-predicate label-type-spec?	<label-type-spec>)

;;; --------------------------------------------------------------------

(declare-core-rtd <interface-type-spec>-rtd)
(declare-core-rcd <interface-type-spec>-rcd)

(declare-core-primitive make-interface-type-spec
    (safe)
  (signatures
   ((<syntactic-identifier>		     ;type-name
     <symbol>				     ;uid
     <syntactic-identifier>		     ;type-descriptor-name
     (alist <symbol> <closure-type-spec>)    ;requested methods table
     (alist <symbol> <syntactic-identifier>) ;implemented methods table
     (list-of <syntactic-identifier>))	     ;implemented-interfaces
    => (<interface-type-spec>))))

(declare-type-predicate interface-type-spec?	<interface-type-spec>)

(declare-core-primitive interface-type-spec.type-descriptor-id
    (safe)
  (signatures
   ((<interface-type-spec>)		=> (<syntactic-identifier>))))

(declare-core-primitive interface-type-spec.method-prototypes-table
    (safe)
  (signatures
   ((<interface-type-spec>)		=> ((alist <symbol> <closure-type-spec>)))))

(declare-core-primitive build-table-for-interface-and-compliant-object-type
    (safe)
  (signatures
   ((<syntactic-identifier> <syntactic-identifier>)	=> ((alist <symbol> <syntactic-identifier>)))))

;;; --------------------------------------------------------------------
;;; operations on type specs

  (declare-core-primitive union-of-type-specs
      (safe)
    (signatures
     ((list-of <object-type-spec>)	=> (<object-type-spec>))))

  /section)


;;;; type signatures

(declare-type-predicate		type-signature?			<type-signature>)
(declare-list-of-type-predicate	list-of-type-signatures?	<type-signature>)

(declare-core-primitive make-type-signature
    (safe)
  (signatures
   (<list>			=> (<type-signature>))))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive type-signature.syntax-object
    (safe)
  (signatures
   ((<type-signature>)		=> (<syntax-object>))))

(declare-core-primitive type-signature.object-type-specs
    (safe)
  (signatures
   ((<type-signature>)		=> (<top>))))

(declare-core-primitive type-signature.min-count
    (safe)
  (signatures
   ((<type-signature>)		=> (<non-negative-fixnum>))))

(declare-core-primitive type-signature.max-count
    (safe)
  (signatures
   ((<type-signature>)		=> (<non-negative-fixnum>))))

(declare-core-primitive type-signature.min-and-max-counts
    (safe)
  (signatures
   ((<type-signature>)		=> (<non-negative-fixnum> <non-negative-fixnum>))))

;;; --------------------------------------------------------------------
;;; predicates

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<type-signature>)	=> (<boolean>)))))
		)))
  (declare type-signature.fully-untyped?)
  (declare type-signature.partially-untyped?)
  (declare type-signature.untyped?)
  (declare type-signature.empty?)
  (declare type-signature.single-type?)
  (declare type-signature.single-top-tag?)
  (declare type-signature.single-type-or-fully-untyped?)
  (declare type-signature.no-return?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; comparison

(declare-type-signature-binary/multi-comparison type-signature=?)

(declare-core-primitive type-signature.super-and-sub?
    (safe)
  (signatures
   ((<type-signature> <type-signature>)		=> (<boolean>))))

(declare-core-primitive type-signature.compatible-super-and-sub?
    (safe)
  (signatures
   ((<type-signature> <type-signature>)		=> (<boolean>))))

;;; --------------------------------------------------------------------
;;; operations

(declare-core-primitive type-signature.match-formals-against-operands
    (safe)
  (signatures
   ((<type-signature> <type-signature>)		=> (<symbol>))))

(declare-core-primitive type-signature.common-ancestor
    (safe)
  (signatures
   ((list-of <type-signature>)		=> (<type-signature>))))

(declare-core-primitive type-signature.union
    (safe)
  (signatures
   ((list-of <type-signature>)		=> (<type-signature>))))


;;;; condition object types

(declare-core-rtd &syntactic-identifier-rtd)
(declare-core-rcd &syntactic-identifier-rcd)
;;make-syntactic-identifier-condition
(declare-condition-type-predicate syntactic-identifier-condition?	&syntactic-identifier)
;;condition-syntactic-identifier

(declare-core-rtd &syntactic-binding-descriptor-rtd)
(declare-core-rcd &syntactic-binding-descriptor-rcd)
;;make-syntactic-binding-descriptor-condition
(declare-condition-type-predicate syntactic-binding-descriptor-condition?	&syntactic-binding-descriptor)
;;condition-syntactic-binding-descriptor

(declare-core-rtd &object-type-spec-rtd)
(declare-core-rcd &object-type-spec-rcd)
;;make-object-type-spec-condition
(declare-condition-type-predicate object-type-spec-condition?			&object-type-spec)
;;condition-object-type-spec

(declare-core-rtd &syntactic-identifier-resolution-rtd)
(declare-core-rcd &syntactic-identifier-resolution-rcd)
;;make-syntactic-identifier-resolution-violation
(declare-condition-type-predicate syntactic-identifier-resolution-violation?	&syntactic-identifier-resolution)

(declare-core-rtd &syntactic-identifier-unbound-rtd)
(declare-core-rcd &syntactic-identifier-unbound-rcd)
;;make-syntactic-identifier-unbound-condition
(declare-condition-type-predicate syntactic-identifier-unbound-condition?	&syntactic-identifier-unbound)

(declare-core-rtd &syntactic-identifier-out-of-context-rtd)
(declare-core-rcd &syntactic-identifier-out-of-context-rcd)
;;make-syntactic-identifier-out-of-context-condition
(declare-condition-type-predicate syntactic-identifier-out-of-context-condition?	&syntactic-identifier-out-of-context)

(declare-core-rtd &syntactic-identifier-not-type-identifier-rtd)
(declare-core-rcd &syntactic-identifier-not-type-identifier-rcd)
;;make-syntactic-identifier-not-type-identifier-condition
(declare-condition-type-predicate syntactic-identifier-not-type-identifier-condition?	&syntactic-identifier-not-type-identifier)

(declare-core-rtd &syntax-definition-expanded-rhs-rtd)
(declare-core-rcd &syntax-definition-expanded-rhs-rcd)
;;make-syntax-definition-expanded-rhs-condition
(declare-condition-type-predicate syntax-definition-expanded-rhs-condition?	&syntax-definition-expanded-rhs)
;;condition-syntax-definition-expanded-rhs

(declare-core-rtd &syntax-definition-expression-return-value-rtd)
(declare-core-rcd &syntax-definition-expression-return-value-rcd)
;;make-syntax-definition-expression-return-value-condition
(declare-condition-type-predicate syntax-definition-expression-return-value-condition?	&syntax-definition-expression-return-value)
;;condition-syntax-definition-expression-return-value

(declare-core-rtd &macro-expansion-trace-rtd)
(declare-core-rcd &macro-expansion-trace-rcd)
;;make-macro-expansion-trace
(declare-condition-type-predicate macro-expansion-trace?	&macro-expansion-trace)
;;macro-expansion-trace-form

(declare-core-rtd &type-signature-rtd)
(declare-core-rcd &type-signature-rcd)
;;make-type-signature-condition
(declare-condition-type-predicate type-signature-condition?	&type-signature)
;;condition-type-signature

;;; --------------------------------------------------------------------

(declare-core-rtd &application-argument-type-name-rtd)
(declare-core-rcd &application-argument-type-name-rcd)
;;make-application-argument-type-name-condition
(declare-condition-type-predicate application-argument-type-name-condition?	&application-argument-type-name)
;;condition-application-argument-type-name

(declare-core-rtd &application-argument-index-rtd)
(declare-core-rcd &application-argument-index-rcd)
;;make-application-argument-index-condition
(declare-condition-type-predicate application-argument-index-condition?	&application-argument-index)
;;condition-application-argument-index

(declare-core-rtd &application-operator-expression-rtd)
(declare-core-rcd &application-operator-expression-rcd)
;;make-application-operator-expression-condition
(declare-condition-type-predicate application-operator-expression-condition?	&application-operator-expression)
;;condition-application-operator-expression

(declare-core-rtd &application-operands-expressions-rtd)
(declare-core-rcd &application-operands-expressions-rcd)
;;make-application-operands-expressions-condition
(declare-condition-type-predicate application-operands-expressions-condition?	&application-operands-expressions)
;;condition-application-operands-expressions

(declare-core-rtd &application-operator-signature-rtd)
(declare-core-rcd &application-operator-signature-rcd)
;;make-application-operator-signature-condition
(declare-condition-type-predicate application-operator-signature-condition?	&application-operator-signature)
;;condition-application-operator-signature

(declare-core-rtd &application-operand-signature-rtd)
(declare-core-rcd &application-operand-signature-rcd)
;;make-application-operand-signature-condition
(declare-condition-type-predicate application-operand-signature-condition?	&application-operand-signature)
;;condition-application-operand-signature

;;; --------------------------------------------------------------------

(declare-core-rtd &expected-type-signature-rtd)
(declare-core-rcd &expected-type-signature-rcd)
;;make-expected-type-signature-condition
(declare-condition-type-predicate expected-type-signature-condition?	&expected-type-signature)

(declare-core-primitive condition-expected-type-signature
    (safe)
  (signatures
   ((&expected-type-signature)		=> (<type-signature>))))

;;;

(declare-core-rtd &returned-type-signature-rtd)
(declare-core-rcd &returned-type-signature-rcd)
;;make-returned-type-signature-condition
(declare-condition-type-predicate returned-type-signature-condition?	&returned-type-signature)

(declare-core-primitive condition-returned-type-signature
    (safe)
  (signatures
   ((&returned-type-signature)		=> (<type-signature>))))

;;; --------------------------------------------------------------------

(declare-core-rtd &type-method-name-rtd)
(declare-core-rcd &type-method-name-rcd)
;;make-type-method-name-condition
(declare-condition-type-predicate condition-type-method-name?	&type-method-name)
;;condition-type-method-name

;;; --------------------------------------------------------------------

(declare-core-rtd &expand-time-type-signature-violation-rtd)
(declare-core-rcd &expand-time-type-signature-violation-rcd)
;;make-expand-time-type-signature-violation
(declare-condition-type-predicate expand-time-type-signature-violation?	&expand-time-type-signature-violation)

(declare-core-rtd &expand-time-type-signature-warning-rtd)
(declare-core-rcd &expand-time-type-signature-warning-rcd)
;;make-expand-time-type-signature-warning
(declare-condition-type-predicate expand-time-type-signature-warning?	&expand-time-type-signature-warning)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
