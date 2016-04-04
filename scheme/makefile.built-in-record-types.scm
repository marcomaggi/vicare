;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of built-in record types and condition object types
;;Date: Tue Dec 22, 2015
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


;;;; syntaxes

(define-syntax (define-built-in-record-type stx)
  (define (%false-or-id? obj)
    (or (identifier? obj)
	(not (syntax->datum obj))))
  (syntax-case stx (methods)
    ((?kwd ?type-name ?parent-name ?constructor ?predicate)
     (and (identifier? #'?type-name)
	  (%false-or-id? #'?parent-name)
	  (%false-or-id? #'?constructor)
	  (identifier? #'?predicate))
     #'(?kwd ?type-name ?parent-name ?constructor ?predicate (methods)))

    ((_    ?type-name ?parent-name ?constructor ?predicate (methods (?field-name ?accessor-name) ...))
     (and (identifier? #'?type-name)
	  (%false-or-id? #'?parent-name)
	  (%false-or-id? #'?constructor)
	  (identifier? #'?predicate))
     (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
       (define (mkid . str*)
	 (datum->syntax #'?type-name (string->symbol (apply string-append str*))))
       (with-syntax
	   ((TYPE-RTD (mkid type-name.str "-rtd"))
	    (TYPE-RCD (mkid type-name.str "-rcd")))
	 #'(set-cons! VICARE-CORE-BUILT-IN-RECORD-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
		      (quote (?type-name
			      ($core-record-type-name
			       . (?type-name TYPE-RTD TYPE-RCD ?parent-name ?constructor ?predicate
					     ((?field-name . ?accessor-name) ...)))))))))
    ))


;;;; built-in record types

(define-built-in-record-type <library>
    <record>
  make-library library?
  (methods
   (uid				library-uid)
   (name			library-name)
   (imp-lib*			library-imp-lib*)
   (vis-lib*			library-vis-lib*)
   (inv-lib*			library-inv-lib*)
   (export-subst		library-export-subst)
   (global-env			library-global-env)
   (typed-locs			library-typed-locs)
   (visit-state			library-visit-state)
   (invoke-state		library-invoke-state)
   (visit-code			library-visit-code)
   (invoke-code			library-invoke-code)
   (guard-code			library-guard-code)
   (guard-lib*			library-guard-lib*)
   (visible?			library-visible?)
   (source-file-name		library-source-file-name)
   (option*			library-option*)
   (foreign-library*		library-foreign-library*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <reader-annotation>
    <record>
  get-annotated-datum reader-annotation?
  (methods
   (expression			reader-annotation-expression)
   (stripped			reader-annotation-stripped)
   (source			reader-annotation-source)
   (textual-position		reader-annotation-textual-position)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <lexical-environment>
    <record>
  #f environment?)

(define-built-in-record-type <interaction-lexical-environment>
    <lexical-environment>
  new-interaction-environment interaction-lexical-environment?)

(define-built-in-record-type <non-interaction-lexical-environment>
    <lexical-environment>
  environment non-interaction-lexical-environment?)

;;; --------------------------------------------------------------------

(define-built-in-record-type <object-type-spec>
    <record>
  #f object-type-spec?
  (methods
   (name				object-type-spec.name)
   (parent-ots			object-type-spec.parent-ots)
   (constructor-stx			object-type-spec.constructor-stx)
   (destructor-stx			object-type-spec.destructor-stx)
   (type-predicate-stx		object-type-spec.type-predicate-stx)
   (safe-accessor-stx		object-type-spec.safe-accessor-stx)
   (safe-mutator-stx			object-type-spec.safe-mutator-stx)
   (applicable-method-stx		object-type-spec.applicable-method-stx)))

(define-built-in-record-type <scheme-type-spec>
    <object-type-spec>
  make-scheme-type-spec scheme-type-spec?)

(define-built-in-record-type <closure-type-spec>
    <object-type-spec>
  make-closure-type-spec closure-type-spec?
  (methods
   (signature			closure-type-spec.signature)))

(define-built-in-record-type <struct-type-spec>
    <object-type-spec>
  make-struct-type-spec struct-type-spec?
  (methods
   (std				struct-type-spec.std)))

(define-built-in-record-type <record-type-spec>
    <object-type-spec>
  make-record-type-spec record-type-spec?
  (methods
   (rtd-id				record-type-spec.rtd-id)
   (rcd-id				record-type-spec.rcd-id)
   (super-protocol-id		record-type-spec.super-protocol-id)))

(define-built-in-record-type <compound-condition-type-spec>
    <objct-type-spec>
  make-compound-condition-type-spec compound-condition-type-spec?
  (methods
   (component-ots*	compound-condition-type-spec.component-ots*)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <union-type-spec>
    <object-type-spec>
  make-union-type-spec union-type-spec?
  (methods
   (component-ots*		union-type-spec.component-ots*)))

(define-built-in-record-type <intersection-type-spec>
    <object-type-spec>
  make-intersection-type-spec intersection-type-spec?
  (methods
   (component-ots*		intersection-type-spec.component-ots*)))

(define-built-in-record-type <complement-type-spec>
    <object-type-spec>
  make-complement-type-spec complement-type-spec?
  (methods
   (item-ots			complement-type-spec.item-ots)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <pair-type-spec>
    <object-type-spec>
  make-pair-type-spec pair-type-spec?
  (methods
   (car-ots			pair-type-spec.car-ots)
   (cdr-ots			pair-type-spec.cdr-ots)))

(define-built-in-record-type <pair-of-type-spec>
    <object-type-spec>
  make-pair-of-type-spec pair-of-type-spec?
  (methods
   (item-ots			pair-of-type-spec.item-ots)))

(define-built-in-record-type <list-type-spec>
    <object-type-spec>
  make-list-type-spec list-type-spec?
  (methods
   (item-ots*		list-type-spec.item-ots*)))

(define-built-in-record-type <list-of-type-spec>
    <object-type-spec>
  make-list-of-type-spec list-of-type-spec?
  (methods
   (item-ots			list-of-type-spec.item-ots)))

(define-built-in-record-type <vector-type-spec>
    <object-type-spec>
  make-vector-type-spec vector-type-spec?
  (methods
   (item-ots*		vector-type-spec.item-ots*)))

(define-built-in-record-type <vector-of-type-spec>
    <object-type-spec>
  make-vector-of-type-spec vector-of-type-spec?
  (methods
   (item-ots			vector-of-type-spec.item-ots)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <type-signature>
    <record>
  make-type-signature type-signature?
  (methods
   (specs			type-signature.specs)
   (syntax-object		type-signature.syntax-object)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <time>
    <record>
  current-time time?
  (methods
   (second			time-second)
   (nanosecond		time-nanosecond)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <stx>
    <record>
  #f stx?
  (methods
   (expr			stx-expr)
   (mark*			stx-mark*)
   (rib*			stx-rib*)
   (annotated-expr*		stx-annotated-expr*)))

(define-built-in-record-type <syntactic-identifier>
    <stx>
  #f syntactic-identifier?
  (methods
   (string			identifier->string)
   (label			syntactic-identifier->label)))

(define-built-in-record-type <syntax-clause-spec>
    <record>
  make-syntax-clause-spec syntax-clause-spec?
  (methods
   (keyword				syntax-clause-spec-keyword)
   (min-number-of-occurrences	syntax-clause-spec-min-number-of-occurrences)
   (max-number-of-occurrences	syntax-clause-spec-max-number-of-occurrences)
   (min-number-of-arguments		syntax-clause-spec-min-number-of-arguments)
   (max-number-of-arguments		syntax-clause-spec-max-number-of-arguments)
   (mutually-inclusive		syntax-clause-spec-mutually-inclusive)
   (mutually-exclusive		syntax-clause-spec-mutually-exclusive)
   (custom-data			syntax-clause-spec-custom-data)))

;;; --------------------------------------------------------------------

(define-built-in-record-type <scheme-type-descriptor>
    <record>
  #f scheme-type-descriptor?
  (methods
   (name				scheme-type-descriptor.name)
   (parent				scheme-type-descriptor.parent)
   (uids-list			scheme-type-descriptor.uids-list)
   (method-retriever			scheme-type-descriptor.method-retriever)))

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
