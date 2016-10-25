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


;;;; syntaxes

(define-syntax (define-built-in-label-type stx)
  (define (%boolean-or-id? obj)
    (or (identifier? obj)
	(boolean? (syntax->datum obj))))
  (define (%false-or-id? obj)
    (or (identifier? obj)
	(not (syntax->datum obj))))
  (syntax-case stx (parent constructor destructor type-predicate equality-predicate comparison-procedure hash-function methods)
    ((?kwd ?type-name
	   (parent ?parent-annotation) (constructor ?constructor) (destructor ?destructor) (type-predicate ?type-predicate))
     (and (identifier? #'?type-name)
	  (%boolean-or-id? #'?constructor)
	  (%false-or-id? #'?destructor)
	  (%false-or-id? #'?type-predicate))
     #'(?kwd ?type-name
	     (parent ?parent-annotation) (constructor ?constructor) (destructor ?destructor) (type-predicate ?type-predicate)
	     (methods)))

    ((?kwd ?type-name
	   (parent ?parent-annotation) (constructor ?constructor) (destructor ?destructor) (type-predicate ?type-predicate)
	   (methods (?method-name ?method-procname) ...))
     (and (identifier? #'?type-name)
	  (%boolean-or-id? #'?constructor)
	  (%false-or-id? #'?destructor)
	  (%false-or-id? #'?type-predicate))
     #'(?kwd ?type-name
	     (parent ?parent-annotation) (constructor ?constructor) (destructor ?destructor) (type-predicate ?type-predicate)
	     (equality-predicate #f) (comparison-procedure #f) (hash-function #f)
	     (methods (?method-name ?method-procname) ...)))

    ((_    ?type-name
	   (parent ?parent-annotation) (constructor ?constructor) (destructor ?destructor) (type-predicate ?type-predicate)
	   (equality-predicate ?equality-predicate) (comparison-procedure ?comparison-procedure) (hash-function ?hash-function)
	   (methods (?method-name ?method-procname) ...))
     (and (identifier? #'?type-name)
	  (%boolean-or-id? #'?constructor)
	  (%false-or-id? #'?destructor)
	  (%false-or-id? #'?type-predicate)
	  (%false-or-id? #'?equality-predicate)
	  (%false-or-id? #'?comparison-procedure)
	  (%false-or-id? #'?hash-function))
     (let ((type-name.str (symbol->string (syntax->datum #'?type-name))))
       (define (mkid . str*)
	 (datum->syntax #'?type-name (string->symbol (apply string-append str*))))
       (with-syntax
	   ((UID	(datum->syntax #'?kwd (string->symbol
					       (string-append "vicare:core-type:" type-name.str)))))
	 #'(set-cons! VICARE-CORE-BUILT-IN-LABEL-TYPES-SYNTACTIC-BINDING-DESCRIPTORS
		      (quote (?type-name
			      ($core-label-type-name
			       . #(?type-name UID ?parent-annotation ?constructor ?destructor ?type-predicate
					      ?equality-predicate ?comparison-procedure ?hash-function
					      #((?method-name . ?method-procname) ...)))))))))
    ))


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


;;;; input/output

(define-built-in-label-type <binary-input-port>
  (parent (or <binary-input-only-port> <binary-input/output-port>))
  (constructor #f)
  (destructor #f)
  (type-predicate #f)
  (equality-predicate #f)
  (comparison-procedure #f)
  (hash-function port-hash)
  (methods
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)))

(define-built-in-label-type <binary-output-port>
  (parent (or <binary-output-only-port> <binary-input/output-port>))
  (constructor #f)
  (destructor #f)
  (type-predicate #f)
  (equality-predicate #f)
  (comparison-procedure #f)
  (hash-function port-hash)
  (methods
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)))

;;; --------------------------------------------------------------------

(define-built-in-label-type <textual-input-port>
  (parent (or <textual-input-only-port> <textual-input/output-port>))
  (constructor #f)
  (destructor #f)
  (type-predicate #f)
  (equality-predicate #f)
  (comparison-procedure #f)
  (hash-function port-hash)
  (methods
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)))

(define-built-in-label-type <textual-output-port>
  (parent (or <textual-output-only-port> <textual-input/output-port>))
  (constructor #f)
  (destructor #f)
  (type-predicate #f)
  (equality-predicate #f)
  (comparison-procedure #f)
  (hash-function port-hash)
  (methods
   (putprop			port-putprop)
   (getprop			port-getprop)
   (remprop			port-remprop)
   (property-list		port-property-list)))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
