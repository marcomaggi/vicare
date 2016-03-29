;;; -*- coding: utf-8-unix -*-
;;;
;;;Copyright (c) 2010-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software and associated documentation files  (the "Software"), to deal in the
;;;Software  without restriction,  including without  limitation the  rights to  use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY OF  ANY  KIND, EXPRESS  OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(library (psyntax.syntactic-binding-properties)
  (export
    ;; identifiers: syntactic binding properties
    syntactic-binding-putprop			$syntactic-binding-putprop
    syntactic-binding-getprop			$syntactic-binding-getprop
    syntactic-binding-remprop			$syntactic-binding-remprop
    syntactic-binding-property-list		$syntactic-binding-property-list)
  (import (rnrs)
    (psyntax.compat)
    (only (psyntax.lexical-environment)
	  id->label
	  error-unbound-identifier))


;;;; identifiers: syntactic binding properties

(define* (syntactic-binding-putprop {id identifier?} {key symbol?} value)
  ($syntactic-binding-putprop id key value))

(define* (syntactic-binding-getprop {id identifier?} {key symbol?})
  ($syntactic-binding-getprop id key))

(define* (syntactic-binding-remprop {id identifier?} {key symbol?})
  ($syntactic-binding-remprop id key))

(define* (syntactic-binding-property-list {id identifier?})
  ($syntactic-binding-property-list id))

;;; --------------------------------------------------------------------

(define* ($syntactic-binding-putprop id key value)
  (cond ((id->label id)
	 => (lambda (label)
	      ($putprop label key value)))
	(else
	 (error-unbound-identifier __who__ id))))

(define* ($syntactic-binding-getprop id key)
  (cond ((id->label id)
	 => (lambda (label)
	      ($getprop label key)))
	(else
	 (error-unbound-identifier __who__ id))))

(define* ($syntactic-binding-remprop id key)
  (cond ((id->label id)
	 => (lambda (label)
	      ($remprop label key)))
	(else
	 (error-unbound-identifier __who__ id))))

(define* ($syntactic-binding-property-list id)
  (cond ((id->label id)
	 => (lambda (label)
	      ($property-list label)))
	(else
	 (error-unbound-identifier __who__ id))))


;;;; done

#| end of library |# )

;;; end of file
