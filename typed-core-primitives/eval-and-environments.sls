;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for eval core primitives
;;Date: Tue Dec 25, 2015
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
(library (typed-core-primitives eval-and-environments)
  (export typed-core-primitives.eval-and-environments)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.eval-and-environments)


;;;; evaluation and lexical environments

(section

(declare-core-primitive eval
    (safe)
  (signatures
   ((_ <lexical-environment>)			=> <list>)
   ((_ <lexical-environment>
       <expander-options> <compiler-options>)	=> <list>)))

(declare-core-primitive make-expander-options
    (safe)
  (signatures
   (((list-of <symbol>))		=> (<expander-options>)))
  (attributes
   (_			effect-free result-true)))

(declare-core-primitive make-compiler-options
    (safe)
  (signatures
   (((list-of <symbol>))		=> (<compiler-options>)))
  (attributes
   (_			effect-free result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive environment
    (safe)
  (signatures
   (<list>			=> (<non-interaction-lexical-environment>)))
  (attributes
   ((_)				result-true)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<fixnum>)		=> (<non-interaction-lexical-environment>)))
		   (attributes
		    (()			result-true))))
		)))
  (declare null-environment)
  (declare scheme-report-environment)
  #| end of LET-SYNTAX |# )

(declare-core-primitive interaction-environment
    (safe)
  (signatures
   (()					=> (<interaction-lexical-environment>))
   ((<interaction-lexical-environment>)	=> (<void>)))
  (attributes
   (()			result-true)
   ((_)			result-true)))

(declare-core-primitive new-interaction-environment
    (safe)
  (signatures
   (()					=> (<interaction-lexical-environment>))
   ((<list>)				=> (<interaction-lexical-environment>)))
  (attributes
   (()			effect-free result-true)
   ((_)			result-true)))

;;; --------------------------------------------------------------------

(declare-core-primitive environment?
    (safe)
  (signatures
   ((<lexical-environment>)		=> (<true>))
   ((<top>)				=> (<boolean>)))
  (attributes
   ((_)					effect-free)))

(declare-type-predicate interaction-lexical-environment?	<interaction-lexical-environment>)
(declare-type-predicate non-interaction-lexical-environment?	<non-interaction-lexical-environment>)

;;; --------------------------------------------------------------------

(declare-core-primitive environment-symbols
    (safe)
  (signatures
   ((<lexical-environment>)		=> (<list>)))
  (attributes
   ((_)					result-true effect-free)))

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who ?return-value-tag)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<non-interaction-lexical-environment>)	=> (?return-value-tag)))
		   (attributes
		    ((_)		result-true effect-free))))
		)))
  (declare environment-libraries	<library>)
  (declare environment-labels		<list>)
  #| end of LET-SYNTAX |# )

(declare-core-primitive environment-binding
    (safe)
  (signatures
   ;; ((<symbol> <non-interaction-lexical-environment>)	=> (<false> <false>))
   ;; ((<symbol> <non-interaction-lexical-environment>)	=> (<symbol> <list>))
   ((<symbol> <non-interaction-lexical-environment>)	=> (<top> <top>)))
  (attributes
   ((_ _)		effect-free)))


/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
