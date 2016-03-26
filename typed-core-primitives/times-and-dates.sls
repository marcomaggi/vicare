;; -*- coding: utf-8-unix -*-
;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for times and dates core primitives
;;Date: Mon Jan  4, 2016
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (typed-core-primitives times-and-dates)
  (export typed-core-primitives.times-and-dates)
  (import (vicare)
    (typed-core-primitives syntaxes))

(define (typed-core-primitives.times-and-dates)


;;;; time and dates, safe functions

(section

(declare-type-predicate time?	<time>)

;;; --------------------------------------------------------------------
;;; constructors

(declare-core-primitive make-time
    (safe)
  (signatures
   ((<exact-integer> <exact-integer>)	=> (<time>)))
  (attributes
   ((_ _)		effect-free result-true)))

(declare-core-primitive current-time
    (safe)
  (signatures
   (()			=> (<time>)))
  (attributes
   (()			effect-free result-true)))

(declare-core-primitive time-from-now
    (safe)
  (signatures
   ((<time>)		=> (<time>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; accessors

(declare-core-primitive time-second
    (safe)
  (signatures
   ((<time>)		=> (<exact-integer>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-nanosecond
    (safe)
  (signatures
   ((<time>)		=> (<exact-integer>)))
  (attributes
   ((_)			effect-free result-true)))

(declare-core-primitive time-gmt-offset
    (safe)
  (signatures
   ((<time>)		=> (<exact-integer>)))
  (attributes
   ((_)			effect-free result-true)))

;;; --------------------------------------------------------------------
;;; comparison

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<time> <time>)	=> (<boolean>)))
		   (attributes
		    ((_ _)		effect-free))))
		)))
  (declare time=?)
  (declare time<?)
  (declare time>?)
  (declare time<=?)
  (declare time>=?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; operations

(let-syntax
    ((declare (syntax-rules ()
		((_ ?who)
		 (declare-core-primitive ?who
		     (safe)
		   (signatures
		    ((<time> <time>)	=> (<time>)))
		   (attributes
		    ((_ _)		effect-free))))
		)))
  (declare time-addition)
  (declare time-difference)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------
;;; miscellaneous

(declare-core-primitive date-string
    (safe)
  (signatures
   (()		=> (<string>)))
  (attributes
   (()		effect-free result-true)))

/section)


;;;; done

#| end of define |# )

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
