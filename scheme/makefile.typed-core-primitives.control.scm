;;
;;Part of: Vicare Scheme
;;Contents: table of expand-time properties for control core primitives
;;Date: Tue Dec 25, 2015
;;
;;Abstract
;;
;;
;;
;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; calling procedures and returning

(section

(declare-core-primitive apply
    (safe)
  (signatures
   ((<procedure> . _)		=> <top>)))

(declare-core-primitive values
    (safe)
  (signatures
   (<top>		=> <top>))
  (attributes
   (_			effect-free)))

(declare-core-primitive call-with-current-continuation
    (safe)
  (signatures
   ((<procedure>)	=> <top>)))

(declare-core-primitive call/cc
    (safe)
  (signatures
   ((<procedure>)	=> <top>)))

(declare-core-primitive call-with-values
    (safe)
  (signatures
   ((<procedure> <procedure>)	=> <top>)))

/section)


;;;; exceptions and dynamic environment, safe procedures

(section

(declare-core-primitive with-exception-handler
    (safe)
  (signatures
   ((<procedure> <procedure>)	=> <top>)))

(declare-core-primitive dynamic-wind
    (safe)
  (signatures
   ((<procedure> <procedure> <procedure>)	=> <top>)))

;;; --------------------------------------------------------------------

(declare-core-primitive raise
    (safe)
  (signatures
   ((<top>)		=> <top>)))

(declare-core-primitive raise-continuable
    (safe)
  (signatures
   ((<top>)		=> <top>)))

/section)


;;;; other primitives

(section

(declare-core-primitive make-guardian
    (safe)
  (signatures
   (()				=> (<procedure>)))
  (attributes
   (()				effect-free result-true)))

(declare-core-primitive make-parameter
    (safe)
  (signatures
   ((<top>)			=> (<procedure>))
   ((<top> <procedure>)	=> (<procedure>)))
  (attributes
   ((_)				effect-free result-true)
   ((_ _)			effect-free result-true)))

/section)


;;;; compensations, safe primitives

(section

(declare-core-primitive run-compensations
    (safe)
  (signatures
   (()			=> (<void>)))
  (attributes
   (()			result-true)))

(declare-core-primitive push-compensation-thunk
    (safe)
  (signatures
   ((<procedure>)	=> (<void>)))
  (attributes
   ((_)			result-true)))

/section)


;;;; invocation and termination procedures

(section

(declare-object-retriever command-line	<nlist>)

(declare-core-primitive exit
    (safe)
  (signatures
   (()				=> (<void>))
   ((<fixnum>)			=> (<void>))))

(declare-parameter exit-hooks	<list>)

/section)


;;;; promises, safe primitives

(section

(declare-type-predicate promise?	<promise>)

(declare-core-primitive make-promise
    (safe)
  (signatures
   ((<procedure>)	=> (<promise>)))
  (attributes
   ((_)			result-true)))

(declare-core-primitive force
    (safe)
  (signatures
   ((<promise>)		=> <top>)))

/section)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; eval: (put 'declare-core-primitive		'scheme-indent-function 2)
;; End:
