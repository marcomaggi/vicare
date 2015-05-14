;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!vicare
(library (ikarus.compiler.code-generation)
  (export
    alt-cogen
    current-primitive-locations
    primitive-public-function-name->location-gensym
    specify-representation
    impose-calling-convention/evaluation-order
    assign-frame-sizes
    color-by-chaitin
    flatten-codes
    initialise-core-primitive-operations)
  (import (rnrs)
    (ikarus.compiler.compat)
    (ikarus.compiler.config)
    (ikarus.compiler.helpers)
    (ikarus.compiler.typedefs)
    (ikarus.compiler.condition-types)
    (ikarus.compiler.unparse-recordised-code)
    (ikarus.compiler.intel-assembly)
    (ikarus.compiler.common-assembly-subroutines)
    (ikarus.compiler.pass-specify-representation)
    (ikarus.compiler.core-primitive-operations)
    (ikarus.compiler.pass-impose-evaluation-order)
    (ikarus.compiler.pass-assign-frame-sizes)
    (ikarus.compiler.pass-color-by-chaitin)
    (ikarus.compiler.pass-flatten-codes))

  (define (alt-cogen x)
    (let* ((x  (specify-representation x))
	   (x  (impose-calling-convention/evaluation-order x))
	   (x  (assign-frame-sizes x))
	   (x  (if (check-compiler-pass-preconditions)
		   (preconditions-for-color-by-chaitin x)
		 x))
	   (x  (color-by-chaitin x))
	   (code-object-sexp* (flatten-codes x)))
      code-object-sexp*))

  (sl-apply-label-func sl-apply-label)

  #| end of library |# )

;;; end of file
