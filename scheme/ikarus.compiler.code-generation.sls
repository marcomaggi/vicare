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
    initialise-core-primitive-operations
    primitive-public-function-name->location-gensym
    current-primitive-locations
    pass-code-generation
    pass-specify-representation
    pass-impose-calling-convention/evaluation-order
    pass-assign-frame-sizes
    pass-color-by-chaitin
    pass-flatten-codes)
  (import (vicare)
    ;;NOTE Here we must load only "(ikarus.compiler.*)" libraries.
    (ikarus.compiler.config)
    (only (ikarus.compiler.helpers)
	  sl-apply-label-func)
    (only (ikarus.compiler.common-assembly-subroutines)
	  current-primitive-locations
	  primitive-public-function-name->location-gensym
	  sl-apply-label)
    (only (ikarus.compiler.core-primitive-operations)
	  initialise-core-primitive-operations)
    (ikarus.compiler.pass-specify-representation)
    (ikarus.compiler.pass-impose-evaluation-order)
    (ikarus.compiler.pass-assign-frame-sizes)
    (ikarus.compiler.pass-color-by-chaitin)
    (ikarus.compiler.pass-flatten-codes))

  (define (pass-code-generation x)
    (let* ((x  (pass-specify-representation x))
	   (x  (pass-impose-calling-convention/evaluation-order x))
	   (x  (pass-assign-frame-sizes x))
	   (x  (begin
		 (when (check-compiler-pass-preconditions)
		   (preconditions-for-color-by-chaitin x))
		 (pass-color-by-chaitin x)))
	   (code-object-sexp* (pass-flatten-codes x)))
      code-object-sexp*))

  (sl-apply-label-func sl-apply-label)

  #| end of library |# )

;;; end of file
