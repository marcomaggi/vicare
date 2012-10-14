;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs
(library (ikarus multiple-values)
  (export call-with-values values)
  (import (except (ikarus)
		  call-with-values
		  values)
    (ikarus system $stack))

  ;;The implementation of these two functions is as assembly subroutines
  ;;defined by the compiler.

  (define call-with-values
    ($make-call-with-values-procedure))
  (define values
    ($make-values-procedure))

  #| end of library |# )

;;; end of file
