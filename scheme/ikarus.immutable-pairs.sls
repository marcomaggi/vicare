;;;
;;;Part of: vicare scheme
;;;Contents: immutable pairs
;;;Date: Sat Jun 13, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus.immutable-pairs)
  (export immutable-pair ipair icar icdr ipair?)
  (import (except (vicare)
		  ipair icar icdr ipair?))

  (define-struct immutable-pair
    (car cdr))

  (define ipair		make-immutable-pair)
  (define ipair?	immutable-pair?)

  (define* (icar {P ipair?})
    ($immutable-pair-car P))

  (define* (icdr {P ipair?})
    ($immutable-pair-cdr P))

  ;;NOTE The printer for immutable pairs is defined in "ikarus.structs.sls".

  #| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
