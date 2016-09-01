;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: immutable pairs
;;;Date: Sat Jun 13, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (export <ipair> ipair icar icdr ipair?)
  (import (except (vicare)
		  <ipair> ipair icar icdr ipair?)
    (vicare system structs))

  ;; (define dummy-begin
  ;;   (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.immutable-pairs begin")))

  (define-struct (<ipair> ipair ipair?)
    (car cdr))

  ;;NOTE  A   custom  printer  for   the  "<ipair>"  struct-type  is   configured  in
  ;;"ikarus.structs.sls".   This is  because STD  handling functions  are initialised
  ;;there and not available here.  (Marco Maggi; Fri Mar 25, 2016)

  (define* (icar {P ipair?})
    ($<ipair>-car P))

  (define* (icdr {P ipair?})
    ($<ipair>-cdr P))

  ;; #!vicare
  ;; (foreign-call "ikrt_print_emergency" #ve(ascii "ikarus.immutable-pairs after"))

  #| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
