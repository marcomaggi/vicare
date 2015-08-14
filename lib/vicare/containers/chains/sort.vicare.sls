;;;
;;;Part of: Vicare Scheme
;;;Contents: sorting chains
;;;Date: Fri Aug 14, 2015
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
(library (vicare containers chains sort)
  (export
    chain-sort-forwards		$chain-sort-forwards)
  (import (vicare)
    (vicare containers chains)
    (vicare containers binary-heaps))


(define* (chain-sort-forwards {item< procedure?} {chain chain?})
  ($chain-sort-forwards item< chain))

(define ($chain-sort-forwards item< chain)
  ($binary-heap-fold!
      (lambda (knil obj)
	($chain-push-rear! knil (make-chain-link obj)))
    '()
    (chain-fold-left-forwards
	(lambda (knil obj)
	  (binary-heap-push! knil obj)
	  knil)
      (make-binary-heap item<)
      chain))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'chain-fold-left-forwards		'scheme-indent-function 1)
;; eval: (put '$binary-heap-fold!		'scheme-indent-function 1)
;; End:
