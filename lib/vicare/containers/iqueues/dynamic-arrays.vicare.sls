;;;
;;;Part of: Vicare Scheme
;;;Contents: iqueue interface for dynamic arrays
;;;Date: Mon Aug 31, 2015
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
(library (vicare containers iqueues dynamic-arrays)
  (export
    <iqueue-dynamic-array>
    make-iqueue-dynamic-array
    iqueue-dynamic-array?)
  (import (vicare)
    (vicare containers iqueues)
    (vicare containers dynamic-arrays))


;;;; type definition and core operations

(module (<iqueue-dynamic-array> make-iqueue-dynamic-array iqueue-dynamic-array?)

  (define-record-type (<iqueue-dynamic-array> make-iqueue-dynamic-array iqueue-dynamic-array?)
    (nongenerative vicare:containers:<iqueue-dynamic-array>)
    (parent <iqueue>)
    (fields (immutable arry))
    (protocol
     (lambda (make-iqueue)
       (lambda* ({A dynamic-array?})
	 ((make-iqueue iqueue-dynamic-array-top iqueue-dynamic-array-push! iqueue-dynamic-array-pop! iqueue-dynamic-array-empty?)
	  A)))))

  (define (iqueue-dynamic-array-top IA)
    ($dynamic-array-front ($<iqueue-dynamic-array>-arry IA)))

  (define (iqueue-dynamic-array-push! IA obj)
    ($dynamic-array-push-rear! ($<iqueue-dynamic-array>-arry IA) obj))

  (define (iqueue-dynamic-array-pop! IA)
    ($dynamic-array-pop-front! ($<iqueue-dynamic-array>-arry IA)))

  (define (iqueue-dynamic-array-empty? IA)
    ($dynamic-array-empty? ($<iqueue-dynamic-array>-arry IA)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
