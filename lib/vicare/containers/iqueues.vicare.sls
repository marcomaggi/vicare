;;;
;;;Part of: Vicare Scheme
;;;Contents: generic interface for queue-like structures
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
(library (vicare containers iqueues)
  (export
    <iqueue>		iqueue?
    iqueue-empty?	iqueue-top
    iqueue-push!	iqueue-pop!)
  (import (vicare))


;;;; type definition and core operations

(define-record-type (<iqueue> make-iqueue iqueue?)
  (nongenerative vicare:containers:<iqueue>)
  (fields (immutable top)
	  (immutable push!)
	  (immutable pop!)
	  (immutable empty?))
  (protocol
   (lambda (make-record)
     (lambda (top push pop empty?)
       (make-record top push pop empty?))))
  #| end of DEFINE-RECORD-TYPE|# )

(define* (iqueue-top {S iqueue?})
  (($<iqueue>-top S) S))

(define* (iqueue-push! {S iqueue?} obj)
  (($<iqueue>-push! S) S obj))

(define* (iqueue-pop! {S iqueue?})
  (($<iqueue>-pop! S) S))

(define* (iqueue-empty? {S iqueue?})
  (($<iqueue>-empty? S) S))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
