;;;
;;;Part of: Vicare Scheme
;;;Contents: non-OO stack interface
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
(library (vicare containers istacks)
  (export
    <istack>			istack?
    istack-empty?		istack-top
    istack-push!		istack-pop!)
  (import (vicare))


;;;; type definition and core operations

(define-record-type (<istack> make-istack istack?)
  (nongenerative vicare:containers:<istack>)
  (fields (immutable top)
	  (immutable push!)
	  (immutable pop!)
	  (immutable empty?))
  (protocol
   (lambda (make-record)
     (lambda (top push pop empty?)
       (make-record top push pop empty?))))
  #| end of DEFINE-RECORD-TYPE|# )

(define* (istack-top {S istack?})
  (($<istack>-top S) S))

(define* (istack-push! {S istack?} obj)
  (($<istack>-push! S) S obj))

(define* (istack-pop! {S istack?})
  (($<istack>-pop! S) S))

(define* (istack-empty? {S istack?})
  (($<istack>-empty? S) S))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
