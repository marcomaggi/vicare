;;;
;;;Part of: Vicare Scheme
;;;Contents: istack interface for stacks
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
(library (vicare containers istacks stacks)
  (export
    <istack-stack>
    make-istack-stack
    istack-stack?)
  (import (vicare)
    (vicare containers istacks)
    (vicare containers stacks))


;;;; type definition

(module (<istack-stack> make-istack-stack istack-stack?)

  (define-record-type (<istack-stack> make-istack-stack istack-stack?)
    (nongenerative vicare:containers:<istack-stack>)
    (parent <istack>)
    (fields (immutable stack))
    (protocol
     (lambda (make-istack)
       (lambda* ({D stack?})
	 ((make-istack istack-stack-empty? istack-stack-top
		       istack-stack-push! istack-stack-pop!)
	  D)))))

  (define (istack-stack-top IS)
    ($stack-top ($<istack-stack>-stack IS)))

  (define (istack-stack-push! IS obj)
    ($stack-push! ($<istack-stack>-stack IS) obj))

  (define (istack-stack-pop! IS)
    ($stack-pop! ($<istack-stack>-stack IS)))

  (define (istack-stack-empty? IS)
    ($stack-empty? ($<istack-stack>-stack IS)))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
