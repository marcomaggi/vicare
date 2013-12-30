;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: condition object types
;;;Date: Mon Dec 30, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (nausicaa language oopp conditions)
  (export
    &tagged-binding-violation
    make-tagged-binding-violation
    tagged-binding-violation?
    tagged-binding-violation)
  (import (vicare))

  (define-condition-type &tagged-binding-violation
      &assertion
    make-tagged-binding-violation
    tagged-binding-violation?)

  (define (tagged-binding-violation who message . irritants)
    (raise
     (condition (make-who-condition who)
		(make-message-condition message)
		(make-tagged-binding-violation)
		(make-irritants-condition irritants))))

  #| end of library |# )

;;; end of file
;; Local Variables:
;; End:
