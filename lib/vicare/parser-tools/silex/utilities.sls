;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: utilities for lexers
;;;Date: Wed Jun  9, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare parser-tools silex utilities)
  (export make-max-count-lexer)
  (import (rnrs))


(define (make-max-count-lexer lexer max-number-of-tokens error-handler)
  ;;Return a  lexer function wrapping  LEXER; if the number  of returned
  ;;tokens  reaches  MAX-NUMBER-OF-TOKENS:  the thunk  ERROR-HANDLER  is
  ;;invoked.
  ;;
  (let ((count 0))
    (lambda ()
      (let ((token (lexer)))
	(set! count (+ 1 count))
	(if (< count max-number-of-tokens)
	    token
	  (error-handler))))))


;;;; done

)

;;; end of file
