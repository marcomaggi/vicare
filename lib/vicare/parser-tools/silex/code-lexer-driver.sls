;;; -*- coding: utf-8-unix -*-
;;;
;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2001  Danny Dube'
;;;Port to R6RS and Vicare integration by Marco Maggi
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
(library (vicare parser-tools silex code-lexer-driver)
  (export make-code-lexer)
  (import (rnrs))
  (define (make-code-lexer tables IS)
    ;; Fabrication d'un lexer a partir de code pre-genere
    (let ((<<EOF>>-pre-action   (vector-ref tables 1))
	  (<<ERROR>>-pre-action (vector-ref tables 2))
	  (rules-pre-action     (vector-ref tables 3))
	  (code                 (vector-ref tables 5)))
      (code <<EOF>>-pre-action <<ERROR>>-pre-action rules-pre-action IS))))

;;; end of file
