;;;SILex - Scheme Implementation of Lex
;;;
;;;Copyright (C) 2001 Danny Dube'
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
(library (vicare parser-tools silex lexer)
  (export
    make-lexer
;;; bindings from (vicare parser-tools silex input-system)
    make-IS		lexer-get-func-offset
    lexer-get-func-getc	lexer-get-func-ungetc
    lexer-get-func-line	lexer-get-func-column
    ;; auxiliary syntaxes
    counters:		port:
    procedure:		string:)
  (import (rnrs)
    (vicare parser-tools silex input-system)
    (vicare parser-tools silex code-lexer-driver)
    (vicare parser-tools silex tree-lexer-driver)
    (vicare parser-tools silex char-lexer-driver))
  (define (make-lexer tables IS)
    (case (vector-ref tables 4) ; automaton type
      ((decision-trees)
       (make-tree-lexer tables IS))
      ((tagged-chars-lists)
       (make-char-lexer tables IS))
      ((code)
       (make-code-lexer tables IS)))))

;;; end of file
