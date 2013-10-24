;;;
;;;Part of: Vicare Silex
;;;Contents: tests for SILex with lexert tables from files
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare language-extensions sentinels)
  (prefix (vicare parser-tools silex lexer) lex.)
  (libtest silex-test)
  (libtest calc-code-lexer)
  (libtest calc-portable-lexer)
  (libtest calc-tree-lexer)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare parser tools: silex from file\n")


(test-calc calc-lexer-table/code)
(test-calc calc-lexer-table/portable)
(test-calc calc-lexer-table/tree)

;;Test getting the chars until the end.
(let* ((IS		(lex.make-IS (lex.string: "1+2+3") (lex.counters: 'line)))
       (lexer		(lex.make-lexer calc-lexer-table/code IS))
       (lexer-getc	(lex.lexer-get-func-getc IS))
       (lexer-ungetc	(lex.lexer-get-func-ungetc IS)))

  (check
      (and (char=? #\1 (lexer-getc))
	   (char=? #\+ (lexer-getc))
	   (char=? #\2 (lexer-getc))
	   (char=? #\+ (lexer-getc))
	   (char=? #\3 (lexer-getc))
	   (eof-object? (lexer-getc))
	   (eof-object? (lexer-getc))
	   (eof-object? (lexer-getc)))
    => #t)

  (lexer-ungetc)
  (lexer-ungetc)
  (lexer-ungetc)
  (lexer-ungetc)
  (lexer-ungetc)

  (check
      (do ((token (lexer) (lexer))
	   (out   '()))
	  ((eof-object? token)
	   (reverse out))
	(set! out (cons token out)))
    => '(1 + 2 + 3)))

(check-report)

;;; end of file
