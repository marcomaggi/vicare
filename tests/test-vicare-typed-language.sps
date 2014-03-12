;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for typed language extensions
;;;Date: Wed Mar 12, 2014
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (prefix (vicare expander object-spec) typ.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: expand-time types\n")


(parametrise ((check-test-name	'parsing-tagged-bindings))

  (check
      (typ.tagged-identifier? #'(brace X fixnum))
    => #t)

  (check
      (typ.tagged-identifier? #'X)
    => #t)

  (check
      (typ.tagged-identifier? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (typ.parse-tagged-identifier #'(brace X fixnum))
    (=> syntax=?)
    #'X #'fixnum)

  (check
      (typ.parse-tagged-identifier #'X)
    (=> syntax=?)
    #'X #f)

;;; --------------------------------------------------------------------
;;; list of tagged identifiers

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}))
    (=> syntax=?)
    #'(a) #'(fixnum))

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}
				       {b string}))
    (=> syntax=?)
    #'(a b) #'(fixnum string))

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}
				       {b string}
				       {c vector}))
    (=> syntax=?)
    #'(a b c) #'(fixnum string vector))

;;;

  (check
      (typ.parse-tagged-identifiers #'(a))
    (=> syntax=?)
    #'(a) #'(#f))

  (check
      (typ.parse-tagged-identifiers #'(a b))
    (=> syntax=?)
    #'(a b) #'(#f #f))

  (check
      (typ.parse-tagged-identifiers #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(#f #f #f))

;;;

  (check
      (typ.parse-tagged-identifiers #'(a
				       {b string}))
    (=> syntax=?)
    #'(a b) #'(#f string))

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}
				       b))
    (=> syntax=?)
    #'(a b) #'(fixnum #f))

  (check
      (typ.parse-tagged-identifiers #'(a
				       {b string}
				       {c vector}))
    (=> syntax=?)
    #'(a b c) #'(#f string vector))

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}
				       b
				       {c vector}))
    (=> syntax=?)
    #'(a b c) #'(fixnum #f vector))

  (check
      (typ.parse-tagged-identifiers #'({a fixnum}
				       {b string}
				       c))
    (=> syntax=?)
    #'(a b c) #'(fixnum string #f))

;;; --------------------------------------------------------------------
;;; tagged formals

;;; tagged

  (check
      (typ.parse-tagged-formals #'({a fixnum}
				   {b string}))
    (=> syntax=?)
    #'(a b) #'(fixnum string))

  (check
      (typ.parse-tagged-formals #'({a fixnum}
				   {b string}
				   {c vector}))
    (=> syntax=?)
    #'(a b c) #'(fixnum string vector))

;;; untagged

  (check
      (typ.parse-tagged-formals #'(a))
    (=> syntax=?)
    #'(a) #'(#f))

  (check
      (typ.parse-tagged-formals #'(a b))
    (=> syntax=?)
    #'(a b) #'(#f #f))

  (check
      (typ.parse-tagged-formals #'(a b c))
    (=> syntax=?)
    #'(a b c) #'(#f #f #f))

;;; mixed tagged and untagged

  (check
      (typ.parse-tagged-formals #'(a
				   {b string}))
    (=> syntax=?)
    #'(a b) #'(#f string))

  (check
      (typ.parse-tagged-formals #'({a fixnum}
				   b))
    (=> syntax=?)
    #'(a b) #'(fixnum #f))

  (check
      (typ.parse-tagged-formals #'(a
				   {b string}
				   {c vector}))
    (=> syntax=?)
    #'(a b c) #'(#f string vector))

  (check
      (typ.parse-tagged-formals #'({a fixnum}
				   b
				   {c vector}))
    (=> syntax=?)
    #'(a b c) #'(fixnum #f vector))

  (check
      (typ.parse-tagged-formals #'({a fixnum}
				   {b string}
				   c))
    (=> syntax=?)
    #'(a b c) #'(fixnum string #f))

;;; args argument

    (check	;tagged args argument
      (typ.parse-tagged-formals #'{args list-of-fixnums})
    (=> syntax=?)
    #'args #'list-of-fixnums)

    (check	;UNtagged args argument
      (typ.parse-tagged-formals #'args)
    (=> syntax=?)
    #'args #f)

;;; rest argument

  (check	;tagged rest
      (typ.parse-tagged-formals #'({a fixnum} . {rest fixnums}))
    (=> syntax=?)
    #'(a . rest) #'(fixnum . fixnums))

  (check	;UNtagged rest
      (typ.parse-tagged-formals #'({a fixnum} . rest))
    (=> syntax=?)
    #'(a . rest) #'(fixnum . #f))

  (check	;tagged rest
      (typ.parse-tagged-formals #'({a fixnum} {b string} . {rest fixnums}))
    (=> syntax=?)
    #'(a b . rest) #'(fixnum string . fixnums))

  (check	;UNtagged rest
      (typ.parse-tagged-formals #'({a fixnum} {b string} . rest))
    (=> syntax=?)
    #'(a b . rest) #'(fixnum string . #f))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; End:
