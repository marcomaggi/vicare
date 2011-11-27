;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for FFI functions
;;;Date: Sun Nov 27, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (vicare)
		(parameterize parametrise))
  (checks)
  (prefix (vicare words)
	  words.)
  (prefix (vicare foreign)
	  ffi.))

(check-set-mode! 'report-failed)
(display "*** testing Vicare FFI\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'pointers))

  (check
      (ffi.pointer? (ffi.integer->pointer 123))
    => #t)

  (check
      (ffi.pointer? '#(123))
    => #f)

  (check
      (catch #f
	(ffi.integer->pointer (+ 10 (words.greatest-machine-word))))
    => (list (+ 10 (words.greatest-machine-word))))

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer->integer (ffi.integer->pointer 123))
    => 123)

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer? (ffi.null-pointer))
    => #t)

  (check
      (ffi.pointer->integer (ffi.null-pointer))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer-diff (ffi.integer->pointer 123)
			(ffi.integer->pointer 100))
    => 23)

  (check
      (ffi.pointer-diff (ffi.integer->pointer 10)
			(ffi.integer->pointer 10))
    => 0)

  (check
      (ffi.pointer-diff (ffi.integer->pointer (words.greatest-machine-word))
			(ffi.integer->pointer 0))
    => (words.greatest-machine-word))

;;; --------------------------------------------------------------------

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) 1000)
    => (ffi.integer->pointer 1123))

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) 0)
    => (ffi.integer->pointer 123))

  (check
      (ffi.pointer-add (ffi.integer->pointer 123) -100)
    => (ffi.integer->pointer 23))


  #t)


;;;; done

(check-report)

;;; end of file
