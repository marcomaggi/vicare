;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for debugging utilities
;;;Date: Sun Mar 25, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (vicare platform words)
	case-word-size)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare debugging utilities\n")


(parametrise ((check-test-name	'words))

  (case-word-size
   ((32)
    (check
	(integer->machine-word #b11100)
      => #b111))
   ((64)
    (check
	(integer->machine-word #b111000)
      => #b111)))

  (check
      (integer->machine-word #x3F)
    => #t)

  (check
      (integer->machine-word #x2F)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (machine-word->integer #t)
    => #x3F)

  (check
      (machine-word->integer #f)
    => #x2F)

  #t)


(parametrise ((check-test-name	'dummy-args))

  (check
      (foreign-call "ikrt_dummy_arg_0")
    => #t)

  (check
      (foreign-call "ikrt_dummy_arg_1" 2)
    => 1002)

  (check
      (foreign-call "ikrt_dummy_arg_2" 2 30)
    => 1032)

  (check
      (foreign-call "ikrt_dummy_arg_3" 2 30 400)
    => 1432)

  (check
      (foreign-call "ikrt_dummy_arg_4" 1 2 4 8)
    => (+ 1000 1 2 4 8))

  (check
      (foreign-call "ikrt_dummy_arg_5" 1 2 4 8 16)
    => (+ 1000 1 2 4 8 16))

  (check
      (foreign-call "ikrt_dummy_arg_6" 1 2 4 8 16 32)
    => (+ 1000 1 2 4 8 16 32))

  (check
      (foreign-call "ikrt_dummy_arg_7" 1 2 4 8 16 32 64)
    => (+ 1000 1 2 4 8 16 32 64))

  (check
      (foreign-call "ikrt_dummy_arg_8" 1 2 4 8 16 32 64 128)
    => (+ 1000 1 2 4 8 16 32 64 128))

  (check
      (foreign-call "ikrt_dummy_arg_9" 1 2 4 8 16 32 64 128 256)
    => (+ 1000 1 2 4 8 16 32 64 128 256))

  #t)


;;;; done

(check-report)

;;; end of file
