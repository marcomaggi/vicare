;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: test utilities
;;;Date: Thu Sep 29, 2011
;;;
;;;Abstract
;;;
;;;	Code  from  "scheme/tests/framework.ss"  file  in  the  original
;;;	Ikarus distribution.
;;;
;;;Copyright (C) 2006-2010 Abdulaziz Ghuloum <aghuloum@cs.indiana.edu>
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

(library (ikarus-test-framework)
  (export define-tests src-file)
  (import (ikarus))

  (define (src-file x)
    (string-append (or (getenv "IKARUS_SRC_DIR") ".") "/" x))
  (define-syntax define-tests
    (syntax-rules ()
      [(_ test-all [p0 e0] ...)
       (define test-all
         (lambda ()
           (let ([p p0] [e e0])
             (unless (p e)
               (error 'test-all "failed"
                      '(p0 e0) e)))
           ...
           (printf "[~s: ~s] Happy Happy Joy Joy\n"
                   (length '(p0 ...))'test-all  )))])))

;;; end of file
