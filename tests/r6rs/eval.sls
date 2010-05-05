;;;Copyright (c) 2008 Matthew Flatt
;;;
;;;This library is free software;  you can redistribute it and/or modify
;;;it  under the  terms of  the GNU  Library General  Public  License as
;;;published by  the Free Software  Foundation; either version 2  of the
;;;License, or (at your option) any later version.
;;;
;;;This library is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;Library General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU Library  General Public
;;;License along with  this library; if not, write  to the Free Software
;;;Foundation,  Inc.,  51  Franklin  Street,  Fifth  Floor,  Boston,  MA
;;;02110-1301 USA.
#!r6rs

(library (tests r6rs eval)
  (export run-eval-tests)
  (import (rnrs)
          (rnrs eval)
          (tests r6rs test))

  (define (run-eval-tests)

    (test (eval '(let ((x 3)) x)
                (environment '(rnrs)))
          3)

    (test (eval
           '(eval:car (eval:cons 2 4))
           (environment
            '(prefix (only (rnrs) car cdr cons null?)
                     eval:)))
          2)

    ;; Check that `eval' at compile-time produces values (such as conditions)
    ;; that make sense at compile time (i.e., no phase crossing):
    (test (eval
           '(let-syntax ([x (lambda (stx)
                              (datum->syntax
                               #'here
                               (condition-message
                                (call/cc
                                 (lambda (esc)
                                   (with-exception-handler
                                    (lambda (exn) (esc exn))
                                    (lambda ()
                                      (eval '(assertion-violation 'exptime "ok")
                                            (environment
                                             '(rnrs)
                                             '(rnrs eval))))))))))])
              x)
           (environment '(rnrs) '(for (rnrs eval) expand)))
          "ok")

    ;;
    ))

