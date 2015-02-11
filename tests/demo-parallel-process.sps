;;;
;;;Part of: Vicare Scheme
;;;Contents: demo of parallel process
;;;Date: Wed Feb 11, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(import (vicare)
  (prefix (vicare posix) px.)
  (prefix (vicare platform constants) const.))

(define (parent-proc child-pid child-stdin child-stdout child-stderr)
  (with-unwind-protection
      (lambda (why)
	(close-output-port child-stdin)
	(close-input-port  child-stdout)
	(close-input-port  child-stderr))
    (lambda ()
      (let loop ((status (px.waitpid child-pid const.WNOHANG)))
	(cond ((not status)
	       ;;Child still running.
	       (yield)
	       (loop (px.waitpid child-pid const.WNOHANG)))
	      ((px.WIFEXITED status)
	       ;;Child exited.
	       (values status (read-all child-stdout) (get-string-all child-stderr)))
	      (else
	       (error #f "child process exited abnormally" status)))))))

(define (child-thunk)
  (guard (E (else
	     (print-condition E)
	     (exit 1)))
    (px.close-ports-in-close-on-exec-mode)
    (write '(1 2 3) (console-output-port))
    (write '(4 5 6) (console-output-port))
    (flush-output-port (console-output-port))
    (put-string (console-error-port) "done\n")
    (flush-output-port (console-error-port))
    (exit 0)))

(define (read-all port)
  (let ((obj (read port)))
    (if (eof-object? obj)
	'()
      (cons obj (read-all port)))))

(coroutine
    (lambda ()
      (px.flush-ports-in-close-on-exec-mode)
      (receive (status out err)
	  (px.fork-with-textual-ports parent-proc child-thunk)
	(printf "out: ~s\n" out)
	(printf "err: ~a\n" err)
	(flush-output-port (current-output-port)))))

(finish-coroutines)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
