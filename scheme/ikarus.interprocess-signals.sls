;;;
;;;Part of: Vicare
;;;Contents: interprocess signals handling
;;;Date: Sat Nov  5, 2011
;;;
;;;Abstract
;;;
;;;	Export  a function  to convert  an interprocess  isgnal  code as
;;;	represented by the  (vicare interprocess-signals) library into a
;;;	string representing the signal symbol.
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
(library (ikarus.interprocess-signals)
  (export interprocess-signal->string)
  (import (except (ikarus)
		  interprocess-signal->string)
    (vicare interprocess-signals)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))


(define-argument-validation (fixnum who obj)
  (fixnum? obj)
  (assertion-violation who "expected fixnum as argument" obj))


(define-syntax interprocess-signal-vector
  (lambda (stx)
    (define (%mk-vector)
      (let* ((max	(fold-left (lambda (max pair)
				     (let ((code (cdr pair)))
				       (cond ((not code)	max)
					     ((< max code)	code)
					     (else		max))))
			  0 interprocess-signal-alist))
	     (vec.len	(fx+ 1 max))
	     ;;All the unused positions are set to #f.
	     (vec	(make-vector vec.len #f)))
	(for-each (lambda (pair)
		    (when (cdr pair)
		      (vector-set! vec (cdr pair) (car pair))))
	  interprocess-signal-alist)
	vec))

    (define interprocess-signal-alist
      `((SIGFPE		. ,SIGFPE)
	(SIGILL		. ,SIGILL)
	(SIGSEGV	. ,SIGSEGV)
	(SIGBUS		. ,SIGBUS)
	(SIGABRT	. ,SIGABRT)
	(SIGIOT		. ,SIGIOT)
	(SIGTRAP	. ,SIGTRAP)
	(SIGEMT		. ,SIGEMT)
	(SIGSYS		. ,SIGSYS)
	(SIGTERM	. ,SIGTERM)
	(SIGINT		. ,SIGINT)
	(SIGQUIT	. ,SIGQUIT)
	(SIGKILL	. ,SIGKILL)
	(SIGHUP		. ,SIGHUP)
	(SIGALRM	. ,SIGALRM)
	(SIGVRALRM	. ,SIGVRALRM)
	(SIGPROF	. ,SIGPROF)
	(SIGIO		. ,SIGIO)
	(SIGURG		. ,SIGURG)
	(SIGPOLL	. ,SIGPOLL)
	(SIGCHLD	. ,SIGCHLD)
	(SIGCLD		. ,SIGCLD)
	(SIGCONT	. ,SIGCONT)
	(SIGSTOP	. ,SIGSTOP)
	(SIGTSTP	. ,SIGTSTP)
	(SIGTTIN	. ,SIGTTIN)
	(SIGTTOU	. ,SIGTTOU)
	(SIGPIPE	. ,SIGPIPE)
	(SIGLOST	. ,SIGLOST)
	(SIGXCPU	. ,SIGXCPU)
	(SIGXSFZ	. ,SIGXSFZ)
	(SIGUSR1	. ,SIGUSR1)
	(SIGUSR2	. ,SIGUSR2)
	(SIGWINCH	. ,SIGWINCH)
	(SIGINFO	. ,SIGINFO)))

    (syntax-case stx ()
      ((?ctx)
       #`(quote #,(datum->syntax #'?ctx (%mk-vector)))))))


(define INTERPROCESS-SIGNAL-VECTOR (interprocess-signal-vector))

(define (interprocess-signal->string negated-interprocess-signal-code)
  ;;Defined  by   Vicare.   Convert  an  interprocess   signal  code  as
  ;;represented  by  the (vicare  interprocess-signals)  library into  a
  ;;string representing the interprocess signal symbol.
  ;;
  (define who 'interprocess-signal->string)
  (with-arguments-validation (who)
      ((fixnum negated-interprocess-signal-code))
    (let ((interprocess-signal-code (unsafe.fx- 0 negated-interprocess-signal-code)))
      (and (unsafe.fx> interprocess-signal-code 0)
	   (unsafe.fx< interprocess-signal-code (vector-length INTERPROCESS-SIGNAL-VECTOR))
	   (vector-ref INTERPROCESS-SIGNAL-VECTOR interprocess-signal-code)))))


;;;; done

)

;;; end of file
