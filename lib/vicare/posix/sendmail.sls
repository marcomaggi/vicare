;;;
;;;Part of: Vicare Scheme
;;;Contents: sending mail with sendmail
;;;Date: Thu Jan 22, 2015
;;;
;;;Abstract
;;;
;;;	This library defines  an API to send mail with  the locally installed program
;;;	"sendmail".
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
(library (vicare posix sendmail)
  (export sendmail)
  (import (vicare)
    (prefix (vicare posix) px.))


(define* (sendmail {message.bv bytevector?})
  (let-values
      (((child-stdin          parent->child-stdin) (px.pipe))
       ((child-stdout->parent child-stdout)        (px.pipe))
       ((child-stderr->parent child-stderr)        (px.pipe)))
    (px.fork
      (lambda (child-pid) ;here we are in the parent
	;;Write message.
	(unwind-protect
	    (px.write parent->child-stdin message.bv)
	  (px.close parent->child-stdin))
	(unwind-protect
	    (begin
	      ;;Wait until the child exits.
	      (let ((status (px.waitpid child-pid 0)))
		(if (and (px.WIFEXITED status)
			 (zero? (px.WEXITSTATUS status)))
		    ;;Read the  output from  "sendmail" generated  by the  "-v" flag.
		    ;;Sendmail writes to its stdout, rather than to its stderr.
		    (%read-stdout-from-child child-stdout->parent)
		  (error __who__
		    "sendmail process exited abnormally"
		    status))))
	  (px.close child-stdout->parent)
	  (px.close child-stderr->parent)))
      (lambda ()	;here we are in the child
	(guard (E (else
		   (print-condition E)
		   (exit 1)))
	  (%prepare-child-file-descriptors child-stdin child-stdout child-stderr)
	  (px.execvp "sendmail" '("sendmail" "-t" "-i" "-v")))))))


;;;; file descriptors

(define (%prepare-child-file-descriptors child-stdin child-stdout child-stderr)
  ;;Setup stdin.
  (begin
    (close-input-port (console-input-port))
    (px.dup2  child-stdin 0)
    (px.close child-stdin))
  ;;Setup stdout.
  (begin
    (close-output-port (console-output-port))
    (px.dup2  child-stdout 1)
    (px.close child-stdout))
  ;;Setup stderr.
  (begin
    (close-output-port (console-error-port))
    (px.dup2  child-stderr 2)
    (px.close child-stderr)))

(define (%read-stdout-from-child fd)
  ;;Read  chunks  of data  from  the  file descriptor  FD,  concatenate  them into  a
  ;;bytevector port, finally return the result as string.
  ;;
  (define-constant buflen
    ;;Let's keep it small to exercise the loop below.
    16)
  (px.fd-set-non-blocking-mode! fd)
  (receive (port extract)
      (open-bytevector-output-port)
    (let next-chunk ((buf (make-bytevector buflen 0)))
      (let ((nread (guard (E ((i/o-eagain-error? E)
			      ;;This exception  is raised when reading  from the file
			      ;;descriptor would block because  no data is available.
			      ;;We interpret this as "input finished".
			      0)
			     (else
			      (raise E)))
		     (px.read fd buf))))
	(if (positive? nread)
	    (begin
	      (put-bytevector port buf 0 nread)
	      (next-chunk buf))
	  (utf8->string (extract)))))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; eval: (put 'px.fork 'scheme-indent-function 0)
;; End:
