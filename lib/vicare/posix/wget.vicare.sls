;;;
;;;Part of: Vicare Scheme
;;;Contents: download files using wget
;;;Date: Sun Jan 25, 2015
;;;
;;;Abstract
;;;
;;;	This library  defines an  API to  download files  with the  locally installed
;;;	program "wget".
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
(library (vicare posix wget)
  (options tagged-language)
  (export wget)
  (import (vicare)
    (vicare expander tags)
    (prefix (vicare posix) px.))


(define (wget . opts)
  (px.fork-with-binary-ports
    ;;Here we are in the parent.
    (lambda (child-pid stdin-port stdout-port stderr-port)
      (unwind-protect
	  (let ((status (px.waitpid child-pid 0)))
	    (if (and (px.WIFEXITED status)
		     (zero? (px.WEXITSTATUS status)))
		(let ((out (get-bytevector-all stdout-port))
		      (err (get-bytevector-all stderr-port)))
		  ;;OUT and ERR can be bytevectors or EOFs.
		  (values (if (bytevector? out) (utf8->string out) out)
			  (if (bytevector? err) (utf8->string err) err)))
	      (error __who__ "wget process exited abnormally" status)))
	(close-output-port stdin-port)
	(close-input-port  stdout-port)
	(close-input-port  stderr-port)))
    ;;Here we are in the child.
    (lambda ()
      (guard (E (else
		 (print-condition E)
		 (exit 1)))
	(px.execvp "wget" `("wget" . ,opts))))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8
;; eval: (put 'px.fork-with-binary-ports 'scheme-indent-function 0)
;; End:
