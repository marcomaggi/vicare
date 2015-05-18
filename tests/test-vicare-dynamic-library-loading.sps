;;;
;;;Part of: Vicare Scheme
;;;Contents: tests dynamic library loading
;;;Date: Mon May 18, 2015
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


#!r6rs
(import (vicare)
  (vicare checks)
  (prefix (vicare libraries) libs.))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: dynamic library loading\n")


(parametrise ((check-test-name	'base))

  (check
      (internal-body

	(library (demo-01)
	  (export fun var)
	  (import (rnrs))
	  (define (fun a b)
	    (+ a b))
	  (define var 123))

	(import (demo-01))

	(let* ((lib (libs.library-dynamic-load-and-intern '(demo-01)))
	       (fun (libs.library-dynamic-retrieve lib 'fun))
	       (var (libs.library-dynamic-retrieve lib 'var)))
	  (values (fun 1 2) var)))
    => 3 123)

  (check
      (internal-body

	(define-values (pregexp-match)
	  (let ((lib (libs.library-dynamic-load-and-intern '(vicare pregexp))))
	    (values (libs.library-dynamic-retrieve lib 'pregexp-match))))

	(pregexp-match "[a-z]+" "ciao hello ciao"))
    => '("ciao"))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
