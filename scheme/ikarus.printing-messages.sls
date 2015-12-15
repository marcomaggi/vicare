;;;
;;;Part of: Vicare Scheme
;;;Contents: printing messages on the console
;;;Date: Wed Sep 30, 2015
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
(library (ikarus.printing-messages)
  (export
    print-stderr-message
    print-error-message
    print-verbose-message
    print-debug-message)
  (import (vicare)
    (prefix (ikarus.options)
	    options::))


(define (print-stderr-message prefix template arg*)
  (with-blocked-exceptions
      (lambda ()
	(let ((P (current-error-port)))
	  (display "vicare: " P)
	  (when prefix
	    (display prefix P))
	  (apply fprintf P template arg*)
	  (newline P)
	  (flush-output-port P)))))

(define (print-error-message template . args)
  (print-stderr-message "error: " template args))

(module (print-verbose-message)

  (define-syntax-rule (print-verbose-message . ?args)
    (when (options::print-verbose-messages?)
      (%print-verbose-message . ?args)))

  (define (%print-verbose-message template . args)
    (print-stderr-message #f template args))

  #| end of module |# )

(module (print-debug-message)

  (define-syntax-rule (print-debug-message . ?args)
    (when (options::print-debug-messages?)
      (%print-debug-message . ?args)))

  (define (%print-debug-message template . args)
    (print-stderr-message #f template args))

  #| end of module |# )


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8-unix
;; End:
