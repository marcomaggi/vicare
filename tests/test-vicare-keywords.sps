;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for keyword objects
;;;Date: Sat Mar 10, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (vicare keywords)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare keywords\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))


(parametrise ((check-test-name	'objects))

  (check
      (keyword? 123)
    => #f)

  (check
      (let ((K (symbol->keyword 'ciao)))
	(keyword? K))
    => #t)

  (check
      (keyword->symbol (symbol->keyword 'ciao))
    => 'ciao)

;;; --------------------------------------------------------------------

  (check
      (keyword=? (symbol->keyword 'ciao)
		 (symbol->keyword 'ciao))
    => #t)

  (check
      (keyword=? (symbol->keyword 'ciao)
		 (symbol->keyword 'hello))
    => #f)

  (check
      (let ((K (symbol->keyword 'ciao)))
	(keyword=? K K))
    => #t)

  (check
      (keyword=? (symbol->keyword 'ciao)
		 'ciao)
    => #f)

  (check
      (keyword=? 'ciao
		 (symbol->keyword 'ciao))
    => #f)

;;; --------------------------------------------------------------------
;;; EQ? comparison

  (check
      (eq? (symbol->keyword 'ciao)
	   (symbol->keyword 'ciao))
    => #f)

  (check
      (eq? (symbol->keyword 'ciao)
	   (symbol->keyword 'hello))
    => #f)

  (check
      (let ((K (symbol->keyword 'ciao)))
	(eq? K K))
    => #t)

  (check
      (eq? (symbol->keyword 'ciao)
	   'ciao)
    => #f)

  (check
      (eq? 'ciao
	   (symbol->keyword 'ciao))
    => #f)

;;; --------------------------------------------------------------------
;;; EQV? comparison

  (check
      (eqv? (symbol->keyword 'ciao)
	    (symbol->keyword 'ciao))
    => #t)

  (check
      (eqv? (symbol->keyword 'ciao)
	    (symbol->keyword 'hello))
    => #f)

  (check
      (let ((K (symbol->keyword 'ciao)))
	(eqv? K K))
    => #t)

  (check
      (eqv? (symbol->keyword 'ciao)
	    'ciao)
    => #f)

  (check
      (eqv? 'ciao
	    (symbol->keyword 'ciao))
    => #f)

;;; --------------------------------------------------------------------
;;; EQUAL? comparison

  (check
      (equal? (symbol->keyword 'ciao)
	      (symbol->keyword 'ciao))
    => #t)

  (check
      (equal? (symbol->keyword 'ciao)
	      (symbol->keyword 'hello))
    => #f)

  (check
      (let ((K (symbol->keyword 'ciao)))
	(equal? K K))
    => #t)

  (check
      (equal? (symbol->keyword 'ciao)
	      'ciao)
    => #f)

  (check
      (equal? 'ciao
	      (symbol->keyword 'ciao))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (keyword-hash (symbol->keyword 'ciao))
    => (symbol-hash 'ciao))

;;; --------------------------------------------------------------------

  (check
      (let-values (((port getter)
		    (open-string-output-port)))
	(display (symbol->keyword 'ciao) port)
	(getter))
    => "#:ciao")

  #t)


(parametrise ((check-test-name	'arguments))

  (check	;options with arguments
      (let-keywords '(#:a 1 #:b 2 #:d 4) args #f
	((with-argument a #\a #:a)
	 (with-argument b #\b #:b)
	 (with-argument c #\c #:c)
	 (with-argument d #\d #:d))
	(list a b c d args))
    => '(1 2 #\c 4 ()))

  (check	;options without arguments
      (let-keywords '(#:a #:b #:d 4) args #f
	((without-argument a #\a #:a #\A)
	 (without-argument b #\b #:b #\B)
	 (without-argument c #\c #:c #\C)
	 (with-argument d #\d #:d))
	(list a b c d args))
    => '(#\A #\B #\c 4 ()))

  (check	;options with arguments, leftover arguments
      (let-keywords '(#:a 1 ciao #:b 2 hello #:d 4) args #f
	((with-argument a #\a #:a)
	 (with-argument b #\b #:b)
	 (with-argument c #\c #:c)
	 (with-argument d #\d #:d))
	(list a b c d args))
    => '(1 2 #\c 4 (ciao hello)))

  (check	;options without arguments, leftover arguments
      (let-keywords '(#:a ciao #:b hello #:d 4) args #f
	((without-argument a #\a #:a #\A)
	 (without-argument b #\b #:b #\B)
	 (without-argument c #\c #:c #\C)
	 (with-argument d #\d #:d))
	(list a b c d args))
    => '(#\A #\B #\c 4 (ciao hello)))

;;; --------------------------------------------------------------------
;;; allow unknown

  (check	;no options
      (let-keywords '(#:a 1 #:b 2 #:d 4) args #t
	()
	args)
    => '(#:a 1 #:b 2 #:d 4))

  (check	;options with arguments, allow unknown
      (let-keywords '(#:a 1 #:b 2 #:d 4 #:ciao 123) args #t
	((with-argument a #\a #:a)
	 (with-argument b #\b #:b)
	 (with-argument c #\c #:c)
	 (with-argument d #\d #:d))
	(list a b c d args))
    => '(1 2 #\c 4 (#:ciao 123)))

;;; --------------------------------------------------------------------
;;; errors

  (check	;unknown option
      (catch #f
	(let-keywords '(#:a) args #f
	  ()
	  args))
    => '(#:a (#:a)))

  (check	;option requires argument
      (catch #f
	(let-keywords '(#:a #:b 123) args #t
	  ((with-argument a 1 #:a)
	   (with-argument b 2 #:b))
	  args))
    => '(#:a #:b (#:a #:b 123)))

  #t)


(parametrise ((check-test-name	'syntaxes))

  (check
      (let ((a 1) (b 2) (c 3) (d 4))
	(let-keywords '() args #f
	  ((with-argument a a #:a)
	   (with-argument b b #:b)
	   (with-argument c c #:c)
	   (with-argument d d #:d))
	  (list a b c d args)))
    => '(1 2 3 4 ()))

  (check
      (let ((a 1) (b 2) (c 3) (d 4))
	(let*-keywords '() args #f
	  ((with-argument a a #:a)
	   (with-argument b a #:b)
	   (with-argument c b #:c)
	   (with-argument d c #:d))
	  (list a b c d args)))
    => '(1 1 1 1 ()))

  (check
      (let ((A 1) (B 2) (C 3) (D 4))
	(letrec-keywords '() args #f
	  ((with-argument a A #:a)
	   (with-argument b B #:b)
	   (with-argument c C #:c)
	   (with-argument d D #:d))
	  (list a b c d args)))
    => '(1 2 3 4 ()))

  (check
      (let ((A 1) (B 2) (C 3) (D 4))
	(letrec*-keywords '() args #f
	  ((with-argument a A #:a)
	   (with-argument b B #:b)
	   (with-argument c C #:c)
	   (with-argument d D #:d))
	  (list a b c d args)))
    => '(1 2 3 4 ()))

  #t)


(parametrise ((check-test-name	'examples))

  (check
      (let-keywords '() args #f
	((without-argument verbosity 0 #:verbose (+ 1 verbosity)))
	verbosity)
    => 0)

  (check
      (let-keywords '(#:verbose) args #f
	((without-argument verbosity 0 #:verbose (+ 1 verbosity)))
	verbosity)
    => 1)

  (check
      (let-keywords '(#:verbose #:verbose #:verbose) args #f
	((without-argument verbosity 0 #:verbose (+ 1 verbosity)))
	verbosity)
    => 3)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
