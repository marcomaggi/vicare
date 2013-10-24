;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (nausicaa language oopp) setter and getter
;;;Date: Thu Jul 28, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing classes setter and getter\n")


;;;; helpers

(define test-environ
  (environment '(except (rnrs)
			define
			lambda		case-lambda
			let		let*
			letrec		letrec*
			let-values	let*-values
			do		set!)
	       '(rename (nausicaa language oopp)
			(define/tags			define)
			(lambda/tags			lambda)
			(case-lambda/tags		case-lambda)
			(let/tags			let)
			(let*/tags			let*)
			(letrec/tags			letrec)
			(letrec*/tags			letrec*)
			(let-values/tags		let-values)
			(let*-values/tags		let*-values)
			(receive/tags			receive)
			(do/tags			do)
			(do*/tags			do*)
			(set!/tags			set!))))

(define-syntax %eval
  (syntax-rules ()
    ((_ ?form)
     (eval ?form test-environ))))

(define (debug-pretty-print thing)
  (pretty-print thing (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-write . args)
  (for-each (lambda (thing)
	      (write thing (current-error-port))
	      (display #\space (current-error-port)))
    args)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define (debug-newline thing)
  (newline (current-error-port))
  (flush-output-port (current-error-port)))

(define-syntax catch-syntax-violation
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((syntax-violation? E)
		(when ?verbose
		  (debug-write (condition-message E)
			       (syntax-violation-subform E)))
		(syntax->datum (syntax-violation-subform E)))
	       (else E))
       . ?body))))

(define-syntax catch-assertion
  (syntax-rules ()
    ((_ ?verbose . ?body)
     (guard (E ((assertion-violation? E)
		(when ?verbose
		  (debug-write (condition-message E)
			       (condition-irritants E)))
		(condition-irritants E))
	       (else E))
       . ?body))))


(parametrise ((check-test-name	'alias))

  (check	;alias of R6RS's set!
      (let ((a 1))
	(set! a 2)
	a)
    => 2)

  #t)


(parametrise ((check-test-name	'transformers))

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key)) ?val)
		   #'(<alpha>-setf ?var ?key ?val))))))

    (define (<alpha>-setf (o <alpha>) key value)
      (set! (o a) (list key value)))

    (check
  	(let (((o <alpha>) (<alpha> (1))))
  	  (set! (o[2]) 3)
  	  (o a))
      => '(2 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key0 ?key1)) ?val)
		   #'(<alpha>-setf ?var ?key0 ?key1 ?val))))))

    (define (<alpha>-setf (o <alpha>) key0 key1 value)
      (set! (o a) (list key0 key1 value)))

    (check
	(let (((o <alpha>) (<alpha> (1))))
	  (set! (o[2 3]) 4)
	  (o a))
      => '(2 3 4))

    #f)

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key0 ?key1)) ?val)
		   #'(<alpha>-setf ?var ?key0 ?key1 ?val)))))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key0 ?key1)))
		   #'(<alpha>-getf ?var ?key0 ?key1))))))

    (define (<alpha>-setf (o <alpha>) key0 key1 value)
      (set! (o a) (list key0 key1 value)))

    (define (<alpha>-getf (o <alpha>) key0 key1)
      (list (o a) key0 key1))

    (check
	(let (((o <alpha>) (<alpha> (1))))
	  (set! (o[2 3]) 4)
	  (o[5 6]))
      => '((2 3 4) 5 6))

    #f)

  #t)


(parametrise ((check-test-name	'nested))

  (let ()	;single nesting

    (define-class <alpha>
      (fields (mutable a))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key)) ?value)
		   #'(set! (?var a) (cons ?key ?value))))))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key)))
		   #'(vector ?key (?var a)))))))

    (define-class <beta>
      (fields (immutable (a <alpha>))
	      (mutable   b))
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key)) ?value)
		   #'(set! (?var b) (cons ?key ?value)))
		  ((?var ((?key0) (?key1)) ?value)
		   #'(let (((tmp <alpha>) (?var a)))
		       (set! tmp[?key1] ?value))))))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?key0)))
		   #'(vector ?key0 (?var b)))
		  ((?var ((?key0) (?key1)))
		   #'(let (((tmp <alpha>) (?var a)))
		       (tmp[?key1])))))))

      (check	;one key set, setter syntax 1
	  (let (((o <beta>) (<beta>[(<alpha>[1]) 2])))
	    (set! (o[777]) 999)
	    (o[333]))
	=> '#(333 (777 . 999)))

      (check	;one key set, setter syntax 2
	  (let (((o <beta>) (<beta>[(<alpha>[1]) 2])))
	    (set! o[777] 999)
	    (o[333]))
	=> '#(333 (777 . 999)))

      (check	;one key set, nested getter setter
	  (let (((o <beta>) (<beta>[(<alpha>[1]) 2])))
	    (set! (o a[777]) 999)
	    (o a[777]))
	=> '#(777 (777 . 999)))

      (check	;two key sets, setter syntax 1
	  (let (((o <beta>) (<beta>[(<alpha>[1]) 2])))
	    (set! (o[333][777]) 999)
	    (o[333][777]))
	=> '#(777 (777 . 999)))

      (check	;two key sets, setter syntax 2
	  (let (((o <beta>) (<beta>[(<alpha>[1]) 2])))
	    (set! o[333][777] 999)
	    (o[333][777]))
	=> '#(777 (777 . 999)))

      #f)

  (let ()	;nested vectors, single label

    #;(define-label <row>
      (setter (lambda (stx)
		((?var ((?col)) ?value)
		 #'(vector-set! ?var (- ?col 1) ?value))))
      (getter (lambda (stx)
		((?var ((?col)))
		 #'(vector-ref ?var (- ?col 1))))))

    (define-label <matrix>
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row)) ?value)
		   #'(vector-set! ?var (- ?row 1) ?value))
		  ((?var ((?row)(?col)) ?value)
		   #'(vector-set! (vector-ref ?var (- ?row 1)) (- ?col 1) ?value))
		  )))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row)))
		   #'(vector-ref ?var ?row))
		  ((?var ((?row)(?col)))
		   #'(vector-ref (vector-ref ?var (- ?row 1)) (- ?col 1)))
		  ))))

    (check
	(let (((M <matrix>) '#(#(11 12 13)	;1st row
			       #(21 22 23)	;2nd row
			       #(31 32 33)	;3rd row
			       )))
	  (M[1][1]))
      => 11)

    (check
	(let (((M <matrix>) '#(#(11 12 13)	;1st row
			       #(21 22 23)	;2nd row
			       #(31 32 33)	;3rd row
			       )))
	  (list (M[1][1]) (M[1][2]) (M[1][3])
		(M[2][1]) (M[2][2]) (M[2][3])
		(M[3][1]) (M[3][2]) (M[3][3])))
      => '(11 12 13  21 22 23  31 32 33))

    (check
	(let (((M <matrix>) (vector (vector 11 12 13)	;1st row
				    (vector 21 22 23)	;2nd row
				    (vector 31 32 33)	;3rd row
				    )))
	  (set! M[2][1] 99)
	  (set! M[3][3] 77)
	  (list (M[1][1]) (M[1][2]) (M[1][3])
		(M[2][1]) (M[2][2]) (M[2][3])
		(M[3][1]) (M[3][2]) (M[3][3])))
      => '(11 12 13  99 22 23  31 32 77))

    #f)

  (let ()	;nested vectors, multiple labels

    (define-label <row>
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?col)) ?value)
		   #'(vector-set! ?var (- ?col 1) ?value)))))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?col)))
		   #'(vector-ref ?var (- ?col 1)))))))

    (define-label <matrix>
      (setter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row)) ?value)
		   #'(vector-set! ?var (- ?row 1) ?value))
		  ((?var ((?row)(?col)) ?value)
		   #'(let (((R <row>) (vector-ref ?var (- ?row 1))))
		       (set! R[?col] ?value)))
		  )))
      (getter (lambda (stx)
		(syntax-case stx ()
		  ((?var ((?row)))
		   #'(vector-ref ?var ?row))
		  ((?var ((?row)(?col)))
		   #'(let (((R <row>) (vector-ref ?var (- ?row 1))))
		       (R[?col])))
		  ))))

    (check
	(let (((M <matrix>) '#(#(11 12 13)	;1st row
			       #(21 22 23)	;2nd row
			       #(31 32 33)	;3rd row
			       )))
	  (M[1][1]))
      => 11)

    (check
	(let (((M <matrix>) '#(#(11 12 13)	;1st row
			       #(21 22 23)	;2nd row
			       #(31 32 33)	;3rd row
			       )))
	  (list (M[1][1]) (M[1][2]) (M[1][3])
		(M[2][1]) (M[2][2]) (M[2][3])
		(M[3][1]) (M[3][2]) (M[3][3])))
      => '(11 12 13  21 22 23  31 32 33))

    (check
	(let (((M <matrix>) (vector (vector 11 12 13) ;1st row
				    (vector 21 22 23) ;2nd row
				    (vector 31 32 33) ;3rd row
				    )))
	  (set! M[2][1] 99)
	  (set! M[3][3] 77)
	  (list (M[1][1]) (M[1][2]) (M[1][3])
		(M[2][1]) (M[2][2]) (M[2][3])
		(M[3][1]) (M[3][2]) (M[3][3])))
      => '(11 12 13  99 22 23  31 32 77))

    #f)

  #t)


(parametrise ((check-test-name	'builtin))

;;; builtin getter/setter

  (check	;vector
      (let (((o <vector>) (vector 0 1 2 3 4)))
	(set! o[2] #\c)
	(o[2]))
    => #\c)

  (check	;bytevector
      (let (((o <bytevector-u8>) (bytevector-copy '#vu8(0 1 2 3))))
  	(set! (o[2]) 10)
	(o[2]))
    => 10)

  (check	;hashtable
      (let (((o <hashtable>) (make-eq-hashtable)))
  	(set! o['ciao] 10)
  	(list (o['ciao])
	      (o['hello])
	      (o['hello] ['salut])))
    => `(10 ,(void) salut))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8
;; End:
