;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (ikarus chars)
  (export
    char->integer	integer->char
    char=?
    char<?		char<=?
    char>?		char>=?)
  (import (except (ikarus)
		  char->integer		integer->char
		  char=?
		  char<?		char<=?
		  char>?		char>=?)
    (vicare syntactic-extensions)
    (prefix (vicare unsafe-operations)
	    unsafe.))

;;;; arguments validation

(define-argument-validation (fixnum-in-range who obj)
  (and (fixnum? obj)
       (unsafe.fx>= obj 0)
       (unsafe.fx<= obj #x10FFFF))
  (assertion-violation who "expected fixnum in range [0, #x10FFFF] as argument" obj))

(define-argument-validation (char who obj)
  (char? obj)
  (assertion-violation who "expected character as argument" obj))

(define-argument-validation (list-of-chars who obj)
  (for-all char? obj)
  (assertion-violation who "expected character as argument"
		       (exists (lambda (x) (and (not (char? x)) x)) obj)))


(define (integer->char N)
  ;;Defined  by  R6RS.   N must  be  a  Unicode  scalar value,  i.e.,  a
  ;;non-negative  exact integer  object  in [0,  #xD7FF] union  [#xE000,
  ;;#x10FFFF].
  ;;
  ;;For a  Unicode scalar value N, INTEGER->CHAR  returns its associated
  ;;character.
  ;;
  (define who 'integer->char)
  (with-arguments-validation (who)
      ((fixnum-in-range	N))
    (cond ((unsafe.fx<= N #xD7FF)
	   (unsafe.fixnum->char N))
	  ((unsafe.fx< N #xE000)
	   (assertion-violation who "integer does not have a unicode representation" N))
	  (else ;(assert (unsafe.fx<= N #x10FFFF))
	   (unsafe.fixnum->char N)))))

(define (char->integer ch)
  ;;Defined  by  R6RS.  Given  a  character,  CHAR->INTEGER returns  its
  ;;Unicode scalar value as an exact integer object.
  ;;
  (with-arguments-validation (char->integer)
      ((char  ch))
    (unsafe.char->fixnum ch)))


(define-syntax define-comparison
  (syntax-rules ()
    ((_ ?name ?unsafe-op)
     (define ?name
       (case-lambda
	((ch1 ch2)
	 (define who '?name)
	 (with-arguments-validation (who)
	     ((char  ch1)
	      (char  ch2))
	   (?unsafe-op ch1 ch2)))

	((ch1 ch2 ch3)
	 (define who '?name)
	 (with-arguments-validation (who)
	     ((char  ch1)
	      (char  ch2)
	      (char  ch3))
	   (and (?unsafe-op ch1 ch2)
		(?unsafe-op ch2 ch3))))

	((ch1 . chars)
	 (define who '?name)
	 (with-arguments-validation (who)
	     ((char  ch1))
	   (let next-char ((ch1    ch1)
			   (chars  chars))
	     (if (null? chars)
		 #t
	       (let ((ch2 (unsafe.car chars)))
		 (with-arguments-validation (who)
		     ((char  ch2))
		   (if (?unsafe-op ch1 ch2)
		       (next-char ch2 (unsafe.cdr chars))
		     (with-arguments-validation (who)
			 ((list-of-chars (unsafe.cdr chars)))
		       #f))))))))
	))
     )))

(define-comparison char=?	unsafe.char=)
(define-comparison char<?	unsafe.char<)
(define-comparison char<=?	unsafe.char<=)
(define-comparison char>?	unsafe.char>)
(define-comparison char>=?	unsafe.char>=)


;;;; done

)


(library (ikarus system chars)
  (export $char= $char->fixnum $fixnum->char)
  (import (ikarus))
  (define $char=	char=?)
  (define $char->fixnum char->integer)
  (define $fixnum->char integer->char))

;;; end of file
