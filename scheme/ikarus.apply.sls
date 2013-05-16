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

(library (ikarus apply)
  (export apply)
  (import (except (ikarus) apply)
    (vicare language-extensions syntaxes)
    (vicare unsafe operations)
    (ikarus system $stack))


;;;; constants

;;Limit artificially set; notice that  it is the maximum number of items
;;in the  last argument of APPLY,  not the maximum  number of arguments.
;;It is known from Issue 7 that a value greater than or equal to 1057112
;;makes Vicare (and Ikarus) crash (Marco Maggi; Oct 28, 2011).
;;
;;As comparison:  LispWorks for Unix  sets this value to  300, LispWorks
;;for Windows and LispWorks for Linux set this value to 255.
;;
(define CALL-ARGUMENTS-LIMIT 8192)


;;;; helpers

(define (%length-or-raise ls)
  ;;This  function is  LENGTH  from "ikarus.lists.ss",  modified to  use
  ;;APPLY as "&who" and to check maximum length.
  ;;
  (define who 'apply)
  (define (%race h t ls n)
    (cond (($fx< CALL-ARGUMENTS-LIMIT n)
	   #f)
	  ((pair? h)
	   (let ((h ($cdr h)))
	     (if (pair? h)
		 (if (not (eq? h t))
		     (%race ($cdr h) ($cdr t) ls ($fx+ n 2))
		   (assertion-violation who "circular list is invalid as argument" ls))
	       (if (null? h)
		   ($fx+ n 1)
		 (assertion-violation who "improper list is invalid as argument" ls)))))
	  ((null? h)
	   n)
	  (else
	   (assertion-violation who "expected proper list as argument" ls))))
  (%race ls ls ls 0))


;;;; arguments validation

(define-argument-validation (procedure who obj)
  (procedure? obj)
  (assertion-violation who "expected procedure as argument" obj))

(define-argument-validation (list who obj)
  (%length-or-raise obj)
  (assertion-violation who
    (string-append "expected proper list as argument with maximum length "
		   (number->string CALL-ARGUMENTS-LIMIT))
    obj))


(define who 'apply)

(define (fixandgo f a0 a1 ls p d)
  (if (null? ($cdr d))
      (let ((last ($car d)))
	($set-cdr! p last)
	(with-arguments-validation (who)
	    ((procedure f ls)
	     (list	last))
	  ($$apply f a0 a1 ls)))
    (fixandgo f a0 a1 ls d ($cdr d))))

(define apply
  ;;Defined  by  R6RS.   LS  must  be  a list.   PROC  should  accept  N
  ;;arguments, where N is number of arguments A plus the length of LS.
  ;;
  ;;The APPLY procedure calls PROC with the elements of the list:
  ;;
  ;;   (append (list A ...) LS)
  ;;
  ;;as the actual arguments.
  ;;
  ;;If a  call to APPLY occurs  in a tail  context, the call to  PROC is
  ;;also in a tail context.
  ;;
  (case-lambda
   ((f ls)
    (with-arguments-validation (who)
	((procedure	f)
	 (list		ls))
      (values)
      ($$apply f ls)))

   ((f a0 ls)
    (with-arguments-validation (who)
	((procedure f)
	 (list      ls))
      ($$apply f a0 ls)))

   ((f a0 a1 ls)
    (with-arguments-validation (who)
	((procedure f)
	 (list      ls))
      ($$apply f a0 a1 ls)))

   ((f a0 a1 . ls)
    ;;Notice that LS is a list  of arguments terminated by a nested list
    ;;of arguments:
    ;;
    ;;  (a2 a3 a4 ... (An An+1 An+2 ...))
    ;;
    ;;so we have to flatten it to:
    ;;
    ;;  (a2 a3 a4 ... An An+1 An+2 ...)
    ;;
    (fixandgo f a0 a1 ls ls ($cdr ls)))
   ))


;;;; done

)

;;; end of file
