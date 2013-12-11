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
    (vicare unsafe operations)
    (vicare arguments validation)
    (ikarus system $stack))


;;;; constants

;;Limit artificially set; notice that it  is the maximum number of items
;;in the  last argument of APPLY,  not the maximum number  of arguments.
;;This limit  attempts to  mitigate the risk  of Scheme  stack overflow,
;;which is be detected at run time.
;;
;;As comparison:  LispWorks for Unix  sets this value to  300, LispWorks
;;for Windows and LispWorks for Linux set this value to 255.
;;
(define-constant CALL-ARGUMENTS-LIMIT 8192)


;;;; helpers

(define who 'apply)

(define (%list-of-arguments? ls)
  ;;This  function is  LENGTH  from "ikarus.lists.ss",  modified to  use
  ;;APPLY as "&who" and to check maximum length.
  ;;
  (define (%error message)
    (procedure-argument-violation who message ls))
  (define (%race h t ls n)
    (cond (($fx< CALL-ARGUMENTS-LIMIT n)
	   #f)
	  ((pair? h)
	   (let ((h ($cdr h)))
	     (if (pair? h)
		 (if (not (eq? h t))
		     (%race ($cdr h) ($cdr t) ls ($fx+ n 2))
		   (%error "circular list is invalid as argument"))
	       (if (null? h)
		   ($fx+ n 1)
		 (%error "improper list is invalid as argument")))))
	  ((null? h)
	   n)
	  (else
	   (%error "expected proper list as argument"))))
  (%race ls ls ls 0))


;;;; arguments validation

(define-argument-validation (list-of-arguments who obj)
  (%list-of-arguments? obj)
  (raise
   (condition (make-implementation-restriction-violation)
	      (make-who-condition who)
	      (make-message-condition
	       (string-append "expected proper list as argument with maximum length "
			      (number->string CALL-ARGUMENTS-LIMIT)))
	      (make-irritants-condition (list obj)))))


(case-define apply
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
  ;;NOTE In  case of  last argument being  a list too  long: we  want to
  ;;raise   an   "&implementation-restriction-violation",  so   we   use
  ;;"with-arguments-validation"   rather   than    the   predicates   of
  ;;CASE-DEFINE*.
  ;;
  ((f ls)
   (with-arguments-validation (who)
       ((procedure		f)
	(list-of-arguments	ls))
     ($$apply f ls)))

  ((f a0 ls)
   (with-arguments-validation (who)
       ((procedure		f)
	(list-of-arguments	ls))
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
   (define (%fix-and-go f a0 a1 ls p d)
     (if (null? ($cdr d))
	 (let ((last ($car d)))
	   ($set-cdr! p last)
	   (with-arguments-validation (who)
	       ((list-of-arguments	last))
	     ($$apply f a0 a1 ls)))
       (%fix-and-go f a0 a1 ls d ($cdr d))))
   (%fix-and-go f a0 a1 ls ls ($cdr ls)))
  #| end of case-define |# )


;;;; done

)

;;; end of file
