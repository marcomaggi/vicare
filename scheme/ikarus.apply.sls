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
  (import (except (vicare) apply)
    (only (vicare system $stack)
	  $$apply))


;;;; helpers

(module (list-of-arguments?)

  (define (list-of-arguments? ls)
    ;;This function is LENGTH from "ikarus.lists.ss", modified to use APPLY as "&who"
    ;;and to check maximum length.
    ;;
    (%race ls ls ls 0))

  (define (%race h t ls n)
    (cond ((fx< CALL-ARGUMENTS-LIMIT n)
	   (raise
	    (condition (%make-base-condition-object)
		       (make-irritants-condition (list ls)))))
	  ((pair? h)
	   (let ((h (cdr h)))
	     (if (pair? h)
		 (if (not (eq? h t))
		     (%race (cdr h) (cdr t) ls (fx+ n 2))
		   (%error ls "circular list is invalid as argument"))
	       (if (null? h)
		   (fxadd1 n)
		 (%error ls "improper list is invalid as argument")))))
	  ((null? h)
	   n)
	  (else
	   (%error ls "expected proper list as argument"))))

  (define (%error ls message)
    (procedure-argument-violation 'apply message #f 'list-of-arguments? ls))

  ;;Limit artificially set; notice that it is the maximum number of items in the last
  ;;argument of APPLY,  not the maximum number of arguments.   This limit attempts to
  ;;mitigate the risk of Scheme stack overflow, which is be detected at run time.
  ;;
  ;;As comparison: LispWorks  for Unix sets this value to  300, LispWorks for Windows
  ;;and LispWorks for Linux set this value to 255.
  ;;
  (define-constant CALL-ARGUMENTS-LIMIT 8192)

  (define %make-base-condition-object
    (let ((C #f))
      (lambda ()
	(or C (condition (make-implementation-restriction-violation)
			 (make-who-condition 'apply)
			 (make-message-condition
			  (string-append "expected proper list as argument with maximum length "
					 (number->string CALL-ARGUMENTS-LIMIT))))))))

  #| end of module |# )


(case-define* apply
  ;;Defined by R6RS.  ARGS  must be a list.  PROC should accept  N arguments, where N
  ;;is number of arguments A plus the length of ARGS.
  ;;
  ;;The APPLY procedure calls PROC with the elements of the list:
  ;;
  ;;   (append (list A ...) ARGS)
  ;;
  ;;as the actual arguments.
  ;;
  ;;If a call to APPLY  occurs in a tail context, the call to PROC  is also in a tail
  ;;context.
  ;;
  ;;NOTE  In case  of last  argument  being a  list too  long:  we want  to raise  an
  ;;"&implementation-restriction-violation",  so   the  predicate  LIST-OF-ARGUMENTS?
  ;;takes care itself of raising an exception.
  ;;
  ;;NOTE  About  "$$apply".  It  is  a  special  directive  for the  compiler,  which
  ;;recognises  the symbol  "$$apply"  in the  core  language.  It  is  not a  proper
  ;;primitive operation implemented with DEFINE-PRIMOP; rather the compiler generates
  ;;assembly code to jump to the predefined assembly routine "SL_apply".
  ;;
  (({proc procedure?} {ls list-of-arguments?})
   ($$apply proc ls))

  (({proc procedure?} a0 {ls list-of-arguments?})
   ($$apply proc a0 ls))

  (({proc procedure?} a0 a1 {ls list-of-arguments?})
   ($$apply proc a0 a1 ls))

  (({proc procedure?} a0 a1 . ls)
   ;;This clause is applied only if there are 3 or more arguments, so we know that LS
   ;;is a pair.
   ;;
   ;;Notice that LS is a list of arguments terminated by a nested list of arguments:
   ;;
   ;;   (a2 a3 ... (An An+1 An+2 ...))
   ;;
   ;;so we have to flatten it to:
   ;;
   ;;   (a2 a3 ... An An+1 An+2 ...)
   ;;
   ;;To perform this flattening, we see LS as:
   ;;
   ;;   (a2 a3 ... . ((An An+1 An+2 ...)))
   ;;
   ;;and we mutate LS replacing:
   ;;
   ;;   ((An An+1 An+2 ...))
   ;;
   ;;with:
   ;;
   ;;   (An An+1 An+2 ...)
   ;;
   (let loop ((next-pair  ls)
	      (tail-pair  (cdr ls)))
     ;;Here we see LS as:
     ;;
     ;;   (a3 a4 ... next-pair)
     ;;
     ;;where:
     ;;
     ;;   next-pair == (an-1 . tail-pair)
     ;;
     ;;If TAIL-PAIR is:
     ;;
     ;;   (an . (an+1 ... . ((An An+1 An+2 ...))))
     ;;
     ;;we loop again; if TAIL-PAIR is:
     ;;
     ;;   ((An An+1 An+2 ...) . ())
     ;;
     ;;we do the flattening by mutating NEXT-PAIR to:
     ;;
     ;;   (an-1 . (An An+1 An+2 ...))
     ;;
     (let ((DD (cdr tail-pair)))
       (if (pair? DD)
	   (loop tail-pair DD)
	 (let ((args (car tail-pair)))
	   (set-cdr! next-pair args)
	   (assert (list-of-arguments? args))
	   ($$apply proc a0 a1 ls))))))

  #| end of CASE-DEFINE* |# )


;;;; done

#| end of library |# )

;;; end of file
