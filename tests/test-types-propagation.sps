;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for type propagation through built-in syntaxes
;;;Date: Sat Apr 30, 2016
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-types-rhs-type-propagation)
  (options typed-language)
  (import (vicare)
    (prefix (vicare expander) expander::)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare typed language: type propagation through built-in syntaxes\n")


;;;; helpers

(define-syntax doit
  (syntax-rules (=>)
    ((_ ?expr => ?expected)
     (check
	 (.syntax-object (type-of ?expr))
       (=> syntax=?)
       (syntax ?expected)))
    ))


(parametrise ((check-test-name	'let-plain))

  (doit (let ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (let ((A "ciao"))
	  A)
	=> (<string>))

  (doit (let ((A "ciao"))
	  (let ((B A))
	    B))
	=> (<string>))

  (doit (let ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (let ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (let ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (let (({a <fixnum>} 1)
	      ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let (({a <positive-fixnum>} 1)
	      ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (let (({a <positive-fixnum>} 1)
	      ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let ((a 1)
	      (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'let-star))

  (doit (let* ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (let* ((A "ciao"))
	  A)
	=> (<string>))

  (doit (let* ((A "ciao"))
	  (let* ((B A))
	    B))
	=> (<string>))

  (doit (let* ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (let* ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (let* ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (let* (({a <fixnum>} 1)
	       ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let* (({a <positive-fixnum>} 1)
	       ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (let* (({a <positive-fixnum>} 1)
	       ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (let* ((a 1)
	       (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'letrec))

  (doit (letrec ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec ((A "ciao"))
	  A)
	=> (<string>))

  (doit (letrec ((A "ciao"))
	  (letrec ((B A))
	    B))
	=> (<string>))

  (doit (letrec ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (letrec ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (letrec (({a <fixnum>} 1)
		 ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec (({a <positive-fixnum>} 1)
		 ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (letrec (({a <positive-fixnum>} 1)
		 ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec ((a 1)
		 (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'letrec*))

  (doit (letrec* ((A 1))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec* ((A "ciao"))
	  A)
	=> (<string>))

  (doit (letrec* ((A "ciao"))
	  (letrec* ((B A))
	    B))
	=> (<string>))

  (doit (letrec* ((A (fxadd1 1)))
	  A)
	=> (<positive-fixnum>))

  (doit (letrec* ((A (fx+ 1 -2)))
	  A)
	=> (<fixnum>))

  (doit (letrec* ((A (cast-signature (<top>) 1)))
	  A)
	=> (<top>))

;;; --------------------------------------------------------------------

  (doit (letrec* (({a <fixnum>} 1)
		  ({b <fixnum>} 2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec* (({a <positive-fixnum>} 1)
		  ({b <positive-fixnum>} 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (doit (letrec* (({a <positive-fixnum>} 1)
		  ({b <negative-fixnum>} -2))
	  (fx+ a b))
	=> (<fixnum>))

  (doit (letrec* ((a 1)
		  (b 2))
	  (fx+ a b))
	=> (<positive-fixnum>))

  (void))


(parametrise ((check-test-name	'call-with-values))

  (doit (call-with-values
	    (lambda () 1)
	  (lambda ({_ <fixnum>} {a <fixnum>})
	    (add1 a)))
	=> (<fixnum>))

  (doit (call-with-values
	    (lambda () 1)
	  (lambda (a)
	    (add1 a)))
	=> (<number>))

  (void))


(parametrise ((check-test-name	'lambda))

  (doit ((lambda ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (doit ((lambda ({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	   (fx+ a b))
	 1 -2)
	=> (<fixnum>))

  (doit ((lambda ({_ <fixnum>} a b)
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((lambda ({a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (void))


(parametrise ((check-test-name	'named-lambda))

  (doit ((named-lambda me ({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (doit ((named-lambda me ({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	   (fx+ a b))
	 1 -2)
	=> (<fixnum>))

  (doit ((named-lambda me ({_ <fixnum>} a b)
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((named-lambda me ({a <fixnum>} {b <fixnum>})
	   (fx+ a b))
	 1 2)
	=> (<fixnum>))

  (void))


(parametrise ((check-test-name	'case-lambda))

  (doit ((case-lambda
	   (({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	    (fx+ a b)))
	 1 -2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <flonum>} a b c)
	    (fl+ a b))
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} {b <flonum>} {c <flonum>})
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} . {b* (list-of <flonum>)})
	    (apply fl+ a b*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((case-lambda
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} . {fl* (list-of <flonum>)})
	    (apply fl+ fl*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <string>} {b <string>} {c <string>})
	    (string-append a b c)))
	 1 2)
	=> (<fixnum>))

  (doit ((case-lambda
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <string>} {b <string>} {c <string>})
	    (string-append a b c)))
	 "1" "2" "3")
	=> (<string>))

  (void))


(parametrise ((check-test-name	'named-case-lambda))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} {a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} {a <positive-fixnum>} {b <negative-fixnum>})
	    (fx+ a b)))
	 1 -2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <flonum>} a b c)
	    (fl+ a b))
	   (({_ <fixnum>} a b)
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} a b c)
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} {b <flonum>} {c <flonum>})
	    (fl+ a b)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} {a <flonum>} . {b* (list-of <flonum>)})
	    (apply fl+ a b*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

  (doit ((named-case-lambda me
	   (({_ <fixnum>} a b)
	    (fx+ a b))
	   (({_ <flonum>} . {fl* (list-of <flonum>)})
	    (apply fl+ fl*)))
	 1.0 2.0 3.0)
	=> (<flonum>))

;;; --------------------------------------------------------------------
;;; type propagation: automatic inference of return values's type signature

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <string>} {b <string>} {c <string>})
	    (string-append a b c)))
	 1 2)
	=> (<fixnum>))

  (doit ((named-case-lambda me
	   (({a <fixnum>} {b <fixnum>})
	    (fx+ a b))
	   (({a <string>} {b <string>} {c <string>})
	    (string-append a b c)))
	 "1" "2" "3")
	=> (<string>))

  (void))


(parametrise ((check-test-name	'receive))

  (doit (receive (a)
	    1
	  123)
	=> (<positive-fixnum>))

  (doit (receive (a b)
	    (values 1 2)
	  (values 123 "ciao"))
	=> (<positive-fixnum> <string>))

;;; --------------------------------------------------------------------

  (doit (receive ({a <fixnum>})
	    1
	  a)
	=> (<fixnum>))

  (doit (receive ({a <positive-fixnum>} {b <string>})
	    (values 123 "ciao")
	  (values a b))
	=> (<positive-fixnum> <string>))

;;; --------------------------------------------------------------------

  (doit (receive (a)
	    1
	  a)
	=> (<top>))

  (doit (receive (a b)
	    (values 123 "ciao")
	  (values a b))
	=> (<top> <top>))

  (void))


(parametrise ((check-test-name	'receive-and-return))

  (doit (receive-and-return (a)
	    1
	  (list a))
	=> (<top>))

  (doit (receive-and-return (a b)
	    (values 1 2)
	  (list a b))
	=> (<top> <top>))

;;; --------------------------------------------------------------------

  (doit (receive-and-return ({a <fixnum>})
	    1
	  (void))
	=> (<fixnum>))

  (doit (receive-and-return ({a <positive-fixnum>} {b <string>})
	    (values 123 "ciao")
	  (list a b))
	=> (<positive-fixnum> <string>))

;;; --------------------------------------------------------------------

  (doit (receive-and-return (a)
	    1
	  (void))
	=> (<top>))

  (doit (receive-and-return (a b)
	    (values 123 "ciao")
	  (list a b))
	=> (<top> <top>))

  (void))


(parametrise ((check-test-name	'cond))

  (doit (cond ((read)	1)
	      ((read)	2)
	      (else	3))
	=> (<positive-fixnum>))

  (doit (cond ((read)	1)
	      ((read)	2)
	      (else	#f))
	=> ((or <positive-fixnum> <false>)))

  (doit (cond ((read)	1)
	      ((read)	2.0)
	      (else	#f))
	=> ((or <positive-fixnum> <positive-flonum> <false>)))

  (void))


(parametrise ((check-test-name	'case))

  (doit (case (read)
	  ((1)		1)
	  ((ciao)	2)
	  (else		3))
	=> (<positive-fixnum>))

  (doit (case (read)
	  ((1)		1)
	  ((ciao)	'ciao)
	  (else		#f))
	=> ((or <positive-fixnum> <false> (enumeration ciao))))

  ;;Without ELSE clause: if no datum matches, CASE returns void.
  ;;
  (doit (case (read)
	  ((1)	1)
	  ((2)	"ciao"))
	=> (<void>))

  ;;Special case of type propagation from the datum to the lambda.
  ;;
  (doit (case (read)
	  ((1)	=> (lambda (x) (list 'hey x)))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <string>
		(list (enumeration hey) <positive-fixnum>)
		<false>)))

  (doit (case (read)
	  ((1)	=> (lambda ({x <positive-fixnum>}) x))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <string> <positive-fixnum> <false>)))

  (doit (case (read)
	  ((1)	=> (lambda (x) 999))
	  ((2)	"ciao")
	  (else	#f))
	=> ((or <string> <positive-fixnum> <false>)))

  (void))


(parametrise ((check-test-name	'and))

  (doit (and)
	=> (<true>))

  (doit (and 1 2 3)
	=> (<positive-fixnum>))

  (doit (and 1 "2" 3)
	=> (<positive-fixnum>))

  (doit (and 1 2 "3")
	=> (<string>))

  (doit (and #f 2 3)
	=> (<false>))

  (doit (and 1 #f 3)
	=> (<false>))

  (doit (and 1 2 #f)
	=> (<false>))

  (doit (and 1 (and 2.1 2.2) 3)
	=> (<positive-fixnum>))

  (doit (and 1 2 (and 3.1 3.2))
	=> (<positive-flonum>))

  (void))


(parametrise ((check-test-name	'or))

  (doit (or)
	=> (<false>))

  ;;This is expanded to:
  ;;
  ;;   (quote 1)
  ;;
  (doit (or 1 2 "3")
	=> (<positive-fixnum>))

  ;;This is expanded to:
  ;;
  ;;   (begin
  ;;     #f
  ;;     (quote 2))
  ;;
  (doit (or #f 2 "3")
	=> (<positive-fixnum>))

  (doit (or #f #f "3")
	=> (<string>))

  ;;This is expanded to:
  ;;
  ;;   (begin
  ;;     #f
  ;;     (quote #t)
  ;;     (quote "ciao"))
  ;;
  (doit (or #f (and #t "ciao") 3.4)
	=> (<string>))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<top>))

  (doit (or (unsafe-cast-signature (<boolean>) (read))
	    (unsafe-cast-signature (<boolean>) (read))
	    (unsafe-cast-signature (<boolean>) (read)))
	=> (<boolean>))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<top>))

  (doit (or (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> (<fixnum>))

  (doit (or (unsafe-cast-signature ((or <false> <string>)) (read))
	    (unsafe-cast-signature (<fixnum>) (read))
	    (unsafe-cast-signature (<top>) (read)))
	=> ((or <string> <fixnum>)))

  (doit (or (unsafe-cast-signature ((or <false> <string>)) (read))
	    (unsafe-cast-signature ((or <false> <vector>)) (read))
	    (unsafe-cast-signature (<fixnum>) (read)))
	=> ((or <string> <vector> <fixnum>)))

  (doit (or (unsafe-cast-signature ((or <false> <fixnum>)) (read))
	    (unsafe-cast-signature ((or <false> <string>)) (read))
	    (unsafe-cast-signature ((or <false> <vector>)) (read)))
	=> ((or <fixnum> <string> <false> <vector>)))

  (doit (or (unsafe-cast-signature ((or <boolean> <fixnum>)) (read))
	    (unsafe-cast-signature ((or <boolean> <string>)) (read))
	    (unsafe-cast-signature ((or <boolean> <vector>)) (read)))
	=> ((or <fixnum> <string> <boolean> <vector>)))

  (doit (or (unsafe-cast-signature (<top>) (read))
	    (unsafe-cast-signature (<top>) (read))
	    (error #f "ciao"))
	=> (<top>))

  ;;This raises an expand-time exception because void is forbidden as argument to OR.
  ;;
  ;; (doit (or (unsafe-cast-signature (<top>) (read))
  ;; 	    (void)
  ;; 	    (unsafe-cast-signature (<top>) (read)))
  ;; 	=> (<void>))

  (void))


(parametrise ((check-test-name	'not))

  (doit (not #t)
	=> (<false>))

  (doit (not #f)
	=> (<true>))

  (doit (not (read))
	=> (<boolean>))

  (void))


(parametrise ((check-test-name	'xor))

;;; XOR returns non-false if only one sub-expression is non-false.

  (doit (xor #f)		=> (<false>))
  (doit (xor 123)		=> (<positive-fixnum>))

  (doit (xor 123 #f)		=> (<positive-fixnum>))
  (doit (xor #f 123)		=> (<positive-fixnum>))

  (doit (xor #f #f)		=> (<false>))
  (doit (xor #f #f)		=> (<false>))

  (doit (xor #f 2 3)		=> (<false>))
  (doit (xor 1 #f 3)		=> (<false>))
  (doit (xor 1 2 #f)		=> (<false>))

  (doit (xor 1 #f #f)		=> (<positive-fixnum>))
  (doit (xor #f 2 #f)		=> (<positive-fixnum>))
  (doit (xor #f #f 3)		=> (<positive-fixnum>))

  (doit (xor "1" #f #f)		=> (<string>))
  (doit (xor #f "2" #f)		=> (<string>))
  (doit (xor #f #f "3")		=> (<string>))

;;; --------------------------------------------------------------------

  (doit (xor (read) #f)		=> (<top>))
  (doit (xor #f     (read))	=> (<top>))
  (doit (xor (read) (read))	=> (<top>))

  (doit (xor (read) #f #f)	=> (<top>))
  (doit (xor #f (read) #f)	=> (<top>))
  (doit (xor #f #f (read))	=> (<top>))

  (doit (xor (read) (read) (read))	=> (<top>))

  (doit (xor (cast-signature (<boolean>) (read))
	     (cast-signature (<boolean>) (read))
	     (cast-signature (<boolean>) (read)))
	=> (<boolean>))

  (void))


(parametrise ((check-test-name	'application))

  (doit (let ((fun (lambda ({a <fixnum>}) a)))
	  (fun 1))
	=> (<fixnum>))

  ;;Exact match between operands and clauses.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <fixnum>} {b <fixnum>}) (fx+ a b)))))
    (doit (fun 1)	=> (<fixnum>))
    (doit (fun 1 -2)	=> (<fixnum>))
    #| end of LET |# )

  ;;Exact match between operands and clauses.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <string>} {b <string>}) (string-append a b)))))
    (doit (fun 1)			=> (<fixnum>))
    (doit (fun "ciao" " mamma")		=> (<string>))
    #| end of LET |# )

  ;;Possible match between operands and clauses: only one clause is possible.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       (({a <string>} {b <string>}) (string-append a b)))))
    (doit (fun (cast-signature (<number>) 1))		=> (<fixnum>))
    (doit (fun (cast-signature (<top>) "ciao")
	       (cast-signature (<top>) " mamma"))	=> (<string>))
    #| end of LET |# )

  ;;Possible match between operands and clauses: multiple clauses are possible.
  ;;
  (let ((fun (case-lambda
	       (({a <fixnum>}) a)
	       ((a b c d)	'no-match)
	       (({a <string>} . {b* (list-of <string>)}) (apply string-append a b*)))))
    (doit (fun (cast-signature (<top>) 1))		=> ((or <string> <fixnum>)))
    (doit (fun (cast-signature (<top>) "ciao"))		=> ((or <string> <fixnum>)))
    (doit (fun (cast-signature (<top>) "ciao")
	       (cast-signature (<top>) " mamma"))	=> (<string>))
    #| end of LET |# )

  (void))


(parametrise ((check-test-name	'doc-examples))

  (debug-print
   (type-of (let ((A 1))
	      (let* ((B A)
		     (C B))
		(letrec ((D C))
		  (letrec* ((E D)
			    (F E))
		    (define (G)
		      F)
		    ((lambda () (G)))))))))

  (doit (let ((A 1))
	  (let* ((B A)
		 (C B))
	    (letrec ((D C))
	      (letrec* ((E D)
			(F E))
		(define (G)
		  F)
		((lambda () (G)))))))
	=> (<positive-fixnum>))

;;; --------------------------------------------------------------------

  (begin
    (debug-print
     (type-of (let ((A 1))
		(let* ((B A)
		       (C B))
		  (letrec ((D C))
		    (letrec* ((E D)
			      (F E))
		      (define (G)
			F)
		      (define H
			(G))
		      ((lambda () H))))))))

    (doit (let ((A 1))
	    (let* ((B A)
		   (C B))
	      (letrec ((D C))
		(letrec* ((E D)
			  (F E))
		  (define (G)
		    F)
		  (define H
		    (G))
		  ((lambda () H))))))
	  => (<positive-fixnum>))

    #| end of BEGIN |#  )

;;; --------------------------------------------------------------------
;;; mutated variables

  (doit (let ((A "ciao"))
	  (set! A "hello")
	  A)
	=> (<string>))

  #;(doit (let ((A 1))
	  (set! A -1)
	  A)
	=> (<fixnum>))

  (doit (let (({A <fixnum>} 1))
	  (set! A -1)
	  A)
	=> (<fixnum>))

;;; --------------------------------------------------------------------
;;; CASE

  (doit (case (read)
	  ((1 "2" 'ciao) => (lambda (x) x))
	  (else #f))
	=> ((or <positive-fixnum> <string> (enumeration ciao) <false>)))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
