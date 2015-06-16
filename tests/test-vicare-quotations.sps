;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for QUOTE and QUASIQUOTE
;;;Date: Tue Jun 16, 2015
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
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for QUOTE and QUASIQUOTE\n")


(parametrise ((check-test-name	'quotation))

  (define %eval-env
    (environment '(rnrs)))

  (define (%eval form)
    (eval form %eval-env))

;;; --------------------------------------------------------------------

  (check
      (try
	  (%eval '(quote))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-form E)))
	  (else E)))
    => '(quote))

  (check
      (try
	  (%eval '(quote 2 3))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-form E)))
	  (else E)))
    => '(quote 2 3))

  (check (quote 1)			=> 1)
  (check (quote (1))			=> (list 1))
  (check (quote (1 . 2))		=> (cons 1 2))
  (check (quote (1 2))			=> (list 1 2))
  (check (quote (1 2 3))		=> (list 1 2 3))

  (check (quote ((1) 2 3))		=> (list (list 1) 2 3))
  (check (quote (1 (2) 3))		=> (list 1 (list 2) 3))
  (check (quote (1 2 (3)))		=> (list 1 2 (list 3)))

  (check (quote ((1 2) 3))		=> (list (list 1 2) 3))
  (check (quote (1 (2 3)))		=> (list 1 (list 2 3)))

  (check (quote ((x y) z))		=> (list (list 'x 'y) 'z))
  (check (quote (x (y z)))		=> (list 'x (list 'y 'z)))

;;; --------------------------------------------------------------------
;;; vectors

  (check (quote #(1 2))			=> '#(1 2))
  (check (quote #(1 (2)))		=> (vector 1 (list 2)))
  (check (quote (#(1 2)))		=> (list '#(1 2)))
  (check (quote #(1 (2 #(3 4) 5)))	=> (vector 1 (list 2 (vector 3 4) 5)))
  (check (quote #(x (y #(s u) z)))	=> (vector 'x (list 'y (vector 's 'u) 'z)))
  (check
      (quote #(1 (2 #(3 (4 . 5)) 6)))
    => (vector 1 (list 2 (vector 3 (cons 4 5)) 6)))

  #t)


(parametrise ((check-test-name	'quasiquotation))

  (define %eval-env
    (environment '(rnrs)))

  (define (%eval form)
    (eval form %eval-env))

;;; --------------------------------------------------------------------
;;; tests from the R6RS document

  (check
      `(list ,(+ 1 2) 4)
    => '(list 3 4))

  (check
      (let ((name 'a))
	`(list ,name ',name))
    => '(list a (quote a)))

  (check
      `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
    => '(a 3 4 5 6 b))

  (check
      `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
    => '((foo 7) . cons))

  (check
      `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
    => '#(10 5 2 4 3 8))

  (check
      (let ((name 'foo))
	`((unquote name name name)))
    => '(foo foo foo))

  (check
      (let ((name '(foo)))
	`((unquote-splicing name name name)))
    => '(foo foo foo))

  (check
      (let ((q '((append x y) (sqrt 9))))
	``(foo ,,@q))
    => '(quasiquote (foo (unquote (append x y) (sqrt 9)))))

  (check
      (let ((x '(2 3))
	    (y '(4 5)))
	`(foo (unquote (append x y) (sqrt 9))))
    => '(foo (2 3 4 5) 3))

;;;

  (check
      `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
    => '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))

  (check
      (let ((name1 'x)
	    (name2 'y))
	`(a `(b ,,name1 ,',name2 d) e))
    => '(a `(b ,x ,'y d) e))


;;; --------------------------------------------------------------------

  (check (quasiquote 1)				=> 1)
  (check (quasiquote (1))			=> (quote (1)))
  (check (quasiquote (1 . 2))			=> (quote (1 . 2)))
  (check (quasiquote (1 2))			=> (quote (1 2)))
  (check (quasiquote (1 2 . 3))			=> (quote (1 2 . 3)))
  (check (quasiquote ((1) (2)))			=> (quote ((1) (2))))
  (check (quasiquote ((1 2) (3 4)))		=> (quote ((1 2) (3 4))))
  (check (quasiquote ((1 2) (3 4)))		=> (quote ((1 2) (3 4))))
  (check (quasiquote ((1 . 2) (3 . 4)))		=> (quote ((1 . 2) (3 . 4))))
  (check (quasiquote ((1 . 2) . (3 . 4)))	=> (quote ((1 . 2) . (3 . 4))))
  (check (quasiquote ((1 2) . 3))		=> (quote ((1 2) . 3)))
  (check (quasiquote ((1 . 2) . 3))		=> (quote ((1 . 2) . 3)))

  (check (quasiquote ())			=> '())
  (check (quasiquote (()))			=> (list '()))
  (check (quasiquote ((())))			=> (list (list '())))
  (check (quasiquote (((1))))			=> (list (list (list 1))))
  (check (quasiquote (() ()))			=> (list '() '()))
  (check (quasiquote (() . ()))			=> (cons '() '()))
  (check (quasiquote (() () ()))		=> (list '() '() '()))
  (check (quasiquote (() () . ()))		=> (cons* '() '() '()))
  (check (quasiquote (() () . ()))		=> (list '() '()))
  (check (quasiquote (() () . (1)))		=> (cons* '() '() (list 1)))
  (check (quasiquote (() () . (1 2)))		=> (cons* '() '() (list 1 2)))

  (check (quasiquote (1 . ()))			=> (quote (1)))
  (check (quasiquote (1 2 . ()))		=> (quote (1 2)))
  (check (quasiquote ((1 . 2) . ()))		=> (quote ((1 . 2))))
  (check (quasiquote ((1 2) . ()))		=> (quote ((1 2))))
  (check (quasiquote ((1 . 2) . ()))		=> (quote ((1 . 2))))

;;; --------------------------------------------------------------------
;;; vector template

  (check (quasiquote #(1))			=> '#(1))
  (check (quasiquote (#(1)))			=> (quote (#(1))))
  (check (quasiquote (#(1) #(2)))		=> (quote (#(1) #(2))))
  (check (quasiquote (#(1 2) (3 4)))		=> (quote (#(1 2) (3 4))))
  (check (quasiquote ((1 2) #(3 4)))		=> (quote ((1 2) #(3 4))))
  (check (quasiquote (#(1 2) (3 . 4)))		=> (quote (#(1 2) (3 . 4))))
  (check (quasiquote ((1 . 2) #(3 4)))		=> (quote ((1 . 2) #(3 4))))
  (check (quasiquote (#(1 2) . 3))		=> (quote (#(1 2) . 3)))
  (check (quasiquote ((1 2) . #(3)))		=> (quote ((1 2) . #(3))))
  (check (quasiquote (#(1 2) . 3))		=> (quote (#(1 2) . 3)))
  (check (quasiquote ((1 . 2) . #(3)))		=> (quote ((1 . 2) . #(3))))

  (check (quasiquote #(#()))			=> '#(#()))
  (check (quasiquote #(#() #()))		=> '#(#() #()))
  (check (quasiquote #(#() #() #()))		=> '#(#() #() #()))
  (check (quasiquote #(1 #() 2))		=> '#(1 #() 2))
  (check (quasiquote #(1 #(2 3) 4))		=> '#(1 #(2 3) 4))

;;; --------------------------------------------------------------------
;;; UNQUOTE

  (check (quasiquote (unquote 1))			=> 1)
  (check (quasiquote (unquote (quote (1 2 3))))		=> (quote (1 2 3)))

  ;;Invalid multi-operand UNQUOTE form outside list and vector templates.
  ;;
  (check
      (try
	  (%eval '(quasiquote (unquote 1 2)))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote 1 2))

  (check (quasiquote ((unquote) 1))			=> (quote (1)))
  (check (quasiquote ((unquote) (+ 1 2)))		=> (quote ((+ 1 2))))
  (check (quasiquote ((unquote '()) 1))			=> (quote (() 1)))
  (check (quasiquote ((unquote '()) ()))		=> (quote (() ())))
  (check (quasiquote ((unquote '()) . ()))		=> (cons '() '()))
  (check (quasiquote ((unquote 1) 2))			=> (quote (1 2)))
  (check (quasiquote ((unquote (+ 1 2)) . 4))		=> (cons 3 4))
  (check (quasiquote ((unquote (+ 1 2)) . (+ 8 9)))	=> (cons 3 (quote (+ 8 9))))
  (check (quasiquote ((unquote (+ 1 2)) 4))		=> (list 3 4))
  (check (quasiquote ((unquote (+ 1 2)) (+ 8 9)))	=> (list 3 (quote (+ 8 9))))

  (check (quasiquote (1 (unquote (+ 2 3))))		=> (quote (1 5)))
  (check (quasiquote (1 . (unquote (+ 2 3))))		=> (cons 1 (+ 2 3)))

  ;;Empty UNQUOTE form in improper tail position.
  ;;
  (check
      (try
	  (%eval '(quasiquote (1 . (unquote))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote))

  (check
      (quasiquote ((unquote (+ 10 1) (+ 20 2) (+ 30 3)) (+ 8 9)))
    => (quote (11 22 33 (+ 8 9))))

  ;;Syntax error: improper list as UNQUOTE form.
  ;;
  (check
      (try
	  (%eval '(quasiquote ((unquote (+ 10 1) (+ 20 2) . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote (+ 10 1) (+ 20 2) . 3))

;;; vector templates

  (check (quasiquote #((unquote) 1))			=> '#(1))
  (check (quasiquote #((unquote) (+ 1 2)))		=> `#(,(quote (+ 1 2))))
  (check (quasiquote #((unquote '()) 1))		=> '#(() 1))
  (check (quasiquote #((unquote '()) ()))		=> '#(() ()))
  (check (quasiquote #((unquote 1) 2))			=> '#(1 2))
  (check (quasiquote #((unquote (+ 1 2)) 4))		=> '#(3 4))
  (check (quasiquote #((unquote (+ 1 2)) (+ 8 9)))	=> `#(3 ,(quote (+ 8 9))))

  (check
      (quasiquote (1 #(2 (unquote (+ 3. 4.)) (unquote (+ 5. 6.)) 7) 8))
    => (list 1 (vector 2 (+ 3. 4.) (+ 5. 6.) 7) 8))

  (check
      (quasiquote #((unquote (+ 10 1) (+ 20 2) (+ 30 3)) (+ 8 9)))
    => `#(11 22 33 ,(quote (+ 8 9))))

  (check
      (quasiquote #(1 #((unquote (+ 2 3)) 4) 5))
    => (vector 1 (vector (+ 2 3) 4) 5))

  (check
      (quasiquote #(1 #((quasiquote (+ 2 3)) 4) 5))
    => (vector 1 (vector (list 'quasiquote (quote (+ 2 3))) 4) 5))

  (check
      (quasiquote #(1 #((quasiquote (+ 2 (unquote (+ 3.1 3.2))) 4) 5)))
    => (vector 1 (vector (quote (quasiquote (+ 2 (unquote (+ 3.1 3.2))) 4)) 5)))

  (check
      (quasiquote #(1 #((quasiquote (+ 2 (unquote (unquote (+ 3.1 3.2)))) 4) 5)))
    => (vector 1 (vector (list 'quasiquote (list '+ 2 (list 'unquote (+ 3.1 3.2))) 4) 5)))

;;;

  ;;Syntax error: improper list as UNQUOTE form.
  ;;
  (check
      (try
	  (%eval '(quasiquote #((unquote (+ 10 1) (+ 20 2) . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote (+ 10 1) (+ 20 2) . 3))

;;; --------------------------------------------------------------------
;;; UNQUOTE-SPLICING

  (check
      (quasiquote ((unquote-splicing (quote (1 2 3)))))
    => (quote (1 2 3)))

  (check (quasiquote ((unquote-splicing) 1))			=> (quote (1)))
  (check (quasiquote ((unquote-splicing) (+ 1 2)))		=> (quote ((+ 1 2))))
  (check (quasiquote ((unquote-splicing '()) 1))		=> (quote (1)))
  (check (quasiquote ((unquote-splicing '()) ()))		=> (list '()))
  (check (quasiquote ((unquote-splicing '()) . ()))		=> '())
  (check (quasiquote ((unquote-splicing (quote (1))) 2))	=> (quote (1 2)))

  ;;Invalid UNQUOTE-SPLICING form outside list and vector templates.
  ;;
  (check
      (try
	  (%eval '(quasiquote (unquote-splicing (list 1))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote-splicing (list 1)))

  (check (quasiquote ((unquote-splicing (list (+ 1 2))) . 4))		=> (cons 3 4))
  (check (quasiquote ((unquote-splicing (list (+ 1 2))) . (+ 8 9)))	=> (cons 3 (quote (+ 8 9))))
  (check (quasiquote ((unquote-splicing (list (+ 1 2))) 4))		=> (list 3 4))
  (check (quasiquote ((unquote-splicing (list (+ 1 2))) (+ 8 9)))	=> (list 3 (quote (+ 8 9))))

  (check (quasiquote (1 (unquote-splicing (list (+ 2 3)))))		=> (quote (1 5)))
  (check (quasiquote (1 . (unquote-splicing (list (+ 2 3)))))		=> (quote (1 5)))

  ;;Empty UNQUOTE-SPLICING form in improper tail position.
  ;;
  (check
      (try
	  (%eval '(quasiquote (1 . (unquote-splicing))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote-splicing))

  (check
      (quasiquote (1 #(2 (unquote-splicing (list (+ 3. 4.)) (list (+ 5. 6.))) 7) 8))
    => (list 1 (vector 2 (+ 3. 4.) (+ 5. 6.) 7) 8))

  (check
      (quasiquote ((unquote-splicing (list (+ 10 1))
				      (list (+ 20 2))
				      (list (+ 30 3)))
		   (+ 8 9)))
    => (quote (11 22 33 (+ 8 9))))

  ;;Syntax error: improper list as UNQUOTE-SPLICING form.
  ;;
  (check
      (try
	  (%eval '(quasiquote ((unquote-splicing (list (+ 10 1))
						  (list (+ 20 2))
						  . 3) (+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote-splicing (list (+ 10 1)) (list (+ 20 2)) . 3))

  ;;Difference between UNQUOTE and UNQUOTE-SPLICING.
  ;;
  (check
      (quasiquote ((unquote (list (+ 10 1))
			     (list (+ 20 2))
			     (list (+ 30 3)))
		   (+ 8 9)))
    => (quote ((11) (22) (33) (+ 8 9))))
  (check
      (quasiquote ((unquote-splicing (list (+ 10 1))
				      (list (+ 20 2))
				      (list (+ 30 3)))
		   (+ 8 9)))
    => (quote (11 22 33 (+ 8 9))))

;;; vector templates

  (check (quasiquote #((unquote-splicing) 1))				=> '#(1))
  (check (quasiquote #((unquote-splicing) (+ 1 2)))			=> `#(,(quote (+ 1 2))))
  (check (quasiquote #((unquote-splicing '()) 1))			=> '#(1))
  (check (quasiquote #((unquote-splicing '()) ()))			=> '#(()))
  (check (quasiquote #((unquote-splicing (quote (1))) 2))		=> '#(1 2))
  (check (quasiquote #((unquote-splicing (list (+ 1 2))) 4))		=> '#(3 4))
  (check (quasiquote #((unquote-splicing (list (+ 1 2))) (+ 8 9)))	=> `#(3 ,(quote (+ 8 9))))
  (check
      (quasiquote #((unquote-splicing (list (+ 10 1))
				       (list (+ 20 2))
				       (list (+ 30 3)))
		    (+ 8 9)))
    => `#(11 22 33 ,(quote (+ 8 9))))

  (check
      (quasiquote #(1 #((unquote-splicing (list (+ 2 3))) 4) 5))
    => (vector 1 (vector (+ 2 3) 4) 5))

  (check
      (quasiquote #(1 #((quasiquote (+ 2 (unquote-splicing (list (+ 3.1 3.2)))) 4) 5)))
    => (vector 1 (vector (quote (quasiquote (+ 2 (unquote-splicing (list (+ 3.1 3.2)))) 4)) 5)))

  (check
      (quasiquote #(1 #((quasiquote (+ 2 (unquote-splicing (unquote-splicing (list (+ 3.1 3.2))))) 4) 5)))
    => (vector 1 (vector (list 'quasiquote (list '+ 2 (list 'unquote-splicing (+ 3.1 3.2))) 4) 5)))

  ;;Syntax error: improper list as UNQUOTE-SPLICING form.
  ;;
  (check
      (try
	  (%eval '(quasiquote #((unquote-splicing (list (+ 10 1))
						   (list (+ 20 2))
						   . 3)
				(+ 8 9))))
	(catch E
	  ((&syntax)
	   (syntax->datum (syntax-violation-subform E)))
	  (else E)))
    => '(unquote-splicing (list (+ 10 1)) (list (+ 20 2)) . 3))

;;; --------------------------------------------------------------------
;;; nested quotation

  (check (quasiquote (1 (quasiquote (2)) 3))		=> (quote (1 (quasiquote (2)) 3)))
  (check (quasiquote (1 (quasiquote (2)) . 3))		=> (quote (1 (quasiquote (2)) . 3)))

  (check (quasiquote ((quasiquote (2)) 3))		=> (quote ((quasiquote (2)) 3)))
  (check (quasiquote ((quasiquote (2)) . 3))		=> (quote ((quasiquote (2)) . 3)))

  (check (quasiquote (1 (quasiquote (2))))		=> (quote (1 (quasiquote (2)))))
  (check (quasiquote (1 . (quasiquote (2))))		=> (quote (1 . (quasiquote (2)))))

  (check
      (quasiquote (1 (quasiquote (unquote (+ 1 2))) 3))
    => (quote (1 (quasiquote (unquote (+ 1 2))) 3)))

  (check
      (quasiquote (1 (quasiquote (unquote (unquote (+ 1 2)))) 3))
    => (quote (1 (quasiquote (unquote 3)) 3)))

  (check
      (quasiquote (1 (quasiquote (unquote (unquote-splicing (list (+ 1 2))))) 3))
    => (list 1 (list 'quasiquote (list 'unquote 3)) 3))

  (check
      (quasiquote (1 (quasiquote (unquote (quasiquote (unquote (+ 1 2))))) 3))
    => (quote (1 (quasiquote (unquote (quasiquote (unquote (+ 1 2))))) 3)))


;;; --------------------------------------------------------------------
;;; misc

  (check
      (quasiquote (1 2 (unquote (+ 3 4))))
    => (list '1 '2 (+ '3 '4)))

  (check
      (quasiquote (1 2 (unquote (+ 3 4)) 5))
    => (list '1 '2 (+ '3 '4) '5))

  (check
      (quasiquote #(1 2 (unquote (+ 3 4))))
    => (vector '1 '2 (+ '3 '4)))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
