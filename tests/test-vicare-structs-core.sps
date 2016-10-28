;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for structs
;;;Date: Mon Oct 24, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011-2012, 2014-2016 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-structs-core)
  (options strict-r6rs)
  (import (vicare)
    (vicare checks)
    (vicare system structs)
    (vicare system $structs))

(print-unicode #f)
(check-set-mode! 'report-failed)
(check-display "*** testing Vicare structs\n")


;;;; helpers

(define-syntax check-argument-violation
  (syntax-rules (=>)
    ((_ ?body => ?result)
     (check
	 (guard (E ((procedure-signature-argument-violation? E)
		    #;(print-condition E)
		    (procedure-signature-argument-violation.offending-value E))
		   ((procedure-arguments-consistency-violation? E)
		    #;(print-condition E)
		    (condition-irritants E))
		   ((procedure-argument-violation? E)
		    (when #f
		      (debug-print (condition-message E)))
		    (let ((D (cdr (condition-irritants E))))
		      (if (pair? D)
			  (car D)
			(condition-irritants E))))
		   (else
		    (print-condition E)
		    E))
	   ?body)
       => ?result))))


(parametrise ((check-test-name	'definition))

  (define-struct color
    (red green blue))

  (check
      (let ((S (make-color 1 2 3)))
	(color? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	(set-color-red!   S 10)
	(set-color-green! S 20)
	(set-color-blue!  S 30)
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	($set-color-red!   S 10)
	($set-color-green! S 20)
	($set-color-blue!  S 30)
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'definition-alternate))

  (define-struct (color make-the-color the-color?)
    (red green blue))

  (check
      (let ((S (make-the-color 1 2 3)))
	(the-color? S))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-the-color 1 2 3)))
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-the-color 1 2 3)))
	(set-color-red!   S 10)
	(set-color-green! S 20)
	(set-color-blue!  S 30)
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(10 20 30))

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-the-color 1 2 3)))
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-the-color 1 2 3)))
	($set-color-red!   S 10)
	($set-color-green! S 20)
	($set-color-blue!  S 30)
	(list ($color-red   S)
	      ($color-green S)
	      ($color-blue  S)))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'rtd))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (define-record-type r6rs-type
    (fields a b c))

  (check
      (struct-type-descriptor? color-rtd)
    => #t)

  (check
      (struct-type-descriptor? (record-type-descriptor r6rs-type))
    => #f)

  (check
      (struct-type-name color-rtd)
    => "color")

  (check
      (symbol? (struct-type-symbol color-rtd))
    => #t)

  (check
      (struct-type-field-names color-rtd)
    => '(red green blue))

;;; --------------------------------------------------------------------

  (check-argument-violation
      (struct-type-name 123)
    => 123)

  (check-argument-violation
      (struct-type-symbol 123)
    => 123)

  (check-argument-violation
      (struct-type-field-names 123)
    => 123)

  #t)


(parametrise ((check-test-name	'nongenerative))

  (define-struct color
    (red green blue)
    (nongenerative non-color))

  (define color-std
    (make-struct-type "color" '(red green blue) 'non-color))

  (check
      (let ((O ((struct-type-constructor color-std) 1 2 3)))
	(values (color? O)
		(color-red O)
		(color-green O)
		(color-blue O)))
    => #t 1 2 3)

  (check
      (let ((O (make-color 1 2 3)))
	(values ((struct-type-predicate color-std) O)
		((struct-type-field-accessor color-std 'red) O)
		((struct-type-field-accessor color-std 'green) O)
		((struct-type-field-accessor color-std 'blue) O)))
    => #t 1 2 3)

  #t)


(parametrise ((check-test-name	'using))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (define make-color
    (struct-type-constructor color-rtd))

  (define color?
    (struct-type-predicate color-rtd))

  (define color-red
    (struct-type-field-accessor color-rtd 0))

  (define color-green
    (struct-type-field-accessor color-rtd 1))

  (define color-blue
    (struct-type-field-accessor color-rtd 2))

  (define set-color-red!
    (struct-type-field-mutator color-rtd 0))

  (define set-color-green!
    (struct-type-field-mutator color-rtd 1))

  (define set-color-blue!
    (struct-type-field-mutator color-rtd 2))

;;; --------------------------------------------------------------------

  (check
      (let ((S (make-color 1 2 3)))
	(color? S))
    => #t)

  (check
      (let ((S (make-color 1 2 3)))
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(1 2 3))

  (check
      (let ((S (make-color 1 2 3)))
	(set-color-red!   S 10)
	(set-color-green! S 20)
	(set-color-blue!  S 30)
	(list (color-red   S)
	      (color-green S)
	      (color-blue  S)))
    => '(10 20 30))

  (check
      (let ((S (make-color 1 2 3)))
	((struct-type-field-accessor color-rtd 'red) S))
    => 1)

  (check
      (let ((S (make-color 1 2 3)))
	((struct-type-field-mutator  color-rtd 'red) S 10)
	((struct-type-field-accessor color-rtd 'red) S))
    => 10)

;;; --------------------------------------------------------------------

  (check-argument-violation
      (struct-type-constructor 123)
    => 123)

  (check-argument-violation
      (struct-type-predicate 123)
    => 123)

  (check-argument-violation
      (struct-type-field-accessor 123 0)
    => 123)

  (check-argument-violation
      (struct-type-field-mutator 123 0)
    => 123)

  (check-argument-violation
      (struct-type-field-accessor color-rtd 3)
    => (list 3 color-rtd))

  (check-argument-violation
      (struct-type-field-mutator color-rtd 3)
    => (list 3 color-rtd))

  #t)


(parametrise ((check-test-name	'inspect))

  (define color-rtd
    (make-struct-type "color" '(red green blue)))

  (define S
    ((struct-type-constructor color-rtd) 1 2 3))

;;; --------------------------------------------------------------------

  (check
      (struct? 123)
    => #f)

  (check
      (struct? color-rtd)
    => #t)

  (check
      (struct? S)
    => #t)

  (check
      (struct? S color-rtd)
    => #t)

  (check
      (struct-length color-rtd)
    => 6)

  (check
      (struct-length S)
    => 3)

  (check
      (struct-name color-rtd)
    => "base-rtd")

  (check
      (struct-name S)
    => "color")

  (check
      (struct-ref S 0)
    => 1)

  (check
      (struct-ref S 1)
    => 2)

  (check
      (struct-ref S 2)
    => 3)

  (check
      (begin
	(struct-set! S 0 10)
	(struct-ref S 0))
    => 10)

  (check
      (begin
	(struct-set! S 1 20)
	(struct-ref S 1))
    => 20)

  (check
      (begin
	(struct-set! S 2 30)
	(struct-ref S 2))
    => 30)

;;; --------------------------------------------------------------------

  (check-argument-violation
      (struct-length 123)
    => 123)

  (check-argument-violation
      (struct-name 123)
    => 123)

  (check-argument-violation
      (struct-set! 123 0 0)
    => 123)

  (check-argument-violation
      (struct-ref 123 0)
    => 123)

  (check-argument-violation
      (struct-set! S 4 0)
    => (list 4 S))

  (check-argument-violation
      (struct-ref S 4)
    => (list 4 S))

;;; --------------------------------------------------------------------

  (check
      (let ((S ((struct-type-constructor color-rtd) 1 2 3)))
	(struct-reset! S)
	(list (struct-ref S 0)
	      (struct-ref S 1)
	      (struct-ref S 2)))
    => `(,(void) ,(void) ,(void)))

  #t)


(parametrise ((check-test-name	'equality))

  (define-struct alpha
    (a b c))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-alpha 1 2 3)))
     (struct=? P P)))

  (check-for-true
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 3)))
     (struct=? P Q)))

  (check-for-false
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 9)))
     (struct=? P Q)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-alpha 1 2 3)))
     (equal? P P)))

  (check-for-true
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 3)))
     (equal? P Q)))

  (check-for-false
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 9)))
     (equal? P Q)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let ((P (make-alpha 1 2 3)))
     (eqv? P P)))

  (check-for-false
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 3)))
     (eqv? P Q)))

  (check-for-false
   (let ((P (make-alpha 1 2 3))
	 (Q (make-alpha 1 2 9)))
     (eqv? P Q)))

  #t)


(parametrise ((check-test-name		'destructor)
	      (struct-guardian-logger	(lambda (S E action)
					  (check-pretty-print (list S E action)))))

  (define-struct alpha
    (a b c))

  (define (alpha-destructor S)
    (display "alpha-destructor\n" stderr)
    (void))

  (set-struct-type-destructor! (type-descriptor alpha) alpha-destructor)

  #;(debug-print ($std-destructor (type-descriptor alpha)))

  (check
      (parametrise ((struct-guardian-logger #t))
  	(let ((S (make-alpha 1 2 3)))
  	  (check-pretty-print S)
  	  (collect)))
    => (void))

  (check
      (let ((S (make-alpha 1 2 3)))
  	(check-pretty-print S)
  	(collect))
    => (void))

  (check
      (let ((S (make-alpha 1 2 3)))
  	(check-pretty-print S)
  	(collect))
    => (void))

  (collect))


(parametrise ((check-test-name	'syntaxes))

  (define-struct alpha
    (a b c))

  (define-struct beta
    (a b c))

;;; --------------------------------------------------------------------
;;; type descriptor

  (check
      (struct-type-descriptor? (struct-type-descriptor alpha))
    => #t)

  (check
      (struct-type-descriptor? (type-descriptor alpha))
    => #t)

  (check
      (struct-type-descriptor? (struct-type-descriptor beta))
    => #t)

  (check
      (struct-type-descriptor? (type-descriptor beta))
    => #t)

;;; --------------------------------------------------------------------
;;; predicate

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru alpha))
    => #t)

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru beta))
    => #f)

  (check
      (is-a? 123 beta)
    => #f)

;;; --------------------------------------------------------------------
;;; generic maker syntax

  (check
      (let ((stru (new alpha 1 2 3)))
  	(alpha? stru))
    => #t)

  (check
      (let ((stru (new beta 1 2 3)))
  	(beta? stru))
    => #t)

;;; --------------------------------------------------------------------
;;; generic predicate syntax

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru alpha))
    => #t)

  (check
      (let ((stru (make-alpha 1 2 3)))
	(is-a? stru beta))
    => #f)

  (check
      (let ((stru (make-alpha 1 2 3)))
	((is-a? _ alpha) stru))
    => #t)

  (check
      (is-a? 123 alpha)
    => #f)

  (check
      (is-a? 123 beta)
    => #f)

  #t)


(parametrise ((check-test-name	'unsafe-std-operations))

  (define-struct alpha
    (a b c))

  (define-struct beta
    (a b c))

  (define alpha-std
    (struct-type-descriptor alpha))

  (define beta-std
    (struct-type-descriptor beta))

  (define (the-beta-destructor S)
    #t)

  (set-struct-type-destructor! beta-std the-beta-destructor)

;;; --------------------------------------------------------------------

  (check
      ($std-std (struct-type-descriptor alpha))
    => (base-rtd))

  (check
      ($std-name (struct-type-descriptor alpha))
    => "alpha")

  (check
      ($std-length (struct-type-descriptor alpha))
    => 3)

  (check
      ($std-fields (struct-type-descriptor alpha))
    => '(a b c))

  (check
      ($std-printer (struct-type-descriptor alpha))
    => #f)

  (check
      (gensym? ($std-symbol (struct-type-descriptor alpha)))
    => #t)

  (check
      ($std-destructor (struct-type-descriptor alpha))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ($std-std beta-std)
    => (base-rtd))

  (check
      ($std-name beta-std)
    => "beta")

  (check
      ($std-length beta-std)
    => 3)

  (check
      ($std-fields beta-std)
    => '(a b c))

  (check
      ($std-printer beta-std)
    => #f)

  (check
      (gensym? ($std-symbol beta-std))
    => #t)

  (check
      ($std-destructor beta-std)
    => the-beta-destructor)

  ;; (check
  ;;     (struct-type-destructor beta-std)
  ;;   => the-beta-destructor)

;;; --------------------------------------------------------------------

  (check
      (let ((std beta-std))
	($set-std-name! std "ciao")
	($std-name std))
    => "ciao")

  (check
      (let ((std beta-std))
	($set-std-length! std 2)
	($std-length std))
    => 2)

  (check
      (let ((std beta-std))
	($set-std-fields! std '(A B))
	($std-fields std))
    => '(A B))

  (check
      (let ((std beta-std)
	    (fun (lambda args (void))))
	($set-std-printer! std fun)
	(eq? fun ($std-printer std)))
    => #t)

  (check
      (let ((std beta-std)
	    (fun (lambda args (void))))
	($set-std-destructor! std fun)
	(eq? fun ($std-destructor std)))
    => #t)

  (check
      (let ((std beta-std)
	    (uid (gensym "uid")))
	(set-symbol-value! uid std)
	($set-std-symbol! std uid)
	(values (eq? uid ($std-symbol std))
		(eq? std (symbol-value uid))))
    => #t #t)

  #t)


;;;; done

(collect)
(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; eval: (put 'check-argument-violation		'scheme-indent-function 1)
;; End:
