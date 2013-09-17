;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for builtin labels
;;;Date: Thu Dec 23, 2010
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


#!vicare
(import (nausicaa)
  (rnrs eval)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing built-in labels\n")


(parametrise ((check-test-name	'booleans))

  (check
      ((<boolean>) #t)
    => #t)

  (check
      ((<boolean>) #f)
    => #t)

  #t)


(parametrise ((check-test-name	'symbols))

  (check
      (is-a? 'ciao <symbol>)
    => #t)

  (check
      (is-a? 123 <symbol>)
    => #f)

  (check
      (let (((o <symbol>) 'ciao))
        (o string))
    => "ciao")

  (check
      (let (((o <symbol>) 'ciao))
        (o string length))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let (((S <symbol>) 'ciao))
	(integer? (S hash)))
    => #t)

  #t)


(parametrise ((check-test-name	'keywords))

  (check
      (is-a? #:ciao <keyword>)
    => #t)

  (check
      (is-a? (<keyword> ('ciao)) <keyword>)
    => #t)

  (check
      (is-a? (<keyword> ("ciao")) <keyword>)
    => #t)

  (check
      (is-a? 123 <keyword>)
    => #f)

  (check
      (let (((o <keyword>) #:ciao))
        (o string))
    => "ciao")

  (check
      (let (((o <keyword>) #:ciao))
        (o string length))
    => 4)

  (check
      (let (((o <keyword>) #:ciao))
        (o symbol))
    => 'ciao)

  #t)


(parametrise ((check-test-name	'pairs))

  (check	;maker
      (let ()
	(<pair> P (<> (1 2)))
	(vector (P car) (P cdr)))
    => '#(1 2))

  (check	;predicate
      ((<pair>) '(1 . 2))
    => #t)

  (check	;virtual fields safe accessors
      (let (((P <pair>) '(1 . 2)))
	(vector (P car) (P cdr)))
    => '#(1 2))

  (check	;virtual fields unsafe accessors
      (let (((P <pair>) '(1 . 2)))
	(vector (P $car) (P $cdr)))
    => '#(1 2))

;;; --------------------------------------------------------------------

  (check	;maker
      (let ()
	(<mutable-pair> P (<> (1 2)))
	(vector (P car) (P cdr)))
    => '#(1 2))

  (check	;predicate
      ((<mutable-pair>) '(1 . 2))
    => #t)

  (check	;virtual fields safe accessors
      (let (((P <mutable-pair>) '(1 . 2)))
	(vector (P car) (P cdr)))
    => '#(1 2))

  (check	;virtual fields unsafe accessors
      (let (((P <mutable-pair>) '(1 . 2)))
	(vector (P $car) (P $cdr)))
    => '#(1 2))

  (check	;virtual fields safe mutators
      (let (((P <mutable-pair>) (cons 1 2)))
	(set! (P car) 10)
	(set! (P cdr) 20)
	(vector (P car) (P cdr)))
    => '#(10 20))

  (check	;virtual fields unsafe mutators
      (let (((P <mutable-pair>) (cons 1 2)))
	(set! (P $car) 10)
	(set! (P $cdr) 20)
	(vector (P car) (P cdr)))
    => '#(10 20))

  #t)


(parametrise ((check-test-name	'lists))

;;; maker

  (check
      (<list> (1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; predicate

  (check
      (is-a? '(1 2 3) <list>)
    => #t)

  (check
      (is-a? '() <list>)
    => #t)

  (check
      (is-a? 123 <list>)
    => #f)

;;; --------------------------------------------------------------------
;;; fields

  (check
      (let (((L <list>) '(1 2 3)))
	(L car))
    => 1)

  (check
      (let (((L <list>) '(1 2 3)))
	(L cdr))
    => '(2 3))

  (check
      (let (((L <list>) '(1 2 3)))
	(L cdr car))
    => 2)

  (check
      (let (((L <list>) '(1 2 3)))
	(L cdr cdr))
    => '(3))

  (check
      (let (((L <list>) '(1 2 3)))
	(L cddr))
    => '(3))

;;; --------------------------------------------------------------------
;;; methods

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o map (lambda (n) (+ 1 n))))
    => '(2 3 4 5 6))

  (check
      (let (((o <list>) '(1 2 3 4 5))
	    (r 0))
	(o for-each (lambda (n) (set! r (+ r n))))
	r)
    => (+ 1 2 3 4 5))

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o find (lambda (n) (= 3 n))))
    => 3)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o for-all (lambda (n) (< 0 n))))
    => #t)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o exists (lambda (n) (if (= 3 n) n #f))))
    => 3)

  #t)


(parametrise ((check-test-name	'chars))

  (check
      ((<char>) #\a)
    => #t)

  (check
      ((<char>) 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((C <char>) #\a))
	(C upcase))
    => #\A)

  (check
      (let (((C <char>) #\A))
	(C downcase))
    => #\a)

  (check
      (let (((C <char>) #\a))
	(C titlecase))
    => #\A)

  (check
      (let (((C <char>) #\a))
	(C foldcase))
    => #\a)

;;; --------------------------------------------------------------------

  (check
      (let (((C <char>) #\a))
	(C alphabetic?))
    => #t)

  (check
      (let (((C <char>) #\1))
	(C numeric?))
    => #t)

  (check
      (let (((C <char>) #\newline))
	(C whitespace?))
    => #t)

  (check
      (let (((C <char>) #\A))
	(C upper-case?))
    => #t)

  (check
      (let (((C <char>) #\A))
	(C lower-case?))
    => #f)

  (check
      (let (((C <char>) #\x01c5))
	(C title-case?))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let (((C <char>) #\xA))
	(C general-category))
    => 'Cc)

  (check
      (let (((C <char>) #\xA))
	(C general-category string))
    => "Cc")

  #t)


(parametrise ((check-test-name	'strings))

;;; predicate

  (check
      (is-a? "ciao" <string>)
    => #t)

  (check
      (is-a? 123 <string>)
    => #f)

;;; --------------------------------------------------------------------
;;; fields

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o length))
    => 4)

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o empty?))
    => #f)

  (check
      (let (((o <string>) ""))
        (o empty?))
    => #t)

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o upcase))
    => "CIAO")

  (check
      (let (((o <string>) (string-copy "ciAO")))
        (o downcase))
    => "ciao")

  (check
      (let (((o <string>) (string-copy "ciAO")))
        (o titlecase))
    => "Ciao")

;;; --------------------------------------------------------------------
;;; methods

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o substring 2))
    => "ao")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o substring 2 3))
    => "a")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o substring 0 -1))
    => "cia")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o substring -3 -1))
    => "ia")

;;; --------------------------------------------------------------------

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o append " mamma"))
    => "ciao mamma")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o list))
    => '(#\c #\i #\a #\o))

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o copy))
    => "ciao")

  (check
      (let (((o <string>) (string-copy "ciao"))
	    (result '()))
        (o for-each (lambda (ch)
		      (set! result (cons ch result))))
	result)
    => (reverse '(#\c #\i #\a #\o)))

;;; --------------------------------------------------------------------
;;; char ref

  (check
      (let (((o <string>) "ciao"))
	(o ref 0))
    => #\c)

  (check
      (let (((o <string>) "ciao"))
	(o ref 1))
    => #\i)

  (check
      (let (((o <string>) "ciao"))
	(o ref -1))
    => #\o)

  (check
      (let (((o <string>) "ciao"))
	(o ref -2))
    => #\a)

;;; --------------------------------------------------------------------
;;; getter

  (check
      (let (((o <string>) "ciao"))
	(o[0]))
    => #\c)

  (check
      (let (((o <string>) "ciao"))
	(o[1]))
    => #\i)

  (check
      (let (((o <string>) "ciao"))
	(o[-1]))
    => #\o)

  (check
      (let (((o <string>) "ciao"))
	(o[-2]))
    => #\a)

;;; --------------------------------------------------------------------
;;; char set

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(o set! 0 #\K)
	o)
    => "Kiao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(o set! 1 #\I)
	o)
    => "cIao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(o set! -2 #\U)
	o)
    => "ciUo")

;;; --------------------------------------------------------------------
;;; setter, syntax 1

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! (o[0]) #\K)
	o)
    => "Kiao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! (o[1]) #\I)
	o)
    => "cIao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! (o[-2]) #\U)
	o)
    => "ciUo")

;;; --------------------------------------------------------------------
;;; setter, syntax 2

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! o[0] #\K)
	o)
    => "Kiao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! o[1] #\I)
	o)
    => "cIao")

  (check
      (let (((o <mutable-string>) (string-copy "ciao")))
	(set! o[-2] #\U)
	o)
    => "ciUo")

;;; --------------------------------------------------------------------
;;; nested setter and getter invocation

  (check
      (let ()
	(define-class <alpha>
	  (fields (str <mutable-string>)))
	(define-class <beta>
	  (parent <alpha>))
	(<beta> B (<> ((string-copy "ciao"))))
	(set! (B str[1]) #\I)
	(vector (B str[0]) (B str[1])))
    => '#(#\c #\I))

  (check
      (let ()
	(define-class <alpha>
	  (fields (str <mutable-string>)))
	(define-class <beta>
	  (fields (a <alpha>)))
	(<beta> B (<> ((<alpha> ((string-copy "ciao"))))))
	(set! (B a str[1]) #\I)
	(vector (B a str[0]) (B a str[1])))
    => '#(#\c #\I))

  #t)


(parametrise ((check-test-name	'vectors))

;;; predicate

  (check
      (is-a? '#(1 2 3) <vector>)
    => #t)

  (check
      (is-a? '#() <vector>)
    => #t)

  (check
      (is-a? 123 <vector>)
    => #f)

;;; --------------------------------------------------------------------
;;; fields

  (check
      (let (((o <vector>) '#(1 2 3 4 5)))
	(o empty?))
    => #f)

  (check
      (let (((o <vector>) '#()))
	(o empty?))
    => #t)

  (check
      (let (((o <vector>) '#(1 2 3 4 5)))
	(o $empty?))
    => #f)

  (check
      (let (((o <vector>) '#()))
	(o $empty?))
    => #t)

  (check
      (let (((o <vector>) '#(1 2 3 4 5)))
	(o length))
    => 5)

  (check
      (let (((o <vector>) '#()))
	(o $length))
    => 0)

;;; --------------------------------------------------------------------
;;; methods

  (check
      (let (((o <vector>) '#(1 2 3 4 5)))
	(o map (lambda (n) (+ 1 n))))
    => '#(2 3 4 5 6))

  (check
      (let (((o <vector>) '#(1 2 3 4 5))
	    (r 0))
	(o for-each (lambda (n) (set! r (+ r n))))
	r)
    => (+ 1 2 3 4 5))

  (check
      (let (((o <vector>) (list->vector '(1 2 3 4 5))))
	(o fill! 1)
	o)
    => '#(1 1 1 1 1))

;;; --------------------------------------------------------------------

  (check
      (let (((o <vector>) '#()))
	(o for-all even?))
    => #t)

  (check
      (let (((o <vector>) '#(3 1 4 1 5 9)))
	(o for-all even?))
    => #f)

  (check
      (let (((o <vector>) '#(2 4 14)))
	(o for-all even?))
    => #t)

  (check
      (let (((o <vector>) '#(2 4 14)))
	(o for-all (lambda (n) (and (even? n) n))))
    => 14)

  (check
      (let (((o <vector>) '#(1 2 3)))
	(o for-all < '#(2 3 4)))
    => #t)

  (check
      (let (((o <vector>) '#(1 2 4)))
	(o for-all <  '#(2 3 4)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <vector>) '#(3 1 4 1 5 9)))
	(o exists even?))
    => #t)

  (check
      (let (((o <vector>)  '#(3 1 1 5 9)))
	(o exists even?))
    => #f)

  (check
      (let (((o <vector>)  '#()))
	(o exists even?))
    => #f)

  (check
      (let (((o <vector>)  '#(2 1 4 14)))
	(o exists (lambda (n) (and (even? n) n))))
    => 2)

  (check
      (let (((o <vector>)  '#(1 2 4)))
	(o exists < '#(2 3 4)))
    => #t)

  (check
      (let (((o <vector>)  '#(1 2 3)))
	(o exists > '#(2 3 4)))
    => #f)

;;; --------------------------------------------------------------------
;;; item ref

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o ref 0))
    => 1)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o ref 1))
    => 2)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o ref -1))
    => 4)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o ref -2))
    => 3)

;;; --------------------------------------------------------------------
;;; getter

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o[0]))
    => 1)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o[1]))
    => 2)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o[-1]))
    => 4)

  (check
      (let (((o <vector>) '#(1 2 3 4)))
	(o[-2]))
    => 3)

;;; --------------------------------------------------------------------
;;; item set

  (check
      (receive-and-return ((o <vector>))
	  (vector 1 2 3 4)
	(o set! 0 9))
    => '#(9 2 3 4))

  (check
      (receive-and-return ((o <vector>))
	  (vector 1 2 3 4)
	(o set! 1 9))
    => '#(1 9 3 4))

  (check
      (receive-and-return ((o <vector>))
	(vector 1 2 3 4)
	(o set! -1 9))
    => '#(1 2 3 9))

  (check
      (receive-and-return ((o <vector>))
	  (vector 1 2 3 4)
	(o set! -2 9))
    => '#(1 2 9 4))

;;; --------------------------------------------------------------------
;;; setter, syntax 2

  (check
      (receive-and-return ((o <vector>)) (vector 1 2 3 4)
	(set! o[0] 9))
    => '#(9 2 3 4))

  (check
      (receive-and-return ((o <vector>)) (vector 1 2 3 4)
	(set! o[1] 9))
    => '#(1 9 3 4))

  (check
      (receive-and-return ((o <vector>)) (vector 1 2 3 4)
	(set! o[-1] 9))
    => '#(1 2 3 9))

  (check
      (receive-and-return ((o <vector>)) (vector 1 2 3 4)
	(set! o[-2] 9))
    => '#(1 2 9 4))

  #t)


(parametrise ((check-test-name	'bytevectors))

  (check
      ((<bytevector>) '#vu8(1 2))
    => #t)

  (check
      ((<bytevector>) '#vu8())
    => #t)

  (check
      ((<bytevector>) '(1 2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((bv <bytevector>) '#vu8(1 2)))
	(bv copy))
    => '#vu8(1 2))

  #f)


(parametrise ((check-test-name	'bytevectors-u8))

  (check
      (let* (((A <bytevector-u8>) '#vu8(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u8>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-u8>) '#vu8(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u8>) (A copy)))
	(set! B[1] 29)
	(B[1]))
    => 29)

  #f)

(parametrise ((check-test-name	'bytevectors-s8))

  (check
      (let* (((A <bytevector-s8>) '#vs8(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s8>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-s8>) '#vs8(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s8>) (A copy)))
	(set! B[1] -29)
	(B[1]))
    => -29)

  #t)


(parametrise ((check-test-name	'bytevectors-u16))

  (check
      (let* (((A <bytevector-u16n>) '#vu16n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u16n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-u16n>) '#vu16n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u16n>) (A copy)))
	(set! B[1] 29)
	(B[1]))
    => 29)

  #f)

(parametrise ((check-test-name	'bytevectors-s16))

  (check
      (let* (((A <bytevector-s16n>) '#vs16n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s16n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-s16n>) '#vs16n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s16n>) (A copy)))
	(set! B[1] -29)
	(B[1]))
    => -29)

  #t)


(parametrise ((check-test-name	'bytevectors-u32))

  (check
      (let* (((A <bytevector-u32n>) '#vu32n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u32n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-u32n>) '#vu32n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u32n>) (A copy)))
	(set! B[1] 29)
	(B[1]))
    => 29)

  #f)

(parametrise ((check-test-name	'bytevectors-s32))

  (check
      (let* (((A <bytevector-s32n>) '#vs32n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s32n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-s32n>) '#vs32n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s32n>) (A copy)))
	(set! B[1] -29)
	(B[1]))
    => -29)

  #t)


(parametrise ((check-test-name	'bytevectors-u64))

  (check
      (let* (((A <bytevector-u64n>) '#vu64n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u64n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-u64n>) '#vu64n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-u64n>) (A copy)))
	(set! B[1] 29)
	(B[1]))
    => 29)

  #f)

(parametrise ((check-test-name	'bytevectors-s64))

  (check
      (let* (((A <bytevector-s64n>) '#vs64n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s64n>) (A copy)))
	(B[1]))
    => 20)

  (check
      (let* (((A <bytevector-s64n>) '#vs64n(10 20 30 40 50 60 70 80))
	     ((B <bytevector-s64n>) (A copy)))
	(set! B[1] -29)
	(B[1]))
    => -29)

  #t)


(parametrise ((check-test-name	'bytevectors-single))

  (check
      (let* (((A <bytevector-singlen>) '#vf4n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-singlen>) (A copy)))
	(B[1]))
    => 20.)

  (check
      (let* (((A <bytevector-singlen>) '#vf4n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-singlen>) (A copy)))
	(set! B[1] 29.)
	(B[1]))
    => 29.)

  #f)

(parametrise ((check-test-name	'bytevectors-single))

  (check
      (let* (((A <bytevector-singlen>) '#vf4n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-singlen>) (A copy)))
	(B[1]))
    => 20.)

  (check
      (let* (((A <bytevector-singlen>) '#vf4n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-singlen>) (A copy)))
	(set! B[1] -29.)
	(B[1]))
    => -29.)

  #t)


(parametrise ((check-test-name	'bytevectors-double))

  (check
      (let* (((A <bytevector-doublen>) '#vf8n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-doublen>) (A copy)))
	(B[1]))
    => 20.)

  (check
      (let* (((A <bytevector-doublen>) '#vf8n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-doublen>) (A copy)))
	(set! B[1] 29.)
	(B[1]))
    => 29.)

  #f)

(parametrise ((check-test-name	'bytevectors-double))

  (check
      (let* (((A <bytevector-doublen>) '#vf8n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-doublen>) (A copy)))
	(B[1]))
    => 20.)

  (check
      (let* (((A <bytevector-doublen>) '#vf8n(10. 20. 30. 40. 50. 60. 70. 80.))
	     ((B <bytevector-doublen>) (A copy)))
	(set! B[1] -29.)
	(B[1]))
    => -29.)

  #t)


(parametrise ((check-test-name	'hashtables))

  (check
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	((<hashtable>) T))
    => #t)

  (check
      (let (((T <hashtable>) (<hashtable-eqv> ((1 #\a) (2 #\b)))))
	((<hashtable>) T))
    => #t)

  (check
      (let (((T <hashtable>) (<string-hashtable> (("1" #\a) ("2" #\b)))))
	((<hashtable>) T))
    => #t)

  (check
      (let (((T <hashtable>) (<string-ci-hashtable> (("1" #\a) ("2" #\b)))))
	((<hashtable>) T))
    => #t)

  (check
      (let (((T <hashtable>) (<symbol-hashtable> (('a #\a) ('b #\b)))))
	((<hashtable>) T))
    => #t)

;;; --------------------------------------------------------------------
;;; fields

  (check
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T size))
    => 2)

;;; --------------------------------------------------------------------
;;; setter, getter

  (check
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(set! (T[1]) #\C)
	(T [1]))
    => #\C)

  (check
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T [3]))
    => (void))

  (check
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T [3] (#f)))
    => #f)

;;; --------------------------------------------------------------------
;;; methods

  (check	;delete!
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T delete! 1)
	(T [1]))
    => (void))

  (check	;clear!
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T clear!)
	(T size))
    => 0)

  (check	;contains?
      (let (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b)))))
	(T contains? 1))
    => #t)

  (check	;copy
      (let* (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b))))
	     ((R <hashtable>) (T copy)))
	(T[1]))
    => #\a)

  (check	;copy
      (let* (((T <hashtable>) (<hashtable-eq> ((1 #\a) (2 #\b))))
	     ((R <hashtable>) (T copy #t)))
	(T mutable?))
    => #t)

  #t)


(parametrise ((check-test-name	'rtd))

  (define-record-type <alpha>
    (fields (mutable a)
	    (immutable b)))

;;; --------------------------------------------------------------------

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	((<record-type-descriptor>) T))
    => #t)

;;; --------------------------------------------------------------------
;;; fields

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T name))
    => '<alpha>)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T parent))
    => #f)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T uid))
    => #f)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T generative?))
    => #t)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T sealed?))
    => #f)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T opaque?))
    => #f)

;;; --------------------------------------------------------------------
;;; methods

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(define R (make-<alpha> 1 2))
	((T predicate) R))
    => #t)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(define R (make-<alpha> 1 2))
	((T accessor 0) R))
    => 1)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(define R (make-<alpha> 1 2))
	((T mutator 0) R 99)
	((T accessor 0) R))
    => 99)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T field-mutable? 0))
    => #t)

  (check
      (let (((T <record-type-descriptor>) (record-type-descriptor <alpha>)))
	(T field-mutable? 1))
    => #f)

  #t)


(parametrise ((check-test-name	'record))

  (define-record-type <alpha>
    (fields (mutable a)
	    (immutable b)))

;;; --------------------------------------------------------------------

  (check
      (let (((R <record>) (make-<alpha> 1 2)))
	(R rtd))
    => (record-type-descriptor <alpha>))

  #t)


(parametrise ((check-test-name	'conditions))

  (define-condition-type &ciao
      &condition
    make-ciao
    ciao?)

;;; --------------------------------------------------------------------

  (check
      ((<condition>) (make-ciao))
    => #t)

  (check
      (let (((C <condition>) (<condition> ((make-ciao)))))
	(for-all ciao? (C simple)))
    => #t)

  (check
      (let (((C <condition>) (<condition> ((make-ciao) (make-ciao)))))
	(for-all ciao? (C simple)))
    => #t)

  #t)


(parametrise ((check-test-name	'transcoders))

  (check
      ((<transcoder>) (native-transcoder))
    => #t)

  (check
      (let (((T <transcoder>) (native-transcoder)))
	(T codec))
    => (transcoder-codec (native-transcoder)))

  (check
      (let (((T <transcoder>) (native-transcoder)))
	(T eol-style))
    => (transcoder-eol-style (native-transcoder)))

  (check
      (let (((T <transcoder>) (native-transcoder)))
	(T error-handling-mode))
    => (transcoder-error-handling-mode (native-transcoder)))

  #t)


(parametrise ((check-test-name	'ports))

  (define (make-binary-input-port)
    (open-bytevector-input-port '#vu8()))

  (define (make-binary-output-port)
    (receive (port getter)
	(open-bytevector-output-port)
      port))

  (define (make-textual-input-port)
    (open-string-input-port ""))

  (define (make-textual-output-port)
    (receive (port getter)
	(open-string-output-port)
      port))

;;; --------------------------------------------------------------------
;;; predicates

  (check ((<port>) stdin)		=> #t)
  (check ((<input-port>) stdin)		=> #t)
  (check ((<output-port>) stdin)	=> #f)
  (check ((<input/output-port>) stdin)	=> #f)
  (check ((<textual-port>) stdin)	=> #t)
  (check ((<binary-port>) stdin)	=> #f)

  (check ((<textual-input-port>) stdin)		=> #t)
  (check ((<textual-output-port>) stdout)	=> #t)

  (check
      ((<binary-input-port>) (make-binary-input-port))
    => #t)

  (check
      ((<binary-output-port>) (make-binary-output-port))
    => #t)

  (check
      ((<textual-input-port>) (make-textual-input-port))
    => #t)

  (check
      ((<textual-output-port>) (make-textual-output-port))
    => #t)

;;; --------------------------------------------------------------------
;;; virtual fields

  (check
      (let (((P <port>) stdin))
	((<transcoder>) (P transcoder)))
    => #t)

  (check
      (let (((P <port>) stdin))
	(P textual?))
    => #t)

  (check
      (let (((P <port>) stdin))
	(P input?))
    => #t)

  (check
      (let (((P <port>) stdout))
	(P output?))
    => #t)

  (check
      (let (((P <port>) stdout))
	(P binary?))
    => #f)

  (check
      (let (((P <port>) stdout))
	(P closed?))
    => #f)

  (check
      (let (((P <port>) stdout))
	(P fd))
    => 1)

  (check
      (let (((P <binary-input-port>) (make-binary-input-port)))
	(P eof?))
    => #t)

  (check
      (let (((P <port>) stdin))
	(P id))
    => "*stdin*")

  (check
      (let (((P <port>) stdin))
	(symbol? (P uid)))
    => #t)

  (check
      (let (((P <port>) stdin))
	(integer? (P hash)))
    => #t)

  (check
      (let (((P <output-port>) stderr))
	(P buffer-mode))
    => 'line)

  #t)


(parametrise ((check-test-name	'numbers))

  (check
      (let (((o <real>) -123))
	(o abs))
    => 123)

  (check
      (let (((o <real>) 123))
	(o abs))
    => 123)

  (check
      (let (((o <real>) 123))
	(o string))
    => "123")

;;; --------------------------------------------------------------------

  (check
      (let (((o <real-valued>) -123))
	(o positive?))
    => #f)

  (check
      (let (((o <real-valued>) -123))
	(o negative?))
    => #t)

  (check
      (let (((o <real-valued>) -123))
	(o non-positive?))
    => #t)

  (check
      (let (((o <real-valued>) -123))
	(o non-negative?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((o <real-valued>) -123))
	(o sign))
    => -1)

  (check
      (let (((o <real-valued>) 123))
	(o sign))
    => 1)

;;; --------------------------------------------------------------------
;;; methods

  (check
      (let (((N <number>) 1.2))
	((<number>) (N sin)))
    => #t)

  #t)


(parametrise ((check-test-name	'pointers))

  (check
      ((<pointer>) (<pointer> (123)))
    => #t)

  (check
      (let (((P <pointer>) (<pointer> (123))))
	(P integer))
    => 123)

  (check
      (let* (((P <pointer>) (<pointer> (123)))
	     ((Q <pointer>) (P clone)))
	(P = Q))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
