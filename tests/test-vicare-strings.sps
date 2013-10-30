;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare
;;;Contents: tests for "ikarus.strings.ss"
;;;Date: Sat Oct 29, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (ikarus)
  (ikarus system $strings)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare string functions\n")


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


(parametrise ((check-test-name	'string-length))

  (check
      (string-length "")
    => 0)

  (check
      (string-length "a")
    => 1)

  (check
      (string-length "abc")
    => 3)

;;; --------------------------------------------------------------------

  (check
      (catch #f
	(string-length 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (string-empty? "")
    => #t)

  (check
      (string-empty? "123")
    => #f)

  #t)


(parametrise ((check-test-name	'string-ref))

  (check
      (string-ref "a" 0)
    => #\a)

  (check
      (string-ref "abc" 2)
    => #\c)

;;; --------------------------------------------------------------------
;;; arguments validation: string

  (check
      (catch #f
	(string-ref 123 1))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  ;;The tests  commented out  trigger an error  while compiling  (in the
  ;;source optimization phase or the code generation phase).

  ;; (check
  ;;     (catch #f
  ;; 	(string-ref "ciao" #\d))
  ;;   => '(#\d))

  ;; (check
  ;;     (catch #f
  ;; 	(string-ref "ciao" 'd))
  ;;   => '(d))

  ;; (check
  ;;     (catch #f
  ;; 	(string-ref "ciao" "d"))
  ;;   => '("d"))

  ;; (check
  ;;     (catch #f
  ;; 	(string-ref "" -1))
  ;;   => '(-1))

  ;; (check
  ;;     (catch #f
  ;; 	(string-ref "" (+ 1 (greatest-fixnum))))
  ;;   => (list (+ 1 (greatest-fixnum))))

  (check
      (catch #f
	(string-ref "" 0))
    => '(0 ""))

  (check
      (catch #f
	(string-ref "abc" 10))
    => '(10 "abc"))

  #t)


(parametrise ((check-test-name	'string-set-bang))

  (check
      (let ((str (string #\a #\b #\c)))
	(string-set! str 0 #\9)
	str)
    => "9bc")

  (check
      (let ((str (string #\a #\b #\c)))
	(string-set! str 1 #\9)
	str)
    => "a9c")

  (check
      (let ((str (string #\a #\b #\c)))
	(string-set! str 2 #\9)
	str)
    => "ab9")

;;; --------------------------------------------------------------------
;;; arguments validation: string

  (check
      (catch #f
	(string-set! 123 1 #\a))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: index

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) #\a #\b))
    => '(#\a "abc"))

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) -1 #\a))
    => '(-1 "abc"))

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) (+ 1 (greatest-fixnum)) #\a))
    => (list (+ 1 (greatest-fixnum)) "abc"))

  (check
      (catch #f
	(string-set! (string) 0 #\a))
    => '(0 ""))

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) 10 #\9))
    => `(10 "abc"))

  #t)


(parametrise ((check-test-name	'make-string))

  (check
      (make-string 0)
    => "")

  (check
      (make-string 1)
    => "\x0;")

  (check
      (make-string 3)
    => "\x0;\x0;\x0;")

;;; --------------------------------------------------------------------

  (check
      (make-string 0 #\A)
    => "")

  (check
      (make-string 1 #\A)
    => "A")

  (check
      (make-string 3 #\A)
    => "AAA")

;;; --------------------------------------------------------------------
;;; arguments validation: length

  (check	;not a number
      (catch #f
	(make-string #t #\A))
    => '(#t))

  (check	;negative
      (catch #f
	(make-string -1 #\A))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(make-string (+ 1 (greatest-fixnum)) #\A))
    => (list (+ 1 (greatest-fixnum))))

;;; --------------------------------------------------------------------
;;; arguments validation: character

  (check	;not a character
      (catch #f
	(make-string 1 #t))
    => '(#t))

  #t)


(parametrise ((check-test-name	'string))

  (check
      (string)
    => "")

  (check
      (string #\a)
    => "a")

  (check
      (string #\a #\b #\c)
    => "abc")

  (check
      (string #\a #\b #\c #\d)
    => "abcd")

  (check
      (string #\a #\b #\c #\d #\e #\f #\g)
    => "abcdefg")

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(string 1))
    => '(1))

  (check
      (catch #f
	(string #\a 1))
    => '(1))

  (check
      (catch #f
	(string #\a #\b 1))
    => '(1))

  (check
      (catch #f
	(string #\a #\b #\c 1))
    => '(1))

  (check
      (catch #f
	(string #\a #\b #\c #\d 1))
    => '(1))

  (check
      (catch #f
	(string #\a #\b #\c #\d #\e 1))
    => '(1))

;;; --------------------------------------------------------------------
;;; unsafe operations

  (check
      ($string)
    => "")

  (check
      ($string #\a)
    => "a")

  (check
      ($string #\a #\b #\c)
    => "abc")

  (check
      ($string #\a #\b #\c #\d)
    => "abcd")

  (check
      ($string #\a #\b #\c #\d #\e #\f #\g)
    => "abcdefg")

  #t)


(parametrise ((check-test-name	'substring))

  (check
      (substring "" 0 0)
    => "")

  (check
      (substring "a" 0 0)
    => "")

  (check
      (substring "abc" 0 0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (substring "a" 1 1)
    => "")

  (check
      (substring "abc" 1 1)
    => "")

  (check
      (substring "abc" 3 3)
    => "")

;;; --------------------------------------------------------------------

  (check
      (substring "a" 0 1)
    => "a")

  (check
      (substring "abc" 1 2)
    => "b")

  (check
      (substring "abc" 2 3)
    => "c")

;;; --------------------------------------------------------------------

  (check
      (substring "abc" 0 3)
    => "abc")

;;; --------------------------------------------------------------------
;;; arguments validation: string

  (check
      (catch #f
	(substring 123 1 1))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: start index

  (check	;not a number
      (catch #f
	(substring "abcd" #t 1))
    => '(#t))

  (check	;negative
      (catch #f
	(substring "abcd" -1 1))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(substring "abcd" (+ 1 (greatest-fixnum)) 1))
    => (list (+ 1 (greatest-fixnum))))

  (check	;too big for string
      (catch #f
	(substring "abcd" 5 6))
    => '(5 4))

;;; --------------------------------------------------------------------
;;; arguments validation: end index

  (check	;not a number
      (catch #f
	(substring "abcd" 1 #t))
    => '(#t))

  (check	;negative
      (catch #f
	(substring "abcd" 1 -1))
    => '(-1))

  (check	;not a fixnum
      (catch #f
	(substring "abcd" 1 (+ 1 (greatest-fixnum))))
    => (list (+ 1 (greatest-fixnum))))

  (check	;too big for string
      (catch #f
	(substring "abcd" 2 6))
    => '(6 4))

;;; --------------------------------------------------------------------
;;; arguments validation: indexes

  (check	;incorrect order
      (catch #f
	(substring "abcd" 2 1))
    => '(2 1))

  #t)


(parametrise ((check-test-name	'string-copy))

  (check
      (string-copy "")
    => "")

  (check
      (string-copy "a")
    => "a")

  (check
      (string-copy "abc")
    => "abc")

;;; --------------------------------------------------------------------
;;; arguments validation: string

  (check
      (catch #f
	(string-copy 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'string-equal))

  (check
      (string=? "" "")
    => #t)

  (check
      (string=? "" "" "")
    => #t)

  (check
      (string=? "a" "a")
    => #t)

  (check
      (string=? "a" "a" "a")
    => #t)

  (check
      (string=? "abc" "abc")
    => #t)

  (check
      (string=? "abc" "abc" "abc")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string=? "a" "")
    => #f)

  (check
      (string=? "" "a")
    => #f)

  (check
      (string=? "a" "" "")
    => #f)

  (check
      (string=? "" "a" "")
    => #f)

  (check
      (string=? "" "" "a")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string=? "abc" "a")
    => #f)

  (check
      (string=? "a" "abc")
    => #f)

  (check
      (string=? "a" "abc" "abc")
    => #f)

  (check
      (string=? "abc" "a" "abc")
    => #f)

  (check
      (string=? "abc" "abc" "a")
    => #f)

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(string=? 123 ""))
    => '(123))

  (check
      (catch #f
	(string=? "" 123))
    => '(123))

  (check
      (catch #f
	(string=? "" "" 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'string-cmp))

  (check-for-false
   (string<? "abcd" "abcd"))

  (check-for-true
   (string<? "abc" "abcd"))

  (check-for-false
   (string<? "abcd" "abc"))

  (check-for-true
   (string<? "ABcd" "abcd"))

  (check-for-false
   (string<? "abcd" "a2cd"))

  (check-for-true
   (string<? "abc" "abcd" "abcde"))

  (check-for-false
   (string<? "abc" "abcde" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string<=? "abcd" "abcd"))

  (check-for-true
   (string<=? "abc" "abcd"))

  (check-for-false
   (string<=? "abcd" "abc"))

  (check-for-true
   (string<=? "ABcd" "abcd"))

  (check-for-false
   (string<=? "abcd" "a2cd"))

  (check-for-true
   (string<=? "abc" "abcd" "abcde"))

  (check-for-false
   (string<=? "abc" "abcde" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (string>? "abcd" "abcd"))

  (check-for-true
   (string>? "abcd" "abc"))

  (check-for-false
   (string>? "abc" "abcd"))

  (check-for-true
   (string>? "abcd" "ABcd"))

  (check-for-false
   (string>? "a2cd" "abcd"))

  (check-for-true
   (string>? "abcde" "abcd" "abc"))

  (check-for-false
   (string>? "abcd" "abcde" "abc"))

;;; --------------------------------------------------------------------

  (check-for-true
   (string>=? "abcd" "abcd"))

  (check-for-true
   (string>=? "abcd" "abc"))

  (check-for-false
   (string>=? "abc" "abcd"))

  (check-for-true
   (string>=? "abcd" "ABcd"))

  (check-for-false
   (string>=? "a2cd" "abcd"))

  (check-for-true
   (string>=? "abcde" "abcd" "abc"))

  (check-for-false
   (string>=? "abcd" "abcde" "abc"))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(string<? 123 "abc"))
    => '(123))

  (check
      (catch #f
	(string<? "abc" 123))
    => '(123))

  (check
      (catch #f
	(string<? "abc" "def" 123))
    => '(123))

  (check
      (catch #f
	(string<=? 123 "abc"))
    => '(123))

  (check
      (catch #f
	(string<=? "abc" 123))
    => '(123))

  (check
      (catch #f
	(string<=? "abc" "def" 123))
    => '(123))

  (check
      (catch #f
	(string>? 123 "abc"))
    => '(123))

  (check
      (catch #f
	(string>? "abc" 123))
    => '(123))

  (check
      (catch #f
	(string>? "abc" "def" 123))
    => '(123))

  (check
      (catch #f
	(string>=? 123 "abc"))
    => '(123))

  (check
      (catch #f
	(string>=? "abc" 123))
    => '(123))

  (check
      (catch #f
	(string>=? "abc" "def" 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'string-to-list))

  (check
      (string->list "")
    => '())

  (check
      (string->list "a")
    => '(#\a))

  (check
      (string->list "abc")
    => '(#\a #\b #\c))

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(string->list 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'list-to-string))

  (check
      (list->string '())
    => "")

  (check
      (list->string '(#\a))
    => "a")

  (check
      (list->string '(#\a #\b #\c))
    => "abc")

;;; --------------------------------------------------------------------
;;; arguments validation

  (check	;not a list
      (catch #f
	(list->string 123))
    => '(123))

  (check	;not a list of chars
      (catch #f
	(list->string '(1)))
    => '(1))

  (check	;not a list of chars
      (catch #f
	(list->string '(#\a 1)))
    => '(1))

  (check	;not a proper list
      (catch #f
	(list->string '(#\a #\b . #\c)))
    => '((#\a #\b . #\c)))

  (check	;not a proper list
      (catch #f
	(list->string '(#\a . #\c)))
    => '((#\a . #\c)))

  (let ((circ '#0=(#\a #\b #\c . #0#)))
    (check	;circular list
	(catch #f
	  (list->string circ))
      => (list circ)))

  ;;This consumes too much memory on my small computer (Marco Maggi; Oct
  ;;30, 2011).
  ;;
  ;; (let* ((tail (make-list (greatest-fixnum) #\a))
  ;; 	 (ell  (cons #\a tail)))
  ;;   (check
  ;; 	(catch #f
  ;; 	  (list->string ell))
  ;;     => ell))

  #t)


(parametrise ((check-test-name	'string-append))

  (check
      (string-append)
    => "")

  (check
      (string-append "")
    => "")

  (check
      (string-append "" "" "")
    => "")

  (check
      (string-append "" "" "" "")
    => "")

  (check
      (string-append "" "" "" "" "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-append "a")
    => "a")

  (check
      (string-append "a" "b")
    => "ab")

  (check
      (string-append "a" "b" "c")
    => "abc")

  (check
      (string-append "a" "b" "c" "d")
    => "abcd")

  (check
      (string-append "a" "b" "c" "d" "e")
    => "abcde")

;;; --------------------------------------------------------------------

  (check
      (string-append "abc")
    => "abc")

  (check
      (string-append "abc" "def")
    => "abcdef")

  (check
      (string-append "abc" "def" "ghi")
    => "abcdefghi")

  (check
      (string-append "abc" "def" "ghi" "lmn")
    => "abcdefghilmn")

  (check
      (string-append "abc" "def" "ghi" "lmn" "opq")
    => "abcdefghilmnopq")

;;; --------------------------------------------------------------------
;;; arguments validation

  (check
      (catch #f
	(string-append 123))
    => '(123))

  (check
      (catch #f
	(string-append "a" 123))
    => '(123))

  (check
      (catch #f
	(string-append "a" "b" 123))
    => '(123))

  (check
      (catch #f
	(string-append "a" "b" "c" 123))
    => '(123))

  (check
      (catch #f
	(string-append "a" "b" "c" "d" 123))
    => '(123))

  (check
      (catch #f
	(string-append "a" "b" "c" "d" "e" 123))
    => '(123))


  #t)


(parametrise ((check-test-name	'reverse-and-concatenate))

;;; arguments validation

  (check
      (catch #f (string-reverse-and-concatenate 123))
    => '(123))

  (check
      (catch #f (string-reverse-and-concatenate '(123)))
    => '((123)))

;;; --------------------------------------------------------------------

  (check
      (string-reverse-and-concatenate '())
    => "")

  (check
      (string-reverse-and-concatenate '(""))
    => "")

  (check
      (string-reverse-and-concatenate '("" ""))
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-reverse-and-concatenate '("123"))
    => "123")

  (check
      (string-reverse-and-concatenate '("456" "123"))
    => "123456")

  (check
      (string-reverse-and-concatenate '("789" "456" "123"))
    => "123456789")

  #t)


(parametrise ((check-test-name	'string-for-each))

  (check
      (with-result
       (string-for-each (lambda (ch)
			  (add-result ch))
	 ""))
    => `(,(void) ()))

  (check
      (with-result
       (string-for-each (lambda (ch)
			  (add-result ch))
	 "a"))
    => `(,(void) (#\a)))

  (check
      (with-result
       (string-for-each (lambda (ch)
			  (add-result ch))
	 "abc"))
    => `(,(void) (#\a #\b #\c)))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 "" ""))
    => `(,(void) ()))

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 "a" "b"))
    => `(,(void) ((#\a . #\b))))

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2)
			  (add-result (cons ch1 ch2)))
	 "abc" "def"))
    => `(,(void) ((#\a . #\d)
		  (#\b . #\e)
		  (#\c . #\f))))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 "" "" ""))
    => `(,(void) ()))

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 "a" "b" "c"))
    => `(,(void) ((#\a #\b #\c))))

  (check
      (with-result
       (string-for-each (lambda (ch1 ch2 ch3)
			  (add-result (list ch1 ch2 ch3)))
	 "abc" "def" "ghi"))
    => `(,(void) ((#\a #\d #\g)
		  (#\b #\e #\h)
		  (#\c #\f #\i))))

;;; --------------------------------------------------------------------
;;; arguments validation: procedure

  (check
      (catch #f
	(string-for-each 123 ""))
    => '(123))

  (check
      (catch #f
	(string-for-each 123 "" ""))
    => '(123))

  (check
      (catch #f
	(string-for-each 123 "" "" ""))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: strings

  (check
      (catch #f
	(string-for-each values 123))
    => '(123))

  (check
      (catch #f
	(string-for-each values "" 123))
    => '(123))

  (check
      (catch #f
	(string-for-each values "" "" 123))
    => '(123))

  (check
      (catch #f
	(string-for-each values "" "" "" 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'string-fill-bang))

  (check
      (string-fill! "" #\a)
    => "")

  (check
      (string-fill! (string #\b) #\a)
    => "a")

  (check
      (string-fill! (string #\b #\c #\d) #\a)
    => "aaa")

;;; --------------------------------------------------------------------
;;; arguments validation: string

  (check
      (catch #f
	(string-fill! 123 #\a))
    => '(123))

;;; --------------------------------------------------------------------
;;; arguments validation: filler

  (check
      (catch #f
	(string-fill! "" 123))
    => '(123))

  #t)


(parametrise ((check-test-name	'string-copy-bang))

  (check
      (let ((dst ""))
	(string-copy! "" 0 dst 0 0)
	dst)
    => "")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "" 0 dst 0 0)
	dst)
    => "abcdefghilm")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "ABC" 0 dst 0 3)
	dst)
    => "ABCdefghilm")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "ABC" 0 dst 1 3)
	dst)
    => "aABCefghilm")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "ABC" 0 dst 7 3)
	dst)
    => "abcdefgABCm")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "ABC" 0 dst 8 3)
	dst)
    => "abcdefghABC")

  (check
      (let ((dst (string-copy "abcdefghilm")))
	(string-copy! "012ABC" 3 dst 8 3)
	dst)
    => "abcdefghABC")

;;; --------------------------------------------------------------------
;;; overlapping source and destination, forwards copy

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 0 str 3 3)
	str)
    => "abcabcghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 0 str 2 3)
	str)
    => "ababcfghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 0 str 1 3)
	str)
    => "aabcefghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 0 str 0 3)
	str)
    => "abcdefghilm")

;;; --------------------------------------------------------------------
;;; overlapping source and destination, backwards copy

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 3 str 0 3)
	str)
    => "defdefghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 2 str 0 3)
	str)
    => "cdedefghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 1 str 0 3)
	str)
    => "bcddefghilm")

  (check
      (let ((str (string-copy "abcdefghilm")))
	(string-copy! str 0 str 0 3)
	str)
    => "abcdefghilm")

  #t)


(parametrise ((check-test-name	'latin1))

  (define test-string
    (let* ((str.len 256)
	   (str     (make-string str.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i str.len)
	   str)
	(string-set! str i (integer->char i)))))

  (define test-bytevector
    (let* ((bv.len 256)
	   (bv     (make-bytevector bv.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i bv.len)
	   bv)
	(bytevector-u8-set! bv i i))))

;;; --------------------------------------------------------------------
;;; argument check

  (check
      (guard (E ((assertion-violation? E)
;;;		 (check-pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(string->latin1 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (check-pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(latin1->string 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (string->latin1 test-string)
    => test-bytevector)

  (check
      (latin1->string test-bytevector)
    => test-string)

  #t)


(parametrise ((check-test-name	'ascii))

  (define test-string
    (let* ((str.len 128)
	   (str     (make-string str.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i str.len)
	   str)
	(string-set! str i (integer->char i)))))

  (define test-bytevector
    (let* ((bv.len 128)
	   (bv     (make-bytevector bv.len)))
      (do ((i 0 (+ 1 i)))
	  ((= i bv.len)
	   bv)
	(bytevector-u8-set! bv i i))))

;;; --------------------------------------------------------------------
;;; argument check

  (check
      (guard (E ((assertion-violation? E)
;;;		 (check-pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(string->ascii 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (check-pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(ascii->string 123))
    => '(123))

;;; --------------------------------------------------------------------

  (check
      (string->ascii test-string)
    => test-bytevector)

  (check
      (ascii->string test-bytevector)
    => test-string)

  #t)


(parametrise ((check-test-name	'utf-16))

  (define test-string "ciao \x1000;")

;;; --------------------------------------------------------------------

  (check
      (utf16le->string (string->utf16le test-string))
    => test-string)

  (check
      (utf16be->string (string->utf16be test-string))
    => test-string)

  (check
      (utf16n->string (string->utf16n test-string))
    => test-string)

;;; --------------------------------------------------------------------

  (check
      (utf16le->string (string->utf16 test-string (endianness little)))
    => test-string)

  (check
      (utf16be->string (string->utf16 test-string (endianness big)))
    => test-string)

  (check
      (utf16n->string (string->utf16 test-string (native-endianness)))
    => test-string)

;;; --------------------------------------------------------------------

  (check
      (utf16->string (string->utf16le test-string) (endianness little))
    => test-string)

  (check
      (utf16->string (string->utf16be test-string) (endianness big))
    => test-string)

  (check
      (utf16->string (string->utf16n test-string) (native-endianness))
    => test-string)

  #t)


(parametrise ((check-test-name	'hex))

  (check
      (ascii->string (bytevector->hex (string->ascii "ciao mamma")))
    => "6369616F206D616D6D61")

  (check
      (bytevector->hex '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    => (string->ascii "000102030405060708090A0B0C0D0E0F"))

  (check
      (bytevector->hex '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
    => (string->ascii "101112131415161718191A1B1C1D1E1F"))

;;; --------------------------------------------------------------------
;;; hex->bytevector

  (check	;upper case
      (ascii->string (hex->bytevector (string->ascii "6369616F206D616D6D61")))
    => "ciao mamma")

  (check	;lower case
      (ascii->string (hex->bytevector (string->ascii "6369616f206d616d6d61")))
    => "ciao mamma")

  (check	;upper case
      (hex->bytevector (string->ascii "000102030405060708090A0B0C0D0E0F"))
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (check	;lower case
      (hex->bytevector (string->ascii "000102030405060708090a0b0c0d0e0f"))
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (check	;upper case
      (hex->bytevector (string->ascii "101112131415161718191A1B1C1D1E1F"))
    => '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

  (check	;lower case
      (hex->bytevector (string->ascii "101112131415161718191a1b1c1d1e1f"))
    => '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

  (check	;invalid input
      (hex->bytevector (string->ascii "101112131415161718191a1Z1c1d1e1f"))
;;;                                                           ^
    => #f)

;;; --------------------------------------------------------------------
;;; bytevector->string-hex

  (check
      (bytevector->string-hex (string->ascii "ciao mamma"))
    => "6369616F206D616D6D61")

  (check
      (bytevector->string-hex '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    => "000102030405060708090A0B0C0D0E0F")

  (check
      (bytevector->string-hex '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
    => "101112131415161718191A1B1C1D1E1F")

;;; --------------------------------------------------------------------
;;; string-hex->bytevector

  (check	;upper case
      (string-hex->bytevector "6369616F206D616D6D61")
    => (string->ascii "ciao mamma"))

  (check	;lower case
      (string-hex->bytevector "6369616f206d616d6d61")
    => (string->ascii "ciao mamma"))

  (check	;upper case
      (string-hex->bytevector "000102030405060708090A0B0C0D0E0F")
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (check	;lower case
      (string-hex->bytevector "000102030405060708090a0b0c0d0e0f")
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

  (check	;upper case
      (string-hex->bytevector "101112131415161718191A1B1C1D1E1F")
    => '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

  (check	;lower case
      (string-hex->bytevector "101112131415161718191a1b1c1d1e1f")
    => '#vu8(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))

  (check	;invalid input
      (string-hex->bytevector "101112131415161718191a1Z1c1d1e1f")
;;;                                                   ^
    => #f)

  #t)


(parametrise ((check-test-name	'base64))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?ascii ?base64)
       (begin
	 (check
	     (ascii->string (bytevector->base64 (string->ascii ?ascii)))
	   => ?base64)
	 (check
	     (ascii->string (base64->bytevector (string->ascii ?base64)))
	   => ?ascii)
	 ))))

;;; --------------------------------------------------------------------

  (doit ""		"")
  (doit "ABC"		"QUJD")
  (doit "H"		"SA==")
  (doit "He"		"SGU=")
  (doit "Hel"		"SGVs")
  (doit "Hell"		"SGVsbA==")
  (doit "Hello"		"SGVsbG8=")
  (doit "this is a test\n"
	"dGhpcyBpcyBhIHRlc3QK")
  (doit "y"		"eQ==")
  (doit "yy"		"eXk=")
  (doit "y "		"eSA=")
  (doit "quickly "	"cXVpY2tseSA=")
  (doit "and"		"YW5k")
  (doit "an"		"YW4=")
  (doit "an "		"YW4g")

  (doit
   "The short red fox ran quickly"
   "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHk=")

  (doit
   "The short red fox ran quickly "
   "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkg")

  (doit
   "The short red fox ran quickly through the green field and jumped over the tall brown bear\n"
   "VGhlIHNob3J0IHJlZCBmb3ggcmFuIHF1aWNrbHkgdGhyb3VnaCB0aGUgZ3JlZW4gZmllbGQgYW5kIGp1bXBlZCBvdmVyIHRoZSB0YWxsIGJyb3duIGJlYXIK")

  (doit
   "Le Poete est semblable au prince des nuees Qui hante la tempete e se rit de l'archer; Exile sul le sol au milieu des huees, Ses ailes de geant l'empechent de marcher."
   "TGUgUG9ldGUgZXN0IHNlbWJsYWJsZSBhdSBwcmluY2UgZGVzIG51ZWVzIFF1aSBoYW50ZSBsYSB0ZW1wZXRlIGUgc2Ugcml0IGRlIGwnYXJjaGVyOyBFeGlsZSBzdWwgbGUgc29sIGF1IG1pbGlldSBkZXMgaHVlZXMsIFNlcyBhaWxlcyBkZSBnZWFudCBsJ2VtcGVjaGVudCBkZSBtYXJjaGVyLg==")

  #t)


(parametrise ((check-test-name	'uri-encoding))

  (let-syntax ((doit (syntax-rules ()
		       ((_ raw encoded)
			(begin
			  (check
			      (uri-encode (string->ascii raw))
			    => (string->ascii encoded))
			  (check
			      (uri-decode (string->ascii encoded))
			    => (string->ascii raw))
			  )))))

    (doit "." ".")
    (doit "-" "-")
    (doit "_" "_")
    (doit "~" "~")
    (doit "%" "%25")
    (doit "?" "%3F")
    (doit "=" "%3D")
    (doit "#" "%23")

    (doit "" "")
    (doit "ciao" "ciao")
    (doit "cia=o" "cia%3Do")
    (doit "ci?a=o" "ci%3Fa%3Do")

    (check
	(uri-encode (string->ascii "ciao"))
      => '#vu8(99 105 97 111))

    (check
	(uri-decode '#vu8(99 105 97 111))
      => '#vu8(99 105 97 111))

;;; --------------------------------------------------------------------

  (let ((all-octets '#vu8(0 1 2 3 4 5 6 7 8 9
			      10 11 12 13 14 15 16 17 18 19
			      20 21 22 23 24 25 26 27 28 29
			      30 31 32 33 34 35 36 37 38 39
			      40 41 42 43 44 45 46 47 48 49
			      50 51 52 53 54 55 56 57 58 59
			      60 61 62 63 64 65 66 67 68 69
			      70 71 72 73 74 75 76 77 78 79
			      80 81 82 83 84 85 86 87 88 89
			      90 91 92 93 94 95 96 97 98 99

			      100 101 102 103 104 105 106 107 108 109
			      110 111 112 113 114 115 116 117 118 119
			      120 121 122 123 124 125 126 127 128 129
			      130 131 132 133 134 135 136 137 138 139
			      140 141 142 143 144 145 146 147 148 149
			      150 151 152 153 154 155 156 157 158 159
			      160 161 162 163 164 165 166 167 168 169
			      170 171 172 173 174 175 176 177 178 179
			      180 181 182 183 184 185 186 187 188 189
			      190 191 192 193 194 195 196 197 198 199

			      200 201 202 203 204 205 206 207 208 209
			      210 211 212 213 214 215 216 217 218 219
			      220 221 222 223 224 225 226 227 228 229
			      230 231 232 233 234 235 236 237 238 239
			      240 241 242 243 244 245 246 247 248 249
			      250 251 252 253 254 255))
	  (all-string "%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~%7F%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF"))

      (check
	  (ascii->string (uri-encode all-octets))
	=> all-string)

      (check
	  (uri-decode (string->ascii all-string))
	=> all-octets))

    #f)

;;; --------------------------------------------------------------------

  (check
      (uri-normalise-encoding '#vu8())
    => '#vu8())

  (check
      (uri-normalise-encoding (string->ascii "ciao"))
    => (string->ascii "ciao"))

  (check
      (uri-normalise-encoding (string->ascii "cia%3do"))
    => (string->ascii "cia%3Do"))

  (check
      (uri-normalise-encoding (string->ascii "cia%3Do"))
    => (string->ascii "cia%3Do"))

  (check
      (uri-normalise-encoding (string->ascii "ci%3fa%3do"))
    => (string->ascii "ci%3Fa%3Do"))

  (check
      (uri-normalise-encoding (string->ascii "ci%3Fa%3Do"))
    => (string->ascii "ci%3Fa%3Do"))

  (check
      (uri-normalise-encoding (string->ascii "%7Eciao"))
    => (string->ascii "~ciao"))

  (check
      (uri-normalise-encoding (string->ascii "ci%5Fao"))
    => (string->ascii "ci_ao"))

  #t)


(parametrise ((check-test-name	'unsafe))

  (check
      ($string= "ciao" (string #\c #\i #\a #\o))
    => #t)

  (check
      ($string= "ciao" "ciao")
    => #t)

  (check
      (let ((S "ciao"))
	($string= S S))
    => #t)

  (check
      ($string= "ciao" "hello")
    => #f)

  #t)


(parametrise ((check-test-name	'conversion))

  (check
      (string->symbol "ciao")
    => 'ciao)

;;; --------------------------------------------------------------------

  (check
      (symbol->string 'ciao)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (string-or-symbol->string "ciao")
    => "ciao")

  (check
      (string-or-symbol->string 'ciao)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (string-or-symbol->symbol "ciao")
    => 'ciao)

  (check
      (string-or-symbol->symbol 'ciao)
    => 'ciao)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
