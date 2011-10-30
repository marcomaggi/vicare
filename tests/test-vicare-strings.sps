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
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (ikarus)
		(parameterize	parametrise))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare string functions\n")


;;;; syntax helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (pretty-print (condition-message E)))
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

;;;FIXME
  #;(check
      (catch #t
	(string-ref "" #\a))
    => '(#\a))

  (check
      (catch #f
	(string-ref "" -1))
    => '(-1))

  (check
      (catch #f
	(string-ref "" (+ 1 (greatest-fixnum))))
    => (list (+ 1 (greatest-fixnum))))

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
    => '(#\a))

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) -1 #\a))
    => '(-1))

  (check
      (catch #f
	(string-set! (string #\a #\b #\c) (+ 1 (greatest-fixnum)) #\a))
    => (list (+ 1 (greatest-fixnum))))

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
;;;		 (pretty-print (condition-message E))
		 (condition-irritants E))
		(else E))
	(string->latin1 123))
    => '(123))

  (check
      (guard (E ((assertion-violation? E)
;;;		 (pretty-print (condition-message E))
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


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
