;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for SRFI 113
;;;Date: Sun Mar  8, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) John Cowan 2013.  All Rights Reserved.
;;;
;;;Permission is hereby  granted, free of charge,  to any person obtaining  a copy of
;;;this software  and associated documentation  files (the ``Software''), to  deal in
;;;the Software without restriction, including  without limitation the rights to use,
;;;copy, modify,  merge, publish, distribute,  sublicense, and/or sell copies  of the
;;;Software,  and to  permit persons  to whom  the Software  is furnished  to do  so,
;;;subject to the following conditions:
;;;
;;;The above  copyright notice and  this permission notice  shall be included  in all
;;;copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED ``AS  IS'', WITHOUT  WARRANTY OF  ANY KIND,  EXPRESS OR
;;;IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES  OF MERCHANTABILITY, FITNESS
;;;FOR A  PARTICULAR PURPOSE AND NONINFRINGEMENT.   IN NO EVENT SHALL  THE AUTHORS OR
;;;COPYRIGHT HOLDERS BE LIABLE FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;AN ACTION OF  CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF  OR IN CONNECTION
;;;WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;


#!r6rs
(import (vicare)
  (vicare checks)
  (srfi :113)
  (srfi :114))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: SRFI 113, sets and bags\n")


;;;; helpers

(define (big x)
  (> x 5))

(define current-test-comparator
  (make-parameter equal?))

(define-syntax test
  (syntax-rules ()
    ((_ ?expected ?form)
     (check ?form (=> (current-test-comparator)) ?expected))
    ((_ ?dummy ?expected ?form)
     (check ?form (=> (current-test-comparator)) ?expected))
    ))

(define-syntax test-assert
  (syntax-rules ()
    ((_ ?form)
     (check-for-true ?form))
    ))

(define-syntax test-error
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (E ((error? E)
		    #t)
		   (else E))
	   ?form)
       => #t))
    ))

(define-syntax test-procedure-argument-violation
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (E ((procedure-argument-violation? E)
		    #t)
		   (else E))
	   ?form)
       => #t))
    ))


(parametrise ((check-test-name	'sets/simple))
  (define nums (set number-comparator))
  ;; nums is now {}
  (define syms (set eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (set-copy nums))
  ;; nums2 is now {}
  (define syms2 (set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (set eq-comparator))
  (define total 0)

  ;; esyms is now {}
  (test-assert (set-empty? esyms))
  (test-assert (set? nums))
  (test-assert (set? syms))
  (test-assert (set? nums2))
  (test-assert (set? syms2))
  (test-assert (not (set? 'a)))
  (set-adjoin! nums 2)
  (set-adjoin! nums 3)
  (set-adjoin! nums 4)
  (set-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (set-size (set-adjoin nums 5)))
  (test 3 (set-size nums))
  (test 3 (set-size (set-delete syms 'd)))
  (test 2 (set-size (set-delete-all syms '(c d))))
  (test 4 (set-size syms))
  (set-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (set-size (set-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 0 (set-size nums2))
  (test 4 (set-size syms2))
  (set-delete! nums 2)
  ;; nums is now {3, 4}
  (test 2 (set-size nums))
  (set-delete! nums 1)
  (test 2 (set-size nums))
  (set! nums2 (set-map number-comparator (lambda (x) (* 10 x)) nums))
  ;; nums2 is now {30, 40}
  (test-assert (set-contains? nums2 30))
  (test-assert (not (set-contains? nums2 3)))
  (set-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (set-fold + 3 nums))
  (set! nums (set eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
   (set=? nums (set-unfold
		eqv-comparator
		(lambda (i) (= i 0))
		(lambda (i) (* i 10))
		(lambda (i) (- i 1))
		5)))
  (test '(a) (set->list (set eq-comparator 'a)))
  (set! syms2 (list->set eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (set-size syms2))
  (test-assert (set-contains? syms2 'e))
  (test-assert (set-contains? syms2 'f))
  (list->set! syms2 '(a b))
  (test 4 (set-size syms2))
  #f)


(parametrise ((check-test-name	'sets/search))
  (define yam (set char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (set char-comparator #\y #\a #\m #\!))
  (define bam (set char-comparator #\b #\a #\m))
  (define ym (set char-comparator #\y #\m))
  (define-values (set1 obj1)
    (set-search! (set-copy yam) #\! failure/insert error))
  (define-values (set2 obj2)
    (set-search! (set-copy yam) #\! failure/ignore error))
  (define-values (set3 obj3)
    (set-search! (set-copy yam) #\y error success/update))
  (define-values (set4 obj4)
    (set-search! (set-copy yam) #\a error success/remove))

  (test-assert (set=? yam! set1))
  (test 1 obj1)
  (test-assert (set=? yam set2))
  (test 2 obj2)
  (test-assert (set=? bam set3))
  (test 3 obj3)
  (test-assert (set=? ym set4))
  (test 4 obj4)
  #f)


(parametrise ((check-test-name	'sets/subsets))
  (define set2 (set number-comparator 1 2))
  (define other-set2 (set number-comparator 1 2))
  (define set3 (set number-comparator 1 2 3))
  (define set4 (set number-comparator 1 2 3 4))
  (define setx (set number-comparator 10 20 30 40))
  (test-assert (set=? set2 other-set2))
  (test-assert (not (set=? set2 set3)))
  (test-assert (not (set=? set2 set3 other-set2)))
  (test-assert (set<? set2 set3 set4))
  (test-assert (not (set<? set2 other-set2)))
  (test-assert (set<=? set2 other-set2 set3))
  (test-assert (not (set<=? set2 set3 other-set2)))
  (test-assert (set>? set4 set3 set2))
  (test-assert (not (set>? set2 other-set2)))
  (test-assert (set>=? set3 other-set2 set2))
  (test-assert (not (set>=? other-set2 set3 set2)))
  #f)


(parametrise ((check-test-name	'sets/ops))
  ;; Potentially mutable
  (define abcd (set eq-comparator 'a 'b 'c 'd))
  (define efgh (set eq-comparator 'e 'f 'g 'h))
  (define abgh (set eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (set eq-comparator 'a 'b 'c 'd))
  (define other-efgh (set eq-comparator 'e 'f 'g 'h))
  (define other-abgh (set eq-comparator 'a 'b 'g 'h))
  (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (set eq-comparator))
  (define ab (set eq-comparator 'a 'b))
  (define cd (set eq-comparator 'c 'd))
  (define ef (set eq-comparator 'e 'f))
  (define gh (set eq-comparator 'g 'h))
  (define cdgh (set eq-comparator 'c 'd 'g 'h))
  (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (set-disjoint? abcd efgh))
  (test-assert (not (set-disjoint? abcd ab)))
  (parameterize ((current-test-comparator set=?))
    (test all (set-union abcd efgh))
    (test abcdgh (set-union abcd abgh))
    (test abefgh (set-union efgh abgh))
    (let ((efgh2 (set-copy efgh)))
      (set-union! efgh2 abgh)
      (test abefgh efgh2)
      (test none (set-intersection abcd efgh))
      (let ((abcd2 (set-copy abcd)))
	(set-intersection! abcd2 efgh)
	(test none abcd2)
	(test ab (set-intersection abcd abgh))
	(test ab (set-intersection abgh abcd))
	(test cd (set-difference abcd ab))
	(test abcd (set-difference abcd gh))
	(test none (set-difference abcd abcd))
	(let ((abcd3 (set-copy abcd)))
	  (set-difference! abcd3 abcd)
	  (test none abcd3)
	  (test cdgh (set-xor abcd abgh))
	  (test all (set-xor abcd efgh))
	  (test none (set-xor abcd other-abcd))
	  (let ((abcd4 (set-copy abcd)))
	    ;; don't test xor! effect
	    (test none (set-xor! abcd4 other-abcd))
	    (test "abcd smashed?" other-abcd abcd)
	    (test "efgh smashed?" other-efgh efgh)
	    (test "abgh smashed?" other-abgh abgh)
	    )))))
  #f)


(parametrise ((check-test-name	'sets/mismatch))
  (define nums (set number-comparator 1 2 3))
  (define syms (set eq-comparator 'a 'b 'c))
  (test-procedure-argument-violation (set=? nums syms))
  (test-procedure-argument-violation (set<? nums syms))
  (test-procedure-argument-violation (set<=? nums syms))
  (test-procedure-argument-violation (set>? nums syms))
  (test-procedure-argument-violation (set>=? nums syms))
  (test-procedure-argument-violation (set-union nums syms))
  (test-procedure-argument-violation (set-intersection nums syms))
  (test-procedure-argument-violation (set-difference nums syms))
  (test-procedure-argument-violation (set-xor nums syms))
  (test-procedure-argument-violation (set-union! nums syms))
  (test-procedure-argument-violation (set-intersection! nums syms))
  (test-procedure-argument-violation (set-difference! nums syms))
  (test-procedure-argument-violation (set-xor! nums syms))
  #f)


(parametrise ((check-test-name	'sets/whole))
  (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (set-copy whole))
  (define whole3 (set-copy whole))
  (define whole4 (set-copy whole))
  (define bottom (set eqv-comparator 1 2 3 4 5))
  (define top (set eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (set-partition big whole))
  (set-partition! big whole4)
  (parameterize ((current-test-comparator set=?))
    (test top (set-filter big whole))
    (test bottom (set-remove big whole))
    (set-filter! big whole2)
    (test-assert (not (set-contains? whole2 1)))
    (set-remove! big whole3)
    (test-assert (not (set-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (set-count big whole))
  (let ((hetero (set eqv-comparator 1 2 'a 3 4))
	(homo  (set eqv-comparator 1 2 3 4 5)))
    (test 'a (set-find symbol? hetero (lambda () (error #f "wrong"))))
    (test-error  (set-find symbol? homo (lambda () (error #f "wrong"))))
    (test-assert (set-any? symbol? hetero))
    (test-assert (set-any? number? hetero))
    (test-assert (not (set-every? symbol? hetero)))
    (test-assert (not (set-every? number? hetero)))
    (test-assert (not (set-any? symbol? homo)))
    (test-assert (set-every? number? homo)))
  #f)


(parametrise ((check-test-name	'sets/lowlevel))
  (define bucket (set string-ci-comparator "abc" "def"))
  (test string-ci-comparator (set-element-comparator bucket))
  (test-assert (set-contains? bucket "abc"))
  (test-assert (set-contains? bucket "ABC"))
  (test "def" (set-member bucket "DEF" "fqz"))
  (test "fqz" (set-member bucket "lmn" "fqz"))
  (let ((nums (set number-comparator 1 2 3)))
    ;; nums is now {1, 2, 3}
    (let ((nums2 (set-replace nums 2.0)))
      ;; nums2 is now {1, 2.0, 3}
      (test-assert (set-any? inexact? nums2))
      (set-replace! nums 2.0)
      ;; nums is now {1, 2.0, 3}
      (test-assert (set-any? inexact? nums))
      (let ((sos (set set-comparator
		      (set eqv-comparator 1 2)
		      (set eqv-comparator 1 2))))
	(test 1 (set-size sos)))))
  #f)


(parametrise ((check-test-name	'bags/simple))
  (define nums (bag number-comparator))
  ;; nums is now {}
  (define syms (bag eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (bag-copy nums))
  ;; nums2 is now {}
  (define syms2 (bag-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (bag eq-comparator))
  (define total 0)
  ;; esyms is now {}
  (test-assert (bag-empty? esyms))
  (test-assert (bag? nums))
  (test-assert (bag? syms))
  (test-assert (bag? nums2))
  (test-assert (bag? syms2))
  (test-assert (not (bag? 'a)))
  (bag-adjoin! nums 2)
  (bag-adjoin! nums 3)
  (bag-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (bag-size (bag-adjoin nums 5)))
  (test 3 (bag-size nums))
  (test 3 (bag-size (bag-delete syms 'd)))
  (test 2 (bag-size (bag-delete-all syms '(c d))))
  (test 4 (bag-size syms))
  (bag-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (bag-size (bag-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 3 (bag-size nums))
  (bag-delete! nums 1)
  (test 3 (bag-size nums))
  (set! nums2 (bag-map number-comparator (lambda (x) (* 10 x)) nums))
  ;; nums2 is now {20, 30, 40}
  (test-assert (bag-contains? nums2 30))
  (test-assert (not (bag-contains? nums2 3)))
  (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 90 total)
  (test 12 (bag-fold + 3 nums))
  (set! nums (bag eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
   (bag=? nums (bag-unfold
		eqv-comparator
		(lambda (i) (= i 0))
		(lambda (i) (* i 10))
		(lambda (i) (- i 1))
		5)))
  (test '(a) (bag->list (bag eq-comparator 'a)))
  (set! syms2 (list->bag eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (bag-size syms2))
  (test-assert (bag-contains? syms2 'e))
  (test-assert (bag-contains? syms2 'f))
  (list->bag! syms2 '(e f))
  ;; syms2 is now {e, e, f, f}
  (test 4 (bag-size syms2))
  #f)


(parametrise ((check-test-name	'bags/search))
  (define yam (bag char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (bag char-comparator #\y #\a #\m #\!))
  (define bam (bag char-comparator #\b #\a #\m))
  (define ym (bag char-comparator #\y #\m))
  (define-values (bag1 obj1)
    (bag-search! (bag-copy yam) #\! failure/insert error))
  (test-assert (bag=? yam! bag1))
  (test 1 obj1)
  (let-values (((bag2 obj2) (bag-search! (bag-copy yam) #\! failure/ignore error)))
    (test-assert (bag=? yam bag2))
    (test 2 obj2)
    (let-values (((bag3 obj3) (bag-search! (bag-copy yam) #\y error success/update)))
      (test-assert (bag=? bam bag3))
      (test 3 obj3)
      (let-values (((bag4 obj4) (bag-search! (bag-copy yam) #\a error success/remove)))
	(test-assert (bag=? ym bag4))
	(test 4 obj4))))
  #f)


(parametrise ((check-test-name	'bags/elemcount))
  (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))
  (test 5 (bag-element-count mybag 1))
  (test 0 (bag-element-count mybag 3))
  #f)


(parametrise ((check-test-name	'bags/subbags))
  (define bag2 (bag number-comparator 1 2))
  (define other-bag2 (bag number-comparator 1 2))
  (define bag3 (bag number-comparator 1 2 3))
  (define bag4 (bag number-comparator 1 2 3 4))
  (define bagx (bag number-comparator 10 20 30 40))
  (test-assert (bag=? bag2 other-bag2))
  (test-assert (not (bag=? bag2 bag3)))
  (test-assert (not (bag=? bag2 bag3 other-bag2)))
  (test-assert (bag<? bag2 bag3 bag4))
  (test-assert (not (bag<? bag2 other-bag2)))
  (test-assert (bag<=? bag2 other-bag2 bag3))
  (test-assert (not (bag<=? bag2 bag3 other-bag2)))
  (test-assert (bag>? bag4 bag3 bag2))
  (test-assert (not (bag>? bag2 other-bag2)))
  (test-assert (bag>=? bag3 other-bag2 bag2))
  (test-assert (not (bag>=? other-bag2 bag3 bag2)))
  #f)


(parametrise ((check-test-name	'bags/multi))
  (define one (bag eqv-comparator 10))
  (define two (bag eqv-comparator 10 10))
  (test-assert (not (bag=? one two)))
  (test-assert (bag<? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag<=? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag=? two two))
  (test-assert (not (bag<? two two)))
  (test-assert (not (bag>? two two)))
  (test-assert (bag<=? two two))
  (test-assert (bag>=? two two))
  (test '((10 . 2))
	(let ((result '()))
	  (bag-for-each-unique
	   (lambda (x y) (set! result (cons (cons x y) result)))
	   two)
	  result))
  (test 25 (bag-fold + 5 two))
  (test 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two))
  #f)


(parametrise ((check-test-name	'bags/ops))
  ;; Potentially mutable
  (define abcd (bag eq-comparator 'a 'b 'c 'd))
  (define efgh (bag eq-comparator 'e 'f 'g 'h))
  (define abgh (bag eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (bag eq-comparator 'a 'b 'c 'd))
  (define other-efgh (bag eq-comparator 'e 'f 'g 'h))
  (define other-abgh (bag eq-comparator 'a 'b 'g 'h))
  (define all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (bag eq-comparator))
  (define ab (bag eq-comparator 'a 'b))
  (define cd (bag eq-comparator 'c 'd))
  (define ef (bag eq-comparator 'e 'f))
  (define gh (bag eq-comparator 'g 'h))
  (define cdgh (bag eq-comparator 'c 'd 'g 'h))
  (define abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (bag-disjoint? abcd efgh))
  (test-assert (not (bag-disjoint? abcd ab)))
  (parameterize ((current-test-comparator bag=?))
    (test all (bag-union abcd efgh))
    (test abcdgh (bag-union abcd abgh))
    (test abefgh (bag-union efgh abgh))
    (let ((efgh2 (bag-copy efgh)))
      (bag-union! efgh2 abgh)
      (test abefgh efgh2)
      (test none (bag-intersection abcd efgh))
      (let ((abcd2 (bag-copy abcd)))
	(bag-intersection! abcd2 efgh)
	(test none abcd2)
	(test ab (bag-intersection abcd abgh))
	(test ab (bag-intersection abgh abcd))
	(test cd (bag-difference abcd ab))
	(test abcd (bag-difference abcd gh))
	(test none (bag-difference abcd abcd))
	(let ((abcd3 (bag-copy abcd)))
	  (bag-difference! abcd3 abcd)
	  (test none abcd3)
	  (test cdgh (bag-xor abcd abgh))
	  (test all (bag-xor abcd efgh))
	  (test none (bag-xor abcd other-abcd))
	  (let ((abcd4 (bag-copy abcd)))
	    (test none (bag-xor! abcd4 other-abcd))
	    (let ((abab (bag eq-comparator 'a 'b 'a 'b))
		  (ab2 (bag-copy ab)))
	      (test abab (bag-sum! ab2 ab))
	      (test abab ab2)
	      (test abab (bag-product 2 ab))
	      (let ((ab3 (bag-copy ab)))
		(bag-product! 2 ab3)
		(test abab ab3)
		(test "abcd smashed?" other-abcd abcd)
		(test "abcd smashed?" other-abcd abcd)
		(test "efgh smashed?" other-efgh efgh)
		(test "abgh smashed?" other-abgh abgh)
		)))))))
  #f)


(parametrise ((check-test-name	'bags/mismatch))
  (define nums (bag number-comparator 1 2 3))
  (define syms (bag eq-comparator 'a 'b 'c))
  (test-procedure-argument-violation (bag=? nums syms))
  (test-procedure-argument-violation (bag<? nums syms))
  (test-procedure-argument-violation (bag<=? nums syms))
  (test-procedure-argument-violation (bag>? nums syms))
  (test-procedure-argument-violation (bag>=? nums syms))
  (test-procedure-argument-violation (bag-union nums syms))
  (test-procedure-argument-violation (bag-intersection nums syms))
  (test-procedure-argument-violation (bag-difference nums syms))
  (test-procedure-argument-violation (bag-xor nums syms))
  (test-procedure-argument-violation (bag-union! nums syms))
  (test-procedure-argument-violation (bag-intersection! nums syms))
  (test-procedure-argument-violation (bag-difference! nums syms))
  #f)


(parametrise ((check-test-name	'bags/whole))
  (define whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (bag-copy whole))
  (define whole3 (bag-copy whole))
  (define whole4 (bag-copy whole))
  (define bottom (bag eqv-comparator 1 2 3 4 5))
  (define top (bag eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (bag-partition big whole))
  (bag-partition! big whole4)
  (parameterize ((current-test-comparator bag=?))
    (test top (bag-filter big whole))
    (test bottom (bag-remove big whole))
    (bag-filter! big whole2)
    (test-assert (not (bag-contains? whole2 1)))
    (bag-remove! big whole3)
    (test-assert (not (bag-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (bag-count big whole))
  (let ((hetero (bag eqv-comparator 1 2 'a 3 4))
	(homo   (bag eqv-comparator 1 2 3 4 5)))
    (test 'a (bag-find symbol? hetero (lambda () (error #f "wrong"))))
    (test-error  (bag-find symbol? homo (lambda () (error #f "wrong"))))
    (test-assert (bag-any? symbol? hetero))
    (test-assert (bag-any? number? hetero))
    (test-assert (not (bag-every? symbol? hetero)))
    (test-assert (not (bag-every? number? hetero)))
    (test-assert (not (bag-any? symbol? homo)))
    (test-assert (bag-every? number? homo)))
  #f)


(parametrise ((check-test-name	'bags/lowlevel))
  (define bucket (bag string-ci-comparator "abc" "def"))
  (test string-ci-comparator (bag-element-comparator bucket))
  (test-assert (bag-contains? bucket "abc"))
  (test-assert (bag-contains? bucket "ABC"))
  (test "def" (bag-member bucket "DEF" "fqz"))
  (test "fqz" (bag-member bucket "lmn" "fqz"))
  (let ((nums (bag number-comparator 1 2 3)))
    ;; nums is now {1, 2, 3}
    (let ((nums2 (bag-replace nums 2.0)))
      ;; nums2 is now {1, 2.0, 3}
      (test-assert (bag-any? inexact? nums2))
      (bag-replace! nums 2.0)
      ;; nums is now {1, 2.0, 3}
      (test-assert (bag-any? inexact? nums))
      (let ((bob (bag bag-comparator
		      (bag eqv-comparator 1 2)
		      (bag eqv-comparator 1 2))))
	(test 2 (bag-size bob)))))
  #f)


(parametrise ((check-test-name	'bags/semantics))
  (define mybag (bag number-comparator 1 2))
  ;; mybag is {1, 2}
  (test 2 (bag-size mybag))
  (bag-adjoin! mybag 1)
  ;; mybag is {1, 1, 2}
  (test 3 (bag-size mybag))
  (test 2 (bag-unique-size mybag))
  (bag-delete! mybag 2)
  ;; mybag is {1, 1}
  (bag-delete! mybag 2)
  (test 2 (bag-size mybag))
  (bag-increment! mybag 1 3)
  ;; mybag is {1, 1, 1, 1, 1}
  (test 5 (bag-size mybag))
  (test-assert (bag-decrement! mybag 1 2))
  ;; mybag is {1, 1, 1}
  (test 3 (bag-size mybag))
  (bag-decrement! mybag 1 5)
  ;; mybag is {}
  (test 0 (bag-size mybag))
  #f)


(parametrise ((check-test-name	'bags/convert))
  (define multi (bag eqv-comparator 1 2 2 3 3 3))
  (define single (bag eqv-comparator 1 2 3))
  (define singleset (set eqv-comparator 1 2 3))
  (define minibag (bag eqv-comparator 'a 'a))
  (define alist '((a . 2)))
  (test alist (bag->alist minibag))
  (test-assert (bag=? minibag (alist->bag eqv-comparator alist)))
  (test-assert (set=? singleset (bag->set single)))
  (test-assert (set=? singleset (bag->set multi)))
  (test-assert (bag=? single (set->bag singleset)))
  (test-assert (not (bag=? multi (set->bag singleset))))
  (set->bag! minibag singleset)
  ;; minibag is now {a, a, a, a, 1, 2, 3}
  (test-assert (bag-contains? minibag 1))
  #f)


(parametrise ((check-test-name	'bags/sumprod))
  (define abb (bag eq-comparator 'a 'b 'b))
  (define aab (bag eq-comparator 'a 'a 'b))
  (define total (bag-sum abb aab))
  (test 3 (bag-count (lambda (x) (eqv? x 'a)) total))
  (test 3 (bag-count (lambda (x) (eqv? x 'b)) total))
  (test 12 (bag-size (bag-product 2 total)))
  (let ((bag1 (bag eqv-comparator 1)))
    (bag-sum! bag1 bag1)
    (test 2 (bag-size bag1))
    (bag-product! 2 bag1)
    (test 4 (bag-size bag1)))
  #f)


(parametrise ((check-test-name	'comparators))
  (define a (set number-comparator 1 2 3))
  (define b (set number-comparator 1 2 4))
  (define aa (bag number-comparator 1 2 3))
  (define bb (bag number-comparator 1 2 4))
  (test-assert (not (=? set-comparator a b)))
  (test-assert (=? set-comparator a (set-copy a)))
  (test-error (<? set-comparator a b))
  (test-assert (not (=? bag-comparator aa bb)))
  (test-assert (=? bag-comparator aa (bag-copy aa)))
  (test-error (<? bag-comparator aa bb))
  (test-assert (not (=? default-comparator a aa)))
  #f)


(parametrise ((check-test-name	'constructors))

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(values (set-contains? S 1)
		(set-contains? S 2)
		(set-contains? S 3)
		(set-size S)))
    => #t #t #t 3)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3 2))
	(values (bag-contains? B 1)
		(bag-contains? B 2)
		(bag-contains? B 3)
		(bag-size B)))
    => #t #t #t 4)

;;; --------------------------------------------------------------------

  (check
      (internal-body
	(define (stop? seed)
	  (fx>? seed 3))

	(define (mapper seed)
	  (number->string seed))

	(define (successor seed)
	  (fxadd1 seed))

	(define S
	  (set-unfold string-comparator stop? mapper successor 1))

	(values (set-contains? S "1")
		(set-contains? S "2")
		(set-contains? S "3")
		(set-size S)))
    => #t #t #t 3)

  (check
      (internal-body
	(define (stop? seed)
	  (fx>? seed 3))

	(define (mapper seed)
	  (number->string seed))

	(define (successor seed)
	  (fxadd1 seed))

	(define B
	  (bag-unfold string-comparator stop? mapper successor 1))

	(values (bag-contains? B "1")
		(bag-contains? B "2")
		(bag-contains? B "3")
		(bag-size B)))
    => #t #t #t 3)

  #t)


(parametrise ((check-test-name	'predicates))

  (check-for-true  (set? (set fixnum-comparator 1 2 3)))
  (check-for-false (set? (bag fixnum-comparator 1 2 3)))
  (check-for-false (set? "ciao"))

  (check-for-true  (bag? (bag fixnum-comparator 1 2 3)))
  (check-for-false (bag? (set fixnum-comparator 1 2 3)))
  (check-for-false (bag? "ciao"))

;;; --------------------------------------------------------------------

  (check-for-true  (set-contains? (set fixnum-comparator 1) 1))
  (check-for-false (set-contains? (set fixnum-comparator 1) 2))
  (check-for-false (set-contains? (set fixnum-comparator) 1))

  (check-for-true  (bag-contains? (bag fixnum-comparator 1) 1))
  (check-for-false (bag-contains? (bag fixnum-comparator 1) 2))
  (check-for-false (bag-contains? (bag fixnum-comparator) 2))

;;; --------------------------------------------------------------------

  (check-for-false (set-empty? (set fixnum-comparator 1)))
  (check-for-true  (set-empty? (set fixnum-comparator)))

  (check-for-false (bag-empty? (bag fixnum-comparator 1)))
  (check-for-true  (bag-empty? (bag fixnum-comparator)))

;;; --------------------------------------------------------------------

  (check-for-true  (set-disjoint? (set fixnum-comparator)
				  (set fixnum-comparator)))

  (check-for-true  (set-disjoint? (set fixnum-comparator 1 2 3)
				  (set fixnum-comparator 4 5 6)))

  (check-for-false (set-disjoint? (set fixnum-comparator 1 2 3 4)
				  (set fixnum-comparator 5 6 3 7)))

  (check-for-true  (bag-disjoint? (bag fixnum-comparator)
				  (bag fixnum-comparator)))

  (check-for-true  (bag-disjoint? (bag fixnum-comparator 1 2 3)
				  (bag fixnum-comparator 4 5 6)))

  (check-for-false (bag-disjoint? (bag fixnum-comparator 1 2 3 4)
				  (bag fixnum-comparator 5 6 3 7)))

  #t)


(parametrise ((check-test-name	'predicates))

  (check-for-true
   (let ((str1 (string #\a #\b #\c))
	 (str2 (string #\a #\b #\c)))
     (let ((S (set string-comparator str1)))
       (eq? str1 (set-member S str2 #f)))))

  (check-for-true
   (let ((str1 (string #\a #\b #\c))
	 (str2 (string #\a #\b #\c)))
     (let ((B (bag string-comparator str1)))
       (eq? str1 (bag-member B str2 #f)))))

;;; --------------------------------------------------------------------

  (check-for-true (comparator? (set-element-comparator (set fixnum-comparator))))
  (check-for-true (comparator? (bag-element-comparator (bag fixnum-comparator))))

  #t)


(parametrise ((check-test-name	'update))

;;; set-adjoin

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(define S^
	  (set-adjoin S 4))
	(list-sort fx<? (set->list S^)))
    => '(1 2 3 4))

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(define S^
	  (set-adjoin S 2))
	(list-sort fx<? (set->list S^)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; bag-adjoin

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3))
	(define B^
	  (bag-adjoin B 4))
	(list-sort fx<? (bag->list B^)))
    => '(1 2 3 4))

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3))
	(define B^
	  (bag-adjoin B 2))
	(list-sort fx<? (bag->list B^)))
    => '(1 2 2 3))

;;; --------------------------------------------------------------------
;;; set-adjoin!

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(set-adjoin! S 4)
	(list-sort fx<? (set->list S)))
    => '(1 2 3 4))

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(set-adjoin! S 2)
	(list-sort fx<? (set->list S)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; bag-adjoin!

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3))
	(bag-adjoin! B 4)
	(list-sort fx<? (bag->list B)))
    => '(1 2 3 4))

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3))
	(bag-adjoin! B 2)
	(list-sort fx<? (bag->list B)))
    => '(1 2 2 3))

;;; --------------------------------------------------------------------
;;; set-replace

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-replace S 1))
	(eq? S S^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-replace S str2))
	(values (set-any? (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-any? (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => #f #t)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-replace S num2))
	(set-any? (lambda (elm)
		    (= elm num1))
		  S^))
    => #t)

;;; --------------------------------------------------------------------
;;; bag-replace

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-replace B 1))
	(eq? B B^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-replace B str2))
	(values (bag-any? (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-any? (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => #f #t)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-replace B num2))
	(bag-find (lambda (elm)
		    (= elm num1))
		  B^
		  void))
    => 2.0)

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-replace B 2.0))
	(list-sort < (bag->list B^)))
    => '(1 2.0 2.0 3))

;;; --------------------------------------------------------------------
;;; set-replace!

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-replace! S 1))
	(eq? S S^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-replace! S str2))
	(values (set-find (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-find (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => (void) "a")

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-replace! S num2))
	(set-find (lambda (elm)
		    (= elm num1))
		  S^))
    => 2.0)

;;; --------------------------------------------------------------------
;;; bag-replace!

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-replace! B 1))
	(eq? B B^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-replace! B str2))
	(values (bag-find (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-find (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => (void) "a")

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-replace! B num2))
	(bag-find (lambda (elm)
		    (= elm num1))
		  B^))
    => 2.0)

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-replace! B 2.0))
	(list-sort < (bag->list B^)))
    => '(1 2.0 2.0 3))

;;; --------------------------------------------------------------------
;;; set-delete

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-delete S 1))
	(eq? S S^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-delete S str2))
	(values (set-any? (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-any? (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-delete S num2))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(set-any? (lambda (elm)
		    (= elm num1))
		  S^))
    => #f)

;;; --------------------------------------------------------------------
;;; bag-delete

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-delete B 1))
	(eq? B B^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-delete B str2))
	(values (bag-any? (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-any? (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-delete B num2))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(bag-any? (lambda (elm)
		    (= elm num1))
		  B^))
    => #f)

  (check
      (internal-body
	(define B  (bag fixnum-comparator 1 2 2 3))
	(define B^ (bag-delete B 2))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-delete B 2.0))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; set-delete!

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-delete! S 1))
	(eq? S S^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-delete! S str2))
	(values (set-any? (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-any? (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-delete! S num2))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(set-any? (lambda (elm)
		    (= elm num1))
		  S^))
    => #f)

;;; --------------------------------------------------------------------
;;; bag-delete!

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-delete! B 1))
	(eq? B B^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-delete! B str2))
	(values (bag-any? (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-any? (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-delete! B num2))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(bag-any? (lambda (elm)
		    (= elm num1))
		  B^))
    => #f)

  (check
      (internal-body
	(define B  (bag fixnum-comparator 1 2 2 3))
	(define B^ (bag-delete! B 2))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-delete! B 2.0))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; set-delete-all

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-delete-all S '(1)))
	(eq? S S^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-delete-all S (list str2)))
	(values (set-any? (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-any? (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-delete-all S (list num2)))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(set-any? (lambda (elm)
		    (= elm num1))
		  S^))
    => #f)

;;; --------------------------------------------------------------------
;;; bag-delete-all

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-delete-all B '(1)))
	(eq? B B^))
    => #f)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-delete-all B (list str2)))
	(values (bag-any? (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-any? (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-delete-all B (list num2)))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(bag-any? (lambda (elm)
		    (= elm num1))
		  B^))
    => #f)

  (check
      (internal-body
	(define B  (bag fixnum-comparator 1 2 2 3))
	(define B^ (bag-delete-all B '(2)))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-delete-all B '(2.0)))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; set-delete-all!

  (check
      (internal-body
	(define S  (set fixnum-comparator))
	(define S^ (set-delete-all! S '(1)))
	(eq? S S^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define S    (set string-comparator str1))
	(define S^   (set-delete-all! S (list str2)))
	(values (set-any? (lambda (elm)
			    (eq? elm str1))
			  S^)
		(set-any? (lambda (elm)
			    (eq? elm str2))
			  S^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define S    (set real-comparator num1))
	(define S^   (set-delete-all! S (list num2)))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(set-any? (lambda (elm)
		    (= elm num1))
		  S^))
    => #f)

;;; --------------------------------------------------------------------
;;; bag-delete-all!

  (check
      (internal-body
	(define B  (bag fixnum-comparator))
	(define B^ (bag-delete-all! B '(1)))
	(eq? B B^))
    => #t)

  (check
      (internal-body
	(define str1 (string #\a))
	(define str2 (string #\a))
	(define B    (bag string-comparator str1))
	(define B^   (bag-delete-all! B (list str2)))
	(values (bag-any? (lambda (elm)
			    (eq? elm str1))
			  B^)
		(bag-any? (lambda (elm)
			    (eq? elm str2))
			  B^)))
    => #f #f)

  (check
      (internal-body
	(define num1 2)
	(define num2 2.0)
	(define B    (bag real-comparator num1))
	(define B^   (bag-delete-all! B (list num2)))
	(assert (fxzero? (comparator-compare real-comparator num1 num2)))
	(bag-any? (lambda (elm)
		    (= elm num1))
		  B^))
    => #f)

  (check
      (internal-body
	(define B  (bag fixnum-comparator 1 2 2 3))
	(define B^ (bag-delete-all! B (list 2)))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

  (check
      (internal-body
	(define B  (bag real-comparator 1 2 2 3))
	(define B^ (bag-delete-all! B (list 2.0)))
	(list-sort < (bag->list B^)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; set-search

  (check	;success and update
      (internal-body
	(define S  (set real-comparator 1 2 3))
	(receive (S^ obj)
	    (set-search! S 2.0
			 ;;failure proc
			 (lambda (insert ignore)
			   (error #f "wrong"))
			 ;;success proc
			 (lambda (true-element update remove)
			   (update 9 'flag)))
	  (values (list-sort < (set->list S^))
		  obj)))
    => '(1 3 9) 'flag)

  (check	;success and remove
      (internal-body
	(define S  (set real-comparator 1 2 3))
	(receive (S^ obj)
	    (set-search! S 2.0
			 ;;failure proc
			 (lambda (insert ignore)
			   (error #f "wrong"))
			 ;;success proc
			 (lambda (true-element update remove)
			   (remove 'flag)))
	  (values (list-sort < (set->list S^))
		  obj)))
    => '(1 3) 'flag)

  (check	;failure and ignore
      (internal-body
	(define S  (set real-comparator 1 2 3))
	(receive (S^ obj)
	    (set-search! S 99
			 ;;failure proc
			 (lambda (insert ignore)
			   (ignore 'flag))
			 ;;success proc
			 (lambda (true-element update remove)
			   (error #f "wrong")))
	  (values (list-sort < (set->list S^))
		  obj)))
    => '(1 2 3) 'flag)

  (check	;failure and insert
      (internal-body
	(define S  (set real-comparator 1 2 3))
	(receive (S^ obj)
	    (set-search! S 99
			 ;;failure proc
			 (lambda (insert ignore)
			   (insert 'flag))
			 ;;success proc
			 (lambda (true-element update remove)
			   (error #f "wrong")))
	  (values (list-sort < (set->list S^))
		  obj)))
    => '(1 2 3 99) 'flag)

;;; --------------------------------------------------------------------
;;; bag-search

  (check	;success and update
      (internal-body
	(define B  (bag real-comparator 1 2 3))
	(receive (B^ obj)
	    (bag-search! B 2.0
			 ;;failure proc
			 (lambda (insert ignore)
			   (error #f "wrong"))
			 ;;success proc
			 (lambda (true-element update remove)
			   (update 9 'flag)))
	  (values (list-sort < (bag->list B^))
		  obj)))
    => '(1 3 9) 'flag)

  (check	;success and remove
      (internal-body
	(define B  (bag real-comparator 1 2 3))
	(receive (B^ obj)
	    (bag-search! B 2.0
			 ;;failure proc
			 (lambda (insert ignore)
			   (error #f "wrong"))
			 ;;success proc
			 (lambda (true-element update remove)
			   (remove 'flag)))
	  (values (list-sort < (bag->list B^))
		  obj)))
    => '(1 3) 'flag)

  (check	;failure and ignore
      (internal-body
	(define B  (bag real-comparator 1 2 3))
	(receive (B^ obj)
	    (bag-search! B 99
			 ;;failure proc
			 (lambda (insert ignore)
			   (ignore 'flag))
			 ;;success proc
			 (lambda (true-element update remove)
			   (error #f "wrong")))
	  (values (list-sort < (bag->list B^))
		  obj)))
    => '(1 2 3) 'flag)

  (check	;failure and insert
      (internal-body
	(define B  (bag real-comparator 1 2 3))
	(receive (B^ obj)
	    (bag-search! B 99
			 ;;failure proc
			 (lambda (insert ignore)
			   (insert 'flag))
			 ;;success proc
			 (lambda (true-element update remove)
			   (error #f "wrong")))
	  (values (list-sort < (bag->list B^))
		  obj)))
    => '(1 2 3 99) 'flag)

  #t)


(parametrise ((check-test-name	'whole))

;;; set-size

  (check
      (internal-body
	(define S
	  (set fixnum-comparator))
	(set-size S))
    => 0)

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 2 3))
	(set-size S))
    => 3)

;;; --------------------------------------------------------------------
;;; bag-size

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator))
	(bag-size B))
    => 0)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 3))
	(bag-size B))
    => 3)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 2 2 3))
	(bag-size B))
    => 4)

;;; --------------------------------------------------------------------
;;; set-find

  (check
      (internal-body
	(define S
	  (set fixnum-comparator))
	(set-find fxzero? S void))
    => (void))

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 0 3))
	(set-find fxzero? S void))
    => 0)

;;; --------------------------------------------------------------------
;;; bag-find

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator))
	(bag-find fxzero? B void))
    => (void))

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 0 3))
	(bag-find fxzero? B void))
    => 0)

  (check
      (internal-body
	(define B
	  (bag real-comparator 1 0 0.0 3))
	(bag-find zero? B void))
    => 0)

;;; --------------------------------------------------------------------
;;; set-count

  (check
      (internal-body
	(define S
	  (set fixnum-comparator))
	(set-count fxzero? S))
    => 0)

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 0 3))
	(set-count fxzero? S))
    => 1)

;;; --------------------------------------------------------------------
;;; bag-count

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator))
	(bag-count fxzero? B))
    => 0)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 0 3))
	(bag-count fxzero? B))
    => 1)

  (check
      (internal-body
	(define B
	  (bag real-comparator 1 0 0.0 3))
	(bag-count zero? B))
    => 2)

;;; --------------------------------------------------------------------
;;; set-any?

  (check
      (internal-body
	(define S
	  (set fixnum-comparator))
	(set-any? fxzero? S))
    => #f)

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 1 0 3))
	(set-any? fxzero? S))
    => #t)

;;; --------------------------------------------------------------------
;;; bag-any?

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator))
	(bag-any? fxzero? B))
    => #f)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 0 3))
	(bag-any? fxzero? B))
    => #t)

  (check
      (internal-body
	(define B
	  (bag real-comparator 1 0 0.0 3))
	(bag-any? zero? B))
    => #t)

;;; --------------------------------------------------------------------
;;; set-every?

  (check
      (internal-body
	(define S
	  (set fixnum-comparator))
	(set-every? fxzero? S))
    => #t)

  (check
      (internal-body
	(define S
	  (set fixnum-comparator 0 1 2))
	(set-every? fxnonnegative? S))
    => #t)

;;; --------------------------------------------------------------------
;;; bag-every?

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator))
	(bag-every? fxzero? B))
    => #t)

  (check
      (internal-body
	(define B
	  (bag fixnum-comparator 1 0 3))
	(bag-every? fxnonnegative? B))
    => #t)

  (check
      (internal-body
	(define B
	  (bag real-comparator 1 0 0.0 3))
	(bag-every? non-negative? B))
    => #t)

  #t)


(parametrise ((check-test-name	'folding))

;;; set-map

  (check
      (set-map string-ci-comparator
	       symbol->string
	       (set eq-comparator))
    (=> set=?)
    (set string-ci-comparator))

  (check
      (set-map string-ci-comparator
	       symbol->string
	       (set eq-comparator 'foo 'bar 'baz))
    (=> set=?)
    (set string-ci-comparator "foo" "bar" "baz"))

  (check
      (set-map integer-comparator
	       (lambda (x)
		 (quotient x 2))
	       (set integer-comparator 1 2 3 4 5))
    (=> set=?)
    (set integer-comparator 0 1 2))

;;; --------------------------------------------------------------------
;;; bag-map

  (check
      (bag-map string-ci-comparator
	       symbol->string
	       (bag eq-comparator))
    (=> bag=?)
    (bag string-ci-comparator))

  (check
      (bag-map string-ci-comparator
	       symbol->string
	       (bag eq-comparator 'foo 'bar 'baz))
    (=> bag=?)
    (bag string-ci-comparator "foo" "bar" "baz"))

  (check
      (list-sort < (bag->list
		    (bag-map integer-comparator
			     (lambda (x)
			       (quotient x 2))
			     (bag integer-comparator 1 2 3 4 5))))
    => '(0 1 1 2 2))

;;; --------------------------------------------------------------------
;;; set-for-each

  (check
      (list-sort < (receive-and-return (rv)
		       '()
		     (set-for-each (lambda (elm)
				     (set-cons! rv elm))
				   (set fixnum-comparator 1 2 3))))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; bag-for-each

  (check
      (list-sort < (receive-and-return (rv)
		       '()
		     (bag-for-each (lambda (elm)
				     (set-cons! rv elm))
				   (bag fixnum-comparator 1 2 2 3))))
    => '(1 2 2 3))

;;; --------------------------------------------------------------------
;;; set-fold

  (check
      (list-sort < (set-fold (lambda (elm knil)
			       (cons elm knil))
			     '()
			     (set fixnum-comparator 1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; bag-fold

  (check
      (list-sort < (bag-fold (lambda (elm knil)
			       (cons elm knil))
			     '()
			     (bag fixnum-comparator 1 2 2 3)))
    => '(1 2 2 3))

;;; --------------------------------------------------------------------
;;; set-filter

  (check
      (set->list
       (set-filter (lambda (elm)
		     (<= 2 elm))
		   (set fixnum-comparator 1 2 3))
       #t)
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-filter

  (check
      (bag->list
       (bag-filter (lambda (elm)
		     (<= 2 elm))
		   (bag fixnum-comparator 1 2 2 3))
       <)
    => '(2 2 3))

;;; --------------------------------------------------------------------
;;; set-filter!

  (check
      (set->list
       (set-filter! (lambda (elm)
		      (<= 2 elm))
		    (set fixnum-comparator 1 2 3))
       #t)
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-filter!

  (check
      (bag->list
       (bag-filter! (lambda (elm)
		      (<= 2 elm))
		    (bag fixnum-comparator 1 2 2 3))
       <)
    => '(2 2 3))

;;; --------------------------------------------------------------------
;;; set-remove

  (check
      (set->list
       (set-remove (lambda (elm)
		     (<= 2 elm))
		   (set fixnum-comparator 1 2 3))
       #t)
    => '(1))

;;; --------------------------------------------------------------------
;;; bag-remove

  (check
      (bag->list
       (bag-remove (lambda (elm)
		     (<= 2 elm))
		   (bag fixnum-comparator 1 2 2 3))
       #t)
    => '(1))

;;; --------------------------------------------------------------------
;;; set-remove!

  (check
      (set->list
       (set-remove! (lambda (elm)
		      (<= 2 elm))
		    (set fixnum-comparator 1 2 3))
       #t)
    => '(1))

;;; --------------------------------------------------------------------
;;; bag-remove!

  (check
      (bag->list
       (bag-remove! (lambda (elm)
		      (<= 2 elm))
		    (bag fixnum-comparator 1 2 2 3))
       #t)
    => '(1))

;;; --------------------------------------------------------------------
;;; set-partition

  (check
      (receive (in out)
	  (set-partition (lambda (elm)
			   (<= 2 elm))
			 (set fixnum-comparator 1 2 2 3))
	(values (set->list in #t)
		(set->list out #t)))
    => '(2 3) '(1))

;;; --------------------------------------------------------------------
;;; set-partition

  (check
      (receive (in out)
	  (bag-partition (lambda (elm)
			   (<= 2 elm))
			 (bag fixnum-comparator 1 2 2 3))
	(values (bag->list in #t)
		(bag->list out #t)))
    => '(2 2 3) '(1))

  #t)


(parametrise ((check-test-name	'copying))

;;; set-copy

  (check
      (let* ((S1 (set fixnum-comparator 1 2 3))
	     (S2 (set-copy S1)))
	(set=? S1 S2))
    => #t)

;;; --------------------------------------------------------------------
;;; bag-copy

  (check
      (let* ((B1 (bag fixnum-comparator 1 2 3))
	     (B2 (bag-copy B1)))
	(bag=? B1 B2))
    => #t)

;;; --------------------------------------------------------------------
;;; set->list

  (check
      (list-sort < (set->list (set fixnum-comparator 1 2 3)))
    => '(1 2 3))

  (check
      (set->list (set fixnum-comparator 1 2 3) #t)
    => '(1 2 3))

  (check
      (set->list (set fixnum-comparator 1 2 3) fx<?)
    => '(1 2 3))

  (check
      (set->list (set fixnum-comparator 1 2 3) fx>?)
    => '(3 2 1))

;;; --------------------------------------------------------------------
;;; bag->list

  (check
      (list-sort < (bag->list (bag fixnum-comparator 1 2 2 3)))
    => '(1 2 2 3))

  (check
      (bag->list (bag fixnum-comparator 1 2 2 3) #t)
    => '(1 2 2 3))

  (check
      (bag->list (bag fixnum-comparator 1 2 2 3) fx<?)
    => '(1 2 2 3))

  (check
      (bag->list (bag fixnum-comparator 1 2 2 3) fx>?)
    => '(3 2 2 1))

;;; --------------------------------------------------------------------
;;; list->set

  (check
      (set->list (list->set fixnum-comparator '(1 2 3)) #t)
    => '(1 2 3))

  (check
      (set->list (list->set fixnum-comparator '(1 2 2 3)) #t)
    => '(1 2 3))

;;; --------------------------------------------------------------------
;;; list->bag

  (check
      (bag->list (list->bag fixnum-comparator '(1 2 2 3)) #t)
    => '(1 2 2 3))

;;; --------------------------------------------------------------------
;;; list->set!

  (check
      (set->list (list->set! (set fixnum-comparator 1 2 3) '(4 5 6)) #t)
    => '(1 2 3 4 5 6))

  (check
      (set->list (list->set! (set fixnum-comparator 1 2 2 3) '(4 5 5 6)) #t)
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------
;;; list->bag!

  (check
      (bag->list (list->bag! (bag fixnum-comparator 1 2 3) '(4 5 6)) #t)
    => '(1 2 3 4 5 6))

  (check
      (bag->list (list->bag! (bag fixnum-comparator 1 2 2 3) '(4 5 5 6)) #t)
    => '(1 2 2 3 4 5 5 6))

  #t)


(parametrise ((check-test-name	'subsets))

  (define-constant S0 (set fixnum-comparator))
  (define-constant S1 (set fixnum-comparator 1))
  (define-constant S2 (set fixnum-comparator 1 2))
  (define-constant S3 (set fixnum-comparator 1 2 3))

  (define-constant B0 (bag fixnum-comparator))
  (define-constant B1 (bag fixnum-comparator 1))
  (define-constant B2 (bag fixnum-comparator 1 2))
  (define-constant B3 (bag fixnum-comparator 1 2 3))

;;; --------------------------------------------------------------------
;;; set=?

  (check-for-true (set=? S0))
  (check-for-true (set=? S0 S0))
  (check-for-true (set=? S0 S0 S0))
  (check-for-true (set=? S0 S0 S0 S0))

  (check-for-true (set=? S1))
  (check-for-true (set=? S1 S1))
  (check-for-true (set=? S1 S1 S1))
  (check-for-true (set=? S1 S1 S1 S1))

  (check-for-false (set=? S0 S1))
  (check-for-false (set=? S0 S1 S0))
  (check-for-false (set=? S0 S0 S1))
  (check-for-false (set=? S0 S0 S1 S0))
  (check-for-false (set=? S0 S0 S0 S1))

  (check-for-false (set=? S3 S1))
  (check-for-false (set=? S3 S1 S3))
  (check-for-false (set=? S3 S3 S1))
  (check-for-false (set=? S3 S3 S1 S3))
  (check-for-false (set=? S3 S3 S3 S1))

;;; --------------------------------------------------------------------
;;; set<?

  (check-for-true  (set<? S0))
  (check-for-false (set<? S0 S0))
  (check-for-false (set<? S0 S0 S0))
  (check-for-false (set<? S0 S0 S0 S0))

  (check-for-true  (set<? S1))
  (check-for-false (set<? S1 S1))
  (check-for-false (set<? S1 S1 S1))
  (check-for-false (set<? S1 S1 S1 S1))

  (check-for-true  (set<? S0 S1))
  (check-for-false (set<? S0 S1 S0))
  (check-for-false (set<? S0 S0 S1))
  (check-for-false (set<? S0 S0 S1 S0))
  (check-for-false (set<? S0 S0 S0 S1))

  (check-for-false (set<? S3 S1))
  (check-for-false (set<? S3 S1 S3))
  (check-for-false (set<? S3 S3 S1))
  (check-for-false (set<? S3 S3 S1 S3))
  (check-for-false (set<? S3 S3 S3 S1))

  (check-for-true  (set<? S1 S2))
  (check-for-false (set<? S2 S1))

  (check-for-true  (set<? S1 S2 S3))
  (check-for-false (set<? S1 S3 S2))
  (check-for-false (set<? S3 S1 S2))

  (check-for-true  (set<? S0 S1 S2 S3))
  (check-for-false (set<? S0 S1 S3 S2))
  (check-for-false (set<? S0 S3 S1 S2))
  (check-for-false (set<? S3 S0 S1 S2))

;;; --------------------------------------------------------------------
;;; set>?

  (check-for-true  (set>? S0))
  (check-for-false (set>? S0 S0))
  (check-for-false (set>? S0 S0 S0))
  (check-for-false (set>? S0 S0 S0 S0))

  (check-for-true  (set>? S1))
  (check-for-false (set>? S1 S1))
  (check-for-false (set>? S1 S1 S1))
  (check-for-false (set>? S1 S1 S1 S1))

  (check-for-false (set>? S0 S1))
  (check-for-false (set>? S0 S1 S0))
  (check-for-false (set>? S0 S0 S1))
  (check-for-false (set>? S0 S0 S1 S0))
  (check-for-false (set>? S0 S0 S0 S1))

  (check-for-true  (set>? S3 S1))
  (check-for-false (set>? S3 S1 S3))
  (check-for-false (set>? S3 S3 S1))
  (check-for-false (set>? S3 S3 S1 S3))
  (check-for-false (set>? S3 S3 S3 S1))

  (check-for-true  (set>? S3 S2))
  (check-for-false (set>? S2 S3))

  (check-for-true  (set>? S3 S2 S1))
  (check-for-false (set>? S3 S1 S2))
  (check-for-false (set>? S1 S3 S2))

  (check-for-true  (set>? S3 S2 S1 S0))
  (check-for-false (set>? S3 S2 S0 S1))
  (check-for-false (set>? S3 S0 S2 S1))
  (check-for-false (set>? S0 S3 S2 S1))

;;; --------------------------------------------------------------------
;;; set<=?

  (check-for-true (set<=? S0))
  (check-for-true (set<=? S0 S0))
  (check-for-true (set<=? S0 S0 S0))
  (check-for-true (set<=? S0 S0 S0 S0))

  (check-for-true (set<=? S1))
  (check-for-true (set<=? S1 S1))
  (check-for-true (set<=? S1 S1 S1))
  (check-for-true (set<=? S1 S1 S1 S1))

  (check-for-true  (set<=? S0 S1))
  (check-for-false (set<=? S0 S1 S0))
  (check-for-true  (set<=? S0 S0 S1))
  (check-for-false (set<=? S0 S0 S1 S0))
  (check-for-true  (set<=? S0 S0 S0 S1))

  (check-for-false (set<=? S3 S1))
  (check-for-false (set<=? S3 S1 S3))
  (check-for-false (set<=? S3 S3 S1))
  (check-for-false (set<=? S3 S3 S1 S3))
  (check-for-false (set<=? S3 S3 S3 S1))

;;; --------------------------------------------------------------------
;;; set>=?

  (check-for-true (set>=? S0))
  (check-for-true (set>=? S0 S0))
  (check-for-true (set>=? S0 S0 S0))
  (check-for-true (set>=? S0 S0 S0 S0))

  (check-for-true (set>=? S1))
  (check-for-true (set>=? S1 S1))
  (check-for-true (set>=? S1 S1 S1))
  (check-for-true (set>=? S1 S1 S1 S1))

  (check-for-false (set>=? S0 S1))
  (check-for-false (set>=? S0 S1 S0))
  (check-for-false (set>=? S0 S0 S1))
  (check-for-false (set>=? S0 S0 S1 S0))
  (check-for-false (set>=? S0 S0 S0 S1))

  (check-for-true  (set>=? S3 S1))
  (check-for-false (set>=? S3 S1 S3))
  (check-for-true  (set>=? S3 S3 S1))
  (check-for-false (set>=? S3 S3 S1 S3))
  (check-for-true  (set>=? S3 S3 S3 S1))

;;; --------------------------------------------------------------------
;;; bag=?

  (check-for-true (bag=? B0))
  (check-for-true (bag=? B0 B0))
  (check-for-true (bag=? B0 B0 B0))
  (check-for-true (bag=? B0 B0 B0 B0))

  (check-for-true (bag=? B1))
  (check-for-true (bag=? B1 B1))
  (check-for-true (bag=? B1 B1 B1))
  (check-for-true (bag=? B1 B1 B1 B1))

  (check-for-false (bag=? B0 B1))
  (check-for-false (bag=? B0 B1 B0))
  (check-for-false (bag=? B0 B0 B1))
  (check-for-false (bag=? B0 B0 B1 B0))
  (check-for-false (bag=? B0 B0 B0 B1))

  (check-for-false (bag=? B3 B1))
  (check-for-false (bag=? B3 B1 B3))
  (check-for-false (bag=? B3 B3 B1))
  (check-for-false (bag=? B3 B3 B1 B3))
  (check-for-false (bag=? B3 B3 B3 B1))

  (check-for-true  (bag=? (bag fixnum-comparator 1 2 2 3)
			  (bag fixnum-comparator 1 2 2 3)))
  (check-for-false (bag=? (bag fixnum-comparator 1 2 3)
			  (bag fixnum-comparator 1 2 2 3)))

;;; --------------------------------------------------------------------
;;; bag<?

  (check-for-true  (bag<? B0))
  (check-for-false (bag<? B0 B0))
  (check-for-false (bag<? B0 B0 B0))
  (check-for-false (bag<? B0 B0 B0 B0))

  (check-for-true  (bag<? B1))
  (check-for-false (bag<? B1 B1))
  (check-for-false (bag<? B1 B1 B1))
  (check-for-false (bag<? B1 B1 B1 B1))

  (check-for-true  (bag<? B0 B1))
  (check-for-false (bag<? B0 B1 B0))
  (check-for-false (bag<? B0 B0 B1))
  (check-for-false (bag<? B0 B0 B1 B0))
  (check-for-false (bag<? B0 B0 B0 B1))

  (check-for-false (bag<? B3 B1))
  (check-for-false (bag<? B3 B1 B3))
  (check-for-false (bag<? B3 B3 B1))
  (check-for-false (bag<? B3 B3 B1 B3))
  (check-for-false (bag<? B3 B3 B3 B1))

  (check-for-true  (bag<? B1 B2))
  (check-for-false (bag<? B2 B1))

  (check-for-true  (bag<? B1 B2 B3))
  (check-for-false (bag<? B1 B3 B2))
  (check-for-false (bag<? B3 B1 B2))

  (check-for-true  (bag<? B0 B1 B2 B3))
  (check-for-false (bag<? B0 B1 B3 B2))
  (check-for-false (bag<? B0 B3 B1 B2))
  (check-for-false (bag<? B3 B0 B1 B2))

  (check-for-true  (bag<? (bag fixnum-comparator 1 2 3)
			  (bag fixnum-comparator 1 2 2 3)))
  (check-for-false (bag<? (bag fixnum-comparator 1 2 2 3)
			  (bag fixnum-comparator 1 2 3)))

;;; --------------------------------------------------------------------
;;; bag>?

  (check-for-true  (bag>? B0))
  (check-for-false (bag>? B0 B0))
  (check-for-false (bag>? B0 B0 B0))
  (check-for-false (bag>? B0 B0 B0 B0))

  (check-for-true  (bag>? B1))
  (check-for-false (bag>? B1 B1))
  (check-for-false (bag>? B1 B1 B1))
  (check-for-false (bag>? B1 B1 B1 B1))

  (check-for-false (bag>? B0 B1))
  (check-for-false (bag>? B0 B1 B0))
  (check-for-false (bag>? B0 B0 B1))
  (check-for-false (bag>? B0 B0 B1 B0))
  (check-for-false (bag>? B0 B0 B0 B1))

  (check-for-true  (bag>? B3 B1))
  (check-for-false (bag>? B3 B1 B3))
  (check-for-false (bag>? B3 B3 B1))
  (check-for-false (bag>? B3 B3 B1 B3))
  (check-for-false (bag>? B3 B3 B3 B1))

  (check-for-true  (bag>? B3 B2))
  (check-for-false (bag>? B2 B3))

  (check-for-true  (bag>? B3 B2 B1))
  (check-for-false (bag>? B3 B1 B2))
  (check-for-false (bag>? B1 B3 B2))

  (check-for-true  (bag>? B3 B2 B1 B0))
  (check-for-false (bag>? B3 B2 B0 B1))
  (check-for-false (bag>? B3 B0 B2 B1))
  (check-for-false (bag>? B0 B3 B2 B1))

  (check-for-false (bag>? (bag fixnum-comparator 1 2 3)
			  (bag fixnum-comparator 1 2 2 3)))
  (check-for-true  (bag>? (bag fixnum-comparator 1 2 2 3)
			  (bag fixnum-comparator 1 2 3)))

;;; --------------------------------------------------------------------
;;; bag<=?

  (check-for-true (bag<=? B0))
  (check-for-true (bag<=? B0 B0))
  (check-for-true (bag<=? B0 B0 B0))
  (check-for-true (bag<=? B0 B0 B0 B0))

  (check-for-true (bag<=? B1))
  (check-for-true (bag<=? B1 B1))
  (check-for-true (bag<=? B1 B1 B1))
  (check-for-true (bag<=? B1 B1 B1 B1))

  (check-for-true  (bag<=? B0 B1))
  (check-for-false (bag<=? B0 B1 B0))
  (check-for-true  (bag<=? B0 B0 B1))
  (check-for-false (bag<=? B0 B0 B1 B0))
  (check-for-true  (bag<=? B0 B0 B0 B1))

  (check-for-false (bag<=? B3 B1))
  (check-for-false (bag<=? B3 B1 B3))
  (check-for-false (bag<=? B3 B3 B1))
  (check-for-false (bag<=? B3 B3 B1 B3))
  (check-for-false (bag<=? B3 B3 B3 B1))

  (check-for-true  (bag<=? (bag fixnum-comparator 1 2 3)
			   (bag fixnum-comparator 1 2 2 3)))
  (check-for-false (bag<=? (bag fixnum-comparator 1 2 2 3)
			   (bag fixnum-comparator 1 2 3)))

;;; --------------------------------------------------------------------
;;; bag>=?

  (check-for-true (bag>=? B0))
  (check-for-true (bag>=? B0 B0))
  (check-for-true (bag>=? B0 B0 B0))
  (check-for-true (bag>=? B0 B0 B0 B0))

  (check-for-true (bag>=? B1))
  (check-for-true (bag>=? B1 B1))
  (check-for-true (bag>=? B1 B1 B1))
  (check-for-true (bag>=? B1 B1 B1 B1))

  (check-for-false (bag>=? B0 B1))
  (check-for-false (bag>=? B0 B1 B0))
  (check-for-false (bag>=? B0 B0 B1))
  (check-for-false (bag>=? B0 B0 B1 B0))
  (check-for-false (bag>=? B0 B0 B0 B1))

  (check-for-true  (bag>=? B3 B1))
  (check-for-false (bag>=? B3 B1 B3))
  (check-for-true  (bag>=? B3 B3 B1))
  (check-for-false (bag>=? B3 B3 B1 B3))
  (check-for-true  (bag>=? B3 B3 B3 B1))

  (check-for-false (bag>=? (bag fixnum-comparator 1 2 3)
			   (bag fixnum-comparator 1 2 2 3)))
  (check-for-true  (bag>=? (bag fixnum-comparator 1 2 2 3)
			   (bag fixnum-comparator 1 2 3)))

  #t)


(parametrise ((check-test-name	'theory))

  (define-constant S0 (set fixnum-comparator))
  (define-constant S1 (set fixnum-comparator 1 2))
  (define-constant S2 (set fixnum-comparator 2 3))
  (define-constant S3 (set fixnum-comparator 3 4))

  (define-constant B0 (bag fixnum-comparator))
  (define-constant B1 (bag fixnum-comparator 1 2))
  (define-constant B2 (bag fixnum-comparator 2 3))
  (define-constant B3 (bag fixnum-comparator 3 4))

;;; --------------------------------------------------------------------
;;; set-union

  (check
      (let ((S (set-union S0)))
	(set->list S #t))
    => '())

  (check
      (let ((S (set-union S0 S1)))
	(set->list S #t))
    => '(1 2))

  (check
      (let ((S (set-union S0 S1 S2)))
	(set->list S #t))
    => '(1 2 3))

  (check
      (let ((S (set-union S1 S3)))
	(set->list S #t))
    => '(1 2 3 4))

  (check
      (let ((S (set-union S0 S1 S2 S3)))
	(set->list S #t))
    => '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; set-union!

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S  (set-union! S0)))
	(set->list S #t))
    => '())

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S1 (set fixnum-comparator 1 2))
	     (S (set-union! S0 S1)))
	(set->list S #t))
    => '(1 2))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S3 (set fixnum-comparator 3 4))
	     (S (set-union! S0 S1 S2)))
	(set->list S #t))
    => '(1 2 3))

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S3 (set fixnum-comparator 3 4))
	     (S (set-union! S1 S3)))
	(set->list S #t))
    => '(1 2 3 4))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S3 (set fixnum-comparator 3 4))
	     (S (set-union! S0 S1 S2 S3)))
	(set->list S #t))
    => '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; set-intersection

  ;;(debug-print (set->list S0 #t)
  ;;  	       (set->list S1 #t)
  ;; 	       (set->list S2 #t)
  ;; 	       (set->list S3 #t))

  (check
      (let ((S (set-intersection S0)))
	(set->list S #t))
    => '())

  (check
      (let ((S (set-intersection S0 S1)))
	(set->list S #t))
    => '())

  (check
      (let ((S (set-intersection S0 S1 S2)))
	(set->list S #t))
    => '())

  (check
      (let ((S (set-intersection S1 S3)))
	(set->list S #t))
    => '())

  (check
      (let ((S (set-intersection S1 S2)))
	(set->list S #t))
    => '(2))

;;; --------------------------------------------------------------------
;;; set-intersection!

  ;;(debug-print (set->list S0 #t)
  ;;  	       (set->list S1 #t)
  ;; 	       (set->list S2 #t)
  ;; 	       (set->list S3 #t))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S (set-intersection! S0)))
	(set->list S #t))
    => '())

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S1 (set fixnum-comparator 1 2))
	     (S (set-intersection! S0 S1)))
	(set->list S #t))
    => '())

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-intersection! S0 S1 S2)))
	(set->list S #t))
    => '())

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S3 (set fixnum-comparator 3 4))
	     (S (set-intersection! S1 S3)))
	(set->list S #t))
    => '())

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-intersection! S1 S2)))
	(set->list S #t))
    => '(2))

;;; --------------------------------------------------------------------
;;; set-difference

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference S1 S2)))
	(set->list S #t))
    => '(1))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference S0 S2)))
	(set->list S #t))
    => '())

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference S2 S0)))
	(set->list S #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; set-difference!

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference! S1 S2)))
	(set->list S #t))
    => '(1))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference! S0 S2)))
	(set->list S #t))
    => '())

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-difference! S2 S0)))
	(set->list S #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; set-xor

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor S1 S2)))
	(set->list S #t))
    => '(1 3))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor S0 S2)))
	(set->list S #t))
    => '(2 3))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor S2 S0)))
	(set->list S #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; set-xor!

  (check
      (let* ((S1 (set fixnum-comparator 1 2))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor! S1 S2)))
	(set->list S #t))
    => '(1 3))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor! S0 S2)))
	(set->list S #t))
    => '(2 3))

  (check
      (let* ((S0 (set fixnum-comparator))
	     (S2 (set fixnum-comparator 2 3))
	     (S (set-xor! S2 S0)))
	(set->list S #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-union

  (check
      (let ((B (bag-union B0)))
	(bag->list B #t))
    => '())

  (check
      (let ((B (bag-union B0 B1)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let ((B (bag-union B0 B1 B2)))
	(bag->list B #t))
    => '(1 2 3))

  (check
      (let ((B (bag-union B1 B3)))
	(bag->list B #t))
    => '(1 2 3 4))

  (check
      (let ((B (bag-union B0 B1 B2 B3)))
	(bag->list B #t))
    => '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; bag-union!

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B  (bag-union! B0)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B1 (bag fixnum-comparator 1 2))
	     (B (bag-union! B0 B1)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B (bag-union! B0 B1 B2)))
	(bag->list B #t))
    => '(1 2 3))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B3 (bag fixnum-comparator 3 4))
	     (B (bag-union! B1 B3)))
	(bag->list B #t))
    => '(1 2 3 4))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B (bag-union! B0 B1 B2 B3)))
	(bag->list B #t))
    => '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; bag-intersection

  ;;(debug-print (bag->list B0 #t)
  ;;  	       (bag->list B1 #t)
  ;; 	       (bag->list B2 #t)
  ;; 	       (bag->list B3 #t))

  (check
      (let ((B (bag-intersection B0)))
	(bag->list B #t))
    => '())

  (check
      (let ((B (bag-intersection B0 B1)))
	(bag->list B #t))
    => '())

  (check
      (let ((B (bag-intersection B0 B1 B2)))
	(bag->list B #t))
    => '())

  (check
      (let ((B (bag-intersection B1 B3)))
	(bag->list B #t))
    => '())

  (check
      (let ((B (bag-intersection B1 B2)))
	(bag->list B #t))
    => '(2))

;;; --------------------------------------------------------------------
;;; bag-intersection!

  ;;(debug-print (bag->list B0 #t)
  ;;  	       (bag->list B1 #t)
  ;; 	       (bag->list B2 #t)
  ;; 	       (bag->list B3 #t))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B (bag-intersection! B0)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B1 (bag fixnum-comparator 1 2))
	     (B (bag-intersection! B0 B1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-intersection! B0 B1 B2)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B3 (bag fixnum-comparator 3 4))
	     (B (bag-intersection! B1 B3)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-intersection! B1 B2)))
	(bag->list B #t))
    => '(2))

;;; --------------------------------------------------------------------
;;; bag-difference

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference B1 B2)))
	(bag->list B #t))
    => '(1))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference B0 B2)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference B2 B0)))
	(bag->list B #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-difference!

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference! B1 B2)))
	(bag->list B #t))
    => '(1))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference! B0 B2)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-difference! B2 B0)))
	(bag->list B #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-xor

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor B1 B2)))
	(bag->list B #t))
    => '(1 3))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor B0 B2)))
	(bag->list B #t))
    => '(2 3))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor B2 B0)))
	(bag->list B #t))
    => '(2 3))

;;; --------------------------------------------------------------------
;;; bag-xor!

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor! B1 B2)))
	(bag->list B #t))
    => '(1 3))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor! B0 B2)))
	(bag->list B #t))
    => '(2 3))

  (check
      (let* ((B0 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator 2 3))
	     (B (bag-xor! B2 B0)))
	(bag->list B #t))
    => '(2 3))

  #t)


(parametrise ((check-test-name	'bag-only))

;;; bag-sum

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-sum B1)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let* ((B1 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator))
	     (B  (bag-sum B1 B2)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator))
	     (B  (bag-sum B1 B2)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B  (bag-sum B1 B2)))
	(bag->list B #t))
    => '(1 2 2 3))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B  (bag-sum B1 B2 B3)))
	(bag->list B #t))
    => '(1 2 2 3 3 4))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B4 (bag fixnum-comparator 4 5))
	     (B  (bag-sum B1 B2 B3 B4)))
	(bag->list B #t))
    => '(1 2 2 3 3 4 4 5))

;;; --------------------------------------------------------------------
;;; bag-sum!

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-sum! B1)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let* ((B1 (bag fixnum-comparator))
	     (B2 (bag fixnum-comparator))
	     (B  (bag-sum! B1 B2)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator))
	     (B  (bag-sum! B1 B2)))
	(bag->list B #t))
    => '(1 2))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B  (bag-sum! B1 B2)))
	(bag->list B #t))
    => '(1 2 2 3))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B  (bag-sum! B1 B2 B3)))
	(bag->list B #t))
    => '(1 2 2 3 3 4))

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B2 (bag fixnum-comparator 2 3))
	     (B3 (bag fixnum-comparator 3 4))
	     (B4 (bag fixnum-comparator 4 5))
	     (B  (bag-sum! B1 B2 B3 B4)))
	(bag->list B #t))
    => '(1 2 2 3 3 4 4 5))

;;; --------------------------------------------------------------------
;;; bag-product

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-product 0 B1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator))
	     (B  (bag-product 2 B1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-product 3 B1)))
	(bag->list B #t))
    => '(1 1 1 2 2 2))


;;; --------------------------------------------------------------------
;;; bag-product!

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-product! 0 B1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator))
	     (B  (bag-product! 2 B1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B1 (bag fixnum-comparator 1 2))
	     (B  (bag-product! 3 B1)))
	(bag->list B #t))
    => '(1 1 1 2 2 2))

;;; --------------------------------------------------------------------
;;; bag-unique-size

  (check
      (let ((B (bag fixnum-comparator)))
	(bag-unique-size B))
    => 0)

  (check
      (let ((B (bag fixnum-comparator 1 2)))
	(bag-unique-size B))
    => 2)

  (check
      (let ((B (bag fixnum-comparator 1 2 2)))
	(bag-unique-size B))
    => 2)

;;; --------------------------------------------------------------------
;;; bag-element-count

  (check
      (let ((B (bag fixnum-comparator)))
	(bag-element-count B 1))
    => 0)

  (check
      (let ((B (bag fixnum-comparator 1 2 3)))
	(bag-element-count B 2))
    => 1)

  (check
      (let ((B (bag fixnum-comparator 1 2 2 2 3)))
	(bag-element-count B 2))
    => 3)

;;; --------------------------------------------------------------------
;;; bag-for-each-unique

  (check
      (with-result
	(let ((B (bag fixnum-comparator 1 2 3)))
	  (bag-for-each-unique (lambda (elm count)
				 (add-result (list elm count)))
			       B)))
    => `(,(void) ((1 1)
		  (2 1)
		  (3 1))))

  (check
      (with-result
	(let ((B (bag fixnum-comparator 1 2 2 3 3 3)))
	  (bag-for-each-unique (lambda (elm count)
				 (add-result (list elm count)))
			       B)))
    => `(,(void) ((1 1)
		  (2 2)
		  (3 3))))

;;; --------------------------------------------------------------------
;;; bag-fold-unique

  (check
      (let ((B (bag fixnum-comparator)))
	(bag-fold-unique (lambda (elm count knil)
			   (cons (list elm count) knil))
	  '()
	  B))
    => '())

  (check
      (let ((B (bag fixnum-comparator 1 2 3)))
	(bag-fold-unique (lambda (elm count knil)
			   (cons (list elm count) knil))
	  '()
	  B))
    => '((1 1)
	 (2 1)
	 (3 1)))

  (check
      (let ((B (bag fixnum-comparator 1 1 1 2 2 3)))
	(bag-fold-unique (lambda (elm count knil)
			   (cons (list elm count) knil))
	  '()
	  B))
    => '((1 3)
	 (2 2)
	 (3 1)))

;;; --------------------------------------------------------------------
;;; bag-increment!

  (check
      (let* ((B (bag fixnum-comparator))
	     (B (bag-increment! B 1 0)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B (bag fixnum-comparator))
	     (B (bag-increment! B 1 1)))
	(bag->list B #t))
    => '(1))

  (check
      (let* ((B (bag fixnum-comparator))
	     (B (bag-increment! B 1 3)))
	(bag->list B #t))
    => '(1 1 1))

  (check
      (let* ((B (bag fixnum-comparator 1 2 3))
	     (B (bag-increment! B 2 0)))
	(bag->list B #t))
    => '(1 2 3))

  (check
      (let* ((B (bag fixnum-comparator 1 2 3))
	     (B (bag-increment! B 3 4)))
	(bag->list B #t))
    => '(1 2 3 3 3 3 3))

;;; --------------------------------------------------------------------
;;; bag-decrement!

  (check
      (let* ((B (bag fixnum-comparator))
	     (B (bag-decrement! B 1 0)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B (bag fixnum-comparator 1))
	     (B (bag-decrement! B 1 0)))
	(bag->list B #t))
    => '(1))

  (check
      (let* ((B (bag fixnum-comparator 1))
	     (B (bag-decrement! B 1 1)))
	(bag->list B #t))
    => '())

  (check
      (let* ((B (bag fixnum-comparator 1 2 2 3))
	     (B (bag-decrement! B 2 0)))
	(bag->list B #t))
    => '(1 2 2 3))

  (check
      (let* ((B (bag fixnum-comparator 1 2 2 3))
	     (B (bag-decrement! B 2 1)))
	(bag->list B #t))
    => '(1 2 3))

  (check
      (let* ((B (bag fixnum-comparator 1 2 2 3))
	     (B (bag-decrement! B 2 2)))
	(bag->list B #t))
    => '(1 3))

;;; --------------------------------------------------------------------
;;; bag->set

  (check
      (let* ((B (bag fixnum-comparator 1 2 2 3))
	     (S (bag->set B)))
	(set->list S #t))
    => '(1 2 3))

  (check
      (let* ((B (bag fixnum-comparator))
	     (S (bag->set B)))
	(set->list S #t))
    => '())

;;; --------------------------------------------------------------------
;;; set->bag

  (check
      (let* ((S (set fixnum-comparator 1 2 3))
	     (B (set->bag S)))
	(bag->list B #t))
    => '(1 2 3))

  (check
      (let* ((S (set fixnum-comparator))
	     (B (set->bag S)))
	(bag->list B #t))
    => '())

;;; --------------------------------------------------------------------
;;; set->bag!

  (check
      (let* ((S (set fixnum-comparator))
	     (B (bag fixnum-comparator))
	     (B (set->bag! B S)))
	(bag->list B #t))
    => '())

  (check
      (let* ((S (set fixnum-comparator 1 2 3))
	     (B (bag fixnum-comparator 4 5 6))
	     (B (set->bag! B S)))
	(bag->list B #t))
    => '(1 2 3 4 5 6))

  (check
      (let* ((S (set fixnum-comparator 1 2 3))
	     (B (bag fixnum-comparator 2 3 4))
	     (B (set->bag! B S)))
	(bag->list B #t))
    => '(1 2 2 3 3 4))

;;; --------------------------------------------------------------------
;;; bag->alist

  (check
      (let ((B (bag fixnum-comparator)))
	(bag->alist B))
    => '())

  (check
      (let ((B (bag fixnum-comparator 1 2 3)))
	(bag->alist B #t))
    => '((1 . 1)
	 (2 . 1)
	 (3 . 1)))

  (check
      (let ((B (bag fixnum-comparator 1 1 2 2 3 3)))
	(bag->alist B fx<?))
    => '((1 . 2)
	 (2 . 2)
	 (3 . 2)))

;;; --------------------------------------------------------------------
;;; alist->bag

  (check
      (let ((B (alist->bag fixnum-comparator '())))
	(bag->alist B))
    => '())

  (check
      (let ((B (alist->bag fixnum-comparator '((1 . 1) (2 . 1) (3 . 1)))))
	(bag->alist B #t))
    => '((1 . 1) (2 . 1) (3 . 1)))

  (check
      (let ((B (alist->bag fixnum-comparator '((1 . 2) (2 . 2) (3 . 2)))))
	(bag->alist B fx<?))
    => '((1 . 2) (2 . 2) (3 . 2)))

  #t)


(parametrise ((check-test-name	'comparator))

;;; set-comparator

  (check
      (let ((S1 (set fixnum-comparator 1 2 3))
	    (S2 (set fixnum-comparator 1 2 3)))
	(comparator-equal? set-comparator S1 S2))
    => #t)

  (check
      (let ((S1 (set fixnum-comparator 1 2 3))
	    (S2 (set fixnum-comparator 1 2 3 4)))
	(comparator-equal? set-comparator S1 S2))
    => #f)

  (check
      (let ((S (set fixnum-comparator 1 2 3)))
	(non-negative-exact-integer? (comparator-hash set-comparator S)))
    => #t)

;;; --------------------------------------------------------------------
;;; bag-comparator

  (check
      (let ((B1 (bag fixnum-comparator 1 2 3))
	    (B2 (bag fixnum-comparator 1 2 3)))
	(comparator-equal? bag-comparator B1 B2))
    => #t)

  (check
      (let ((B1 (bag fixnum-comparator 1 2 3))
	    (B2 (bag fixnum-comparator 1 2 3 4)))
	(comparator-equal? bag-comparator B1 B2))
    => #f)

  (check
      (let ((B (bag fixnum-comparator 1 2 3)))
	(non-negative-exact-integer? (comparator-hash bag-comparator B)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
