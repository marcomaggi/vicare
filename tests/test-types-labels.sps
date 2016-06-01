;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for DEFINE-LABEL
;;;Date: Mon Apr 25, 2016
;;;
;;;Abstract
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
(program (test-types-labels)
  (options typed-language)
  (import (vicare)
    (vicare language-extensions labels)
    (vicare checks)
    (prefix (vicare expander) expander::))

(check-set-mode! 'report-failed)
(check-display "*** test Vicare typed language: DEFINE-LABEL\n")


;;;; helpers

(define-type <all-fixnums>
  (or <non-negative-fixnum> <negative-fixnum>))

(define-syntax matching
  (syntax-rules (=>)
    ((_ ?one ?two => ?expected)
     (check
	 (type-annotation-matching ?one ?two)
       => (quote ?expected)))
    ))


(parametrise ((check-test-name	'type-inspection))

  ;;No predicate, so this is acts as synonym for "<fixnum>".
  ;;
  (define-label <fx>
    (parent <fixnum>))

  ;;No predicate, so this is acts as synonym for "<fixnum>".
  ;;
  (define-label <pos-fx>
    (parent <fixnum>)
    (type-predicate
      (lambda (parent?)
	(lambda (obj)
	  (and (parent?   obj)
	       (positive? obj))))))

;;; --------------------------------------------------------------------
;;; super and sub

  (check-for-true	(type-annotation-super-and-sub? <fixnum> <fx>))
  (check-for-true	(type-annotation-super-and-sub? <fx> <fixnum>))
  (check-for-true	(type-annotation-super-and-sub? <fx> <positive-fixnum>))
  (check-for-true	(type-annotation-super-and-sub? <fx> <zero-fixnum>))
  (check-for-true	(type-annotation-super-and-sub? <fx> <negative-fixnum>))
  (check-for-true	(type-annotation-super-and-sub? <fx> <all-fixnums>))

  (check-for-true	(type-annotation-super-and-sub? <fixnum> <pos-fx>))
  (check-for-false	(type-annotation-super-and-sub? <pos-fx> <fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <pos-fx> <positive-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <pos-fx> <zero-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <pos-fx> <negative-fixnum>))
  (check-for-false	(type-annotation-super-and-sub? <pos-fx> <all-fixnums>))

;;; --------------------------------------------------------------------
;;; matching

  (matching <top>			<fx>			=> exact-match)
  (matching <exact-integer>		<fx>			=> exact-match)
  (matching (parent-of <fixnum>)	<fx>			=> exact-match)
  (matching <fixnum>			<fx>			=> exact-match)

  (matching <fx>			<fixnum>		=> exact-match)
  (matching <fx>			<positive-fixnum>	=> exact-match)
  (matching <fx>			<zero-fixnum>		=> exact-match)
  (matching <fx>			<negative-fixnum>	=> exact-match)
  (matching <fx>			<all-fixnums>		=> exact-match)
  (matching <fx>			<string>		=> no-match)
  (matching <fx>			<top>			=> possible-match)
  (matching <fx>			<exact-integer>		=> possible-match)

  ;;The type "<fx>"  is not equal to  an ancestor of "<fixnum>",  while "<fixnum>" is
  ;;equal to an ancestor of "<fx>".
  (matching (ancestor-of <fixnum>) <fx>			=> no-match)
  (matching (ancestor-of <fx>) <fixnum>			=> exact-match)
  (matching <fx> (ancestor-of <fixnum>) 		=> no-match)
  (matching <fixnum> (ancestor-of <fx>)			=> exact-match)

  (matching <string> <fx>				=> no-match)
  (matching <fx> <string>				=> no-match)

;;;

  (matching <top>		<pos-fx>		=> exact-match)
  (matching <exact-integer>	<pos-fx>		=> exact-match)
  (matching <fixnum>		<pos-fx>		=> exact-match)

  (matching <pos-fx>		<top>			=> possible-match)
  (matching <pos-fx>		<number>		=> possible-match)
  (matching <pos-fx>		<integer>		=> possible-match)
  (matching <pos-fx>		<exact-integer>		=> possible-match)
  (matching <pos-fx>		<fixnum>		=> possible-match)
  (matching <pos-fx>		<positive-fixnum>	=> possible-match)
  (matching <pos-fx>		<zero-fixnum>		=> possible-match)
  (matching <pos-fx>		<negative-fixnum>	=> possible-match)
  (matching <pos-fx>		<all-fixnums>		=> possible-match)

  (matching <pos-fx>		(parent-of <fixnum>)	=> possible-match)

  ;;The type "<pos-fx>"  is not equal to an ancestor  of "<fixnum>", while "<fixnum>"
  ;;is equal to an ancestor of "<pos-fx>".
  (matching <pos-fx> (ancestor-of <fixnum>)		=> no-match)
  (matching <fixnum> (ancestor-of <pos-fx>)		=> exact-match)
  (matching (ancestor-of <fixnum>) <pos-fx>		=> no-match)
  (matching (ancestor-of <pos-fx>) <fixnum>		=> exact-match)

  ;;The type "<exact>" is a union containing "<fixnum>".
  (matching <pos-fx>		<exact>			=> possible-match)
  (matching <exact>		<pos-fx>		=> exact-match)

  (matching <string>		<pos-fx>		=> no-match)
  (matching <pos-fx>		<string>		=> no-match)

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'basic))

;;; without type predicate

  (internal-body
    (define-label <my-fixnum>
      (parent <all-fixnums>))

    (check-for-true	(is-a? 123 <my-fixnum>))
    (check-for-false	(is-a? "ciao" <my-fixnum>))

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; with type predicate

  (internal-body
    (define-label <comparison-fixnum>
      (parent <all-fixnums>)
      (type-predicate
	(lambda (parent?)
	  (lambda ({_ <boolean>} obj)
	    (and (parent? obj)
		 (fx<=? obj +1)
		 (fx>=? obj -1)))))
      (method ({foo <string>} {O <comparison-fixnum>})
	(number->string O)))

;;;
    (check-for-true	(type-annotation-super-and-sub? <fixnum> <comparison-fixnum>))
    (check-for-false	(type-annotation-super-and-sub? <comparison-fixnum> <fixnum>))
    (check-for-false	(type-annotation-super-and-sub? <positive-fixnum> <comparison-fixnum>))

    (matching <number>			<comparison-fixnum>	=> exact-match)
    (matching <integer>			<comparison-fixnum>	=> exact-match)
    (matching <exact-integer>		<comparison-fixnum>	=> exact-match)
    ;;The type "<comparison-fixnum>" is not equal to an ancestor of "<fixnum>".
    (matching (ancestor-of <fixnum>)	<comparison-fixnum>	=> no-match)
    (matching <fixnum>			<comparison-fixnum>	=> exact-match)
    (matching <zero-fixnum>		<comparison-fixnum>	=> possible-match)
    (matching <positive-fixnum>		<comparison-fixnum>	=> possible-match)
    (matching <negative-fixnum>		<comparison-fixnum>	=> possible-match)
    (matching <all-fixnums>		<comparison-fixnum>	=> exact-match)

    (matching <comparison-fixnum>	<number>		=> possible-match)
    (matching <comparison-fixnum>	<integer>		=> possible-match)
    (matching <comparison-fixnum>	<exact-integer>		=> possible-match)
    ;;The type "<comparison-fixnum>" is not equal to an ancestor of "<fixnum>".
    (matching <comparison-fixnum>	(ancestor-of <fixnum>)	=> no-match)
    (matching <comparison-fixnum>	<fixnum>		=> possible-match)
    (matching <comparison-fixnum>	<positive-fixnum>	=> possible-match)
    (matching <comparison-fixnum>	<zero-fixnum>		=> possible-match)
    (matching <comparison-fixnum>	<negative-fixnum>	=> possible-match)
    (matching <comparison-fixnum>	<all-fixnums>		=> possible-match)
;;;
    (check-for-true	(is-a? +1 <comparison-fixnum>))
    (check-for-true	(is-a?  0 <comparison-fixnum>))
    (check-for-true	(is-a? -1 <comparison-fixnum>))
    (check-for-false	(is-a? +2 <comparison-fixnum>))
    (check-for-false	(is-a? -2 <comparison-fixnum>))

    (check-for-true	(is-a? (unsafe-cast-signature (<comparison-fixnum>) 0) <comparison-fixnum>))

    (check-for-true	(is-a? (unsafe-cast-signature (<top>) +1) <comparison-fixnum>))
    (check-for-true	(is-a? (unsafe-cast-signature (<top>)  0) <comparison-fixnum>))
    (check-for-true	(is-a? (unsafe-cast-signature (<top>) -1) <comparison-fixnum>))
    (check-for-false	(is-a? (unsafe-cast-signature (<top>) +2) <comparison-fixnum>))
    (check-for-false	(is-a? (unsafe-cast-signature (<top>) -2) <comparison-fixnum>))

    (void))

  (void))


(parametrise ((check-test-name	'methods))

  (check
      (internal-body
	(define-label <my-fixnum>
	  (parent <all-fixnums>)
	  (method (doit {O <my-fixnum>})
	    999))
	(define {O <my-fixnum>}
	  123)
	(values (.hash O) (.doit O)))
    => 123 999)

  (internal-body
    (define-label <comparison-fixnum>
      (parent <all-fixnums>)
      (type-predicate
	(lambda (parent?)
	  (lambda ({_ <boolean>} obj)
	    (and (parent? obj)
		 (fx<=? obj +1)
		 (fx>=? obj -1)))))
      (method ({foo <string>} {O <comparison-fixnum>})
	(number->string O)))

    (check
	(let (({O <comparison-fixnum>} 1))
	  (.foo O))
      => "1")

    (check
	(.foo (unsafe-cast-signature (<comparison-fixnum>) +1))
      => "1")

    (void))

  (void))


(parametrise ((check-test-name	'case-methods))

  (check
      (internal-body

	(define-label <my-string>
	  (parent <string>)
	  (case-method doit
	    (({_ <my-string>} {O <my-string>} {suffix <string>})
	     (string-append O suffix))))

	(define {O <my-string>}
	  "ciao")

	(values (.doit O " mamma")
		(.doit (.doit O " mamma") " ho")))
    => "ciao mamma" "ciao mamma ho")

  (check
      (internal-body

	(define-label <my-string>
	  (parent <string>)
	  (case-method doit
	    (({_ <my-string>} {O <my-string>} {suffix <string>})
	     (string-append O suffix))
	    (({_ <my-string>} {O <my-string>} {prefix <string>} {suffix <string>})
	     (string-append prefix O suffix))))

	(define {O <my-string>}
	  "ciao")

	(values (.doit O "hey, " " mamma")
		(.doit (.doit O " mamma") " ho")))
    => "hey, ciao mamma" "ciao mamma ho")

  (void))


(parametrise ((check-test-name	'constructor))

  (internal-body
    (define-label <twofx>
      (parent (vector <fixnum> <fixnum>))
      (constructor ({A <fixnum>} {B <fixnum>})
	(vector A B))
      (type-predicate
	(lambda (parent?)
	  (lambda (obj)
	    (and (parent? obj)
		 (fixnum? (vector-ref obj 0))
		 (fixnum? (vector-ref obj 1)))))))

    (define {O <twofx>}
      (new <twofx> 1 2))

;;;
    (check-for-true	(is-a? '#(8 9) <twofx>))
    (check-for-false	(is-a? '#(a b) <twofx>))
;;;
    (check-for-true (type-annotation-super-and-sub? (vector <fixnum> <fixnum>) <twofx>))
    (check-for-true (type-annotation-super-and-sub? <vector> <twofx>))
    (check-for-true (type-annotation-super-and-sub? <nevector> <twofx>))

    (check (type-annotation-matching <nevector> <twofx>)	=> 'exact-match)
    (check (type-annotation-matching <twofx> <nevector>)	=> 'possible-match)

    (check (type-annotation-matching (vector <fixnum> <fixnum>) <twofx>)	=> 'exact-match)
    (check (type-annotation-matching <twofx> (vector <fixnum> <fixnum>))	=> 'possible-match)

    (check (type-annotation-matching (vector-of <fixnum>) <twofx>)	=> 'exact-match)
    (check (type-annotation-matching <twofx> (vector-of <fixnum>))	=> 'possible-match)
;;;
    (check (.ref O 0)	=> 1)
    (check (.ref O 1)	=> 2)

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------

  ;;Constructor with two clauses.
  ;;
  (internal-body
    (define-label <some-fx>
      (parent <nevector>)
      (constructor ({A <fixnum>} {B <fixnum>})
	(vector A B))
      (constructor ({A <fixnum>} {B <fixnum>} {C <fixnum>})
	(vector A B C))
      (type-predicate
	(lambda (parent?)
	  (lambda (obj)
	    (and (parent? obj)
		 (vector-for-all fixnum? obj))))))

    (define {A <some-fx>}
      (new <some-fx> 1 2))

    (define {B <some-fx>}
      (new <some-fx> 4 5 6))

    (check (.ref A 0)	=> 1)
    (check (.ref B 2)	=> 6)

    #| end of INTERNAL-BODY |# )

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'destructor))

  (internal-body
    (define-label <twofx>
      (parent (vector <fixnum> <fixnum>))
      (constructor ({A <fixnum>} {B <fixnum>})
	(vector A B))
      (destructor ({O <twofx>})
	(add-result (list 'destroyed O))
	999)
      (type-predicate
	(lambda (parent?)
	  (lambda (obj)
	    (and (parent? obj)
		 (fixnum? (vector-ref obj 0))
		 (fixnum? (vector-ref obj 1)))))))

    (define {O <twofx>}
      (new <twofx> 1 2))

    (check-for-true (type-annotation-super-and-sub? <vector> <empty-vector>))
    (check-for-true (type-annotation-super-and-sub? <vector> <nevector>))
    (check-for-true (type-annotation-super-and-sub? <nevector> (vector <fixnum> <fixnum>)))
    (check-for-true (type-annotation-super-and-sub? <nevector> <twofx>))

    (check
	(with-result
	  (delete O))
      => '(999 ((destroyed #(1 2)))))

    #| end of INTERNAL-BODY |# )

  #| end of PARAMETRISE |# )


(parametrise ((check-test-name	'misc-operations))

  (define-label <my-fixnum>
    (parent <fixnum>)
    (equality-predicate
      (lambda (parent-func)
	fx=?))
    (comparison-procedure
      (lambda ({parent-func (comparison-procedure <fixnum>)})
	(lambda ({a <my-fixnum>} {b <my-fixnum>})
	  (cond ((fx=? a b)	 0)
		((fx<=? a b)	-1)
		(else		+1)))))
    (hash-function
      (lambda (parent-func)
	(lambda (obj)
	  (add1 (parent-func obj))))))

;;; --------------------------------------------------------------------

  (check-for-true	((equality-predicate <my-fixnum>) 1 1))
  (check-for-false	((equality-predicate <my-fixnum>) 1 2))

;;; --------------------------------------------------------------------

  (check
      ((comparison-procedure <my-fixnum>) 1 1)
    => 0)

  (check
      ((comparison-procedure <my-fixnum>) 1 2)
    => -1)

  (check
      ((comparison-procedure <my-fixnum>) 2 1)
    => +1)

;;; --------------------------------------------------------------------

  (check
      ((hash-function <my-fixnum>) 1)
    => (add1 (fixnum-hash 1)))

  (check
      ((hash-function <my-fixnum>) 123)
    => (add1 (fixnum-hash 123)))

  (void))


(parametrise ((check-test-name	'doc-examples))

  ;;Just an alias for "<string>".
  ;;
  (internal-body

    (define-label <String>
      (parent <string>))

    (define {O <String>}
      "ciao")

    (check (.length O)	=> 4)
    (check (.hash O)	=> (string-hash "ciao"))

    #| end of INTERNAL-BODY |# )

  ;;Custom hash function.
  ;;
  (internal-body

    (define-label <String>
      (parent <string>)
      (hash-function
	(lambda (parent-func)
	  (lambda (S)
	    (if (string-empty? S)
		0
	      (char-hash (string-ref S 0)))))))

    (define {O <String>}
      "ciao")

    (check (.hash O)	=> (char-hash #\c))

    #| end of INTERNAL-BODY |# )

  ;;Method to increment a fixnum.
  ;;
  (internal-body

    (define-label <fx>
      (parent <fixnum>)
      (method (incr {O <fx>})
	(fxadd1 O)))

    (define {O <fx>}
      10)

    (check (.incr O)	=> 11)

    #| end of INTERNAL-BODY |# )

  ;;Method to append prefixes and suffixes.
  ;;
  (internal-body

    (define-label <String>
      (parent <string>)
      (case-method append
	(({_ <String>} {O <String>} {suff <String>})
	 (string-append O suff))
	(({_ <String>} {O <String>} {pref <String>} {suff <String>})
	 (string-append pref O suff))))

    (define {O <String>}
      "ciao")

    (check (.append O "-suff")		=> "ciao-suff")
    (check (.append O "pref-" "-suff")	=> "pref-ciao-suff")

    (check (.length (.append O "pref-" "-suff"))	=> 14)

    #| end of INTERNAL-BODY |# )

  ;;Comparison fixnum.
  ;;
  (internal-body

    (define-label <comparison-fixnum>
      (parent (or <non-negative-fixnum> <negative-fixnum>))
      (type-predicate
	(lambda (parent?)
	  (lambda ({_ <boolean>} obj)
	    (and (parent? obj)
		 (fx<=? obj +1)
		 (fx>=? obj -1))))))

    (check (is-a? +1 <comparison-fixnum>)	=> #t)
    (check (is-a? -1 <comparison-fixnum>)	=> #t)
    (check (is-a?  0 <comparison-fixnum>)	=> #t)

    (check (is-a? +2 <comparison-fixnum>)	=> #f)
    (check (is-a? -2 <comparison-fixnum>)	=> #f)

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; constructor

  (internal-body
    (define-label <vec>
      (parent <nevector>)
      (constructor (a b)
	(vector a b))
      (constructor (a b c)
	(vector a b c)))

    (check (new <vec> 1 2)	=> '#(1 2))
    (check (new <vec> 1 2 3)	=> '#(1 2 3))

    (check-for-true	(new <vec> 1 2))

    #| end of INTERNAL-BODY |# )

;;; --------------------------------------------------------------------
;;; destructor

  (internal-body

    (define-label <vec>
      (parent <vector>)
      (destructor ({O <vec>})
	`(deleted ,O)))

    (define {O <vec>}
      '#(1 2))

    (check
	(delete O)
      => '(deleted #(1 2)))

    #| end of INTERNAL-BODY |# )

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
