;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for parser-tools libraries
;;;Date: Tue Sep  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (prefix (nausicaa parser-tools source-locations) sl.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Nausicaa parser tools: source location objects\n")


(parametrise ((check-test-name 'core))

  (check	;normal building
      (let (((S sl.<source-location>) (sl.<source-location> ((sl.line: 10)
							     (sl.column: 20)
							     (sl.offset: 30)))))
	(list (S specified?) (S unspecified?)
	      (S start?)
	      (S line) (S column) (S offset)))
    => '(#t #f #f 10 20 30))

  (check	;default building creates "start" location
      (let (((S sl.<source-location>) (sl.<source-location> ())))
	(list (S specified?) (S unspecified?)
	      (S start?)
	      (S line) (S column) (S offset)))
    => '(#t #f #t 1 1 0))

  #t)


(parametrise ((check-test-name		'hashable))

  (module (<hashable-source-location>)
    (import (prefix (vicare language-extensions makers) mk.))

    (define-class <hashable-source-location>
      (parent sl.<source-location>)
      (mixins (<hashable-and-properties-clauses>
	       (<class>		<hashable-source-location>)))

      (protocol
       (lambda (make-source-location)
	 (lambda (specified? line column offset)
	   ((make-source-location specified? line column offset)
	    #f ;;field from <hashable-and-properties-clauses>
	    ))))

      (maker
       (lambda (stx)
	 (syntax-case stx ()
	   ((_ (?unspecified))
	    (and (identifier? #'?unspecified)
		 (identifier=symbol? #'?unspecified 'unspecified))
	    #'(make-<hashable-source-location> #f 1 1 0))
	   ((_ (?expr ...))
	    #'(%make-hashable-source-location ?expr ...)))))

      #| end of class |# )

    (mk.define-maker %make-hashable-source-location
	(make-<hashable-source-location> #t)
      ;;These default values represent the start location of a source of
      ;;characters.
      ((sl.line:	1)
       (sl.column:	1)
       (sl.offset:	0)))

    #| end of module |# )

;;; --------------------------------------------------------------------

  (check	;normal building
      (let (((S <hashable-source-location>) (<hashable-source-location> ((sl.line: 10)
									 (sl.column: 20)
									 (sl.offset: 30)))))
	(list (S specified?) (S unspecified?)
	      (S start?)
	      (S line) (S column) (S offset)))
    => '(#t #f #f 10 20 30))

  (check	;default building creates "start" location
      (let (((S <hashable-source-location>) (<hashable-source-location> ())))
	(list (S specified?) (S unspecified?)
	      (S start?)
	      (S line) (S column) (S offset)))
    => '(#t #f #t 1 1 0))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (let (((L <hashable-source-location>) (<hashable-source-location> ())))
     (integer? (L hash))))

  (check
      (let ((A (<hashable-source-location> ()))
	    (B (<hashable-source-location> ()))
	    (T (make-hashtable (lambda ((L <hashable-source-location>))
				 (L hash))
			       eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L property-list))
    => '())

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L putprop 'ciao 'salut)
	(L getprop 'ciao))
    => 'salut)

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L getprop 'ciao))
    => #f)

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L putprop 'ciao 'salut)
	(L remprop 'ciao)
	(L getprop 'ciao))
    => #f)

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L putprop 'ciao 'salut)
	(L putprop 'hello 'ohayo)
	(list (L getprop 'ciao)
	      (L getprop 'hello)))
    => '(salut ohayo))

  (check
      (let (((L <hashable-source-location>) (<hashable-source-location> ())))
	(L putprop 'ciao 'salut)
	(L putprop 'hello 'ohayo)
	(L property-list))
    => '((ciao . salut)
	 (hello . ohayo)))

  #t)


(parametrise ((check-test-name 'unspecified))

  (check
      ((sl.<source-location> #:predicate) (sl.unspecified-source-location))
    => #t)

  (check
      (let (((S sl.<source-location>) (sl.unspecified-source-location)))
	(list (S specified?) (S unspecified?)))
    => '(#f #t))

  (check
      (let (((S sl.<source-location>) (sl.<source-location> (unspecified))))
	(list (S specified?) (S line) (S column) (S offset)))
    => '(#f 1 1 0))

  (check
      (let (((S sl.<source-location>) (sl.<source-location> (unspecified))))
	(list (S specified?) (S unspecified?)))
    => '(#f #t))

  (check
      (let (((S sl.<source-location>) (sl.unspecified-source-location)))
	(S string))
    => "no-source")

  (check
      (let (((S sl.<source-location>) (sl.unspecified-source-location)))
	(parametrise ((sl.unspecified-source-location-string "boh"))
	  (S string)))
    => "boh")

  #t)


(parametrise ((check-test-name 'comparison))

  (let (((A sl.<source-location>) (sl.<source-location> ((sl.offset: 3))))
  	((B sl.<source-location>) (sl.<source-location> ((sl.offset: 3)))))

    (check-for-true	(A = B))
    (check-for-false	(A < B))
    (check-for-false	(A > B))
    (check-for-true	(A <= B))
    (check-for-true	(A >= B))

    #f)

  (let (((A sl.<source-location>) (sl.<source-location> ((sl.offset: 3))))
  	((B sl.<source-location>) (sl.<source-location> ((sl.offset: 5)))))

    (check-for-false	(A = B))
    (check-for-true	(A < B))
    (check-for-false	(A > B))
    (check-for-true	(A <= B))
    (check-for-false	(A >= B))

    #f)

  (let (((A sl.<source-location>) (sl.<source-location> ((sl.offset: 5))))
  	((B sl.<source-location>) (sl.<source-location> ((sl.offset: 3)))))

    (check-for-false	(A = B))
    (check-for-false	(A < B))
    (check-for-true	(A > B))
    (check-for-false	(A <= B))
    (check-for-true	(A >= B))

    #f)

  #t)


(parametrise ((check-test-name 'update))

  (check	;token length
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (L update 4))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: (+ 20 4))
				 (sl.offset: (+ 30 4)))))
	(M = R))
    => #t)

  (check	;newline char
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (L update #\newline))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   (+ 10 1))
				 (sl.column: 1)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;return char
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (L update #\return))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: (+ 20 1))
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;not honored return char
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-honor-return #f))
	    (L update #\return)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: (+ 20 1))
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;honored return char
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-honor-return #t))
	    (L update #\return)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 1)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;tab char, default
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (L update #\tab))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 24)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;tab char, 8chars
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-tab-function sl.source-location-tab-function/8chars))
	    (L update #\tab)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 24)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;tab char, table function
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-tab-function sl.source-location-tab-function/tab-table))
	    (L update #\tab)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 24)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;tab char, table function, tab table
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-tab-function sl.source-location-tab-function/tab-table)
			(sl.source-location-tab-table    '(6 12 18 24 30 36)))
	    (L update #\tab)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 24)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  (check	;tab char, table function, short tab table
      (let ()
	(define #(L sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 20)
				 (sl.offset: 30))))
	(define #(M sl.<source-location>)
	  (parametrise ((sl.source-location-tab-function sl.source-location-tab-function/tab-table)
			(sl.source-location-tab-table    '(6 12 18)))
	    (L update #\tab)))
	(define #(R sl.<source-location>)
	  (sl.<source-location> ((sl.line:   10)
				 (sl.column: 24)
				 (sl.offset: (+ 30 1)))))
	(M = R))
    => #t)

  #t)


(parametrise ((check-test-name 'misc))

  (check
      (object->string (sl.<source-location> ((sl.line: 10)
					     (sl.column: 20)
					     (sl.offset: 30))))
    => "10:20")

  (check
      (let (((S sl.<source-location>) (sl.<source-location> ((sl.line: 10)
							     (sl.column: 20)
							     (sl.offset: 30)))))
	(list (S line string) (S column string) (S offset string)))
    => '("10" "20" "30"))

  #t)


;;;; done

(check-report)

;;; end of file
