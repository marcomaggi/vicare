;;;
;;;Part of: Vicare Scheme
;;;Contents: test for general SRFI 0
;;;Date: Thu Dec 20, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (vicare)
  (srfi :0)
  (prefix (srfi private registry)
	  private.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing SRFI libraries: SRFI 0 cond-expand\n")

(when #f
  (check-pretty-print private.run-time-features)
  (check-pretty-print private.expand-time-features))


(parametrise ((check-test-name	'srfis))

  (define-syntax test
    (syntax-rules ()
      ((_ ?num ?name)
       (begin
	 (check
	     (cond-expand
	      ((srfi ?num)	#t)
	      (else		#f))
	   => #t)
	 (check
	     (cond-expand
	      ((srfi ?num ?name)	#t)
	      (else			#f))
	   => #t)
	 ))))

  (define-syntax test1
    (syntax-rules ()
      ((_ ?num ?name)
       (begin
	 (check
	     (cond-expand
	      (?num	#t)
	      (else	#f))
	   => #t)
	 (check
	     (cond-expand
	      (?name	#t)
	      (else	#f))
	   => #t)
	 ))))

;;; --------------------------------------------------------------------

  (test :0	cond-expand)
  (test :1	lists)
  (test :2	and-let*)
  #;(test :5	let)
  (test :6	basic-string-ports)
  (test :8	receive)
  (test :9	records)
  (test :11	let-values)
  (test :13	strings)
  (test :14	char-sets)
  (test :16	case-lambda)
  #;(test :17	generalized-set!)
  #;(test :18	multithreading)
  (test :19	time)
  #;(test :21	real-time-multithreading)
  (test :23	error)
  (test :25	multi-dimensional-arrays)
  (test :26	cut)
  (test :27	random-bits)
  #;(test :28	basic-format-strings)
  #;(test :29	localization)
  (test :31	rec)
  (test :37	args-fold)
  (test :38	with-shared-structure)
  (test :39	parameters)
  (test :41	streams)
  (test :42	eager-comprehensions)
  (test :43	vectors)
  #;(test :44	collections)
  (test :45	lazy)
  #;(test :46	syntax-rules)
  #;(test :47	arrays)
  (test :48	intermediate-format-strings)
  #;(test :51	rest-values)
  #;(test :54	cat)
  #;(test :57	records)
  #;(test :59	vicinities)
  #;(test :60	integer-bits)
  (test :61	cond)
  #;(test :63	arrays)
  (test :64	testing)
  #;(test :66	octet-vectors)
  (test :67	compare-procedures)
  (test :69	basic-hash-tables)
  #;(test :71	let)
  #;(test :74	blobs)
  (test :78	lightweight-testing)
  #;(test :86	mu-and-nu)
  #;(test :87	case)
  #;(test :95	sorting-and-merging)
  (test :98	os-environment-variables)
  (test :99	records)
  (test :101	random-access-lists)

;;; --------------------------------------------------------------------

  (test1 srfi-0		srfi-0-cond-expand)
  (test1 srfi-1		srfi-1-lists)
  (test1 srfi-2		srfi-2-and-let*)
  #;(test1 srfi-5	srfi-5-let)
  (test1 srfi-6	srfi-6-basic-string-ports)
  (test1 srfi-8	srfi-8-receive)
  (test1 srfi-9	srfi-9-records)
  (test1 srfi-11	srfi-11-let-values)
  (test1 srfi-13	srfi-13-strings)
  (test1 srfi-14	srfi-14-char-sets)
  (test1 srfi-16	srfi-16-case-lambda)
  #;(test1 srfi-17	srfi-17-generalized-set!)
  #;(test1 srfi-18	srfi-18-multithreading)
  (test1 srfi-19	srfi-19-time)
  #;(test1 srfi-21	srfi-21-real-time-multithreading)
  (test1 srfi-23	srfi-23-error)
  (test1 srfi-25	srfi-25-multi-dimensional-arrays)
  (test1 srfi-26	srfi-26-cut)
  (test1 srfi-27	srfi-27-random-bits)
  #;(test1 srfi-28	srfi-28-basic-format-strings)
  #;(test1 srfi-29	srfi-29-localization)
  (test1 srfi-31	srfi-31-rec)
  (test1 srfi-37	srfi-37-args-fold)
  (test1 srfi-38	srfi-38-with-shared-structure)
  (test1 srfi-39	srfi-39-parameters)
  (test1 srfi-41	srfi-41-streams)
  (test1 srfi-42	srfi-42-eager-comprehensions)
  (test1 srfi-43	srfi-43-vectors)
  #;(test1 srfi-44	srfi-44-collections)
  (test1 srfi-45	srfi-45-lazy)
  #;(test1 srfi-46	srfi-46-syntax-rules)
  #;(test1 srfi-47	srfi-47-arrays)
  (test1 srfi-48	srfi-48-intermediate-format-strings)
  #;(test1 srfi-51	srfi-51-rest-values)
  #;(test1 srfi-54	srfi-54-cat)
  #;(test1 srfi-57	srfi-57-records)
  #;(test1 srfi-59	srfi-59-vicinities)
  #;(test1 srfi-60	srfi-60-integer-bits)
  (test1 srfi-61	srfi-61-cond)
  #;(test1 srfi-63	srfi-63-arrays)
  (test1 srfi-64	srfi-64-testing)
  #;(test1 srfi-66	srfi-66-octet-vectors)
  (test1 srfi-67	srfi-67-compare-procedures)
  (test1 srfi-69	srfi-69-basic-hash-tables)
  #;(test1 srfi-71	srfi-71-let)
  #;(test1 srfi-74	srfi-74-blobs)
  (test1 srfi-78	srfi-78-lightweight-testing)
  #;(test1 srfi-86	srfi-86-mu-and-nu)
  #;(test1 srfi-87	srfi-87-case)
  #;(test1 srfi-95	srfi-95-sorting-and-merging)
  (test1 srfi-98	srfi-98-os-environment-variables)
  (test1 srfi-99	srfi-99-records)

  #t)


(parametrise ((check-test-name	'implementation))

  (check
      (cond-expand
       (vicare		#t)
       (else		#f))
    => #t)

  (check
      (cond-expand
       (sagittarius	#f)
       (else		#t))
    => #t)

  #t)


(parametrise ((check-test-name	'os))

  (check
      (cond-expand
       ((and linux posix)	#t)
       (else			#f))
    => #t)

  (check
      (cond-expand
       (linux		#t)
       (else		#f))
    => #t)

  (check
      (cond-expand
       (posix		#t)
       (else		#f))
    => #t)

  #t)


(parametrise ((check-test-name	'composition))

  (check
      (cond-expand
       ((and srfi-0 srfi-1)	#t)
       (else			#f))
    => #t)

  (check
      (cond-expand
       ((and srfi-0 ciao srfi-1)
	#t)
       (else #f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (cond-expand
       ((or srfi-0 ciao)
	#t)
       (else #f))
    => #t)

  (check
      (cond-expand
       ((or ciao srfi-0)
	#t)
       (else #f))
    => #t)

  (check
      (cond-expand
       ((or hello ciao)
	#t)
       (else #f))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (cond-expand
       ((not ciao)
	#t)
       (else #f))
    => #t)

  (check
      (cond-expand
       ((not srfi-0)
	#t)
       (else #f))
    => #f)

  #f)


;;;; done

(check-report)

;;; end of file
