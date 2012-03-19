;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for r6rs records
;;;Date: Fri Mar 16, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rename (vicare)
		(make-record-type-descriptor make-record-type-descriptor*)
		(make-record-constructor-descriptor make-record-constructor-descriptor*))
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare R6RS records\n")


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

(define-auxiliary-syntaxes
  name: parent: uid: sealed: opaque: fields:)

(define-syntax make-record-type-descriptor
  (syntax-rules (name: parent: uid: sealed: opaque: fields:)
    ((_ (name: ?name) (parent: ?parent) (uid: ?uid)
	(sealed: ?sealed) (opaque: ?opaque) (fields: ?fields))
     (make-record-type-descriptor* ?name ?parent ?uid ?sealed ?opaque ?fields))))

(define-syntax make-record-constructor-descriptor
  (syntax-rules (rtd: parent-rcd: protocol:)
    ((_ (rtd: ?rtd) (parent-rcd: ?prcd) (protocol: ?protocol))
     (make-record-constructor-descriptor* ?rtd ?prcd ?protocol))))


(parametrise ((check-test-name	'make-rtd))

  (define parent-0
    (make-record-type-descriptor
     (name: 'rtd-parent-0) (parent: #f) (uid: 'rtd-parent-0)
     (sealed: #f) (opaque: #f) (fields: '#())))

  (define parent-1
    (make-record-type-descriptor
     (name: 'rtd-parent-1) (parent: #f) (uid: 'rtd-parent-1)
     (sealed: #f) (opaque: #f) (fields: '#())))

;;; --------------------------------------------------------------------

  (check	;some correct configuration values
      (let ((rtd (make-record-type-descriptor (name:	'make-rtd)
					      (parent:	#f)
					      (uid:	'make-rtd-0)
					      (sealed:	#f)
					      (opaque:	#f)
					      (fields:	'#()))))
	(list (record-type-descriptor? rtd)
	      (record-type-name rtd)
	      (record-type-parent rtd)
	      (record-type-uid rtd)
	      (record-type-generative? rtd)
	      (record-type-sealed? rtd)
	      (record-type-opaque? rtd)
	      (record-type-field-names rtd)))
    => '(#t make-rtd #f make-rtd-0 #f #f #f #()))

  (check	;other correct configuration values
      (let ((rtd (make-record-type-descriptor (name:	'make-rtd)
					      (parent:	#f)
					      (uid:	#f)
					      (sealed:	#t)
					      (opaque:	#t)
					      (fields:	'#((mutable a) (immutable b))))))
	(list (record-type-descriptor? rtd)
	      (record-type-name rtd)
	      (record-type-parent rtd)
	      (record-type-uid rtd)
	      (record-type-generative? rtd)
	      (record-type-sealed? rtd)
	      (record-type-opaque? rtd)
	      (record-type-field-names rtd)
	      (record-field-mutable? rtd 0)
	      (record-field-mutable? rtd 1)))
    => '(#t make-rtd #f #f #t #t #t #(a b) #t #f))

  (check	;non-generative defined twice
      (let ((rtd1 (make-record-type-descriptor (name:	'make-rtd)
					       (parent:	#f)
					       (uid:	'make-rtd-1)
					       (sealed:	#f)
					       (opaque:	#f)
					       (fields:	'#())))
	    (rtd2 (make-record-type-descriptor (name:	'make-rtd)
					       (parent:	#f)
					       (uid:	'make-rtd-1)
					       (sealed:	#f)
					       (opaque:	#f)
					       (fields:	'#()))))
	(eq? rtd1 rtd2))
    => #t)

  (check	;non-generative defined twice with different names
      (let ((name1	'make-rtd-one)
	    (name2	'make-rtd-two)
	    (parent	#f)
	    (uid	'make-rtd-2)
	    (sealed?	#f)
	    (opaque?	#f)
	    (fields	'#()))
	(let ((rtd1 (make-record-type-descriptor* name1 parent uid sealed? opaque? fields))
	      (rtd2 (make-record-type-descriptor* name2 parent uid sealed? opaque? fields)))
	  (eq? rtd1 rtd2)))
    => #t)

;;; --------------------------------------------------------------------
;;; errors, non-generative defined twice with different values

  (let ((rtd (make-record-type-descriptor* 'N parent-0 'make-rtd-3 #f #f '#())))
    (check	;different parent
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-3 #f #f '#())))
      => (list rtd (list 'N #f 'make-rtd-3 #f #f '#()))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-4 #f #f '#())))
    (check	;different parent
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N parent-0 'make-rtd-4 #f #f '#())))
      => (list rtd (list 'N parent-0 'make-rtd-4 #f #f '#()))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-5 #f #f '#())))
    (check	;different sealed
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-5 #t #f '#())))
      => (list rtd (list 'N #f 'make-rtd-5 #t #f '#()))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-6 #f #f '#())))
    (check	;different opaque
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-6 #f #t '#())))
      => (list rtd (list 'N #f 'make-rtd-6 #f #t '#()))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-7 #f #f '#())))
    (check	;different fields
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-7 #f #f '#((mutable a)))))
      => (list rtd (list 'N #f 'make-rtd-7 #f #f '#((mutable a))))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-8 #f #f '#((mutable a)))))
    (check	;different fields
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-8 #f #f '#((immutable a)))))
      => (list rtd (list 'N #f 'make-rtd-8 #f #f '#((immutable a))))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-9 #f #f '#((immutable a)))))
    (check	;different fields
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-9 #f #f '#((immutable b)))))
      => (list rtd (list 'N #f 'make-rtd-9 #f #f '#((immutable b))))))

;;; --------------------------------------------------------------------
;;; errors, wrong UID

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: "ciao")
	 (sealed: #f) (opaque: #f) (fields: '#())))
    => '("ciao"))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #t)
	 (sealed: #f) (opaque: #f) (fields: '#())))
    => '(#t))

;;; --------------------------------------------------------------------
;;; errors, wrong sealed

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: "ciao") (opaque: #f) (fields: '#())))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; errors, wrong opaque

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: "ciao") (fields: '#())))
    => '("ciao"))

;;; --------------------------------------------------------------------
;;; errors, wrong fields

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '())))
    => '(()))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '#(a))))
    => '(#(a)))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '#((a b c)))))
    => '(#((a b c))))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '#((mutable 123)))))
    => '(#((mutable 123))))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '#((ciao ciao)))))
    => '(#((ciao ciao))))

  (check
      (catch #f
	(make-record-type-descriptor
	 (name: 'a-name) (parent: #f) (uid: #f)
	 (sealed: #f) (opaque: #f) (fields: '#((mutable a) (immutable 1)))))
    => '(#((mutable a) (immutable 1))))

  #t)


(parametrise ((check-test-name	'make-rcd))

;;; no parent

  (check	;correct configuration values, no fields
      (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd)))
	(pred (maker)))
    => #t)

  (check	;correct configuration values, with fields
      (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#((mutable a) (immutable b)))))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd))
	     (get-a	(record-accessor rtd 0))
	     (get-b	(record-accessor rtd 1)))
	(let ((R (maker 1 2)))
	  (list (pred R)
		(get-a R)
		(get-b R))))
    => '(#t 1 2))

;;; --------------------------------------------------------------------
;;; with parent

  (check	;correct configuration values, no fields
      (let* ((prtd	(make-record-type-descriptor
			 (name: 'parent) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
	     (rtd	(make-record-type-descriptor
			 (name: 'name) (parent: prtd) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
	     (prcd	(make-record-constructor-descriptor
			 (rtd: prtd) (parent-rcd: #f) (protocol: #f)))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: prcd) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd)))
	(pred (maker)))
    => #t)

  (check	;correct configuration values, with child fields
      (let* ((prtd	(make-record-type-descriptor
			 (name: 'parent) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
	     (rtd	(make-record-type-descriptor
			 (name: 'name) (parent: prtd) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#((mutable a) (immutable b)))))
	     (prcd	(make-record-constructor-descriptor
			 (rtd: prtd) (parent-rcd: #f) (protocol: #f)))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: prcd) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd))
	     (get-a	(record-accessor    rtd 0))
	     (get-b	(record-accessor    rtd 1)))
	(let ((R (maker 1 2)))
	  (list (pred R)
		(get-a R)
		(get-b R))))
    => '(#t 1 2))

  (check	;correct configuration values, with parent fields
      (let* ((prtd	(make-record-type-descriptor
			 (name: 'parent) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#((mutable a) (immutable b)))))
	     (rtd	(make-record-type-descriptor
			 (name: 'name) (parent: prtd) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#())))
	     (prcd	(make-record-constructor-descriptor
			 (rtd: prtd) (parent-rcd: #f) (protocol: #f)))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: prcd) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd))
	     ;;parent accessors
	     (get-a	(record-accessor    prtd 0))
	     (get-b	(record-accessor    prtd 1)))
	(let ((R (maker 1 2)))
	  (list (pred R)
		(get-a R)
		(get-b R))))
    => '(#t 1 2))

  (check	;correct configuration values, with parent fields
      (let* ((prtd	(make-record-type-descriptor
			 (name: 'parent) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#())))
	     (rtd	(make-record-type-descriptor
			 (name: 'name) (parent: prtd) (uid: #f)
			 (sealed: #f) (opaque: #f)
			 (fields: '#((mutable a) (immutable b)))))
	     (prcd	(make-record-constructor-descriptor
			 (rtd: prtd) (parent-rcd: #f) (protocol: #f)))
	     (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: prcd) (protocol: #f)))
	     (maker	(record-constructor rcd))
	     (pred	(record-predicate   rtd))
	     ;;child accessors
	     (get-a	(record-accessor    rtd 0))
	     (get-b	(record-accessor    rtd 1)))
	(let ((R (maker 1 2)))
	  (list (pred R)
		(get-a R)
		(get-b R))))
    => '(#t 1 2))

;;; --------------------------------------------------------------------
;;; error, rtd argument

  (check	;not an rtd
      (catch #f
	(let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#()))))
	  (make-record-constructor-descriptor
	   (rtd: 123) (parent-rcd: #f) (protocol: #f))))
    => '(123))

;;; --------------------------------------------------------------------
;;; error, rcd argument

  (check	;not an rcd
      (catch #f
	(let* ((prtd	(make-record-type-descriptor
			 (name: 'parent) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
	       (rtd	(make-record-type-descriptor
			 (name: 'name) (parent: prtd) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#()))))
	  (make-record-constructor-descriptor
	   (rtd: rtd) (parent-rcd: 123) (protocol: #f))))
    => '(123))

  (let* ((rtd-0	(make-record-type-descriptor
		 (name: 'rtd-0) (parent: #f) (uid: #f)
		 (sealed: #f) (opaque: #f) (fields: '#())))
	 (rtd-1	(make-record-type-descriptor
		 (name: 'rtd-1) (parent: #f) (uid: #f)
		 (sealed: #f) (opaque: #f) (fields: '#())))
	 (rcd-0	(make-record-constructor-descriptor
		 (rtd: rtd-0) (parent-rcd: #f) (protocol: #f))))
    (check	;parent RCD for RTD without parent
	(catch #f
	  (make-record-constructor-descriptor
	   (rtd: rtd-1) (parent-rcd: rcd-0) (protocol: #f)))
      => (list rtd-1 rcd-0)))

  (let* ((rtd-0	(make-record-type-descriptor
		 (name: 'rtd-0) (parent: #f) (uid: #f)
		 (sealed: #f) (opaque: #f) (fields: '#())))
	 (rtd-1	(make-record-type-descriptor
		 (name: 'rtd-1) (parent: #f) (uid: #f)
		 (sealed: #f) (opaque: #f) (fields: '#())))
	 (rtd-2	(make-record-type-descriptor
		 (name: 'rtd-1) (parent: rtd-1) (uid: #f)
		 (sealed: #f) (opaque: #f) (fields: '#())))
	 (rcd-0	(make-record-constructor-descriptor
		 (rtd: rtd-0) (parent-rcd: #f) (protocol: #f))))
    (check	;parent RCD for wrong RTD
	(catch #f
	  (make-record-constructor-descriptor
	   (rtd: rtd-2) (parent-rcd: rcd-0) (protocol: #f)))
      => (list rtd-2 rcd-0)))

;;; --------------------------------------------------------------------
;;; error, protocol argument

  (check	;not a function
      (catch #f
	(let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#()))))
	  (make-record-constructor-descriptor
	   (rtd: rtd) (parent-rcd: #f) (protocol: 123))))
    => '(123))

  (let ((prot (lambda (a b)
		(list a b))))
    (check	;protocol accepting wrong num of args
	(catch #f
	  (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		 (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		 (maker	(record-constructor rcd)))
	    (maker)))
      => (list prot 1)))

  (let* ((maker	(lambda (a b)
		  (list a b)))
	 (prot	(lambda (make-top)
		  maker)))
    (check	;maker accepting wrong num of args
	(catch #f
	  (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		 (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		 (maker	(record-constructor rcd)))
	    (maker)))
      => (list maker 0)))

  (let ((prot (lambda (make-top)
		(add-result 1)
		(lambda ()
		  (make-top)))))
    (check	;protocol function called only once for every RECORD-CONSTRUCTOR
	(with-result
	 (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		(rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		(maker	(record-constructor rcd)))
	   (maker)
	   (maker)
	   #t))
      => '(#t (1))))

  (let* ((flag 0)
	 (prot (lambda (make-top)
		 (add-result flag)
		 (set! flag (fx+ 1 flag))
		 (lambda ()
		   (make-top)))))
    (check	;protocol function called only once for every RECORD-CONSTRUCTOR
	(with-result
	 (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		(rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		(maker1	(record-constructor rcd))
		(maker2	(record-constructor rcd)))
	   #t))
      => '(#t (0 1))))

  #;(let* ((maker	(lambda ()
		  (void)))
	 (prot	(lambda (make-top)
		  maker)))
    (check	;maker returning void
	(catch #t
	  (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		 (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		 (maker	(record-constructor rcd)))
	    (maker)))
      => (list maker 0)))

  #;(let* ((maker	(lambda ()
		  123))
	 (prot	(lambda (make-top)
		  maker)))
    (check	;maker returning non-record
	(catch #t
	  (let* ((rtd	(make-record-type-descriptor
			 (name: 'rtd-0) (parent: #f) (uid: #f)
			 (sealed: #f) (opaque: #f) (fields: '#())))
		 (rcd	(make-record-constructor-descriptor
			 (rtd: rtd) (parent-rcd: #f) (protocol: prot)))
		 (maker	(record-constructor rcd)))
	    (maker)))
      => (list maker 0)))


  #t)


(parametrise ((check-test-name	'misc))

  (let ()
    (define-record-type <alpha>
      (nongenerative ciao-hello-ciao-1)
      (fields a))

    (check
	(record-rtd (make-<alpha> 1))
      => (record-type-descriptor <alpha>))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
;;Local Variables:
;;eval: (put 'catch 'scheme-indent-function 1)
;;End:
