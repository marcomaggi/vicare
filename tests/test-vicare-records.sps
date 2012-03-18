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
		(make-record-type-descriptor make-record-type-descriptor*))
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
  (syntax-rules (name parent uid sealed? opaque? fields)
    ((_ (name: ?name) (parent: ?parent) (uid: ?uid)
	(sealed: ?sealed) (opaque: ?opaque) (fields: ?fields))
     (make-record-type-descriptor* ?name ?parent ?uid ?sealed ?opaque ?fields))))


(parametrise ((check-test-name	'make-rtd))

  (define parent-0
    (make-record-type-descriptor (name: 'make-rtd-parent-0) (parent: #f)
				 (uid: 'make-rtd-parent-0) (opaque: #f)
				 (sealed: #f) (fields: '#())))

  (define parent-1
    (make-record-type-descriptor (name: 'make-rtd-parent-1) (parent: #f)
				 (uid: 'make-rtd-parent-1) (opaque: #f)
				 (sealed: #f) (fields: '#())))

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
    (check	;different opaque
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-7 #f #f '#((mutable a)))))
      => (list rtd (list 'N #f 'make-rtd-7 #f #f '#((mutable a))))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-8 #f #f '#((mutable a)))))
    (check	;different opaque
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-8 #f #f '#((immutable a)))))
      => (list rtd (list 'N #f 'make-rtd-8 #f #f '#((immutable a))))))

  (let ((rtd (make-record-type-descriptor* 'N #f 'make-rtd-9 #f #f '#((immutable a)))))
    (check	;different opaque
	(catch #f
;;;                                             name parent uid sealed? opaque? fields
	  (eq? rtd (make-record-type-descriptor* 'N #f 'make-rtd-9 #f #f '#((immutable b)))))
      => (list rtd (list 'N #f 'make-rtd-9 #f #f '#((immutable b))))))

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
