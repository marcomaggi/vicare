;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: object specifications for built-in type tags
;;;Date: Fri Mar 14, 2014
;;;
;;;Abstract
;;;
;;;	This  library must  be  visited before  using  the built-in  tag
;;;	identifiers.
;;;
;;;Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare language-extensions tags)
  (options visit-upon-loading)
  (export
    <top>
    <boolean>
    <char>
    <symbol>
    <keyword>
    <pointer>
    <transcoder>
    <procedure>

    <fixnum>
    <flonum>
    <ratnum>
    <bignum>
    <compnum>
    <cflonum>
    <exact-integer>
    <integer-valued>
    <integer>
    <rational-valued>
    <rational>
    <real-valued>
    <real>
    <complex>
    <number>

    <string>
    <vector>
    <pair>
    <list>
    <bytevector>
    <hashtable>
    <record>
    <record-type-descriptor>
    <struct>
    <struct-type-descriptor>
    <condition>

    <port>
    <input-port>
    <output-port>
    <input/output-port>
    <textual-port>
    <binary-port>
    <textual-input-port>
    <textual-output-port>
    <textual-input/output-port>
    <binary-input-port>
    <binary-output-port>
    <binary-input/output-port>
    )
  (import (vicare)
    (vicare expander object-type-specs)
    (vicare expander tags))


;;;; helpers

(define (<top>? obj)
  #t)


(define-syntax init
  ;;Why  are  we   using  this  DEFINE-SYNTAX  rather   than  a  simpler
  ;;BEGIN-FOR-SYNTAX?   Because  the  code in  BEGIN-FOR-SYNTAX  is  not
  ;;included in the  visit code of the library, so  it is only evaluated
  ;;when the library is loaded from  source; to have this code evaluated
  ;;when the library is loaded in binary form we have to do it this way.
  (begin
    (set-identifier-object-type-spec! #'<top>
      (make-object-type-spec #'<top> #'<top>?))
    (set-identifier-object-type-spec! #'<boolean>
      (make-object-type-spec #'<boolean> #'boolean? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<char>
      (make-object-type-spec #'<char> #'char? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<symbol>
      (make-object-type-spec #'<symbol> #'symbol? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<keyword>
      (make-object-type-spec #'<keyword> #'keyword? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<pointer>
      (make-object-type-spec #'<pointer> #'pointer? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<transcoder>
      (make-object-type-spec #'<transcoder> #'transcoder? #f #f #f #'<top>))
    (set-identifier-object-type-spec! #'<procedure>
      (make-object-type-spec #'<procedure> #'procedure? #f #f #f #'<top>))

;;; --------------------------------------------------------------------
;;; numbers

    (set-identifier-object-type-spec! #'<number>
      (make-object-type-spec #'<number> #'number? #f #f #f #'<number>))

    (set-identifier-object-type-spec! #'<complex>
      (make-object-type-spec #'<complex> #'complex? #f #f #f #'<number>))

    (set-identifier-object-type-spec! #'<real-valued>
      (make-object-type-spec #'<real-valued> #'real-valued? #f #f #f #'<complex>))

    (set-identifier-object-type-spec! #'<real>
      (make-object-type-spec #'<real> #'real? #f #f #f #'<real-valued>))

    (set-identifier-object-type-spec! #'<rational-valued>
      (make-object-type-spec #'<rational-valued> #'rational-valued? #f #f #f #'<real>))

    (set-identifier-object-type-spec! #'<rational>
      (make-object-type-spec #'<rational> #'rational? #f #f #f #'<rational-valued>))

    (set-identifier-object-type-spec! #'<integer-valued>
      (make-object-type-spec #'<integer-valued> #'integer-valued? #f #f #f #'<rational-valued>))

    (set-identifier-object-type-spec! #'<integer>
      (make-object-type-spec #'<integer> #'integer? #f #f #f #'<rational-valued>))

    (set-identifier-object-type-spec! #'<exact-integer>
      (make-object-type-spec #'<exact-integer> #'exact-integer? #f #f #f #'<integer>))

    (set-identifier-object-type-spec! #'<fixnum>
      (make-object-type-spec #'<fixnum> #'fixnum? #f #f #f #'<exact-integer>))

    (set-identifier-object-type-spec! #'<bignum>
      (make-object-type-spec #'<bignum> #'bignum? #f #f #f #'<exact-integer>))

    (set-identifier-object-type-spec! #'<flonum>
      (make-object-type-spec #'<flonum> #'flonum? #f #f #f #'<real>))

    (set-identifier-object-type-spec! #'<ratnum>
      (make-object-type-spec #'<ratnum> #'ratnum? #f #f #f #'<rational>))

    (set-identifier-object-type-spec! #'<compnum>
      (make-object-type-spec #'<compnum> #'compnum? #f #f #f #'<complex>))

    (set-identifier-object-type-spec! #'<cflonum>
      (make-object-type-spec #'<cflonum> #'cflonum? #f #f #f #'<complex>))

;;; --------------------------------------------------------------------
;;; compound objects

    (set-identifier-object-type-spec! #'<string>
      (make-object-type-spec #'<string> #'string?))
    (set-identifier-object-type-spec! #'<vector>
      (make-object-type-spec #'<vector> #'vector?))
    (set-identifier-object-type-spec! #'<pair>
      (make-object-type-spec #'<pair> #'pair?
			     (lambda (slot-name-id safe?)
			       (case-identifiers slot-name-id
				 ((car)	#'car)
				 ((cdr)	#'cdr)
				 (else
				  (syntax-violation '<pair> "unknown slot name" slot-name-id))))
			     (lambda (slot-name-id safe?)
			       (case-identifiers slot-name-id
				 ((car)	#'set-car!)
				 ((cdr)	#'set-cdr!)
				 (else
				  (syntax-violation '<pair> "unknown slot name" slot-name-id))))))
    (set-identifier-object-type-spec! #'<list>
      (make-object-type-spec #'<list> #'list?))
    (set-identifier-object-type-spec! #'<bytevector>
      (make-object-type-spec #'<bytevector> #'bytevector?))
    (set-identifier-object-type-spec! #'<hashtable>
      (make-object-type-spec #'<hashtable> #'hashtable?))
    (set-identifier-object-type-spec! #'<record>
      (make-object-type-spec #'<record> #'record?))
    (set-identifier-object-type-spec! #'<record-type-descriptor>
      (make-object-type-spec #'<record-type-descriptor> #'record-type-descriptor?))
    (set-identifier-object-type-spec! #'<struct>
      (make-object-type-spec #'<struct> #'<struct>?))
    (set-identifier-object-type-spec! #'<struct-type-descriptor>
      (make-object-type-spec #'<struct-type-descriptor> #'struct-type-descriptor?))
    (set-identifier-object-type-spec! #'<condition>
      (make-object-type-spec #'<condition> #'condition?))

;;; --------------------------------------------------------------------
;;; input/output ports

    (set-identifier-object-type-spec! #'<port>
      (make-object-type-spec #'<port> #'port?))
    (set-identifier-object-type-spec! #'<input-port>
      (make-object-type-spec #'<input-port> #'input-port?))
    (set-identifier-object-type-spec! #'<output-port>
      (make-object-type-spec #'<output-port> #'output-port?))
    (set-identifier-object-type-spec! #'<input/output-port>
      (make-object-type-spec #'<input/output-port> #'input/output-port?))
    (set-identifier-object-type-spec! #'<textual-port>
      (make-object-type-spec #'<textual-port> #'textual-port?))
    (set-identifier-object-type-spec! #'<binary-port>
      (make-object-type-spec #'<binary-port> #'binary-port?))
    (set-identifier-object-type-spec! #'<textual-input-port>
      (make-object-type-spec #'<textual-input-port> #'textual-input-port?))
    (set-identifier-object-type-spec! #'<textual-output-port>
      (make-object-type-spec #'<textual-output-port> #'textual-output-port?))
    (set-identifier-object-type-spec! #'<textual-input/output-port>
      (make-object-type-spec #'<textual-input/output-port> #'textual-input/output-port?))
    (set-identifier-object-type-spec! #'<binary-input-port>
      (make-object-type-spec #'<binary-input-port> #'binary-input-port?))
    (set-identifier-object-type-spec! #'<binary-output-port>
      (make-object-type-spec #'<binary-output-port> #'binary-output-port?))
    (set-identifier-object-type-spec! #'<binary-input/output-port>
      (make-object-type-spec #'<binary-input/output-port> #'binary-input/output-port?))

    (lambda (stx) #'(void))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'set-identifier-object-type-spec! 'scheme-indent-function 1)
;; End:
