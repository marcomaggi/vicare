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
    <integer>
    <exact-integer>
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
    (vicare expander type-spec)
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
    (set-identifier-type-spec! #'<top>
      (make-type-spec #'<top> #'<top>?))
    (set-identifier-type-spec! #'<boolean>
      (make-type-spec #'<boolean> #'boolean?))
    (set-identifier-type-spec! #'<char>
      (make-type-spec #'<char> #'char?))
    (set-identifier-type-spec! #'<symbol>
      (make-type-spec #'<symbol> #'symbol?))
    (set-identifier-type-spec! #'<keyword>
      (make-type-spec #'<keyword> #'keyword?))
    (set-identifier-type-spec! #'<pointer>
      (make-type-spec #'<pointer> #'pointer?))
    (set-identifier-type-spec! #'<transcoder>
      (make-type-spec #'<transcoder> #'transcoder?))
    (set-identifier-type-spec! #'<procedure>
      (make-type-spec #'<procedure> #'procedure?))

    (set-identifier-type-spec! #'<fixnum>
      (make-type-spec #'<fixnum> #'fixnum?))
    (set-identifier-type-spec! #'<flonum>
      (make-type-spec #'<flonum> #'flonum?))
    (set-identifier-type-spec! #'<ratnum>
      (make-type-spec #'<ratnum> #'ratnum?))
    (set-identifier-type-spec! #'<bignum>
      (make-type-spec #'<bignum> #'bignum?))
    (set-identifier-type-spec! #'<compnum>
      (make-type-spec #'<compnum> #'compnum?))
    (set-identifier-type-spec! #'<cflonum>
      (make-type-spec #'<cflonum> #'cflonum?))
    (set-identifier-type-spec! #'<integer>
      (make-type-spec #'<integer> #'integer?))
    (set-identifier-type-spec! #'<exact-integer>
      (make-type-spec #'<exact-integer> #'exact-integer?))
    (set-identifier-type-spec! #'<real>
      (make-type-spec #'<real> #'real?))
    (set-identifier-type-spec! #'<complex>
      (make-type-spec #'<complex> #'complex?))
    (set-identifier-type-spec! #'<number>
      (make-type-spec #'<number> #'number?))

    (set-identifier-type-spec! #'<string>
      (make-type-spec #'<string> #'string?))
    (set-identifier-type-spec! #'<vector>
      (make-type-spec #'<vector> #'vector?))
    (set-identifier-type-spec! #'<pair>
      (make-type-spec #'<pair> #'pair?
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
    (set-identifier-type-spec! #'<list>
      (make-type-spec #'<list> #'list?))
    (set-identifier-type-spec! #'<bytevector>
      (make-type-spec #'<bytevector> #'bytevector?))
    (set-identifier-type-spec! #'<hashtable>
      (make-type-spec #'<hashtable> #'hashtable?))
    (set-identifier-type-spec! #'<record>
      (make-type-spec #'<record> #'record?))
    (set-identifier-type-spec! #'<record-type-descriptor>
      (make-type-spec #'<record-type-descriptor> #'record-type-descriptor?))
    (set-identifier-type-spec! #'<struct>
      (make-type-spec #'<struct> #'<struct>?))
    (set-identifier-type-spec! #'<struct-type-descriptor>
      (make-type-spec #'<struct-type-descriptor> #'struct-type-descriptor?))
    (set-identifier-type-spec! #'<condition>
      (make-type-spec #'<condition> #'condition?))

    (set-identifier-type-spec! #'<port>
      (make-type-spec #'<port> #'port?))
    (set-identifier-type-spec! #'<input-port>
      (make-type-spec #'<input-port> #'input-port?))
    (set-identifier-type-spec! #'<output-port>
      (make-type-spec #'<output-port> #'output-port?))
    (set-identifier-type-spec! #'<input/output-port>
      (make-type-spec #'<input/output-port> #'input/output-port?))
    (set-identifier-type-spec! #'<textual-port>
      (make-type-spec #'<textual-port> #'textual-port?))
    (set-identifier-type-spec! #'<binary-port>
      (make-type-spec #'<binary-port> #'binary-port?))
    (set-identifier-type-spec! #'<textual-input-port>
      (make-type-spec #'<textual-input-port> #'textual-input-port?))
    (set-identifier-type-spec! #'<textual-output-port>
      (make-type-spec #'<textual-output-port> #'textual-output-port?))
    (set-identifier-type-spec! #'<textual-input/output-port>
      (make-type-spec #'<textual-input/output-port> #'textual-input/output-port?))
    (set-identifier-type-spec! #'<binary-input-port>
      (make-type-spec #'<binary-input-port> #'binary-input-port?))
    (set-identifier-type-spec! #'<binary-output-port>
      (make-type-spec #'<binary-output-port> #'binary-output-port?))
    (set-identifier-type-spec! #'<binary-input/output-port>
      (make-type-spec #'<binary-input/output-port> #'binary-input/output-port?))

    (lambda (stx) #'(void))))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'set-identifier-type-spec! 'scheme-indent-function 1)
;; End:
