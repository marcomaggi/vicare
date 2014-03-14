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
    (vicare expander object-spec)
    (vicare expander tags))


;;;; helpers

(define (<top>? obj)
  #t)


(begin-for-syntax

  (set-identifier-object-spec! #'<top>
    (make-object-spec #'<top> #'<top>?))
  (set-identifier-object-spec! #'<boolean>
    (make-object-spec #'<boolean> #'boolean?))
  (set-identifier-object-spec! #'<char>
    (make-object-spec #'<char> #'char?))
  (set-identifier-object-spec! #'<symbol>
    (make-object-spec #'<symbol> #'symbol?))
  (set-identifier-object-spec! #'<keyword>
    (make-object-spec #'<keyword> #'keyword?))
  (set-identifier-object-spec! #'<pointer>
    (make-object-spec #'<pointer> #'pointer?))
  (set-identifier-object-spec! #'<transcoder>
    (make-object-spec #'<transcoder> #'transcoder?))
  (set-identifier-object-spec! #'<procedure>
    (make-object-spec #'<procedure> #'procedure?))

  (set-identifier-object-spec! #'<fixnum>
    (make-object-spec #'<fixnum> #'fixnum?))
  (set-identifier-object-spec! #'<flonum>
    (make-object-spec #'<flonum> #'flonum?))
  (set-identifier-object-spec! #'<ratnum>
    (make-object-spec #'<ratnum> #'ratnum?))
  (set-identifier-object-spec! #'<bignum>
    (make-object-spec #'<bignum> #'bignum?))
  (set-identifier-object-spec! #'<compnum>
    (make-object-spec #'<compnum> #'compnum?))
  (set-identifier-object-spec! #'<cflonum>
    (make-object-spec #'<cflonum> #'cflonum?))
  (set-identifier-object-spec! #'<integer>
    (make-object-spec #'<integer> #'integer?))
  (set-identifier-object-spec! #'<exact-integer>
    (make-object-spec #'<exact-integer> #'exact-integer?))
  (set-identifier-object-spec! #'<real>
    (make-object-spec #'<real> #'real?))
  (set-identifier-object-spec! #'<complex>
    (make-object-spec #'<complex> #'complex?))
  (set-identifier-object-spec! #'<number>
    (make-object-spec #'<number> #'number?))

  (set-identifier-object-spec! #'<string>
    (make-object-spec #'<string> #'string?))
  (set-identifier-object-spec! #'<vector>
    (make-object-spec #'<vector> #'vector?))
  (set-identifier-object-spec! #'<pair>
    (make-object-spec #'<pair> #'pair?
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
  (set-identifier-object-spec! #'<list>
    (make-object-spec #'<list> #'list?))
  (set-identifier-object-spec! #'<bytevector>
    (make-object-spec #'<bytevector> #'bytevector?))
  (set-identifier-object-spec! #'<hashtable>
    (make-object-spec #'<hashtable> #'hashtable?))
  (set-identifier-object-spec! #'<record>
    (make-object-spec #'<record> #'record?))
  (set-identifier-object-spec! #'<record-type-descriptor>
    (make-object-spec #'<record-type-descriptor> #'record-type-descriptor?))
  (set-identifier-object-spec! #'<struct>
    (make-object-spec #'<struct> #'<struct>?))
  (set-identifier-object-spec! #'<struct-type-descriptor>
    (make-object-spec #'<struct-type-descriptor> #'struct-type-descriptor?))
  (set-identifier-object-spec! #'<condition>
    (make-object-spec #'<condition> #'condition?))

  (set-identifier-object-spec! #'<port>
    (make-object-spec #'<port> #'port?))
  (set-identifier-object-spec! #'<input-port>
    (make-object-spec #'<input-port> #'input-port?))
  (set-identifier-object-spec! #'<output-port>
    (make-object-spec #'<output-port> #'output-port?))
  (set-identifier-object-spec! #'<input/output-port>
    (make-object-spec #'<input/output-port> #'input/output-port?))
  (set-identifier-object-spec! #'<textual-port>
    (make-object-spec #'<textual-port> #'textual-port?))
  (set-identifier-object-spec! #'<binary-port>
    (make-object-spec #'<binary-port> #'binary-port?))
  (set-identifier-object-spec! #'<textual-input-port>
    (make-object-spec #'<textual-input-port> #'textual-input-port?))
  (set-identifier-object-spec! #'<textual-output-port>
    (make-object-spec #'<textual-output-port> #'textual-output-port?))
  (set-identifier-object-spec! #'<textual-input/output-port>
    (make-object-spec #'<textual-input/output-port> #'textual-input/output-port?))
  (set-identifier-object-spec! #'<binary-input-port>
    (make-object-spec #'<binary-input-port> #'binary-input-port?))
  (set-identifier-object-spec! #'<binary-output-port>
    (make-object-spec #'<binary-output-port> #'binary-output-port?))
  (set-identifier-object-spec! #'<binary-input/output-port>
    (make-object-spec #'<binary-input/output-port> #'binary-input/output-port?))

  #| end of begin-for-syntax |# )


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'set-identifier-object-spec! 'scheme-indent-function 1)
;; End:
