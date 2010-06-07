;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2008,2009  Abdulaziz Ghuloum
;;; Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



(library (ikarus not-yet-implemented)
  (export
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port )

  (import (except (ikarus)
    bitwise-reverse-bit-field
    bitwise-rotate-bit-field
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port))

  (define-syntax not-yet
    (syntax-rules ()
      [(_ x* ...)
       (begin
         (define (bug op)
           (define-condition-type &url &condition
             make-url-condition
             url-condition?
            (url condition-url))
           (raise
             (condition
               (make-error)
               (make-who-condition 'ikarus)
               (make-message-condition "primitive not supported")
               (make-message-condition
                 "Please visit the Ikarus FAQs page for more information")
               (make-url-condition
                 "https://answers.launchpad.net/ikarus/+faqs")
               (make-irritants-condition (list op)))))
         (define (x* . args) (bug 'x*))
         ...)]))

  (not-yet
    ;;; should be implemented
    bitwise-rotate-bit-field bitwise-reverse-bit-field
    ;;; won't be implemented
    make-custom-binary-input/output-port
    make-custom-textual-input/output-port
    open-file-input/output-port
    ))


