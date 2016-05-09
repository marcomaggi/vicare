;;;Copyright (c) 2015, 2016 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!vicare
(library (psyntax.config)
  (export
    ;; initialisation
    initialise-expander
    expander-initialisation/initialise-label-gensyms-and-interned-libraries

    ;; options
    expander-language
    typed-language-enabled?
    strict-r6rs-enabled?
    enable-all-warnings
    disable-all-warnings
    warn-about-logic-constants
    warn-about-not-returning-expressions
    warn-about-compatible-operands-signature-in-procedure-application

    #| end of EXPORT |# )
  (import (rnrs)
    (psyntax.compat))


;;;; helpers

(define-syntax define-parameter-boolean-option
  (syntax-rules ()
    ((_ ?who)
     (define-parameter-boolean-option ?who #f))
    ((_ ?who ?default)
     (define ?who
       (make-parameter ?default
	 (lambda (value)
	   (and value #t)))))
    ))


;;;; initialiseation

(define expander-initialisation/initialise-label-gensyms-and-interned-libraries
  (make-parameter #f))

(define initialise-expander
  (let ((expander-initialised? #f))
    (lambda ()
      (unless expander-initialised?
	(print-expander-debug-message "initialising expander internals")
	(let ((func (expander-initialisation/initialise-label-gensyms-and-interned-libraries)))
	  (when func (func)))
	(set! expander-initialised? #t)))))


;;;; options: behaviour

(define expander-language
  (make-parameter 'default
    (lambda (obj)
      (case obj
	((strict-r6rs typed default) obj)
	(else
	 (assertion-violation 'expander-language
	   "wrong parameter value, expected one among: strict-r6rs, typed, default"
	   obj))))))

(case-define typed-language-enabled?
  (()
   (eq? 'typed (expander-language)))
  ((obj)
   (expander-language (if obj 'typed 'default))
   (eq? 'typed (expander-language))))

(case-define strict-r6rs-enabled?
  (()
   (eq? 'strict-r6rs (expander-language)))
  ((obj)
   (expander-language (if obj 'strict-r6rs 'default))
   (eq? 'strict-r6rs (expander-language))))


;;;; options: warnings

(define warn-about-logic-constants
  ;;When set to true:  raise a "&warning" when an operand in  a logic expression (if,
  ;;and, or, ...) always returns true or always returns false; otherwise do nothing.
  ;;
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define warn-about-not-returning-expressions
  ;;When set to true: raise a "&warning"  when an expression evaluated for its return
  ;;values is typed as not returning; otherwise do nothing.
  ;;
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

(define warn-about-compatible-operands-signature-in-procedure-application
  ;;When set to true: raise a "&warning" when the type signature of the operands in a
  ;;procedure application does not match the expected signature of the arguments, but
  ;;it is compatible so the code compiles; otherwise do nothing.
  ;;
  (make-parameter #f
    (lambda (obj)
      (if obj #t #f))))

;;; --------------------------------------------------------------------

(define (enable-all-warnings)
  (warn-about-logic-constants #t)
  (warn-about-not-returning-expressions #t)
  (warn-about-compatible-operands-signature-in-procedure-application #t))

(define (disable-all-warnings)
  (warn-about-logic-constants #f)
  (warn-about-not-returning-expressions #f)
  (warn-about-compatible-operands-signature-in-procedure-application #f))


;;;; done

#| end of library |# )

;;; end of file
