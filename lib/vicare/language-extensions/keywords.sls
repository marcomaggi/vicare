;;;
;;;Part of: Vicare/Scheme
;;;Contents: keywords utilities
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;	This library is derived from old code in Nausicaa/Scheme.
;;;
;;;Copyright (c) 2009-2010, 2012-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare language-extensions keywords)
  (export
    ;; reexported from (vicare)
    symbol->keyword
    keyword->symbol
    keyword?
    keyword=?
    keyword-hash

    ;; conversion with strings
    keyword->string	string->keyword

    ;; function arguments
    let-keywords	let*-keywords
    letrec-keywords	letrec*-keywords
    with-argument	without-argument)
  (import (vicare)
    (vicare language-extensions syntaxes)
    (vicare arguments validation))


;;;; string utilities

(define (keyword->string key)
  (define who 'keyword->string)
  (with-arguments-validation (who)
      ((keyword	key))
    (symbol->string (keyword->symbol key))))

(define (string->keyword str)
  (define who 'string->keyword)
  (with-arguments-validation (who)
      ((string	str))
    (symbol->keyword (string->symbol str))))


(define-syntax let-keywords
  (syntax-rules ()
    ((_ ?input-arguments ?args-var ?allow-unknown (?option-spec ...) ?form0 ?form ...)
     (%let-parse-keywords ley-keywords let
			  ?input-arguments ?args-var ?allow-unknown (?option-spec ...)
			  ?form0 ?form ...))))

(define-syntax let*-keywords
  (syntax-rules ()
    ((_ ?input-arguments ?args-var ?allow-unknown (?option-spec ...) ?form0 ?form ...)
     (%let-parse-keywords let*-keywords let*
			  ?input-arguments ?args-var ?allow-unknown (?option-spec ...)
			  ?form0 ?form ...))))

(define-syntax letrec-keywords
  (syntax-rules ()
    ((_ ?input-arguments ?args-var ?allow-unknown (?option-spec ...) ?form0 ?form ...)
     (%let-parse-keywords letrec-keywords letrec
			  ?input-arguments ?args-var ?allow-unknown (?option-spec ...)
			  ?form0 ?form ...))))

(define-syntax letrec*-keywords
  (syntax-rules ()
    ((_ ?input-arguments ?args-var ?allow-unknown (?option-spec ...) ?form0 ?form ...)
     (%let-parse-keywords letrec*-keywords letrec*
			  ?input-arguments ?args-var ?allow-unknown (?option-spec ...)
			  ?form0 ?form ...))))

(define-auxiliary-syntaxes
  with-argument without-argument)

(define-syntax %let-parse-keywords
  (lambda (stx)
    (define who '%let-parse-keywords)
    (define-struct opts
      (names defs keys))
    (define (%parse-options-spec specs names defs keys)
      (syntax-case specs (with-argument without-argument)
	(()
	 (values names defs keys))
	(((with-argument ?name ?default ?keyword) . ?specs)
	 (and (identifier? #'?name)
	      (keyword? (syntax->datum #'?keyword)))
	 (%parse-options-spec #'?specs
			      (cons #'?name	names)
			      (cons #'?default	defs)
			      ;;The UNQUOTE  in this form  is matched by
			      ;;the QUASIQUOTE in the output form.
			      #`((?keyword #f . (unquote (lambda (v) (set! ?name v)))) . #,keys)))
	(((without-argument ?name ?default ?keyword ?when-given) . ?specs)
	 (and (identifier? #'?name)
	      (keyword? (syntax->datum #'?keyword)))
	 (%parse-options-spec #'?specs
			      (cons #'?name	names)
			      (cons #'?default	defs)
			      ;;The UNQUOTE in  this form are matched by
			      ;;the QUASIQUOTE in the output form.
			      #`((?keyword (unquote (lambda () (set! ?name ?when-given)))
					   . (unquote (lambda (v) (set! ?name v))))
				 . #,keys)))
	((?wrong-spec . ?other-specs)
	 (syntax-violation who
	   "invalid option specification for keyword arguments parser"
	   (syntax->datum stx)
	   (syntax->datum #'?wrong-spec)))))
    (syntax-case stx ()
      ((_ ?who ?let ?input-arguments ?args-var ?allow-unknown (?option-spec ...) ?form0 ?form ...)
       (and (identifier? #'?who)
	    (identifier? #'?let)
	    (identifier? #'?args-var))
       (let-values (((names defs keys)
		     (%parse-options-spec #'(?option-spec ...) '() '() '())))
	 (with-syntax (((NAME ...)	(reverse names))
		       ((DEFAULT ...)	(reverse defs))
		       (KEYWORDS-ALIST	keys))
	   #'(?let ((NAME DEFAULT) ...)
		   (let ((?args-var (%parse-keywords '?who ?input-arguments ?allow-unknown
						     (quasiquote KEYWORDS-ALIST))))
		     ?form0 ?form ...))))))))

(define (%parse-keywords who input-arguments allow-unknown options-specs)
  (let next-input-argument ((arguments input-arguments))
    (if (null? arguments)
	'()
      (let ((arg (car arguments)))
	(cond ((assp (lambda (key)
		       (keyword=? arg key))
		 options-specs)
	       => (lambda (spec)
		    (let ((without-arg-thunk	(cadr spec))
			  (with-arg-closure	(cddr spec)))
		      (if without-arg-thunk
			  ;;Keyword option without argument.
			  (begin
			    (without-arg-thunk)
			    (next-input-argument (cdr arguments)))
			;;Keyword option with argument.
			(let ((arguments (cdr arguments)))
			  (cond ((null? arguments)
				 (assertion-violation who
				   "keyword option requires argument" arg input-arguments))
				((keyword? (car arguments))
				 (assertion-violation who
				   "value for keyword with argument cannot be a keyword"
				   arg (car arguments) input-arguments))
				(else
				  (with-arg-closure (car arguments))
				  (next-input-argument (cdr arguments)))))))))

	      ;;Input option is an unknown keyword.
	      ((keyword? arg)
	       (if allow-unknown
		   (cons arg (next-input-argument (cdr arguments)))
		 (assertion-violation who "unknown keyword argument" arg input-arguments)))

	      ;;Input option is a value.
	      ((not (keyword? arg))
	       (cons arg (next-input-argument (cdr arguments)))))))))


;;;; done

)

;;; end of file
