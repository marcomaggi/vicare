;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi, 2015.
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.


#!r6rs
(library (ikarus.pretty-formats)
  (export get-fmt pretty-format)
  (import (except (vicare) pretty-format))

  (define h (make-eq-hashtable))

  (define (get-fmt name)
    (hashtable-ref h name #f))

  (define (set-fmt! name fmt)
    (hashtable-set! h name fmt))

  (define pretty-format
    (lambda (x)
      (unless (symbol? x)
        (die 'pretty-format "not a symbol" x))
      (case-lambda
       [() (hashtable-ref h x #f)]
       [(v) (hashtable-set! h x v)])))


;;;; standard formats

(set-fmt! 'quote '(pretty-format-reader-macro . "'"))
(set-fmt! 'unquote '(pretty-format-reader-macro . ","))
(set-fmt! 'unquote-splicing '(pretty-format-reader-macro . ",@"))
(set-fmt! 'quasiquote '(pretty-format-reader-macro . "`"))
(set-fmt! 'syntax '(pretty-format-reader-macro . "#'"))
(set-fmt! 'quasisyntax '(pretty-format-reader-macro . "#`"))
(set-fmt! 'unsyntax '(pretty-format-reader-macro . "#,"))
(set-fmt! 'unsyntax-splicing '(pretty-format-reader-macro . "#,@"))
;;(set-fmt! '|#primitive| '(pretty-format-reader-macro . "#%"))
(set-fmt! 'let '(alt
		 (_ (0 [e 0 e] ...) tab e ...)
		 (_ x (0 [e 0 e] ...) tab e ...)))
(set-fmt! 'letrec '(_ (0 [e 0 e] ...) tab e ...))
(set-fmt! 'letrec* '(_ (0 [e 0 e] ...) tab e ...))
(set-fmt! 'let-syntax '(_ (0 [e 0 e] ...) tab e ...))
(set-fmt! 'letrec-syntax '(_ (0 [e 0 e] ...) tab e ...))
(set-fmt! 'let* '(_ (0 [e 0 e] ...) tab e ...))
(set-fmt! 'let-values '(_ (0 [e 0 e] ...) tab e tab e* ...))
(set-fmt! 'cond '(_ tab [0 e ...] ...))
(set-fmt! 'define '(_ name tab e ...))
(set-fmt! 'set! '(_ name tab e))
(set-fmt! 'case-lambda
	  '(_ tab [0 e ...] ...))
(set-fmt! 'struct-case
	  '(_ e tab [e 0 e ...] ...))
(set-fmt! 'if '(_ test 3 e ...))
(set-fmt! 'and '(and test 4 e ...))
(set-fmt! 'or '(or test 3 e ...))
(set-fmt! 'begin '(_ tab e ...))
(set-fmt! 'lambda '(_ fmls tab e tab e* ...))
(set-fmt! 'case '(_ e tab [e 0 e] ...))
(set-fmt! 'syntax-rules '(_ kwd* tab [e 0 e] ...))
(set-fmt! 'syntax-case '(_ expr kwd*
			   tab (e 0 e 0 e ...) ...))
(set-fmt! 'module '(alt (_ (fill ...) tab e ...)
			(_ name (fill ...) tab e ...)))
(set-fmt! 'library '(_ name tab e ...))
(set-fmt! 'import '(_ tab e ...))

(set-fmt! 'receive
	  '(_ (var ...)
	      tab expr
	      tab body0 body ...))
(set-fmt! 'receive-and-return
	  '(_ (var ...)
	      tab expr
	      tab body0 body ...))


;;;; done

#| end of library |# )

;;; end of file
