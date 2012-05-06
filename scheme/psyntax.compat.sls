;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (psyntax compat)
  (export
    define-record			make-struct-type
    make-parameter			parametrise
    format				gensym
    eval-core
    symbol-value			set-symbol-value!
    keyword?				pretty-print

    ;; reading source code and interpreting the resule
    get-annotated-datum			read-library-source-file
    annotation?				annotation-expression
    annotation-stripped			annotation-source
    annotation-textual-position

    ;; source position condition objects
    make-source-position-condition	source-position-condition?
    source-position-byte		source-position-character
    source-position-line		source-position-column
    source-position-port-id

    label-binding			set-label-binding!
    remove-location

    ;; error handlers
    library-version-mismatch-warning
    library-stale-warning
    file-locator-resolution-error)
  (import (except (ikarus)
		  ;;This binding is in  the makefile and this EXCEPT and
		  ;;the  IMPORT below can  be removed  at the  next boot
		  ;;image rotation (Marco Maggi; Wed Mar 28, 2012).
		  annotation-textual-position)
    (only (ikarus.reader)
	  read-library-source-file ;this is not in makefile.sps
	  annotation-textual-position)
    (only (ikarus.compiler)
	  eval-core)
    (only (ikarus system $symbols)
	  $unintern-gensym))


(define (library-version-mismatch-warning name depname filename)
  (fprintf (current-error-port)
	   "*** Vicare warning: library ~s has an inconsistent dependency \
            on library ~s; file ~s will be recompiled from source.\n"
	   name depname filename))

(define (library-stale-warning name filename)
  (fprintf (current-error-port)
	   "*** Vicare warning: library ~s is stale; file ~s will be \
            recompiled from source.\n"
	   name filename))

(define (file-locator-resolution-error libname failed-list pending-list)
  (define-condition-type &library-resolution &condition
    make-library-resolution-condition
    library-resolution-condition?
    (library condition-library)
    (files condition-files))
  (define-condition-type &imported-from &condition
    make-imported-from-condition imported-from-condition?
    (importing-library importing-library))
  (raise
   (apply condition (make-error)
	  (make-who-condition 'expander)
	  (make-message-condition "cannot locate library in library-path")
	  (make-library-resolution-condition libname failed-list)
	  (map make-imported-from-condition pending-list))))

(define-syntax define-record
  (syntax-rules ()
    [(_ name (field* ...) printer)
     (begin
       (define-struct name (field* ...))
       (module ()
	 (set-rtd-printer! (type-descriptor name)
			   printer)))]
    [(_ name (field* ...))
     (define-struct name (field* ...))]))

(define (set-label-binding! label binding)
  (set-symbol-value! label binding))

(define (label-binding label)
  (and (symbol-bound? label) (symbol-value label)))

(define (remove-location x)
  ($unintern-gensym x))


;;;; done

)

;;; end of file
