;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: syntax utilities
;;;Date: Sat Oct  5, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (vicare language-extensions identifier-substitutions)
  (export
    single-identifier-subst
    multi-identifier-subst)
  (import (vicare))


(define (single-identifier-subst src-id dst-stx stx)
  ;;Visit the syntax object STX  substituting all the occurrences of the
  ;;identifier SRC-ID with the syntax object DST-STX; return the result.
  ;;Identifiers are compared with FREE-IDENTIFIER=?.
  ;;
  ;;This  function  recognises  SYNTAX,  QUASISYNTAX,  UNSYNTAX,  QUOTE,
  ;;QUASIQUOTE and UNQUOTE syntaxes  in STX and processes them according
  ;;to the following rules:
  ;;
  ;;* QUOTE and SYNTAX forms outside QUASIQUOTE and QUASISYNTAX are left
  ;;alone: no substitution is performed.
  ;;
  ;;*  QUASIQUOTE  and QUASISYNTAX  are  visited  and substitutions  are
  ;;performed only in UNQUOTE and  UNSYNTAX subforms at the same nesting
  ;;level of the outer enclosing QUASIQUOTE and QUASISYNTAX form.
  ;;
  ;;*  QUOTE  forms  inside  SYNTAX  and  QUASISYNTAX  are  visited  and
  ;;substitutions performed.
  ;;
  ;;*  SYNTAX  forms  inside   QUOTE  and  QUASIQUOTE  are  visited  and
  ;;substitutions performed.
  ;;
  (define-inline (main)
    (assert (identifier? src-id))
    (if (and (identifier? dst-stx)
	     (free-identifier=? src-id dst-stx))
	stx
      (%subst stx src-id dst-stx)))

  (define (%subst stx src dst)
    (syntax-case stx (syntax quasisyntax quote quasiquote)
      (?id
       (identifier? #'?id)
       (let ((id #'?id))
	 (if (free-identifier=? src id) dst id)))

      ((syntax . ?stuff)
       stx)
      ((quasisyntax . ?stuff)
       #`(quasisyntax . #,(%subst-inside-quasisyntax 0 #'?stuff (lambda (stx)
								  (%subst stx src dst)))))

      ((quote . ?stuff)
       stx)
      ((quasiquote . ?stuff)
       #`(quasiquote . #,(%subst-inside-quasiquote 0 #'?stuff (lambda (stx)
								(%subst stx src dst)))))

      ((?car . ?cdr)
       (cons (%subst #'?car src dst)
	     (%subst #'?cdr src dst)))
      (#(?item ...)
       (list->vector (%subst #'(?item ...) src dst)))
      (_ stx)))

  (main))


(define (multi-identifier-subst ids-map stx)
  ;;Substitute  occurrences of  identifiers  in STX,  return the  result
  ;;which can be STX itself.  STX must be a syntax object.  IDS-MAP must
  ;;be a  syntax object  holding a possibly  empty lists of  lists, each
  ;;holding two identifiers:
  ;;
  ;;   ((?SRC ?DST) ...)
  ;;
  ;;each  ?SRC  identifier  is  substituted  with ?DST.   The  rules  of
  ;;substitution are the same as the ones for SINGLE-IDENTIFIER-SUBST.
  ;;
  (define-inline (main)
    (let loop ((src	'())
	       (dst	'())
	       (ids-map	ids-map))
      (syntax-case ids-map ()
	(()
	 (%subst stx src dst))
	(((?src ?dst) . ?rest)
	 (identifier? #'?src)
	 (loop (cons #'?src src)
	       (cons #'?dst dst)
	       #'?rest))
	(_
	 (assertion-violation 'multi-identifier-subst "invalid identifiers map" ids-map)))))

  (define (%subst stx src dst)
    (syntax-case stx (syntax quasisyntax quote quasiquote)
      (?id
       (identifier? #'?id)
       (let ((id #'?id))
	 (let search ((src src)
		      (dst dst))
	   (cond ((null? src)
		  id)
		 ((free-identifier=? (car src) id)
		  (car dst))
		 (else
		  (search (cdr src) (cdr dst)))))))

      ((syntax . ?stuff)
       stx)
      ((quasisyntax . ?stuff)
       #`(quasisyntax . #,(%subst-inside-quasisyntax 0 #'?stuff (lambda (stx)
								  (%subst stx src dst)))))

      ((quote . ?stuff)
       stx)
      ((quasiquote . ?stuff)
       #`(quasiquote . #,(%subst-inside-quasiquote 0 #'?stuff (lambda (stx)
								(%subst stx src dst)))))

      ((?car . ?cdr)
       (cons (%subst #'?car src dst)
	     (%subst #'?cdr src dst)))
      (#(?item ...)
       (list->vector (%subst #'(?item ...) src dst)))
      (_ stx)))

  (main))


(define (%subst-inside-quasisyntax nesting-level stx process)
  ;;Subroutine for SINGLE-IDENTIFIER-SUBST and MULTI-IDENTIFIER-SUBST.
  ;;
  (define-inline (recurse ?stx)
    (%subst-inside-quasisyntax nesting-level ?stx process))
  (define-inline (recurse+1 ?stx)
    (%subst-inside-quasisyntax (fx+ nesting-level 1) ?stx process))
  (define-inline (recurse-1 ?stx)
    (%subst-inside-quasisyntax (fx- nesting-level 1) ?stx process))
  (syntax-case stx (unsyntax quasisyntax)
    ((unsyntax . ?stuff)
     ;;Notice the trick needed to compose an output form having UNSYNTAX
     ;;as first subform; doing:
     ;;
     ;;   #`(unsyntax ---)
     ;;
     ;;does not work; doing:
     ;;
     ;;   #`(#,#'unsyntax ---)
     ;;
     ;;works.
     ;;
     #`(#,#'unsyntax . #,(if (fxzero? nesting-level)
			     (process #'?stuff)
			   (recurse-1 #'?stuff))))
    ((quasisyntax . ?stuff)
     #`(quasisyntax . #,(recurse+1 #'?stuff)))
    ((?car . ?cdr)
     (cons (recurse #'?car) (recurse #'?cdr)))
    (#(?item ...)
     (list->vector (recurse #'(?item ...))))
    (_ stx)))


(define (%subst-inside-quasiquote nesting-level stx process)
  ;;Subroutine for SINGLE-IDENTIFIER-SUBST and MULTI-IDENTIFIER-SUBST.
  ;;
  (define-inline (recurse ?stx)
    (%subst-inside-quasiquote nesting-level ?stx process))
  (define-inline (recurse+1 ?stx)
    (%subst-inside-quasiquote (fx+ nesting-level 1) ?stx process))
  (define-inline (recurse-1 ?stx)
    (%subst-inside-quasiquote (fx- nesting-level 1) ?stx process))
  (syntax-case stx (unquote quasiquote)
    ((unquote . ?stuff)
     #`(unquote . #,(if (fxzero? nesting-level)
			(process #'?stuff)
		      (recurse-1 #'?stuff))))
    ((quasiquote . ?stuff)
     #`(quasiquote . #,(recurse+1 #'?stuff)))
    ((?car . ?cdr)
     (cons (recurse #'?car) (recurse #'?cdr)))
    (#(?item ...)
     (list->vector (recurse #'(?item ...))))
    (_ stx)))


;;;; done

)

;;; end of file
