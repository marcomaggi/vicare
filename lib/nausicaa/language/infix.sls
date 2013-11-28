;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare/Scheme
;;;Contents: implementation of the INFIX syntax
;;;Date: Tue May 18, 2010
;;;
;;;Abstract
;;;
;;;	The parser  table and the  general concept  of the package  is a
;;;	rework of Guile-Arith  by Ian Grant.  The parser  driver is from
;;;	the Lalr-scm package  by Dominique Boucher; the  parser table is
;;;	also generated using Lalr-scm.
;;;
;;;	  The parser table  is build in the  Nausicaa/Scheme package and
;;;	copied here, verbatim, from the file:
;;;
;;;		nausicaa/language/infix/sexp-parser.sls
;;;
;;;Copyright (c) 2010, 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Copyright (C) 2000 The Free Software Foundation
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
(library (nausicaa language infix)
  (export
    infix incr! decr! xor ? :
    % && !! ^^ ~~
    (rename (incr!	++)
	    (decr!	--))
    & ! ^ ~ << >>
    fx& fx! fx^ fx~ fx<< fx>>)
  (import (vicare)
    (except (vicare language-extensions infix)
	    infix incr! decr!)
    (vicare language-extensions infix parser-utils)
    (vicare language-extensions infix tokens)
    (for (nausicaa language increments)
      run expand)
    (for (only (nausicaa language oopp)
	       begin/tags)
      run expand))


(define-syntax infix
  (let ()

    (define-syntax case-stx
      (syntax-rules (else)
	((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
	 (cond ((memv-stx ?atom (syntax ?s) ...) ?e ...) ... (else ?b ...)))))

    (define-syntax memv-stx
      (syntax-rules ()
	((_ ?atom ?stx)
	 (free-identifier=? ?atom ?stx))
	((_ ?atom ?stx ...)
	 (or (free-identifier=? ?atom ?stx) ...))))

    (define tok.incr!	(make-<lexical-token> 'INCR	(cons #'pre-incr! #'post-incr!)))
    (define tok.decr!	(make-<lexical-token> 'DECR	(cons #'pre-decr! #'post-decr!)))

    (define (atom->token atom kont)
      (case-stx atom
	((incr!)	tok.incr!)
	((decr!)	tok.decr!)
	(else		(kont atom))))

    (make-infix-transformer atom->token #'begin/tags)))


;;;; done

)

;;; end of file
;; Local Variables:
;; eval: (put 'case-stx 'scheme-indent-function 1)
;; End:
