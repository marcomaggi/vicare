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
(library (vicare language-extensions infix)
  (export infix incr! decr! xor ? :
	  (rename (mod					%)
		  (and					&&)
		  (or					!!)
		  (xor					^^)
		  (not					~~)
		  (incr!				++)
		  (decr!				--))
	  (rename (bitwise-and				&)
		  (bitwise-ior				!)
		  (bitwise-xor				^)
		  (bitwise-not				~)
		  (bitwise-arithmetic-shift-left	<<)
		  (bitwise-arithmetic-shift-right	>>))
	  (rename (fxand				fx&)
		  (fxior				fx!)
		  (fxxor				fx^)
		  (fxnot				fx~)
		  (fxarithmetic-shift-left		fx<<)
		  (fxarithmetic-shift-right		fx>>)))
  (import (vicare)
    (vicare language-extensions infix parser-utils)
    (vicare language-extensions infix auxiliary-syntaxes)
    (vicare language-extensions increments))
  (define-syntax infix (make-infix-transformer)))

;;; end of file
