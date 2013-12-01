;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: helper library for infix syntax
;;;Date: Thu Nov 28, 2013
;;;
;;;Abstract
;;;
;;;	The parser  table and the  general concept  of the package  is a
;;;	rework of Guile-Arith  by Ian Grant.  The parser  driver is from
;;;	the Lalr-scm package  by Dominique Boucher; the  parser table is
;;;	also generated using Lalr-scm.
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
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare language-extensions infix tokens)
  (export
    make-<lexical-token>
    <lexical-token>?
    <lexical-token>-category
    <lexical-token>-value)
  (import (vicare))
  (define make-<lexical-token>		cons)
  (define <lexical-token>?		pair?)
  (define <lexical-token>-category	car)
  (define <lexical-token>-value		cdr))

;;; end of file
