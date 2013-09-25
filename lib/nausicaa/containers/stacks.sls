;;;
;;;Part of: Vicare Scheme
;;;Contents: stack class definition
;;;Date: Wed Oct 14, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa containers stacks)
  (export
    <stack>
    (rename (stack.vicare-arguments-validation
	     <stack>.vicare-arguments-validation)
	    (stack/false.vicare-arguments-validation
	     <stack>/false.vicare-arguments-validation))
    list->stack
    vector->stack)
  (import (nausicaa)
    (vicare containers stacks))
  (define-label <stack>
    (protocol (lambda () make-stack))
    (predicate stack?)
    (virtual-fields
     (immutable (empty?		<boolean>)	stack-empty?)
     (immutable ($empty?	<boolean>)	$stack-empty?)
     (immutable (not-empty?	<boolean>)	stack-not-empty?)
     (immutable ($not-empty?	<boolean>)	$stack-not-empty?)
     (immutable (size		<integer>)	stack-size)
     (immutable ($size		<integer>)	$stack-size))
    (methods
     (hash		stack-hash)
     ($hash		$stack-hash)

     (putprop		stack-putprop)
     ($putprop		$stack-putprop)
     (getprop		stack-getprop)
     ($getprop		$stack-getprop)
     (remprop		stack-remprop)
     ($remprop		$stack-remprop)
     (property-list	stack-property-list)
     ($property-list	$stack-property-list)

     (top		stack-top)
     ($top		$stack-top)
     (push!		stack-push!)
     ($push!		$stack-push!)
     (pop!		stack-pop!)
     ($pop!		$stack-pop!)
     (purge!		stack-purge!)
     ($purge!		$stack-purge!)

     (list		stack->list)
     ($list		$stack->list)
     (vector		stack->vector)
     ($vector		$stack->vector))
    #| end of label |# )
  #| end of library |# )

;;; end of file
