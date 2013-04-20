;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for the AMB operator
;;;Date: Thu Apr 18, 2013
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
(import (vicare)
  (vicare language-extensions amb)
  (only (vicare language-extensions syntaxes)
	define-values)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare: amb operator\n")


(parametrise ((check-test-name	'core))

  (check
      (with-ambiguous-choices
       (amb (amb) 1))
    => 1)

  (check
      (with-ambiguous-choices
       (amb 1 (amb)))
    => 1)

  (check	;successfully find value
      (with-result
       (with-ambiguous-choices
	(let ((N (amb 1 2 3 4)))
	  (add-result N)
	  (unless (< 2 N)
	    (amb))
	  (unless (even? N)
	    (amb))
	  N)))
    => '(4 (1 2 3 4)))

  (check	;successfully find value, test AMB-ASSERT
      (with-result
       (with-ambiguous-choices
	(let ((N (amb 1 2 3 4)))
	  (add-result N)
	  (amb-assert (< 2 N))
	  (amb-assert (even? N))
	  N)))
    => '(4 (1 2 3 4)))

  (check	;successfully find sequence
      (with-result
       (with-ambiguous-choices
	(guard (E ((amb-exhaustion? E)
		   #t)
		  (else
		   #f))
	  (let ((N (amb 1 2 3 4 5 6 7 8 9 10)))
	    (amb-assert (odd? N))
	    (add-result N)
	    (amb)))))
    => '(#t (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check	;seed, square, cube
      (with-ambiguous-choices
       (let* ((A (amb 1 2 3))
	      (B (amb 5 9 11)))
	 (amb-assert (= (square A) B))
	 (let ((C (amb 21 24 27 33)))
	   (amb-assert (= (cube A) C))
	   (list A B C))))
    => '(3 9 27))

  (check 	;seed, square, cube
      (let ()
	(define (print . args)
	  (when #f
	    (apply fprintf (current-error-port) args)))
	(with-ambiguous-choices
	 (let ((A (amb 1 3)))
	   (print "A=~a\n" A)
	   (let ((B (amb 5 9 11)))
	     (print "\tB=~a\n" B)
	     (amb-assert (= (square A) B))
	     (let ((C (amb 13 27 31)))
	       (print "\t\tC=~a\n" C)
	       (amb-assert (= (cube A) C))
	       (list A B C))))))
    => '(3 9 27))

  #t)


(parametrise ((check-test-name	'handler))

  (check
      (call/cc
	  (lambda (escape)
	    (with-ambiguous-choices
	     (with-amb-exhaustion-handler
		 (lambda ()
		   (escape #t))
	       (lambda ()
		 (amb)
		 #f)))))
    => #t)

  #t)


(parametrise ((check-test-name	'permute))

  (check
      (with-ambiguous-choices
       (amb-permute (amb-permute) 1))
    => 1)

  (check
      (with-ambiguous-choices
       (amb-permute 1 (amb-permute)))
    => 1)

  (check
      (with-ambiguous-choices
       (let ((N (amb-permute 1 2 3 4)))
	 (unless (< 2 N)
	   (amb))
	 (unless (even? N)
	   (amb))
	 N))
    => 4)

  (check
      (with-ambiguous-choices
       (let ((N (amb-permute 1 2 3 4)))
	 (amb-assert (< 2 N))
	 (amb-assert (even? N))
	 N))
    => 4)

  (check
      (with-ambiguous-choices
       (parametrise ((amb-random-fixnum-maker random))
	 (let ((N (amb-permute 1 2 3 4)))
	   (amb-assert (< 2 N))
	   (amb-assert (even? N))
	   N)))
    => 4)

  #t)


(parametrise ((check-test-name	'random))

  (check
      (with-ambiguous-choices
       (let ((R (amb-random 1 2)))
	 (amb-assert (even? R))
	 R))
    => 2)

  (check
      (let ((counter 10))
	(with-ambiguous-choices
	 (let ((R (amb-random 1 3 5 7)))
	   (if (zero? counter)
	       #t
	     (begin
	       (set! counter (+ -1 counter))
	       (amb-assert (even? R))
	       R)))))
    => #t)

  #t)


(parametrise ((check-test-name	'generator))

  (check	;success
      (let ()
	(define-values (empty? enqueue! dequeue!)
	  (make-queue '(1 2 3 4)))

	(define (generator)
	  (if (empty?)
	      (amb)
	    (dequeue!)))

	(with-ambiguous-choices
	 (let ((R (amb-thunk generator)))
	   (amb-assert (<= 3 R))
	   R)))
    => 3)

  (check	;failure
      (let ()
	(define-values (empty? enqueue! dequeue!)
	  (make-queue '(1 2 3 4)))

	(define (generator)
	  (if (empty?)
	      (amb)
	    (dequeue!)))

	(call/cc
	    (lambda (escape)
	      (with-ambiguous-choices
	       (with-amb-exhaustion-handler
		   (lambda ()
		     (escape #t))
		 (lambda ()
		   (let ((R (amb-thunk generator)))
;;;(check-pretty-print R)
		     (amb-assert (< 10 R))
		     R)))))))
    => #t)

  #t)


(parametrise ((check-test-name	'failures))

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else #f))
	(eval '(amb (amb) 1)
	      (environment '(vicare language-extensions amb))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (E ((error? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #f)

  (check
      (guard (E ((amb-exhaustion? E)
		 #t)
		(else #f))
	(raise (make-amb-exhaustion)))
    => #t)

;;; --------------------------------------------------------------------

  (check	;AMB failure
      (with-result
       (with-ambiguous-choices
	(guard (E ((amb-exhaustion? E)
		   #t)
		  (else
		   #f))
	  (let ((N (amb 1 3 5 7)))
	    (add-result N)
	    (amb-assert (even? N))
	    N))))
    => '(#t (1 3 5 7)))

  (check	;AMB-PERMUTE failure
      (with-ambiguous-choices
       (guard (E ((amb-exhaustion? E)
		  #t)
		 (else
		  #f))
	 (let ((N (amb-permute 1 3 5 7)))
	   (add-result N)
	   (amb-assert (even? N))
	   N)))
    => #t)

  #t)


(parametrise ((check-test-name	'coloring))

  (check
      (let ()
	(define-record-type node
	  (fields (immutable name)
		  ;;List of NODE records representing the adjacency list
		  ;;of this node.
		  (mutable neighbors)
		  ;;Symbol representing this node's color.
		  (mutable color))
	  (protocol (lambda (maker)
		      (lambda (name)
			(maker name '() #f)))))

	(define-syntax define-nodes
	  (syntax-rules ()
	    ((_ ?nodes-var (?node (?neighbor ...)) ...)
	     (begin
	       (define ?node (make-node (quote ?node)))
	       ...
	       (module ()
		 (node-neighbors-set! ?node (list ?neighbor ...))
		 ...)
	       (define ?nodes-var
		 (list ?node ...))
	       (module ()
		 (assert-graph-consistency ?nodes-var))))
	    ))

	(define (assert-graph-consistency nodes)
	  ;;Verify that every  node is present in the  adjacency list of
	  ;;all its neighbors.
	  ;;
	  (define who 'assert-graph-consistency)
	  (for-each
	      (lambda (node)
		(for-each
		    (lambda (neighbor)
		      (unless (memq node (node-neighbors neighbor))
			(assertion-violation who
			  "incorrect node links"
			  (node-name node)
			  (node-name neighbor))))
		  (node-neighbors node)))
	    nodes))

	;;We are interested  in nations that face each  other, even when
	;;there is a sea between them.
	;;
	(define-nodes europe-facing-nations
	  (portugal		(spain))
	  (spain		(portugal andorra france))
	  (andorra		(spain france))
	  (france		(spain andorra monaco italy switzerland
				       germany luxembourg belgium
				       united-kingdom))
	  (united-kingdom	(france belgium netherlands denmark norway
					iceland ireland))
	  (ireland		(united-kingdom iceland))
	  (monaco		(france))
	  (italy		(france greece albania montenegro croatia slovenia
					austria switzerland san-marino))
	  (san-marino		(italy))
	  (switzerland		(france italy austria germany liechtenstein))
	  (liechtenstein	(switzerland austria))
	  (germany		(france switzerland austria czech-republic
					poland sweden denmark netherlands
					belgium luxembourg))
	  (belgium		(france luxembourg germany netherlands
					united-kingdom))
	  (netherlands		(belgium germany united-kingdom))
	  (luxembourg		(france germany belgium))
	  (austria		(italy slovenia hungary slovakia czech-republic
				       germany switzerland liechtenstein))
	  (slovenia		(italy croatia hungary austria))
	  (croatia		(italy montenegro bosnia serbia hungary
				       slovenia))
	  (bosnia		(croatia montenegro serbia))
	  (montenegro		(croatia italy albania serbia bosnia))
	  (albania		(italy greece macedonia serbia montenegro))
	  (greece		(italy cyprus bulgaria macedonia albania))
	  (cyprus		(greece))
	  (macedonia		(albania greece bulgaria serbia))
	  (bulgaria		(macedonia greece romania serbia))
	  (serbia		(montenegro albania macedonia bulgaria
					    romania hungary croatia bosnia))
	  (romania		(serbia bulgaria hungary))
	  (hungary		(slovenia croatia serbia romania slovakia
					  austria))
	  (slovakia		(austria hungary poland czech-republic))
	  (czech-republic	(germany austria slovakia poland))
	  (poland		(germany czech-republic slovakia sweden))
	  (denmark		(united-kingdom germany sweden norway))
	  (sweden		(norway denmark germany poland finland))
	  (norway		(united-kingdom denmark sweden finland iceland))
	  (finland		(sweden norway))
	  (iceland		(ireland united-kingdom norway)))

	(define (choose-color)
	  ;;Every time we call this function: we start a new choice.
	  ;;
	  (amb-permute 'red 'yellow 'blue 'green))

	(define (validate-single-node-color node)
	  ;;Test the color of NODE  against the colors of its neighbors:
	  ;;NODE  must have  color  different from  its neighbors.   The
	  ;;neighbors may not have a color yet: their COLOR field can be
	  ;;set  to  #f.  This  is  used  to  both validate  a  possible
	  ;;solution and build a "better" first choice.
	  ;;
	  (amb-assert (not (memq (node-color node)
				 (map node-color
				   (node-neighbors node))))))

	(define (validate-all-nodes-color all-nodes)
	  ;;Validate  a possible  solution: every  node must  have color
	  ;;different from its neighbors.
	  ;;
	  (for-all validate-single-node-color all-nodes))

	(define (color-nations nations)
	  (with-ambiguous-choices
	   ;;Build an initial choice.
	   (for-each
	       (lambda (nation)
		 (node-color-set! nation (choose-color))
		 (validate-single-node-color nation))
	     nations)
	   ;;Validate the choice and backtrack if needed.
	   (validate-all-nodes-color nations)))

	(define (print-colors nations)
	  (for-each
	      (lambda (nation)
		(print "~a: ~a\n"
		       (node-name nation)
		       (node-color nation))
		(for-each
		    (lambda (neighbor)
		      (print "\t~a: ~a\n"
			     (node-name neighbor)
			     (node-color neighbor)))
		  (node-neighbors nation)))
	    europe-facing-nations))

	(define (print . args)
	  (apply fprintf (current-error-port) args))

	#;(amb-backtrack-log (lambda ()
			     (check-pretty-print 'backtrack)))
	#;(print "number of nations: ~a\n"
	       (length europe-facing-nations))
	(color-nations europe-facing-nations)
	#;(print-colors  europe-facing-nations)
	#t)
    => #t)

  #t)


#;(parametrise ((check-test-name	'tasks))

  (define (print . args)
    (apply fprintf (current-error-port) args))

  (define-record-type task
    (fields (immutable id)
		;Symbol, uniquely identifies a task.
	    (immutable time)
		;Positive exact integer, time needed to complete.
	    (immutable effort)))
		;Positive exact integer, effort needed to complete.

  (define tasks
    (let more ((num    25)
	       (tasks  '()))
      (if (zero? num)
	  tasks
	(more (- num 1)
	      (cons (make-task (gensym)
			       (random 24)
			       (random 80))
		    tasks)))))

  #;(print "tasks: ~a\n" tasks)

  (define-values (empty? enqueue! dequeue!)
    (apply make-queue tasks))

  (define scheduling
    (let loop ((available-time    24)
	       (available-effort  100)
	       (days              '()))
      (if (empty?)
	  days
	(let ((task (dequeue!)))
	  ))))

  #t)


;;;; done

(check-report)

;;; end of file
