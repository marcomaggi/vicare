;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: command line options parsing
;;;Date: Wed Nov 11, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2010, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare getopts)
  (export

    command-line-option
    make-command-line-option			command-line-option?
    command-line-option-brief			command-line-option-long
    command-line-option-requires-argument?	command-line-option-description
    command-line-option-action
    command-line-option.vicare-arguments-validation
    list-of-command-line-options.vicare-arguments-validation

    getopts			getopts-options
    define-command-line-option

    ;; auxiliary syntaxes
    brief			long
    require-argument		description
    action

    ;; condition objects
    &getopts		make-getopts-condition	getopts-condition?

    &option		make-option-condition	option-condition?
    condition-option

    &argument		make-argument-condition		argument-condition?
    condition-argument

    &brief/long		make-brief/long-condition	brief/long-condition?
    condition-brief/long

    &unknown-option	make-unknown-option-condition	unknown-option-condition?

    &option-requires-value
    make-option-requires-value-condition
    option-requires-value-condition?

    &option-requires-no-value
    make-option-requires-no-value-condition
    option-requires-no-value-condition?

    &invalid-option
    make-invalid-option-condition
    invalid-option-condition?

    raise-unknown-option
    raise-option-requires-value
    raise-option-requires-no-value
    raise-invalid-option)
  (import (vicare)
    (vicare arguments validation)
    (vicare unsafe operations)
    (vicare language-extensions syntaxes))


;;;; helpers

(define (%find-brief brief-char options)
  ;;Given the  brief option char  BRIEF-CHAR scan the list  of supported
  ;;options in OPTIONS and return the COMMAND-LINE-OPTION record itself.
  ;;If not found, return false.
  ;;
  (cond ((null? options)
	 #f)
	((let ((option-brief (command-line-option-brief (car options))))
	   (and option-brief (char=? brief-char option-brief)))
	 (car options))
	(else
	 (%find-brief brief-char (cdr options)))))

(define (%find-long long-string options)
  ;;Given the long option string  LONG-STRING scan the list of supported
  ;;options in OPTIONS and return the COMMAND-LINE-OPTION record itself.
  ;;If not found, return false.
  ;;
  (cond ((null? options)
	 #f)
	((let ((option-long (command-line-option-long (car options))))
	   (and option-long (string=? long-string option-long)))
	 (car options))
	(else
	 (%find-long long-string (cdr options)))))

(define-syntax %string-index
  ;;Return the position of ?CHAR in ?STR, or false if not found.
  ;;
  (syntax-rules ()
    ((_ ?char ?str)
     (let* ((str ?str)
	    (len (string-length str)))
       (let loop ((i 0))
	 (and (< i len)
	      (if (char=? ?char (string-ref str i))
		  i
		(loop (+ 1 i)))))))))


(define-record-type command-line-option
  (nongenerative vicare:getopts:command-line-option)
  (fields (immutable brief)
		;A Scheme char representing a brief option.
	  (immutable long)
		;A Scheme string representing a long option, without the
		;leading "--".
	  (immutable requires-argument?)
		;Boolean, true if this option requires an argument.
	  (immutable description)
		;Scheme string describing this option.
	  (immutable action)
		;Closure to be invoked when this option is found.
	  )
  (protocol
   (lambda (make-record)
     (lambda (brief long require-argument? description action)
       (define who 'make-command-line-option)
       (with-arguments-validation (who)
	   ((char/false		brief)
	    (string/false	long)
	    (string		description)
	    (procedure/false	action))
	 (make-record brief long
		      (if require-argument? #t #f)
		      description action))))))

(define-argument-validation (command-line-option who obj)
  (command-line-option? obj)
  (assertion-violation who
    "expected command line option record as argument" obj))

(define-argument-validation (list-of-command-line-options who obj)
  (for-all command-line-option? obj)
  (assertion-violation who
    "expected list of command line option record as argument" obj))

;;; --------------------------------------------------------------------

(define-auxiliary-syntaxes
  brief long require-argument description action)

(define-syntax define-command-line-option
  (let*-constants
      ((who	'define-command-line-option)
       (specs	(list (make-syntax-clause-spec #'brief			0 1 1 1 '() '())
		      (make-syntax-clause-spec #'long			0 1 1 1 '() '())
		      (make-syntax-clause-spec #'require-argument	0 1 1 1 '() '())
		      (make-syntax-clause-spec #'description		0 1 1 1 '() '())
		      (make-syntax-clause-spec #'action			0 1 1 1 '() '()))))
    (define-struct options
      (brief long require-argument? description semantic-action))
    (lambda (stx)
      (define (main stx)
	(syntax-case stx ()
	  ((_ ?name . ?clauses)
	   (identifier? #'?name)
	   (let* ((knil    (make-options #f #f #f "undocumented option" #'%default-semantic-action))
		  (clauses (syntax-clauses-unwrap #'?clauses synner))
		  (opt     (syntax-clauses-fold-specs combine knil specs clauses synner)))
	     (with-syntax
		 ((BRIEF		(options-brief opt))
		  (LONG			(options-long opt))
		  (REQUIRE-ARGUMENT	(options-require-argument? opt))
		  (DESCRIPTION		(options-description opt))
		  (ACTION		(options-semantic-action opt)))
	       #'(define ?name
		   (make-command-line-option BRIEF LONG REQUIRE-ARGUMENT DESCRIPTION ACTION)))))
	  (_
	   (synner "invalid syntax for command line option definition"))))

      (define (combine opt spec args)
	;;Remember that ARGS  is a vector of vectors  of syntax objects.
	;;We know that:
	;;
	;;* All the clauses must appear 0 or 1 time; so ARGS is always a
	;;  vector of a single argument.
	;;
	;;* All the clauses accept  a single mandatory argument, so each
	;;  nested vector will have a single argument.
	;;
	(define-inline (get-arg)
	  (vector-ref (vector-ref args 0) 0))
	(case-identifiers (syntax-clause-spec-keyword spec)
	  ((brief)		(set-options-brief!		opt (get-arg)))
	  ((long)		(set-options-long!		opt (get-arg)))
	  ((require-argument)	(set-options-require-argument?!	opt (get-arg)))
	  ((description)	(set-options-description!	opt (get-arg)))
	  ((action)		(set-options-semantic-action!	opt (get-arg))))
	opt)

      (define synner
	(case-lambda
	 ((message)
	  (synner message #f))
	 ((message subform)
	  (syntax-violation who message stx subform))))

      (main stx))))

(define (%default-semantic-action . args)
  (error #f "missing semantic action while parsing command line option" args))


(define-condition-type &getopts &error
  make-getopts-condition getopts-condition?)

(define-condition-type &option &condition
  make-option-condition option-condition?
  (option condition-option))

(define-condition-type &argument &condition
  make-argument-condition argument-condition?
  (argument condition-argument))

(define-condition-type &brief/long &condition
  make-brief/long-condition brief/long-condition?
  (brief/long condition-brief/long))

;;; --------------------------------------------------------------------

(define-condition-type &unknown-option &getopts
  make-unknown-option-condition unknown-option-condition?)

(define (raise-unknown-option who brief/long argument message)
  (raise-continuable (condition (make-who-condition who)
				(make-unknown-option-condition)
				(make-brief/long-condition brief/long)
				(make-argument-condition argument)
				(make-message-condition message))))

;;; --------------------------------------------------------------------

(define-condition-type &option-requires-value &getopts
  make-option-requires-value-condition option-requires-value-condition?)

(define (raise-option-requires-value who option argument message)
  (raise-continuable (condition (make-who-condition who)
				(make-option-requires-value-condition)
				(make-option-condition option)
				(make-argument-condition argument)
				(make-message-condition message))))

;;; --------------------------------------------------------------------

(define-condition-type &option-requires-no-value &getopts
  make-option-requires-no-value-condition option-requires-no-value-condition?)

(define (raise-option-requires-no-value who option argument message)
  (raise-continuable (condition (make-who-condition who)
				(make-option-requires-no-value-condition)
				(make-option-condition option)
				(make-argument-condition argument)
				(make-message-condition message))))

;;; --------------------------------------------------------------------

(define-condition-type &invalid-option &getopts
  make-invalid-option-condition invalid-option-condition?)

(define (raise-invalid-option who option argument message)
  (raise-continuable (condition (make-who-condition who)
				(make-invalid-option-condition)
				(make-option-condition option)
				(make-argument-condition argument)
				(make-message-condition message))))


(define-enumeration getopts-configuration
  (delayed
		;If given the actions are returned in a list rather that
		;immediately evaluated.
   ignore-multiple-double-dashes
		;If given multiple '--'  arguments are all ignored; else
		;the first marks the end  of options, the subsequent are
		;common non-options.
   )
  getopts-options)


(define getopts
  (case-lambda
   ((command-line options argument-action)
    (getopts command-line options argument-action (getopts-options)))

   ((command-line options argument-action config-options)
    (define who 'getopts)
    (with-arguments-validation (who)
	((list-of-command-line-options	options))
      (define actions '())
      (define-syntax delayed?
	(identifier-syntax (enum-set-member? 'delayed config-options)))
      (define-syntax ignore-multiple-double-dashes?
	(identifier-syntax (enum-set-member? 'ignore-multiple-double-dashes config-options)))
      (define (%deal-with-non-option argument)
	(if delayed?
	    (set-cons! actions (lambda ()
				 (argument-action argument)))
	  (argument-action argument)))
      (define (%deal-with-option-with-value option value)
	(let ((action ($command-line-option-action option)))
	  (if delayed?
	      (set-cons! actions (lambda ()
				   (action option value)))
	    (action option value))))
      (define (%deal-with-option-without-value option)
	(let ((action ($command-line-option-action option)))
	  (if delayed?
	      (set-cons! actions (lambda ()
				   (action option)))
	    (action option))))

      (let parse-next-argument ((command-line		command-line)
				(marked-end-of-options?	#f))
	(if (null? command-line)
	    (if delayed?
		(reverse actions)
	      #t)
	  (let* ((arg.ptr (car command-line))
		 (arg.len (string-length arg.ptr)))

	    (define-syntax arg.char
	      (syntax-rules ()
		((_ ?index)
		 (string-ref arg.ptr ?index))))

	    (define-syntax arg.char=?
	      (syntax-rules ()
		((_ ?char ?index)
		 (char=? ?char (arg.char ?index)))))

	    (cond (marked-end-of-options?
		   (when (or (not ignore-multiple-double-dashes?)
			     (not (string=? "--" arg.ptr)))
		     (%deal-with-non-option arg.ptr))
		   (parse-next-argument (cdr command-line) marked-end-of-options?))

		  ((zero? arg.len)
		   ;;In truth, this should never happen.
		   (parse-next-argument (cdr command-line) marked-end-of-options?))

		  ((string=? "--" arg.ptr)
		   (parse-next-argument (cdr command-line) #t))

		  ((or (= 1 arg.len) (not (arg.char=? #\- 0)))
		   ;;An  option has  at least  2 chars  and begins  with a
		   ;;dash; if it's not so, this argument is a non-option.
		   (%deal-with-non-option arg.ptr)
		   (parse-next-argument (cdr command-line) marked-end-of-options?))

;;;From now on we have established that the first char is #\-.

		  ((and (arg.char=? #\- 0) (arg.char=? #\- 1))
		   ;;The argument looks like a long option.
		   (let* ((equal-index	(%string-index #\= arg.ptr))
			  (long-string	(substring arg.ptr 2 (or equal-index arg.len)))
			  (option		(%find-long long-string options)))
		     (cond ((not option)
			    (raise-unknown-option 'getopts long-string arg.ptr "unknown long option")
			    (parse-next-argument (cdr command-line) marked-end-of-options?))
			   (($command-line-option-requires-argument? option)
			    (cond (equal-index
				   ;;Found  an option-with-value  with the
				   ;;format '--option=value'.
				   (%deal-with-option-with-value
				    option (substring arg.ptr (+ 1 equal-index) arg.len))
				   (parse-next-argument (cdr command-line) marked-end-of-options?))
				  ((null? (cdr command-line))
				   ;;One  more  argument  was expected  as
				   ;;option's value.
				   (raise-option-requires-value 'getopts option arg.ptr
								"option requires value"))
				  (else
				   ;;Found  an   option  with  value  with
				   ;;format '--option value'.
				   (%deal-with-option-with-value option (cadr command-line))
				   (parse-next-argument (cddr command-line) marked-end-of-options?))))
			   (equal-index
			    (raise-option-requires-no-value 'getopts option arg.ptr
							    "option with no value with '=' attached"))
			   (else
			    (%deal-with-option-without-value option)
			    (parse-next-argument (cdr command-line) marked-end-of-options?)))))

;;;Maybe this is a brief option:
;;;
;;;- If (= 2 arg.len), this may be  a single brief option (example: -h).
;;;
;;;- Else a train of brief options (example: -hif).
;;;
;;;- Else a brief option with attached value (example: -afile).

		  (else
		   (do ((j 1 (+ 1 j)))
		       ((>= j arg.len)
			(parse-next-argument (cdr command-line) marked-end-of-options?))

;;;To exit  this loop we have to  set J to ARG.LEN,  rather than calling
;;;PARSE-NEXT-ARGUMENT directly.

		     (let* ((brief-char	(arg.char j))
			    (option	(%find-brief brief-char options)))

		       (cond ((not option)
			      (raise-unknown-option 'getopts brief-char arg.ptr "unknown brief option"))

			     (($command-line-option-requires-argument? option)
			      (cond ((and (< 1 j) (not (= (+ 1 j) arg.len)))
				     ;;This brief  option requires a value
				     ;;but it  is neither a  single option
				     ;;with value  attached (-Ovalue), nor
				     ;;the  last  in  the  argument  (-abO
				     ;;value).
				     ;;
				     ;;This  could be  correct (-abOvalue)
				     ;;but it would  be difficult to read,
				     ;;so we forbid it.
				     (raise-invalid-option
				      'getopts option arg.ptr
				      "train of single char options with value attached"))
				    ((< (+ 1 j) arg.len)
				     ;;Found a  brief option with attached
				     ;;value (-Ovalue).
				     (%deal-with-option-with-value option
								   (substring arg.ptr (+ 1 j) arg.len))
				     (set! j arg.len))
				    ((null? (cdr command-line))
				     ;;One  more argument was  expected as
				     ;;option's value.
				     (raise-option-requires-value 'getopts option arg.ptr
								  "option requires value"))
				    (else
				     ;;Found a brief  option with value in
				     ;;the next argument.
				     (%deal-with-option-with-value option (cadr command-line))
				     (set! j arg.len))))

			     (else
			      ;;Found  a  brief  option  with  no  value.
			      (%deal-with-option-without-value option))))))))))))))


;;;; done

)

;;; end of file
