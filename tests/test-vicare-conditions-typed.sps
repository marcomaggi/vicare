;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for condition object types under typed language
;;;Date: Tue Sep 15, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(program (test-vicare-conditions-typed)
  (options tagged-language)
  (import (vicare)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare libraries: tests for condition object-types under typed language\n")


(parametrise ((check-test-name	'generic-type-maker))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?type (?arg ...))
       (begin
	 (check
	     (is-a? (new ?type ?arg ...) ?type)
	   => #t)
	 ))
      ))

  (doit &condition					())
  (doit &who						('it))
  (doit &message					("it"))
  (doit &irritants					('(it)))

  (doit &warning					())
  (doit &serious					())
  (doit &violation					())
  (doit &assertion					())
  (doit &non-continuable				())
  (doit &implementation-restriction			())
  (doit &lexical					())
  (doit &syntax						(#f #f))
  (doit &undefined					())

  (doit &i/o						())
  (doit &i/o-read					())
  (doit &i/o-write					())
  (doit &i/o-invalid-position				(#f))
  (doit &i/o-filename					("filename"))
  (doit &i/o-file-protection				("filename"))
  (doit &i/o-file-is-read-only				("filename"))
  (doit &i/o-file-already-exists			("filename"))
  (doit &i/o-file-does-not-exist			("filename"))
  (doit &i/o-port					((current-input-port)))
  (doit &i/o-decoding					((current-input-port)))
  (doit &i/o-encoding					((current-output-port) #\C))
  (doit &i/o-eagain					())

  (doit &errno						(1))
  (doit &out-of-memory-error				())
  (doit &h_errno					(1))
  (doit &no-infinities					())
  (doit &no-nans					())
  (doit &interrupted					())
  (doit &source-position				("the-port" 1 1 1 1))

  (doit &failed-expression-condition			(#f))
  (doit &procedure-precondition-violation		())
  (doit &procedure-postcondition-violation		())
  (doit &procedure-argument-violation			())
  (doit &procedure-signature-argument-violation		(1 #f #f))
  (doit &procedure-signature-return-value-violation	(1 #f #f))
  (doit &procedure-arguments-consistency-violation	())
  (doit &expression-return-value-violation		())
  (doit &non-reinstatable				())
  (doit &string-encoding				())
  (doit &string-decoding				())
  (doit &utf8-string-encoding				())
  (doit &utf16-string-encoding				())
  (doit &utf32-string-encoding				())
  (doit &utf8-string-decoding				())
  (doit &utf16-string-decoding				())
  (doit &utf32-string-decoding				())
  (doit &utf8-string-decoding-invalid-octet		('#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-2-tuple		('#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-3-tuple		('#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-4-tuple		('#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-2-tuple	('#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-3-tuple	('#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-4-tuple	('#vu8() 0 '()))
  (doit &utf16-string-decoding-invalid-first-word	('#vu8() 0 0))
  (doit &utf16-string-decoding-invalid-second-word	('#vu8() 0 0 0))
  (doit &utf16-string-decoding-missing-second-word	('#vu8() 0 0))
  (doit &utf16-string-decoding-standalone-octet		('#vu8() 0 0))
  (doit &utf32-string-decoding-invalid-word		('#vu8() 0 0))
  (doit &utf32-string-decoding-orphan-octets		('#vu8() 0 '()))

  (void))


(parametrise ((check-test-name	'generic-type-predicate))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?type ?builder)
       (begin
	 (check
	     (is-a? ?builder ?type)
	   => #t)
	 (check
	     (let (({O ?type} ?builder))
	       (is-a? O ?type))
	   => #t)
	 (check
	     (is-a? (condition ?builder) ?type)
	   => #t)
	 (check
	     (is-a? 123 ?type)
	   => #f)
	 ))
      ))

  (doit &condition					(make-who-condition 'it))
  (doit &who						(make-who-condition 'it))
  (doit &message					(make-message-condition "it"))
  (doit &irritants					(make-irritants-condition '(it)))

  (doit &warning					(make-warning))
  (doit &serious					(make-serious-condition))
  (doit &violation					(make-violation))
  (doit &assertion					(make-assertion-violation))
  (doit &non-continuable				(make-non-continuable-violation))
  (doit &implementation-restriction			(make-implementation-restriction-violation))
  (doit &lexical					(make-lexical-violation))
  (doit &syntax						(make-syntax-violation #f #f))
  (doit &undefined					(make-undefined-violation))

  (doit &i/o						(make-i/o-error))
  (doit &i/o-read					(make-i/o-read-error))
  (doit &i/o-write					(make-i/o-write-error))
  (doit &i/o-invalid-position				(make-i/o-invalid-position-error #f))
  (doit &i/o-filename					(make-i/o-filename-error "filename"))
  (doit &i/o-file-protection				(make-i/o-file-protection-error "filename"))
  (doit &i/o-file-is-read-only				(make-i/o-file-is-read-only-error "filename"))
  (doit &i/o-file-already-exists			(make-i/o-file-already-exists-error "filename"))
  (doit &i/o-file-does-not-exist			(make-i/o-file-does-not-exist-error "filename"))
  (doit &i/o-port					(make-i/o-port-error (current-input-port)))
  (doit &i/o-decoding					(make-i/o-decoding-error (current-input-port)))
  (doit &i/o-encoding					(make-i/o-encoding-error (current-output-port) #\C))
  (doit &i/o-eagain					(make-i/o-eagain))

  (doit &errno						(make-errno-condition 1))
  (doit &out-of-memory-error				(make-out-of-memory-error))
  (doit &h_errno					(make-h_errno-condition 1))
  (doit &no-infinities					(make-no-infinities-violation))
  (doit &no-nans					(make-no-nans-violation))
  (doit &interrupted					(make-interrupted-condition))
  (doit &source-position				(make-source-position-condition "the-port" 1 1 1 1))

  (doit &failed-expression-condition			(make-failed-expression-condition #f))
  (doit &procedure-precondition-violation		(make-procedure-precondition-violation))
  (doit &procedure-postcondition-violation		(make-procedure-postcondition-violation))
  (doit &procedure-argument-violation			(make-procedure-argument-violation))
  (doit &procedure-signature-argument-violation		(make-procedure-signature-argument-violation 1 #f #f))
  (doit &procedure-signature-return-value-violation	(make-procedure-signature-return-value-violation 1 #f #f))
  (doit &procedure-arguments-consistency-violation	(make-procedure-arguments-consistency-violation))
  (doit &expression-return-value-violation		(make-expression-return-value-violation))
  (doit &non-reinstatable				(make-non-reinstatable-violation))
  (doit &string-encoding				(make-string-encoding-error))
  (doit &string-decoding				(make-string-decoding-error))
  (doit &utf8-string-encoding				(make-utf8-string-encoding-error))
  (doit &utf16-string-encoding				(make-utf16-string-encoding-error))
  (doit &utf32-string-encoding				(make-utf32-string-encoding-error))
  (doit &utf8-string-decoding				(make-utf8-string-decoding-error))
  (doit &utf16-string-decoding				(make-utf16-string-decoding-error))
  (doit &utf32-string-decoding				(make-utf32-string-decoding-error))
  (doit &utf8-string-decoding-invalid-octet		(make-utf8-string-decoding-invalid-octet '#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-2-tuple		(make-utf8-string-decoding-invalid-2-tuple '#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-3-tuple		(make-utf8-string-decoding-invalid-3-tuple '#vu8() 0 '()))
  (doit &utf8-string-decoding-invalid-4-tuple		(make-utf8-string-decoding-invalid-4-tuple '#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-2-tuple	(make-utf8-string-decoding-incomplete-2-tuple '#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-3-tuple	(make-utf8-string-decoding-incomplete-3-tuple '#vu8() 0 '()))
  (doit &utf8-string-decoding-incomplete-4-tuple	(make-utf8-string-decoding-incomplete-4-tuple '#vu8() 0 '()))
  (doit &utf16-string-decoding-invalid-first-word	(make-utf16-string-decoding-invalid-first-word '#vu8() 0 0))
  (doit &utf16-string-decoding-invalid-second-word	(make-utf16-string-decoding-invalid-second-word '#vu8() 0 0 0))
  (doit &utf16-string-decoding-missing-second-word	(make-utf16-string-decoding-missing-second-word '#vu8() 0 0))
  (doit &utf16-string-decoding-standalone-octet		(make-utf16-string-decoding-standalone-octet '#vu8() 0 0))
  (doit &utf32-string-decoding-invalid-word		(make-utf32-string-decoding-invalid-word '#vu8() 0 0))
  (doit &utf32-string-decoding-orphan-octets		(make-utf32-string-decoding-orphan-octets '#vu8() 0 '()))

  (void))


(parametrise ((check-test-name	'generic-slot-accessors))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?type ?builder ((?slot ?value) ...))
       (let (({O ?type} ?builder))
	 (begin
	   (check
	       (slot-ref O ?slot)
	     => (quasiquote ?value))
	   (check
	       (method-call ?slot O)
	     => (quasiquote ?value))
	   (check
	       (method-call-late-binding (quote ?slot) O)
	     => (quasiquote ?value)))
	 ...
	 ))
      ))

  (doit &who		(make-who-condition 'it)		((who it)))
  (doit &message	(make-message-condition "it")		((message "it")))
  (doit &irritants	(make-irritants-condition '(it))	((irritants (it))))
  (doit &syntax		(make-syntax-violation 1 2)		((form 1) (subform 2)))

  (doit &i/o-invalid-position
	(make-i/o-invalid-position-error #f)
	((position #f)))
  (doit &i/o-filename
	(make-i/o-filename-error "filename")
	((filename "filename")))
  (doit &i/o-port
	(make-i/o-port-error (current-input-port))
	((port ,(current-input-port))))
  (doit &i/o-encoding
	(make-i/o-encoding-error (current-output-port) #\C)
	((char #\C)))


  (doit &errno
	(make-errno-condition 1)
	((code 1)))
  (doit &h_errno
	(make-h_errno-condition 1)
	((code 1)))
  (doit &source-position
	(make-source-position-condition "the-port" 1 2 3 4)
	((port-id	"the-port")
	 (byte		1)
	 (character	2)
	 (line		3)
	 (column	4)))

  (doit &failed-expression-condition
	(make-failed-expression-condition #f)
	((failed-expression		#f)))
  (doit &procedure-signature-argument-violation
	(make-procedure-signature-argument-violation 1 #f #t)
	((one-based-argument-index	1)
	 (failed-expression		#f)
	 (offending-value		#t)))
  (doit &procedure-signature-return-value-violation
	(make-procedure-signature-return-value-violation 1 #f #t)
	((one-based-return-value-index	1)
	 (failed-expression		#f)
	 (offending-value		#t)))

  (doit &utf8-string-decoding-invalid-octet
	(make-utf8-string-decoding-invalid-octet '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-invalid-2-tuple
	(make-utf8-string-decoding-invalid-2-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-invalid-3-tuple
	(make-utf8-string-decoding-invalid-3-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-invalid-4-tuple
	(make-utf8-string-decoding-invalid-4-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-incomplete-2-tuple
	(make-utf8-string-decoding-incomplete-2-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-incomplete-3-tuple
	(make-utf8-string-decoding-incomplete-3-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))
  (doit &utf8-string-decoding-incomplete-4-tuple
	(make-utf8-string-decoding-incomplete-4-tuple '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))

  (doit &utf16-string-decoding-invalid-first-word
	(make-utf16-string-decoding-invalid-first-word '#vu8() 0 1)
	((bytevector	#vu8())
	 (index		0)
	 (word		1)))
  (doit &utf16-string-decoding-invalid-second-word
	(make-utf16-string-decoding-invalid-second-word '#vu8() 0 1 2)
	((bytevector	#vu8())
	 (index		0)
	 (first-word	1)
	 (second-word	2)))
  (doit &utf16-string-decoding-missing-second-word
	(make-utf16-string-decoding-missing-second-word '#vu8() 0 1)
	((bytevector	#vu8())
	 (index		0)
	 (word		1)))
  (doit &utf16-string-decoding-standalone-octet
	(make-utf16-string-decoding-standalone-octet '#vu8() 0 1)
	((bytevector	#vu8())
	 (index		0)
	 (octet		1)))

  (doit &utf32-string-decoding-invalid-word
	(make-utf32-string-decoding-invalid-word '#vu8() 0 1)
	((bytevector	#vu8())
	 (index		0)
	 (word		1)))
  (doit &utf32-string-decoding-orphan-octets
	(make-utf32-string-decoding-orphan-octets '#vu8() 0 '())
	((bytevector	#vu8())
	 (index		0)
	 (octets	())))

  (void))


;;;; done

(check-report)

#| end of program |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:
