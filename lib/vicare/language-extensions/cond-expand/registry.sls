;;;Copyright 2010 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (vicare language-extensions cond-expand registry)
  (export
    expand-time-features
    run-time-features
    available-features)
  (import (vicare)
    (for (prefix (vicare language-extensions cond-expand platform-features) platform.)
      run expand)
    (for (prefix (vicare language-extensions cond-expand configuration-features) config.)
      run expand))


(define-syntax make-expand-time-features
  (lambda (_)
    (define SRFIs
      '((0		cond-expand)
	(1		lists)
	(2		and-let*)
	#;(5		let)
	(6		basic-string-ports)
	(8		receive)
	(9		records)
	(11		let-values)
	(13		strings)
	(14		char-sets)
	(16		case-lambda)
	#;(17		generalized-set!)
	#;(18		multithreading)
	(19		time)
	#;(21		real-time-multithreading)
	(23		error)
	(25		multi-dimensional-arrays)
	(26		cut)
	(27		random-bits)
	(28		basic-format-strings)
	#;(29		localization)
	(31		rec)
	(37		args-fold)
	(38		with-shared-structure)
	(39		parameters)
	(41		streams)
	(42		eager-comprehensions)
	(43		vectors)
	#;(44		collections)
	(45		lazy)
	#;(46		syntax-rules)
	#;(47		arrays)
	(48		intermediate-format-strings)
	#;(51		rest-values)
	#;(54		cat)
	#;(57		records)
	#;(59		vicinities)
	#;(60		integer-bits)
	(61		cond)
	#;(63		arrays)
	(64		testing)
	#;(66		octet-vectors)
	(67		compare-procedures)
	(69		basic-hash-tables)
	#;(71		let)
	#;(74		blobs)
	(78		lightweight-testing)
	#;(86		mu-and-nu)
	#;(87		case)
	#;(95		sorting-and-merging)
	(98		os-environment-variables)
	(99		records)
	(101		random-access-lists)
	(111		boxes)))

    (define (SRFI-names srfi-entry)
      (define entry.number   car)
      (define entry.mnemonic cadr)
      (define (make-symbol . args)
	(string->symbol (apply string-append
			       (map (lambda (a)
				      (if (symbol? a)
					  (symbol->string a)
					a))
				 args))))
      (let* ((n-str	(number->string (entry.number srfi-entry)))
	     (colon-n	(make-symbol ":"     n-str))
	     (srfi-n	(make-symbol "srfi-" n-str))
	     (srfi-n-m	(make-symbol srfi-n (make-symbol "-" (entry.mnemonic srfi-entry)))))
	;;The  first   two  are   recommended  by   SRFI-97.   Examples:
	;;"srfi-11", "srfi-11-let-values".
	;;
	;;The  last two  are  the  two types  of  SRFI-97 library  name.
	;;Examples: "(srfi :11)", "(srfi :11 let-values)".
	;;
	(list srfi-n
	      srfi-n-m
	      `(srfi ,colon-n)
	      `(srfi ,colon-n ,(entry.mnemonic srfi-entry)))))

    (let* ((srfi-features         (apply append (map SRFI-names SRFIs)))
	   (expand-time-features  (platform.expand-time-features))
	   (language-features     '(r6rs))
	   (config-features       (config.configuration-time-features))
	   (all-features          (append srfi-features
					  expand-time-features
					  language-features
					  config-features)))
      #`(quote #,(datum->syntax #'ignored all-features)))))

(define expand-time-features
  (make-expand-time-features))

(define run-time-features
  (platform.run-time-features))

(define available-features
  (append run-time-features expand-time-features))


;;;; done

)

;;; end of file
