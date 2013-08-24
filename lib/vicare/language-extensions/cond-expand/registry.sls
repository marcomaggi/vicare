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
      (let ((SRFI-ENABLED?  (vicare-built-with-srfi-enabled))
	    (POSIX-ENABLED? (vicare-built-with-posix-enabled)))
	`(#(0		cond-expand			,SRFI-ENABLED?)
	  #(1		lists				,SRFI-ENABLED?)
	  #(2		and-let*			,SRFI-ENABLED?)
	  #;#(5		let				,SRFI-ENABLED?)
	  #(6		basic-string-ports		,SRFI-ENABLED?)
	  #(8		receive				,SRFI-ENABLED?)
	  #(9		records				,SRFI-ENABLED?)
	  #(11		let-values			,SRFI-ENABLED?)
	  #(13		strings				,SRFI-ENABLED?)
	  #(14		char-sets			,SRFI-ENABLED?)
	  #(16		case-lambda			,SRFI-ENABLED?)
	  #;#(17	generalized-set!		,SRFI-ENABLED?)
	  #;#(18	multithreading			,SRFI-ENABLED?)
	  #(19		time				,SRFI-ENABLED?)
	  #;#(21	real-time-multithreading	,SRFI-ENABLED?)
	  #(23		error				,SRFI-ENABLED?)
	  #(25		multi-dimensional-arrays	,SRFI-ENABLED?)
	  #(26		cut				,SRFI-ENABLED?)
	  #(27		random-bits			,SRFI-ENABLED?)
	  #(28		basic-format-strings		,SRFI-ENABLED?)
	  #;#(29	localization			,SRFI-ENABLED?)
	  #(31		rec				,SRFI-ENABLED?)
	  #(37		args-fold			,SRFI-ENABLED?)
	  #(38		with-shared-structure		,SRFI-ENABLED?)
	  #(39		parameters			,SRFI-ENABLED?)
	  #(41		streams				,SRFI-ENABLED?)
	  #(42		eager-comprehensions		,SRFI-ENABLED?)
	  #(43		vectors				,SRFI-ENABLED?)
	  #;#(44	collections			,SRFI-ENABLED?)
	  #(45		lazy				,SRFI-ENABLED?)
	  #;#(46	syntax-rules			,SRFI-ENABLED?)
	  #;#(47	arrays				,SRFI-ENABLED?)
	  #(48		intermediate-format-strings	,SRFI-ENABLED?)
	  #;#(51	rest-values			,SRFI-ENABLED?)
	  #;#(54	cat				,SRFI-ENABLED?)
	  #;#(57	records				,SRFI-ENABLED?)
	  #;#(59	vicinities			,SRFI-ENABLED?)
	  #;#(60	integer-bits			,SRFI-ENABLED?)
	  #(61		cond				,SRFI-ENABLED?)
	  #;#(63	arrays				,SRFI-ENABLED?)
	  #(64		testing				,SRFI-ENABLED?)
	  #;#(66	octet-vectors			,SRFI-ENABLED?)
	  #(67		compare-procedures		,SRFI-ENABLED?)
	  #(69		basic-hash-tables		,SRFI-ENABLED?)
	  #;#(71	let				,SRFI-ENABLED?)
	  #;#(74	blobs				,SRFI-ENABLED?)
	  #(78		lightweight-testing		,SRFI-ENABLED?)
	  #;#(86	mu-and-nu			,SRFI-ENABLED?)
	  #;#(87	case				,SRFI-ENABLED?)
	  #;#(95	sorting-and-merging		,SRFI-ENABLED?)
	  #(98		os-environment-variables	,SRFI-ENABLED?)
	  #(99		records				,SRFI-ENABLED?)
	  #(101		random-access-lists		,SRFI-ENABLED?)
	  #(106		socket				,(and SRFI-ENABLED? POSIX-ENABLED?))
	  #(111		boxes				,SRFI-ENABLED?)
	  )))

    (define (SRFI-names srfi-entry)
      (define (entry.number entry)
	(vector-ref entry 0))
      (define (entry.mnemonic entry)
	(vector-ref entry 1))
      (define (entry.enabled? entry)
	(vector-ref entry 2))
      (define (make-symbol . args)
	(string->symbol (apply string-append
			       (map (lambda (a)
				      (if (symbol? a)
					  (symbol->string a)
					a))
				 args))))
      (if (entry.enabled? srfi-entry)
	  (let* ((n-str     (number->string (entry.number srfi-entry)))
		 (colon-n   (make-symbol ":"     n-str))
		 (srfi-n    (make-symbol "srfi-" n-str))
		 (srfi-n-m  (make-symbol srfi-n (make-symbol "-" (entry.mnemonic srfi-entry)))))
	    ;;The  first  two  are recommended  by  SRFI-97.   Examples:
	    ;;"srfi-11", "srfi-11-let-values".
	    ;;
	    ;;The last  two are the  two types of SRFI-97  library name.
	    ;;Examples: "(srfi :11)", "(srfi :11 let-values)".
	    ;;
	    (list srfi-n
		  srfi-n-m
		  `(srfi ,colon-n)
		  `(srfi ,colon-n ,(entry.mnemonic srfi-entry))))
	'()))

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
