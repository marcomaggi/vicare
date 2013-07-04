;; ikarus.wordsize.scm --
;;

(module (wordsize
	 boot.case-word-size
	 fixnum-width
	 greatest-fixnum
	 least-fixnum
	 )

  ;;Remember  that WORDSIZE  is  the  number of  bytes  in a  platform's
  ;;machine word: 4 on 32-bit platforms, 8 on 64-bit platforms.
  (module (wordsize)
    (include "ikarus.config.ss" #t))

  (define-syntax boot.case-word-size
    ;;We really need to  define this macro so that it  uses the value of
    ;;WORDSIZE just defined by the "ikarus.config.ss" file.
    ;;
    (lambda (stx)
      (module (wordsize)
	(include "ikarus.config.ss" #t))
      (syntax-case stx ()
	((_ ((32) . ?body-32) ((64) . ?body-64))
	 (case wordsize
	   ((4)
	    #'(begin . ?body-32))
	   ((8)
	    #'(begin . ?body-64))
	   (else
	    (syntax-violation 'boot.case-word-size "invalid wordsize" stx wordsize)))))))

  (boot.case-word-size
   ((32)
    (define-syntax-rule (fixnum-width)
      30)
    (define-syntax-rule (greatest-fixnum)
      +536870911)
    (define-syntax-rule (least-fixnum)
      -536870912))
   ((64)
    (define-syntax-rule (fixnum-width)
      61)
    (define-syntax-rule (greatest-fixnum)
      +1152921504606846975)
    (define-syntax-rule (least-fixnum)
      -1152921504606846976)))

  #| end of module |# )

;;; end of file
