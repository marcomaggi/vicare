;;;Copyright (c) 2009 Abdulaziz Ghuloum
;;;Modified by Marco Maggi.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.

#!vicare
(library (vicare numerics flonum-formatter)
  (options typed-language)
  (export format-flonum)
  (import (vicare))


(define (format-flonum positive? {digits (list-of <char>)} {expt <fixnum>})
  (define (to-string positive? ls)
    ;;Convert alist  of characters to the corresponding  string adding a
    ;;sign character as appropriate.
    ;;
    (if positive?
	(list->string ls)
      (list->string (cons #\- ls))))
  (define (format-flonum-no-expt expt d0 d*)
    ;;Given a  first char D0 and  the subsequent list of  chars D* build
    ;;and  return  a new  list  of  characters  having the  decimal  "."
    ;;inserted as appropriate for the positive exponent EXPT.
    ;;
    (define (next x)
      (if (null? x)
	  (values #\0 '())
	(values (car x) (cdr x))))
    (cons d0
	  (if (= expt 1)
	      (if (null? d*) '(#\. #\0) (cons #\. d*))
	    (let-values (((d0 d*) (next d*)))
	      (format-flonum-no-expt (- expt 1) d0 d*)))))
  (define (format-flonum-no-expt/neg expt d*)
    ;;Given a  first char D0 and  the subsequent list of  chars D* build
    ;;and  return a new  list of  characters prepending  the appropriate
    ;;number of zeros to format a number like "0.00123".
    ;;
    (if (zero? expt)
	d*
      (cons #\0 (format-flonum-no-expt/neg (+ expt 1) d*))))
  (let ((d0 (car digits))
	(d* (cdr digits)))
    (cond ((null? d*)
	   (cond ((char=? d0 #\0)
		  (if positive? "0.0" "-0.0"))
		 ((= expt 1)
		  (if positive?
		      (string d0 #\. #\0)
		    (string #\- d0 #\. #\0)))
		 ((= expt 0)
		  (if positive?
		      (string #\0 #\. d0)
		    (string #\- #\0 #\. d0)))
		 (else
		  (string-append (if positive? "" "-")
				 (string d0) "e" (number->string (- expt 1))))))
	  ((<= 1 expt 9)
	   (to-string positive? (format-flonum-no-expt expt d0 d*)))
	  ((<= -3 expt 0)
	   (to-string positive? (cons* #\0 #\. (format-flonum-no-expt/neg expt digits))))
	  (else
	   (string-append (if positive? "" "-")
			  (string d0) "." (list->string d*)
			  "e" (number->string (- expt 1)))))))


;;;; done

)

;;; end of file
