;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;
;;;Abstract
;;;
;;; 	The primitive  operations on a  transcoder value are  defined in
;;; 	"pass-specify-rep-primops.ss".   A transcoder  value  is just  a
;;; 	word tagged  to make  it of disjoint  type; the payload  of this
;;; 	word is an 8-bit vector whose format is as follows:
;;;
;;;	   765 432 10
;;;         |   |   |
;;;         |   |   -- error handling mode
;;;         |   ------ end of line style
;;;         ---------- codec
;;;
;;;	Notice that the payload can be made bigger if need arises.
;;;
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (ikarus codecs)
  (export native-eol-style
	  latin-1-codec utf-8-codec utf-bom-codec
	  utf-16-codec utf-16le-codec utf-16be-codec
          make-transcoder native-transcoder buffer-mode?
          transcoder-codec transcoder-eol-style
          transcoder-error-handling-mode)
  (import (except (ikarus)
		  native-eol-style
		  latin-1-codec utf-8-codec utf-bom-codec
		  utf-16-codec utf-16le-codec utf-16be-codec
		  make-transcoder native-transcoder
		  buffer-mode? transcoder-codec
		  transcoder-eol-style transcoder-error-handling-mode)
    (ikarus system $transcoders)
    (prefix (rename (ikarus system $fx)
		    ($fx=	fx=)	;comparison
		    ($fxlogor	fxior)	;inclusive logic OR
		    ($fxlogand	fxand))	;logic AND
	    unsafe.))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))

(define-syntax %unsafe.fxior
  (syntax-rules ()
    ((_ ?op1)
     ?op1)
    ((_ ?op1 ?op2)
     (unsafe.fxior ?op1 ?op2))
    ((_ ?op1 ?op2 . ?ops)
     (unsafe.fxior ?op1 (%unsafe.fxior ?op2 . ?ops)))))

(define-inline (%assert-value-is-transcoder ?obj ?who)
  (unless (transcoder? ?obj)
    (assertion-violation ?who "not a transcoder" ?obj)))


(define (latin-1-codec)
  'latin-1-codec)

(define (utf-8-codec)
  'utf-8-codec)

(define (utf-16-codec)
  'utf-16-codec)

(define (utf-16le-codec)
  'utf-16le-codec)

(define (utf-16be-codec)
  'utf-16be-codec)

(define (utf-bom-codec)
  'utf-bom-codec)

(define (native-eol-style)
  'none)


(define error-handling-mode-alist
  ;;2 bits are reserved for the error handling mode.
  ;;
  '((ignore	. #b01)
    (raise	. #b10)
    (replace	. #b11)))
(define error-handling-mode-mask #b11)

(define eol-style-alist
  ;;3 bits are reserved for the error handling mode.
  ;;
;;;                 43210
  '((none	. #b00000)
    (lf		. #b00100)
    (cr		. #b01000)
    (crlf	. #b01100)
    (nel	. #b10000)
    (crnel	. #b10100)
    (ls		. #b11000)))
(define eol-style-mask #b11100)

(define codec-alist
  ;;3 bits are reserved for the codec.
  ;;
;;;                         76543210
  '((latin-1-codec	. #b00100000)
    (utf-8-codec	. #b01000000)
    (utf-16-codec	. #b01100000)
    (utf-16le-codec	. #b10000000)
    (utf-16be-codec	. #b10100000)
    (utf-bom-codec	. #b11000000)
    ))
(define codec-mask	  #b11100000)


(define (%reverse-alist-lookup bits alist)
  (cond ((null? alist)
	 #f)
	((unsafe.fx= (cdar alist) bits)
	 (caar alist))
	(else
	 (%reverse-alist-lookup bits (cdr alist)))))

(define (%codec->fixnum x who)
  (cond ((assq x codec-alist)
	 => cdr)
	(else
	 (assertion-violation who "not a valid codec" x))))

(define (%eol-style->fixnum x who)
  (cond ((assq x eol-style-alist)
	 => cdr)
	(else
	 (assertion-violation who "not a valid eol-style" x))))

(define (%error-handling-mode->fixnum x who)
  (cond ((assq x error-handling-mode-alist)
	 => cdr)
	(else
	 (assertion-violation who "not a valid error-handling mode" x))))


(define make-transcoder
  (case-lambda
   ((codec eol-style handling-mode)
    (define who 'make-transcoder)
    ($data->transcoder (%unsafe.fxior (%error-handling-mode->fixnum handling-mode who)
				      (%eol-style->fixnum	    eol-style     who)
				      (%codec->fixnum		    codec         who))))
   ((codec eol-style)
    (make-transcoder codec eol-style 'replace))
   ((codec)
    (make-transcoder codec 'none 'replace))))

(define native-transcoder
  (make-parameter (make-transcoder 'utf-8-codec 'none 'replace)
    (lambda (obj)
      (if (transcoder? obj)
	  obj
	(assertion-violation 'native-transcoder "expected transcoder value" obj)))))

(define (transcoder-codec x)
  (define who 'transcoder-codec)
  (%assert-value-is-transcoder x who)
  (let ((tag (unsafe.fxand ($transcoder->data x) codec-mask)))
    (or (%reverse-alist-lookup tag codec-alist)
	(assertion-violation who "transcoder has no codec" x))))

(define (transcoder-eol-style x)
  (define who 'transcoder-eol-style)
  (%assert-value-is-transcoder x who)
  (let ((tag (unsafe.fxand ($transcoder->data x) eol-style-mask)))
    (or (%reverse-alist-lookup tag eol-style-alist)
	(assertion-violation who "transcoder has no eol-style" x))))

(define (transcoder-error-handling-mode x)
  (define who 'transcoder-error-handling-mode)
  (%assert-value-is-transcoder x who)
  (let ((tag (unsafe.fxand ($transcoder->data x) error-handling-mode-mask)))
    (or (%reverse-alist-lookup tag error-handling-mode-alist)
	(assertion-violation who "transcoder has no error-handling mode" x))))

(define (buffer-mode? x)
  (and (memq x '(none line block)) #t))


;;;; done

)

;;; end of file
