;;;Copyright (c) 2011-2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of the  GNU General  Public  License version  3  as published  by the  Free
;;;Software Foundation.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; type predicates

;;Defined by R6RS.  Return #t if X is a port.
(define (port? x)
  (primop::port? x))


;;;; input/output, binary/textual, unsafe predicates

;;The following predicates have  to be used after the argument  has been validated as
;;port value.  The following predicates are *not*  affected by the fact that the port
;;is closed.
;;
(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?bits)
			  (define (?who x)
			    ($fx= ($fxand ($port-tag x) ?bits) ?bits)))
			 )))
  (define-predicate $binary-port?			BINARY-PORT-TAG)
  (define-predicate $textual-port?			TEXTUAL-PORT-TAG)
  ;;
  (define-predicate $input-port?			INPUT-PORT-TAG)
  (define-predicate $output-port?			OUTPUT-PORT-TAG)
  (define-predicate $input/output-port?			INPUT/OUTPUT-PORT-TAG)
  ;;
  ;;
  (define-predicate $binary-input-port?			BINARY-INPUT-PORT-BITS)
  (define-predicate $binary-output-port?		BINARY-OUTPUT-PORT-BITS)
  ;;
  (define-predicate $textual-input-port?		TEXTUAL-INPUT-PORT-BITS)
  (define-predicate $textual-output-port?		TEXTUAL-OUTPUT-PORT-BITS)
  #| end of LET-SYNTAX |# )

(define ($input-only-port? port)
  ;;True if PORT is input and not input/output.
  ;;
  (and ($input-port? port)
       ($fxzero? ($fxand ($port-attrs port) INPUT/OUTPUT-PORT-TAG))))

(define ($output-only-port? port)
  ;;True if PORT is output and not input/output.
  ;;
  (and ($output-port? port)
       ($fxzero? ($fxand ($port-attrs port) INPUT/OUTPUT-PORT-TAG))))


;;;; input/output, binary/textual, safe predicates

;;The following predicates are *not* affected by the fact that the port is closed.
;;
(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?unsafe-who)
			  (define (?who x)
			    (and (port? x) (?unsafe-who x))))
			 )))
  (define-predicate binary-port?	$binary-port?)
  (define-predicate textual-port?	$textual-port?)
  (define-predicate input-only-port?	$input-only-port?)
  (define-predicate output-only-port?	$output-only-port?)
  (define-predicate input/output-port?	$input/output-port?)
  #| end of LET-SYNTAX |# )

(define (input-port? x)
  ;;Defined by R6RS.  Return #t if X is  an input port or a combined input and output
  ;;port.
  (and (port? x)
       (or ($input-port? x)
	   ($input/output-port? x))))

(define (output-port? x)
  ;;Defined by R6RS.  Return #t if X is an output port or a combined input and output
  ;;port.
  (and (port? x)
       (or ($output-port? x)
	   ($input/output-port? x))))

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?i/o-pred ?b/t-pred)
			  (define (?who obj)
			    (and (?i/o-pred obj)
				 (?b/t-pred obj))))
			 )))
  (define-predicate binary-input-port?		input-port?		$binary-port?)
  (define-predicate textual-input-port?        	input-port?		$textual-port?)
  (define-predicate binary-output-port?        	output-port?		$binary-port?)
  (define-predicate textual-output-port?       	output-port?		$textual-port?)
  (define-predicate binary-input/output-port?  	input/output-port?	$binary-port?)
  (define-predicate textual-input/output-port? 	input/output-port?	$textual-port?)
  #| end of LET-SYNTAX |# )

;;; --------------------------------------------------------------------

(define (binary-input-only-port? x)
  (and (port? x)
       ($input-port?  x)
       ($binary-port? x)))

(define (binary-output-only-port? x)
  (and (port? x)
       ($output-port? x)
       ($binary-port? x)))

(define (textual-input-only-port? x)
  (and (port? x)
       ($input-port?  x)
       ($textual-port? x)))

(define (textual-output-only-port? x)
  (and (port? x)
       ($output-port? x)
       ($textual-port? x)))


;;;; open and closed port predicates

(define (open-binary-input-or-output-port? obj)
  (and (open-binary-port? obj)
       (not ($input/output-port? obj))))

(define* (port-closed? {port port?})
  ;;Defined by Ikarus.  Return true if PORT has already been closed.
  ;;
  ($port-closed? port))

(define ($port-closed? port)
  (with-port (port)
    ($fx= ($fxand port.attributes CLOSED-PORT-TAG) CLOSED-PORT-TAG)))

(define (closed-port? obj)
  ;;Defined by Vicare.  Return true if OBJ is a port and it is closed.
  ;;
  (and (port? obj)
       ($port-closed? obj)))

(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?pred ...)
			  (define (?who obj)
			    (and (?pred obj) ...
				 (not ($port-closed? obj)))))
			 )))
  (define-predicate open-port?				port?)
  ;;
  (define-predicate open-input-port?			input-port?)
  (define-predicate open-output-port?			output-port?)
  (define-predicate open-input/output-port?		input/output-port?)
  ;;
  (define-predicate open-textual-port?			textual-port?)
  (define-predicate open-binary-port?			binary-port?)
  ;;
  (define-predicate open-binary-input-port?		input-port?		$binary-port?)
  (define-predicate open-binary-output-port?		output-port?		$binary-port?)
  (define-predicate open-binary-input/output-port?	input/output-port?	$binary-port?)
  ;;
  (define-predicate open-textual-input-port?		input-port?		$textual-port?)
  (define-predicate open-textual-output-port?		output-port?		$textual-port?)
  (define-predicate open-textual-input/output-port?	input/output-port?	$textual-port?)
  #| end of LET-SYNTAX |# )


;;;; miscellaneous predicates

;;The following predicates have  to be used after the argument  has been validated as
;;port value.  The following predicates are *not*  affected by the fact that the port
;;is closed.
;;
(let-syntax
    ((define-predicate (syntax-rules ()
			 ((_ ?who ?bits)
			  (define (?who x)
			    ($fx= ($fxand ($port-tag x) ?bits) ?bits)))
			 )))
  (define-predicate $port-buffer-mode-none?	BUFFER-MODE-NONE-TAG)
  (define-predicate $port-buffer-mode-line?	BUFFER-MODE-LINE-TAG)
  ;;
  (define-predicate $guarded-port?		GUARDED-PORT-TAG)
  (define-predicate $port-with-extraction?	PORT-WITH-EXTRACTION-TAG)
  (define-predicate $port-with-fd-device?	PORT-WITH-FD-DEVICE)
  #| end of LET-SYNTAX |# )

(define-syntax-rule ($last-port-operation-was-input? port)
  ;;True if PORT is  input or PORT is input/output and the  last operation was input;
  ;;in other words: the buffer may contain input bytes.
  ;;
  ($input-port? port))

(define-syntax-rule ($last-port-operation-was-output? port)
  ;;True if PORT is output or PORT is input/output and the last operation was output;
  ;;in other words: the buffer may contain output bytes.
  ;;
  ($output-port? port))


;;;; done

;;; end of file
;; Local Variables:
;; mode: vicare
;; End:
