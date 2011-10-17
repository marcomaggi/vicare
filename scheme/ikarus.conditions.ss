;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus conditions)
  (export condition? simple-conditions condition-predicate
          condition condition-accessor print-condition

          ;;; too much junk
          make-message-condition message-condition?
          condition-message make-warning warning?
          make-serious-condition serious-condition? make-error
          error? make-violation violation? make-assertion-violation
          assertion-violation? make-irritants-condition
          irritants-condition? condition-irritants
          make-who-condition who-condition? condition-who
          make-non-continuable-violation non-continuable-violation?
          make-implementation-restriction-violation
          implementation-restriction-violation?
          make-lexical-violation lexical-violation?
          make-syntax-violation syntax-violation?
          syntax-violation-form syntax-violation-subform
          make-undefined-violation undefined-violation?
          make-i/o-error i/o-error? make-i/o-read-error
          i/o-read-error? make-i/o-write-error i/o-write-error?
          make-i/o-invalid-position-error
          i/o-invalid-position-error? i/o-error-position
          make-i/o-filename-error i/o-filename-error?
          i/o-error-filename make-i/o-file-protection-error
          i/o-file-protection-error? make-i/o-file-is-read-only-error
          i/o-file-is-read-only-error?
          make-i/o-file-already-exists-error
          i/o-file-already-exists-error?
          make-i/o-file-does-not-exist-error
          i/o-file-does-not-exist-error? make-i/o-port-error
          i/o-port-error? i/o-error-port make-i/o-decoding-error
          i/o-decoding-error? make-i/o-encoding-error
          i/o-encoding-error? i/o-encoding-error-char
          no-infinities-violation? make-no-infinities-violation
          no-nans-violation? make-no-nans-violation
          interrupted-condition? make-interrupted-condition
          make-source-position-condition source-position-condition?
          source-position-port-id
	  source-position-byte source-position-character
	  source-position-line source-position-column

          &condition-rtd &condition-rcd &message-rtd &message-rcd
          &warning-rtd &warning-rcd &serious-rtd &serious-rcd
          &error-rtd &error-rcd &violation-rtd &violation-rcd
          &assertion-rtd &assertion-rcd &irritants-rtd
          &irritants-rcd &who-rtd &who-rcd &non-continuable-rtd
          &non-continuable-rcd &implementation-restriction-rtd
          &implementation-restriction-rcd &lexical-rtd &lexical-rcd
          &syntax-rtd &syntax-rcd &undefined-rtd &undefined-rcd
          &i/o-rtd &i/o-rcd &i/o-read-rtd &i/o-read-rcd
          &i/o-write-rtd &i/o-write-rcd &i/o-invalid-position-rtd
          &i/o-invalid-position-rcd &i/o-filename-rtd
          &i/o-filename-rcd &i/o-file-protection-rtd
          &i/o-file-protection-rcd &i/o-file-is-read-only-rtd
          &i/o-file-is-read-only-rcd &i/o-file-already-exists-rtd
          &i/o-file-already-exists-rcd &i/o-file-does-not-exist-rtd
          &i/o-file-does-not-exist-rcd &i/o-port-rtd &i/o-port-rcd
          &i/o-decoding-rtd &i/o-decoding-rcd &i/o-encoding-rtd
          &i/o-encoding-rcd &no-infinities-rtd &no-infinities-rcd
          &no-nans-rtd &no-nans-rcd
          &interrupted-rtd &interrupted-rcd
          &source-position-rtd &source-position-rcd

	  &i/o-eagain make-i/o-eagain i/o-eagain-error?
	  &i/o-eagain-rtd &i/o-eagain-rcd)
  (import
    (rnrs records inspection)
    (rnrs records procedural)
    (only (rnrs) record-type-descriptor record-constructor-descriptor record-predicate)
    (only (ikarus records procedural) rtd? rtd-subtype?)
    (except (ikarus) define-condition-type condition? simple-conditions
          condition condition-predicate condition-accessor
          print-condition

          ;;; more junk

          &condition &message &warning &serious &error &violation
          &assertion &irritants &who &non-continuable
          &implementation-restriction &lexical &syntax &undefined
          &i/o &i/o-read &i/o-write &i/o-invalid-position
          &i/o-filename &i/o-file-protection &i/o-file-is-read-only
          &i/o-file-already-exists &i/o-file-does-not-exist
          &i/o-port &i/o-decoding &i/o-encoding &no-infinities
          &no-nans

          make-message-condition message-condition?
          condition-message make-warning warning?
          make-serious-condition serious-condition? make-error
          error? make-violation violation? make-assertion-violation
          assertion-violation? make-irritants-condition
          irritants-condition? condition-irritants
          make-who-condition who-condition? condition-who
          make-non-continuable-violation non-continuable-violation?
          make-implementation-restriction-violation
          implementation-restriction-violation?
          make-lexical-violation lexical-violation?
          make-syntax-violation syntax-violation?
          syntax-violation-form syntax-violation-subform
          make-undefined-violation undefined-violation?
          make-i/o-error i/o-error? make-i/o-read-error
          i/o-read-error? make-i/o-write-error i/o-write-error?
          make-i/o-invalid-position-error
          i/o-invalid-position-error? i/o-error-position
          make-i/o-filename-error i/o-filename-error?
          i/o-error-filename make-i/o-file-protection-error
          i/o-file-protection-error? make-i/o-file-is-read-only-error
          i/o-file-is-read-only-error?
          make-i/o-file-already-exists-error
          i/o-file-already-exists-error?
          make-i/o-file-does-not-exist-error
          i/o-file-does-not-exist-error? make-i/o-port-error
          i/o-port-error? i/o-error-port make-i/o-decoding-error
          i/o-decoding-error? make-i/o-encoding-error
          i/o-encoding-error? i/o-encoding-error-char
          no-infinities-violation? make-no-infinities-violation
          no-nans-violation? make-no-nans-violation

	  &i/o-eagain make-i/o-eagain i/o-eagain-error?
	  &i/o-eagain-rtd &i/o-eagain-rcd

          interrupted-condition? make-interrupted-condition
          make-source-position-condition source-position-condition?
          source-position-port-id
	  source-position-byte source-position-character
	  source-position-line source-position-column
	  ))

  (define-record-type &condition
    (nongenerative))

  (define &condition-rtd (record-type-descriptor &condition))
  (define &condition-rcd (record-constructor-descriptor &condition))

  (define-record-type compound-condition
    (nongenerative)
    (fields (immutable components))
    (sealed #t)
    (opaque #f))

  (define (condition? x)
    (or (&condition? x)
        (compound-condition? x)))

  (define condition
    (case-lambda
      [() (make-compound-condition '())]
      [(x)
       (if (condition? x)
           x
           (die 'condition "not a condition type" x))]
      [x*
       (let ([ls
              (let f ([x* x*])
                (cond
                  [(null? x*) '()]
                  [(&condition? (car x*))
                   (cons (car x*) (f (cdr x*)))]
                  [(compound-condition? (car x*))
                   (append (simple-conditions (car x*)) (f (cdr x*)))]
                  [else (die 'condition "not a condition" (car x*))]))])
         (cond
           [(null? ls) (make-compound-condition '())]
           [(null? (cdr ls)) (car ls)]
           [else (make-compound-condition ls)]))]))

  (define (simple-conditions x)
    (cond
      [(compound-condition? x) (compound-condition-components x)]
      [(&condition? x) (list x)]
      [else (die 'simple-conditions "not a condition" x)]))

  (define (condition-predicate rtd)
    (unless (rtd? rtd)
      (die 'condition-predicate "not a record type descriptor" rtd))
    (unless (rtd-subtype? rtd (record-type-descriptor &condition))
      (die 'condition-predicate "not a descendant of &condition" rtd))
    (let ([p? (record-predicate rtd)])
      (lambda (x)
        (or (p? x)
            (and (compound-condition? x)
                 (let f ([ls (compound-condition-components x)])
                   (and (pair? ls)
                        (or (p? (car ls))
                            (f (cdr ls))))))))))

  (define (condition-accessor rtd proc)
    (unless (rtd? rtd)
      (die 'condition-accessor "not a record type descriptor" rtd))
    (unless (procedure? proc)
      (die 'condition-accessor "not a procedure" proc))
    (unless (rtd-subtype? rtd (record-type-descriptor &condition))
      (die 'condition-accessor "not a descendant of &condition" rtd))
    (let ([p? (record-predicate rtd)])
      (lambda (x)
        (cond
          [(p? x) (proc x)]
          [(compound-condition? x)
           (let f ([ls (compound-condition-components x)])
             (cond
               [(pair? ls)
                (if (p? (car ls))
                    (proc (car ls))
                    (f (cdr ls)))]
               [else
                (die 'condition-accessor "not a condition of correct type" x rtd)]))]
          [else
           (die 'condition-accessor "not a condition of correct type" x rtd)]))))

  (define-syntax define-condition-type
    (lambda (x)
      (define (mkname name suffix)
        (datum->syntax name
           (string->symbol
             (string-append
               (symbol->string (syntax->datum name))
               suffix))))
      (syntax-case x ()
        [(ctxt name super constructor predicate (field* accessor*) ...)
         (and (identifier? #'name)
              (identifier? #'super)
              (identifier? #'constructor)
              (identifier? #'predicate)
              (andmap identifier? #'(field* ...))
              (andmap identifier? #'(accessor* ...)))
         (with-syntax ([(aux-accessor* ...) (generate-temporaries #'(accessor* ...))]
                       [rtd (mkname #'name "-rtd")]
                       [rcd (mkname #'name "-rcd")])
            #'(begin
               (define-record-type (name constructor p?)
                  (parent super)
                  (fields (immutable field* aux-accessor*) ...)
                  (nongenerative)
                  (sealed #f) (opaque #f))
               (define predicate (condition-predicate (record-type-descriptor name)))
               (define accessor* (condition-accessor (record-type-descriptor name) aux-accessor*))
               ...
               (define rtd (record-type-descriptor name))
               (define rcd (record-constructor-descriptor name))))])))

  (define-condition-type &message &condition
    make-message-condition message-condition?
    (message condition-message))

  (define-condition-type &warning &condition
    make-warning warning?)

  (define-condition-type &serious &condition
    make-serious-condition serious-condition?)

  (define-condition-type &error &serious
    make-error error?)

  (define-condition-type &violation &serious
    make-violation violation?)

  (define-condition-type &assertion &violation
    make-assertion-violation assertion-violation?)

  (define-condition-type &irritants &condition
    make-irritants-condition irritants-condition?
    (irritants condition-irritants))

  (define-condition-type &who &condition
    make-who-condition who-condition?
    (who condition-who))

  (define-condition-type &non-continuable &violation
    make-non-continuable-violation non-continuable-violation?)

  (define-condition-type &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)

  (define-condition-type &lexical &violation
    make-lexical-violation lexical-violation?)

  (define-condition-type &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))

  (define-condition-type &undefined &violation
    make-undefined-violation undefined-violation?)

  (define-condition-type &i/o &error
    make-i/o-error i/o-error?)

  (define-condition-type &i/o-read &i/o
    make-i/o-read-error i/o-read-error?)

  (define-condition-type &i/o-write &i/o
    make-i/o-write-error i/o-write-error?)

  (define-condition-type &i/o-invalid-position &i/o
    make-i/o-invalid-position-error i/o-invalid-position-error?
    (position i/o-error-position))

  (define-condition-type &i/o-filename &i/o
    make-i/o-filename-error i/o-filename-error?
    (filename i/o-error-filename))

  (define-condition-type &i/o-file-protection &i/o-filename
    make-i/o-file-protection-error i/o-file-protection-error?)

  (define-condition-type &i/o-file-is-read-only &i/o-file-protection
    make-i/o-file-is-read-only-error i/o-file-is-read-only-error?)

  (define-condition-type &i/o-file-already-exists &i/o-filename
    make-i/o-file-already-exists-error i/o-file-already-exists-error?)

  (define-condition-type &i/o-file-does-not-exist &i/o-filename
    make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?)

  (define-condition-type &i/o-port &i/o
    make-i/o-port-error i/o-port-error?
    (port i/o-error-port))

  (define-condition-type &i/o-decoding &i/o-port
    make-i/o-decoding-error i/o-decoding-error?)

  (define-condition-type &i/o-encoding &i/o-port
    make-i/o-encoding-error i/o-encoding-error?
    (char i/o-encoding-error-char))

  (define-condition-type &no-infinities &implementation-restriction
    make-no-infinities-violation no-infinities-violation?)

  (define-condition-type &no-nans &implementation-restriction
    make-no-nans-violation no-nans-violation?)

  ;;; ikarus-specific conditions
  (define-condition-type &interrupted &serious
    make-interrupted-condition interrupted-condition?)

  (define-condition-type &source-position &condition
    make-source-position-condition source-position-condition?
    (port-id	source-position-port-id)
    (byte	source-position-byte)
    (character	source-position-character)
    (line	source-position-line)
    (column	source-position-column))

  ;;; Vicare specific conditions
  (define-condition-type &i/o-eagain &i/o
    make-i/o-eagain i/o-eagain-error?)

  (define print-condition
    (let ()
      (define (print-simple-condition x p)
        (let* ([rtd (record-rtd x)]
               [rf (let l ([rtd rtd] [accum '()])
                     (if rtd
                       (l (record-type-parent rtd)
                          (cons
                            (cons rtd (record-type-field-names rtd))
                            accum))
                       (remp (lambda (a) (zero? (vector-length (cdr a))))
                             accum)))]
               [rf-len (apply + (map vector-length
                                     (map cdr rf)))])
          (let ([name (record-type-name rtd)])
            (display name p))
          (case rf-len
            [(0) (newline p)]
            [(1)
             (display ": " p)
             (write ((record-accessor (caar rf) 0) x) p)
             (newline p)]
            [else
             (display ":\n" p)
             (for-each
               (lambda (a)
                 (let f ([i 0] [rtd (car a)] [v (cdr a)])
                   (unless (= i (vector-length v))
                     (display "       " p)
                     (display (vector-ref v i) p)
                     (display ": " p)
                     (write ((record-accessor rtd i) x) p)
                     (newline p)
                     (f (+ i 1) rtd v))))
               rf)])))
      (define (print-condition x p)
        (cond
          [(condition? x)
           (let ([ls (simple-conditions x)])
             (if (null? ls)
                 (display "Condition object with no further information\n" p)
	       (begin
		 (display " Condition components:\n" p)
		 (let f ([ls ls] [i 1])
		   (unless (null? ls)
		     (display "   " p)
		     (display i p)
		     (display ". " p)
		     (print-simple-condition (car ls) p)
		     (f (cdr ls) (+ i 1)))))))
	   (flush-output-port p)]
          [else
           (display " Non-condition object: " p)
           (write x p)
           (newline p)
	   (flush-output-port p)]))
      (case-lambda
        [(x)
         (print-condition x (console-output-port))]
        [(x port)
         (if (output-port? port)
             (print-condition x port)
             (die 'print-condition "not an output port" port))])))


  )

