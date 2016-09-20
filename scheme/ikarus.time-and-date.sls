;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(library (ikarus time-and-date)
  (export
    ;; time spans
    <time>
    <time>-rtd		<time>-rcd
    <time>-equality-predicate
    <time>-comparison-procedure
    <time>-hash-function
    make-time		time?
    time-seconds	time-nanoseconds
    time-addition	time-difference
    time=?
    time<?		time<=?
    time>?		time>=?

    ;; epoch time
    <epoch-time>
    <epoch-time>-rtd	<epoch-time>-rcd
    <epoch-time>-equality-predicate
    <epoch-time>-comparison-procedure
    <epoch-time>-hash-function
    epoch-time?
    current-time
    epoch-time-gmt-offset

    ;; date functions
    date-string)
  (import (except (vicare)
		  ;; time spans
		  <time>
		  <time>-rtd		<time>-rcd
		  make-time		time?
		  time-seconds		time-nanoseconds
		  time-addition		time-difference
		  time=?
		  time<?		time<=?
		  time>?		time>=?

		  ;; epoch time
		  <epoch-time>
		  <epoch-time>-rtd	<epoch-time>-rcd
		  current-time
		  time-gmt-offset

		  ;; date functions
		  date-string

		  ;;FIXME  To be  removed at  the next  boot image  rotation.  (Marco
		  ;;Maggi; Mon Sep 19, 2016)
		  compar-fixnum)
    (ikarus records syntactic)
    ;;FIXME To be fixed  at the next boot image rotation.  (Marco  Maggi; Mon Sep 19,
    ;;2016)
    (only (ikarus.comparison-procedures) #;(vicare system comparison-procedures)
	  compar-fixnum)
    #| end of IMPORT |# )


;;;; base time spans representation

(define-core-record-type <time>
  (nongenerative vicare:core-type:<time>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (fields
    (immutable megasecs)
		;An <exact-integer> representing the megaseconds.
    (immutable secs)
		;A <fixnum> representing the seconds.
    (immutable nanoseconds)
		;A <fixnum> representing the nanoseconds.
    #| end of FIELDS |# )

  (protocol
    (lambda (make-record)
      (define (normalise-triplet megasecs secs nanosecs)
	;;Normalise a triplet: megaseconds, seconds, nanoseconds.
	;;
	(receive (nanos.secs nanos.nanos)
	    (div-and-mod nanosecs #e1e9)
	  (let ((secs (+ secs nanos.secs)))
	    (receive (secs.megas secs.secs)
		(div-and-mod secs #e1e6)
	      (let ((megasecs (+ megasecs secs.megas)))
		(values megasecs secs.secs nanos.nanos))))))

      (case-define* make-<time>
	(({secs exact-integer?} {nanosecs exact-integer?})
	 (make-<time> 0 secs nanosecs))
	(({megasecs exact-integer?} {secs exact-integer?} {nanosecs exact-integer?})
	 (receive (megasecs secs nanosecs)
	     (normalise-triplet megasecs secs nanosecs)
	   (make-record megasecs secs nanosecs))))

      make-<time>))

  (equality-predicate
    (lambda ()
      (lambda (T1 T2)
	(time=? T1 T2))))

  (hash-function
    (lambda ()
      (lambda (T)
	;;The megaseconds change rarely.  So we do not use them here.
	(fxior (fixnum-hash (time-secs		T))
	       (fixnum-hash (time-nanoseconds	T))))))

  (comparison-procedure
    (lambda ()
      (lambda (T1 T2)
	(let ((cmp (compar-exact-integer (time-megasecs T1) (time-megasecs T2))))
	  (if (fxzero? cmp)
	      (let ((cmp (compar-fixnum (time-secs T1) (time-secs T2))))
		(if (fxzero? cmp)
		    (compar-fixnum (time-nanoseconds T1) (time-nanoseconds T2))
		  cmp))
	    cmp)))))

  (custom-printer
    (named-lambda <time>-printer (T port subprinter)
      (display "#[<time>" port)
      (display " secs="  port) (display (time-seconds     T) port)
      (display " nsecs=" port) (display (time-nanoseconds T) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

;;; --------------------------------------------------------------------
;;; public fields accessors

(define* (time-seconds {x time?})
  (+ (* (time-megasecs x) #e1e6)
     (time-secs x)))


;;;; time operations

(define* (time-addition {time1 time?} {time2 time?})
  ;;Compute the addition  between two times: time1  - time2 and return  a time struct
  ;;representing it.
  ;;
  (make-time (+ (time-megasecs  time1) (time-megasecs  time2))
	     (+ (time-secs      time1) (time-secs      time2))
	     (+ (time-nanoseconds time1) (time-nanoseconds time2))))

;;; --------------------------------------------------------------------

(define* (time-difference {time1 time?} {time2 time?})
  ;;Compute the difference between two times: time1  - time2 and return a time struct
  ;;representing it.
  ;;
  (make-time (- (time-megasecs  time1) (time-megasecs  time2))
	     (- (time-secs      time1) (time-secs      time2))
	     (- (time-nanoseconds time1) (time-nanoseconds time2))))


;;;; time comparison

(define* (time=? {time1 time?} {time2 time?})
  ;;Return true if the time objects are equal, else return false.
  ;;
  (and (= (time-megasecs  time1) (time-megasecs  time2))
       (= (time-secs      time1) (time-secs      time2))
       (= (time-nanoseconds time1) (time-nanoseconds time2))))

;;; --------------------------------------------------------------------

(define* (time<? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is less than TIME2, else return false.
  ;;
  (or (< (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (< (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (< (time-nanoseconds time1)
		       (time-nanoseconds time2)))))))

;;; --------------------------------------------------------------------

(define* (time<=? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is less than or equal to TIME2, else return false.
  ;;
  (or (< (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (< (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (<= (time-nanoseconds time1)
			(time-nanoseconds time2)))))))

;;; --------------------------------------------------------------------

(define* (time>? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is greater than TIME2, else return false.
  ;;
  (or (> (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (> (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (> (time-nanoseconds time1)
		       (time-nanoseconds time2)))))))

;;; --------------------------------------------------------------------

(define* (time>=? {time1 time?} {time2 time?})
  ;;Return true if TIME1 is greater than or equal to TIME2, else return false.
  ;;
  (or (> (time-megasecs time1)
	 (time-megasecs time2))
      (and (= (time-megasecs time1)
	      (time-megasecs time2))
	   (or (> (time-secs time1)
		  (time-secs time2))
	       (and (= (time-secs time1)
		       (time-secs time2))
		    (>= (time-nanoseconds time1)
			(time-nanoseconds time2)))))))


;;;; time since the Epoch

(define-core-record-type <epoch-time>
  (nongenerative vicare:core-types:<epoch-time>)
  (define-type-descriptors)
  (strip-angular-parentheses)
  (parent <time>)

  (fields
    (immutable gmt-offset)
		;Fixnum representing the  GMT offset in seconds of  this time object,
		;as determined by the POSIX function "gmtime_r()".
    #| end of FIELDS |# )

  (protocol
    (lambda (make-time)
      (named-lambda make-<epoch-time> ()
	(let ((fxs (foreign-call "ikrt_current_time" (make-vector 4))))
	  ((make-time (vector-ref fxs 0)  ;megaseconds
		      (vector-ref fxs 1)  ;seconds
		      (vector-ref fxs 2)) ;nanoseconds
	   (vector-ref fxs 3))))))

  (equality-predicate
    (lambda (<time>-equality-predicate)
      (lambda (ET1 ET2)
	(and (<time>-equality-predicate ET1 ET2)
	     (fx=? (epoch-time-gmt-offset ET1)
		   (epoch-time-gmt-offset ET2))))))

  (comparison-procedure
    (lambda (<time>-comparison-procedure)
      (lambda (ET1 ET2)
	(let ((cmp (<time>-comparison-procedure ET1 ET2)))
	  (if (fxzero? cmp)
	      (compar-fixnum (epoch-time-gmt-offset ET1)
			     (epoch-time-gmt-offset ET2))
	    cmp)))))

  ;; (hash-function
  ;;   (lambda (time-hash)
  ;;     time-hash))

  (custom-printer
    (lambda (T port subprinter)
      (display "#[<epoch-time>"	port)
      (display " secs="		port) (display (time-seconds		T) port)
      (display " nsecs="	port) (display (time-nanoseconds	T) port)
      (display " gmt-offset="	port) (display (epoch-time-gmt-offset	T) port)
      (display "]" port)))

  #| end of DEFINE-RECORD-TYPE |# )

(define (current-time)
  (make-epoch-time))


;;;; date functions

(define (date-string)
  (let ((s (make-bytevector 10)))
    (foreign-call "ikrt_bvftime" s #ve(ascii "%F"))
    (utf8->string s)))


;;;; done

#| end of library |# )

;;; end of file
