;;;Ikarus Scheme -- A compiler for R6RS Scheme.
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;;Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under  the terms of  the GNU General  Public License version  3 as
;;;published by the Free Software Foundation.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus system time-and-date)
  (export
    current-time	time?
    time-second		time-nanosecond
    time-gmt-offset	date-string)
  (import (except (ikarus)
		  time
		  current-time		time?
		  time-second		time-nanosecond
		  time-gmt-offset	date-string)
    (vicare arguments validation))


(define-struct time
  (msecs
		;Exact integer representing the megaseconds.
   secs
		;Exact integer representing the seconds
   usecs
		;Exact integer representing the microseconds.
   ))

(define-argument-validation (time who obj)
  (time? obj)
  (assertion-violation who "expected time object as argument" obj))

(define (current-time)
  (foreign-call "ikrt_current_time" (make-time 0 0 0)))

(define (time-second x)
  (define who 'time-second)
  (with-arguments-validation (who)
      ((time	x))
    (+ (* ($time-msecs x) #e10e5)
       ($time-secs x))))

(define (time-nanosecond x)
  (define who 'time-nanosecond)
  (with-arguments-validation (who)
      ((time	x))
    (* ($time-usecs x) 1000)))

(define (time-gmt-offset x)
  (define who 'time-gmt-offset)
  (with-arguments-validation (who)
      ((time	x))
    (foreign-call "ikrt_gmt_offset" x)))

;;; --------------------------------------------------------------------

(define (date-string)
  (let ((s (make-bytevector 10)))
    (foreign-call "ikrt_bvftime" s (string->utf8 "%F"))
    (utf8->string s)))


;;;; done

)

;;; end of file
