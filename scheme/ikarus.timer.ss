;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; Modified by Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ikarus timers)
  (export
    time-it verbose-timer
    time-and-gather

    stats?
    stats-user-secs		stats-user-usecs
    stats-sys-secs		stats-sys-usecs
    stats-real-secs		stats-real-usecs
    stats-collection-id
    stats-gc-user-secs		stats-gc-user-usecs
    stats-gc-sys-secs		stats-gc-sys-usecs
    stats-gc-real-secs		stats-gc-real-usecs
    stats-bytes-minor		stats-bytes-major
    )
  (import (except (ikarus)
		  time-it verbose-timer

		  ;;To  be uncommented  when the  boot image  is rebuilt
		  ;;including these bindings, too.
		  ;;
		  ;; time-and-gather
		  ;; stats?
		  ;; stats-user-secs		stats-user-usecs
		  ;; stats-sys-secs		stats-sys-usecs
		  ;; stats-real-secs		stats-real-usecs
		  ;; stats-collection-id
		  ;; stats-gc-user-secs		stats-gc-user-usecs
		  ;; stats-gc-sys-secs		stats-gc-sys-usecs
		  ;; stats-gc-real-secs		stats-gc-real-usecs
		  ;; stats-bytes-minor		stats-bytes-major
		  ))

  (define-record-type stats
    ;;Do  not change  the  order of  the  fields!!!  It  must match  the
    ;;implementation  of  "ikrt_stats_now()" in  "src/ikarus-runtime.c".
    ;;Originally there was this structure:
    ;;
    ;; (define-struct stats
    ;;   (user-secs		user-usecs
    ;;    sys-secs		sys-usecs
    ;;    real-secs		real-usecs
    ;;    collection-id
    ;;    gc-user-secs		gc-user-usecs
    ;;    gc-sys-secs		gc-sys-usecs
    ;;    gc-real-secs		gc-real-usecs
    ;;    bytes-minor		bytes-major))
    ;;
    (fields user-secs		user-usecs
	    sys-secs		sys-usecs
	    real-secs		real-usecs
	    collection-id
	    gc-user-secs	gc-user-usecs
	    gc-sys-secs		gc-sys-usecs
	    gc-real-secs	gc-real-usecs
	    bytes-minor		bytes-major)
    (protocol (lambda (maker)
		(lambda ()
		  (maker #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))))
    (nongenerative vicare:ikarus.timer:stats)
    (opaque #t)
    (sealed #t))

  (define (set-stats! t)
    (foreign-call "ikrt_stats_now" t))

  (define verbose-timer (make-parameter #f))

  (define (print-stats message t1 t0)
    (define (print-time msg msecs gc-msecs)
      (fprintf
        (console-error-port)
        "    ~a ms elapsed ~a time, including ~a ms collecting\n" msecs msg
              gc-msecs))
    (define (msecs s1 s0 u1 u0)
      (+ (* (- s1 s0) 1000) (quotient (- u1 u0) 1000)))
    (if message
        (fprintf (console-error-port) "running stats for ~a:\n" message)
        (fprintf (console-error-port) "running stats:\n"))
    (let ([collections
           (fx- (stats-collection-id t1) (stats-collection-id t0))])
      (case collections
        [(0) (display "    no collections\n" (console-error-port))]
        [(1) (display "    1 collection\n" (console-error-port))]
        [else (fprintf (console-error-port) "    ~a collections\n" collections)]))
    (print-time "cpu"
       (+ (msecs (stats-user-secs t1)  (stats-user-secs t0)
                 (stats-user-usecs t1) (stats-user-usecs t0))
          (msecs (stats-sys-secs t1)  (stats-sys-secs t0)
                 (stats-sys-usecs t1) (stats-sys-usecs t0)))
       (+ (msecs (stats-gc-user-secs t1)  (stats-gc-user-secs t0)
                 (stats-gc-user-usecs t1) (stats-gc-user-usecs t0))
          (msecs (stats-gc-sys-secs t1)  (stats-gc-sys-secs t0)
                 (stats-gc-sys-usecs t1) (stats-gc-sys-usecs t0))))
    (print-time "real"
        (msecs (stats-real-secs t1) (stats-real-secs t0)
               (stats-real-usecs t1) (stats-real-usecs t0))
        (msecs (stats-gc-real-secs t1) (stats-gc-real-secs t0)
               (stats-gc-real-usecs t1) (stats-gc-real-usecs t0)))
    (when (verbose-timer)
      (print-time "user"
         (msecs (stats-user-secs t1)  (stats-user-secs t0)
                (stats-user-usecs t1) (stats-user-usecs t0))
         (msecs (stats-gc-user-secs t1)  (stats-gc-user-secs t0)
                (stats-gc-user-usecs t1) (stats-gc-user-usecs t0)))
      (print-time "sys"
         (msecs (stats-sys-secs t1)  (stats-sys-secs t0)
                (stats-sys-usecs t1) (stats-sys-usecs t0))
         (msecs (stats-gc-sys-secs t1)  (stats-gc-sys-secs t0)
                (stats-gc-sys-usecs t1) (stats-gc-sys-usecs t0))))
    (fprintf (console-error-port) "    ~a bytes allocated\n"
        (diff-bytes
          (stats-bytes-minor t0)
          (stats-bytes-major t0)
          (stats-bytes-minor t1)
          (stats-bytes-major t1))))

  (define time-it
    (case-lambda
      [(proc)
       (time-it #f proc)]
      [(message proc)
       (unless (procedure? proc)
         (die 'time-it "not a procedure" proc))
       (let* ([t0 (make-stats)]
              [t1 (make-stats)])
         (define k
           (case-lambda
             [(v)
              (set-stats! t1)
              (print-stats message t1 t0)
              v]
             [v*
              (set-stats! t1)
              (print-stats message t1 t0)
              (apply values v*)]))
         (set-stats! t0)
         (call-with-values proc k))]))

  (define (time-and-gather gather proc)
    (define who 'time-and-gather)
    (unless (procedure? gather)
      (assertion-violation who "expected procedure as first argument" gather))
    (unless (procedure? proc)
      (assertion-violation who "expected procedure as second argument" proc))
    (let* ((t0 (make-stats))
	   (t1 (make-stats)))
      (define kont
	(case-lambda
	 ((v)
	  (set-stats! t1)
	  (gather t0 t1)
	  v)
	 (v*
	  (set-stats! t1)
	  (gather t0 t1)
	  (apply values v*))))
      (set-stats! t0)
      (call-with-values proc kont)))

  (define (diff-bytes mnr0 mjr0 mnr1 mjr1)
    (+ (fx- mnr1 mnr0) (* (fx- mjr1 mjr0) #x10000000)))

)
