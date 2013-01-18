#!r6rs
(import (vicare))

(define $number-of-seconds-in-one-day (* 60 60 24))
(define $tai-epoch-in-jd 4881175/2)

(define (read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (exact jd) $tai-epoch-in-jd) $number-of-seconds-in-one-day))
  (define (convert-sec sec)
    (exact sec))
  (let ((port  (open-input-file filename))
        (table '()))
    (let loop ((line (get-line port)))
      (unless (eof-object? line)
        (let ((port (open-string-input-port (string-append "(" line ")"))))
          (let* ((data (read   port))
                 (year (car    data))
                 (jd   (cadddr (cdr data)))
                 (secs (cadddr (cdddr data))))
            (when (>= year 1972)
              (set! table (cons (cons (convert-jd jd) (convert-sec secs))
table)))))
        (loop (get-line port))))
    table))

(define $leap-second-table
  (read-tai-utc-data (cadr (command-line))))
(pretty-print $leap-second-table)
(flush-output-port (current-output-port))

;;; end of file
