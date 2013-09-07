;;;
;;;Part of: Vicare Scheme
;;;Contents: tests for kmp library
;;;Date: Thu Jun 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



#!r6rs
(import (vicare)
  (vicare checks)
  (vicare containers strings)
  (vicare containers vectors)
  (vicare containers knuth-morris-pratt))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare containers: knuth-morris-pratt library\n")


(parameterise ((check-test-name 'string))

  (let ()

    (define (return-match-past text pattern)
      (let* ((text-past		(string-length text))
	     (pattern-start	0)
	     (pattern-past	(string-length pattern))
	     (rv		(%kmp-make-restart-vector
				 char=? string-ref
				 pattern pattern-start pattern-past)))
	(let loop ((ti 0)
		   (pi pattern-start))
	  (or (and (= pi pattern-past) ti) ; found
	      (and (not (= ti text-past))  ; not found
		   (loop (+ 1 ti)
			 (%kmp-step char=? string-ref rv
				    (string-ref text ti)
				    pi pattern pattern-start)))))))

;;; --------------------------------------------------------------------

    (check
	(let* ((str1 "") (beg1 0) (end1 (string-length str1))
	       (str2 "hello") (beg2 0) (end2 (string-length str2)))
	  (%kmp-search char=? string-ref str1 beg1 end1 str2 beg2 end2))
      => #f)

    (check
	(let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	       (str2 "hello") (beg2 0) (end2 (string-length str2)))
	  (%kmp-search char=? string-ref str1 beg1 end1 str2 beg2 end2))
      => 5)

    (check
	(let* ((str1 "ciao h he hel hell hello salut") (beg1 0) (end1 (string-length str1))
	       ;;     0123456789012345678901234567890
	       ;;     0         1         2         3
	       (str2 "hello") (beg2 0) (end2 (string-length str2)))
	  (%kmp-search char=? string-ref str1 beg1 end1 str2 beg2 end2))
      => 19)

;;; --------------------------------------------------------------------

    (check
	(let* ((text "ciao hello salut") (pattern "hello"))
	  ;;          01234567890123456
	  ;;          0         1
	  (return-match-past text pattern))
      => 10)

    (check
	(let* ((text "ciao hello salut") (pattern "hola"))
	  ;;          01234567890123456
	  ;;          0         1
	  (return-match-past text pattern))
      => #f)

    (check
	(let* ((text "ciao hell salut") (pattern "hola"))
	  ;;          01234567890123456
	  ;;          0         1
	  (return-match-past text pattern))
      => #f)

    )

;;; --------------------------------------------------------------------

  (let ()

    (define (return-match-past end-of-text? get-next-char pattern pattern-start pattern-past)
      (let ((rv (%kmp-make-restart-vector char=? string-ref
					  pattern pattern-start pattern-past)))
	(let loop ((ti 0)
		   (pi pattern-start))
	  (or (and (= pi pattern-past) ti) ; found
	      (and (not (end-of-text?))	   ; not found
		   (loop (+ 1 ti)
			 (%kmp-step char=? string-ref rv
				    (get-next-char)
				    pi pattern pattern-start)))))))

    (check
	(let* ((text "ciao hello salut")
	       ;;     01234567890123456
	       ;;     0         1
	       (pattern "hello")
	       (ti 0)
	       (end-of-text?	(lambda ()
				  (= ti (string-length text))))
	       (get-next-char	(lambda ()
				  (begin0
				      (string-ref text ti)
				    (set! ti (+ 1 ti))))))
	  (return-match-past end-of-text? get-next-char
			     pattern 0 (string-length pattern)))
      => 10)

    (check
	(let* ((text "ciao hell salut")
	       ;;     0123456789012345
	       ;;     0         1
	       (pattern "hello")
	       (ti 0)
	       (end-of-text?	(lambda ()
				  (= ti (string-length text))))
	       (get-next-char	(lambda ()
				  (let ((ch (string-ref text ti)))
				    (set! ti (+ 1 ti))
				    ch))))
	  (return-match-past end-of-text? get-next-char
			     pattern 0 (string-length pattern)))
      => #f)

    (check
	(let* ((text "ciao hello salut")
	       ;;     01234567890123456
	       ;;     0         1
	       (pattern "salut")
	       (ti 0)
	       (end-of-text?	(lambda ()
				  (= ti (string-length text))))
	       (get-next-char	(lambda ()
				  (let ((ch (string-ref text ti)))
				    (set! ti (+ 1 ti))
				    ch))))
	  (return-match-past end-of-text? get-next-char
			     pattern 0 (string-length pattern)))
      => 16)

    (check
	(let* ((text "ciao hello salut")
	       ;;     01234567890123456
	       ;;     0         1
	       (pattern "salut")
	       (port	(open-string-input-port text))
	       (end-of-text?	(lambda ()
				  (eof-object? (peek-char port))))
	       (get-next-char	(lambda ()
				  (read-char port))))
	  (return-match-past end-of-text? get-next-char
			     pattern 0 (string-length pattern)))
      => 16)

    (check
	(let* ((text "ciao hello salut")
	       ;;     01234567890123456
	       ;;     0         1
	       (pattern "hola")
	       (port	(open-string-input-port text))
	       (end-of-text?	(lambda ()
				  (eof-object? (peek-char port))))
	       (get-next-char	(lambda ()
				  (read-char port))))
	  (return-match-past end-of-text? get-next-char
			     pattern 0 (string-length pattern)))
      => #f)
    )

;;; --------------------------------------------------------------------

  (check
      (let* ((text		"ciao hello salut")
	     ;;                  01234567890123456
	     ;;			 0         1
	     (text-start	0)
	     (text-past		(string-length text))
	     (pattern		"hello")
	     (pattern-start	0)
	     (pattern-past	(string-length pattern))
	     (restart-vector	(%kmp-make-restart-vector char=? string-ref
							  pattern pattern-start pattern-past)))
	(let ((i (%kmp-partial-search char=? string-ref restart-vector pattern-start
				      text text-start text-past
				      pattern pattern-start)))
	  (or (<= 0 i) ;; not found
	      (- i)))) ;; found, return match past index
    => 10)

  (check
      (let* ((strings '("ciao h " "he hel h" "ell hel" "lo salut"))
  	     (end-of-data?	(lambda ()
  				  (null? strings)))
  	     (get-next-chunk	(lambda ()
  				  (begin0
				      (car strings)
  				    (set! strings (cdr strings))))))
  	(let* ((pattern "hello")
  	       (pattern-start 0)
  	       (pattern-past (string-length pattern))
  	       (restart-vector (%kmp-make-restart-vector char=? string-ref
							 pattern pattern-start pattern-past)))
  	  (let loop ((pi 0))
  	    (and (not (end-of-data?)) ;; not found
  		 (let* ((buf (get-next-chunk))
  			(pi  (%kmp-partial-search
  			      char=? string-ref restart-vector pi
  			      buf 0 (string-length buf)
  			      pattern pattern-start)))
  		   (if (< pi 0)
  		       (cons buf (- pi)) ;; found
  		     (loop pi)))))))
    => '("lo salut" . 2))

  (check
      (let* ((strings '("ciao h " "he hel h" "ell hello" " salut"))
  	     (end-of-data?	(lambda ()
  				  (null? strings)))
  	     (get-next-chunk	(lambda ()
  				  (begin0
				      (car strings)
  				    (set! strings (cdr strings))))))
  	(let* ((pattern "hello")
  	       (pattern-start 0)
  	       (pattern-past (string-length pattern))
  	       (restart-vector (%kmp-make-restart-vector char=? string-ref
							 pattern pattern-start pattern-past)))
  	  (let loop ((pi 0))
  	    (and (not (end-of-data?)) ;; not found
  		 (let* ((buf (get-next-chunk))
  			(pi  (%kmp-partial-search
  			      char=? string-ref restart-vector pi
  			      buf 0 (string-length buf)
  			      pattern pattern-start)))
  		   (if (< pi 0)
  		       (cons buf (- pi)) ;; found
  		     (loop pi)))))))
    => '("ell hello" . 9))

  )


(parameterise ((check-test-name 'vector))

  (check
      (let* ((text '#()) (pattern '#(0 1 2)))
	(%kmp-search = vector-ref
		     text 0 (vector-length text)
		     pattern 0 (vector-length pattern)))
    => #f)

  (check
      (let* ((text '#(0 1 2 3 4 5 6 7 8 9)) (pattern '#(3 4 5)))
	(%kmp-search = vector-ref
		     text 0 (vector-length text)
		     pattern 0 (vector-length pattern)))
    => 3)

  (check
      (let* ((text '#(0 1 2 3 10 3 4 10 3 4 5 7 8 9)) (pattern '#(3 4 5)))
	(%kmp-search = vector-ref
		     text 0 (vector-length text)
		     pattern 0 (vector-length pattern)))
    => 8)

  )


;;;; done

(check-report)

;;; end of file
