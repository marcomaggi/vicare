;;;Copyright 2009 Derick Eddington.  My MIT-style license is in the file
;;;named LICENSE from  the original collection this  file is distributed
;;;with.

#!r6rs
(library (vicare language-extensions cond-expand OS-id-features)
  (export OS-id-features)
  (import (rnrs))
  (define (OS-id-features OS-id features-alist)
    ;;Given a string "OS-id" with  a format like "i686-pc-linux-gnu" and
    ;;an alist FEATURES-Alist with entries like:
    ;;
    ;;   (("linux"	linux posix)
    ;;    ("solaris"	solaris posix)
    ;;    ("darwin"	darwin posix)
    ;;    ...)
    ;;
    ;;scan "OS-id" looking  for a substring equal to the  string being a
    ;;key in the  alist; for every match collect  the associated feature
    ;;symbols.
    ;;
    ;;Return the whole list of feature symbols.
    ;;
    (define OS-id-len (string-length OS-id))
    (define (OS-id-contains? str)
      (define str-len (string-length str))
      (let loop ((i 0))
        (and (<= (+ i str-len) OS-id-len)
             (or (string-ci=? str (substring OS-id i (+ i str-len)))
                 (loop (+ 1 i))))))
    (apply append
           (map cdr (filter (lambda (x)
			      (OS-id-contains? (car x)))
		      features-alist)))))

;;; end of file
