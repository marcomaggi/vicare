;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare Scheme
;;;Contents: test implementation of POSIX functions
;;;Date: Mon Jun  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (rename (vicare) #;(ikarus)
		(parameterize	parametrise))
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare POSIX functions\n")


;;;; helpers

(define-syntax with-temporary-file
  (syntax-rules ()
    ((_ (?pathname) . ?body)
     (let ((ptn ?pathname))
       (system (string-append "echo 123 > " ptn))
       (unwind-protect
	   (begin . ?body)
	 (system (string-append "rm -f " ptn)))))))


(parametrise ((check-test-name	'errno-strings))

  (check
      (errno->string EPERM)
    => "EPERM")

  (check
      (errno->string EEXIST)
    => "EEXIST")

  #t)


(parametrise ((check-test-name	'signal-strings))

  (check
      (interprocess-signal->string SIGKILL)
    => "SIGKILL")

  (check
      (interprocess-signal->string SIGSEGV)
    => "SIGSEGV")

  #t)


(parametrise ((check-test-name	'environ))

  (check
      (getenv "CIAO-CIAO-CIAO-MARE")
    => #f)

  (check
      (let ()
	(setenv "CIAO" "" #t)
	(getenv "CIAO"))
    => "")

  (check
      (let ()
	(unsetenv "CIAO")
	(setenv "CIAO" "fusilli" #t)
	(getenv "CIAO"))
    => "fusilli")

  (check
      (let ()
	(unsetenv "CIAO")
	(setenv "CIAO" "fusilli" #t)
	(setenv "CIAO" "spaghetti" #f)
	(getenv "CIAO"))
    => "fusilli")

  (check
      (let ()
	(unsetenv "SALUT")
	(setenv "SALUT" "fusilli" #t)
	(setenv "SALUT" "fusilli" #f)
	(getenv "SALUT"))
    => "fusilli")

;;; --------------------------------------------------------------------

;;;  (pretty-print (environ))
;;;  (pretty-print (hashtable-keys (environ-table)))(newline)

  (check
      (let ((table (environ-table)))
	(hashtable-contains? table "PATH"))
    => #t)

  (check
      (hashtable-contains? (environ->table (table->environ (environ->table (environ)))) "PATH")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (begin
	(setenv "CIAO" "ciao" #t)
	(unsetenv "CIAO")
	(getenv "CIAO"))
    => #f)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (begin
  ;; 	(clearenv)
  ;; 	(putenv* 'CIAO "ciao")
  ;; 	(getenv 'CIAO))
  ;;   => "ciao")

  ;; (check
  ;;     (begin
  ;; 	(clearenv)
  ;; 	(putenv "CIAO=ciao")
  ;; 	(getenv 'CIAO))
  ;;   => "ciao")

  ;; (check
  ;;     (guard (E ((assertion-violation? E)
  ;; 		 #t)
  ;; 		(else (condition-message E)))
  ;; 	(putenv 'CIAO "ciao"))
  ;;   => #t)

  #t)


(parametrise ((check-test-name	'getpid))

  #;(begin
    (display "result of getpid() is " )
    (display (getpid))
    (newline))

  (check
      (fixnum? (getpid))
    => #t)

  (check
      (fixnum? (getppid))
    => #t)

  #t)


(parametrise ((check-test-name	'system))

  (check
      (system "echo innocuous output from 'system()' call ; exit 0")
    => 0)

  #t)


(parametrise ((check-test-name	'fork))

  (check
      (fork (lambda (child-pid)
	      (display (format "after fork in parent, parent pid=~s, child pid=~s\n"
			 (getpid) child-pid)
		       (current-error-port))
	      #t)
	    (lambda ()
	      (display (format "after fork in child,  parent pid=~s, child pid=~s\n"
			 (getppid) (getpid))
		       (current-error-port))
	      (exit)))
    => #t)

  #t)


(parametrise ((check-test-name	'waiting))

  (check
      (let ((status (fork (lambda (pid)
			    (wait))
			  (lambda ()
			    (nanosleep 0 1000)
			    (exit 0)))))
	(WIFEXITED status))
    => #t)

  (check
      (let ((status (fork (lambda (pid)
			    (waitpid pid 0))
			  (lambda ()
			    (nanosleep 0 1000)
			    (exit 0)))))
	(WIFEXITED status))
    => #t)

  #t)


(parametrise ((check-test-name	'exec))

;;; execv

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execv "/bin/ls" '("ls" "Makefile"))
	      (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execl

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execl "/bin/ls" "ls" "Makefile")
	      (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execve

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execve "/bin/ls" '("ls" "Makefile") '("VALUE=123"))
	      (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execle

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execle "/bin/ls" '("ls" "Makefile") "VALUE=123")
	      (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execvp

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execvp "ls" '("ls" "Makefile"))
	      (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execlp

  (check
      (fork (lambda (pid)
	      (let ((status (waitpid pid 0)))
		(and (WIFEXITED status)
		     (WEXITSTATUS status))))
	    (lambda ()
	      (execlp "ls" "ls" "Makefile")
	      (exit 9)))
    => 0)

  #t)


(parametrise ((check-test-name	'termination-status))

  (check
      (let ((status (system "exit 0")))
	(WIFEXITED status))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((status (system "exit 0")))
	(and (WIFEXITED status)
	     (WEXITSTATUS status)))
    => 0)

  (check
      (let ((status (system "exit 1")))
	(and (WIFEXITED status)
	     (WEXITSTATUS status)))
    => 1)

  (check
      (let ((status (system "exit 2")))
	(and (WIFEXITED status)
	     (WEXITSTATUS status)))
    => 2)

  (check
      (let ((status (system "exit 4")))
	(and (WIFEXITED status)
	     (WEXITSTATUS status)))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let ((status (system "exit 0")))
	(WIFSIGNALED status))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((status (system "exit 0")))
	(WCOREDUMP status))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((status (system "exit 0")))
	(WIFSTOPPED status))
    => #f)

  #t)


(parametrise ((check-test-name	'stat))

  (check
      (let ((S (stat "Makefile")))
	(check-pretty-print S)
	(struct-stat? S))
    => #t)

  (check
      (let ((S (lstat "Makefile")))
	(check-pretty-print S)
	(struct-stat? S))
    => #t)

;;; --------------------------------------------------------------------

  (check (file-is-directory?		"Makefile" #f)	=> #f)
  (check (file-is-char-device?		"Makefile" #f)	=> #f)
  (check (file-is-block-device?		"Makefile" #f)	=> #f)
  (check (file-is-regular-file?		"Makefile" #f)	=> #t)
  (check (file-is-symbolic-link?	"Makefile" #f)	=> #f)
  (check (file-is-socket?		"Makefile" #f)	=> #f)
  (check (file-is-fifo?			"Makefile" #f)	=> #f)
  (check (file-is-message-queue?	"Makefile" #f)	=> #f)
  (check (file-is-semaphore?		"Makefile" #f)	=> #f)
  (check (file-is-shared-memory?	"Makefile" #f)	=> #f)

  (let ((mode (struct-stat-st_mode (stat "Makefile"))))
    (check (S_ISDIR mode)	=> #f)
    (check (S_ISCHR mode)	=> #f)
    (check (S_ISBLK mode)	=> #f)
    (check (S_ISREG mode)	=> #t)
    (check (S_ISLNK mode)	=> #f)
    (check (S_ISSOCK mode)	=> #f)
    (check (S_ISFIFO mode)	=> #f))

;;; --------------------------------------------------------------------

  (check (file-exists? "Makefile")		=> #t)
  (check (file-exists? "this-does-not-exists")	=> #f)

  (check
      (exact? (file-size "Makefile"))
    => #t)

  (check (access "Makefile" R_OK)		=> #t)
  (check (access "Makefile" W_OK)		=> #t)
  (check (access "Makefile" X_OK)		=> #f)
  (check (access "Makefile" F_OK)		=> #t)
  (check (access "Makefile" (fxand R_OK W_OK))	=> #t)

  (check (file-readable? "Makefile")		=> #t)
  (check (file-writable? "Makefile")		=> #t)
  (check (file-executable? "Makefile")		=> #f)

;;; --------------------------------------------------------------------

  (check
      (let ((time (file-atime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  (check
      (let ((time (file-mtime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  (check
      (let ((time (file-ctime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  #t)


(parametrise ((check-test-name	'file-system))

  (check
      (with-temporary-file ("tmp")
	(chown "tmp" 1000 1000))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (with-temporary-file ("tmp")
	(chmod "tmp" #o755))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let ((mask (getumask)))
	(umask #o755)
	(umask mask))
    => #o755)

;;; --------------------------------------------------------------------

  (check
      (with-temporary-file ("tmp")
	(utime "tmp" 12 34)
	(list (file-atime "tmp")
	      (file-mtime "tmp")))
    => (list (* #e1e9 12)
	     (* #e1e9 34)))

  (check
      (with-temporary-file ("tmp")
	(utimes "tmp" 12 0 34 0)
	(list (file-atime "tmp")
	      (file-mtime "tmp")))
    => (list (* #e1e9 12)
	     (* #e1e9 34)))

  (check
      (with-temporary-file ("tmp")
	(lutimes "tmp" 12 0 34 0)
	(list (file-atime "tmp")
	      (file-mtime "tmp")))
    => (list (* #e1e9 12)
	     (* #e1e9 34)))

  #t)


(parametrise ((check-test-name	'links))

  (check
      (with-temporary-file ("one")
	(unwind-protect
	    (begin
	      (link "one" "two")
	      (file-is-regular-file? "two" #f))
	  (system "rm -f two")))
    => #t)

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (symlink "one" "two")
  	      (file-is-symbolic-link? "two" #f))
  	  (system "rm -f two")))
    => #t)

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (symlink "one" "two")
  	      (readlink/string "two"))
  	  (system "rm -f two")))
    => "one")

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (symlink "one" "two")
  	      (realpath/string "two"))
  	  (system "rm -f two")))
    => (string-append (getcwd/string) "/one"))

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (rename "one" "two")
  	      (list (file-exists? "one")
		    (file-exists? "two")))
  	  (system "rm -f two")))
    => '(#f #t))

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (unlink "one")
  	      (file-exists? "one"))
  	  (system "rm -f one")))
    => #f)

  #t)


(parametrise ((check-test-name	'directories))

  (check
      (with-result
       (unwind-protect
	   (begin
	     (mkdir "one" S_IRWXU)
	     (add-result (file-exists? "one"))
	     (rmdir "one")
	     (file-exists? "one"))
	 (system "rm -fr one")))
    => '(#f (#t)))

  (let ((pwd (getcwd/string)))
    (check
	(unwind-protect
	    (begin
	      (mkdir "one" S_IRWXU)
	      (chdir "one")
	      (getcwd/string))
	  (chdir pwd)
	  (system "rm -fr one"))
      => (string-append pwd "/one")))

;;; --------------------------------------------------------------------

  (check	;verify that no error occurs, even when double closing
      (let ((stream (opendir "..")))
	(check-pretty-print stream)
	(do ((entry (readdir/string stream) (readdir/string stream)))
	    ((not entry)
	     (closedir stream)
	     (directory-stream? stream))
	  (check-pretty-print (list 'directory-entry entry))))
    => #t)

  (check	;verify that no error occurs, even when double closing
      (let ((stream (opendir "..")))
	(check-pretty-print stream)
	(do ((i 0 (+ 1 i)))
	    ((= 2 i)))
	(let ((pos (telldir stream)))
	  (rewinddir stream)
	  (seekdir stream pos))
	(do ((entry (readdir/string stream) (readdir/string stream)))
	    ((not entry)
	     (closedir stream)
	     (directory-stream? stream))
	  (check-pretty-print (list 'directory-entry entry))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-temporary-file 'scheme-indent-function 1)
;; End:
