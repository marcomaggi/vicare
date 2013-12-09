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
;;;Copyright (c) 2010-2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!vicare
(import (vicare)
  (prefix (vicare posix)
	  px.)
  (prefix (vicare ffi)
	  ffi.)
  (vicare platform constants)
  (vicare arguments validation)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare POSIX functions\n")


;;;; helpers

(define-syntax catch
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((assertion-violation? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax catch-error
  (syntax-rules ()
    ((_ print? . ?body)
     (guard (E ((error? E)
		(when print?
		  (check-pretty-print (condition-message E)))
		(condition-irritants E))
	       (else E))
       (begin . ?body)))))

(define-syntax with-temporary-file
  (syntax-rules ()
    ((_ (?pathname) . ?body)
     (let ((ptn ?pathname))
       (px.system (string-append "echo 123 > " ptn))
       (unwind-protect
	   (begin . ?body)
	 (remove-tmp-file ptn))))))

(define (remove-tmp-file pathname)
  (when (file-exists? pathname)
    (delete-file pathname)))


(parametrise ((check-test-name	'cond-expand))

  (check
      (px.cond-expand
       (px.read #t)
       (else #f))
    => #t)

  #t)


(parametrise ((check-test-name	'errno-strings))

  (check
      (px.errno->string EPERM)
    => "EPERM")

  (check
      (px.errno->string EEXIST)
    => "EEXIST")

  #t)


(parametrise ((check-test-name	'signal-strings))

  (check
      (px.interprocess-signal->string SIGKILL)
    => "SIGKILL")

  (check
      (px.interprocess-signal->string SIGSEGV)
    => "SIGSEGV")

  #t)


(parametrise ((check-test-name	'environ))

  (check
      (px.getenv "CIAO-CIAO-CIAO-MARE")
    => #f)

  (check
      (let ()
	(px.setenv "CIAO" "" #t)
	(px.getenv "CIAO"))
    => "")

  (check
      (let ()
	(px.unsetenv "CIAO")
	(px.setenv "CIAO" "fusilli" #t)
	(px.getenv "CIAO"))
    => "fusilli")

  (check
      (let ()
	(px.unsetenv "CIAO")
	(px.setenv "CIAO" "fusilli" #t)
	(px.setenv "CIAO" "spaghetti" #f)
	(px.getenv "CIAO"))
    => "fusilli")

  (check
      (let ()
	(px.unsetenv "SALUT")
	(px.setenv "SALUT" "fusilli" #t)
	(px.setenv "SALUT" "fusilli" #f)
	(px.getenv "SALUT"))
    => "fusilli")

;;; --------------------------------------------------------------------

;;;(check-pretty-print (environ))
;;;(check-pretty-print (hashtable-keys (px.environ-table)))

  (check
      (let ((table (px.environ-table)))
	(hashtable-contains? table "PATH"))
    => #t)

  (check
      (hashtable-contains?
       (px.environ->table (px.table->environ (px.environ->table (px.environ)))) "PATH")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (begin
	(px.setenv "CIAO" "ciao" #t)
	(px.unsetenv "CIAO")
	(px.getenv "CIAO"))
    => #f)

;;; --------------------------------------------------------------------

  ;; (check
  ;;     (begin
  ;; 	(clearenv)
  ;; 	(putenv* 'CIAO "ciao")
  ;; 	(px.getenv 'CIAO))
  ;;   => "ciao")

  ;; (check
  ;;     (begin
  ;; 	(clearenv)
  ;; 	(putenv "CIAO=ciao")
  ;; 	(px.getenv 'CIAO))
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
      (fixnum? (px.getpid))
    => #t)

  (check
      (fixnum? (px.getppid))
    => #t)

  #t)


(parametrise ((check-test-name	'uids))

  (when #f
    (fprintf (current-error-port)
	     "UID=~a, GID=~a, EUID=~a, EGID=~a, login=~a\n"
	     (px.getuid) (px.getgid) (px.geteuid) (px.getegid)
	     (px.getlogin/string))
    (fprintf (current-error-port) "groups=~a\n" (px.getgroups))
    (fprintf (current-error-port) "passwd=~a\n" (px.getpwuid (px.getuid)))
    (fprintf (current-error-port) "passwd=~a\n" (px.getpwnam (px.getlogin/string)))
    (fprintf (current-error-port) "group=~a\n" (px.getgrgid (px.getgid)))
    (fprintf (current-error-port) "group=~a\n"
	     (px.getgrnam (px.struct-group-gr_name (px.getgrgid (px.getgid)))))
    (check-pretty-print (px.user-entries))
    (check-pretty-print (px.group-entries))
    #f)

;;; --------------------------------------------------------------------

  (check
      (fixnum? (px.getuid))
    => #t)

  (check
      (fixnum? (px.getgid))
    => #t)

  (check
      (fixnum? (px.geteuid))
    => #t)

  (check
      (fixnum? (px.getegid))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (px.struct-passwd? (px.getpwuid (px.getuid)))
    => #t)

  (check
      (px.struct-passwd? (px.getpwnam (px.getlogin/string)))
    => #t)

  (check
      (for-all px.struct-passwd? (px.user-entries))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (px.struct-group? (px.getgrgid (px.getgid)))
    => #t)

  (check
      (px.struct-group? (px.getgrnam (px.struct-group-gr_name (px.getgrgid (px.getgid)))))
    => #t)

  (check
      (for-all px.struct-group? (px.group-entries))
    => #t)

  #t)


(parametrise ((check-test-name	'system))

  (check
      (px.system (if check-quiet-tests?
		     "exit 0"
		   "echo innocuous output from 'system()' call ; exit 0"))
    => 0)

  #t)


(parametrise ((check-test-name	'fork))

  (check
      (px.fork (lambda (child-pid)
		 (check-display (format "after fork in parent, parent pid=~s, child pid=~s\n"
				  (px.getpid) child-pid))
		 #t)
	       (lambda ()
		 (check-display (format "after fork in child,  parent pid=~s, child pid=~s\n"
				  (px.getppid) (px.getpid)))
		 (exit)))
    => #t)

  #t)


(parametrise ((check-test-name	'waiting))

  (check
      (let ((status (px.fork (lambda (pid)
			       (px.wait))
			     (lambda ()
			       (px.nanosleep 0 1000)
			       (exit 0)))))
	(px.WIFEXITED status))
    => #t)

  (check
      (let ((status (px.fork (lambda (pid)
			       (px.waitpid pid 0))
			     (lambda ()
			       (px.nanosleep 0 1000)
			       (exit 0)))))
	(px.WIFEXITED status))
    => #t)

  #t)


(parametrise ((check-test-name	'exec))

;;; execv

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execv "/bin/ls" '("ls" "Makefile"))
		 (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execl

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execl "/bin/ls" "ls" "Makefile")
		 (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execve

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execve "/bin/ls" '("ls" "Makefile") '("VALUE=123"))
		 (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execle

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execle "/bin/ls" '("ls" "Makefile") "VALUE=123")
		 (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execvp

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execvp "ls" '("ls" "Makefile"))
		 (exit 9)))
    => 0)

;;; --------------------------------------------------------------------
;;; execlp

  (check
      (px.fork (lambda (pid)
		 (let ((status (px.waitpid pid 0)))
		   (and (px.WIFEXITED status)
			(px.WEXITSTATUS status))))
	       (lambda ()
		 (px.execlp "ls" "ls" "Makefile")
		 (exit 9)))
    => 0)

  #t)


(parametrise ((check-test-name	'termination-status))

  (check
      (let ((status (px.system "exit 0")))
	(px.WIFEXITED status))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(and (px.WIFEXITED status)
	     (px.WEXITSTATUS status)))
    => 0)

  (check
      (let ((status (px.system "exit 1")))
	(and (px.WIFEXITED status)
	     (px.WEXITSTATUS status)))
    => 1)

  (check
      (let ((status (px.system "exit 2")))
	(and (px.WIFEXITED status)
	     (px.WEXITSTATUS status)))
    => 2)

  (check
      (let ((status (px.system "exit 4")))
	(and (px.WIFEXITED status)
	     (px.WEXITSTATUS status)))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(px.WIFSIGNALED status))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(px.WCOREDUMP status))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((status (px.system "exit 0")))
	(px.WIFSTOPPED status))
    => #f)

  #t)


(parametrise ((check-test-name	'stat))

  (check
      (let ((S (px.stat "Makefile")))
;;;	(check-pretty-print S)
	(px.struct-stat? S))
    => #t)

  (check
      (let ((S (px.lstat "Makefile")))
;;;	(check-pretty-print S)
	(px.struct-stat? S))
    => #t)

;;; --------------------------------------------------------------------

  (check (px.file-is-directory?		"Makefile" #f)	=> #f)
  (check (px.file-is-char-device?	"Makefile" #f)	=> #f)
  (check (px.file-is-block-device?	"Makefile" #f)	=> #f)
  (check (px.file-is-regular-file?	"Makefile" #f)	=> #t)
  (check (px.file-is-symbolic-link?	"Makefile" #f)	=> #f)
  (check (px.file-is-socket?		"Makefile" #f)	=> #f)
  (check (px.file-is-fifo?		"Makefile" #f)	=> #f)
  (check (px.file-is-message-queue?	"Makefile" #f)	=> #f)
  (check (px.file-is-semaphore?		"Makefile" #f)	=> #f)
  (check (px.file-is-shared-memory?	"Makefile" #f)	=> #f)

  (let ((mode (px.struct-stat-st_mode (px.stat "Makefile"))))
    (check (px.S_ISDIR mode)	=> #f)
    (check (px.S_ISCHR mode)	=> #f)
    (check (px.S_ISBLK mode)	=> #f)
    (check (px.S_ISREG mode)	=> #t)
    (check (px.S_ISLNK mode)	=> #f)
    (check (px.S_ISSOCK mode)	=> #f)
    (check (px.S_ISFIFO mode)	=> #f))

;;; --------------------------------------------------------------------

  (check (file-exists? "Makefile")		=> #t)
  (check (file-exists? "this-does-not-exists")	=> #f)

  (check
      (exact? (px.file-size "Makefile"))
    => #t)

  (check (px.access "Makefile" R_OK)			=> #t)
  (check (px.access "Makefile" W_OK)			=> #t)
  (check (px.access "Makefile" X_OK)			=> #f)
  (check (px.access "Makefile" F_OK)			=> #t)
  (check (px.access "Makefile" (fxand R_OK W_OK))	=> #t)

  (check (px.file-readable? "Makefile")		=> #t)
  (check (px.file-writable? "Makefile")		=> #t)
  (check (px.file-executable? "Makefile")	=> #f)

;;; --------------------------------------------------------------------

  (check
      (let ((time (px.file-atime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  (check
      (let ((time (px.file-mtime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  (check
      (let ((time (px.file-ctime "Makefile")))
;;;	(check-pretty-print time)
	(exact? time))
    => #t)

  #t)


(parametrise ((check-test-name	'file-system))

  (check
      (with-temporary-file ("tmp")
  	(px.chown "tmp" (px.getuid) (px.getgid)))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (with-temporary-file ("tmp")
	(px.chmod "tmp" #o755))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let ((mask (px.getumask)))
	(px.umask #o755)
	(px.umask mask))
    => #o755)

;;; --------------------------------------------------------------------

  (check
      (with-temporary-file ("tmp")
	(px.utime "tmp" 12 34)
	(list (px.file-atime "tmp")
	      (px.file-mtime "tmp")))
    => (list (* #e1e9 12)
	     (* #e1e9 34)))

  (check
      (with-temporary-file ("tmp")
  	(px.utimes "tmp" 12 0 34 0)
  	(list (px.file-atime "tmp")
  	      (px.file-mtime "tmp")))
    => (list (* #e1e9 12)
  	     (* #e1e9 34)))

  (check
      (with-temporary-file ("tmp")
  	(px.lutimes "tmp" 12 0 34 0)
  	(list (px.file-atime "tmp")
  	      (px.file-mtime "tmp")))
    => (list (* #e1e9 12)
  	     (* #e1e9 34)))

  #t)


(parametrise ((check-test-name	'links))

  (check
      (with-temporary-file ("one")
	(unwind-protect
	    (begin
	      (px.link "one" "two")
	      (px.file-is-regular-file? "two" #f))
	  (px.system "rm -f two")))
    => #t)

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.symlink "one" "two")
  	      (px.file-is-symbolic-link? "two" #f))
  	  (px.system "rm -f two")))
    => #t)

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.symlink "one" "two")
  	      (px.readlink/string "two"))
  	  (px.system "rm -f two")))
    => "one")

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.symlink "one" "two")
  	      (px.realpath/string "two"))
  	  (px.system "rm -f two")))
    => (string-append (px.getcwd/string) "/one"))

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.rename "one" "two")
  	      (list (file-exists? "one")
		    (file-exists? "two")))
  	  (px.system "rm -f two")))
    => '(#f #t))

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.unlink "one")
  	      (file-exists? "one"))
  	  (px.system "rm -f one")))
    => #f)

  (check
      (with-temporary-file ("one")
  	(unwind-protect
  	    (begin
  	      (px.remove "one")
  	      (file-exists? "one"))
  	  (px.system "rm -f one")))
    => #f)

  #t)


(parametrise ((check-test-name	'directories))

  (check
      (with-result
       (unwind-protect
	   (begin
	     (px.mkdir "one" S_IRWXU)
	     (add-result (file-exists? "one"))
	     (px.rmdir "one")
	     (file-exists? "one"))
	 (px.system "rm -fr one")))
    => '(#f (#t)))

  (let ((pwd (px.getcwd/string)))
    (check
	(unwind-protect
	    (begin
	      (px.mkdir "one" S_IRWXU)
	      (px.chdir "one")
	      (px.getcwd/string))
	  (px.chdir pwd)
	  (px.system "rm -fr one"))
      => (string-append pwd "/one")))

;;; --------------------------------------------------------------------

  (check	;verify that no error occurs, even when double closing
      (let ((stream (px.opendir "..")))
;;;	(check-pretty-print stream)
	(do ((entry (px.readdir/string stream) (px.readdir/string stream)))
	    ((not entry)
	     (px.closedir stream)
	     (px.directory-stream? stream))
;;;	  (check-pretty-print (list 'directory-entry entry))
	  #f))
    => #t)

  (check	;verify that no error occurs, even when double closing
      (let ((stream (px.opendir "..")))
;;;	(check-pretty-print stream)
	(do ((i 0 (+ 1 i)))
	    ((= 2 i)))
	(let ((pos (px.telldir stream)))
	  (px.rewinddir stream)
	  (px.seekdir stream pos))
	(do ((entry (px.readdir/string stream) (px.readdir/string stream)))
	    ((not entry)
	     (px.closedir stream)
	     (px.directory-stream? stream))
;;;	  (check-pretty-print (list 'directory-entry entry))
	  #f))
    => #t)

  #t)


(parametrise ((check-test-name	'fds))

  (check
      (begin
	(px.system "rm -f tmp")
	(let ((fd (px.open "tmp"
			   (fxior O_CREAT O_EXCL O_RDWR)
			   (fxior S_IRUSR S_IWUSR))))
	  (unwind-protect
	      (begin
		(px.write fd '#vu8(1 2 3 4))
		(px.lseek fd 0 SEEK_SET)
		(let ((buffer (make-bytevector 4)))
		  (list (px.read fd buffer) buffer)))
	    (px.close fd)
	    (px.system "rm -f tmp"))))
    => '(4 #vu8(1 2 3 4)))

  (check
      (begin
	(px.system "rm -f tmp")
	(let ((fd (px.open "tmp"
			   (fxior O_CREAT O_EXCL O_RDWR)
			   (fxior S_IRUSR S_IWUSR))))
	  (unwind-protect
	      (begin
		(px.pwrite fd '#vu8(1 2 3 4) #f 0)
		(px.lseek fd 0 SEEK_SET)
		(let ((buffer (make-bytevector 4)))
		  (list (px.pread fd buffer #f 0) buffer)))
	    (px.close fd)
	    (px.system "rm -f tmp"))))
    => '(4 #vu8(1 2 3 4)))

;;; --------------------------------------------------------------------

  (check
      (with-result
       (px.system "rm -f tmp")
       (let ((fd (px.open "tmp"
			  (fxior O_CREAT O_EXCL O_RDWR)
			  (fxior S_IRUSR S_IWUSR))))
	 (unwind-protect
	     (begin
	       (add-result (px.writev fd '(#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9))))
	       (px.lseek fd 0 SEEK_SET)
	       (let ((buffers (list (make-bytevector 4)
				    (make-bytevector 4)
				    (make-bytevector 2))))
		 (add-result (px.readv fd buffers))
		 buffers))
	   (px.close fd)
	   (px.system "rm -f tmp"))))
    => '((#vu8(0 1 2 3) #vu8(4 5 6 7) #vu8(8 9)) (10 10)))

;;; --------------------------------------------------------------------

  (check
      (begin
	(px.system "rm -f tmp")
	(let ((fd (px.open "tmp"
			   (fxior O_CREAT O_EXCL O_RDWR)
			   (fxior S_IRUSR S_IWUSR))))
	  (unwind-protect
	      (fixnum? (px.fcntl fd F_GETFL #f))
	    (px.close fd)
	    (px.system "rm -f tmp"))))
    => #t)

  (check	;close on exec test
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (px.fd-in-close-on-exec-mode? in)))
    => #f)

  (check	;close on exec setting
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (px.fd-set-close-on-exec-mode! in)
	  (px.fd-in-close-on-exec-mode? in)))
    => #t)

  (check	;close on exec setting and unsetting
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (px.fd-set-close-on-exec-mode! in)
	  (px.fd-unset-close-on-exec-mode! in)
	  (px.fd-in-close-on-exec-mode? in)))
    => #f)

;;; --------------------------------------------------------------------
;;; lockf

  (check
      (with-compensations
	(remove-tmp-file "tmp")
	(push-compensation (remove-tmp-file "tmp"))
	(letrec ((fd (compensate
			 (px.open "tmp"
				  (fxior O_CREAT O_EXCL O_RDWR)
				  (fxior S_IRUSR S_IWUSR))
		       (with
			(px.close fd)))))
	  (px.lockf fd F_TEST 0)))
    => 0)

  (check
      (with-compensations
	(remove-tmp-file "tmp")
	(push-compensation (remove-tmp-file "tmp"))
	(letrec ((fd (compensate
			 (px.open "tmp"
				  (fxior O_CREAT O_EXCL O_RDWR)
				  (fxior S_IRUSR S_IWUSR))
		       (with
			(px.close fd)))))
	  (px.lockf fd F_TLOCK 0)
	  (px.lockf fd F_TEST 0)))
    => 0)

;;; --------------------------------------------------------------------
;;; pipe

  (check
      (let-values (((in ou) (px.pipe)))
	(px.write ou '#vu8(1 2 3 4))
	(let ((bv (make-bytevector 4)))
	  (px.read in bv)
	  bv))
    => '#vu8(1 2 3 4))

  (check	;raw pipes to child process
      (let-values (((child-stdin       parent-to-child) (px.pipe))
		   ((parent-from-child child-stdout)    (px.pipe)))
	(px.fork (lambda (pid) ;parent
		   (let ((buf (make-bytevector 1)))
		     (px.read  parent-from-child buf)
		     (px.write parent-to-child   '#vu8(2))
		     buf))
		 (lambda () ;child
		   (begin ;setup stdin
		     (close-input-port (current-input-port))
		     (px.dup2 child-stdin 0)
		     (px.close child-stdin))
		   (begin ;setup stdout
		     (close-output-port (current-output-port))
		     (px.dup2 child-stdout 1)
		     (px.close child-stdout))
		   (let ((buf (make-bytevector 1)))
		     (px.write 1 '#vu8(1))
		     (px.read  0 buf)
;;;		  (check-pretty-print buf)
		     (assert (equal? buf '#vu8(2)))
		     (exit 0)))))
    => '#vu8(1))

  (check	;port pipes to child process
      (let-values (((child-stdin       parent-to-child) (px.pipe))
		   ((parent-from-child child-stdout)    (px.pipe)))
	(px.fork
	 (lambda (pid) ;parent
	   (let* ((inp (make-textual-file-descriptor-input-port
			parent-from-child "in" (native-transcoder)))
		  (oup (make-textual-file-descriptor-output-port
			parent-to-child "out" (native-transcoder)))
		  (buf (get-string-n inp 4)))
	     (display "hello" oup)
	     (flush-output-port oup)
	     buf))
	 (lambda ()	;child
	   (guard (E (else
		      (check-pretty-print E)
		      (exit 1)))
	     (begin ;setup stdin
	       (close-input-port (current-input-port))
	       (current-input-port
		(make-textual-file-descriptor-input-port
		 child-stdin "*stdin*" (native-transcoder))))
	     (begin ;setup stdout
	       (close-output-port (current-output-port))
	       (current-output-port
		(make-textual-file-descriptor-output-port
		 child-stdout "*stdout*" (native-transcoder))))
	     (display "ciao")
	     (flush-output-port (current-output-port))
	     (let ((data (get-string-n (current-input-port) 5)))
;;;	       (check-pretty-print data)
	       (assert (equal? data "hello"))
	       (exit 0))))))
    => "ciao")

;;; --------------------------------------------------------------------
;;; select

  (check	;timeout
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let-values (((r w e) (px.select #f `(,in) '() `(,in ,ou) 0 0)))
	      (equal? (list r w e)
		      '(() () ())))
	  (px.close in)
	  (px.close ou)))
    => #t)

  (check	;read ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (begin
	      (px.write ou '#vu8(1))
	      (let-values (((r w e) (px.select #f `(,in) '() `(,in) 0 0)))
		(equal? (list r w e)
			`((,in) () ()))))
	  (px.close in)
	  (px.close ou)))
    => #t)

  (check	;write ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let-values (((r w e) (px.select #f '() `(,ou) `(,ou) 0 0)))
	      (equal? (list r w e)
		      `(() (,ou) ())))
	  (px.close in)
	  (px.close ou)))
    => #t)

;;; --------------------------------------------------------------------
;;; select-fd

  (check	;timeout
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let-values (((r w e) (px.select-fd in 0 0)))
	      (equal? (list r w e)
		      '(#f #f #f)))
	  (px.close in)
	  (px.close ou)))
    => #t)

  (check	;read ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (begin
	      (px.write ou '#vu8(1))
	      (let-values (((r w e) (px.select-fd in 0 0)))
		(equal? (list r w e)
			`(,in #f #f))))
	(px.close in)
	(px.close ou)))
    => #t)

  (check	;write ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let-values (((r w e) (px.select-fd ou 0 0)))
	      (equal? (list r w e)
		      `(#f ,ou #f)))
	  (px.close in)
	  (px.close ou)))
    => #t)

;;; --------------------------------------------------------------------
;;; select-fd-readable?

  (check	;timeout
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (px.select-fd-readable? in 0 0)
	  (px.close in)
	  (px.close ou)))
    => #f)

  (check	;read ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (begin
	      (px.write ou '#vu8(1))
	      (px.select-fd-readable? in 0 0))
	  (px.close in)
	  (px.close ou)))
    => #t)

;;; --------------------------------------------------------------------
;;; select-fd-writable?

  (check	;timeout
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (px.select-fd-writable? in 0 0)
	  (px.close in)
	  (px.close ou)))
    => #f)

  (check	;read ready
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (px.select-fd-writable? ou 0 0)
	  (px.close in)
	  (px.close ou)))
    => #t)

;;; --------------------------------------------------------------------
;;; select-port

  (check	;timeout
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (let ((inp (make-binary-file-descriptor-input-port* in "inp")))
	    (receive (r w e)
		(px.select-port inp 0 0)
	      (list r w e)))))
    => '(#f #f #f))

  (check	;read ready
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (let ((inp (make-binary-file-descriptor-input-port* in "inp")))
	    (px.write ou '#vu8(1))
	    (receive (r w e)
		(px.select-port inp 0 0)
	      (equal? (list r w e)
		      `(,inp #f #f))))))
    => #t)

  (check	;write ready
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (let ((oup (make-binary-file-descriptor-output-port* ou "oup")))
	    (receive (r w e)
		(px.select-port oup 0 0)
	      (equal? (list r w e)
		      `(#f ,oup #f))))))
    => #t)

;;; --------------------------------------------------------------------
;;; poll

  (check
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (let* ((vec (vector (vector in 0 0)
				(vector ou 0 0)))
		   (rv  (px.poll vec 10)))
	      (equal? (list rv vec)
		      `(0 #(#(,in 0 0)
			    #(,ou 0 0)))))
	  (px.close in)
	  (px.close ou)))
    => #t)

  (check
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (let* ((vec `#(#(,in 0 0) #(,ou 0 0)))
		 (rv  (px.poll vec 10)))
	    (equal? (list rv vec)
		    `(0 #(#(,in 0 0)
			  #(,ou 0 0)))))))
    => #t)

  (check
      (let-values (((in ou) (px.pipe)))
	(unwind-protect
	    (begin
	      (px.write ou '#vu8(1))
	      (let* ((vec (vector (vector in POLLIN 0)
				  (vector ou POLLOUT 0)))
		     (rv  (px.poll vec 10))
		     (buf (make-bytevector 1)))
		(px.read in buf)
		(equal? (list rv buf vec)
			`(2 #vu8(1) #(#(,in ,POLLIN  ,POLLIN)
				      #(,ou ,POLLOUT ,POLLOUT))))))
	  (px.close in)
	  (px.close ou)))
    => #t)

  (check
      (with-compensations
	(receive (in ou)
	    (px.pipe)
	  (push-compensation (px.close in))
	  (push-compensation (px.close ou))
	  (px.write ou '#vu8(1))
	  (let* ((vec `#(#(,in ,POLLIN 0)
			 #(,ou ,POLLOUT 0)))
		 (rv  (px.poll vec 10))
		 (buf (make-bytevector 1)))
	    (let ((res1 (equal? vec `#(#(,in ,POLLIN  ,POLLIN)
				       #(,ou ,POLLOUT ,POLLOUT)))))
	      (px.read in buf)
	      (list res1 rv '#vu8(1))))))
    => '(#t 2 #vu8(1)))

;;; --------------------------------------------------------------------
;;; truncate and ftruncate

  (check
      (let ((name "vicare.test"))
	(when (file-exists? name)
	  (delete-file name))
	(unwind-protect
	    (begin
	      (with-output-to-file name
		(lambda ()
		  (display "01234567890123")))
	      (px.truncate name 10)
	      (px.file-size name))
	  (delete-file name)))
    => 10)

  (check
      (let ((name "vicare.test"))
	(when (file-exists? name)
	  (delete-file name))
	(unwind-protect
	    (begin
	      (with-output-to-file name
		(lambda ()
		  (display "01234567890123")))
	      (let ((fd (px.open name O_RDWR 0)))
		(unwind-protect
		    (px.ftruncate fd 10)
		  (px.close fd)))
	      (px.file-size name))
	  (delete-file name)))
    => 10)

  #t)


(parametrise ((check-test-name	'fdsets))

;;; sizeof

  (check
      (fixnum? (px.sizeof-fd-set))
    => #t)

  (check
      (fixnum? (px.sizeof-fd-set 2))
    => #t)

;;; --------------------------------------------------------------------
;;; bytevector array

  (check
      (let ((bv (px.make-fd-set-bytevector)))
	(bytevector-length bv))
    => (px.sizeof-fd-set))

  (check
      (let ((bv (px.make-fd-set-bytevector 3)))
	(bytevector-length bv))
    => (px.sizeof-fd-set 3))

;;; --------------------------------------------------------------------
;;; pointer array

  (check
      (let ((P (px.make-fd-set-pointer)))
	(pointer-null? P))
    => #f)

  (check
      (let ((P (px.make-fd-set-pointer 3)))
	(pointer-null? P))
    => #f)

;;; --------------------------------------------------------------------
;;; memory-block array

  (check
      (let ((mb (px.make-fd-set-memory-block)))
	(memory-block-size mb))
    => (px.sizeof-fd-set))

  (check
      (let ((mb (px.make-fd-set-memory-block 3)))
	(memory-block-size mb))
    => (px.sizeof-fd-set 3))

;;; --------------------------------------------------------------------
;;; operations with bytevectors, single set

  (check
      (let ((S (px.make-fd-set-bytevector)))
	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-bytevector)))
	(px.FD_SET 123 S)
	(px.FD_ISSET 123 S))
    => #t)

  (check
      (let ((S (px.make-fd-set-bytevector)))
	(px.FD_SET 123 S)
	(px.FD_CLR 123 S)
	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-bytevector)))
	(px.FD_SET 123 S)
	(px.FD_ZERO S)
	(px.FD_ISSET 123 S))
    => #f)

;;; --------------------------------------------------------------------
;;; operations with bytevectors, multiple sets

  (check
      (let ((S (px.make-fd-set-bytevector 3)))
	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-bytevector 3)))
	(px.FD_SET 123 S 1)
	(px.FD_ISSET 123 S 1))
    => #t)

  (check
      (let ((S (px.make-fd-set-bytevector 3)))
	(px.FD_SET 123 S 1)
	(px.FD_CLR 123 S 1)
	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-bytevector 3)))
	(px.FD_SET 123 S 1)
	(px.FD_ZERO S 1)
	(px.FD_ISSET 123 S 1))
    => #f)

;;; --------------------------------------------------------------------
;;; operations with pointers, single set

  (check
      (let ((S (px.make-fd-set-pointer)))
	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-pointer)))
  	(px.FD_SET 123 S)
  	(px.FD_ISSET 123 S))
    => #t)

  (check
      (let ((S (px.make-fd-set-pointer)))
  	(px.FD_SET 123 S)
  	(px.FD_CLR 123 S)
  	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-pointer)))
  	(px.FD_SET 123 S)
  	(px.FD_ZERO S)
  	(px.FD_ISSET 123 S))
    => #f)

;;; --------------------------------------------------------------------
;;; operations with pointers, multiple sets

  (check
      (let ((S (px.make-fd-set-pointer 3)))
  	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-pointer 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_ISSET 123 S 1))
    => #t)

  (check
      (let ((S (px.make-fd-set-pointer 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_CLR 123 S 1)
  	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-pointer 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_ZERO S 1)
  	(px.FD_ISSET 123 S 1))
    => #f)

;;; --------------------------------------------------------------------
;;; operations with memory-blocks, single set

  (check
      (let ((S (px.make-fd-set-memory-block)))
	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-memory-block)))
  	(px.FD_SET 123 S)
  	(px.FD_ISSET 123 S))
    => #t)

  (check
      (let ((S (px.make-fd-set-memory-block)))
  	(px.FD_SET 123 S)
  	(px.FD_CLR 123 S)
  	(px.FD_ISSET 123 S))
    => #f)

  (check
      (let ((S (px.make-fd-set-memory-block)))
  	(px.FD_SET 123 S)
  	(px.FD_ZERO S)
  	(px.FD_ISSET 123 S))
    => #f)

;;; --------------------------------------------------------------------
;;; operations with memory-blocks, multiple sets

  (check
      (let ((S (px.make-fd-set-memory-block 3)))
  	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-memory-block 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_ISSET 123 S 1))
    => #t)

  (check
      (let ((S (px.make-fd-set-memory-block 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_CLR 123 S 1)
  	(px.FD_ISSET 123 S 1))
    => #f)

  (check
      (let ((S (px.make-fd-set-memory-block 3)))
  	(px.FD_SET 123 S 1)
  	(px.FD_ZERO S 1)
  	(px.FD_ISSET 123 S 1))
    => #f)

;;; --------------------------------------------------------------------
;;; select-from-sets, bytevectors

  (check	;timeout
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-bytevector))
		   ((wfds)  (px.make-fd-set-bytevector))
		   ((efds)  (px.make-fd-set-bytevector)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      ;;OU is always writable
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou efds)
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r #f)
		      (eq? w #f)
		      (eq? e #f))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t))

  (check	;read/write ready
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-bytevector))
		   ((wfds)  (px.make-fd-set-bytevector))
		   ((efds)  (px.make-fd-set-bytevector)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou wfds)
	      (px.FD_SET ou efds)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r rfds)
		      (eq? w wfds)
		      (eq? e efds)
		      (px.FD_ISSET in rfds)
		      (px.FD_ISSET ou wfds))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t #t #t))

;;; --------------------------------------------------------------------
;;; select-from-sets, pointers

  (check	;timeout
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-pointer))
		   ((wfds)  (px.make-fd-set-pointer))
		   ((efds)  (px.make-fd-set-pointer)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      ;;OU is always writable
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou efds)
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r #f)
		      (eq? w #f)
		      (eq? e #f))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t))

  (check	;read/write ready
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-pointer))
		   ((wfds)  (px.make-fd-set-pointer))
		   ((efds)  (px.make-fd-set-pointer)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou wfds)
	      (px.FD_SET ou efds)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r rfds)
		      (eq? w wfds)
		      (eq? e efds)
		      (px.FD_ISSET in rfds)
		      (px.FD_ISSET ou wfds))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t #t #t))

;;; --------------------------------------------------------------------
;;; select-from-sets, memory-blocks

  (check	;timeout
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-memory-block))
		   ((wfds)  (px.make-fd-set-memory-block))
		   ((efds)  (px.make-fd-set-memory-block)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      ;;OU is always writable
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou efds)
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r #f)
		      (eq? w #f)
		      (eq? e #f))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t))

  (check	;read/write ready
      (let-values (((in ou) (px.pipe))
		   ((rfds)  (px.make-fd-set-memory-block))
		   ((wfds)  (px.make-fd-set-memory-block))
		   ((efds)  (px.make-fd-set-memory-block)))
	(unwind-protect
	    (begin
	      (px.FD_SET in rfds)
	      (px.FD_SET in wfds)
	      (px.FD_SET in efds)
	      (px.FD_SET ou rfds)
	      (px.FD_SET ou wfds)
	      (px.FD_SET ou efds)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let-values (((r w e) (px.select-from-sets #f rfds wfds efds 0 0)))
;;;		(check-pretty-print (list r w e))
		(list (eq? r rfds)
		      (eq? w wfds)
		      (eq? e efds)
		      (px.FD_ISSET in rfds)
		      (px.FD_ISSET in wfds)
		      (px.FD_ISSET in efds)
		      (px.FD_ISSET ou rfds)
		      (px.FD_ISSET ou wfds)
		      (px.FD_ISSET ou efds))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #t #t   #t #f #f  #f #t #f))

;;; --------------------------------------------------------------------
;;; select-from-sets-array, bytevectors

  (check	;timeout
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-bytevector 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      ;;OU is always writable
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 2)
	      (px.select-from-sets-array #f fdsets 0 0))
	  (px.close in)
	  (px.close ou)))
    => #f)

  (check	;read/write ready
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-bytevector 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 1)
	      (px.FD_SET ou fdsets 2)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let ((rv (px.select-from-sets-array #f fdsets 0 0)))
		(list (px.FD_ISSET in fdsets 0)
		      (px.FD_ISSET in fdsets 1)
		      (px.FD_ISSET in fdsets 2)
		      (px.FD_ISSET ou fdsets 0)
		      (px.FD_ISSET ou fdsets 1)
		      (px.FD_ISSET ou fdsets 2))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #f #f  #f #t #f))

;;; --------------------------------------------------------------------
;;; select-from-sets-array, pointers

  (check	;timeout
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-pointer 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      ;;OU is always writable
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 2)
	      (px.select-from-sets-array #f fdsets 0 0))
	  (px.close in)
	  (px.close ou)))
    => #f)

  (check	;read/write ready
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-pointer 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 1)
	      (px.FD_SET ou fdsets 2)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let ((rv (px.select-from-sets-array #f fdsets 0 0)))
		(list (px.FD_ISSET in fdsets 0)
		      (px.FD_ISSET in fdsets 1)
		      (px.FD_ISSET in fdsets 2)
		      (px.FD_ISSET ou fdsets 0)
		      (px.FD_ISSET ou fdsets 1)
		      (px.FD_ISSET ou fdsets 2))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #f #f  #f #t #f))

;;; --------------------------------------------------------------------
;;; select-from-sets-array, memory-blocks

  (check	;timeout
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-memory-block 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      ;;OU is always writable
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 2)
	      (px.select-from-sets-array #f fdsets 0 0))
	  (px.close in)
	  (px.close ou)))
    => #f)

  (check	;read/write ready
      (let-values (((in ou)  (px.pipe))
		   ((fdsets) (px.make-fd-set-memory-block 3)))
	(unwind-protect
	    (begin
	      (px.FD_SET in fdsets 0)
	      (px.FD_SET in fdsets 1)
	      (px.FD_SET in fdsets 2)
	      (px.FD_SET ou fdsets 0)
	      (px.FD_SET ou fdsets 1)
	      (px.FD_SET ou fdsets 2)
	      (assert (= 1 (px.write ou '#vu8(1))))
	      (let ((rv (px.select-from-sets-array #f fdsets 0 0)))
		(list (px.FD_ISSET in fdsets 0)
		      (px.FD_ISSET in fdsets 1)
		      (px.FD_ISSET in fdsets 2)
		      (px.FD_ISSET ou fdsets 0)
		      (px.FD_ISSET ou fdsets 1)
		      (px.FD_ISSET ou fdsets 2))))
	  (px.close in)
	  (px.close ou)))
    => '(#t #f #f  #f #t #f))

  #t)


(parametrise ((check-test-name	'config))

  (check
      (px.sysconf _SC_JOB_CONTROL)
    => _POSIX_JOB_CONTROL)

  (check
      (px.pathconf "Makefile" _PC_NAME_MAX)
    => NAME_MAX)

  (check
      (px.confstr/string _CS_PATH)
    => "/bin:/usr/bin")

  #t)


(parametrise ((check-test-name	'split-search-path))

  (check
      (receive (root tail)
	  (px.split-pathname-root-and-tail "a/b")
	(list root tail))
    => '("a" "b"))

  (check
      (receive (root tail)
	  (px.split-pathname-root-and-tail "ciao")
	(list root tail))
    => '("" "ciao"))

  #t)


(parametrise ((check-test-name	'split-search-path))

  (check
      (px.split-search-path-bytevector '#vu8())
    => '())

  (check
      (px.split-search-path-bytevector '#ve(ascii "ciao"))
    => '(#ve(ascii "ciao")))

  (check
      (px.split-search-path-bytevector '#ve(ascii "ciao:"))
    => '(#ve(ascii "ciao")))

  (check
      (px.split-search-path-bytevector '#ve(ascii ":ciao"))
    => '(#ve(ascii "ciao")))

  (check
      (px.split-search-path-bytevector '#ve(ascii ":"))
    => '())

  (check
      (px.split-search-path-bytevector '#ve(ascii "::::"))
    => '())

  (check
      (px.split-search-path-bytevector '#ve(ascii "ciao:hello"))
    => '(#ve(ascii "ciao") #ve(ascii "hello")))

  (check
      (px.split-search-path-bytevector '#ve(ascii "ciao:hello:salut"))
    => '(#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (check
      (px.split-search-path-bytevector '#ve(ascii "ciao:::hello"))
    => '(#ve(ascii "ciao") #ve(ascii "hello")))

;;; --------------------------------------------------------------------

  (check
      (px.split-search-path-string "")
    => '())

  (check
      (px.split-search-path-string "ciao")
    => '("ciao"))

  (check
      (px.split-search-path-string "ciao:")
    => '("ciao"))

  (check
      (px.split-search-path-string ":ciao")
    => '("ciao"))

  (check
      (px.split-search-path-string ":")
    => '())

  (check
      (px.split-search-path-string "::::")
    => '())

  (check
      (px.split-search-path-string "ciao:hello")
    => '("ciao" "hello"))

  (check
      (px.split-search-path-string "ciao:hello:salut")
    => '("ciao" "hello" "salut"))

  (check
      (px.split-search-path-string "ciao:::hello")
    => '("ciao" "hello"))

;;; --------------------------------------------------------------------

  (check
      (px.split-search-path "")
    => '())

  (check
      (px.split-search-path '#vu8())
    => '())

  (check
      (px.split-search-path "ciao:hello:salut")
    => '("ciao" "hello" "salut"))

  (check
      (px.split-search-path '#ve(ascii "ciao:hello:salut"))
    => '(#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  #t)


(parametrise ((check-test-name	'split-pathname))

  (define-syntax doit
    (syntax-rules (=>)
      ((_ ?form => ?abs-result ?comp-result)
       (check
	   (call-with-values
	       (lambda () ?form)
	     list)
	 => '(?abs-result ?comp-result)))))

  (doit (px.split-pathname-bytevector '#ve(ascii "ciao"))
	=> #f (#ve(ascii "ciao")))

  (doit (px.split-pathname-bytevector '#ve(ascii "ciao/"))
	=> #f (#ve(ascii "ciao")))

  (doit (px.split-pathname-bytevector '#ve(ascii "/ciao"))
	=> #t (#ve(ascii "ciao")))

  (doit (px.split-pathname-bytevector '#ve(ascii "/"))
	=> #t ())

  (doit (px.split-pathname-bytevector '#ve(ascii "////"))
	=> #t ())

  (doit (px.split-pathname-bytevector '#ve(ascii "ciao/hello"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello")))

  (doit (px.split-pathname-bytevector '#ve(ascii "ciao/hello/salut"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (doit (px.split-pathname-bytevector '#ve(ascii "/ciao/hello/salut"))
	=> #t (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  (doit (px.split-pathname-bytevector '#ve(ascii "ciao///hello"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello")))

;;; --------------------------------------------------------------------

  (doit (px.split-pathname-string "ciao")
	=> #f ("ciao"))

  (doit (px.split-pathname-string "ciao/")
	=> #f ("ciao"))

  (doit (px.split-pathname-string "/ciao")
	=> #t ("ciao"))

  (doit (px.split-pathname-string "/")
	=> #t ())

  (doit (px.split-pathname-string "////")
	=> #t ())

  (doit (px.split-pathname-string "ciao/hello")
	=> #f ("ciao" "hello"))

  (doit (px.split-pathname-string "ciao/hello/salut")
	=> #f ("ciao" "hello" "salut"))

  (doit (px.split-pathname-string "ciao///hello")
	=> #f ("ciao" "hello"))

;;; --------------------------------------------------------------------

  (doit (px.split-pathname "ciao/hello/salut")
	=> #f ("ciao" "hello" "salut"))

  (doit (px.split-pathname "/ciao/hello/salut")
	=> #t ("ciao" "hello" "salut"))

  (doit (px.split-pathname '#ve(ascii "ciao/hello/salut"))
	=> #f (#ve(ascii "ciao") #ve(ascii "hello") #ve(ascii "salut")))

  #t)


(parametrise ((check-test-name	'find-executable))

  (check	;first char is slash
      (px.find-executable-as-string "/usr/bin/ls")
    => "/usr/bin/ls")

  (check
      (px.find-executable-as-string "ls")
    => "/usr/bin/ls")

  (check
      (px.find-executable-as-string "this-cannot-exist")
    => #f)

  (when #f
    (fprintf (current-error-port)
	     "vicare executable ~a\n" (px.find-executable-as-string "vicare"))
    (fprintf (current-error-port)
	     "vicare-executable-string => ~a\n" (px.vicare-executable-as-string)))

  #t)


(parametrise ((check-test-name	'realtime-clock))

  (check
      (px.struct-timespec?
       (px.clock-getres CLOCK_MONOTONIC (px.make-struct-timespec 0 0)))
    => #t)

  (check
      (px.struct-timespec?
       (px.clock-gettime CLOCK_MONOTONIC (px.make-struct-timespec 0 0)))
    => #t)

  (check
      (let ((cid (px.clock-getcpuclockid 0)))
;;;	(check-pretty-print (list 'process-clock-id cid))
	(integer? cid))
    => #t)

  (check
      (px.struct-timespec?
       (px.clock-gettime (px.clock-getcpuclockid 0)
	 (px.make-struct-timespec 0 0)))
    => #t)
  #t)


(parametrise ((check-test-name	'resources))

  (when RLIMIT_SIGPENDING
    (check
	(let ((rlim (px.getrlimit RLIMIT_SIGPENDING)))
;;;	(check-pretty-print rlim)
	  (px.struct-rlimit? rlim))
      => #t))

  (when RLIMIT_SIGPENDING
    (check
	(let ((rlim (px.getrlimit RLIMIT_SIGPENDING)))
;;;	(check-pretty-print rlim)
	  (px.setrlimit RLIMIT_SIGPENDING rlim)
	  #t)
      => #t))

  (when RUSAGE_SELF
    (check
	(let ((rusa (px.getrusage RUSAGE_SELF)))
;;;	(check-pretty-print rusa)
	  (px.struct-rusage? rusa))
      => #t))

  #t)


(parametrise ((check-test-name	'validation-clauses))

  (define-syntax catch
    (syntax-rules ()
      ((_ print? . ?body)
       (guard (E ((assertion-violation? E)
		  (when print?
		    (check-pretty-print (condition-message E)))
		  (condition-irritants E))
		 (else E))
	 (begin . ?body)))))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?print ?validator . ?objs)
       (catch ?print
	 (let ((who 'test))
	   (with-arguments-validation (who)
	       ((?validator . ?objs))
	     #t))))))

;;; --------------------------------------------------------------------
;;; file-descriptor

  (check
      (doit #f px.file-descriptor 123)
    => #t)

  (check
      (doit #f px.file-descriptor -1)
    => '(-1))

  (check
      (doit #f px.file-descriptor (greatest-fixnum))
    => `(,(greatest-fixnum)))

  (check
      (doit #f px.file-descriptor 'ciao)
    => '(ciao))

;;; --------------------------------------------------------------------
;;; file-descriptor/false

  (check
      (doit #f px.file-descriptor/false 123)
    => #t)

  (check
      (doit #f px.file-descriptor/false -1)
    => '(-1))

  (check
      (doit #f px.file-descriptor/false (greatest-fixnum))
    => `(,(greatest-fixnum)))

  (check
      (doit #f px.file-descriptor/false #f)
    => #t)

  (check
      (doit #f px.file-descriptor/false 'ciao)
    => '(ciao))

  #t)


;;;; done

(flush-output-port (current-output-port))
(flush-output-port (current-error-port))

(when #f
  (fprintf (current-error-port) "running gc ")
  (flush-output-port (current-error-port))
  (do ((i 0 (+ 1 i)))
      ((= i 1024))
    (fprintf (current-error-port) "~a " i)
    (flush-output-port (current-error-port))
    (collect))
  (check-newline))

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-temporary-file 'scheme-indent-function 1)
;; eval: (put 'catch 'scheme-indent-function 1)
;; eval: (put 'catch-error 'scheme-indent-function 1)
;; eval: (put 'px.set-struct-timespec-tv_sec! 'scheme-indent-function 1)
;; eval: (put 'px.clock-gettime 'scheme-indent-function 1)
;; End:
