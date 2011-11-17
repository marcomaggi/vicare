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


#!vicare
(import (rename (vicare) #;(ikarus)
		(parameterize	parametrise))
  (prefix (vicare posix)
	  px.)
  (vicare platform-constants)
  (vicare syntactic-extensions)
  (checks))

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

;;;  (pretty-print (environ))
;;;  (pretty-print (hashtable-keys (px.environ-table)))(newline)

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


(parametrise ((check-test-name	'system))

  (check
      (px.system "echo innocuous output from 'system()' call ; exit 0")
    => 0)

  #t)


(parametrise ((check-test-name	'fork))

  (check
      (px.fork (lambda (child-pid)
		 (display (format "after fork in parent, parent pid=~s, child pid=~s\n"
			    (px.getpid) child-pid)
			  (current-error-port))
		 #t)
	       (lambda ()
		 (display (format "after fork in child,  parent pid=~s, child pid=~s\n"
			    (px.getppid) (px.getpid))
			  (current-error-port))
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
      (let ((S (lstat "Makefile")))
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
	(px.chown "tmp" 1000 1000))
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
  	      (px.posix-remove "one")
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
		(px.posix-write fd '#vu8(1 2 3 4) 4)
		(px.lseek fd 0 SEEK_SET)
		(let ((buffer (make-bytevector 4)))
		  (list (px.posix-read fd buffer 4) buffer)))
	    (px.close fd))))
    => '(4 #vu8(1 2 3 4)))

  (check
      (begin
	(px.system "rm -f tmp")
	(let ((fd (px.open "tmp"
			   (fxior O_CREAT O_EXCL O_RDWR)
			   (fxior S_IRUSR S_IWUSR))))
	  (unwind-protect
	      (begin
		(px.pwrite fd '#vu8(1 2 3 4) 4 0)
		(px.lseek fd 0 SEEK_SET)
		(let ((buffer (make-bytevector 4)))
		  (list (px.pread fd buffer 4 0) buffer)))
	    (px.close fd))))
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
	   (px.close fd))))
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
	    (px.close fd))))
    => #t)

;;; --------------------------------------------------------------------
;;; pipe

  (check
      (let-values (((in ou) (px.pipe)))
	(px.posix-write ou '#vu8(1 2 3 4) 4)
	(let ((bv (make-bytevector 4)))
	  (px.posix-read in bv 4)
	  bv))
    => '#vu8(1 2 3 4))

  (check	;raw pipes to child process
      (let-values (((child-stdin       parent-to-child) (px.pipe))
		   ((parent-from-child child-stdout)    (px.pipe)))
	(px.fork (lambda (pid) ;parent
		   (let ((buf (make-bytevector 1)))
		     (px.posix-read  parent-from-child buf 1)
		     (px.posix-write parent-to-child   '#vu8(2) 1)
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
		     (px.posix-write 1 '#vu8(1) 1)
		     (px.posix-read  0 buf 1)
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

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;timeout
	    (let-values (((r w e) (px.select #f `(,in) '() `(,in ,ou) 0 0)))
	      (list r w e))
	  => '(() () ()))
      (px.close in)
      (px.close ou)))

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;read ready
	    (begin
	      (px.posix-write ou '#vu8(1) 1)
	      (let-values (((r w e) (px.select #f `(,in) '() `(,in) 0 0)))
		(list r w e)))
	  => `((,in) () ()))
      (px.close in)
      (px.close ou)))

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;write ready
	    (let-values (((r w e) (px.select #f '() `(,ou) `(,ou) 0 0)))
	      (list r w e))
	  => `(() (,ou) ()))
      (px.close in)
      (px.close ou)))

;;; --------------------------------------------------------------------
;;; select-fd

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;timeout
	    (let-values (((r w e) (px.select-fd in 0 0)))
	      (list r w e))
	  => '(#f #f #f))
      (px.close in)
      (px.close ou)))

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;read ready
	    (begin
	      (px.posix-write ou '#vu8(1) 1)
	      (let-values (((r w e) (px.select-fd in 0 0)))
		(list r w e)))
	  => `(,in #f #f))
      (px.close in)
      (px.close ou)))

  (let-values (((in ou) (px.pipe)))
    (unwind-protect
	(check	;write ready
	    (let-values (((r w e) (px.select-fd ou 0 0)))
	      (list r w e))
	  => `(#f ,ou #f))
      (px.close in)
      (px.close ou)))

  #t)


(parametrise ((check-test-name	'sockets))

  (check
      (px.sockaddr_un.pathname/string (px.make-sockaddr_un "/tmp/marco/the-unix-socket"))
    => "/tmp/marco/the-unix-socket")

;;; --------------------------------------------------------------------

  (check
      (let ((sockaddr (px.make-sockaddr_in '#vu8(1 2 3 4) 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(1 2 3 4) 88))

  (check
      (let* ((addr	(let ((bv (make-bytevector 4)))
			  (bytevector-u32-set! bv 0 INADDR_LOOPBACK (endianness big))
			  bv))
	     (sockaddr (px.make-sockaddr_in addr 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(127 0 0 1) 88))

  (check
      (let* ((addr	(let ((bv (make-bytevector 4)))
			  (bytevector-u32-set! bv 0 INADDR_BROADCAST (endianness big))
			  bv))
	     (sockaddr (px.make-sockaddr_in addr 88)))
	(list (px.sockaddr_in.in_addr sockaddr)
	      (px.sockaddr_in.in_port sockaddr)))
    => '(#vu8(255 255 255 255) 88))

;;; --------------------------------------------------------------------

  (check
      (let ((sockaddr (px.make-sockaddr_in6 '#vu16b(1 2 3 4  5 6 7 8) 88)))
	(list (px.sockaddr_in6.in6_addr sockaddr)
	      (px.sockaddr_in6.in6_port sockaddr)))
    => '(#vu16b(1 2 3 4  5 6 7 8) 88))

;;; --------------------------------------------------------------------

  (check
      (px.in6addr_loopback)
    => '#vu8(0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 1))

  (check
      (px.in6addr_any)
    => '#vu8(0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0))

;;; --------------------------------------------------------------------

  (check
      (px.inet-aton "127.0.0.1")
    => '#vu8(127 0 0 1))

  (check
      (catch-error #f
	(px.inet-aton "ciao"))
    => '("ciao"))

  (check
      (px.inet-ntoa/string '#vu8(127 0 0 1))
    => "127.0.0.1")

;;; --------------------------------------------------------------------

  (check
      (px.inet-pton AF_INET "127.0.0.1")
    => '#vu8(127 0 0 1))

  (check
      (catch-error #f
	(px.inet-pton AF_INET "ciao"))
    => `(,AF_INET "ciao"))

  (check
      (px.inet-pton AF_INET6 "1:2:3:4:5:6:7:8")
    => '#vu16b(1 2 3 4 5 6 7 8))

  (check
      (px.inet-ntop/string AF_INET '#vu8(127 0 0 1))
    => "127.0.0.1")

  (check
      (px.inet-ntop/string AF_INET6 '#vu16b(1 2 3 4 5 6 7 8))
    => "1:2:3:4:5:6:7:8")

;;; --------------------------------------------------------------------

  (check
      (let ((S (px.gethostbyname "localhost")))
;;;(check-pretty-print S)
	(list (px.struct-hostent? S)
	      (utf8->string (px.struct-hostent-h_name S))
	      (px.struct-hostent-h_aliases   S)
	      (px.struct-hostent-h_addrtype  S)
	      (px.struct-hostent-h_length    S)
	      (px.struct-hostent-h_addr_list S)
	      (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" () ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (let ((S (px.gethostbyname2 "localhost" AF_INET)))
;;;(check-pretty-print S)
	(list (px.struct-hostent? S)
	      (utf8->string (px.struct-hostent-h_name S))
	      (px.struct-hostent-h_aliases   S)
	      (px.struct-hostent-h_addrtype  S)
	      (px.struct-hostent-h_length    S)
	      (px.struct-hostent-h_addr_list S)
	      (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" () ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (let ((S (px.gethostbyaddr '#vu8(127 0 0 1))))
;;;(check-pretty-print S)
	(list (px.struct-hostent? S)
	      (utf8->string (px.struct-hostent-h_name S))
	      (px.struct-hostent-h_aliases   S)
	      (px.struct-hostent-h_addrtype  S)
	      (px.struct-hostent-h_length    S)
	      (px.struct-hostent-h_addr_list S)
	      (px.struct-hostent-h_addr      S)))
    => `(#t "localhost" () ,AF_INET 4 (#vu8(127 0 0 1)) #vu8(127 0 0 1)))

  (check
      (for-all px.struct-hostent? (px.host-entries))
    => #t)

;;;  (check-pretty-print (cons '/etc/hosts (px.host-entries)))

;;; --------------------------------------------------------------------

  ;;GETADDRINFO  works only  when we  are  connected on  the Net  (Marco
  ;;Maggi; Thu Nov 17, 2011).
  (when #f
    (check
	(let* ((hints	(make-struct-addrinfo AI_CANONNAME AF_INET SOCK_STREAM 0 #f #f #f))
	       (rv	(px.getaddrinfo "localhost" "smtp" hints)))
	  (for-all struct-addrinfo? rv))
      => #t)

    (check
	(let ((rv (px.getaddrinfo "localhost" "smtp" #f)))
	  (for-all struct-addrinfo? rv))
      => #t)

    (check
	(let ((rv (px.getaddrinfo "localhost" #f #f)))
	  (for-all struct-addrinfo? rv))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (check
      (let ((rv (px.getprotobyname "icmp")))
;;;	(check-pretty-print rv)
	(px.struct-protoent? rv))
    => #t)

  (check
      (let ((rv (px.getprotobyname "udp")))
;;;	(check-pretty-print rv)
	(px.struct-protoent? rv))
    => #t)

  (check
      (let ((rv (px.getprotobyname "tcp")))
;;;	(check-pretty-print rv)
	(px.struct-protoent? rv))
    => #t)

  (check
      (let ((rv (px.getprotobynumber 6)))
;;;	(check-pretty-print rv)
	(px.struct-protoent? rv))
    => #t)

;;;  (check-pretty-print (px.protocol-entries))

;;; --------------------------------------------------------------------

  (check
      (let ((rv (px.getservbyname "smtp" "tcp")))
;;;	(check-pretty-print rv)
	(px.struct-servent? rv))
    => #t)

  (check
      (let ((rv (px.getservbyname "http" "tcp")))
;;;	(check-pretty-print rv)
	(px.struct-servent? rv))
    => #t)

  (check
      (let ((rv (px.getservbyname "ntp" "tcp")))
;;;	(check-pretty-print rv)
	(px.struct-servent? rv))
    => #t)

  (check
      (let ((rv (px.getservbyport 25 "tcp")))
;;;	(check-pretty-print rv)
	(px.struct-servent? rv))
    => #t)

  (check
      (for-all px.struct-servent? (px.service-entries))
    => #t)

;;;  (check-pretty-print (px.service-entries))

  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-temporary-file 'scheme-indent-function 1)
;; eval: (put 'catch 'scheme-indent-function 1)
;; eval: (put 'catch-error 'scheme-indent-function 1)
;; End:
