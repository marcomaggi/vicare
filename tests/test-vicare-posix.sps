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

;;; --------------------------------------------------------------------

  (when #f

    (check
	(let ((rv (px.getnetbyname "loopback")))
	  (check-pretty-print rv)
	  (px.struct-netent? rv))
      => #t)

    (check
	(let ((rv (px.getnetbyaddr (bytevector-u32-ref '#vu8(127 0 0 0) 0 (endianness big))
				   AF_INET)))
	  (check-pretty-print rv)
	  (px.struct-netent? rv))
      => #t)

    (check
	(for-all px.struct-netent? (px.network-entries))
      => #t)

    (check-pretty-print (px.network-entries))

    #f)

  #t)


(parametrise ((check-test-name	'net))

  (check
      (let-values (((a b) (px.socketpair AF_LOCAL SOCK_DGRAM 0)))
	(px.posix-write a '#vu8(1 2 3 4) 4)
	(let ((buf (make-bytevector 4)))
	  (px.posix-read b buf 4)
	  (px.shutdown a SHUT_RDWR)
	  (px.shutdown b SHUT_RDWR)
	  buf))
    => '#vu8(1 2 3 4))

;;; --------------------------------------------------------------------
;;; options

  (check
      (let* ((sock   (px.socket AF_LOCAL SOCK_STREAM 0))
	     (result (px.getsockopt/int sock SOL_SOCKET SO_DEBUG)))
	(px.shutdown sock SHUT_RDWR)
	result)
    => 0)

  (check
      (let ((sock   (px.socket AF_LOCAL SOCK_STREAM 0)))
	(px.setsockopt/int sock SOL_SOCKET SO_REUSEADDR 1)
	(let ((result (px.getsockopt/int sock SOL_SOCKET SO_REUSEADDR)))
	  (px.shutdown sock SHUT_RDWR)
	  result))
    => 1)

  ;;SETSOCKOPT fails with "permission denied".
  ;;
  ;; (check
  ;;     (with-compensations
  ;; 	(let ((sock (make-sock/c)))
  ;; 	  (px.setsockopt sock (socket-option debug) #t)
  ;; 	  (px.getsockopt sock (socket-option debug))))
  ;;   => #t)

;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option reuseaddr)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option reuseaddr) #t)
;; 	  (px.getsockopt sock (socket-option reuseaddr))))
;;     => #t)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option keepalive)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option keepalive) #t)
;; 	  (px.getsockopt sock (socket-option keepalive))))
;;     => #t)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option dontroute)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option dontroute) #t)
;; 	  (px.getsockopt sock (socket-option dontroute))))
;;     => #t)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option broadcast)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option broadcast) #t)
;; 	  (px.getsockopt sock (socket-option broadcast))))
;;     => #t)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option oobinline)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option oobinline) #t)
;; 	  (px.getsockopt sock (socket-option oobinline))))
;;     => #t)

;; ;;; --------------------------------------------------------------------

;;   ;; (check
;;   ;;     (with-compensations
;;   ;; 	(px.getsockopt (make-sock/c) (socket-option sndbuf)))
;;   ;;   => 111616)

;; ;;;This test fails  for no documented reason; it looks  like the size of
;; ;;;the buffer  set by  SO_SNDBUF is a  suggestion to the  system, rather
;; ;;;than  a  strict  order.   See  also  the  proof  C  language  program
;; ;;;"src/proofs/socket-options.c".
;; ;;;
;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option sndbuf) 1000)
;; 	  (px.getsockopt sock (socket-option sndbuf))))
;;     => 1000)

;; ;;; --------------------------------------------------------------------

;;   ;; (check
;;   ;;     (with-compensations
;;   ;; 	(px.getsockopt (make-sock/c) (socket-option rcvbuf)))
;;   ;;   => 111616)

;; ;;;This test fails  for no documented reason; it looks  like the size of
;; ;;;the buffer  set by  SO_RCVBUF is a  suggestion to the  system, rather
;; ;;;than  a  strict  order.   See  also  the  proof  C  language  program
;; ;;;"src/proofs/socket-options.c".
;; ;;;
;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option rcvbuf) 1000)
;; 	  (px.getsockopt sock (socket-option rcvbuf))))
;;     => 1000)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option linger)))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option linger) 100)
;; 	  (px.getsockopt sock (socket-option linger))))
;;     => 100)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option linger) 0)
;; 	  (px.getsockopt sock (socket-option linger))))
;;     => #f)

;;   (check
;;       (with-compensations
;; 	(let ((sock (make-sock/c)))
;; 	  (px.setsockopt sock (socket-option linger) 100)
;; 	  (px.setsockopt sock (socket-option linger) #f)
;; 	  (px.getsockopt sock (socket-option linger))))
;;     => #f)

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option style)))
;;     (=> enum-set=?) (socket-style stream))

;; ;;; --------------------------------------------------------------------

;;   (check
;;       (with-compensations
;; 	(px.getsockopt (make-sock/c) (socket-option error)))
;;     => 0)

;;   #t)

;; 
;; (parametrise ((check-test-name	'local-stream))

;;   (define (make-sock/c)
;;     (letrec ((sock (compensate
;; 		       (px.socket* (namespace local) (style stream))
;; 		     (with
;; 		      (px.close sock)))))
;;       sock))

;;   (check	;strings
;;       (with-result
;;        (with-compensations
;; 	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
;; 		(sockaddr	(make-<sockaddr-un> pathname)))
;; 	   (when (file-exists? pathname)
;; 	     (px.unlink pathname))
;; 	   (push-compensation (when (file-exists? pathname)
;; 				(px.unlink pathname)))
;; 	   (letrec ((master-sock (make-sock/c))
;; 		    (client-sock (make-sock/c)))
;; 	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	     (px.bind   master-sock sockaddr)
;; 	     (px.listen master-sock 2)
;; 	     (px.connect client-sock sockaddr)
;; 	     (receive (server-sock client-address)
;; 		 (px.accept master-sock)
;; 	       (push-compensation (px.close server-sock))
;; 	       (add-result (<sockaddr-un>? sockaddr))
;; 	       (px.send/string server-sock "ciao client")
;; 	       (add-result (px.recv/string client-sock 100))
;; 	       (px.send/string client-sock "ciao server")
;; 	       (add-result (px.recv/string server-sock 100))
;; 	       (px.send/string server-sock "quit client")
;; 	       (add-result (px.recv/string client-sock 100))
;; 	       (px.send/string client-sock "quit server")
;; 	       (add-result (px.recv/string server-sock 100))
;; 	       (px.shutdown* server-sock both)
;; 	       (px.shutdown* client-sock both)
;; 	       #t)))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check 	;bytevectors
;;       (with-result
;;        (with-compensations
;; 	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
;; 		(sockaddr	(make-<sockaddr-un> pathname)))
;; 	   (when (file-exists? pathname)
;; 	     (px.unlink pathname))
;; 	   (push-compensation (when (file-exists? pathname)
;; 				(px.unlink pathname)))
;; 	   (letrec ((master-sock (make-sock/c))
;; 		    (client-sock (make-sock/c)))
;; 	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	     (px.bind   master-sock sockaddr)
;; 	     (px.listen master-sock 2)
;; 	     (px.connect client-sock sockaddr)
;; 	     (receive (server-sock client-address)
;; 		 (px.accept master-sock)
;; 	       (push-compensation (px.close server-sock))
;; 	       (add-result (<sockaddr-un>? sockaddr))
;; 	       (px.send/bytevector server-sock (string->utf8 "ciao client"))
;; 	       (add-result (utf8->string (px.recv/bytevector client-sock 100)))
;; 	       (px.send/bytevector client-sock (string->utf8 "ciao server"))
;; 	       (add-result (utf8->string (px.recv/bytevector server-sock 100)))
;; 	       (px.send/bytevector server-sock (string->utf8 "quit client"))
;; 	       (add-result (utf8->string (px.recv/bytevector client-sock 100)))
;; 	       (px.send/bytevector client-sock (string->utf8 "quit server"))
;; 	       (add-result (utf8->string (px.recv/bytevector server-sock 100)))
;; 	       #t)))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check	;memblocks
;;       (with-result
;;        (with-compensations
;; 	 (let* ((pathname	(string-append (px.getenv "TMPDIR") "/proof"))
;; 		(sockaddr	(make-<sockaddr-un> pathname)))
;; 	   (when (file-exists? pathname)
;; 	     (px.unlink pathname))
;; 	   (push-compensation (when (file-exists? pathname)
;; 				(px.unlink pathname)))
;; 	   (letrec ((master-sock (make-sock/c))
;; 		    (client-sock (make-sock/c)))
;; 	     (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	     (px.bind   master-sock sockaddr)
;; 	     (px.listen master-sock 2)
;; 	     (px.connect client-sock sockaddr)
;; 	     (receive (server-sock client-address)
;; 		 (px.accept master-sock)
;; 	       (push-compensation (px.close server-sock))
;; 	       (add-result (<sockaddr-un>? sockaddr))
;; 	       (px.send/memblock server-sock (string->memblock "ciao client" malloc/c))
;; 	       (add-result (memblock->string (px.recv/memblock client-sock 100 malloc/c)))
;; 	       (px.send/memblock client-sock (string->memblock "ciao server" malloc/c))
;; 	       (add-result (memblock->string (px.recv/memblock server-sock 100 malloc/c)))
;; 	       (px.send/memblock server-sock (string->memblock "quit client" malloc/c))
;; 	       (add-result (memblock->string (px.recv/memblock client-sock 100 malloc/c)))
;; 	       (px.send/memblock client-sock (string->memblock "quit server" malloc/c))
;; 	       (add-result (memblock->string (px.recv/memblock server-sock 100 malloc/c)))
;; 	       #t)))))
;;     => '(#t (#t "ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check	;processes
;;       (with-compensations
;; 	(let* ((pathname (string-append (px.getenv "TMPDIR") "/proof"))
;; 	       (sockaddr (make-<sockaddr-un> pathname)))
;; 	  (when (file-exists? pathname)
;; 	    (px.unlink pathname))
;; 	  (let ((pid (px.fork)))
;; 	    (if pid
;; 		(begin ;parent, server
;; 		  (push-compensation (when (file-exists? pathname)
;; 				       (px.remove pathname)))
;; 		  (letrec ((server-sock (make-sock/c)))
;; 		    (px.setsockopt server-sock (socket-option reuseaddr) #t)
;; 		    (px.bind   server-sock sockaddr)
;; 		    (px.listen server-sock 2)
;; 		    (receive (client-sock client-address)
;; 			(px.accept server-sock)
;; 		      (push-compensation (px.close client-sock))
;; 		      (with-result
;; 		       (add-result (<sockaddr-un>? client-address))
;; 		       (px.send/string client-sock "ciao client")
;; 		       (add-result (px.recv/string client-sock 100))
;; 		       (px.send/string client-sock "quit client")
;; 		       (add-result (px.recv/string client-sock 100))
;; 		       (receive (pid1 status)
;; 			   (px.waitpid pid 0)
;; 			 (pid=? pid pid1))))))
;; 	      (begin ;child, client
;; 		(letrec ((client-sock (make-sock/c)))
;; 		  ;;Give the  server some time to setup  itself and reach
;; 		  ;;the ACCEPT call.
;; 		  (wait-for-awhile)
;; 		  (px.connect client-sock sockaddr)
;; 		  (px.recv/string client-sock 100)
;; 		  (px.send/string client-sock "ciao server")
;; 		  (px.recv/string client-sock 100)
;; 		  (px.send/string client-sock "quit server")
;; 		  (exit)))))))
;;     => '(#t (#t "ciao server" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (let* ((pathname (string-append (px.getenv "TMPDIR") "/proof")))
;;     (when (file-exists? pathname)
;;       (px.unlink pathname))

;;     (check	;get peer address
;; 	(with-result
;; 	 (with-compensations
;; 	   (letrec ((sockaddr (make-<sockaddr-un> pathname)))
;; 	     (letrec ((master-sock (make-sock/c))
;; 		      (client-sock (make-sock/c)))
;; 	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	       (compensate
;; 		   (px.bind master-sock sockaddr)
;; 		 (with
;; 		  (when (file-exists? pathname)
;; 		    (px.unlink pathname))))
;; 	       (px.listen master-sock 2)
;; 	       (px.connect client-sock sockaddr)
;; 	       (let ((client-sockaddr (px.getsockname client-sock))
;; 		     (server-sockaddr (px.getpeername client-sock)))
;; 		 (add-result (<sockaddr-un>-pathname server-sockaddr))
;; 		 (add-result (<sockaddr-un>-pathname client-sockaddr))
;; 		 #t)))))
;;       => `(#t (,pathname ""))))

;;   #t)

;; 
;; (parametrise ((check-test-name	'local-datagram))

;;   (define (make-sock/c)
;;     (letrec ((sock (compensate
;; 		       (px.socket* (namespace local)
;; 				      (style datagram))
;; 		     (with
;; 		      (px.close sock)))))
;;       sock))

;;   (check	;strings
;;       (with-result
;;        (with-compensations
;; 	 (let* ((TMPDIR		(px.getenv "TMPDIR"))
;; 		(one-pathname	(string-append TMPDIR "/proof-one"))
;; 		(two-pathname	(string-append TMPDIR "/proof-two")))
;; 	   (when (file-exists? one-pathname) (px.unlink one-pathname))
;; 	   (when (file-exists? two-pathname) (px.unlink two-pathname))
;; 	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
;; 		 (two-addr	(make-<sockaddr-un> two-pathname)))
;; 	     (push-compensation (when (file-exists? one-pathname)
;; 				  (px.unlink one-pathname)))
;; 	     (push-compensation (when (file-exists? two-pathname)
;; 				  (px.unlink two-pathname)))
;; 	     (let ((one-sock (make-sock/c))
;; 		   (two-sock (make-sock/c)))
;; 	       (px.bind one-sock one-addr)
;; 	       (px.bind two-sock two-addr)
;; 	       (px.sendto/string one-sock "ciao i'm one" two-addr)
;; 	       (receive (result peer-address)
;; 		   (px.recvfrom/string two-sock 100)
;; 		 (add-result result)
;; 		 (px.sendto/string two-sock "ciao i'm two" peer-address))
;; 	       (receive (result peer-address)
;; 		   (px.recvfrom/string one-sock 100)
;; 		 (add-result result)
;; 		 (px.sendto/string one-sock "one quits" peer-address))
;; 	       (receive (result peer-address)
;; 		   (px.recvfrom/string two-sock 100)
;; 		 (add-result result)
;; 		 (px.sendto/string one-sock "two quits" peer-address))
;; 	       (receive (result peer-address)
;; 	       	 (px.recvfrom/string one-sock 100)
;; 	         (add-result result))
;; 	       #t)))))
;;     => '(#t ("ciao i'm one" "ciao i'm two" "one quits" "two quits")))

;; ;;; --------------------------------------------------------------------

;;   (check	;processes
;;       (with-result
;;        (with-compensations
;; 	 (let* ((TMPDIR		(px.getenv "TMPDIR"))
;; 		(one-pathname	(string-append TMPDIR "/proof-one"))
;; 		(two-pathname	(string-append TMPDIR "/proof-two")))
;; 	   (when (file-exists? one-pathname) (px.unlink one-pathname))
;; 	   (when (file-exists? two-pathname) (px.unlink two-pathname))
;; 	   (let ((one-addr	(make-<sockaddr-un> one-pathname))
;; 		 (two-addr	(make-<sockaddr-un> two-pathname)))
;; 	     (push-compensation (when (file-exists? one-pathname)
;; 				  (px.unlink one-pathname)))
;; 	     (push-compensation (when (file-exists? two-pathname)
;; 				  (px.unlink two-pathname)))
;; 	     (let ((pid (px.fork)))
;; 	       (if pid
;; 		   (let ((sock (make-sock/c))) ;parent, one
;; 		     (px.bind sock one-addr)
;; 		     (wait-for-awhile)
;; 		     (px.sendto/string sock "ciao i'm one" two-addr)
;; 		     (receive (result peer-address)
;; 			 (px.recvfrom/string sock 100)
;; 		       (add-result result)
;; 		       (px.sendto/string sock "one quits" peer-address))
;; 		     (receive (result peer-address)
;; 			 (px.recvfrom/string sock 100)
;; 		       (add-result result))
;; 		     (receive (pid1 status)
;; 			 (px.waitpid pid 0)
;; 		       (pid=? pid pid1)))
;; 		 (let ((sock (make-sock/c))) ;child, two
;; 		   (px.bind sock two-addr)
;; 		   (wait-for-awhile)
;; 		   (receive (result peer-address)
;; 		       (px.recvfrom/string sock 100)
;; 		     (px.sendto/string sock "ciao i'm two" peer-address))
;; 		   (receive (result peer-address)
;; 		       (px.recvfrom/string sock 100)
;; 		     (px.sendto/string sock "two quits" peer-address))
;; 		   (exit))))))))
;;     => '(#t ("ciao i'm two" "two quits")))

;; ;;; --------------------------------------------------------------------

;;   (let* ((TMPDIR	(px.getenv "TMPDIR"))
;; 	 (one-pathname	(string-append TMPDIR "/proof-one"))
;; 	 (two-pathname	(string-append TMPDIR "/proof-two")))

;;     (check	;get sock name
;; 	;;Notice that  on Linux GETPEERNAME does not  work with datagram
;; 	;;sockets, it raises the "endpoint not connected" error.
;; 	(with-result
;; 	 (with-compensations
;; 	   (when (file-exists? one-pathname) (px.unlink one-pathname))
;; 	   (when (file-exists? two-pathname) (px.unlink two-pathname))
;; 	   (let ((one-addr (make-<sockaddr-un> one-pathname))
;; 		 (two-addr (make-<sockaddr-un> two-pathname))
;; 		 (one-sock (make-sock/c))
;; 		 (two-sock (make-sock/c)))
;; 	       (compensate
;; 		   (px.bind one-sock one-addr)
;; 		 (with
;; 		  (when (file-exists? one-pathname)
;; 		    (px.unlink one-pathname))))
;; 	       (compensate
;; 		   (px.bind two-sock two-addr)
;; 		 (with
;; 		  (when (file-exists? two-pathname)
;; 		    (px.unlink two-pathname))))
;; 	       (px.sendto/string one-sock "ciao i'm one" two-addr)
;; 	       (let ((one-sockaddr (px.getsockname one-sock)))
;; 		 (add-result (<sockaddr-un>-pathname one-sockaddr))
;; 		 #t))))
;;       => `(#t (,one-pathname))))

;;   #t)

;; 
;; (parametrise ((check-test-name	'inet-stream))

;;   (define (make-sock/c)
;;     (letrec ((sock (compensate
;; 		       (px.socket* (namespace inet)
;; 				      (style stream))
;; 		     (with
;; 		      (px.close sock)))))
;;       sock))

;;   (define (make-sock6/c)
;;     (letrec ((sock (compensate
;; 		       (px.socket* (namespace inet6)
;; 				      (style stream))
;; 		     (with
;; 		      (px.close sock)))))
;;       sock))

;;   (check	;inet
;;       (with-result
;;        (with-compensations
;;   	 (let ((hostent (px.gethostbyname "localhost")))
;; 	   (when hostent
;; 	     (let ((sockaddr	(make-<sockaddr-in> (car (<hostent>-addrlist hostent)) tcp/ip-port))
;; 		   (master-sock	(make-sock/c))
;; 		   (client-sock	(make-sock/c)))
;; 	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	       (px.bind   master-sock sockaddr)
;; 	       (px.listen master-sock 2)
;; 	       (px.connect client-sock sockaddr)
;; 	       (receive (server-sock client-address)
;; 		   (px.accept master-sock)
;; 		 (push-compensation (px.close server-sock))
;; 		 (px.send/string server-sock "ciao client")
;; 		 (add-result (px.recv/string client-sock 100))
;; 		 (px.send/string client-sock "ciao server")
;; 		 (add-result (px.recv/string server-sock 100))
;; 		 (px.send/string server-sock "quit client")
;; 		 (add-result (px.recv/string client-sock 100))
;; 		 (px.send/string client-sock "quit server")
;; 		 (add-result (px.recv/string server-sock 100))
;; 		 #t))))))
;;     => '(#t ("ciao client" "ciao server" "quit client" "quit server")))

;;   (check	;inet6
;;       (with-result
;;        (with-compensations
;; 	 (let ((sockaddr	(make-<sockaddr-in6> '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) tcp/ip-port))
;; 	       (master-sock	(make-sock6/c))
;; 	       (client-sock	(make-sock6/c)))
;; 	   (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	   (px.bind   master-sock sockaddr)
;; 	   (px.listen master-sock 2)
;; 	   (px.connect client-sock sockaddr)
;; 	   (receive (server-sock client-address)
;; 	       (px.accept master-sock)
;; 	     (push-compensation (px.close server-sock))
;; 	     (px.send/string server-sock "ciao client")
;; 	     (add-result (px.recv/string client-sock 100))
;; 	     (px.send/string client-sock "ciao server")
;; 	     (add-result (px.recv/string server-sock 100))
;; 	     (px.send/string server-sock "quit client")
;; 	     (add-result (px.recv/string client-sock 100))
;; 	     (px.send/string client-sock "quit server")
;; 	     (add-result (px.recv/string server-sock 100))
;; 	     #t))))
;;     => '(#t ("ciao client" "ciao server" "quit client" "quit server")))

;; ;;; --------------------------------------------------------------------

;;   (check	;get peer name, inet
;;       (with-result
;;        (with-compensations
;;   	 (let ((hostent (px.gethostbyname "localhost")))
;; 	   (when hostent
;; 	     (let ((sockaddr	(make-<sockaddr-in> (car (<hostent>-addrlist hostent)) tcp/ip-port))
;; 		   (master-sock	(make-sock/c))
;; 		   (client-sock	(make-sock/c)))
;; 	       (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	       (px.bind   master-sock sockaddr)
;; 	       (px.listen master-sock 2)
;; 	       (px.connect client-sock sockaddr)
;; 	       (receive (server-sock client-address)
;; 		   (px.accept master-sock)
;; 		 (let ((master-sockaddr (px.getsockname master-sock))
;; 		       (server-sockaddr (px.getsockname server-sock))
;; 		       (client-sockaddr (px.getpeername server-sock)))
;; 		   (add-result (px.inet-ntoa (<sockaddr-in>-addr server-sockaddr)))
;; 		   (add-result (px.inet-ntoa (<sockaddr-in>-addr client-sockaddr)))
;; 		   (add-result (<sockaddr-in>-port master-sockaddr))
;; 		   #t)))))))
;;     => `(#t ("127.0.0.1" "127.0.0.1" ,tcp/ip-port)))

;;   (check	;get peer name, inet6
;;       (with-result
;;        (with-compensations
;; 	 (let ((sockaddr	(make-<sockaddr-in6> '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) tcp/ip-port))
;; 	       (master-sock	(make-sock6/c))
;; 	       (client-sock	(make-sock6/c)))
;; 	   (px.setsockopt master-sock (socket-option reuseaddr) #t)
;; 	   (px.bind   master-sock sockaddr)
;; 	   (px.listen master-sock 2)
;; 	   (px.connect client-sock sockaddr)
;; 	   (receive (server-sock client-address)
;; 	       (px.accept master-sock)
;; 	     (let ((master-sockaddr (px.getsockname master-sock))
;; 		   (server-sockaddr (px.getsockname server-sock))
;; 		   (client-sockaddr (px.getpeername server-sock)))
;; 	       (add-result (px.inet-ntop (socket-address-format inet6)
;; 					    (<sockaddr-in6>-addr server-sockaddr)))
;; 	       (add-result (px.inet-ntop (socket-address-format inet6)
;; 					    (<sockaddr-in6>-addr client-sockaddr)))
;; 	       (add-result (<sockaddr-in6>-port master-sockaddr))
;; 	       #t)))))
;;     => `(#t ("::1" "::1" ,tcp/ip-port)))

;;   #t)

;; 
;; (parametrise ((check-test-name	'socketpair))

;;   (check
;;       (with-result
;;        (with-compensations
;;   	 (receive (one-sock two-sock)
;; 	     (px.socketpair* (namespace local) (style stream))
;; 	   (push-compensation (px.close one-sock))
;; 	   (push-compensation (px.close two-sock))
;; 	   (px.send/string one-sock "ciao two")
;; 	   (add-result (px.recv/string two-sock 100))
;; 	   (px.send/string two-sock "ciao one")
;; 	   (add-result (px.recv/string one-sock 100))
;; 	   (px.send/string one-sock "quit two")
;; 	   (add-result (px.recv/string two-sock 100))
;; 	   (px.send/string two-sock "quit one")
;; 	   (add-result (px.recv/string one-sock 100))
;; 	   #t)))
;;     => '(#t ("ciao two" "ciao one" "quit two" "quit one")))

;;   #t)


  #t)


;;;; done

(check-report)

;;; end of file
;; Local Variables:
;; eval: (put 'with-temporary-file 'scheme-indent-function 1)
;; eval: (put 'catch 'scheme-indent-function 1)
;; eval: (put 'catch-error 'scheme-indent-function 1)
;; End:
