/*
  Part of: Vicare Scheme
  Contents: print platform features library
  Date: Sat Jul 21, 2012

  Abstract



  Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>


int
main (int argc, const char *const argv[])
{
  printf(";;; -*- coding: utf-8-unix -*-\n\
;;;\n\
;;;Part of: Vicare Scheme\n\
;;;Contents: static platform inspection\n\
;;;Date: Sat Jul 21, 2012\n\
;;;\n\
;;;Abstract\n\
;;;\n\
;;;\n\
;;;\n\
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>\n\
;;;\n\
;;;This program is free software:  you can redistribute it and/or modify\n\
;;;it under the terms of the  GNU General Public License as published by\n\
;;;the Free Software Foundation, either version 3 of the License, or (at\n\
;;;your option) any later version.\n\
;;;\n\
;;;This program is  distributed in the hope that it  will be useful, but\n\
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of\n\
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU\n\
;;;General Public License for more details.\n\
;;;\n\
;;;You should  have received a  copy of  the GNU General  Public License\n\
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
;;;\n\
\n\
\n\
#!r6rs\n\
(library (vicare platform features)\n\
  (export\n\
    HAVE_ACCEPT\n\
    HAVE_ACCESS\n\
    HAVE_ACOSH\n\
    HAVE_ALARM\n\
    HAVE_ARPA_INET_H\n\
    HAVE_ASINH\n\
    HAVE_ASSERT_H\n\
    HAVE_ATANH\n\
    HAVE_BIND\n\
    HAVE_CACOS\n\
    HAVE_CACOSH\n\
    HAVE_CASIN\n\
    HAVE_CASINH\n\
    HAVE_CATAN\n\
    HAVE_CATANH\n\
    HAVE_CCOS\n\
    HAVE_CCOSH\n\
    HAVE_CEXP\n\
    HAVE_CHDIR\n\
    HAVE_CHMOD\n\
    HAVE_CHOWN\n\
    HAVE_CLEARENV\n\
    HAVE_CLOCK\n\
    HAVE_CLOCK_GETCPUCLOCKID\n\
    HAVE_CLOCK_GETRES\n\
    HAVE_CLOCK_GETTIME\n\
    HAVE_CLOCK_SETTIME\n\
    HAVE_CLOG\n\
    HAVE_CLOG10\n\
    HAVE_CLOSE\n\
    HAVE_CLOSEDIR\n\
    HAVE_COMPLEX_H\n\
    HAVE_CONFIG_H\n\
    HAVE_CONFSTR\n\
    HAVE_CONNECT\n\
    HAVE_COSH\n\
    HAVE_CPOW\n\
    HAVE_CSIN\n\
    HAVE_CSINH\n\
    HAVE_CSQRT\n\
    HAVE_CTAN\n\
    HAVE_CTANH\n\
    HAVE_CTERMID\n\
    HAVE_DAEMON\n\
    HAVE_DECL_ENVIRON\n\
    HAVE_DIRENT_H\n\
    HAVE_DIRFD\n\
    HAVE_DUP\n\
    HAVE_DUP2\n\
    HAVE_ENDGRENT\n\
    HAVE_ENDHOSTENT\n\
    HAVE_ENDNETENT\n\
    HAVE_ENDPROTOENT\n\
    HAVE_ENDPWENT\n\
    HAVE_ENDSERVENT\n\
    HAVE_EPOLL_CREATE\n\
    HAVE_EPOLL_CREATE1\n\
    HAVE_EPOLL_CTL\n\
    HAVE_EPOLL_WAIT\n\
    HAVE_ERF\n\
    HAVE_ERFC\n\
    HAVE_ERRNO_H\n\
    HAVE_EXECV\n\
    HAVE_EXECVE\n\
    HAVE_EXECVP\n\
    HAVE_FCHDIR\n\
    HAVE_FCHMOD\n\
    HAVE_FCHOWN\n\
    HAVE_FCNTL\n\
    HAVE_FCNTL_H\n\
    HAVE_FDATASYNC\n\
    HAVE_FDOPENDIR\n\
    HAVE_FNMATCH\n\
    HAVE_FNMATCH_H\n\
    HAVE_FORK\n\
    HAVE_FPATHCONF\n\
    HAVE_FSYNC\n\
    HAVE_FTRUNCATE\n\
    HAVE_FUTIMES\n\
    HAVE_GAI_STRERROR\n\
    HAVE_GETADDRINFO\n\
    HAVE_GETCWD\n\
    HAVE_GETEGID\n\
    HAVE_GETENV\n\
    HAVE_GETEUID\n\
    HAVE_GETGID\n\
    HAVE_GETGRENT\n\
    HAVE_GETGRGID\n\
    HAVE_GETGRNAM\n\
    HAVE_GETGROUPS\n\
    HAVE_GETHOSTBYADDR\n\
    HAVE_GETHOSTBYNAME\n\
    HAVE_GETHOSTBYNAME2\n\
    HAVE_GETHOSTENT\n\
    HAVE_GETITIMER\n\
    HAVE_GETLOGIN\n\
    HAVE_GETNETBYADDR\n\
    HAVE_GETNETBYNAME\n\
    HAVE_GETNETENT\n\
    HAVE_GETPEERNAME\n\
    HAVE_GETPGRP\n\
    HAVE_GETPID\n\
    HAVE_GETPPID\n\
    HAVE_GETPROTOBYNAME\n\
    HAVE_GETPROTOBYNUMBER\n\
    HAVE_GETPROTOENT\n\
    HAVE_GETPWENT\n\
    HAVE_GETPWNAM\n\
    HAVE_GETPWUID\n\
    HAVE_GETRLIMIT\n\
    HAVE_GETRUSAGE\n\
    HAVE_GETSERVBYNAME\n\
    HAVE_GETSERVBYPORT\n\
    HAVE_GETSERVENT\n\
    HAVE_GETSID\n\
    HAVE_GETSOCKNAME\n\
    HAVE_GETSOCKOPT\n\
    HAVE_GETTIMEOFDAY\n\
    HAVE_GETUID\n\
    HAVE_GLOB\n\
    HAVE_GLOB_H\n\
    HAVE_GMTIME\n\
    HAVE_GMTIME_R\n\
    HAVE_GRP_H\n\
    HAVE_HISTORY_H\n\
    HAVE_ICONV\n\
    HAVE_ICONV_CLOSE\n\
    HAVE_ICONV_OPEN\n\
    HAVE_IF_INDEXTONAME\n\
    HAVE_IF_NAMEINDEX\n\
    HAVE_IF_NAMETOINDEX\n\
    HAVE_INET_ATON\n\
    HAVE_INET_NTOA\n\
    HAVE_INET_NTOP\n\
    HAVE_INET_PTON\n\
    HAVE_HTONL\n\
    HAVE_HTONS\n\
    HAVE_NTOHL\n\
    HAVE_NTOHS\n\
    HAVE_INOTIFY_ADD_WATCH\n\
    HAVE_INOTIFY_INIT\n\
    HAVE_INOTIFY_INIT1\n\
    HAVE_INOTIFY_RM_WATCH\n\
    HAVE_J0\n\
    HAVE_J1\n\
    HAVE_JN\n\
    HAVE_HYPOT\n\
    HAVE_CBRT\n\
    HAVE_KILL\n\
    HAVE_LGAMMA_R\n\
    HAVE_LIBFFI\n\
    HAVE_LIBICONV\n\
    HAVE_LIBREADLINE\n\
    HAVE_LIMITS_H\n\
    HAVE_LINK\n\
    HAVE_LISTEN\n\
    HAVE_LOCKF\n\
    HAVE_LOCALTIME_R\n\
    HAVE_LSEEK\n\
    HAVE_LSTAT\n\
    HAVE_LUTIMES\n\
    HAVE_MADVISE\n\
    HAVE_MATH_H\n\
    HAVE_MKDIR\n\
    HAVE_MKDTEMP\n\
    HAVE_MKFIFO\n\
    HAVE_MKSTEMP\n\
    HAVE_MKTIME\n\
    HAVE_MLOCK\n\
    HAVE_MLOCKALL\n\
    HAVE_MMAP\n\
    HAVE_MPROTECT\n\
    HAVE_MQUEUE_H\n\
    HAVE_MQ_CLOSE\n\
    HAVE_MQ_GETATTR\n\
    HAVE_MQ_OPEN\n\
    HAVE_MQ_RECEIVE\n\
    HAVE_MQ_SEND\n\
    HAVE_MQ_SETATTR\n\
    HAVE_MQ_TIMEDRECEIVE\n\
    HAVE_MQ_TIMEDSEND\n\
    HAVE_MQ_UNLINK\n\
    HAVE_MREMAP\n\
    HAVE_MSYNC\n\
    HAVE_MUNLOCK\n\
    HAVE_MUNLOCKALL\n\
    HAVE_MUNMAP\n\
    HAVE_NANOSLEEP\n\
    HAVE_NETDB_H\n\
    HAVE_NETINET_IN_H\n\
    HAVE_NET_IF_H\n\
    HAVE_OPEN\n\
    HAVE_OPENDIR\n\
    HAVE_PATHCONF\n\
    HAVE_PAUSE\n\
    HAVE_PIPE\n\
    HAVE_POLL\n\
    HAVE_POLL_H\n\
    HAVE_PREAD\n\
    HAVE_PRLIMIT\n\
    HAVE_PWD_H\n\
    HAVE_PWRITE\n\
    HAVE_RAISE\n\
    HAVE_RAND\n\
    HAVE_READ\n\
    HAVE_READDIR\n\
    HAVE_READLINE_H\n\
    HAVE_READLINE_HISTORY\n\
    HAVE_READLINE_HISTORY_H\n\
    HAVE_READLINE_READLINE_H\n\
    HAVE_READLINK\n\
    HAVE_READV\n\
    HAVE_REALPATH\n\
    HAVE_RECV\n\
    HAVE_RECVFROM\n\
    HAVE_REGCOMP\n\
    HAVE_REGEX_H\n\
    HAVE_REMOVE\n\
    HAVE_RENAME\n\
    HAVE_REWINDDIR\n\
    HAVE_RMDIR\n\
    HAVE_RUSAGE_RU_IDRSS\n\
    HAVE_RUSAGE_RU_INBLOCK\n\
    HAVE_RUSAGE_RU_ISRSS\n\
    HAVE_RUSAGE_RU_IXRSS\n\
    HAVE_RUSAGE_RU_MAJFLT\n\
    HAVE_RUSAGE_RU_MAXRSS\n\
    HAVE_RUSAGE_RU_MINFLT\n\
    HAVE_RUSAGE_RU_MSGRCV\n\
    HAVE_RUSAGE_RU_MSGSND\n\
    HAVE_RUSAGE_RU_NIVCSW\n\
    HAVE_RUSAGE_RU_NSIGNALS\n\
    HAVE_RUSAGE_RU_NSWAP\n\
    HAVE_RUSAGE_RU_NVCSW\n\
    HAVE_RUSAGE_RU_OUBLOCK\n\
    HAVE_RUSAGE_RU_STIME\n\
    HAVE_RUSAGE_RU_UTIME\n\
    HAVE_SEEKDIR\n\
    HAVE_SELECT\n\
    HAVE_SEMAPHORE_H\n\
    HAVE_SEM_CLOSE\n\
    HAVE_SEM_DESTROY\n\
    HAVE_SEM_GETVALUE\n\
    HAVE_SEM_INIT\n\
    HAVE_SEM_OPEN\n\
    HAVE_SEM_POST\n\
    HAVE_SEM_TIMEDWAIT\n\
    HAVE_SEM_TRYWAIT\n\
    HAVE_SEM_UNLINK\n\
    HAVE_SEM_WAIT\n\
    HAVE_SEND\n\
    HAVE_SENDTO\n\
    HAVE_SETEGID\n\
    HAVE_SETENV\n\
    HAVE_SETEUID\n\
    HAVE_SETGID\n\
    HAVE_SETGRENT\n\
    HAVE_SETHOSTENT\n\
    HAVE_SETITIMER\n\
    HAVE_SETNETENT\n\
    HAVE_SETPGID\n\
    HAVE_SETPROTOENT\n\
    HAVE_SETPWENT\n\
    HAVE_SETREGID\n\
    HAVE_SETREUID\n\
    HAVE_SETRLIMIT\n\
    HAVE_SETSERVENT\n\
    HAVE_SETSID\n\
    HAVE_SETSOCKOPT\n\
    HAVE_SETUID\n\
    HAVE_SHM_OPEN\n\
    HAVE_SHM_UNLINK\n\
    HAVE_SHUTDOWN\n\
    HAVE_SIGACTION\n\
    HAVE_SIGADDSET\n\
    HAVE_SIGALTSTACK\n\
    HAVE_SIGEMPTYSET\n\
    HAVE_SIGFILLSET\n\
    HAVE_SIGINFO_SI_ADDR\n\
    HAVE_SIGINFO_SI_ADDR_LSB\n\
    HAVE_SIGINFO_SI_BAND\n\
    HAVE_SIGINFO_SI_CODE\n\
    HAVE_SIGINFO_SI_ERRNO\n\
    HAVE_SIGINFO_SI_FD\n\
    HAVE_SIGINFO_SI_INT\n\
    HAVE_SIGINFO_SI_OVERRUN\n\
    HAVE_SIGINFO_SI_PID\n\
    HAVE_SIGINFO_SI_PTR\n\
    HAVE_SIGINFO_SI_SIGNO\n\
    HAVE_SIGINFO_SI_STATUS\n\
    HAVE_SIGINFO_SI_STIME\n\
    HAVE_SIGINFO_SI_TIMERID\n\
    HAVE_SIGINFO_SI_TRAPNO\n\
    HAVE_SIGINFO_SI_UID\n\
    HAVE_SIGINFO_SI_UTIME\n\
    HAVE_SIGINFO_SI_VALUE\n\
    HAVE_SIGNALFD\n\
    HAVE_SIGNAL_H\n\
    HAVE_SIGPROCMASK\n\
    HAVE_SIGTIMEDWAIT\n\
    HAVE_SIGWAITINFO\n\
    HAVE_SINH\n\
    HAVE_SOCKET\n\
    HAVE_SOCKETPAIR\n\
    HAVE_STAT\n\
    HAVE_STAT_ST_ATIM\n\
    HAVE_STAT_ST_ATIMESPEC\n\
    HAVE_STAT_ST_ATIME_USEC\n\
    HAVE_STAT_ST_CTIM\n\
    HAVE_STAT_ST_CTIMESPEC\n\
    HAVE_STAT_ST_CTIME_USEC\n\
    HAVE_STAT_ST_MTIM\n\
    HAVE_STAT_ST_MTIMESPEC\n\
    HAVE_STAT_ST_MTIME_USEC\n\
    HAVE_STDARG_H\n\
    HAVE_STDDEF_H\n\
    HAVE_STDINT_H\n\
    HAVE_STDIO_H\n\
    HAVE_STDLIB_H\n\
    HAVE_STRERROR\n\
    HAVE_STRFTIME\n\
    HAVE_STRINGS_H\n\
    HAVE_STRING_H\n\
    HAVE_STRUCT_ADDRINFO\n\
    HAVE_STRUCT_EPOLL_EVENT\n\
    HAVE_STRUCT_GROUP\n\
    HAVE_STRUCT_HOSTENT\n\
    HAVE_STRUCT_IN6_ADDR\n\
    HAVE_STRUCT_NETENT\n\
    HAVE_STRUCT_PASSWD\n\
    HAVE_STRUCT_PROTOENT\n\
    HAVE_STRUCT_SERVENT\n\
    HAVE_STRUCT_SOCKADDR_IN\n\
    HAVE_STRUCT_SOCKADDR_IN6\n\
    HAVE_STRUCT_SOCKADDR_UN\n\
    HAVE_STRUCT_TIMESPEC\n\
    HAVE_STRUCT_TM\n\
    HAVE_STRUCT_TMS\n\
    HAVE_SYMLINK\n\
    HAVE_SYNC\n\
    HAVE_SYSCONF\n\
    HAVE_SYSTEM\n\
    HAVE_SYS_EPOLL_H\n\
    HAVE_SYS_INOTIFY_H\n\
    HAVE_SYS_IOCTL_H\n\
    HAVE_SYS_MMAN_H\n\
    HAVE_SYS_PARAM_H\n\
    HAVE_SYS_RESOURCE_H\n\
    HAVE_SYS_SIGNALFD_H\n\
    HAVE_SYS_SOCKET_H\n\
    HAVE_SYS_STAT_H\n\
    HAVE_SYS_TIMERFD_H\n\
    HAVE_SYS_TIMES_H\n\
    HAVE_SYS_TIME_H\n\
    HAVE_SYS_TYPES_H\n\
    HAVE_SYS_UIO_H\n\
    HAVE_SYS_UN_H\n\
    HAVE_SYS_WAIT_H\n\
    HAVE_TANH\n\
    HAVE_TCGETPGRP\n\
    HAVE_TCGETSID\n\
    HAVE_TCSETPGRP\n\
    HAVE_TELLDIR\n\
    HAVE_TERMIOS_H\n\
    HAVE_TGAMMA\n\
    HAVE_TIME\n\
    HAVE_TIMEGM\n\
    HAVE_TIMELOCAL\n\
    HAVE_TIMERFD_CREATE\n\
    HAVE_TIMERFD_GETTIME\n\
    HAVE_TIMERFD_SETTIME\n\
    HAVE_TIMER_CREATE\n\
    HAVE_TIMER_DELETE\n\
    HAVE_TIMER_GETOVERRUN\n\
    HAVE_TIMER_GETTIME\n\
    HAVE_TIMER_SETTIME\n\
    HAVE_TIMES\n\
    HAVE_TIME_H\n\
    HAVE_TRUNCATE\n\
    HAVE_UMASK\n\
    HAVE_UNISTD_H\n\
    HAVE_UNLINK\n\
    HAVE_UNSETENV\n\
    HAVE_UTIME\n\
    HAVE_UTIMES\n\
    HAVE_UTIME_H\n\
    HAVE_WAIT\n\
    HAVE_WAITID\n\
    HAVE_WAITPID\n\
    HAVE_WCOREDUMP\n\
    HAVE_WEXITSTATUS\n\
    HAVE_WIFCONTINUED\n\
    HAVE_WIFEXITED\n\
    HAVE_WIFSIGNALED\n\
    HAVE_WIFSTOPPED\n\
    HAVE_WORDEXP\n\
    HAVE_WORDEXP_H\n\
    HAVE_WRITE\n\
    HAVE_WRITEV\n\
    HAVE_WSTOPSIG\n\
    HAVE_WTERMSIG\n\
    HAVE_Y0\n\
    HAVE_Y1\n\
    HAVE_YN\n\
    HAVE_fstat\n\
    HAVE_ioctl\n\
    RL_READLINE_VERSION)\n\
  (import (rnrs))\n\
\n\
;;;; helpers\n\
\n\
(define-syntax define-inline-constant\n\
  (syntax-rules ()\n\
    ((_ ?name ?value)\n\
     (define-syntax ?name (identifier-syntax ?value)))))\n\
\n\
\n\
;;;; code\n\n");


printf("(define-inline-constant HAVE_ACCEPT %s)\n",
#ifdef HAVE_ACCEPT
  "#t"
#else
  "#f"
#endif
  );
  printf("(define-inline-constant HAVE_ACCESS %s)\n",
#ifdef HAVE_ACCESS
  "#t"
#else
  "#f"
#endif
);
printf("(define-inline-constant HAVE_ACOSH %s)\n",
#ifdef HAVE_ACOSH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ALARM %s)\n",
#ifdef HAVE_ALARM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ARPA_INET_H %s)\n",
#ifdef HAVE_ARPA_INET_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ASINH %s)\n",
#ifdef HAVE_ASINH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ASSERT_H %s)\n",
#ifdef HAVE_ASSERT_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ATANH %s)\n",
#ifdef HAVE_ATANH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_BIND %s)\n",
#ifdef HAVE_BIND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CACOS %s)\n",
#ifdef HAVE_CACOS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CACOSH %s)\n",
#ifdef HAVE_CACOSH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CASIN %s)\n",
#ifdef HAVE_CASIN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CASINH %s)\n",
#ifdef HAVE_CASINH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CATAN %s)\n",
#ifdef HAVE_CATAN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CATANH %s)\n",
#ifdef HAVE_CATANH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CCOS %s)\n",
#ifdef HAVE_CCOS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CCOSH %s)\n",
#ifdef HAVE_CCOSH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CEXP %s)\n",
#ifdef HAVE_CEXP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CHDIR %s)\n",
#ifdef HAVE_CHDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CHMOD %s)\n",
#ifdef HAVE_CHMOD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CHOWN %s)\n",
#ifdef HAVE_CHOWN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLEARENV %s)\n",
#ifdef HAVE_CLEARENV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOCK %s)\n",
#ifdef HAVE_CLOCK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOCK_GETCPUCLOCKID %s)\n",
#ifdef HAVE_CLOCK_GETCPUCLOCKID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOCK_GETRES %s)\n",
#ifdef HAVE_CLOCK_GETRES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOCK_GETTIME %s)\n",
#ifdef HAVE_CLOCK_GETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOCK_SETTIME %s)\n",
#ifdef HAVE_CLOCK_SETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOG %s)\n",
#ifdef HAVE_CLOG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOG10 %s)\n",
#ifdef HAVE_CLOG10
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOSE %s)\n",
#ifdef HAVE_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CLOSEDIR %s)\n",
#ifdef HAVE_CLOSEDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_COMPLEX_H %s)\n",
#ifdef HAVE_COMPLEX_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CONFIG_H %s)\n",
#ifdef HAVE_CONFIG_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CONFSTR %s)\n",
#ifdef HAVE_CONFSTR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CONNECT %s)\n",
#ifdef HAVE_CONNECT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_COSH %s)\n",
#ifdef HAVE_COSH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CPOW %s)\n",
#ifdef HAVE_CPOW
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CSIN %s)\n",
#ifdef HAVE_CSIN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CSINH %s)\n",
#ifdef HAVE_CSINH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CSQRT %s)\n",
#ifdef HAVE_CSQRT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CTAN %s)\n",
#ifdef HAVE_CTAN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CTANH %s)\n",
#ifdef HAVE_CTANH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CTERMID %s)\n",
#ifdef HAVE_CTERMID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DAEMON %s)\n",
#ifdef HAVE_DAEMON
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DECL_ENVIRON %s)\n",
#ifdef HAVE_DECL_ENVIRON
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DIRENT_H %s)\n",
#ifdef HAVE_DIRENT_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DIRFD %s)\n",
#ifdef HAVE_DIRFD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DUP %s)\n",
#ifdef HAVE_DUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_DUP2 %s)\n",
#ifdef HAVE_DUP2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDGRENT %s)\n",
#ifdef HAVE_ENDGRENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDHOSTENT %s)\n",
#ifdef HAVE_ENDHOSTENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDNETENT %s)\n",
#ifdef HAVE_ENDNETENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDPROTOENT %s)\n",
#ifdef HAVE_ENDPROTOENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDPWENT %s)\n",
#ifdef HAVE_ENDPWENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ENDSERVENT %s)\n",
#ifdef HAVE_ENDSERVENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EPOLL_CREATE %s)\n",
#ifdef HAVE_EPOLL_CREATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EPOLL_CREATE1 %s)\n",
#ifdef HAVE_EPOLL_CREATE1
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EPOLL_CTL %s)\n",
#ifdef HAVE_EPOLL_CTL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EPOLL_WAIT %s)\n",
#ifdef HAVE_EPOLL_WAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ERF %s)\n",
#ifdef HAVE_ERF
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ERFC %s)\n",
#ifdef HAVE_ERFC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ERRNO_H %s)\n",
#ifdef HAVE_ERRNO_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EXECV %s)\n",
#ifdef HAVE_EXECV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EXECVE %s)\n",
#ifdef HAVE_EXECVE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_EXECVP %s)\n",
#ifdef HAVE_EXECVP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FCHDIR %s)\n",
#ifdef HAVE_FCHDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FCHMOD %s)\n",
#ifdef HAVE_FCHMOD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FCHOWN %s)\n",
#ifdef HAVE_FCHOWN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FCNTL %s)\n",
#ifdef HAVE_FCNTL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FCNTL_H %s)\n",
#ifdef HAVE_FCNTL_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FDATASYNC %s)\n",
#ifdef HAVE_FDATASYNC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FDOPENDIR %s)\n",
#ifdef HAVE_FDOPENDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FNMATCH %s)\n",
#ifdef HAVE_FNMATCH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FNMATCH_H %s)\n",
#ifdef HAVE_FNMATCH_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FORK %s)\n",
#ifdef HAVE_FORK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FPATHCONF %s)\n",
#ifdef HAVE_FPATHCONF
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FSYNC %s)\n",
#ifdef HAVE_FSYNC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FTRUNCATE %s)\n",
#ifdef HAVE_FTRUNCATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_FUTIMES %s)\n",
#ifdef HAVE_FUTIMES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GAI_STRERROR %s)\n",
#ifdef HAVE_GAI_STRERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETADDRINFO %s)\n",
#ifdef HAVE_GETADDRINFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETCWD %s)\n",
#ifdef HAVE_GETCWD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETEGID %s)\n",
#ifdef HAVE_GETEGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETENV %s)\n",
#ifdef HAVE_GETENV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETEUID %s)\n",
#ifdef HAVE_GETEUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETGID %s)\n",
#ifdef HAVE_GETGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETGRENT %s)\n",
#ifdef HAVE_GETGRENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETGRGID %s)\n",
#ifdef HAVE_GETGRGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETGRNAM %s)\n",
#ifdef HAVE_GETGRNAM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETGROUPS %s)\n",
#ifdef HAVE_GETGROUPS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETHOSTBYADDR %s)\n",
#ifdef HAVE_GETHOSTBYADDR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETHOSTBYNAME %s)\n",
#ifdef HAVE_GETHOSTBYNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETHOSTBYNAME2 %s)\n",
#ifdef HAVE_GETHOSTBYNAME2
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETHOSTENT %s)\n",
#ifdef HAVE_GETHOSTENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETITIMER %s)\n",
#ifdef HAVE_GETITIMER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETLOGIN %s)\n",
#ifdef HAVE_GETLOGIN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETNETBYADDR %s)\n",
#ifdef HAVE_GETNETBYADDR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETNETBYNAME %s)\n",
#ifdef HAVE_GETNETBYNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETNETENT %s)\n",
#ifdef HAVE_GETNETENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPEERNAME %s)\n",
#ifdef HAVE_GETPEERNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPGRP %s)\n",
#ifdef HAVE_GETPGRP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPID %s)\n",
#ifdef HAVE_GETPID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPPID %s)\n",
#ifdef HAVE_GETPPID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPROTOBYNAME %s)\n",
#ifdef HAVE_GETPROTOBYNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPROTOBYNUMBER %s)\n",
#ifdef HAVE_GETPROTOBYNUMBER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPROTOENT %s)\n",
#ifdef HAVE_GETPROTOENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPWENT %s)\n",
#ifdef HAVE_GETPWENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPWNAM %s)\n",
#ifdef HAVE_GETPWNAM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETPWUID %s)\n",
#ifdef HAVE_GETPWUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETRLIMIT %s)\n",
#ifdef HAVE_GETRLIMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETRUSAGE %s)\n",
#ifdef HAVE_GETRUSAGE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSERVBYNAME %s)\n",
#ifdef HAVE_GETSERVBYNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSERVBYPORT %s)\n",
#ifdef HAVE_GETSERVBYPORT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSERVENT %s)\n",
#ifdef HAVE_GETSERVENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSID %s)\n",
#ifdef HAVE_GETSID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSOCKNAME %s)\n",
#ifdef HAVE_GETSOCKNAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETSOCKOPT %s)\n",
#ifdef HAVE_GETSOCKOPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETTIMEOFDAY %s)\n",
#ifdef HAVE_GETTIMEOFDAY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GETUID %s)\n",
#ifdef HAVE_GETUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GLOB %s)\n",
#ifdef HAVE_GLOB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GLOB_H %s)\n",
#ifdef HAVE_GLOB_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GMTIME %s)\n",
#ifdef HAVE_GMTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GMTIME_R %s)\n",
#ifdef HAVE_GMTIME_R
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_GRP_H %s)\n",
#ifdef HAVE_GRP_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_HISTORY_H %s)\n",
#ifdef HAVE_HISTORY_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ICONV %s)\n",
#ifdef HAVE_ICONV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ICONV_CLOSE %s)\n",
#ifdef HAVE_ICONV_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ICONV_OPEN %s)\n",
#ifdef HAVE_ICONV_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_IF_INDEXTONAME %s)\n",
#ifdef HAVE_IF_INDEXTONAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_IF_NAMEINDEX %s)\n",
#ifdef HAVE_IF_NAMEINDEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_IF_NAMETOINDEX %s)\n",
#ifdef HAVE_IF_NAMETOINDEX
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INET_ATON %s)\n",
#ifdef HAVE_INET_ATON
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INET_NTOA %s)\n",
#ifdef HAVE_INET_NTOA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INET_NTOP %s)\n",
#ifdef HAVE_INET_NTOP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INET_PTON %s)\n",
#ifdef HAVE_INET_PTON
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_HTONL %s)\n",
#ifdef HAVE_HTONL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_HTONS %s)\n",
#ifdef HAVE_HTONS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NTOHL %s)\n",
#ifdef HAVE_NTOHL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NTOHS %s)\n",
#ifdef HAVE_NTOHS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INOTIFY_ADD_WATCH %s)\n",
#ifdef HAVE_INOTIFY_ADD_WATCH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INOTIFY_INIT %s)\n",
#ifdef HAVE_INOTIFY_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INOTIFY_INIT1 %s)\n",
#ifdef HAVE_INOTIFY_INIT1
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_INOTIFY_RM_WATCH %s)\n",
#ifdef HAVE_INOTIFY_RM_WATCH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_J0 %s)\n",
#ifdef HAVE_J0
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_J1 %s)\n",
#ifdef HAVE_J1
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_JN %s)\n",
#ifdef HAVE_JN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_HYPOT %s)\n",
#ifdef HAVE_HYPOT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_CBRT %s)\n",
#ifdef HAVE_CBRT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_KILL %s)\n",
#ifdef HAVE_KILL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LGAMMA_R %s)\n",
#ifdef HAVE_LGAMMA_R
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LIBFFI %s)\n",
#ifdef HAVE_LIBFFI
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LIBICONV %s)\n",
#ifdef HAVE_LIBICONV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LIBREADLINE %s)\n",
#ifdef HAVE_LIBREADLINE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LIMITS_H %s)\n",
#ifdef HAVE_LIMITS_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LINK %s)\n",
#ifdef HAVE_LINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LISTEN %s)\n",
#ifdef HAVE_LISTEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LOCKF %s)\n",
#ifdef HAVE_LOCKF
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LOCALTIME_R %s)\n",
#ifdef HAVE_LOCALTIME_R
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LSEEK %s)\n",
#ifdef HAVE_LSEEK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LSTAT %s)\n",
#ifdef HAVE_LSTAT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_LUTIMES %s)\n",
#ifdef HAVE_LUTIMES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MADVISE %s)\n",
#ifdef HAVE_MADVISE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MATH_H %s)\n",
#ifdef HAVE_MATH_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MKDIR %s)\n",
#ifdef HAVE_MKDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MKDTEMP %s)\n",
#ifdef HAVE_MKDTEMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MKFIFO %s)\n",
#ifdef HAVE_MKFIFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MKSTEMP %s)\n",
#ifdef HAVE_MKSTEMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MKTIME %s)\n",
#ifdef HAVE_MKTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MLOCK %s)\n",
#ifdef HAVE_MLOCK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MLOCKALL %s)\n",
#ifdef HAVE_MLOCKALL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MMAP %s)\n",
#ifdef HAVE_MMAP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MPROTECT %s)\n",
#ifdef HAVE_MPROTECT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQUEUE_H %s)\n",
#ifdef HAVE_MQUEUE_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_CLOSE %s)\n",
#ifdef HAVE_MQ_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_GETATTR %s)\n",
#ifdef HAVE_MQ_GETATTR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_OPEN %s)\n",
#ifdef HAVE_MQ_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_RECEIVE %s)\n",
#ifdef HAVE_MQ_RECEIVE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_SEND %s)\n",
#ifdef HAVE_MQ_SEND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_SETATTR %s)\n",
#ifdef HAVE_MQ_SETATTR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_TIMEDRECEIVE %s)\n",
#ifdef HAVE_MQ_TIMEDRECEIVE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_TIMEDSEND %s)\n",
#ifdef HAVE_MQ_TIMEDSEND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MQ_UNLINK %s)\n",
#ifdef HAVE_MQ_UNLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MREMAP %s)\n",
#ifdef HAVE_MREMAP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MSYNC %s)\n",
#ifdef HAVE_MSYNC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MUNLOCK %s)\n",
#ifdef HAVE_MUNLOCK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MUNLOCKALL %s)\n",
#ifdef HAVE_MUNLOCKALL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_MUNMAP %s)\n",
#ifdef HAVE_MUNMAP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NANOSLEEP %s)\n",
#ifdef HAVE_NANOSLEEP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NETDB_H %s)\n",
#ifdef HAVE_NETDB_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NETINET_IN_H %s)\n",
#ifdef HAVE_NETINET_IN_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_NET_IF_H %s)\n",
#ifdef HAVE_NET_IF_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_OPEN %s)\n",
#ifdef HAVE_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_OPENDIR %s)\n",
#ifdef HAVE_OPENDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PATHCONF %s)\n",
#ifdef HAVE_PATHCONF
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PAUSE %s)\n",
#ifdef HAVE_PAUSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PIPE %s)\n",
#ifdef HAVE_PIPE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_POLL %s)\n",
#ifdef HAVE_POLL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_POLL_H %s)\n",
#ifdef HAVE_POLL_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PREAD %s)\n",
#ifdef HAVE_PREAD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PRLIMIT %s)\n",
#ifdef HAVE_PRLIMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PWD_H %s)\n",
#ifdef HAVE_PWD_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_PWRITE %s)\n",
#ifdef HAVE_PWRITE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RAISE %s)\n",
#ifdef HAVE_RAISE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RAND %s)\n",
#ifdef HAVE_RAND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READ %s)\n",
#ifdef HAVE_READ
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READDIR %s)\n",
#ifdef HAVE_READDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READLINE_H %s)\n",
#ifdef HAVE_READLINE_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READLINE_HISTORY %s)\n",
#ifdef HAVE_READLINE_HISTORY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READLINE_HISTORY_H %s)\n",
#ifdef HAVE_READLINE_HISTORY_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READLINE_READLINE_H %s)\n",
#ifdef HAVE_READLINE_READLINE_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READLINK %s)\n",
#ifdef HAVE_READLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_READV %s)\n",
#ifdef HAVE_READV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_REALPATH %s)\n",
#ifdef HAVE_REALPATH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RECV %s)\n",
#ifdef HAVE_RECV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RECVFROM %s)\n",
#ifdef HAVE_RECVFROM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_REGCOMP %s)\n",
#ifdef HAVE_REGCOMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_REGEX_H %s)\n",
#ifdef HAVE_REGEX_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_REMOVE %s)\n",
#ifdef HAVE_REMOVE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RENAME %s)\n",
#ifdef HAVE_RENAME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_REWINDDIR %s)\n",
#ifdef HAVE_REWINDDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RMDIR %s)\n",
#ifdef HAVE_RMDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_IDRSS %s)\n",
#ifdef HAVE_RUSAGE_RU_IDRSS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_INBLOCK %s)\n",
#ifdef HAVE_RUSAGE_RU_INBLOCK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_ISRSS %s)\n",
#ifdef HAVE_RUSAGE_RU_ISRSS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_IXRSS %s)\n",
#ifdef HAVE_RUSAGE_RU_IXRSS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_MAJFLT %s)\n",
#ifdef HAVE_RUSAGE_RU_MAJFLT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_MAXRSS %s)\n",
#ifdef HAVE_RUSAGE_RU_MAXRSS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_MINFLT %s)\n",
#ifdef HAVE_RUSAGE_RU_MINFLT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_MSGRCV %s)\n",
#ifdef HAVE_RUSAGE_RU_MSGRCV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_MSGSND %s)\n",
#ifdef HAVE_RUSAGE_RU_MSGSND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_NIVCSW %s)\n",
#ifdef HAVE_RUSAGE_RU_NIVCSW
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_NSIGNALS %s)\n",
#ifdef HAVE_RUSAGE_RU_NSIGNALS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_NSWAP %s)\n",
#ifdef HAVE_RUSAGE_RU_NSWAP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_NVCSW %s)\n",
#ifdef HAVE_RUSAGE_RU_NVCSW
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_OUBLOCK %s)\n",
#ifdef HAVE_RUSAGE_RU_OUBLOCK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_STIME %s)\n",
#ifdef HAVE_RUSAGE_RU_STIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_RUSAGE_RU_UTIME %s)\n",
#ifdef HAVE_RUSAGE_RU_UTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEEKDIR %s)\n",
#ifdef HAVE_SEEKDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SELECT %s)\n",
#ifdef HAVE_SELECT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEMAPHORE_H %s)\n",
#ifdef HAVE_SEMAPHORE_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_CLOSE %s)\n",
#ifdef HAVE_SEM_CLOSE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_DESTROY %s)\n",
#ifdef HAVE_SEM_DESTROY
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_GETVALUE %s)\n",
#ifdef HAVE_SEM_GETVALUE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_INIT %s)\n",
#ifdef HAVE_SEM_INIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_OPEN %s)\n",
#ifdef HAVE_SEM_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_POST %s)\n",
#ifdef HAVE_SEM_POST
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_TIMEDWAIT %s)\n",
#ifdef HAVE_SEM_TIMEDWAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_TRYWAIT %s)\n",
#ifdef HAVE_SEM_TRYWAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_UNLINK %s)\n",
#ifdef HAVE_SEM_UNLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEM_WAIT %s)\n",
#ifdef HAVE_SEM_WAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SEND %s)\n",
#ifdef HAVE_SEND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SENDTO %s)\n",
#ifdef HAVE_SENDTO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETEGID %s)\n",
#ifdef HAVE_SETEGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETENV %s)\n",
#ifdef HAVE_SETENV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETEUID %s)\n",
#ifdef HAVE_SETEUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETGID %s)\n",
#ifdef HAVE_SETGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETGRENT %s)\n",
#ifdef HAVE_SETGRENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETHOSTENT %s)\n",
#ifdef HAVE_SETHOSTENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETITIMER %s)\n",
#ifdef HAVE_SETITIMER
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETNETENT %s)\n",
#ifdef HAVE_SETNETENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETPGID %s)\n",
#ifdef HAVE_SETPGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETPROTOENT %s)\n",
#ifdef HAVE_SETPROTOENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETPWENT %s)\n",
#ifdef HAVE_SETPWENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETREGID %s)\n",
#ifdef HAVE_SETREGID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETREUID %s)\n",
#ifdef HAVE_SETREUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETRLIMIT %s)\n",
#ifdef HAVE_SETRLIMIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETSERVENT %s)\n",
#ifdef HAVE_SETSERVENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETSID %s)\n",
#ifdef HAVE_SETSID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETSOCKOPT %s)\n",
#ifdef HAVE_SETSOCKOPT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SETUID %s)\n",
#ifdef HAVE_SETUID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SHM_OPEN %s)\n",
#ifdef HAVE_SHM_OPEN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SHM_UNLINK %s)\n",
#ifdef HAVE_SHM_UNLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SHUTDOWN %s)\n",
#ifdef HAVE_SHUTDOWN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGACTION %s)\n",
#ifdef HAVE_SIGACTION
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGADDSET %s)\n",
#ifdef HAVE_SIGADDSET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGALTSTACK %s)\n",
#ifdef HAVE_SIGALTSTACK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGEMPTYSET %s)\n",
#ifdef HAVE_SIGEMPTYSET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGFILLSET %s)\n",
#ifdef HAVE_SIGFILLSET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_ADDR %s)\n",
#ifdef HAVE_SIGINFO_SI_ADDR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_ADDR_LSB %s)\n",
#ifdef HAVE_SIGINFO_SI_ADDR_LSB
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_BAND %s)\n",
#ifdef HAVE_SIGINFO_SI_BAND
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_CODE %s)\n",
#ifdef HAVE_SIGINFO_SI_CODE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_ERRNO %s)\n",
#ifdef HAVE_SIGINFO_SI_ERRNO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_FD %s)\n",
#ifdef HAVE_SIGINFO_SI_FD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_INT %s)\n",
#ifdef HAVE_SIGINFO_SI_INT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_OVERRUN %s)\n",
#ifdef HAVE_SIGINFO_SI_OVERRUN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_PID %s)\n",
#ifdef HAVE_SIGINFO_SI_PID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_PTR %s)\n",
#ifdef HAVE_SIGINFO_SI_PTR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_SIGNO %s)\n",
#ifdef HAVE_SIGINFO_SI_SIGNO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_STATUS %s)\n",
#ifdef HAVE_SIGINFO_SI_STATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_STIME %s)\n",
#ifdef HAVE_SIGINFO_SI_STIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_TIMERID %s)\n",
#ifdef HAVE_SIGINFO_SI_TIMERID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_TRAPNO %s)\n",
#ifdef HAVE_SIGINFO_SI_TRAPNO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_UID %s)\n",
#ifdef HAVE_SIGINFO_SI_UID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_UTIME %s)\n",
#ifdef HAVE_SIGINFO_SI_UTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGINFO_SI_VALUE %s)\n",
#ifdef HAVE_SIGINFO_SI_VALUE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGNALFD %s)\n",
#ifdef HAVE_SIGNALFD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGNAL_H %s)\n",
#ifdef HAVE_SIGNAL_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGPROCMASK %s)\n",
#ifdef HAVE_SIGPROCMASK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGTIMEDWAIT %s)\n",
#ifdef HAVE_SIGTIMEDWAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SIGWAITINFO %s)\n",
#ifdef HAVE_SIGWAITINFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SINH %s)\n",
#ifdef HAVE_SINH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SOCKET %s)\n",
#ifdef HAVE_SOCKET
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SOCKETPAIR %s)\n",
#ifdef HAVE_SOCKETPAIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT %s)\n",
#ifdef HAVE_STAT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_ATIM %s)\n",
#ifdef HAVE_STAT_ST_ATIM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_ATIMESPEC %s)\n",
#ifdef HAVE_STAT_ST_ATIMESPEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_ATIME_USEC %s)\n",
#ifdef HAVE_STAT_ST_ATIME_USEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_CTIM %s)\n",
#ifdef HAVE_STAT_ST_CTIM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_CTIMESPEC %s)\n",
#ifdef HAVE_STAT_ST_CTIMESPEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_CTIME_USEC %s)\n",
#ifdef HAVE_STAT_ST_CTIME_USEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_MTIM %s)\n",
#ifdef HAVE_STAT_ST_MTIM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_MTIMESPEC %s)\n",
#ifdef HAVE_STAT_ST_MTIMESPEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STAT_ST_MTIME_USEC %s)\n",
#ifdef HAVE_STAT_ST_MTIME_USEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STDARG_H %s)\n",
#ifdef HAVE_STDARG_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STDDEF_H %s)\n",
#ifdef HAVE_STDDEF_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STDINT_H %s)\n",
#ifdef HAVE_STDINT_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STDIO_H %s)\n",
#ifdef HAVE_STDIO_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STDLIB_H %s)\n",
#ifdef HAVE_STDLIB_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRERROR %s)\n",
#ifdef HAVE_STRERROR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRFTIME %s)\n",
#ifdef HAVE_STRFTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRINGS_H %s)\n",
#ifdef HAVE_STRINGS_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRING_H %s)\n",
#ifdef HAVE_STRING_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_ADDRINFO %s)\n",
#ifdef HAVE_STRUCT_ADDRINFO
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_EPOLL_EVENT %s)\n",
#ifdef HAVE_STRUCT_EPOLL_EVENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_GROUP %s)\n",
#ifdef HAVE_STRUCT_GROUP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_HOSTENT %s)\n",
#ifdef HAVE_STRUCT_HOSTENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_IN6_ADDR %s)\n",
#ifdef HAVE_STRUCT_IN6_ADDR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_NETENT %s)\n",
#ifdef HAVE_STRUCT_NETENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_PASSWD %s)\n",
#ifdef HAVE_STRUCT_PASSWD
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_PROTOENT %s)\n",
#ifdef HAVE_STRUCT_PROTOENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_SERVENT %s)\n",
#ifdef HAVE_STRUCT_SERVENT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_SOCKADDR_IN %s)\n",
#ifdef HAVE_STRUCT_SOCKADDR_IN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_SOCKADDR_IN6 %s)\n",
#ifdef HAVE_STRUCT_SOCKADDR_IN6
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_SOCKADDR_UN %s)\n",
#ifdef HAVE_STRUCT_SOCKADDR_UN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_TIMESPEC %s)\n",
#ifdef HAVE_STRUCT_TIMESPEC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_TM %s)\n",
#ifdef HAVE_STRUCT_TM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_STRUCT_TMS %s)\n",
#ifdef HAVE_STRUCT_TMS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYMLINK %s)\n",
#ifdef HAVE_SYMLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYNC %s)\n",
#ifdef HAVE_SYNC
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYSCONF %s)\n",
#ifdef HAVE_SYSCONF
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYSTEM %s)\n",
#ifdef HAVE_SYSTEM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_EPOLL_H %s)\n",
#ifdef HAVE_SYS_EPOLL_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_INOTIFY_H %s)\n",
#ifdef HAVE_SYS_INOTIFY_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_IOCTL_H %s)\n",
#ifdef HAVE_SYS_IOCTL_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_MMAN_H %s)\n",
#ifdef HAVE_SYS_MMAN_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_PARAM_H %s)\n",
#ifdef HAVE_SYS_PARAM_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_RESOURCE_H %s)\n",
#ifdef HAVE_SYS_RESOURCE_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_SIGNALFD_H %s)\n",
#ifdef HAVE_SYS_SIGNALFD_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_SOCKET_H %s)\n",
#ifdef HAVE_SYS_SOCKET_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_STAT_H %s)\n",
#ifdef HAVE_SYS_STAT_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_TIMERFD_H %s)\n",
#ifdef HAVE_SYS_TIMERFD_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_TIMES_H %s)\n",
#ifdef HAVE_SYS_TIMES_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_TIME_H %s)\n",
#ifdef HAVE_SYS_TIME_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_TYPES_H %s)\n",
#ifdef HAVE_SYS_TYPES_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_UIO_H %s)\n",
#ifdef HAVE_SYS_UIO_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_UN_H %s)\n",
#ifdef HAVE_SYS_UN_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_SYS_WAIT_H %s)\n",
#ifdef HAVE_SYS_WAIT_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TANH %s)\n",
#ifdef HAVE_TANH
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TCGETPGRP %s)\n",
#ifdef HAVE_TCGETPGRP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TCGETSID %s)\n",
#ifdef HAVE_TCGETSID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TCSETPGRP %s)\n",
#ifdef HAVE_TCSETPGRP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TELLDIR %s)\n",
#ifdef HAVE_TELLDIR
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TERMIOS_H %s)\n",
#ifdef HAVE_TERMIOS_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TGAMMA %s)\n",
#ifdef HAVE_TGAMMA
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIME %s)\n",
#ifdef HAVE_TIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMEGM %s)\n",
#ifdef HAVE_TIMEGM
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMELOCAL %s)\n",
#ifdef HAVE_TIMELOCAL
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMERFD_CREATE %s)\n",
#ifdef HAVE_TIMERFD_CREATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMERFD_GETTIME %s)\n",
#ifdef HAVE_TIMERFD_GETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMERFD_SETTIME %s)\n",
#ifdef HAVE_TIMERFD_SETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMER_CREATE %s)\n",
#ifdef HAVE_TIMER_CREATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMER_DELETE %s)\n",
#ifdef HAVE_TIMER_DELETE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMER_GETOVERRUN %s)\n",
#ifdef HAVE_TIMER_GETOVERRUN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMER_GETTIME %s)\n",
#ifdef HAVE_TIMER_GETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMER_SETTIME %s)\n",
#ifdef HAVE_TIMER_SETTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIMES %s)\n",
#ifdef HAVE_TIMES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TIME_H %s)\n",
#ifdef HAVE_TIME_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_TRUNCATE %s)\n",
#ifdef HAVE_TRUNCATE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UMASK %s)\n",
#ifdef HAVE_UMASK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UNISTD_H %s)\n",
#ifdef HAVE_UNISTD_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UNLINK %s)\n",
#ifdef HAVE_UNLINK
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UNSETENV %s)\n",
#ifdef HAVE_UNSETENV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UTIME %s)\n",
#ifdef HAVE_UTIME
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UTIMES %s)\n",
#ifdef HAVE_UTIMES
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_UTIME_H %s)\n",
#ifdef HAVE_UTIME_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WAIT %s)\n",
#ifdef HAVE_WAIT
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WAITID %s)\n",
#ifdef HAVE_WAITID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WAITPID %s)\n",
#ifdef HAVE_WAITPID
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WCOREDUMP %s)\n",
#ifdef HAVE_WCOREDUMP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WEXITSTATUS %s)\n",
#ifdef HAVE_WEXITSTATUS
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WIFCONTINUED %s)\n",
#ifdef HAVE_WIFCONTINUED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WIFEXITED %s)\n",
#ifdef HAVE_WIFEXITED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WIFSIGNALED %s)\n",
#ifdef HAVE_WIFSIGNALED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WIFSTOPPED %s)\n",
#ifdef HAVE_WIFSTOPPED
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WORDEXP %s)\n",
#ifdef HAVE_WORDEXP
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WORDEXP_H %s)\n",
#ifdef HAVE_WORDEXP_H
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WRITE %s)\n",
#ifdef HAVE_WRITE
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WRITEV %s)\n",
#ifdef HAVE_WRITEV
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WSTOPSIG %s)\n",
#ifdef HAVE_WSTOPSIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_WTERMSIG %s)\n",
#ifdef HAVE_WTERMSIG
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_Y0 %s)\n",
#ifdef HAVE_Y0
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_Y1 %s)\n",
#ifdef HAVE_Y1
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_YN %s)\n",
#ifdef HAVE_YN
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_fstat %s)\n",
#ifdef HAVE_fstat
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant HAVE_ioctl %s)\n",
#ifdef HAVE_ioctl
  "#t"
#else
  "#f"
#endif
  );
printf("(define-inline-constant RL_READLINE_VERSION %s)\n",
#ifdef RL_READLINE_VERSION
  "#t"
#else
  "#f"
#endif
  );


  printf("\n\
;;;; done\n\
\n\
)\n\
\n\
;;; end of file\n");
  exit(EXIT_SUCCESS);
}

/* end of file */
