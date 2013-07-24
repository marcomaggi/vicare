dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare Scheme],[0.3d1],[marco.maggi-ipsu@poste.it],
  [vicare-scheme],[http://marcomaggi.github.com/vicare.html])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([autotools])
AC_CONFIG_AUX_DIR([autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

AC_LANG([C])
AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

#page
#### additional configuration command line options

# This must always be defined.  When true: the bottom page of the Scheme
# stack segment  is protected  from read/write operation,  so that  if a
# stack  overflow happens  it results  in a  SIGSEGV rather  than memory
# corruption.
AC_DEFINE([IK_PROTECT_FROM_STACK_OVERFLOW],[1],
  [Define to true to protect the Scheme stack from overflow.])

AS_VAR_SET_IF(VFLAGS,,[AS_VAR_SET(VFLAGS,["-O2"])])

VICARE_ENABLE_OPTION([DEBUG],[debug],[no],
  [whether debugging features are included in compiled Scheme libraries],
  [enable compilation of Scheme libraries with debugging features])
AM_CONDITIONAL([WANT_DEBUG],
  [test "x$vicare_enable_DEBUG" = xyes])

AM_COND_IF([WANT_DEBUG],
  [AS_VAR_SET(VFLAGS,"$VFLAGS -g")])

VICARE_ENABLE_OPTION([ARGUMENTS_VALIDATION],[arguments-validation],[yes],
  [whether arguments validation is enabled],
  [enable arguments validation in the boot image])
AM_CONDITIONAL([WANT_ARGUMENTS_VALIDATION],
  [test "x$vicare_enable_ARGUMENTS_VALIDATION" = xyes])

vicare_subst_ARGUMENTS_VALIDATION=1
AM_COND_IF([WANT_ARGUMENTS_VALIDATION],
  [vicare_subst_ARGUMENTS_VALIDATION=1],
  [vicare_subst_ARGUMENTS_VALIDATION=0])
AC_SUBST([VICARE_ARGUMENTS_VALIDATION],
  [$vicare_subst_ARGUMENTS_VALIDATION])

dnl --------------------------------------------------------------------

VICARE_ENABLE_OPTION([SRFI],[srfi],[yes],
  [whether to install the SRFI libraries],
  [enable installation of SRFI libraries])
AM_CONDITIONAL([WANT_SRFI],[test "x$vicare_enable_SRFI" = xyes])

VICARE_ENABLE_OPTION([POSIX],[posix],[yes],
  [whether POSIX features are enabled],
  [enable POSIX features])
AM_CONDITIONAL([WANT_POSIX],[test "x$vicare_enable_POSIX" = xyes])

VICARE_ENABLE_OPTION([GLIBC],[glibc],[yes],
  [whether GNU C Library features are enabled],
  [enable GNU C Library features])
AM_CONDITIONAL([WANT_GLIBC],[test "x$vicare_enable_GLIBC" = xyes])

VICARE_ENABLE_OPTION([LINUX],[linux],[yes],
  [whether Linux features are enabled],
  [enable Linux features])
AM_CONDITIONAL([WANT_LINUX],[test "x$vicare_enable_LINUX" = xyes])

dnl --------------------------------------------------------------------

VICARE_ENABLE_OPTION([BINFMT],[binfmt],[no],
  [whether usage of the binfmt_misc Linux kernel module is enabled],
  [enable usage of the binfmt_misc linux kernel module])
AM_CONDITIONAL([WANT_BINFMT],[test "x$vicare_enable_BINFMT" = xyes])

VICARE_ENABLE_OPTION([TIME_TESTS],[time-tests],[no],
  [whether to enable timing of tests],
  [enable timing of tests])
AM_CONDITIONAL([WANT_TIME_TESTS],[test "x$vicare_enable_TIME_TESTS" = xyes])

AM_COND_IF([WANT_TIME_TESTS],
  [AC_CHECK_PROG([TIME_PROGRAM],[time],[time],[:])])

dnl --------------------------------------------------------------------

dnl This  must come before  searching for POSIX functions.   For example
dnl the POSIX semaphore functions appear to need this library.
VICARE_WITH_LIBRARY([PTHREAD],[pthread],[check],
  [whether to link with pthread],
  [link executable with pthread],
  [pthread.h],[pthread],[main])
AM_CONDITIONAL([WANT_PTHREAD],[test "x$vicare_with_PTHREAD" = xyes])

#page
#### basic system inspection

AM_PROG_AS
AC_PROG_CC
AM_PROG_CC_C_O

case "$target_os" in
  *linux*)
    LDFLAGS="-rdynamic $LDFLAGS"
    ;;
  *bsd*)
    LDFLAGS="-Wl,-E $LDFLAGS"
    ;;
  *cygwin*)
    LDFLAGS="-Wl,-E -Wl,--export-all-symbols $LDFLAGS"
    ;;
  *darwin*)
    AC_DEFINE([LIBFFI_ON_DARWIN],[1],
              [Mark code returned by Libffi executable because
               libffi does not do that yet.  This so far is only
               important on Snow Leopard in 64-bit mode but we
               mark it on all darwins anyways.])
    ;;
  esac

AC_CHECK_SIZEOF(void *)
if test "$ac_cv_sizeof_void_p" = 4; then
   AC_DEFINE(FLAT_TABLES, 1,
     [use flat segment and dirty vectors (not used yet)])
fi
AC_SUBST([VICARE_SIZE_OF_VOIDP],[$ac_cv_sizeof_void_p])
AC_DEFINE_UNQUOTED([VICARE_SIZE_OF_VOIDP],
  [$ac_cv_sizeof_void_p],
  [size of words])

AC_C_BIGENDIAN([AC_SUBST(VICARE_PLATFORM_ENDIANNESS,[big])],
  [AC_SUBST(VICARE_PLATFORM_ENDIANNESS,[little])])

AC_SYS_LARGEFILE

# Checks for libraries.
AC_SEARCH_LIBS(dlsym, dl,, [AC_MSG_ERROR([Cannot find libdl])])
AC_SEARCH_LIBS(cos, m,, [AC_MSG_ERROR([Cannot find libm])])
AC_SEARCH_LIBS(nanosleep, rt,, [AC_MSG_ERROR([Cannot find nanosleep])])
AC_SEARCH_LIBS(socket, socket,, [AC_MSG_ERROR([Cannot find socket])])

AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([assert.h dlfcn.h errno.h fcntl.h signal.h     \
                  stdint.h stdlib.h string.h strings.h stdarg.h \
                  errno.h stddef.h                              \
                  sys/mman.h sys/resource.h sys/stat.h          \
                  sys/time.h sys/types.h sys/wait.h time.h      \
                  unistd.h])

AC_CHECK_HEADERS([arpa/inet.h dirent.h fstab.h inttypes.h limits.h mntent.h net/if.h netinet/in.h pwd.h signal.h termios.h sys/mman.h sys/select.h sys/socket.h sys/times.h sys/timex.h sys/types.h math.h complex.h])

AM_COND_IF([WANT_POSIX],
  [AC_CHECK_LIB([rt],[mq_open])
   AC_CHECK_HEADERS([fnmatch.h ftw.h glob.h grp.h mqueue.h netdb.h paths.h poll.h utime.h regex.h wordexp.h sys/ioctl.h sys/mount.h sys/un.h sys/utsname.h sys/uio.h semaphore.h])])

AM_COND_IF([WANT_LINUX],
  [AC_CHECK_HEADERS([sys/epoll.h sys/signalfd.h sys/timerfd.h sys/inotify.h])])

AC_HEADER_TIME

AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_LONG_LONG_INT
AC_TYPE_UNSIGNED_LONG_LONG_INT
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UID_T
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

VICARE_CHECK_PAGESIZE
AC_DEFINE_UNQUOTED([IK_PAGESIZE],[$vicare_cv_pagesize],[Platform page size.])
VICARE_CHECK_PAGESHIFT($vicare_cv_pagesize)
AC_DEFINE_UNQUOTED([IK_PAGESHIFT],[$vicare_cv_pageshift],[Page shift bit count.])

dnl Declared obsolete by GNU Autoconf.
dnl
dnl AC_STRUCT_TM

AC_CACHE_SAVE

#page
#### checks for library functions

AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_CHECK_FUNCS([open close read write lseek fcntl],,
   [AC_MSG_ERROR([missing needed platform function],[1])])
AC_CHECK_FUNCS([bzero memmove memset mmap munmap sqrt strerror getaddrinfo],,
   [AC_MSG_ERROR([missing needed platform function],[1])])
AC_CHECK_FUNCS([stat lstat realpath unlink mkdir],,
   [AC_MSG_ERROR([missing needed platform function],[1])])
AC_CHECK_FUNCS([gettimeofday time localtime_r mktime gmtime_r strftime],,
   [AC_MSG_ERROR([missing needed platform function],[1])])
AC_CHECK_TYPES([struct tm],,,[VICARE_INCLUDES])
AC_CHECK_FUNCS([getenv])
AC_CHECK_DECLS([environ])
AC_CHECK_FUNCS([sigaltstack sigemptyset sigaddset sigfillset sigprocmask sigaction])
AC_CHECK_TYPES([struct timespec])
AC_CHECK_FUNCS([hypot cbrt])

AC_CACHE_SAVE

#page
#### tests for specific features

AC_CACHE_CHECK([if unsetenv has return value],
  [vicare_cv_UNSETENV_HAS_RETURN_VALUE],
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
int main (void)
{
  int retval = unsetenv("PATH");
  return 0;
}])],
    [vicare_cv_UNSETENV_HAS_RETURN_VALUE=1],
    [vicare_cv_UNSETENV_HAS_RETURN_VALUE=0])])
AC_DEFINE_UNQUOTED([UNSETENV_HAS_RETURN_VALUE],
  $vicare_cv_UNSETENV_HAS_RETURN_VALUE,
  [whether the function unsetenv has a return value])

AC_DEFUN([VICARE_SIZEOF],
  [AC_CACHE_CHECK([the size of '$1'],
    [$2],
    [AC_COMPUTE_INT([$2],
      [sizeof($1)],[AC_INCLUDES_DEFAULT],[$2="#f"])])
  $3="$$2"
  AC_SUBST([$3])])

VICARE_SIZEOF([char],[vicare_cv_sizeof_char],[SIZEOF_CHAR])
VICARE_SIZEOF([short],[vicare_cv_sizeof_short],[SIZEOF_SHORT])
VICARE_SIZEOF([int],[vicare_cv_sizeof_int],[SIZEOF_INT])
VICARE_SIZEOF([long],[vicare_cv_sizeof_long],[SIZEOF_LONG])
VICARE_SIZEOF([long long],[vicare_cv_sizeof_long_long],[SIZEOF_LONG_LONG])

VICARE_SIZEOF([size_t],[vicare_cv_sizeof_size_t],[SIZEOF_SIZE_T])
VICARE_SIZEOF([ssize_t],[vicare_cv_sizeof_ssize_t],[SIZEOF_SSIZE_T])
VICARE_SIZEOF([off_t],[vicare_cv_sizeof_off_t],[SIZEOF_OFF_T])
VICARE_SIZEOF([ptrdiff_t],[vicare_cv_sizeof_ptrdiff_t],[SIZEOF_PTRDIFF_T])

AC_CACHE_SAVE

#page
#### POSIX system inspection

AC_CHECK_MEMBER([struct stat.st_atimespec],
   AC_DEFINE(HAVE_STAT_ST_ATIMESPEC, 1,
             [stat struct has st_atimespec field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_atim],
   AC_DEFINE(HAVE_STAT_ST_ATIM, 1,
             [stat struct has st_atim field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_atime_usec],
   AC_DEFINE(HAVE_STAT_ST_ATIME_USEC, 1,
             [stat struct has st_atime_usec field]),
   [],
   [[#include <sys/stat.h>]])

dnl --------------------------------------------------------------------

AC_CHECK_MEMBER([struct stat.st_mtimespec],
   AC_DEFINE(HAVE_STAT_ST_MTIMESPEC, 1,
             [stat struct has st_mtimespec field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_mtim],
   AC_DEFINE(HAVE_STAT_ST_MTIM, 1,
             [stat struct has st_mtim field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_mtime_usec],
   AC_DEFINE(HAVE_STAT_ST_MTIME_USEC, 1,
             [stat struct has st_mtime_usec field]),
   [],
   [[#include <sys/stat.h>]])

dnl --------------------------------------------------------------------

AC_CHECK_MEMBER([struct stat.st_ctimespec],
   AC_DEFINE(HAVE_STAT_ST_CTIMESPEC, 1,
             [stat struct has st_ctimespec field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_ctim],
   AC_DEFINE(HAVE_STAT_ST_CTIM, 1,
             [stat struct has st_ctim field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_ctime_usec],
   AC_DEFINE(HAVE_STAT_ST_CTIME_USEC, 1,
             [stat struct has st_ctime_usec field]),
   [],
   [[#include <sys/stat.h>]])

dnl --------------------------------------------------------------------

AM_COND_IF([WANT_POSIX],[
  AC_CHECK_FUNCS([setenv unsetenv])
  AC_CHECK_FUNCS([getpid getppid])
  AC_CHECK_FUNCS([system fork execv execve execvp])
  AC_CHECK_FUNCS([waitpid wait])
  VICARE_CHECK_WMACROS([WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WCOREDUMP WIFSTOPPED WSTOPSIG])
  AC_CHECK_FUNCS([raise kill pause])
  AC_CHECK_FUNCS([fstat access])
  AC_CHECK_FUNCS([chown fchown chmod fchmod umask utime utimes lutimes futimes])
  AC_CHECK_FUNCS([link symlink readlink remove rename])
  AC_CHECK_FUNCS([rmdir getcwd chdir fchdir opendir fdopendir readdir closedir rewinddir telldir seekdir])
  AC_CHECK_FUNCS([open close read pread write pwrite lseek readv writev poll select ioctl dup dup2 pipe mkfifo truncate ftruncate lockf])
  AC_CHECK_FUNCS([msync mremap madvise mlock munlock mlockall munlockall mprotect])
  AC_CHECK_TYPES([struct sockaddr_un],,,[VICARE_INCLUDES])
  AC_CHECK_TYPES([struct sockaddr_in],,,[VICARE_INCLUDES])
  AC_CHECK_TYPES([struct sockaddr_in6],,,[VICARE_INCLUDES])
  AC_CHECK_TYPES([struct in_addr],,,[VICARE_INCLUDES])
  AC_CHECK_TYPES([struct in6_addr],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([inet_aton inet_ntoa inet_pton inet_ntop htonl htons ntohl ntohs])
  AC_CHECK_TYPES([struct hostent],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([gethostbyname gethostbyname2 gethostbyaddr sethostent gethostent endhostent])
  AC_CHECK_TYPES([struct addrinfo],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getaddrinfo gai_strerror])
  AC_CHECK_TYPES([struct protoent],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getprotobyname getprotobynumber setprotoent getprotoent endprotoent])
  AC_CHECK_TYPES([struct servent],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getservbyname getservbyport setservent getservent endservent])
  AC_CHECK_TYPES([struct netent],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getnetbyname getnetbyaddr setnetent getnetent endnetent])
  AC_CHECK_FUNCS([socket shutdown socketpair connect listen accept bind getpeername getsockname])
  AC_CHECK_FUNCS([send recv sendto recvfrom])
  AC_CHECK_FUNCS([getsockopt setsockopt])
  AC_CHECK_FUNCS([getuid getgid geteuid getegid getgroups seteuid setuid setreuid setegid setgid setregid getlogin])
  AC_CHECK_TYPES([struct passwd],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getpwuid getpwnam setpwent getpwent endpwent])
  AC_CHECK_TYPES([struct group],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([getgrgid getgrnam setgrent getgrent endgrent])
  AC_CHECK_FUNCS([ctermid setsid getsid getpgrp setpgid tcgetpgrp tcsetpgrp tcgetsid])
  AC_CHECK_TYPES([struct tms],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([clock times timelocal timegm nanosleep])
  AC_CHECK_FUNCS([setitimer getitimer alarm])
  AC_CHECK_FUNCS([getrusage getrlimit setrlimit])
  AC_CHECK_FUNCS([sysconf pathconf fpathconf confstr])

  dnl These require linking with -lrt.
  AC_CHECK_FUNCS([mq_open mq_close mq_unlink mq_send mq_receive mq_setattr mq_getattr mq_timedsend mq_timedreceive])
  dnl These require linking with -lrt.
  AC_CHECK_FUNCS([clock_getres clock_gettime clock_settime clock_getcpuclockid])
  dnl These require linking with -lrt.
  AC_CHECK_FUNCS([shm_open shm_unlink])
  dnl These require linking with -lrt.
  AC_CHECK_FUNCS([sem_init sem_destroy sem_open sem_close sem_unlink sem_post sem_wait sem_trywait sem_timedwait sem_getvalue])
  dnl These require linking with -lrt.
  AC_CHECK_FUNCS([timer_create timer_delete timer_settime timer_gettime timer_getoverrun])

  AC_CHECK_FUNCS([sigwaitinfo sigtimedwait])

  AC_CHECK_MEMBER([siginfo_t.si_signo],
     [AC_DEFINE([HAVE_SIGINFO_SI_SIGNO],[1],[siginfo_t struct has si_signo field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_errno],
     [AC_DEFINE([HAVE_SIGINFO_SI_ERRNO],[1],[siginfo_t struct has si_errno field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_code],
     [AC_DEFINE([HAVE_SIGINFO_SI_CODE],[1],[siginfo_t struct has si_code field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_trapno],
     [AC_DEFINE([HAVE_SIGINFO_SI_TRAPNO],[1],[siginfo_t struct has si_trapno field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_pid],
     [AC_DEFINE([HAVE_SIGINFO_SI_PID],[1],[siginfo_t struct has si_pid field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_uid],
     [AC_DEFINE([HAVE_SIGINFO_SI_UID],[1],[siginfo_t struct has si_uid field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_status],
     [AC_DEFINE([HAVE_SIGINFO_SI_STATUS],[1],[siginfo_t struct has si_status field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_utime],
     [AC_DEFINE([HAVE_SIGINFO_SI_UTIME],[1],[siginfo_t struct has si_utime field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_stime],
     [AC_DEFINE([HAVE_SIGINFO_SI_STIME],[1],[siginfo_t struct has si_stime field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_value],
     [AC_DEFINE([HAVE_SIGINFO_SI_VALUE],[1],[siginfo_t struct has si_value field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_int],
     [AC_DEFINE([HAVE_SIGINFO_SI_INT],[1],[siginfo_t struct has si_int field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_ptr],
     [AC_DEFINE([HAVE_SIGINFO_SI_PTR],[1],[siginfo_t struct has si_ptr field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_overrun],
     [AC_DEFINE([HAVE_SIGINFO_SI_OVERRUN],[1],[siginfo_t struct has si_overrun field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_timerid],
     [AC_DEFINE([HAVE_SIGINFO_SI_TIMERID],[1],[siginfo_t struct has si_timerid field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_addr],
     [AC_DEFINE([HAVE_SIGINFO_SI_ADDR],[1],[siginfo_t struct has si_addr field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_band],
     [AC_DEFINE([HAVE_SIGINFO_SI_BAND],[1],[siginfo_t struct has si_band field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_fd],
     [AC_DEFINE([HAVE_SIGINFO_SI_FD],[1],[siginfo_t struct has si_fd field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])
  AC_CHECK_MEMBER([siginfo_t.si_addr_lsb],
     [AC_DEFINE([HAVE_SIGINFO_SI_ADDR_LSB],[1],[siginfo_t struct has si_addr_lsb field])],
     [],[[#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif]])

dnl --------------------------------------------------------------------

  AC_CHECK_MEMBER([struct rusage.ru_utime],
     [AC_DEFINE([HAVE_RUSAGE_RU_UTIME],[1],[struct rusage has ru_utime field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_stime],
     [AC_DEFINE([HAVE_RUSAGE_RU_STIME],[1],[struct rusage has ru_stime field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_maxrss],
     [AC_DEFINE([HAVE_RUSAGE_RU_MAXRSS],[1],[struct rusage has ru_maxrss field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_ixrss],
     [AC_DEFINE([HAVE_RUSAGE_RU_IXRSS],[1],[struct rusage has ru_ixrss field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_idrss],
     [AC_DEFINE([HAVE_RUSAGE_RU_IDRSS],[1],[struct rusage has ru_idrss field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_isrss],
     [AC_DEFINE([HAVE_RUSAGE_RU_ISRSS],[1],[struct rusage has ru_isrss field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_minflt],
     [AC_DEFINE([HAVE_RUSAGE_RU_MINFLT],[1],[struct rusage has ru_minflt field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_majflt],
     [AC_DEFINE([HAVE_RUSAGE_RU_MAJFLT],[1],[struct rusage has ru_majflt field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_nswap],
     [AC_DEFINE([HAVE_RUSAGE_RU_NSWAP],[1],[struct rusage has ru_nswap field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_inblock],
     [AC_DEFINE([HAVE_RUSAGE_RU_INBLOCK],[1],[struct rusage has ru_inblock field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_oublock],
     [AC_DEFINE([HAVE_RUSAGE_RU_OUBLOCK],[1],[struct rusage has ru_oublock field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_msgsnd],
     [AC_DEFINE([HAVE_RUSAGE_RU_MSGSND],[1],[struct rusage has ru_msgsnd field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_msgrcv],
     [AC_DEFINE([HAVE_RUSAGE_RU_MSGRCV],[1],[struct rusage has ru_msgrcv field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_nsignals],
     [AC_DEFINE([HAVE_RUSAGE_RU_NSIGNALS],[1],[struct rusage has ru_nsignals field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_nvcsw],
     [AC_DEFINE([HAVE_RUSAGE_RU_NVCSW],[1],[struct rusage has ru_nvcsw field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])
  AC_CHECK_MEMBER([struct rusage.ru_nivcsw],
     [AC_DEFINE([HAVE_RUSAGE_RU_NIVCSW],[1],[struct rusage has ru_nivcsw field])],
     [],[[#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
#endif]])

  ])

AC_CACHE_SAVE

#page
#### errno codes

AC_DEFUN([VICARE_ERRNO_TEST],[VICARE_VALUEOF_TEST([$1],[$1])])

VICARE_ERRNO_TEST([EPERM])
VICARE_ERRNO_TEST([ENOENT])
VICARE_ERRNO_TEST([ESRCH])
VICARE_ERRNO_TEST([EINTR])
VICARE_ERRNO_TEST([EIO])
VICARE_ERRNO_TEST([ENXIO])
VICARE_ERRNO_TEST([E2BIG])
VICARE_ERRNO_TEST([ENOEXEC])
VICARE_ERRNO_TEST([EBADF])
VICARE_ERRNO_TEST([ECHILD])
VICARE_ERRNO_TEST([EAGAIN])
VICARE_ERRNO_TEST([ENOMEM])
VICARE_ERRNO_TEST([EACCES])
VICARE_ERRNO_TEST([EFAULT])
VICARE_ERRNO_TEST([ENOTBLK])
VICARE_ERRNO_TEST([EBUSY])
VICARE_ERRNO_TEST([EEXIST])
VICARE_ERRNO_TEST([EXDEV])
VICARE_ERRNO_TEST([ENODEV])
VICARE_ERRNO_TEST([ENOTDIR])
VICARE_ERRNO_TEST([EISDIR])
VICARE_ERRNO_TEST([EINVAL])
VICARE_ERRNO_TEST([ENFILE])
VICARE_ERRNO_TEST([EMFILE])
VICARE_ERRNO_TEST([ENOTTY])
VICARE_ERRNO_TEST([ETXTBSY])
VICARE_ERRNO_TEST([EFBIG])
VICARE_ERRNO_TEST([ENOSPC])
VICARE_ERRNO_TEST([ESPIPE])
VICARE_ERRNO_TEST([EROFS])
VICARE_ERRNO_TEST([EMLINK])
VICARE_ERRNO_TEST([EPIPE])
VICARE_ERRNO_TEST([EDOM])
VICARE_ERRNO_TEST([ERANGE])
VICARE_ERRNO_TEST([EDEADLK])
VICARE_ERRNO_TEST([ENAMETOOLONG])
VICARE_ERRNO_TEST([ENOLCK])
VICARE_ERRNO_TEST([ENOSYS])
VICARE_ERRNO_TEST([ENOTEMPTY])
VICARE_ERRNO_TEST([ELOOP])
VICARE_ERRNO_TEST([EWOULDBLOCK])
VICARE_ERRNO_TEST([ENOMSG])
VICARE_ERRNO_TEST([EIDRM])
VICARE_ERRNO_TEST([ECHRNG])
VICARE_ERRNO_TEST([EL2NSYNC])
VICARE_ERRNO_TEST([EL3HLT])
VICARE_ERRNO_TEST([EL3RST])
VICARE_ERRNO_TEST([ELNRNG])
VICARE_ERRNO_TEST([EUNATCH])
VICARE_ERRNO_TEST([ENOCSI])
VICARE_ERRNO_TEST([EL2HLT])
VICARE_ERRNO_TEST([EBADE])
VICARE_ERRNO_TEST([EBADR])
VICARE_ERRNO_TEST([EXFULL])
VICARE_ERRNO_TEST([ENOANO])
VICARE_ERRNO_TEST([EBADRQC])
VICARE_ERRNO_TEST([EBADSLT])
VICARE_ERRNO_TEST([EDEADLOCK])
VICARE_ERRNO_TEST([EBFONT])
VICARE_ERRNO_TEST([ENOSTR])
VICARE_ERRNO_TEST([ENODATA])
VICARE_ERRNO_TEST([ETIME])
VICARE_ERRNO_TEST([ENOSR])
VICARE_ERRNO_TEST([ENONET])
VICARE_ERRNO_TEST([ENOPKG])
VICARE_ERRNO_TEST([EREMOTE])
VICARE_ERRNO_TEST([ENOLINK])
VICARE_ERRNO_TEST([EADV])
VICARE_ERRNO_TEST([ESRMNT])
VICARE_ERRNO_TEST([ECOMM])
VICARE_ERRNO_TEST([EPROTO])
VICARE_ERRNO_TEST([EMULTIHOP])
VICARE_ERRNO_TEST([EDOTDOT])
VICARE_ERRNO_TEST([EBADMSG])
VICARE_ERRNO_TEST([EOVERFLOW])
VICARE_ERRNO_TEST([ENOTUNIQ])
VICARE_ERRNO_TEST([EBADFD])
VICARE_ERRNO_TEST([EREMCHG])
VICARE_ERRNO_TEST([ELIBACC])
VICARE_ERRNO_TEST([ELIBBAD])
VICARE_ERRNO_TEST([ELIBSCN])
VICARE_ERRNO_TEST([ELIBMAX])
VICARE_ERRNO_TEST([ELIBEXEC])
VICARE_ERRNO_TEST([EILSEQ])
VICARE_ERRNO_TEST([ERESTART])
VICARE_ERRNO_TEST([ESTRPIPE])
VICARE_ERRNO_TEST([EUSERS])
VICARE_ERRNO_TEST([ENOTSOCK])
VICARE_ERRNO_TEST([EDESTADDRREQ])
VICARE_ERRNO_TEST([EMSGSIZE])
VICARE_ERRNO_TEST([EPROTOTYPE])
VICARE_ERRNO_TEST([ENOPROTOOPT])
VICARE_ERRNO_TEST([EPROTONOSUPPORT])
VICARE_ERRNO_TEST([ESOCKTNOSUPPORT])
VICARE_ERRNO_TEST([EOPNOTSUPP])
VICARE_ERRNO_TEST([EPFNOSUPPORT])
VICARE_ERRNO_TEST([EAFNOSUPPORT])
VICARE_ERRNO_TEST([EADDRINUSE])
VICARE_ERRNO_TEST([EADDRNOTAVAIL])
VICARE_ERRNO_TEST([ENETDOWN])
VICARE_ERRNO_TEST([ENETUNREACH])
VICARE_ERRNO_TEST([ENETRESET])
VICARE_ERRNO_TEST([ECONNABORTED])
VICARE_ERRNO_TEST([ECONNRESET])
VICARE_ERRNO_TEST([ENOBUFS])
VICARE_ERRNO_TEST([EISCONN])
VICARE_ERRNO_TEST([ENOTCONN])
VICARE_ERRNO_TEST([ESHUTDOWN])
VICARE_ERRNO_TEST([ETOOMANYREFS])
VICARE_ERRNO_TEST([ETIMEDOUT])
VICARE_ERRNO_TEST([ECONNREFUSED])
VICARE_ERRNO_TEST([EHOSTDOWN])
VICARE_ERRNO_TEST([EHOSTUNREACH])
VICARE_ERRNO_TEST([EALREADY])
VICARE_ERRNO_TEST([EINPROGRESS])
VICARE_ERRNO_TEST([ESTALE])
VICARE_ERRNO_TEST([EUCLEAN])
VICARE_ERRNO_TEST([ENOTNAM])
VICARE_ERRNO_TEST([ENAVAIL])
VICARE_ERRNO_TEST([EISNAM])
VICARE_ERRNO_TEST([EREMOTEIO])
VICARE_ERRNO_TEST([EDQUOT])
VICARE_ERRNO_TEST([ENOMEDIUM])
VICARE_ERRNO_TEST([EMEDIUMTYPE])
VICARE_ERRNO_TEST([ECANCELED])
VICARE_ERRNO_TEST([ENOKEY])
VICARE_ERRNO_TEST([EKEYEXPIRED])
VICARE_ERRNO_TEST([EKEYREVOKED])
VICARE_ERRNO_TEST([EKEYREJECTED])
VICARE_ERRNO_TEST([EOWNERDEAD])
VICARE_ERRNO_TEST([ENOTRECOVERABLE])

AC_CACHE_SAVE

#page
#### interprocess signal constants

AC_DEFUN([VICARE_SIGNAL_TEST],[VICARE_VALUEOF_TEST([$1],[$1])])

VICARE_SIGNAL_TEST([NSIG])

# program error
VICARE_SIGNAL_TEST([SIGFPE])
VICARE_SIGNAL_TEST([SIGILL])
VICARE_SIGNAL_TEST([SIGSEGV])
VICARE_SIGNAL_TEST([SIGBUS])
VICARE_SIGNAL_TEST([SIGABRT])
VICARE_SIGNAL_TEST([SIGIOT])
VICARE_SIGNAL_TEST([SIGTRAP])
VICARE_SIGNAL_TEST([SIGEMT])
VICARE_SIGNAL_TEST([SIGSYS])

# termination
VICARE_SIGNAL_TEST([SIGTERM])
VICARE_SIGNAL_TEST([SIGINT])
VICARE_SIGNAL_TEST([SIGQUIT])
VICARE_SIGNAL_TEST([SIGKILL])
VICARE_SIGNAL_TEST([SIGHUP])

# alarm
VICARE_SIGNAL_TEST([SIGALRM])
VICARE_SIGNAL_TEST([SIGVRALRM])
VICARE_SIGNAL_TEST([SIGPROF])

# asynchronous I/O
VICARE_SIGNAL_TEST([SIGIO])
VICARE_SIGNAL_TEST([SIGURG])
VICARE_SIGNAL_TEST([SIGPOLL])

# job control
VICARE_SIGNAL_TEST([SIGCHLD])
VICARE_SIGNAL_TEST([SIGCLD])
VICARE_SIGNAL_TEST([SIGCONT])
VICARE_SIGNAL_TEST([SIGSTOP])
VICARE_SIGNAL_TEST([SIGTSTP])
VICARE_SIGNAL_TEST([SIGTTIN])
VICARE_SIGNAL_TEST([SIGTTOU])

# operation error
VICARE_SIGNAL_TEST([SIGPIPE])
VICARE_SIGNAL_TEST([SIGLOST])
VICARE_SIGNAL_TEST([SIGXCPU])
VICARE_SIGNAL_TEST([SIGXSFZ])

# misc
VICARE_SIGNAL_TEST([SIGUSR1])
VICARE_SIGNAL_TEST([SIGUSR2])
VICARE_SIGNAL_TEST([SIGWINCH])
VICARE_SIGNAL_TEST([SIGINFO])

AC_CACHE_SAVE

#page
#### miscellaneous platform constants

VICARE_CONSTANT_TESTS([CHAR_BIT SCHAR_MIN SCHAR_MAX UCHAR_MAX CHAR_MIN CHAR_MAX SHRT_MIN SHRT_MAX USHRT_MAX INT_MIN INT_MAX UINT_MAX LONG_MIN LONG_MAX ULONG_MAX LONG_LONG_MIN LONG_LONG_MAX ULONG_LONG_MAX WCHAR_MAX RAND_MAX])

## constants for waitpid and similar
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([WAIT_ANY WAIT_MYPGRP WCONTINUED WEXITED
     WNOHANG WNOWAIT WSTOPPED WUNTRACED])],
  [VICARE_CONSTANT_FALSES([WAIT_ANY WAIT_MYPGRP WCONTINUED WEXITED
     WNOHANG WNOWAIT WSTOPPED WUNTRACED])])
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([P_PID P_PGID P_ALL])],
  [VICARE_CONSTANT_FALSES([P_PID P_PGID P_ALL])])
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([CLD_CONTINUED CLD_DUMPED CLD_EXITED
     CLD_KILLED CLD_STOPPED CLD_TRAPPED])],
  [VICARE_CONSTANT_FALSES([CLD_CONTINUED CLD_DUMPED CLD_EXITED
     CLD_KILLED CLD_STOPPED CLD_TRAPPED])])

dnl These are not fixnums.
dnl
dnl VICARE_CONSTANT_TEST([__WCLONE])
dnl VICARE_CONSTANT_TEST([__WALL])
dnl VICARE_CONSTANT_TEST([__WNOTHREAD])

## constants for file system inspection

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([S_IFDIR S_IFCHR S_IFBLK S_IFREG S_IFLNK S_IFSOCK S_IFIFO])],
  [VICARE_CONSTANT_FALSES([S_IFDIR S_IFCHR S_IFBLK S_IFREG S_IFLNK S_IFSOCK S_IFIFO])])

AC_CACHE_SAVE

## seek whence arguments
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SEEK_SET SEEK_CUR SEEK_END])],
  [VICARE_CONSTANT_FALSES([SEEK_SET SEEK_CUR SEEK_END])])

## file descriptor related flags
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([O_ACCMODE O_APPEND O_ASYNC O_CREAT O_EXCL
     O_EXEC O_EXLOCK O_FSYNC O_IGNORE_CTTY O_NDELAY O_NOCTTY O_NOLINK
     O_NONBLOCK O_NOTRANS O_RDONLY O_RDWR O_READ O_SHLOCK O_SYNC
     O_TRUNC O_WRITE O_WRONLY])],
  [VICARE_CONSTANT_FALSES([O_ACCMODE O_APPEND O_ASYNC O_CREAT O_EXCL
     O_EXEC O_EXLOCK O_FSYNC O_IGNORE_CTTY O_NDELAY O_NOCTTY O_NOLINK
     O_NONBLOCK O_NOTRANS O_RDONLY O_RDWR O_READ O_SHLOCK O_SYNC
     O_TRUNC O_WRITE O_WRONLY])])

## This is GNU specific.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TEST([O_NOATIME])],
  [VICARE_CONSTANT_FALSE([O_NOATIME])])

## These are Linux specific.
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([O_CLOEXEC O_DIRECT O_DIRECTORY O_LARGEFILE O_NOFOLLOW FD_CLOEXEC])],
  [VICARE_CONSTANT_FALSES([O_CLOEXEC O_DIRECT O_DIRECTORY O_LARGEFILE O_NOFOLLOW FD_CLOEXEC])])
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([EPOLL_CLOEXEC EPOLL_CTL_ADD EPOLL_CTL_MOD
     EPOLL_CTL_DEL EPOLLIN EPOLLOUT EPOLLRDHUP EPOLLPRI EPOLLERR
     EPOLLHUP EPOLLET EPOLLONESHOT])],
  [VICARE_CONSTANT_FALSES([EPOLL_CLOEXEC EPOLL_CTL_ADD EPOLL_CTL_MOD
     EPOLL_CTL_DEL EPOLLIN EPOLLOUT EPOLLRDHUP EPOLLPRI EPOLLERR
     EPOLLHUP EPOLLET EPOLLONESHOT])])

## Memory-mapped input/output.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([MAP_PRIVATE MAP_SHARED MAP_FIXED MAP_ANON
     MAP_ANONYMOUS MAP_32BITS MAP_DENYWRITE MAP_EXECUTABLE MAP_FILE
     MAP_GROWSDOWN MAP_LOCKED MAP_NONBLOCK MAP_NORESERVE MAP_POPULATE
     MAP_STACK MAP_AUTOGROW MAP_AUTORESRV MAP_COPY PROT_READ PROT_WRITE
     PROT_EXEC PROT_NONE MS_SYNC MS_ASYNC MREMAP_MAYMOVE MCL_CURRENT MCL_FUTURE])],
  [VICARE_CONSTANT_FALSES([MAP_PRIVATE MAP_SHARED MAP_FIXED MAP_ANON
     MAP_ANONYMOUS MAP_32BITS MAP_DENYWRITE MAP_EXECUTABLE MAP_FILE
     MAP_GROWSDOWN MAP_LOCKED MAP_NONBLOCK MAP_NORESERVE MAP_POPULATE
     MAP_STACK MAP_AUTOGROW MAP_AUTORESRV MAP_COPY PROT_READ PROT_WRITE
     PROT_EXEC PROT_NONE MS_SYNC MS_ASYNC MREMAP_MAYMOVE MCL_CURRENT MCL_FUTURE])])
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([MADV_NORMAL MADV_RANDOM MADV_SEQUENTIAL
     MADV_WILLNEED MADV_DONTNEED])],
  [VICARE_CONSTANT_FALSES([MADV_NORMAL MADV_RANDOM MADV_SEQUENTIAL
     MADV_WILLNEED MADV_DONTNEED])])
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([POSIX_MADV_NORMAL POSIX_MADV_RANDOM
     POSIX_MADV_SEQUENTIAL POSIX_MADV_WILLNEED POSIX_MADV_DONTNEED])],
  [VICARE_CONSTANT_FALSES([POSIX_MADV_NORMAL POSIX_MADV_RANDOM
     POSIX_MADV_SEQUENTIAL POSIX_MADV_WILLNEED POSIX_MADV_DONTNEED])])

AC_CACHE_SAVE

## ioctl action selection
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([F_DUPFD F_GETFD F_GETFL F_GETLK
     F_GETOWN F_SETFD F_SETFL F_SETLKW F_SETLK F_SETOWN
     F_RDLCK F_UNLCK F_WRLCK])],
  [VICARE_CONSTANT_FALSES([F_DUPFD F_GETFD F_GETFL F_GETLK
     F_GETOWN F_SETFD F_SETFL F_SETLKW F_SETLK F_SETOWN
     F_RDLCK F_UNLCK F_WRLCK])])

## miscellaneous file-related constants
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([R_OK W_OK X_OK F_OK])],
  [VICARE_CONSTANT_FALSES([R_OK W_OK X_OK F_OK])])

## miscellaneous constants
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([L_ctermid L_tmpnam CLOCKS_PER_SEC])],
  [VICARE_CONSTANT_FALSES([L_ctermid L_tmpnam CLOCKS_PER_SEC])])

## mode bits
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP
     S_IXGRP S_IROTH S_IWOTH S_IXOTH S_IRWXU S_IRWXG S_IRWXO
     S_ISUID S_ISGID S_ISVTX])],
  [VICARE_CONSTANT_FALSES([S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP
     S_IXGRP S_IROTH S_IWOTH S_IXOTH S_IRWXU S_IRWXG S_IRWXO
     S_ISUID S_ISGID S_ISVTX])])

## dirent stuff
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([DT_BLK DT_CHR DT_DIR DT_FIFO DT_LNK
     DT_REG DT_SOCK DT_UNKNOWN])],
  [VICARE_CONSTANT_FALSES([DT_BLK DT_CHR DT_DIR DT_FIFO DT_LNK
     DT_REG DT_SOCK DT_UNKNOWN])])

## constants related to ftw() and nftw()
AM_COND_IF([WANT_GLIBC],
  [VICARE_CONSTANT_TESTS([FTW_F FTW_D FTW_NS FTW_DNR FTW_SL FTW_DP
     FTW_SLN FTW_PHYS FTW_MOUNT FTW_CHDIR FTW_DEPTH FTW_ACTIONRETVAL
     FTW_CONTINUE FTW_STOP FTW_SKIP_SUBTREE FTW_SKIP_SIBLINGS])],
  [VICARE_CONSTANT_FALSES([FTW_F FTW_D FTW_NS FTW_DNR FTW_SL FTW_DP
     FTW_SLN FTW_PHYS FTW_MOUNT FTW_CHDIR FTW_DEPTH FTW_ACTIONRETVAL
     FTW_CONTINUE FTW_STOP FTW_SKIP_SUBTREE FTW_SKIP_SIBLINGS])])

## setitimer() stuff
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([ITIMER_REAL ITIMER_VIRTUAL ITIMER_PROF])],
  [VICARE_CONSTANT_FALSES([ITIMER_REAL ITIMER_VIRTUAL ITIMER_PROF])])

## miscellaneous
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([EOF MAXSYMLINKS MAXPATHLEN])],
  [VICARE_CONSTANT_FALSES([EOF MAXSYMLINKS MAXPATHLEN])])

## select related symbols
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([FD_SETSIZE])],
  [VICARE_CONSTANT_FALSES([FD_SETSIZE])])

## poll related symbols
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([POLLIN POLLPRI POLLOUT POLLRDHUP POLLERR POLLHUP POLLNVAL])],
  [VICARE_CONSTANT_FALSES([POLLIN POLLPRI POLLOUT POLLRDHUP POLLERR POLLHUP POLLNVAL])])

## max size of string for cuserid()
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TEST([L_cuserid])],
  [VICARE_CONSTANT_FALSE([L_cuserid])])

AC_CACHE_SAVE

## system capacity limitations
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([ARG_MAX _POSIX_ARG_MAX CHILD_MAX
     _POSIX_CHILD_MAX OPEN_MAX _POSIX_OPEN_MAX STREAM_MAX
     _POSIX_STREAM_MAX TZNAME_MAX _POSIX_TZNAME_MAX NGROUPS_MAX
     SSIZE_MAX RE_DUP_MAX])],
  [VICARE_CONSTANT_FALSES([ARG_MAX _POSIX_ARG_MAX CHILD_MAX
     _POSIX_CHILD_MAX OPEN_MAX _POSIX_OPEN_MAX STREAM_MAX
     _POSIX_STREAM_MAX TZNAME_MAX _POSIX_TZNAME_MAX NGROUPS_MAX
     SSIZE_MAX RE_DUP_MAX])])

## overall system options
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_POSIX_JOB_CONTROL _POSIX_SAVED_IDS
     _POSIX2_C_DEV _POSIX2_FORT_DEV _POSIX2_FORT_RUN
     _POSIX2_LOCALEDEF _POSIX2_SW_DEV])],
  [VICARE_CONSTANT_FALSES([_POSIX_JOB_CONTROL _POSIX_SAVED_IDS
     _POSIX2_C_DEV _POSIX2_FORT_DEV _POSIX2_FORT_RUN
     _POSIX2_LOCALEDEF _POSIX2_SW_DEV])])

## supported POSIX version
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_POSIX_VERSION _POSIX2_C_VERSION])],
  [VICARE_CONSTANT_FALSES([_POSIX_VERSION _POSIX2_C_VERSION])])

## sysconf constants
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_SC_ARG_MAX _SC_CHILD_MAX _SC_CLK_TCK _SC_NGROUPS_MAX _SC_OPEN_MAX _SC_STREAM_MAX _SC_TZNAME_MAX _SC_JOB_CONTROL _SC_SAVED_IDS _SC_REALTIME_SIGNALS _SC_PRIORITY_SCHEDULING _SC_TIMERS _SC_ASYNCHRONOUS_IO _SC_PRIORITIZED_IO _SC_SYNCHRONIZED_IO _SC_FSYNC _SC_MAPPED_FILES _SC_MEMLOCK _SC_MEMLOCK_RANGE _SC_MEMORY_PROTECTION _SC_MESSAGE_PASSING _SC_SEMAPHORES _SC_SHARED_MEMORY_OBJECTS _SC_AIO_LISTIO_MAX _SC_AIO_MAX _SC_AIO_PRIO_DELTA_MAX _SC_DELAYTIMER_MAX _SC_MQ_OPEN_MAX _SC_MQ_PRIO_MAX _SC_VERSION _SC_PAGESIZE _SC_PAGE_SIZE _SC_RTSIG_MAX _SC_SEM_NSEMS_MAX _SC_SEM_VALUE_MAX _SC_SIGQUEUE_MAX _SC_TIMER_MAX _SC_BC_BASE_MAX _SC_BC_DIM_MAX _SC_BC_SCALE_MAX _SC_BC_STRING_MAX _SC_COLL_WEIGHTS_MAX _SC_EQUIV_CLASS_MAX _SC_EXPR_NEST_MAX _SC_LINE_MAX _SC_RE_DUP_MAX _SC_CHARCLASS_NAME_MAX _SC_2_VERSION _SC_2_C_BIND _SC_2_C_DEV _SC_2_FORT_DEV _SC_2_FORT_RUN _SC_2_SW_DEV _SC_2_LOCALEDEF _SC_PII _SC_PII_XTI _SC_PII_SOCKET _SC_PII_INTERNET _SC_PII_OSI _SC_POLL _SC_SELECT _SC_UIO_MAXIOV _SC_IOV_MAX _SC_PII_INTERNET_STREAM _SC_PII_INTERNET_DGRAM _SC_PII_OSI_COTS _SC_PII_OSI_CLTS _SC_PII_OSI_M _SC_T_IOV_MAX _SC_THREADS _SC_THREAD_SAFE_FUNCTIONS _SC_GETGR_R_SIZE_MAX _SC_GETPW_R_SIZE_MAX _SC_LOGIN_NAME_MAX _SC_TTY_NAME_MAX _SC_THREAD_DESTRUCTOR_ITERATIONS _SC_THREAD_KEYS_MAX _SC_THREAD_STACK_MIN _SC_THREAD_THREADS_MAX _SC_THREAD_ATTR_STACKADDR _SC_THREAD_ATTR_STACKSIZE _SC_THREAD_PRIORITY_SCHEDULING _SC_THREAD_PRIO_INHERIT _SC_THREAD_PRIO_PROTECT _SC_THREAD_PROCESS_SHARED _SC_NPROCESSORS_CONF _SC_NPROCESSORS_ONLN _SC_PHYS_PAGES _SC_AVPHYS_PAGES _SC_ATEXIT_MAX _SC_PASS_MAX _SC_XOPEN_VERSION _SC_XOPEN_XCU_VERSION _SC_XOPEN_UNIX _SC_XOPEN_CRYPT _SC_XOPEN_ENH_I18N _SC_XOPEN_SHM _SC_2_CHAR_TERM _SC_2_C_VERSION _SC_2_UPE _SC_XOPEN_XPG2 _SC_XOPEN_XPG3 _SC_XOPEN_XPG4 _SC_CHAR_BIT _SC_CHAR_MAX _SC_CHAR_MIN _SC_INT_MAX _SC_INT_MIN _SC_LONG_BIT _SC_WORD_BIT _SC_MB_LEN_MAX _SC_NZERO _SC_SSIZE_MAX _SC_SCHAR_MAX _SC_SCHAR_MIN _SC_SHRT_MAX _SC_SHRT_MIN _SC_UCHAR_MAX _SC_UINT_MAX _SC_ULONG_MAX _SC_USHRT_MAX _SC_NL_ARGMAX _SC_NL_LANGMAX _SC_NL_MSGMAX _SC_NL_NMAX _SC_NL_SETMAX _SC_NL_TEXTMAX _SC_XBS5_ILP32_OFF32 _SC_XBS5_ILP32_OFFBIG _SC_XBS5_LP64_OFF64 _SC_XBS5_LPBIG_OFFBIG _SC_XOPEN_LEGACY _SC_XOPEN_REALTIME _SC_XOPEN_REALTIME_THREADS _SC_ADVISORY_INFO _SC_BARRIERS _SC_BASE _SC_C_LANG_SUPPORT _SC_C_LANG_SUPPORT_R _SC_CLOCK_SELECTION _SC_CPUTIME _SC_THREAD_CPUTIME _SC_DEVICE_IO _SC_DEVICE_SPECIFIC _SC_DEVICE_SPECIFIC_R _SC_FD_MGMT _SC_FIFO _SC_PIPE _SC_FILE_ATTRIBUTES _SC_FILE_LOCKING _SC_FILE_SYSTEM _SC_MONOTONIC_CLOCK _SC_MULTI_PROCESS _SC_SINGLE_PROCESS _SC_NETWORKING _SC_READER_WRITER_LOCKS _SC_SPIN_LOCKS _SC_REGEXP _SC_REGEX_VERSION _SC_SHELL _SC_SIGNALS _SC_SPAWN _SC_SPORADIC_SERVER _SC_THREAD_SPORADIC_SERVER _SC_SYSTEM_DATABASE _SC_SYSTEM_DATABASE_R _SC_TIMEOUTS _SC_TYPED_MEMORY_OBJECTS _SC_USER_GROUPS _SC_USER_GROUPS_R _SC_2_PBS _SC_2_PBS_ACCOUNTING _SC_2_PBS_LOCATE _SC_2_PBS_MESSAGE _SC_2_PBS_TRACK _SC_SYMLOOP_MAX _SC_STREAMS _SC_2_PBS_CHECKPOINT _SC_V6_ILP32_OFF32 _SC_V6_ILP32_OFFBIG _SC_V6_LP64_OFF64 _SC_V6_LPBIG_OFFBIG _SC_HOST_NAME_MAX _SC_TRACE _SC_TRACE_EVENT_FILTER _SC_TRACE_INHERIT _SC_TRACE_LOG _SC_LEVEL1_ICACHE_SIZE _SC_LEVEL1_ICACHE_ASSOC _SC_LEVEL1_ICACHE_LINESIZE _SC_LEVEL1_DCACHE_SIZE _SC_LEVEL1_DCACHE_ASSOC _SC_LEVEL1_DCACHE_LINESIZE _SC_LEVEL2_CACHE_SIZE _SC_LEVEL2_CACHE_ASSOC _SC_LEVEL2_CACHE_LINESIZE _SC_LEVEL3_CACHE_SIZE _SC_LEVEL3_CACHE_ASSOC _SC_LEVEL3_CACHE_LINESIZE _SC_LEVEL4_CACHE_SIZE _SC_LEVEL4_CACHE_ASSOC _SC_LEVEL4_CACHE_LINESIZE _SC_IPV6 _SC_RAW_SOCKETS])],
  [VICARE_CONSTANT_FALSES([_SC_ARG_MAX _SC_CHILD_MAX _SC_CLK_TCK _SC_NGROUPS_MAX _SC_OPEN_MAX _SC_STREAM_MAX _SC_TZNAME_MAX _SC_JOB_CONTROL _SC_SAVED_IDS _SC_REALTIME_SIGNALS _SC_PRIORITY_SCHEDULING _SC_TIMERS _SC_ASYNCHRONOUS_IO _SC_PRIORITIZED_IO _SC_SYNCHRONIZED_IO _SC_FSYNC _SC_MAPPED_FILES _SC_MEMLOCK _SC_MEMLOCK_RANGE _SC_MEMORY_PROTECTION _SC_MESSAGE_PASSING _SC_SEMAPHORES _SC_SHARED_MEMORY_OBJECTS _SC_AIO_LISTIO_MAX _SC_AIO_MAX _SC_AIO_PRIO_DELTA_MAX _SC_DELAYTIMER_MAX _SC_MQ_OPEN_MAX _SC_MQ_PRIO_MAX _SC_VERSION _SC_PAGESIZE _SC_PAGE_SIZE _SC_RTSIG_MAX _SC_SEM_NSEMS_MAX _SC_SEM_VALUE_MAX _SC_SIGQUEUE_MAX _SC_TIMER_MAX _SC_BC_BASE_MAX _SC_BC_DIM_MAX _SC_BC_SCALE_MAX _SC_BC_STRING_MAX _SC_COLL_WEIGHTS_MAX _SC_EQUIV_CLASS_MAX _SC_EXPR_NEST_MAX _SC_LINE_MAX _SC_RE_DUP_MAX _SC_CHARCLASS_NAME_MAX _SC_2_VERSION _SC_2_C_BIND _SC_2_C_DEV _SC_2_FORT_DEV _SC_2_FORT_RUN _SC_2_SW_DEV _SC_2_LOCALEDEF _SC_PII _SC_PII_XTI _SC_PII_SOCKET _SC_PII_INTERNET _SC_PII_OSI _SC_POLL _SC_SELECT _SC_UIO_MAXIOV _SC_IOV_MAX _SC_PII_INTERNET_STREAM _SC_PII_INTERNET_DGRAM _SC_PII_OSI_COTS _SC_PII_OSI_CLTS _SC_PII_OSI_M _SC_T_IOV_MAX _SC_THREADS _SC_THREAD_SAFE_FUNCTIONS _SC_GETGR_R_SIZE_MAX _SC_GETPW_R_SIZE_MAX _SC_LOGIN_NAME_MAX _SC_TTY_NAME_MAX _SC_THREAD_DESTRUCTOR_ITERATIONS _SC_THREAD_KEYS_MAX _SC_THREAD_STACK_MIN _SC_THREAD_THREADS_MAX _SC_THREAD_ATTR_STACKADDR _SC_THREAD_ATTR_STACKSIZE _SC_THREAD_PRIORITY_SCHEDULING _SC_THREAD_PRIO_INHERIT _SC_THREAD_PRIO_PROTECT _SC_THREAD_PROCESS_SHARED _SC_NPROCESSORS_CONF _SC_NPROCESSORS_ONLN _SC_PHYS_PAGES _SC_AVPHYS_PAGES _SC_ATEXIT_MAX _SC_PASS_MAX _SC_XOPEN_VERSION _SC_XOPEN_XCU_VERSION _SC_XOPEN_UNIX _SC_XOPEN_CRYPT _SC_XOPEN_ENH_I18N _SC_XOPEN_SHM _SC_2_CHAR_TERM _SC_2_C_VERSION _SC_2_UPE _SC_XOPEN_XPG2 _SC_XOPEN_XPG3 _SC_XOPEN_XPG4 _SC_CHAR_BIT _SC_CHAR_MAX _SC_CHAR_MIN _SC_INT_MAX _SC_INT_MIN _SC_LONG_BIT _SC_WORD_BIT _SC_MB_LEN_MAX _SC_NZERO _SC_SSIZE_MAX _SC_SCHAR_MAX _SC_SCHAR_MIN _SC_SHRT_MAX _SC_SHRT_MIN _SC_UCHAR_MAX _SC_UINT_MAX _SC_ULONG_MAX _SC_USHRT_MAX _SC_NL_ARGMAX _SC_NL_LANGMAX _SC_NL_MSGMAX _SC_NL_NMAX _SC_NL_SETMAX _SC_NL_TEXTMAX _SC_XBS5_ILP32_OFF32 _SC_XBS5_ILP32_OFFBIG _SC_XBS5_LP64_OFF64 _SC_XBS5_LPBIG_OFFBIG _SC_XOPEN_LEGACY _SC_XOPEN_REALTIME _SC_XOPEN_REALTIME_THREADS _SC_ADVISORY_INFO _SC_BARRIERS _SC_BASE _SC_C_LANG_SUPPORT _SC_C_LANG_SUPPORT_R _SC_CLOCK_SELECTION _SC_CPUTIME _SC_THREAD_CPUTIME _SC_DEVICE_IO _SC_DEVICE_SPECIFIC _SC_DEVICE_SPECIFIC_R _SC_FD_MGMT _SC_FIFO _SC_PIPE _SC_FILE_ATTRIBUTES _SC_FILE_LOCKING _SC_FILE_SYSTEM _SC_MONOTONIC_CLOCK _SC_MULTI_PROCESS _SC_SINGLE_PROCESS _SC_NETWORKING _SC_READER_WRITER_LOCKS _SC_SPIN_LOCKS _SC_REGEXP _SC_REGEX_VERSION _SC_SHELL _SC_SIGNALS _SC_SPAWN _SC_SPORADIC_SERVER _SC_THREAD_SPORADIC_SERVER _SC_SYSTEM_DATABASE _SC_SYSTEM_DATABASE_R _SC_TIMEOUTS _SC_TYPED_MEMORY_OBJECTS _SC_USER_GROUPS _SC_USER_GROUPS_R _SC_2_PBS _SC_2_PBS_ACCOUNTING _SC_2_PBS_LOCATE _SC_2_PBS_MESSAGE _SC_2_PBS_TRACK _SC_SYMLOOP_MAX _SC_STREAMS _SC_2_PBS_CHECKPOINT _SC_V6_ILP32_OFF32 _SC_V6_ILP32_OFFBIG _SC_V6_LP64_OFF64 _SC_V6_LPBIG_OFFBIG _SC_HOST_NAME_MAX _SC_TRACE _SC_TRACE_EVENT_FILTER _SC_TRACE_INHERIT _SC_TRACE_LOG _SC_LEVEL1_ICACHE_SIZE _SC_LEVEL1_ICACHE_ASSOC _SC_LEVEL1_ICACHE_LINESIZE _SC_LEVEL1_DCACHE_SIZE _SC_LEVEL1_DCACHE_ASSOC _SC_LEVEL1_DCACHE_LINESIZE _SC_LEVEL2_CACHE_SIZE _SC_LEVEL2_CACHE_ASSOC _SC_LEVEL2_CACHE_LINESIZE _SC_LEVEL3_CACHE_SIZE _SC_LEVEL3_CACHE_ASSOC _SC_LEVEL3_CACHE_LINESIZE _SC_LEVEL4_CACHE_SIZE _SC_LEVEL4_CACHE_ASSOC _SC_LEVEL4_CACHE_LINESIZE _SC_IPV6 _SC_RAW_SOCKETS])])

AC_CACHE_SAVE

## limits on file system capacity
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([LINK_MAX MAX_CANON MAX_INPUT NAME_MAX
     PATH_MAX PIPE_BUF MAXNAMLEN FILENAME_MAX])],
  [VICARE_CONSTANT_FALSES([LINK_MAX MAX_CANON MAX_INPUT NAME_MAX
     PATH_MAX PIPE_BUF MAXNAMLEN FILENAME_MAX])])

## optional features in file support
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_POSIX_CHOWN_RESTRICTED _POSIX_NO_TRUNC _POSIX_VDISABLE])],
  [VICARE_CONSTANT_FALSES([_POSIX_CHOWN_RESTRICTED _POSIX_NO_TRUNC _POSIX_VDISABLE])])

## minimum values for file system limits
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_POSIX_LINK_MAX _POSIX_MAX_CANON
     _POSIX_MAX_INPUT _POSIX_NAME_MAX _POSIX_PATH_MAX _POSIX_PIPE_BUF
     SYMLINK_MAX POSIX_REC_INCR_XFER_SIZE POSIX_REC_MAX_XFER_SIZE
     POSIX_REC_MIN_XFER_SIZE POSIX_REC_XFER_ALIGN])],
  [VICARE_CONSTANT_FALSES([_POSIX_LINK_MAX _POSIX_MAX_CANON
     _POSIX_MAX_INPUT _POSIX_NAME_MAX _POSIX_PATH_MAX _POSIX_PIPE_BUF
     SYMLINK_MAX POSIX_REC_INCR_XFER_SIZE POSIX_REC_MAX_XFER_SIZE
     POSIX_REC_MIN_XFER_SIZE POSIX_REC_XFER_ALIGN])])

## constants for pathconf and fpathconf
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_PC_LINK_MAX _PC_MAX_CANON _PC_MAX_INPUT
     _PC_NAME_MAX _PC_PATH_MAX _PC_PIPE_BUF _PC_CHOWN_RESTRICTED
     _PC_NO_TRUNC _PC_VDISABLE _PC_SYNC_IO _PC_ASYNC_IO _PC_PRIO_IO
     _PC_FILESIZEBITS _PC_REC_INCR_XFER_SIZE _PC_REC_MAX_XFER_SIZE
     _PC_REC_MIN_XFER_SIZE _PC_REC_XFER_ALIGN])],
  [VICARE_CONSTANT_FALSES([_PC_LINK_MAX _PC_MAX_CANON _PC_MAX_INPUT
     _PC_NAME_MAX _PC_PATH_MAX _PC_PIPE_BUF _PC_CHOWN_RESTRICTED
     _PC_NO_TRUNC _PC_VDISABLE _PC_SYNC_IO _PC_ASYNC_IO _PC_PRIO_IO
     _PC_FILESIZEBITS _PC_REC_INCR_XFER_SIZE _PC_REC_MAX_XFER_SIZE
     _PC_REC_MIN_XFER_SIZE _PC_REC_XFER_ALIGN])])

## glibc system inspection constants
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([BC_BASE_MAX _POSIX_BC_BASE_MAX BC_DIM_MAX
     _POSIX_BC_DIM_MAX BC_SCALE_MAX _POSIX_BC_SCALE_MAX BC_STRING_MAX
     _POSIX_BC_STRING_MAX COLL_WEIGHTS_MAX _POSIX_COLL_WEIGHTS_MAX
     EXPR_NEXT_MAX _POSIX_EXPR_NEXT_MAX LINE_MAX _POSIX_LINE_MAX
     EQUIV_CLASS_MAX _POSIX_EQUIV_CLASS_MAX])],
  [VICARE_CONSTANT_FALSES([BC_BASE_MAX _POSIX_BC_BASE_MAX BC_DIM_MAX
     _POSIX_BC_DIM_MAX BC_SCALE_MAX _POSIX_BC_SCALE_MAX BC_STRING_MAX
     _POSIX_BC_STRING_MAX COLL_WEIGHTS_MAX _POSIX_COLL_WEIGHTS_MAX
     EXPR_NEXT_MAX _POSIX_EXPR_NEXT_MAX LINE_MAX _POSIX_LINE_MAX
     EQUIV_CLASS_MAX _POSIX_EQUIV_CLASS_MAX])])

## string-valued system configuration parameters
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([_CS_PATH _CS_LFS_CFLAGS _CS_LFS_LDFLAGS
     _CS_LFS_LIBS _CS_LFS_LINTFLAGS _CS_LFS64_CFLAGS _CS_LFS64_LDFLAGS
     _CS_LFS64_LIBS _CS_LFS64_LINTFLAGS])],
  [VICARE_CONSTANT_FALSES([_CS_PATH _CS_LFS_CFLAGS _CS_LFS_LDFLAGS
     _CS_LFS_LIBS _CS_LFS_LINTFLAGS _CS_LFS64_CFLAGS _CS_LFS64_LDFLAGS
     _CS_LFS64_LIBS _CS_LFS64_LINTFLAGS])])

## file system files pathnames
AM_COND_IF([WANT_POSIX],
  [VICARE_STRING_CONSTANT_TESTS([_PATH_MNTTAB _PATH_FSTAB _PATH_MOUNTED])],
  [VICARE_CONSTANT_FALSES([_PATH_MNTTAB _PATH_FSTAB _PATH_MOUNTED])])

AC_CACHE_SAVE

## mount options
AM_COND_IF([WANT_POSIX],
  [VICARE_STRING_CONSTANT_TESTS([FSTAB_RW FSTAB_RQ FSTAB_RO FSTAB_SW FSTAB_XX])],
  [VICARE_CONSTANT_FALSES([FSTAB_RW FSTAB_RQ FSTAB_RO FSTAB_SW FSTAB_XX])])

## values for the mnt_type field of struct mtab
AM_COND_IF([WANT_POSIX],
  [VICARE_STRING_CONSTANT_TESTS([MNTTYPE_IGNORE MNTTYPE_NFS MNTTYPE_SWAP])],
  [VICARE_CONSTANT_FALSES([MNTTYPE_IGNORE MNTTYPE_NFS MNTTYPE_SWAP])])

## values for the mnt_opts field of struct mtab
AM_COND_IF([WANT_POSIX],
  [VICARE_STRING_CONSTANT_TESTS([MNTOPT_DEFAULTS MNTOPT_RO MNTOPT_RW
     MNTOPT_SUID MNTOPT_NOSUID MNTOPT_NOAUTO])],
  [VICARE_CONSTANT_FALSES([MNTOPT_DEFAULTS MNTOPT_RO MNTOPT_RW
     MNTOPT_SUID MNTOPT_NOSUID MNTOPT_NOAUTO])])

## options for mount
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([MS_MGC_MASK MS_REMOUNT MS_RDONLY S_IMMUTABLE
     S_APPEND MS_NOSUID MS_NOEXEC MS_NODEV MS_SYNCHRONOUS MS_MANDLOCK
     MS_NOATIME MS_NODIRATIME])],
  [VICARE_CONSTANT_FALSES([MS_MGC_MASK MS_REMOUNT MS_RDONLY S_IMMUTABLE
     S_APPEND MS_NOSUID MS_NOEXEC MS_NODEV MS_SYNCHRONOUS MS_MANDLOCK
     MS_NOATIME MS_NODIRATIME])])

## flags for umount2
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TEST([MNT_FORCE])],
  [VICARE_CONSTANT_FALSE([MNT_FORCE])])

## sockets constants
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_RDM
     SOCK_SEQPACKET SOCK_DCCP SOCK_PACKET SOCK_CLOEXEC SOCK_NONBLOCK
     AF_LOCAL PF_LOCAL AF_UNIX PF_UNIX AF_FILE PF_FILE AF_INET PF_INET
     AF_INET6 PF_INET6 AF_UNSPEC PF_UNSPEC SHUT_RD SHUT_WR SHUT_RDWR
     IPPORT_RESERVED IPPORT_USERRESERVED IFNAMSIZ])],
  [VICARE_CONSTANT_FALSES([SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_RDM
     SOCK_SEQPACKET SOCK_DCCP SOCK_PACKET SOCK_CLOEXEC SOCK_NONBLOCK
     AF_LOCAL PF_LOCAL AF_UNIX PF_UNIX AF_FILE PF_FILE AF_INET PF_INET
     AF_INET6 PF_INET6 AF_UNSPEC PF_UNSPEC SHUT_RD SHUT_WR SHUT_RDWR
     IPPORT_RESERVED IPPORT_USERRESERVED IFNAMSIZ])])

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([MSG_OOB MSG_PEEK MSG_DONTROUTE])],
  [VICARE_CONSTANT_FALSES([MSG_OOB MSG_PEEK MSG_DONTROUTE])])

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SOL_SOCKET SO_DEBUG SO_REUSEADDR
     SO_KEEPALIVE SO_DONTROUTE SO_LINGER SO_BROADCAST SO_OOBINLINE
     SO_SNDBUF SO_RCVBUF SO_TYPE SO_STYLE SO_ERROR])],
  [VICARE_CONSTANT_FALSES([SOL_SOCKET SO_DEBUG SO_REUSEADDR
     SO_KEEPALIVE SO_DONTROUTE SO_LINGER SO_BROADCAST SO_OOBINLINE
     SO_SNDBUF SO_RCVBUF SO_TYPE SO_STYLE SO_ERROR])])

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([INADDR_LOOPBACK INADDR_ANY INADDR_BROADCAST
     INADDR_NONE INET_ADDRSTRLEN INET6_ADDRSTRLEN])],
  [VICARE_CONSTANT_FALSES([INADDR_LOOPBACK INADDR_ANY INADDR_BROADCAST
     INADDR_NONE INET_ADDRSTRLEN INET6_ADDRSTRLEN])])

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([HOST_NOT_FOUND TRY_AGAIN NO_RECOVERY NO_ADDRESS])],
  [VICARE_CONSTANT_FALSES([HOST_NOT_FOUND TRY_AGAIN NO_RECOVERY NO_ADDRESS])])

# getaddrinfo flags
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([AI_ADDRCONFIG AI_ALL AI_CANONNAME
     AI_NUMERIC_HOST AI_NUMERIC_SERV AI_PASSIVE AI_V4MAPPED])],
  [VICARE_CONSTANT_FALSES([AI_ADDRCONFIG AI_ALL AI_CANONNAME
     AI_NUMERIC_HOST AI_NUMERIC_SERV AI_PASSIVE AI_V4MAPPED])])
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([AI_IDN AI_CANONIDN AI_IDN_ALLOW_UNASSIGNED
     AI_IDN_USE_STD3_ASCII_RULES])],
  [VICARE_CONSTANT_FALSES([AI_IDN AI_CANONIDN AI_IDN_ALLOW_UNASSIGNED
     AI_IDN_USE_STD3_ASCII_RULES])])

# getaddrinfo return values
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([EAI_ADDRFAMILY EAI_AGAIN EAI_BADFLAGS
     EAI_FAIL EAI_FAMILY EAI_MEMORY EAI_NODATA EAI_NONAME
     EAI_SERVICE EAI_SOCKTYPE EAI_SYSTEM])],
  [VICARE_CONSTANT_FALSES([EAI_ADDRFAMILY EAI_AGAIN EAI_BADFLAGS
     EAI_FAIL EAI_FAMILY EAI_MEMORY EAI_NODATA EAI_NONAME
     EAI_SERVICE EAI_SOCKTYPE EAI_SYSTEM])])

AC_CACHE_SAVE

# mathematics
VICARE_DOUBLEOF_TESTS([M_E M_LOG2E M_LOG10E M_LN2 M_LN10 M_PI
  M_PI_2 M_PI_4 M_1_PI M_2_PI M_2_SQRTPI M_SQRT2 M_SQRT1_2])

# fnmatch()
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([FNM_NOMATCH FNM_FILE_NAME FNM_PATHNAME
     FNM_PERIOD FNM_NOESCAPE FNM_LEADING_DIR FNM_CASEFOLD
     FNM_EXTMATCH])],
  [VICARE_CONSTANT_FALSES([FNM_NOMATCH FNM_FILE_NAME FNM_PATHNAME
     FNM_PERIOD FNM_NOESCAPE FNM_LEADING_DIR FNM_CASEFOLD
     FNM_EXTMATCH])])

# glob() flags
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([GLOB_ERR GLOB_MARK GLOB_NOCHECK GLOB_NOSORT
     GLOB_NOESCAPE GLOB_PERIOD GLOB_MAGCHAR GLOB_BRACE GLOB_NOMAGIC
     GLOB_TILDE GLOB_TILDE_CHECK GLOB_ONLYDIR GLOB_ABORTED GLOB_NOMATCH
     GLOB_NOSPACE])],
  [VICARE_CONSTANT_FALSES([GLOB_ERR GLOB_MARK GLOB_NOCHECK GLOB_NOSORT
     GLOB_NOESCAPE GLOB_PERIOD GLOB_MAGCHAR GLOB_BRACE GLOB_NOMAGIC
     GLOB_TILDE GLOB_TILDE_CHECK GLOB_ONLYDIR GLOB_ABORTED GLOB_NOMATCH
     GLOB_NOSPACE])])

# POSIX regexps
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([REG_BADBR REG_BADPAT REG_BADRPT REG_ECOLLATE
     REG_ECTYPE REG_EESCAPE REG_ESUBREG REG_EBRACK REG_EPAREN REG_EBRACE
     REG_ERANGE REG_ESPACE REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE
     REG_NOTBOL REG_NOTEOL REG_NOMATCH REG_ESPACE])],
  [VICARE_CONSTANT_FALSES([REG_BADBR REG_BADPAT REG_BADRPT REG_ECOLLATE
     REG_ECTYPE REG_EESCAPE REG_ESUBREG REG_EBRACK REG_EPAREN REG_EBRACE
     REG_ERANGE REG_ESPACE REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE
     REG_NOTBOL REG_NOTEOL REG_NOMATCH REG_ESPACE])])

# word expansion
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([WRDE_NOCMD WRDE_SHOWERR WRDE_UNDEF WRDE_BADCHAR
     WRDE_BADVAL WRDE_CMDSUB WRDE_NOSPACE WRDE_SYNTAX])],
  [VICARE_CONSTANT_FALSES([WRDE_NOCMD WRDE_SHOWERR WRDE_UNDEF WRDE_BADCHAR
     WRDE_BADVAL WRDE_CMDSUB WRDE_NOSPACE WRDE_SYNTAX])])

# POSIX timers
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([CLOCK_REALTIME CLOCK_MONOTONIC CLOCK_MONOTONIC_RAW
     CLOCK_PROCESS_CPUTIME_ID CLOCK_THREAD_CPUTIME_ID TIMER_ABSTIME])],
  [VICARE_CONSTANT_FALSES([CLOCK_REALTIME CLOCK_MONOTONIC CLOCK_MONOTONIC_RAW
     CLOCK_PROCESS_CPUTIME_ID CLOCK_THREAD_CPUTIME_ID TIMER_ABSTIME])])

# signalfd
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([SFD_CLOEXEC SFD_NONBLOCK])],
  [VICARE_CONSTANT_FALSES([SFD_CLOEXEC SFD_NONBLOCK])])

# timerfd
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([TFD_CLOEXEC TFD_NONBLOCK TFD_TIMER_ABSTIME])],
  [VICARE_CONSTANT_FALSES([TFD_CLOEXEC TFD_NONBLOCK TFD_TIMER_ABSTIME])])

# inotify
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([IN_ACCESS IN_ATTRIB IN_CLOSE_WRITE IN_CLOSE_NOWRITE
     IN_CREATE IN_DELETE IN_DELETE_SELF IN_MODIFY IN_MOVE_SELF IN_MOVED_FROM
     IN_MOVED_TO IN_OPEN IN_NONBLOCK IN_CLOEXEC IN_ALL_EVENTS IN_MOVE IN_CLOSE])],
  [VICARE_CONSTANT_FALSES([IN_ACCESS IN_ATTRIB IN_CLOSE_WRITE IN_CLOSE_NOWRITE
     IN_CREATE IN_DELETE IN_DELETE_SELF IN_MODIFY IN_MOVE_SELF IN_MOVED_FROM
     IN_MOVED_TO IN_OPEN IN_NONBLOCK IN_CLOEXEC IN_ALL_EVENTS IN_MOVE IN_CLOSE])])
AM_COND_IF([WANT_LINUX],
  [VICARE_CONSTANT_TESTS([IN_DONT_FOLLOW IN_EXCL_UNLINK IN_MASK_ADD IN_ONESHOT
     IN_ONLYDIR IN_IGNORED IN_ISDIR IN_Q_OVERFLOW IN_UNMOUNT])],
  [VICARE_CONSTANT_FALSES([IN_DONT_FOLLOW IN_EXCL_UNLINK IN_MASK_ADD IN_ONESHOT
     IN_ONLYDIR IN_IGNORED IN_ISDIR IN_Q_OVERFLOW IN_UNMOUNT])])

dnl --------------------------------------------------------------------

# Struct "siginfo_t", values for field `si_code'.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SI_ASYNCNL SI_TKILL SI_SIGIO SI_ASYNCIO SI_MESGQ
     SI_TIMER SI_QUEUE SI_USER SI_KERNEL])],
  [VICARE_CONSTANT_FALSES([SI_ASYNCNL SI_TKILL SI_SIGIO SI_ASYNCIO SI_MESGQ
     SI_TIMER SI_QUEUE SI_USER SI_KERNEL])])

# Struct "siginfo_t", `si_code' values for SIGILL signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([ILL_ILLOPC ILL_ILLOPN ILL_ILLADR ILL_ILLTRP
     ILL_PRVOPC ILL_PRVREG ILL_COPROC ILL_BADSTK])],
  [VICARE_CONSTANT_FALSES([ILL_ILLOPC ILL_ILLOPN ILL_ILLADR ILL_ILLTRP
     ILL_PRVOPC ILL_PRVREG ILL_COPROC ILL_BADSTK])])

# Struct "siginfo_t", `si_code' values for SIGFPE signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([FPE_INTDIV FPE_INTOVF FPE_FLTDIV FPE_FLTOVF
     FPE_FLTUND FPE_FLTRES FPE_FLTINV FPE_FLTSUB])],
  [VICARE_CONSTANT_FALSES([FPE_INTDIV FPE_INTOVF FPE_FLTDIV FPE_FLTOVF
     FPE_FLTUND FPE_FLTRES FPE_FLTINV FPE_FLTSUB])])

# Struct "siginfo_t", `si_code' values for SIGSEGV signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SEGV_MAPERR SEGV_ACCERR])],
  [VICARE_CONSTANT_FALSES([SEGV_MAPERR SEGV_ACCERR])])

# Struct "siginfo_t", `si_code' values for SIGBUS signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([BUS_ADRALN BUS_ADRERR BUS_OBJERR])],
  [VICARE_CONSTANT_FALSES([BUS_ADRALN BUS_ADRERR BUS_OBJERR])])

# Struct "siginfo_t", `si_code' values for SIGTRAP signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([TRAP_BRKPT TRAP_TRACE])],
  [VICARE_CONSTANT_FALSES([TRAP_BRKPT TRAP_TRACE])])

# Struct "siginfo_t", `si_code' values for SIGCHLD signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([CLD_EXITED CLD_KILLED CLD_DUMPED
     CLD_TRAPPED CLD_STOPPED CLD_CONTINUED])],
  [VICARE_CONSTANT_FALSES([CLD_EXITED CLD_KILLED CLD_DUMPED
     CLD_TRAPPED CLD_STOPPED CLD_CONTINUED])])

# Struct "siginfo_t", `si_code' values for SIGPOLL signal.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([POLL_IN POLL_OUT POLL_MSG POLL_ERR POLL_PRI POLL_HUP])],
  [VICARE_CONSTANT_FALSES([POLL_IN POLL_OUT POLL_MSG POLL_ERR POLL_PRI POLL_HUP])])

# Struct "sigevent_t", `sigev_notify' values.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([SIGEV_SIGNAL SIGEV_NONE SIGEV_THREAD SIGEV_THREAD_ID])],
  [VICARE_CONSTANT_FALSES([SIGEV_SIGNAL SIGEV_NONE SIGEV_THREAD SIGEV_THREAD_ID])])

# For "setrlimit()" and similar.
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([
     RLIMIT_AS RLIMIT_CORE RLIMIT_CPU RLIMIT_DATA RLIMIT_FSIZE RLIMIT_LOCKS
     RLIMIT_MEMLOCK RLIMIT_MSGQUEUE RLIMIT_NICE RLIMIT_NOFILE RLIMIT_NPROC
     RLIMIT_RSS RLIMIT_RTPRIO RLIMIT_RTTIME RLIMIT_SIGPENDING RLIMIT_STACK])],
  [VICARE_CONSTANT_FALSES([
     RLIMIT_AS RLIMIT_CORE RLIMIT_CPU RLIMIT_DATA RLIMIT_FSIZE RLIMIT_LOCKS
     RLIMIT_MEMLOCK RLIMIT_MSGQUEUE RLIMIT_NICE RLIMIT_NOFILE RLIMIT_NPROC
     RLIMIT_RSS RLIMIT_RTPRIO RLIMIT_RTTIME RLIMIT_SIGPENDING RLIMIT_STACK])])

# For "getrusage()".
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([RUSAGE_SELF RUSAGE_CHILDREN RUSAGE_THREAD])],
  [VICARE_CONSTANT_FALSES([RUSAGE_SELF RUSAGE_CHILDREN RUSAGE_THREAD])])

# For "lockf()".
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([F_LOCK F_TLOCK F_ULOCK F_TEST])],
  [VICARE_CONSTANT_FALSES([F_LOCK F_TLOCK F_ULOCK F_TEST])])

dnl --------------------------------------------------------------------

# Constants from "/usr/include/netinet/in.h"
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([IPPROTO_IP IPPROTO_HOPOPTS IPPROTO_ICMP IPPROTO_IGMP IPPROTO_IPIP IPPROTO_TCP IPPROTO_EGP IPPROTO_PUP IPPROTO_UDP IPPROTO_IDP IPPROTO_TP IPPROTO_DCCP IPPROTO_IPV6 IPPROTO_ROUTING IPPROTO_FRAGMENT IPPROTO_RSVP IPPROTO_GRE IPPROTO_ESP IPPROTO_AH IPPROTO_ICMPV6 IPPROTO_NONE IPPROTO_DSTOPTS IPPROTO_MTP IPPROTO_ENCAP IPPROTO_PIM IPPROTO_COMP IPPROTO_SCTP IPPROTO_UDPLITE IPPROTO_RAW])],
  [VICARE_CONSTANT_FALSES([IPPROTO_IP IPPROTO_HOPOPTS IPPROTO_ICMP IPPROTO_IGMP IPPROTO_IPIP IPPROTO_TCP IPPROTO_EGP IPPROTO_PUP IPPROTO_UDP IPPROTO_IDP IPPROTO_TP IPPROTO_DCCP IPPROTO_IPV6 IPPROTO_ROUTING IPPROTO_FRAGMENT IPPROTO_RSVP IPPROTO_GRE IPPROTO_ESP IPPROTO_AH IPPROTO_ICMPV6 IPPROTO_NONE IPPROTO_DSTOPTS IPPROTO_MTP IPPROTO_ENCAP IPPROTO_PIM IPPROTO_COMP IPPROTO_SCTP IPPROTO_UDPLITE IPPROTO_RAW])])

# Constants from "/usr/include/netinet/in.h"
AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([IPPORT_ECHO IPPORT_DISCARD IPPORT_SYSTAT IPPORT_DAYTIME IPPORT_NETSTAT IPPORT_FTP IPPORT_TELNET IPPORT_SMTP IPPORT_TIMESERVER IPPORT_NAMESERVER IPPORT_WHOIS IPPORT_MTP IPPORT_TFTP IPPORT_RJE IPPORT_FINGER IPPORT_TTYLINK IPPORT_SUPDUP IPPORT_EXECSERVER IPPORT_LOGINSERVER IPPORT_CMDSERVER IPPORT_EFSSERVER IPPORT_BIFFUDP IPPORT_WHOSERVER IPPORT_ROUTESERVER IPPORT_RESERVED IPPORT_USERRESERVED])],
  [VICARE_CONSTANT_FALSES([IPPORT_ECHO IPPORT_DISCARD IPPORT_SYSTAT IPPORT_DAYTIME IPPORT_NETSTAT IPPORT_FTP IPPORT_TELNET IPPORT_SMTP IPPORT_TIMESERVER IPPORT_NAMESERVER IPPORT_WHOIS IPPORT_MTP IPPORT_TFTP IPPORT_RJE IPPORT_FINGER IPPORT_TTYLINK IPPORT_SUPDUP IPPORT_EXECSERVER IPPORT_LOGINSERVER IPPORT_CMDSERVER IPPORT_EFSSERVER IPPORT_BIFFUDP IPPORT_WHOSERVER IPPORT_ROUTESERVER IPPORT_RESERVED IPPORT_USERRESERVED])])

AM_COND_IF([WANT_POSIX],
  [VICARE_CONSTANT_TESTS([IN_CLASSA_NET IN_CLASSA_NSHIFT IN_CLASSA_HOST IN_CLASSA_MAX IN_CLASSB_NET IN_CLASSB_NSHIFT IN_CLASSB_HOST IN_CLASSB_MAX IN_CLASSC_NET IN_CLASSC_NSHIFT IN_CLASSC_HOST])],
  [VICARE_CONSTANT_FALSES([IN_CLASSA_NET IN_CLASSA_NSHIFT IN_CLASSA_HOST IN_CLASSA_MAX IN_CLASSB_NET IN_CLASSB_NSHIFT IN_CLASSB_HOST IN_CLASSB_MAX IN_CLASSC_NET IN_CLASSC_NSHIFT IN_CLASSC_HOST])])

AC_CACHE_SAVE

#page
#### tests for GNU C Library specific features

AM_COND_IF([WANT_GLIBC],[
  AC_CHECK_FUNCS([clearenv dirfd mkstemp mkdtemp sync fsync fdatasync])
  AC_CHECK_FUNCS([if_nametoindex if_indextoname if_nameindex])
  AC_CHECK_FUNCS([creal cimag])
  AC_CHECK_FUNCS([csin ccos ctan casin cacos catan])
  AC_CHECK_FUNCS([cexp clog clog10 csqrt cpow])
  AC_CHECK_FUNCS([sinh cosh tanh csinh ccosh ctanh asinh acosh atanh casinh cacosh catanh])
  AC_CHECK_FUNCS([erf erfc lgamma_r tgamma j0 j1 jn y0 y1 yn])
  AC_CHECK_FUNCS([rand srand])
  AC_CHECK_FUNCS([fnmatch glob regcomp regexec regfree regerror])
  AC_CHECK_FUNCS([wordexp wordfree])

  VICARE_CHECK_NETINET_IN_CLASS_MACROS([IN_CLASSA IN_CLASSB IN_CLASSC IN_CLASSD IN_MULTICAST IN_EXPERIMENTAL IN_BADCLASS])
  VICARE_CHECK_NETINET_IN6_ADDR_MACROS([IN6_IS_ADDR_UNSPECIFIED IN6_IS_ADDR_LOOPBACK IN6_IS_ADDR_LINKLOCAL IN6_IS_ADDR_SITELOCAL IN6_IS_ADDR_V4MAPPED IN6_IS_ADDR_V4COMPAT IN6_IS_ADDR_MULTICAST IN6_IS_ADDR_MC_NODELOCAL IN6_IS_ADDR_MC_LINKLOCAL IN6_IS_ADDR_MC_SITELOCAL IN6_IS_ADDR_MC_ORGLOCAL IN6_IS_ADDR_MC_GLOBAL])
  VICARE_CHECK_NETINET_IN6_ADDR_MACRO_TWO([IN6_ARE_ADDR_EQUAL])

  ])

AC_CACHE_SAVE

#page
#### tests for GNU+Linux specific features

AM_COND_IF([WANT_LINUX],[
  VICARE_CHECK_WMACRO([WIFCONTINUED])

  AC_CACHE_CHECK([availability of waitid],
    [vicare_cv_HAVE_WAITID],
    [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
int main (void)
{
  idtype_t  idtype  = P_PID;
  id_t      id      = 0;
  siginfo_t info;
  int       options = 0;
  int retval = waitid(idtype, id, &info, options);
  return 0;
}])],
      [vicare_cv_HAVE_WAITID=1],
      [vicare_cv_HAVE_WAITID=0])])
  AC_DEFINE_UNQUOTED([HAVE_WAITID],
    $vicare_cv_HAVE_WAITID,
    [whether the Linux specific waitid function is available])

  AC_CHECK_TYPES([struct epoll_event],,,[VICARE_INCLUDES])
  AC_CHECK_FUNCS([epoll_create epoll_create1 epoll_ctl epoll_wait])
  AC_CHECK_FUNCS([signalfd])
  AC_CHECK_FUNCS([timerfd_create timerfd_settime timerfd_gettime])
  AC_CHECK_FUNCS([prlimit])
  AC_CHECK_FUNCS([inotify_init inotify_init1 inotify_add_watch inotify_rm_watch])
  AC_CHECK_FUNCS([daemon])
])

dnl --------------------------------------------------------------------

AC_CACHE_SAVE

#page
#### external libraries

dnl AC_CHECK_LIB(m,sin)

AC_CHECK_HEADERS([gmp.h],,[AC_MSG_ERROR([
ERROR: Cannot find libgmp.
ERROR: You might want to supply CFLAGS=-I</path/to/include> (containing
ERROR: the gmp.h header file), and LDFLAGS=-L</path/to/lib> (containing
ERROR: libgmp.so) if libgmp is installed in a non-standard location.
ERROR: libgmp can be obtained from <http://gmplib.org>.])])

# check that gmp is present
AC_CHECK_LIB([gmp],[__gmpn_add],[],[AC_MSG_ERROR([
ERROR: Cannot find libgmp.
ERROR: You might want to supply CFLAGS=-I</path/to/include> (containing
ERROR: the gmp.h header file), and LDFLAGS=-L</path/to/lib> (containing
ERROR: libgmp.so) if libgmp is installed in a non-standard location.
ERROR: libgmp can be obtained from <http://gmplib.org>.])])

dnl --------------------------------------------------------------------
dnl Optional binding to Libffi

VICARE_WITH_LIBRARY([LIBFFI],[libffi],[check],
  [whether to link with Libffi],
  [link executable with Libffi],
  [ffi.h],[ffi],[ffi_call])
AM_CONDITIONAL([WANT_LIBFFI],[test "x$vicare_with_LIBFFI" = xyes])

dnl --------------------------------------------------------------------
dnl Check for Libiconv (optional)

VICARE_WITH_LIBRARY([LIBICONV],[libiconv],[check],
  [whether to link with Libiconv],
  [link executable with Libiconv],
  [iconv.h],[iconv],[iconv_open])
AM_CONDITIONAL([WANT_LIBICONV],[test "x$vicare_with_LIBICONV" = xyes])

AM_COND_IF([WANT_LIBICONV],
  [AC_CHECK_FUNCS([iconv_open iconv_close iconv])])

dnl --------------------------------------------------------------------
dnl Optional binding for GNU Readline or compatible library

AC_MSG_CHECKING([whether to link with GNU Readline or compatible library])
AC_ARG_WITH([readline],
  [AS_HELP_STRING([--with-readline],
     [enable support for GNU readline or compatible (default is check)])],
  [AS_CASE([$withval],
     [yes],  [with_READLINE=yes],
     [no],   [with_READLINE=no],
     [check],[with_READLINE=check],
     [AC_MSG_ERROR([bad value $withval for --with-readline])])],
  [with_READLINE=check])
AC_MSG_RESULT([$with_READLINE])

vicare_with_READLINE=no
AS_IF([test "x$with_READLINE" != "xno"],
  [AX_LIB_READLINE
   AS_IF([test "x$ax_cv_lib_readline" != "xno"],
     [vicare_with_READLINE=yes],
     [AS_IF([test "x$with_READLINE" = xyes],
        [# The user requested support readline or die, but
         # readline was not found.
         AC_MSG_ERROR([GNU Readline or compatible not found])],
        [# The user  requested optional support for readline,
         # but readline was not found.
         AC_MSG_WARN([GNU Readline or compatible not found])])])],
   [AC_MSG_NOTICE([Readline support was not requested])])
AM_CONDITIONAL([WANT_READLINE],[test "x$vicare_with_READLINE" = xyes])

dnl --------------------------------------------------------------------
dnl Check for CRE2 (optional)

with_CRE2=no
AC_MSG_CHECKING([whether to link with CRE2])
AC_ARG_WITH([cre2],
  [AS_HELP_STRING([--with-cre2],[link executable with CRE2 (default is no)])],
  [AS_CASE([$withval],
     [yes],  [with_CRE2=yes],
     [no],   [with_CRE2=no],
     [check],[with_CRE2=check],
     [AC_MSG_ERROR([bad value $withval for --with-cre2])])],
  [with_CRE2=no])
AC_MSG_RESULT([$with_CRE2])

# We must check for both CRE2 and re2!!!

vicare_with_CRE2=no
CRE2_LIBS=
AS_IF([test "x$with_CRE2" != xno],
  [# Check for header file and shared (or static) library.
   # No need to check for "re2.h" header file.
   AC_CHECK_LIB([re2],[main],
     [have_re2=yes],
     [AC_MSG_WARN([test for RE2 library failed])])
   AC_CHECK_LIB([cre2],[cre2_version_string],
     [vicare_have_cre2=yes],[vicare_have_cre2=no],[-lre2])
   AC_CHECK_HEADERS([cre2.h],[vicare_have_cre2_h=yes],[vicare_have_cre2_h=no])
   AS_IF([test "x$vicare_have_cre2" = xyes && test "x$vicare_have_cre2_h" = xyes],
     [# Both the library and the header were found: success!
      AC_MSG_NOTICE([cre2 support enabled])
      AC_DEFINE([HAVE_CRE2],[1],[define if you have a cre2 library])
      vicare_with_CRE2=yes
      CRE2_LIBS="-lcre2 -lre2"],

     [test "x$with_CRE2" = xyes],
     [# The user requested support for library or die, but either
      # the header or the library or both were not found.
      AS_IF([test "x$vicare_have_cre2_h" = xno],
            [AC_MSG_ERROR([cre2.h cannot be found.])],
            [test "x$vicare_have_cre2" = xno],
            [AC_MSG_ERROR([cre2 cannot be found])])],

     [# The user requested a check for optional cre2 support, but
      # either the header or the library or both were not found.
      AC_MSG_WARN([cre2 not found])])],
  [AC_MSG_NOTICE([cre2 was not requested])])
AM_CONDITIONAL([WANT_CRE2],[test "x$vicare_with_CRE2" = xyes])

AM_COND_IF([WANT_CRE2],[cre2_val=1],[cre2_val=0])
AC_DEFINE_UNQUOTED([ENABLE_CRE2],[$cre2_val],[adds support for CRE2])
AC_SUBST([CRE2_LIBS])

AC_CACHE_SAVE

#page
#### finish

AC_SUBST([VFLAGS])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES(Makefile
                src/Makefile scheme/Makefile doc/Makefile lib/Makefile
                tests/Makefile demos/Makefile
                src/vicare-platform.h
                lib/vicare/platform/configuration.sls
                lib/vicare/platform/constants.sls
                lib/vicare/platform/errno.sls
                lib/vicare/platform/words.sls
                meta/slackware/doinst.sh)
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
