# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(vicare, 0.2d2, marco.maggi-ipsu@poste.it)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(vicare, 0.2d2, gnu)
AC_CONFIG_SRCDIR([src/])

AC_LANG([C])

#page
#### additional configuration command line options

vicare_ARGUMENTS_VALIDATION=1
AC_MSG_CHECKING([whether arguments validation is enabled])
AC_ARG_ENABLE([arguments-validation],
  [AS_HELP_STRING([--disable-arguments-validation],
     [disable arguments validation in the boot image (default is enable)])],
  [AC_MSG_RESULT([$enableval])
  if test "z$enableval" = zno
  then vicare_ARGUMENTS_VALIDATION=0
  else vicare_ARGUMENTS_VALIDATION=1
  fi],
  [vicare_ARGUMENTS_VALIDATION=1
   AC_MSG_RESULT([yes])])

AC_SUBST([VICARE_ARGUMENTS_VALIDATION],[$vicare_ARGUMENTS_VALIDATION])

#page
#### basic system inspection

AM_PROG_AS
# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O

CFLAGS="$CFLAGS -DNDEBUG -O3 -Wall"

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
    AC_DEFINE(HACK_FFI, 1,
              [Mark code returned by libffi executable because
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
AC_CHECK_HEADERS([assert.h dlfcn.h errno.h fcntl.h signal.h \
                  stdint.h stdlib.h string.h strings.h \
                  sys/mman.h sys/resource.h sys/stat.h \
                  sys/time.h sys/types.h sys/wait.h time.h \
                  unistd.h])

AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM

AC_CACHE_SAVE

#page
#### other system inspection

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

AC_CHECK_MEMBER([struct stat.st_atime_usec],
   AC_DEFINE(HAVE_STAT_ST_ATIME_USEC, 1,
             [stat struct has st_atime_usec field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_mtime_usec],
   AC_DEFINE(HAVE_STAT_ST_MTIME_USEC, 1,
             [stat struct has st_mtime_usec field]),
   [],
   [[#include <sys/stat.h>]])

AC_CHECK_MEMBER([struct stat.st_ctime_usec],
   AC_DEFINE(HAVE_STAT_ST_CTIME_USEC, 1,
             [stat struct has st_ctime_usec field]),
   [],
   [[#include <sys/stat.h>]])

#page
#### checks for library functions

AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_CHECK_FUNCS([sigaltstack bzero gettimeofday memmove memset munmap setenv sqrt strerror getaddrinfo])

AC_CACHE_SAVE

#page
#### external libraries

AC_CHECK_LIB(pthread, main)

AC_CHECK_HEADERS([gmp.h],,[AC_MSG_ERROR([
ERROR: Cannot find libgmp.
ERROR: You might want to supply CFLAGS=-I</path/to/include> (containing
ERROR: the gmp.h header file), and LDFLAGS=-L</path/to/lib> (containing
ERROR: libgmp.so) if libgmp is installed in a non-standard location.
ERROR: libgmp can be obtained from <http://gmplib.org>. ])])

# check that gmp is present
AC_HAVE_LIBRARY([gmp],,[AC_MSG_ERROR([
ERROR: Cannot find libgmp.
ERROR: You might want to supply CFLAGS=-I</path/to/include> (containing
ERROR: the gmp.h header file), and LDFLAGS=-L</path/to/lib> (containing
ERROR: libgmp.so) if libgmp is installed in a non-standard location.
ERROR: libgmp can be obtained from <http://gmplib.org>. ])])

AC_ARG_ENABLE(libffi,
  AS_HELP_STRING([--enable-libffi], [enable support for libffi (default is check)]),
  [true], [enable_libffi=check])

# Check for libffi (optional)

libffi_val=0
if test "$enable_libffi" != no; then
  AC_CHECK_HEADER([ffi.h], [have_ffi_h=yes], [have_ffi_h=no])
  AC_CHECK_LIB(ffi,ffi_call, [have_libffi=yes], [have_libffi=no])
  if test "$have_libffi" = yes && test "$have_ffi_h" = yes; then
    libffi_val=1
    LIBS="$LIBS -lffi"
  elif test "$enable_libffi" = yes; then
    if test "$have_ffi_h" = no; then
      AC_MSG_ERROR([ffi.h cannot be found.
Please specify the location of the header file using
  ./configure CPPFLAGS=-I<path/to/ffi.h>   <other-options ...>
])
    fi
    if test "$have_libffi" = no; then
      AC_MSG_ERROR([libffi cannot be found.
Please specify the location of the library file using
  ./configure LDFLAGS=-L<path/to/libffi.ld|dylib|so|etc.>  <other-options ...>
])
    fi
  else
    AC_MSG_WARN([libffi not found -- FFI disabled])
  fi
fi # "$enable_ffi" != no

AC_DEFINE_UNQUOTED(ENABLE_LIBFFI, [$libffi_val], [adds support for libffi])

AC_CACHE_SAVE

#page
#### custom macros

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_ERRNO_H
#  include <errno.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
])

AC_DEFUN([VICARE_VALUEOF_TEST],[
  VALUEOF_$1="#f"
  AC_CACHE_CHECK([the value of '$2'],
    [vicare_cv_valueof_$1],
    [AC_COMPUTE_INT([vicare_cv_valueof_$1],
       [$2],
       [VICARE_INCLUDES],
       [vicare_cv_valueof_$1="#f"])])
   VALUEOF_$1="$vicare_cv_valueof_$1"
   AC_SUBST([VALUEOF_$1])])

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

AC_DEFUN([VICARE_CONSTANT_TEST],[VICARE_VALUEOF_TEST([$1],[$1])])

## constants for waitpid and similar
VICARE_CONSTANT_TEST([WAIT_ANY])
VICARE_CONSTANT_TEST([WAIT_MYPGRP])
VICARE_CONSTANT_TEST([WCONTINUED])
VICARE_CONSTANT_TEST([WEXITED])
VICARE_CONSTANT_TEST([WNOHANG])
VICARE_CONSTANT_TEST([WNOWAIT])
VICARE_CONSTANT_TEST([WSTOPPED])
VICARE_CONSTANT_TEST([WUNTRACED])
VICARE_CONSTANT_TEST([P_PID])
VICARE_CONSTANT_TEST([P_PGID])
VICARE_CONSTANT_TEST([P_ALL])
VICARE_CONSTANT_TEST([CLD_CONTINUED])
VICARE_CONSTANT_TEST([CLD_DUMPED])
VICARE_CONSTANT_TEST([CLD_EXITED])
VICARE_CONSTANT_TEST([CLD_KILLED])
VICARE_CONSTANT_TEST([CLD_STOPPED])
VICARE_CONSTANT_TEST([CLD_TRAPPED])

dnl These are not fixnums.
dnl
dnl VICARE_CONSTANT_TEST([__WCLONE])
dnl VICARE_CONSTANT_TEST([__WALL])
dnl VICARE_CONSTANT_TEST([__WNOTHREAD])

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

#page
#### tests for GNU C Library specific features

AC_CHECK_FUNCS([clearenv])

#page
#### tests for GNU+Linux specific features

AC_CACHE_CHECK([availability of WIFCONTINUED],
  [vicare_cv_HAVE_WIFCONTINUED],
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
int main (void)
{
  int status = 0;
  int retval = WIFCONTINUED(status);
  return 0;
}])],
    [vicare_cv_HAVE_WIFCONTINUED=1],
    [vicare_cv_HAVE_WIFCONTINUED=0])])
AC_DEFINE_UNQUOTED([HAVE_WIFCONTINUED],
  $vicare_cv_HAVE_WIFCONTINUED,
  [whether the Linux specific WIFCONTINUED macro is available])

dnl --------------------------------------------------------------------

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

dnl --------------------------------------------------------------------

AC_CACHE_SAVE

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES(Makefile src/Makefile scheme/Makefile doc/Makefile lib/Makefile tests/Makefile lib/vicare/installation-configuration.sls lib/vicare/platform-constants.sls)
AC_OUTPUT #(Makefile src/Makefile scheme/Makefile doc/Makefile)

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
