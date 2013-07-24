### custom macros

m4_include([m4/ax_lib_readline.m4])

dnl $1 - upper case option name
dnl $2 - command line option name "--enable-$2"
dnl $3 - default (yes, no)
dnl $4 - text for the "checking option... " message
dnl $5 - text for the "enable option... " message
AC_DEFUN([VICARE_ENABLE_OPTION],
  [vicare_enable_$1=$3
   AC_MSG_CHECKING([$4])
   AC_ARG_ENABLE([$2],
     [AS_HELP_STRING([--enable-$2],
        [$5 (default is $3)])],
     [AS_CASE([$enableval],
        [yes],[vicare_enable_$1=yes],
        [no], [vicare_enable_$1=no],
        [AC_MSG_ERROR([bad value $enableval for --enable-$2])])],
     [vicare_enable_$1=$3])
   AC_MSG_RESULT([$vicare_enable_$1])])

dnl $1 - upper case option name
dnl $2 - command line option name "--with-$2"
dnl $3 - default (yes, no, check)
dnl $4 - text for the "checking option... " message
dnl $5 - text for the "enable option... " message
dnl $6 - header file
dnl $7 - library identifier (as in the linker option -l$7)
dnl $8 - function to search in the library (use "main" when unsure)
AC_DEFUN([VICARE_WITH_LIBRARY],
  [with_$1=$3
   AC_MSG_CHECKING([$4])
   AC_ARG_WITH([$2],
     [AS_HELP_STRING([--with-$2],[$5 (default is $3)])],
     [AS_CASE([$withval],
        [yes],  [with_$1=yes],
        [no],   [with_$1=no],
        [check],[with_$1=check],
        [AC_MSG_ERROR([bad value $withval for --with-$2])])],
     [with_$1=$3])
     AC_MSG_RESULT([$with_$1])
dnl
   vicare_with_$1=no
   AS_IF([test "x$with_$2" != xno],
     [# Check for header file and shared (or static) library.
      AC_CHECK_HEADER([$6],[vicare_have_$2_h=yes],[vicare_have_$2_h=no])
      AC_CHECK_LIB([$7],[$8],[vicare_have_$2=yes],[vicare_have_$2=no])
      AS_IF([test "$vicare_have_$2" = yes && test "$vicare_have_$2_h" = yes],
        [# Both the library and the header were found: success!
         AC_MSG_NOTICE([$2 support enabled])
         AC_DEFINE([HAVE_$1],[1],[define if you have a $2 library])
         vicare_with_$1=yes
         LIBS="$LIBS -l$7"],

        [test "$with_$2" = yes],
        [# The user requested support for library or die, but either
         # the header or the library or both were not found.
         AS_IF([test "$vicare_have_$2_h" = no],
               [AC_MSG_ERROR([$2.h cannot be found.])],
               [test "$vicare_have_$2" = no],
               [AC_MSG_ERROR([$2 cannot be found])])],

        [# The user requested a check for optional $2 support, but
         # either the header or the library or both were not found.
         AC_MSG_WARN([$2 not found])])],
     [AC_MSG_NOTICE([$2 was not requested])])])

dnl page
m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_ERRNO_H
#  include <errno.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_FNMATCH_H
#  include <fnmatch.h>
#endif
#ifdef HAVE_GLOB_H
#  include <glob.h>
#endif
#ifdef HAVE_SYS_INOTIFY_H
#  include <sys/inotify.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_MATH_H
#  include <math.h>
#endif
#ifdef HAVE_REGEX_H
#  include <regex.h>
#endif
#ifdef HAVE_SEMAPHORE_H
#  include <semaphore.h>
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
#ifdef HAVE_SYS_SIGNALFD_H
#  include <sys/signalfd.h>
#endif
#ifdef HAVE_SYS_TIMERFD_H
#  include <sys/timerfd.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#ifdef HAVE_ARPA_INET_H
#  include <arpa/inet.h>
#endif
#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_FTW_H
#  include <ftw.h>
#endif
#ifdef HAVE_FSTAB_H
#  include <fstab.h>
#endif
#ifdef HAVE_GRP_H
#  include <grp.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif
#ifdef HAVE_LIMITS_H
#  include <limits.h>
#endif
#ifdef HAVE_MNTENT_H
#  include <mntent.h>
#endif
#ifdef HAVE_MQUEUE_H
#  include <mqueue.h>
#endif
#ifdef HAVE_NETDB_H
#  include <netdb.h>
#endif
#ifdef HAVE_NET_IF_H
#  include <net/if.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#  include <netinet/tcp.h>
#endif
#ifdef HAVE_PATHS_H
#  include <paths.h>
#endif
#ifdef HAVE_POLL_H
#  include <poll.h>
#endif
#ifdef HAVE_PWD_H
#  include <pwd.h>
#endif
#ifdef HAVE_SIGNAL_H
#  include <signal.h>
#endif
#ifdef HAVE_TIME_H
#  include <time.h>
#endif
#ifdef HAVE_UTIME_H
#  include <utime.h>
#endif
#ifdef HAVE_SYS_EPOLL_H
#  include <sys/epoll.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#  include <sys/mount.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
#endif
#ifdef HAVE_SYS_TIMEX_H
#  include <sys/timex.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#  include <sys/utsname.h>
#endif
#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_WORDEXP_H
#  include <wordexp.h>
#endif
])

dnl page
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

AC_DEFUN([VICARE_CONSTANT_TEST],[VICARE_VALUEOF_TEST([$1],[$1])])
AC_DEFUN([VICARE_CONSTANT_TESTS],[m4_map_args_w($1,[VICARE_CONSTANT_TEST(],[)])])

AC_DEFUN([VICARE_CONSTANT_FALSE],
  [VALUEOF_$1="#f"
   AC_SUBST([VALUEOF_$1])])
AC_DEFUN([VICARE_CONSTANT_FALSES],[m4_map_args_w($1,[VICARE_CONSTANT_FALSE(],[)])])

dnl page
AC_DEFUN([VICARE_STRINGOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the string value of '$1'],
     [vicare_cv_stringof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([VICARE_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%s", $2);
           return ferror (f) || fclose (f) != 0;
        }])],
        [vicare_cv_stringof_$1=`cat conftest.val`],
        [vicare_cv_stringof_$1=""],
	[vicare_cv_stringof_$1=""])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_stringof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([VICARE_STRING_CONSTANT_TEST],[VICARE_STRINGOF_TEST([$1],[$1])])
AC_DEFUN([VICARE_STRING_CONSTANT_TESTS],[m4_map_args_w($1,[VICARE_STRING_CONSTANT_TEST(],[)])])

dnl page
AC_DEFUN([VICARE_DOUBLEOF_TEST],
  [VALUEOF_$1=""
   AC_CACHE_CHECK([the floating point value of '$1'],
     [vicare_cv_doubleof_$1],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([VICARE_INCLUDES
        int main (void)
        {
           FILE *f = fopen ("conftest.val", "w");
           fprintf(f, "%f", $1);
           return ferror (f) || fclose (f) != 0;
        }])],
        [vicare_cv_doubleof_$1=`cat conftest.val`],
        [vicare_cv_doubleof_$1=""],
	[vicare_cv_doubleof_$1="0.0"])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_doubleof_$1"
   AC_SUBST([VALUEOF_$1])])

AC_DEFUN([VICARE_DOUBLEOF_TESTS],[m4_map_args_w($1,[VICARE_DOUBLEOF_TEST(],[)])])

dnl page
AC_DEFUN([VICARE_CHECK_WMACRO],
  [AC_CACHE_CHECK([availability of $1],
     [vicare_cv_HAVE_$1],
     [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif
int main (void)
{
  int status = 0;
  int retval = $1(status);
  return 0;
}])],
     [vicare_cv_HAVE_$1=1],
     [vicare_cv_HAVE_$1=0])])
  AC_DEFINE_UNQUOTED([HAVE_$1],
    $vicare_cv_HAVE_$1,
    [whether the macro $1 is available])])

AC_DEFUN([VICARE_CHECK_WMACROS],[m4_map_args_w($1,[VICARE_CHECK_WMACRO(],[)])])

dnl --------------------------------------------------------------------

AC_DEFUN([VICARE_CHECK_NETINET_IN_CLASS_MACRO],
  [AC_CACHE_CHECK([availability of $1],
     [vicare_cv_HAVE_$1],
     [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
int main (void)
{
  in_addr_t addr = 0;
  int retval = $1(addr);
  return 0;
}])],
     [vicare_cv_HAVE_$1=1],
     [vicare_cv_HAVE_$1=0])])
  AC_DEFINE_UNQUOTED([HAVE_$1],
    $vicare_cv_HAVE_$1,
    [whether the macro $1 is available])])

AC_DEFUN([VICARE_CHECK_NETINET_IN_CLASS_MACROS],[m4_map_args_w($1,[VICARE_CHECK_NETINET_IN_CLASS_MACRO(],[)])])

dnl --------------------------------------------------------------------

AC_DEFUN([VICARE_CHECK_NETINET_IN6_ADDR_MACRO],
  [AC_CACHE_CHECK([availability of $1],
     [vicare_cv_HAVE_$1],
     [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
int main (void)
{
  struct in6_addr addr;
  int retval = $1(&addr);
  return 0;
}])],
     [vicare_cv_HAVE_$1=1],
     [vicare_cv_HAVE_$1=0])])
  AC_DEFINE_UNQUOTED([HAVE_$1],
    $vicare_cv_HAVE_$1,
    [whether the macro $1 is available])])

AC_DEFUN([VICARE_CHECK_NETINET_IN6_ADDR_MACROS],[m4_map_args_w($1,[VICARE_CHECK_NETINET_IN6_ADDR_MACRO(],[)])])

AC_DEFUN([VICARE_CHECK_NETINET_IN6_ADDR_MACRO_TWO],
  [AC_CACHE_CHECK([availability of $1],
     [vicare_cv_HAVE_$1],
     [AC_COMPILE_IFELSE([AC_LANG_SOURCE([AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
int main (void)
{
  struct in6_addr addr1, addr2;
  int retval = $1(&addr1, &addr2);
  return 0;
}])],
     [vicare_cv_HAVE_$1=1],
     [vicare_cv_HAVE_$1=0])])
  AC_DEFINE_UNQUOTED([HAVE_$1],
    $vicare_cv_HAVE_$1,
    [whether the macro $1 is available])])

dnl page
AC_DEFUN([VICARE_CHECK_PAGESIZE],
  [AC_CACHE_CHECK([page size],
     [vicare_cv_pagesize],
     [AC_COMPUTE_INT([vicare_cv_pagesize],[sysconf(_SC_PAGESIZE)],[
       #include <unistd.h>
     ],[vicare_cv_pagesize=4096])])])

dnl VICARE_CHECK_PAGESHIFT($pagesize)
dnl
dnl Determine  the number  of bits  to  right-shift a  pointer value  to
dnl obtain  the  index of  the  page (of  size  IK_PAGESIZE)  it is  in.
dnl Defaults to 12 which is the value for a page size of 4096.
dnl
dnl The test assumes that the page size is an exact power of 2.
dnl
AC_DEFUN([VICARE_CHECK_PAGESHIFT],
  [AC_CACHE_CHECK([page shift bit count],
     [vicare_cv_pageshift],
     [AC_RUN_IFELSE([AC_LANG_SOURCE([
        int main (void)
        {
           int count=0;
	   int roller=$1 - 1;
           FILE *f = fopen ("conftest.val", "w");
           while (roller) {
             ++count;
	     roller >>= 1;
           }
           fprintf(f, "%d", count);
           return ferror (f) || fclose (f) != 0;
        }])],
        [vicare_cv_pageshift=`cat conftest.val`],
        [vicare_cv_pageshift=12],
	[vicare_cv_pageshift=12])
      rm -f conftest.val])])])

### end of file
