dnl
dnl Part of: Vicare Scheme
dnl Contents: macros for vicare scheme
dnl Date: Wed Jul 16, 2014
dnl
dnl Abstract
dnl
dnl	Macros used in "configure.ac" by the package Vicare Scheme.
dnl
dnl Copyright (C) 2014 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free  Software Foundation, either  version 3 of the  License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the  hope that it will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
dnl MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received a copy  of the GNU General  Public License
dnl along      with      this       program.       If      not,      see
dnl <http://www.gnu.org/licenses/>.
dnl

dnl Wrapper for AC_ARG_ENABLE which adds  verbose messages and defines a
dnl shell variable "vicare_enable_$1" set to "yes" or "no".
dnl
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

dnl Wrapper  for  AC_CHECK_HEADER  and   AC_CHECK_LIB  which  tests  the
dnl availability of a foreign C language library and a single associated
dnl C language  header file.   The inclusion of  the foreign  library is
dnl selected with  an option for  "configure".  Link flags are  added to
dnl the variable LIBS.
dnl
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
      AC_LANG_PUSH([C])
      AC_CHECK_HEADER([$6],[vicare_have_$2_h=yes],[vicare_have_$2_h=no])
      AC_CHECK_LIB([$7],[$8],[vicare_have_$2=yes],[vicare_have_$2=no])
      AC_LANG_POP([C])
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
#ifdef HAVE_BITS_SOCKET_H
#  include <bits/socket.h>
#endif
#ifdef HAVE_LINUX_ICMP_H
#  include <linux/icmp.h>
#endif
#ifdef HAVE_SYS_UTSNAME_H
#  include <sys/utsname.h>
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
#ifdef HAVE_NETINET_ETHER_H
#  include <netinet/ether.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#  include <netinet/tcp.h>
#endif
#ifdef HAVE_NETINET_UDP_H
#  include <netinet/udp.h>
#endif
#ifdef HAVE_NETINET_IGMP_H
#  include <netinet/igmp.h>
#endif
#ifdef HAVE_NETPACKET_PACKET_H
#  include <netpacket/packet.h>
#endif
#ifdef HAVE_NET_ETHERNET_H
#  include <net/ethernet.h>
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
dnl Testing integer values to be substituted in Scheme libraries.

dnl Wrapper for AC_COMPUTE_INT which computes  and caches the value of a
dnl C  language constant.   For example,  to  compute the  value of  the
dnl "errno" constant EPERM we do:
dnl
dnl    VICARE_VALUEOF_TEST([EPERM],[EPERM])
dnl
dnl this macro expansion: defines  the shell variable "VALUEOF_EPERM" to
dnl the    value    of    EPERM;     defines    the    shell    variable
dnl "vicare_cv_valueof_EPERM" to  cache the  value; defines  an Autoconf
dnl substitution for the symbol "VALUEOF_EPERM".
dnl
dnl $1 - the stem  to use  to define  shell variables  representing the
dnl      result of this test
dnl $2 - the name of a C language constant whose value is an integer
dnl
AC_DEFUN([VICARE_VALUEOF_TEST],
  [VALUEOF_$1="#f"
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

dnl Define a variable and substitution set to the Scheme false value.
AC_DEFUN([VICARE_CONSTANT_FALSE],
  [VALUEOF_$1="#f"
   AC_SUBST([VALUEOF_$1])])
AC_DEFUN([VICARE_CONSTANT_FALSES],[m4_map_args_w($1,[VICARE_CONSTANT_FALSE(],[)])])

dnl page
dnl Testing string values to be substituted in Scheme libraries.

dnl Wrapper for AC_RUN_IFELSE which computes and caches the value of a C
dnl language string constant.  For example,  to compute the value of the
dnl POSIX constant FSTAB_RW we do:
dnl
dnl    VICARE_STRINGOF_TEST([FSTAB_RW],[FSTAB_RW])
dnl
dnl this macro expansion: defines  the shell variable "VALUEOF_FSTAB_RW"
dnl to   the   value   of   FSTAB_RW;   defines   the   shell   variable
dnl "vicare_cv_stringof_FSTAB_RW"  to   cache  the  value;   defines  an
dnl Autoconf substitution for the symbol "VALUEOF_FSTAB_RW".
dnl
dnl $1 - the stem  to use  to define  shell variables  representing the
dnl      result of this test
dnl $2 - the name of a C language constant whose value is a string
dnl
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
dnl Testing integer values to be substituted in Scheme libraries.

dnl Wrapper for AC_RUN_IFELSE which computes and caches the value of a C
dnl language "double float" constant.  For example, to compute the value
dnl of the constant M_LOG2E we do:
dnl
dnl    VICARE_DOUBLEOF_TEST([M_LOG2E],[M_LOG2E])
dnl
dnl this macro  expansion: defines the shell  variable "VALUEOF_M_LOG2E"
dnl to   the   value   of    M_LOG2E;   defines   the   shell   variable
dnl "vicare_cv_doubleof_M_LOG2E" to cache the value; defines an Autoconf
dnl substitution for the symbol "VALUEOF_M_LOG2E".
dnl
dnl $1 - the stem  to use  to define  shell variables  representing the
dnl      result of this test
dnl $2 - the name of a C language constant whose value is a double
dnl
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
dnl Special tests.

dnl Test the availability of one  of the macros: WIFEXITED, WEXITSTATUS,
dnl WIFSIGNALED,  WTERMSIG,  WCOREDUMP,  WIFSTOPPED, WSTOPSIG.   If  the
dnl macro is  available: HAVE_$1 is defined  to 1, otherwise to  0.  The
dnl result of the test is cached.
dnl
dnl $1 - The macro name.
dnl
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

dnl Test the  availability of one  of the macros:  IN_CLASSA, IN_CLASSB,
dnl IN_CLASSC,  IN_CLASSD,  IN_MULTICAST, IN_EXPERIMENTAL,  IN_BADCLASS.
dnl If the macro is available: HAVE_$1  is defined to 1, otherwise to 0.
dnl The result of the test is cached.
dnl
dnl $1 - The macro name.
dnl
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

dnl Test the availability of one of the macros: IN6_IS_ADDR_UNSPECIFIED,
dnl IN6_IS_ADDR_LOOPBACK,  IN6_IS_ADDR_LINKLOCAL, IN6_IS_ADDR_SITELOCAL,
dnl IN6_IS_ADDR_V4MAPPED,  IN6_IS_ADDR_V4COMPAT,  IN6_IS_ADDR_MULTICAST,
dnl IN6_IS_ADDR_MC_NODELOCAL,                  IN6_IS_ADDR_MC_LINKLOCAL,
dnl IN6_IS_ADDR_MC_SITELOCAL,                   IN6_IS_ADDR_MC_ORGLOCAL,
dnl IN6_IS_ADDR_MC_GLOBAL.   If  the  macro  is  available:  HAVE_$1  is
dnl defined to 1, otherwise to 0.  The result of the test is cached.
dnl
dnl $1 - The macro name.
dnl
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

dnl --------------------------------------------------------------------

dnl Test the availability of one  of the macros: IN6_ARE_ADDR_EQUAL.  If
dnl the macro  is available: HAVE_$1  is defined  to 1, otherwise  to 0.
dnl The result of the test is cached.
dnl
dnl $1 - The macro name.
dnl
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
dnl Special tests.

AC_DEFUN([VICARE_CHECK_MMAP_ALLOCATION_GRANULARITY],
  [AC_CACHE_CHECK([mmap allocation granularity],
     [vicare_cv_mmap_allocation_granularity],
     [AC_COMPUTE_INT([vicare_cv_mmap_allocation_granularity],[sysconf(_SC_PAGESIZE)],[
       #include <unistd.h>
     ],[vicare_cv_mmap_allocation_granularity=4096])])])

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


dnl end of file
dnl Local Variables:
dnl mode: m4
dnl End:
