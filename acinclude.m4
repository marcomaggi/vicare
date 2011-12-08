### custom macros

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
#ifdef HAVE_MATH_H
#  include <math.h>
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
#ifdef HAVE_NETDB_H
#  include <netdb.h>
#endif
#ifdef HAVE_NET_IF_H
#  include <net/if.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
#ifdef HAVE_PATHS_H
#  include <paths.h>
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
        [vicare_cv_stringof_$1=""])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_stringof_$1"
   AC_SUBST([VALUEOF_$1])])

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
        [vicare_cv_doubleof_$1=""])
      rm -f conftest.val])
   VALUEOF_$1="$vicare_cv_doubleof_$1"
   AC_SUBST([VALUEOF_$1])])

### end of file
