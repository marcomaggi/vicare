#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(vicare, 0.1d4, marco.maggi-ipsu@poste.it)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(vicare, 0.1d4, gnu)
AC_CONFIG_SRCDIR([src/])


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

AC_SYS_LARGEFILE

# Checks for libraries.
AC_SEARCH_LIBS(dlsym, dl,, [AC_MSG_ERROR([Cannot find libdl])])
AC_SEARCH_LIBS(cos, m,, [AC_MSG_ERROR([Cannot find libm])])
AC_SEARCH_LIBS(nanosleep, rt,, [AC_MSG_ERROR([Cannot find nanosleep])])
AC_SEARCH_LIBS(socket, socket,, [AC_MSG_ERROR([Cannot find socket])])


# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT


AC_CHECK_HEADERS([assert.h dlfcn.h errno.h fcntl.h signal.h \
                  stdint.h stdlib.h string.h strings.h \
                  sys/mman.h sys/resource.h sys/stat.h \
                  sys/time.h sys/types.h sys/wait.h time.h \
                  unistd.h])

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



# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_HEADER_TIME
AC_STRUCT_TM


# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_STAT
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_CHECK_FUNCS([sigaltstack bzero gettimeofday memmove memset munmap setenv sqrt strerror getaddrinfo])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES(Makefile src/Makefile scheme/Makefile doc/Makefile lib/Makefile benchmarks/Makefile tests/Makefile)
AC_OUTPUT #(Makefile src/Makefile scheme/Makefile doc/Makefile)

