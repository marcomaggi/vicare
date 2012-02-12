dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([vicare-template],[0.1d0],[marco.maggi-ipsu@poste.it])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE
LT_PREREQ([2.2])
LT_INIT
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([m4])
dnl AM_MAINTAINER_MODE
AC_CANONICAL_HOST

#page
#### compiler

AM_INIT_AUTOMAKE([-Wall])

AC_PROG_CC
AC_PROG_CC_C_O

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### template configuration

dnl AC_CHECK_HEADERS([template.h],,
dnl   [AC_MSG_FAILURE([missing template.h header],[2])])
dnl AC_CHECK_LIB([template],[template],,
dnl   [AC_MSG_FAILURE([missing Template library],[2])])

vicare_template_VERSION_INTERFACE_CURRENT=0
vicare_template_VERSION_INTERFACE_REVISION=0
vicare_template_VERSION_INTERFACE_AGE=0
AC_DEFINE_UNQUOTED([vicare_template_VERSION_INTERFACE_CURRENT],
  [$vicare_template_VERSION_INTERFACE_CURRENT],
  [current interface number])
AC_DEFINE_UNQUOTED([vicare_template_VERSION_INTERFACE_REVISION],
  [$vicare_template_VERSION_INTERFACE_REVISION],
  [current interface implementation number])
AC_DEFINE_UNQUOTED([vicare_template_VERSION_INTERFACE_AGE],
  [$vicare_template_VERSION_INTERFACE_AGE],
  [current interface age number])
AC_DEFINE_UNQUOTED([vicare_template_VERSION_INTERFACE_STRING],
  ["$vicare_template_VERSION_INTERFACE_CURRENT.$vicare_template_VERSION_INTERFACE_REVISION"],
  [library interface version])
AC_SUBST([vicare_template_VERSION_INTERFACE_CURRENT])
AC_SUBST([vicare_template_VERSION_INTERFACE_REVISION])
AC_SUBST([vicare_template_VERSION_INTERFACE_AGE])

#page
#### symbols

dnl Stuff
dnl VICARE_CONSTANT_TESTS([XML_TRUE XML_FALSE])

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile doc/Makefile tests/Makefile lib/Makefile lib/vicare/template/constants.sls])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
