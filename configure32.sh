# configure32.sh --
#

set -xe

prefix=/usr/local
libdir=${prefix}/lib
# LIBFFI_VERSION=3.2.1
# LIBFFI_INCLUDEDIR=${libdir}/libffi-${LIBFFI_VERSION}/include

# NOTE TO  SELF Marco, this  script should be  run first to  prepare the
# build infrastructure; then do:
#
#   $ bash
#   $ source ~/.bash_profile.32bit
#
# to load a custom environment for 32-bit development, including the
# Slackware's multilib 32-bit setup.
#
# On  Slackware   64-bit  with  Alien's  multilib   (compat32)  packages
# installed: the source command below will setup the environment needed
# to build 32-bit applications.
#
#   $ source /etc/profile.d/32dev.sh
#
# If there  are problems  running this  script: try  to just  invoke the
# plain "configure" with maintainer mode enabled.
#

# NOTE For some reason unknown to me: when compiling for 32-bit with
# GCC 5.3.0 and "-O3" the function "ikrt_bignum_shift_right()" is
# miscompiled in  some case and an  error ensues.  IMHO a  compiler bug.
# Everything is all right for 64-bit.  For this reason the C code should
# be compiled with "-O2" on 32-bit platforms.  Life is hard.  (Marco
# Maggi; Sat Jan 7, 2017)

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=./config.cache				\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    --with-pthread					\
    CFLAGS='-m32 -O2 -pedantic'				\
    CPPFLAGS=""						\
    LDFLAGS='-m32'					\
    "$@"

### end of file
