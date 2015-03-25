# configure32.sh --
#

set -xe

prefix=/usr/local
libdir=${prefix}/lib
LIBFFI_VERSION=3.2.1
LIBFFI_INCLUDEDIR=${libdir}/libffi-${LIBFFI_VERSION}/include

# On  Slackware 64-bit  with Alien's  compat32 packages  installed: this
# will setup the environment needed to build 32-bit applications.
. /etc/profile.d/32dev.sh

../configure \
    --enable-maintainer-mode							\
    --prefix="${prefix}"							\
    --with-pthread								\
    CFLAGS='-m32 -O3 -pedantic'							\
    CPPFLAGS=""									\
    LDFLAGS='-m32'								\
    VFLAGS='--no-drop-assertions --check-compiler-pass-preconditions'		\
    "$@"

### end of file
