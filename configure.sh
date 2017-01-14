# configure.sh --
#

set -xe

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi
LIBFFI_VERSION=3.2.1
LIBFFI_INCLUDEDIR=${libdir}/libffi-${LIBFFI_VERSION}/include

VFLAGS=${VFLAGS:=-O2}

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=../config.cache			\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    --enable-binfmt					\
    --enable-time-tests					\
    --enable-scheme-script				\
    --enable-file-magic					\
    --with-pthread					\
    CFLAGS='-O3 -pedantic'				\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"			\
    VFLAGS="$VFLAGS"					\
    "$@"

### end of file
