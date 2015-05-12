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

VFLAGS=${VFLAGS:=-O2 --no-drop-assertions --check-compiler-pass-preconditions}

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=../config.cache			\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    --enable-descriptive-labels-generation		\
    --enable-binfmt					\
    --enable-time-tests					\
    --enable-scheme-script				\
    --with-pthread					\
    --with-cre2						\
    CFLAGS='-O3 -pedantic'				\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"			\
    VFLAGS="$VFLAGS"					\
    "$@"

### end of file
