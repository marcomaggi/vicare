# configure.sh --
#

set -xe

prefix=/usr/local
LIBFFI_VERSION=3.2.1
if test -d /lib64
then
    libdir=${prefix}/lib64
    LIBFFI_INCLUDEDIR=${prefix}/lib64/libffi-${LIBFFI_VERSION}/include
else
    libdir=${prefix}/lib
    LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-${LIBFFI_VERSION}/include
fi

../configure \
    --enable-maintainer-mode					\
    --config-cache						\
    --cache-file=./config.cache					\
    --prefix="${prefix}"					\
    --libdir="${libdir}"					\
    --enable-binfmt						\
    --enable-time-tests						\
    --enable-scheme-script					\
    --with-pthread						\
    --with-cre2							\
    CFLAGS='-O3 -pedantic'					\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR} -D__FAKE_CYGWIN__=1"	\
    VFLAGS='-O3'						\
    "$@"

### end of file
