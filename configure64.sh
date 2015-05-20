# configure.sh --
#

set -xe

prefix=/usr/local
libdir=${prefix}/lib64
LIBFFI_VERSION=3.2.1
LIBFFI_INCLUDEDIR=${libdir}/libffi-${LIBFFI_VERSION}/include

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=config.cache				\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    --enable-binfmt					\
    --enable-time-tests					\
    --with-pthread					\
    CFLAGS='-m64 -O3 -pedantic'				\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"			\
    LDFLAGS='-m64'					\
    VFLAGS='-O3'					\
    "$@"

### end of file
