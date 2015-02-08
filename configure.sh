# configure.sh --
#

set -xe

prefix=/usr/local
if test -d /lib64
then
    libdir=${prefix}/lib64
    LIBFFI_INCLUDEDIR=${prefix}/lib64/libffi-3.2.1/include
else
    libdir=${prefix}/lib
    LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-3.2.1/include
fi

../configure \
    --enable-maintainer-mode				\
    --config-cache					\
    --cache-file=../config.cache			\
    --prefix="${prefix}"				\
    --libdir="${libdir}"				\
    --enable-binfmt					\
    --enable-time-tests					\
    --enable-scheme-script				\
    --with-pthread					\
    --with-cre2						\
    CFLAGS='-O3 -pedantic'				\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"			\
    LDFLAGS='-L/usr/local/lib -L/usr/local/lib64'	\
    VFLAGS='-O3'					\
    "$@"

## LDFLAGS='-L/usr/local/lib -lpthread'

### end of file
