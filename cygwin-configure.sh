# configure.sh --
#

set -xe

prefix=/usr/local
LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-3.0.13/include

../configure \
    --enable-maintainer-mode							\
    --config-cache								\
    --cache-file=../config.cache						\
    --prefix="${prefix}"							\
    --disable-linux								\
    --enable-time-tests								\
    --with-pthread=check							\
    CFLAGS='-O3 -pedantic'							\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"						\
    LDFLAGS='-L/usr/local/lib -L/usr/local/lib64'				\
    VFLAGS='--no-drop-assertions --check-compiler-pass-preconditions'		\
    "$@"

## LDFLAGS='-L/usr/local/lib -lpthread'

### end of file
