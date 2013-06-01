# configure.sh --
#

set -xe

prefix=/usr/local
LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-3.0.11/include

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="${prefix}"                        \
    --enable-binfmt                             \
    --enable-time-tests                         \
    --with-pthread                              \
    --with-cre2                                 \
    CFLAGS='-O3'				\
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"           \
    "$@"

## LDFLAGS='-L/usr/local/lib -lpthread'

### end of file
