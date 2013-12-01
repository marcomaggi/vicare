# cygwin-configure.sh --
#

set -xe

prefix=/cygwin/usr/local

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache-bare           \
    --prefix="${prefix}"                        \
    --disable-binfmt                            \
    --disable-time-tests                        \
    --disable-posix                             \
    --disable-glibc                             \
    --disable-linux                             \
    --without-pthread                           \
    --without-libffi                            \
    --without-libiconv                          \
    --without-readline                          \
    --without-cre2                              \
    CFLAGS='-O3'				\
    "$@"

### end of file
