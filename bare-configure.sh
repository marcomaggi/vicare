# configure.sh --
#

set -xe

prefix=/usr/local

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
    CFLAGS='-O3 -pedantic'			\
    "$@"

### end of file
