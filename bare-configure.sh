# configure.sh --
#

set -xe

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache-bare           \
    --prefix="${prefix}"                        \
    --libdir="${libdir}"                        \
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
