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
    --prefix="${prefix}"                        \
    --libdir="${libdir}"                        \
    --config-cache                              \
    --cache-file=./config.cache-bare		\
    --disable-binfmt                            \
    --disable-time-tests                        \
    --disable-posix                             \
    --disable-glibc                             \
    --disable-linux                             \
    --without-libiconv                          \
    --without-pthread                           \
    --without-libffi                            \
    --without-readline                          \
    CFLAGS='-O3 -pedantic'			\
    "$@"

### end of file
