# prepare.sh --
#
# Run this to rebuild the infrastructure and configure.

set -xe

(set -xe ;  cd .. && sh autogen.sh)

prefix=/usr/local
LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-3.0.10/include

../configure \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="${prefix}"                        \
    --enable-shared --disable-static            \
    --enable-libffi                             \
    --with-iconv                                \
    --with-cre2                                 \
    CFLAGS='-g -O3 -march=i686 -mtune=i686'     \
    CPPFLAGS="-I${LIBFFI_INCLUDEDIR}"           \
    LDFLAGS='-L/usr/local/lib -lpthread'        \
    "$@"

### end of file
