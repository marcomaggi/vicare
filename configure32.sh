# configure32.sh --
#

set -xe

prefix=/usr/local
LIBFFI_INCLUDEDIR=${prefix}/lib/libffi-3.2.1/include

# On  Slackware 64-bit  with Alien's  compat32 packages  installed: this
# will setup the environment needed to build 32-bit applications.
. /etc/profile.d/32dev.sh

../configure \
    --enable-maintainer-mode				\
    --prefix="${prefix}"				\
    --with-pthread					\
    CFLAGS='-m32 -O3 -pedantic'				\
    CPPFLAGS=""						\
    LDFLAGS='-m32'					\
    VFLAGS='-O3'					\
    "$@"

## LDFLAGS='-L/usr/local/lib -lpthread'

### end of file
