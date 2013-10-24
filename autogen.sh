# autogen.sh --
#
# Run this in the top source directory to rebuild the infrastructure.

LIBTOOLIZE=${LIBTOOLIZE:=libtoolize}

set -xe
test -d autotools		|| mkdir autotools
test -f autotools/libtool.m4	|| ${LIBTOOLIZE}
autoreconf --warnings=all --install --verbose "$@"

### end of file
