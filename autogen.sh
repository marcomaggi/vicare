# autogen.sh --
#
# Run this in the top source directory to rebuild the infrastructure.

export PATH=/bin:/usr/local/bin:/usr/bin:$PATH

set -xe
test -d autotools		|| mkdir autotools
test -f autotools/libtool.m4	|| libtoolize
autoreconf --warnings=all --install --verbose "$@"

### end of file
