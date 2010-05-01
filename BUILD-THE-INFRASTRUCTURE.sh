# BUILD-THE-INFRASTRUCTURE.sh --
#
# Run this in the top source directory to rebuild the infrastructure.

set -xe

aclocal
autoheader
automake --foreign --add-missing
autoconf

### end of file
