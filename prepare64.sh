# prepare64.sh --
#
# Run this to rebuild the infrastructure and configure.

set -xe

(cd .. && sh autogen.sh)
sh ../configure64.sh

### end of file
