# prepare32.sh --
#
# Run this to rebuild the infrastructure and configure.

set -xe

(cd .. && sh autogen.sh)
sh ../configure32.sh

### end of file
