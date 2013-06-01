#!/bin/sh
#
# Run the SHELLD demo program.

DIRECTORY=$(dirname $(realpath $0))
EXECUTABLE=$DIRECTORY/shelld.sps

exec "$EXECUTABLE" \
    --debug                                             \
    $VFLAGS                                             \
    --                                                  \
    --verbose                                           \
    --pid-file ~/var/run/vicare-shelld.pid              \
    --log-file ~/var/log/vicare-shelld.log              \
    "$@"

#    --daemon

### end of file
