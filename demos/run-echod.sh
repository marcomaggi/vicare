#!/bin/sh
#
# Run the ECHOD demo program.

DIRECTORY=$(dirname $(realpath $0))
EXECUTABLE=$DIRECTORY/echod.sps

exec "$EXECUTABLE" \
    --debug                                             \
    $VFLAGS                                             \
    --                                                  \
    --verbose                                           \
    --pid-file ~/var/run/vicare-echod.pid               \
    --log-file ~/var/log/vicare-echod.log               \
    "$@"

#    --daemon

### end of file
