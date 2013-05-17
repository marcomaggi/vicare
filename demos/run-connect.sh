#!/bin/sh
#
# Run the CONNECT demo program.

DIRECTORY=$(dirname $(realpath $0))
EXECUTABLE=$DIRECTORY/connect.sps

exec "$EXECUTABLE" \
    --debug                                             \
    $VFLAGS                                             \
    --                                                  \
    --verbose                                           \
    --log-file ~/var/log/vicare-connect.log             \
    --interface localhost                               \
    --port 8081                                         \
    "$@"

### end of file
