#!/bin/sh
#
# Run the CONNECT demo program.

DIRECTORY=$(dirname $(realpath $0))
EXECUTABLE=$DIRECTORY/connect.sps

exec "$EXECUTABLE" \
    $VFLAGS                                             \
    --                                                  \
    localhost 8081                                      \
    --verbose                                           \
    --log-file -                                        \
    --recv-first                                        \
    "$@"

### end of file
