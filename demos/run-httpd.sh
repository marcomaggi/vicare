#!/bin/sh
#
# Run the HTTPD demo program.

exec ./httpd.sps \
    --debug                                             \
    --                                                  \
    --verbose                                           \
    --document-root ~/share/public-html                 \
    --pid-file ~/var/run/vicare-httpd.pid               \
    --log-file ~/var/log/vicare-httpd.log

### end of file
