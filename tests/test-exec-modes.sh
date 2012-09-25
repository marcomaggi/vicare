#!/bin/sh
#
# Tests for execution modes.
#

VICARE=$1
BOOTFILE=$2
SRCDIR=$3
RUN="$VICARE --boot $BOOTFILE"
TESTDIR=$SRCDIR/exec-modes-helpers

echo ; echo ; set -x
$RUN --version
set +x ; echo ; echo ; echo ; set -x
$RUN --version-only
set +x ; echo ; echo ; echo ; set -x
$RUN --license
set +x ; echo ; echo ; echo ; set -x
$RUN --help
set +x ; echo ; echo ; echo ; set -x

cat "$TESTDIR/r6rs-forms"
$RUN --eval-file "$TESTDIR/r6rs-forms"
set +x ; echo ; echo ; echo ; set -x

cat "$TESTDIR/r6rs-program.sps"
$RUN --r6rs-script "$TESTDIR/r6rs-program.sps"
set +x ; echo ; echo ; echo ; set -x

cat "$TESTDIR/r6rs-program.sps"
$RUN --r6rs-repl "$TESTDIR/r6rs-program.sps"
set +x ; echo ; echo ; echo ; set -x

### end of file
