# gdbinit --
#
# Copy this file in the directory from which we run "vicare", which is
# usually "$(top_builddir)/tests".  Run gdb as "gdb --args" so that
# command line arguments in the Makefile are handed to "vicare" rather
# than to "gdb".
#
# From the commmand line of make:
#
#   $ make GDB='gdb --args' ...
#

directory ../../src
run

### end of file
