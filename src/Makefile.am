## Process this file with automake to produce Makefile.in

bin_PROGRAMS = vicare
#bin_PROGRAMS = ikarus

SRCS	= \
	cpu_has_sse2.S			\
	ikarus-collect.c		\
	ikarus-data.h			\
	ikarus-enter.S			\
	ikarus-exec.c			\
	ikarus-fasl.c			\
	ikarus-ffi.c			\
	ikarus-flonums.c		\
	ikarus-getaddrinfo.c		\
	ikarus-getaddrinfo.h		\
	ikarus-io.c			\
	ikarus-linux.c			\
	ikarus-main.c			\
	ikarus-numerics.c		\
	ikarus-object-utilities.c	\
	ikarus-pointers.c		\
	ikarus-posix.c			\
	ikarus-print.c			\
	ikarus-runtime.c		\
	ikarus-symbol-table.c		\
	ikarus-verify-integrity.c	\
	ikarus-weak-pairs.c		\
	ikarus-winmmap.c		\
	ikarus-winmmap.h		\
	ikarus.h

vicare_SOURCES = $(SRCS) ikarus.c
#ikarus_SOURCES = $(SRCS) ikarus.c
#scheme_script_SOURCES = $(SRCS) scheme-script.c

nodist_vicare_SOURCES = bootfileloc.h
BUILT_SOURCES = bootfileloc.h

bootfileloc.h: Makefile
	echo '#define BOOTFILE "$(pkglibdir)/vicare.boot"' >$@
	echo '#define EXEFILE "$(bindir)/vicare"' >>$@

CLEANFILES = bootfileloc.h

### end of file
