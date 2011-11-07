## Process this file with automake to produce Makefile.in

bin_PROGRAMS = vicare
#bin_PROGRAMS = ikarus scheme-script

SRCS	= \
	ikarus-collect.c		\
	ikarus-exec.c			\
	ikarus-fasl.c			\
	ikarus-flonums.c		\
	ikarus-main.c			\
	ikarus-numerics.c		\
	ikarus-print.c			\
	ikarus-runtime.c		\
	ikarus-symbol-table.c		\
	ikarus-verify-integrity.c	\
	ikarus-weak-pairs.c		\
	ikarus-winmmap.c		\
	ikarus-data.h			\
	ikarus-winmmap.h		\
	ikarus-enter.S			\
	cpu_has_sse2.S			\
	ikarus-io.c			\
	ikarus-getaddrinfo.h		\
	ikarus-getaddrinfo.c		\
	ikarus-posix.c			\
	ikarus-main.h			\
	ikarus-pointers.c		\
	ikarus-ffi.c

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
