## Process this file with automake to produce Makefile.in

vicare_template_CURRENT	= @vicare_template_VERSION_INTERFACE_CURRENT@
vicare_template_REVISION= @vicare_template_VERSION_INTERFACE_REVISION@
vicare_template_AGE	= @vicare_template_VERSION_INTERFACE_AGE@
vicare_template_VERSION	= $(vicare_template_CURRENT):$(vicare_template_REVISION):$(vicare_template_AGE)

lib_LTLIBRARIES			= libvicare-template.la
libvicare_template_la_LDFLAGS	= -version-info $(vicare_template_VERSION)
libvicare_template_la_SOURCES	= vicare-template.c

### end of file
