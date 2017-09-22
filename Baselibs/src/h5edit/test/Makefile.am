#
# Copyright by The HDF Group 2010-2013.
# All rights reserved.
#
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# H5edir test Makefile(.in)
#

# Activate this later.
include $(top_srcdir)/config/commence.am

# Test programs and scripts
TEST_SCRIPT=$(srcdir)/testh5edit.sh

check_PROGRAMS=			# none
check_SCRIPTS=$(TEST_SCRIPT)
SCRIPT_DEPEND=../src/edit$(EXEEXT)

# No binary to build
bin_PROGRAMS=

# Temporary files.
CHECK_CLEANFILES+=
DISTCLEANFILES=

include $(top_srcdir)/config/conclude.am
