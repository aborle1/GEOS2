# HDF-EOS5 src Makefile.am

# Boilerplate definitions
include $(top_srcdir)/config/include.am

## Note that gctp/include contains several header files with the same names
## as files in $(top_srcdir)/include.  This doesn't currently cause problems
## in this directory, but may if Automake changes.
INCLUDES=-I$(top_srcdir)/include/

# Set LDFLAGS to allow the HDF-EOS library to use extern variables from
# HDF5
LDFLAGS=-Wl,-single_module

# Build HDF-EOS5
lib_LTLIBRARIES=libhe5_hdfeos.la

# Source files for library
libhe5_hdfeos_la_SOURCES=EHapi.c GDapi.c PTapi.c SWapi.c TSapi.c ZAapi.c
