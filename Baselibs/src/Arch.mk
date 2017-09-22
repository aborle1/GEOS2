# Baselibs Build System
#
#    System/site dependent options.

#
# REVISION HISTORY:
#
# 08Aug2008  da Silva  Moved here from GNUmakefile for better CM.
#
#-------------------------------------------------------------------------

#
#                            ----------------
#                               Compiler
#                            ----------------
#

# Intel
# -----
  ifneq ($(wildcard $(shell which ifort 2> /dev/null)),)
       CPPFLAGS += -DpgiFortran
  endif

# PGI
# ---
  ifneq ($(wildcard $(shell which pgf90 2> /dev/null)),)
       CPPFLAGS += -DpgiFortran
       PGI_FLAG = -PGI
  endif

#
#                            ----------------
#                                 Linux
#                            ----------------
#

ifeq ($(ARCH),Linux)
   SITE := $(patsubst discover%,NCCS,$(SITE))
   SITE := $(patsubst dali%,NCCS,$(SITE))
   SITE := $(patsubst borg%,NCCS,$(SITE))
   SITE := $(patsubst jibb%,NCCS,$(SITE))
   SITE := $(patsubst jcc%,NCCS,$(SITE))
   SITE := $(patsubst pfe%,NAS,$(SITE))
   SITE := $(patsubst bridge%,NAS,$(SITE))
   CFLAGS := -fPIC 
   export CFLAGS

   MODULECMD = $(shell which modulecmd 2> /dev/null)

   CURLSSL = --with-ssl
   ifeq ($(SITE),NCCS)
      ENABLE_GPFS = --enable-gpfs
      ESMF_COMM := intelmpi
      LINK_GPFS = -lgpfs
   endif
   ifeq ($(SITE),NAS)
      ESMF_COMM := mpi
      CURLSSL = --without-ssl
   endif
   ifeq ($(FC),gfortran)
      FFLAGS += -fno-second-underscore
      FCFLAGS += -fno-second-underscore
      CPPFLAGS += -DgFortran
   endif
   PYTHON := $(dir $(shell which python))../
endif

#
#                            ----------------
#                            MacOS X (Darwin)
#                            ----------------
#

ifeq ($(ARCH),Darwin)
   MODULECMD = $(shell which modulecmd 2> /dev/null)
   PYTHON := $(dir $(shell which python))../
endif

#
#                               --------
#                                  AIX
#                               --------
#

ifeq ($(ARCH),AIX)
   INSTALL_OPT=./install-sh -c
endif


