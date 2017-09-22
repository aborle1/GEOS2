SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  08Jan2013  jstassi  Created with gmm script.
#
#.......................................................................

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for make versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif

#------------------------------
# Make sure ESMADIR is defined
# -----------------------------
ifndef ESMADIR
   ESMADIR := $(PWD)/../../../..
endif

#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies

########################################################################

ALLDIRS = NSIDC-OSTIA_SST-ICE_blend

SUBDIRS = $(wildcard $(ALLDIRS))

########################################################################

#-----------------------
# Standard ESMA Targets
#-----------------------
TARGETS = esma_install esma_clean esma_distclean esma_doc \
          install clean distclean doc         

$(TARGETS):
	@echo
	@if [ "$(SUBDIRS)" != "" ]; then ( \
	   SUBTARGET=$@; export SUBTARGET; make -f $(GNUMAKEFILE) subdirs \
	) fi
	make -f $(GNUMAKEFILE) local_$@

local_esma_install local_install:
	@echo No local install in here

local_esma_clean local_clean:
	$(RM) *~ $(BINS) *.[aox] *.[Mm][Oo][Dd]

local_esma_distclean local_distclean:
	$(RM) *~ $(BINS) *.[aoxd] *.[Mm][Oo][Dd]

local_esma_doc local_doc:
	@echo No documentation here

esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make install       (builds and install under ESMADIR)"
	@echo "% make clean         (removes deliverables: *.[aox], etc)"
	@echo "% make distclean     (leaves in the same state as cvs co)"
	@echo "% make esma_doc      (generates PDF, installs under ESMADIR)"
	@echo "% make help          (this message)"
	@echo
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"

########################################################################

subdirs:
	@for subdir in $(SUBDIRS); do \
	   ( echo "> cd $$subdir; make -e $(SUBTARGET)"; \
	     cd $$subdir; make -e $(SUBTARGET) ) \
	done; \

########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(INC_LIST) /usr/include

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
ifneq ($(findstring clean,$(SUBTARGET)),clean)
   -include $(ALLDEPS)
endif
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
