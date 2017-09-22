#
#  Implement esmf specific targets
#

#                          -----------
#                          Environment
#                          -----------

ifndef ESMF_DIR
  ESMF_DIR := $(shell dirname `pwd`)/src/esmf
endif

  ESMF_BOPT           ?= O
  ESMF_INSTALL_PREFIX ?= $(prefix)
  ESMF_COMPILER       ?= $(F90)

# ifort
# -----
  ifeq ($(ESMF_COMPILER), ifort) 

      ifeq ($(CXX),icpc)
         IS_INTEL_V10 = $(shell $(CXX) --version | sed q | awk '{print $$3}')
         ifeq ($(findstring 10., $(IS_INTEL_V10)), 10.)
           ESMF_COMPILER = intel10
         else
           ESMF_COMPILER = intel
         endif
      else
         ESMF_COMPILER = intelgcc
         
         # These fix an issue that intelgcc does not do -fPIC
         ESMF_SO_F90COMPILEOPTS = -fPIC
         ESMF_SO_F90LINKOPTS    = -shared
         ESMF_SO_F90LINKOPTSEXE = -Wl,-export-dynamic
         ESMF_SO_CXXCOMPILEOPTS = -fPIC
         ESMF_SO_CXXLINKOPTS    = -shared
         ESMF_SO_CXXLINKOPTSEXE = -Wl,-export-dynamic
         export ESMF_SO_F90COMPILEOPTS
         export ESMF_SO_F90LINKOPTS
         export ESMF_SO_F90LINKOPTSEXE
         export ESMF_SO_CXXCOMPILEOPTS
         export ESMF_SO_CXXLINKOPTS
         export ESMF_SO_CXXLINKOPTSEXE
      endif

# PGI
# ---
  else
  ifeq ($(ESMF_COMPILER),pgf90)
     ESMF_COMPILER = pgi

# NAG
# ---
  else
  ifeq ($(ESMF_COMPILER),nagfor)
     ESMF_COMPILER = nag

# gfortran
# --------
  else
  ifeq ($(ESMF_COMPILER),gfortran)
     ESMF_COMPILER = gfortran
     ESMF_CXXOPTFLAG_G = -O0 -g3 -gdwarf-2
     ESMF_F90OPTFLAG_G = -O0 -g3 -gdwarf-2 -fbounds-check
     export ESMF_CXXOPTFLAG_G
     export ESMF_F90OPTFLAG_G

# Lahey
# -----
  else
  ifeq ($(ESMF_COMPILER),lf95)
     ESMF_COMPILER = lahey

# xlf90
# -----
  else
  ifeq ($(ESMF_COMPILER),xlf90_r)
     ESMF_COMPILER=default
  endif # ibm xl

  endif # lahey
  endif # gfortran
  endif # nag
  endif # pgi
  endif # intel

# Customize OpenMPI wrappers
# --------------------------
  ifeq ($(ESMF_COMM),openmpi)
      OMPI_FC := $(F90)
      export OMPI_FC
      ESMF_OPENMP := OFF
      export ESMF_OPENMP
  endif 

  $(warning Using $(ESMF_COMPILER) as the ESMF compiler (FC=$(FC)))
  $(warning Using $(ESMF_MACHINE)  as the ESMF machine)

#      ESMF_COMM 
#      ESMF_ABI
#      ESMF_OS

# ESMF NetCDF and PIO Building
# ----------------------------
  ifeq ($(ESMF_PIO),internal)
     ESMF_NETCDF := user
     ESMF_NETCDF_INCLUDE := $(ESMF_INSTALL_PREFIX)/include/netcdf
     ESMF_NETCDF_LIBPATH := $(ESMF_INSTALL_PREFIX)/lib

     LIB_NETCDF = $(shell $(prefix)/bin/nf-config --flibs)

     ESMF_NETCDF_LIBS := -L$(ESMF_INSTALL_PREFIX)/lib -lnetcdff -lnetcdf $(LIB_NETCDF)

     $(warning Building ESMF_PIO as $(ESMF_PIO))
     $(warning Using ESMF_NETCDF as $(ESMF_NETCDF))
     $(warning Using ESMF_NETCDF_INCLUDE as $(ESMF_NETCDF_INCLUDE))
     $(warning Using ESMF_NETCDF_LIBPATH as $(ESMF_NETCDF_LIBPATH))
     $(warning Using ESMF_NETCDF_LIBS as $(ESMF_NETCDF_LIBS))

     export ESMF_NETCDF ESMF_NETCDF_INCLUDE ESMF_NETCDF_LIBPATH ESMF_NETCDF_LIBS
  endif

export ESMF_DIR ESMF_BOPT ESMF_COMPILER ESMF_INSTALL_PREFIX 

esmf.config config: 
	@echo "Customized ESMF build..."
	@touch esmf.config

esmf.all all: esmf.config
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e all)

esmf.lib lib: esmf.config
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e lib)

esmf.info info:
	@echo "ESMF configuration"
	@(cd $(ESMF_DIR); $(MAKE) -e info)

esmf.script_info script_info:
	@echo "ESMF configuration"
	@(cd $(ESMF_DIR); $(MAKE) -e script_info)

esmf.check check:
	@echo "ESMF configuration"
	@(cd $(ESMF_DIR); $(MAKE) -e check)
	@touch esmf.check

esmf.examples:
	@echo "Running ESMF Examples..."
	@(cd $(ESMF_DIR); $(MAKE) -e examples)

esmf.clean clean:
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e clean)

esmf.distclean distclean:
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e distclean)

# Do installation from here to avoid ESMF's weird directory names
esmf.install install: esmf.config
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e lib)
	@mkdir -p                          $(ESMF_INSTALL_PREFIX)/lib	
	-@cp -pr $(ESMF_DIR)/lib/*/*/*     $(ESMF_INSTALL_PREFIX)/lib
	@mkdir -p                          $(ESMF_INSTALL_PREFIX)/include/esmf
	-@cp -pr $(ESMF_DIR)/mod/*/*/*     $(ESMF_INSTALL_PREFIX)/include/esmf
	-@cp -pr $(ESMF_DIR)/src/include/* $(ESMF_INSTALL_PREFIX)/include/esmf
	@touch esmf.install

# Until Intel 16 bug fixed, the ESMF apps are built separately if requested
esmf.apps: esmf.install
	@(cd $(ESMF_DIR); $(MAKE) -e build_apps ESMF_LDIR=$(ESMF_INSTALL_PREFIX)/lib ESMF_LIBDIR=$(ESMF_INSTALL_PREFIX)/lib ESMF_MODDIR=$(ESMF_INSTALL_PREFIX)/include/esmf)
	@mkdir -p                          $(ESMF_INSTALL_PREFIX)/bin
	-@cp -pr $(ESMF_DIR)/apps/*/*/*    $(ESMF_INSTALL_PREFIX)/bin

%: 
	@echo "Customized ESMF build..."
	@(cd $(ESMF_DIR); $(MAKE) -e $@)

