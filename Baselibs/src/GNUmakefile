#
# RecursiveGNU makefile for Baselibs. 
#
# NOTE: If you would like to add new packages, look at module
#       supplibs under the opengrads CVS repository on sourceforge:
#
#            http://sourceforge.net/cvs/?group_id=161773 
#
# Packages such as NetCDF-4, HDF-5 and OPeNDAP have already been implemented
# there in a structure very similar to this one.
#
# !REVISION HISTORY:
#
#  25Jul2007  da Silva  First implementation
#  11Aug2008  da Silva  Aded Base.mk/Arch.mk to simplify CM. 
#
#-------------------------------------------------------------------------

# System dependent defauls
# ------------------------
  include Base.mk
  include Arch.mk
  include baselibs-config.mk

# Use these in recursive Makefiles
# --------------------------------
  export prefix
  export CC CXX F77 F90 FC MPICC MPICXX MPIFC ES_CC ES_CXX ES_FC
  export ESMF_COMM ESMF_DIR

# When HDF-5 is available, build NetCDF-4 with it,
# otherwise perform a vanilla NetCDF-4 build
# -----------------------------------------------
  NC_LIBS = -lm
  NC_CC  = $(CC)
  NC_CXX = $(CXX)
  NC_FC  = $(FC)
  NC_F77 = $(FC)
  ifeq ($(wildcard hdf5),hdf5)
        H5_PARALLEL=--enable-parallel
        ifeq ($(ESMF_COMM),mpiuni)
           H5_PARALLEL=--disable-parallel
        endif
        INC_HDF5 = $(prefix)/include/hdf5
        LIB_HDF5 = $(wildcard $(foreach lib, hdf5_hl hdf5 z sz,\
           $(prefix)/lib/lib$(lib).a) )
        NC_ENABLE_HDF5 = --enable-netcdf-4 --with-hdf5=$(prefix)
        NC_LIBS = $(LIB_HDF5) $(LINK_GPFS) -lm 
        ifeq ($(H5_PARALLEL),--enable-parallel)
           NC_CC  = $(MPICC)
           NC_FC  = $(MPIFC)
           NC_F77 = $(MPIFC)
           NC_CXX = $(MPICXX)
           NC_PAR_TESTS = --enable-parallel-tests
           H5_CC  = $(prefix)/bin/h5pcc
        endif
        
        ifeq ($(H5_PARALLEL),--disable-parallel)
           H5_CC  = $(prefix)/bin/h5cc
        endif
  endif

# netCDF has an issue with Intel 15+. It must be compiled with -g -O0 to
# avoid the problem (which manifests as a crash in ESMF_cfio where in a
# FPE is thrown in an NF90_DEF_VAR for time!).
# ----------------------------------------------------------------------

  ifeq ($(FC), ifort)
     NC_CFLAGS = "-g -O0"
  endif

# HDF5, MVAPICH2 2.1rc1 (at least), and SLES 11 SP3 have an issue. Aaron
# Knister of NCCS determined that:
#
#    If you set the environment variable RTLD_DEEPBIND to 0 then the
#    segfault disappears. I'm trying to better understand this, but works by
#    altering the behaviour of glibc when loading NSS modules. The symbol
#    resolution order seems to change with it set (for the better in this
#    case).
#
# So, we test if we are using mvapich2 and are on SP3.
# ---------------------------------------------------------------------

  ifeq ($(ESMF_COMM), mvapich2)
    IS_SLES11_SP3 = $(shell cat /etc/SuSE-release | awk '/PATCH/ {print $3}')
       ifeq ($(findstring 3, $(IS_SLES11_SP3)), 3)
          HDF5_DEEPBIND = "export RTLD_DEEPBIND=0"
       endif
  endif

# PGI does not seem to be able to build the Fortran 2003
# bindings as of PGI 14.7
# ------------------------------------------------------
  NCO_OPENMP = --enable-openmp
  ifeq ($(FC),pgf90)
     NCO_OPENMP = --disable-openmp
  endif

# From Tom Clune: NAG is different. It has a different version and FPIC
# flag. It also needs extra flags to compile code that is not as string
# as it would like.
# ---------------------------------------------------------------------
  FORTRAN_FPIC = -fPIC
  FORTRAN_VERSION = --version
  ifeq ($(FC),nagfor)
     FORTRAN_FPIC = -PIC
     FORTRAN_VERSION = -V
     NAG_FCFLAGS = -fpp -mismatch_all
     NAG_DUSTY = -dusty
  endif

#-------------------------------------------------------------------------

#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

# On Darwin, don't build uuid. Use the built-in one.
ifeq ($(ARCH),Darwin)
ALLDIRS = jpeg zlib szlib curl hdf4 hdf5 h5edit netcdf netcdf-fortran udunits2 nco cdo esmf pFUnit hdfeos cmor hdfeos5 SDPToolkit
else
ALLDIRS = jpeg zlib szlib curl hdf4 hdf5 h5edit netcdf netcdf-fortran udunits2 nco cdo esmf pFUnit hdfeos uuid cmor hdfeos5 SDPToolkit
endif

SUBDIRS = $(wildcard $(ALLDIRS) )
TARGETS = all lib install

INC_SUPP :=  $(foreach subdir, \
            / /zlib /szlib /jpeg /hdf5 /hdf /uuid /netcdf /udunits2,\
            -I$(prefix)/include$(subdir) $(INC_EXTRA) )

verify:
	@echo NC_CC     = $(NC_CC)
	@echo NC_CXX    = $(NC_CXX)
	@echo NC_FC     = $(NC_FC)
	@echo NC_F77    = $(NC_F77)
	@echo NC_CFLAGS = $(NC_CFLAGS) 
	@ argv="$(SUBDIRS)" ;\
        ( echo "-------+---------+---------+--------------" );  \
        ( echo "Config | Install |  Check  |   Package" );      \
        ( echo "-------+---------+---------+--------------" );  \
	  for d in $$argv; do			      \
            if test -e $$d.config ; then              \
               cfg='  ok  '                          ;\
            else                                      \
               cfg='  --  '                          ;\
            fi                                       ;\
            if test -e $$d.install ; then             \
               inst='  ok   '                        ;\
            else                                      \
               inst='  --   '                        ;\
            fi                                       ;\
            if test -e $$d.check ; then               \
               chck='  ok   '                        ;\
            else                                      \
               chck='  --   '                        ;\
            fi                                       ;\
            ( echo "$$cfg | $$inst | $$chck | $$d" ); \
	  done ;                                      \
          ( echo "-------+---------+---------+--------------" )


.PHONY: $(TARGETS) baselibs-config versions prelim

.NOTPARALLEL: baselibs-config

CMAKEFOUND := $(shell command -v cmake 2> /dev/null)

prelim: cmake-check echo-compilers baselibs-config versions

echo-compilers:
	$(warning Using $(FC) for Fortran compiler)
	$(warning Using $(MPIFC) for MPI Fortran compiler)
	$(warning Using $(ES_FC) for ESMF Fortran compiler)
	$(warning Using $(CC) for C compiler)
	$(warning Using $(MPICC) for MPI C compiler)
	$(warning Using $(ES_CC) for ESMF C compiler)
	$(warning Using $(CXX) for C++ compiler)
	$(warning Using $(MPICXX) for MPIC++ compiler)
	$(warning Using $(ES_CXX) for ESMF C++ compiler)

cmake-check:
ifndef CMAKEFOUND
	$(warning "cmake is not available. Please install cmake or add to path as pFUnit requires it.")
	$(warning "For now, we build without pFUnit")
	$(eval ALLDIRS := $(filter-out pFUnit,$(ALLDIRS)))
	$(warning ALLDIRS: $(ALLDIRS))
endif

baselibs-config: baselibs-config.mk
	@echo "ALLDIRS: $(ALLDIRS)"
  @echo "PREFIX: $(prefix)"
	@echo "Doing preliminaries"
	@mkdir -p $(prefix)/etc
	@cp VERSION $(prefix)/etc
	@cp ChangeLog $(prefix)/etc
	@cp CONTENTS $(prefix)/etc
	@$(if $(MODULECMD),$(MODULECMD) sh list -t >& $(prefix)/etc/MODULES,echo "Modules not found")
	@rm -f $(prefix)/etc/CONFIG
	@echo "CC: $(CC)" >> $(prefix)/etc/CONFIG
	@echo "CC --version: $(shell $(CC) --version 2>&1)" >> $(prefix)/etc/CONFIG
	@echo "CXX: $(CXX)" >> $(prefix)/etc/CONFIG
	@echo "CXX --version: $(shell $(CXX) --version 2>&1)" >> $(prefix)/etc/CONFIG
	@echo "FC: $(FC)" >> $(prefix)/etc/CONFIG
	@echo "FC --version: $(shell $(FC) $(FORTRAN_VERSION) 2>&1)" >> $(prefix)/etc/CONFIG
	@echo "F77: $(F77)" >> $(prefix)/etc/CONFIG
	@echo "F77 --version: $(shell $(F77) $(FORTRAN_VERSION) 2>&1)" >> $(prefix)/etc/CONFIG
	@echo "F90: $(F90)" >> $(prefix)/etc/CONFIG
	@echo "F90 --version: $(shell $(F90) $(FORTRAN_VERSION) 2>&1)" >> $(prefix)/etc/CONFIG
	@echo "" >> $(prefix)/etc/CONFIG
	@echo "MPICC: $(MPICC)" >> $(prefix)/etc/CONFIG
	@echo "MPICXX: $(MPICXX)" >> $(prefix)/etc/CONFIG
	@echo "MPIFC: $(MPIFC)" >> $(prefix)/etc/CONFIG
	@echo "" >> $(prefix)/etc/CONFIG
	@echo "ESMF_COMM: $(ESMF_COMM)" >> $(prefix)/etc/CONFIG
	@echo "ES_CC: $(ES_CC)" >> $(prefix)/etc/CONFIG
	@echo "ES_CXX: $(ES_CXX)" >> $(prefix)/etc/CONFIG
	@echo "ES_FC: $(ES_FC)" >> $(prefix)/etc/CONFIG
	@echo "" >> $(prefix)/etc/CONFIG
	@echo "CONFIG_SETUP: $(CONFIG_SETUP)" >> $(prefix)/etc/CONFIG
	@echo "SYSNAME: $(SYSNAME)" >> $(prefix)/etc/CONFIG
	@echo "ARCH: $(ARCH)" >> $(prefix)/etc/CONFIG
	@echo "SITE: $(SITE)" >> $(prefix)/etc/CONFIG
	@echo "MACH: $(MACH)" >> $(prefix)/etc/CONFIG
	@echo "Kernel: $(shell uname -r)" >> $(prefix)/etc/CONFIG
	@echo "Hostname: $(shell hostname)" >> $(prefix)/etc/CONFIG
	@echo "USER: $(USER)" >> $(prefix)/etc/CONFIG
	@echo "Making baselibs-config script"
	@sed -e "$$CONFIG_SEDFILE_BODY" baselibs-config.bsub > $(prefix)/etc/baselibs-config
	@chmod +x $(prefix)/etc/baselibs-config

versions: make_versions.sh
	$(shell ./make_versions.sh $(prefix))

$(TARGETS): prelim
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			      \
            ( echo Building $$t ... );                \
            ( $(MAKE) $$d.config    );                \
            ( $(MAKE) $$d.$$t       );                \
	  done

clean: 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			      \
	    ( $(MAKE) $$d.$$t ) \
	  done

distclean: 
	@/bin/rm -rf *.config *.install *.check
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			      \
	    ( $(MAKE) $$d.$$t ) \
	  done

check:
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			      \
            ( echo Checking $$d ... ); \
            ( $(MAKE) $$d.$$t       ); \
	  done

realclean:
	@$(MAKE) distclean
	@/bin/rm -rf $(prefix)

#                      ----------------
#                      Customized rules
#                      ----------------


#                         Config
#                         ......


jpeg.config: jpeg/configure
	@echo "Configuring jpeg"
	@(cd jpeg; \
	  export PATH="$(prefix)/bin:$(PATH)" ;\
	  export CPPFLAGS="$(INC_SUPP)";\
	  export LIBS="-lm";\
	  ./configure --prefix=$(prefix) \
          --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
		      --includedir=$(prefix)/include/$* \
		      --disable-f90 --disable-shared \
		      INSTALL="$(INSTALL_OPT)"  \
		      CC=$(CC) CXX=$(CXX) FC=$(FC) )
	@touch $@

hdf4.config: hdf4/README.txt
	@echo Configuring hdf4
	@(cd hdf4; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export LDFLAGS="-lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --program-suffix="-hdf4"\
                      --includedir=$(prefix)/include/hdf \
                      --with-jpeg=$(prefix)/include/jpeg,$(prefix)/lib \
                      --with-szlib=$(prefix)/include/szlib,$(prefix)/lib \
                      --with-zlib=$(prefix)/include/zlib,$(prefix)/lib \
                      --disable-netcdf \
                      CFLAGS="$(CFLAGS)" FFLAGS="$(NAG_FCFLAGS) $(NAG_DUSTY)" CC=$(CC) FC=$(FC) CXX=$(CXX) )
	touch $@

hdf5.config: hdf5/README.txt
	@echo Configuring hdf5
	@(cd hdf5; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export LDFLAGS="-lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/hdf5 \
                      --with-szlib=$(prefix)/include/szlib,$(prefix)/lib \
                      --with-zlib=$(prefix)/include/zlib,$(prefix)/lib \
                      --disable-shared --disable-cxx\
                      --enable-hl --enable-fortran --disable-sharedlib-rpath \
                      $(ENABLE_GPFS) $(H5_PARALLEL) --enable-fortran2003 \
                      CFLAGS="$(CFLAGS)" FCFLAGS="$(NAG_FCFLAGS)" CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	touch $@

LIB_CURL = $(shell $(prefix)/bin/curl-config --libs)	
netcdf.config : netcdf/configure
	@echo "Configuring netcdf $*"
	@(cd netcdf; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP)";\
          export CFLAGS="$(CFLAGS) $(NC_CFLAGS)";\
          export LIBS="-L$(prefix)/lib -lmfhdf -ldf -lsz -ljpeg $(LINK_GPFS) $(LIB_CURL) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/netcdf \
                      --enable-hdf4 \
                      --enable-dap \
                      $(NC_PAR_TESTS) \
                      --disable-shared \
                      --enable-netcdf-4 \
                      CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	@touch $@

LIB_NETCDF = $(shell $(prefix)/bin/nc-config --libs)
netcdf-fortran.config : netcdf-fortran/configure
	@echo "Configuring netcdf-fortran $*"
	@(cd netcdf-fortran; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) -I$(prefix)/include/netcdf $(INC_SUPP)";\
          export LIBS="-L$(prefix)/lib $(LIB_NETCDF) $(LIB_CURL)" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/netcdf \
                      $(NC_PAR_TESTS) \
                      --disable-shared \
                      --enable-f90 \
                      FFLAGS="$(FORTRAN_FPIC) $(NAG_FCFLAGS)" FCFLAGS="$(FORTRAN_FPIC) $(NAG_FCFLAGS)" \
                      CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	@touch $@

udunits2.config : udunits2/configure
	@echo "Configuring udunits2 $*"
	@(cd udunits2; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP)";\
          export LIBS="-L$(prefix)/lib -lmfhdf -ldf -lsz -ljpeg $(LINK_GPFS) $(LIB_CURL) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/udunits2 \
                      --disable-shared \
                      CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	@touch $@

cmor.config: cmor/configure
	@echo "Configuring cmor $*"
	@(cd cmor; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CFLAGS="-fPIC $(CPPFLAGS) $(INC_SUPP) -fPIC";\
          export LIBS="-L$(prefix)/lib -lmfhdf -ldf -lsz -ljpeg $(LINK_GPFS) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/cmor \
                      --enable-fortran \
                      --enable-color   \
                      --with-python=$(PYTHON) \
                      --with-szlib=$(prefix) \
                      --with-zlib=$(prefix)  \
                      --with-udunits2=$(prefix)  \
                      --with-netcdf=$(prefix) \
                      --with-hdf5=$(prefix) \
                      --with-uuid=$(prefix) \
                      --with-cdat=$(prefix) \
                      CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	@touch $@


INC_HDF5 = $(prefix)/include/hdf5
LIB_HDF5 = $(wildcard $(foreach lib, hdf5_hl hdf5 z sz curl,\
           $(prefix)/lib/lib$(lib).a) )
LIB_CURL = $(shell $(prefix)/bin/curl-config --libs)	
nco.config : nco/configure
	@echo "Configuring nco $*"
	@(cd nco; \
          export NETCDF_ROOT="$(prefix)/"; \
          export NETCDF_LIB="$(prefix)/lib"; \
          export NETCDF_INC="$(prefix)/include/netcdf"; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP) -I$(prefix)/include/netcdf";\
          export LIBS="-L$(prefix)/lib $(LIB_HDF5) -lmfhdf -ldf -lsz  -ljpeg $(LINK_GPFS) $(LIB_CURL) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/nco \
                      --disable-ncoxx \
                      --disable-ncap2 \
                      --disable-shared --enable-static \
                      --disable-nco_cplusplus \
                      --disable-mpi --disable-gsl \
                      $(NCO_OPENMP) \
                      --enable-netcdf4  \
                      CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77)  )
	@touch $@

INC_HDF5 = $(prefix)/include/hdf5
LIB_HDF5 = $(wildcard $(foreach lib, hdf5_hl hdf5 z sz curl,\
           $(prefix)/lib/lib$(lib).a) )
h5edit.config : h5edit/configure
	@echo "Configuring h5edit $*"
	@(cd h5edit; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP)";\
          export LIBS="-L$(prefix)/lib $(LIB_HDF5) -lmfhdf -ldf -lsz  -ljpeg $(LINK_GPFS) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/h5edit \
                      CC=$(H5_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77)  )
	@touch $@


szlib.config : szlib/configure
	@echo "Configuring szlib"
	@(cd szlib; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(INC_SUPP)";\
          export LIBS="-lm";\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/szlib \
                      --disable-f90 --disable-shared \
                      CCFLAGS="$(CFLAGS)" CC=$(CC) CXX=$(CXX) FC=$(FC) )
	@touch $@

zlib.config : zlib/configure
	@echo Configuring zlib
	@(cd zlib; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          ./configure --prefix=$(prefix) \
                      --includedir=$(prefix)/include/zlib \
                      --libdir=$(prefix)/lib )
	touch $@


curl.config : curl/configure
	@echo "Configuring curl"
	@(cd curl; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(INC_SUPP)";\
          export LIBS="-lm";\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/ \
                      --libdir=$(prefix)/lib \
                      --with-zlib=$(prefix) \
                      --disable-ldap \
                      --enable-manual \
                      --disable-shared \
                      --enable-static \
                      $(CURLSSL) \
                      --without-libidn \
                      CCFLAGS="$(CFLAGS)" CC=$(CC) CXX=$(CXX) FC=$(FC) )
	@touch $@

SDPToolkit.config: 
	@echo "Configuring SDPToolkit"
	@mkdir -p ./SDPToolkit/bin/linux64/
	@mkdir -p ./SDPToolkit/bin/linux/
	@cp ./SDPToolkit/bin/mkpgslib ./SDPToolkit/bin/linux64/
	@cp ./SDPToolkit/bin/mkpgslib ./SDPToolkit/bin/linux/
	@touch ./SDPToolkit/bin/linux64/pgs-env.csh
	@touch ./SDPToolkit/bin/linux/pgs-env.csh
	@touch $@

cdo.config: cdo/configure
	@echo "Configuring cdo $*"
	@(cd cdo; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP)";\
          export LIBS="-L$(prefix)/lib $(LIB_NETCDF) $(LIB_CURL) -lexpat -lmfhdf -ldf -lsz -ljpeg $(LINK_GPFS) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --build=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu \
                      --includedir=$(prefix)/include/cdo \
                      --with-szlib=$(prefix) \
                      --with-zlib=$(prefix) \
                      --with-hdf5=$(prefix) \
                      --with-netcdf=$(prefix) \
                      --with-udunits2=$(prefix) \
                      --disable-grib --disable-openmp \
                      --disable-shared --enable-static \
                      FCFLAGS="$(NAG_FCFLAGS)" CC=$(NC_CC) FC=$(NC_FC) CXX=$(NC_CXX) F77=$(NC_F77) )
	@touch $@

pFUnit.config: 
	@echo "Configuring pFUnit (Serial+OpenMP)"
	@mkdir -p ./pFUnit/build-serial
	@(cd ./pFUnit/build-serial; \
		cmake -DMPI=NO -DOPENMP=YES -DCMAKE_INSTALL_PREFIX=$(prefix)/pFUnit/pFUnit-serial .. )
ifneq ($(ESMF_COMM),mpiuni)
	@echo "Configuring pFUnit (MPI+OpenMP)"
	@mkdir -p ./pFUnit/build-mpi
	@(cd ./pFUnit/build-mpi; \
		cmake -DMPI=YES -DOPENMP=YES -DCMAKE_INSTALL_PREFIX=$(prefix)/pFUnit/pFUnit-mpi .. )
endif
	@touch $@

esmf.config : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) config

hdfeos.config : 
	@$(MAKE) -e -f hdfeos.mk config

hdfeos5.config : 
	@$(MAKE) -e -f hdfeos5.mk CFLAGS="$(CFLAGS) $(LINK_GPFS)" FFLAGS="$(NAG_FCFLAGS) $(NAG_DUSTY)" CC=$(NC_CC) CXX=$(NC_CXX) FC=$(NC_FC) F77=$(NC_F77) config /home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu 

#                         Install
#                         .......


hdf5.install: hdf5.config
	@(cd hdf5; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          "$(HDF5_DEEPBIND)" ;\
          $(MAKE) install)
	@touch $@

pFUnit.install: pFUnit.config
	@echo "Installing pFUnit (Serial+OpenMP)"
	@(cd ./pFUnit/build-serial; \
		$(MAKE) install )
ifneq ($(ESMF_COMM),mpiuni)
	@echo "Installing pFUnit (MPI+OpenMP)"
	@(cd ./pFUnit/build-mpi; \
		$(MAKE) install )
endif
	@touch $@

SDPToolkit.install: 
	@echo "Installing SDPToolkit"
	@mkdir -p $(prefix)/include/SDPToolkit
	@(cd SDPToolkit; \
          export OMPI_CC="`which $(CC)`" ;\
          export OMPI_FC="`which $(F90)`" ;\
          ./SDPToolkit.mkdir.py; \
          echo y | bin/INSTALL        -batch \
                   $(PGI_FLAG)   \
                   -fc_path      `which $(NC_FC)`  \
                   -cc_path      `which $(NC_CC)`  \
                   -pgshome      `pwd` \
                   -hdfhome      $(prefix) \
                   -hdf5home     $(prefix) \
                   -hdfeos_home  $(prefix) \
                   -hdfeos5_home $(prefix) \
                   -sziphome     $(prefix) \
                   -zlibhome     $(prefix) \
                   -jpeghome     $(prefix) \
                   -netcdfhome   $(prefix) )
	@test -d ./SDPToolkit/bin/linux    || cp ./SDPToolkit/bin/linux/* $(prefix)/bin
	@test -d ./SDPToolkit/bin/linux    || cp ./SDPToolkit/lib/linux/* $(prefix)/lib
	@test -d ./SDPToolkit/bin/linux64  || cp ./SDPToolkit/bin/linux64/* $(prefix)/bin
	@test -d ./SDPToolkit/bin/linux64  || cp ./SDPToolkit/lib/linux64/* $(prefix)/lib
	@cp -r ./SDPToolkit/include/* $(prefix)/include/SDPToolkit/
	@touch $@

# MAT: Note that on Mac machines there seems to be an issue with the libtool setup
#      in nco. If you just run nco, it never makes the libnco.a library, or at least
#      does not make it correctly. As the nco/m4/libtool.m4 and, say, the 
#      netcdf/m4/libtool.m4 files are nearly the same, this seems to be an issue in
#      NCO. Until NCO can solve this issue the solution is a three-part run:
#
#      1. Make install
#      2. After it crashes, go into nco/src/nco and run:
#         $ ar r libnco.a *.o
#         $ mv libnco.a .libs/
#      3. Run make install again to build the rest of nco
#
#      Also, on Macs, the makeinfo installed is too old to build the info files
#      for NCO. As we do not really need them, we remove the doc from the Makefile

ifeq ($(ARCH),Darwin)
nco.install: nco.config
	@echo "Installing nco with hack for Darwin"
	@(cd nco; \
          export NETCDF_ROOT="$(prefix)/"; \
          export NETCDF_LIB="$(prefix)/lib"; \
          export NETCDF_INC="$(prefix)/include/netcdf"; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP) -I$(prefix)/include/netcdf";\
          export LIBS="-L$(prefix)/lib $(LIB_HDF5) -lmfhdf -ldf -lsz  -ljpeg $(LINK_GPFS) $(LIB_CURL) -ldl -lm" ;\
	  sed -i '' '/^SUBDIRS/s/doc//' Makefile ;\
	  $(MAKE) install ;\
	  cd src/nco ;\
	  ar r libnco.a *.o ;\
	  mv libnco.a .libs/ ;\
	  cd ../.. ;\
	  $(MAKE) install )
	@touch $@
else
nco.install: nco.config
	@echo "Installing nco"
	@(cd nco; \
          export NETCDF_ROOT="$(prefix)/"; \
          export NETCDF_LIB="$(prefix)/lib"; \
          export NETCDF_INC="$(prefix)/include/netcdf"; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(CPPFLAGS) $(INC_SUPP) -I$(prefix)/include/netcdf";\
          export LIBS="-L$(prefix)/lib $(LIB_HDF5) -lmfhdf -ldf -lsz  -ljpeg $(LINK_GPFS) $(LIB_CURL) -ldl -lm" ;\
	  $(MAKE) install )
	@touch $@
endif

jpeg.install: jpeg.config
	@echo "Installing jpeg"
	@mkdir -p $(prefix)/bin $(prefix)/lib $(prefix)/include/jpeg
	@(cd jpeg; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) -e install-lib includedir=$(prefix)/include/jpeg)
	touch $@

#      Need to call make all before make install for uuid
#      ...................................................

uuid.install: uuid.config
	@echo "Installing uuid"
	@mkdir -p $(prefix)/bin $(prefix)/lib $(prefix)/include/uuid
	@(cd uuid; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) all; \
          $(MAKE) -e install)
	touch $@

esmf.install : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) install

esmf.apps : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) apps

esmf.examples : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) examples

esmf.python : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) python

esmf.pythoncheck : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) pythoncheck

hdfeos.install : 
	@$(MAKE) -e -f hdfeos.mk install

hdfeos5.install : 
	@$(MAKE) -e -f hdfeos5.mk CFLAGS="$(CFLAGS) $(LINK_GPFS)" CC=$(NC_CC) CXX=$(NC_CXX) FC=$(NC_FC) F77=$(NC_F77) install 

#                          Clean
#                          .....

hdfeos.clean : 
	@$(MAKE) -e -f hdfeos.mk clean

hdfeos.distclean : 
	@$(MAKE) -e -f hdfeos.mk distclean

hdfeos5.clean : 
	@$(MAKE) -e -f hdfeos5.mk CFLAGS="$(CFLAGS) $(LINK_GPFS)" CC=$(NC_CC) CXX=$(NC_CXX) FC=$(NC_FC) F77=$(NC_F77) clean 

hdfeos5.distclean : 
	@$(MAKE) -e -f hdfeos5.mk CFLAGS="$(CFLAGS) $(LINK_GPFS)" CC=$(NC_CC) CXX=$(NC_CXX) FC=$(NC_FC) F77=$(NC_F77) distclean 

esmf.clean : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) clean

esmf.distclean : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) distclean

SDPToolkit.clean:
	@echo "Cleaning SDPToolkit"
	@rm -rf ./SDPToolkit/lib
	@rm -rf ./SDPToolkit/obj
	@rm -rf ./SDPToolkit/objcpp
	@rm -rf ./SDPToolkit/test

SDPToolkit.distclean:
	@echo "Cleaning SDPToolkit"
	@rm -rf ./SDPToolkit/lib
	@rm -rf ./SDPToolkit/obj
	@rm -rf ./SDPToolkit/objcpp
	@rm -rf ./SDPToolkit/test

pFUnit.clean: 
	@echo "Cleaning pFUnit"
	@rm -rf ./pFUnit/build-serial
	@rm -rf ./pFUnit/build-mpi

pFUnit.distclean: 
	@echo "Cleaning pFUnit"
	@rm -rf ./pFUnit/build-serial
	@rm -rf ./pFUnit/build-mpi

#                          Check
#                          .....

esmf.check : esmf_rules.mk
	@$(MAKE) -e -f esmf_rules.mk  CFLAGS="$(CFLAGS)" CC=$(ES_CC) CXX=$(ES_CXX) FC=$(ES_FC) PYTHON=$(PYTHON) ESMF_INSTALL_PREFIX=$(prefix) check

curl.check: curl.install
	@echo "Checking curl"
	@echo "We explicitly do not check cURL due to how long it takes"

cmor.check: cmor.install
	@echo "Checking cmor"
	@(cd cmor; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CFLAGS="-fPIC $(CPPFLAGS) $(INC_SUPP) -fPIC";\
          export LIBS="-L$(prefix)/lib -lmfhdf -ldf -lsz -ljpeg $(LINK_GPFS) -ldl -lm" ;\
			 $(MAKE) test)
	@touch $@

SDPToolkit.check: SDPToolkit.install
	@echo "Checking SDPToolkit"
	@echo "Nothing to do for SDPToolkit"

pFUnit.check: pFUnit.install
	@echo "Checking pFUnit (Serial+OpenMP)"
	@(cd ./pFUnit/build-serial; \
		$(MAKE) tests )
ifneq ($(ESMF_COMM),mpiuni)
	@echo "Checking pFUnit (MPI+OpenMP)"
	@(cd ./pFUnit/build-mpi; \
		$(MAKE) tests )
endif
	@touch $@

hdfeos5.check: hdfeos5.install
	@echo "Checking hdfeos5"
	@echo "We explicitly do not check hdfeos5 at this moment due to side-effects"

#                        -------------
#                        Generic rules
#                        -------------

%.config : %/configure
	@echo "Configuring generic $*"
	@(cd $*; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          export CPPFLAGS="$(INC_SUPP)";\
	  export LIBS="-lm";\
          ./configure --prefix=$(prefix) \
                      --includedir=$(prefix)/include/$* \
                      --disable-f90 --disable-shared --enable-static \
                      CC=$(CC) CXX=$(CXX) FC=$(F90) )
	@touch $@

%.all: %.config
	@(cd $*; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) all)

%.install: %.config
	@(cd $*; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) install)
	@touch $@

%.check: %.install
	@(cd $*; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) check)
	@touch $@

%.clean: %
	@(cd $<; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) clean)

%.distclean: %
	@(cd $<; \
          export PATH="$(prefix)/bin:$(PATH)" ;\
          $(MAKE) distclean)


#                        ------------
#                        Dependencies
#                        ------------

#------------------------------------------------------------------------

