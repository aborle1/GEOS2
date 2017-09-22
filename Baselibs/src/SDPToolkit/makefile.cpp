#----------------------------------------------------------------------------
# file:         makefile for SDP Toolkit (for internal ECS usage only)
#
# environment:  requires environment established by the ECS .buildrc file
# 
# environment variables dependencies:
#   ARCH     architecture (i.e. hardware/os type)
#
#
# author:  Guru Tej S. Khalsa / Space Applications Corp.
#
# history:
#       10-Oct-1997 GTSK Initital version
#       06-Mar-1998 GTSK Added ability to build "flavors" of Toolkits
#
# notes:
#       1) This file is only for use by ECS Configuration Management (CM)
#	   personnel and software.  This file should not be distributed
#	   as part of the standard SDP Toolkit delivery.
#       2) The Toolkit can be build in many different configurations (even
#          for a single architecture).  These include any combination of
#          SCF/DAAC, f77/f90 and debug/optimize (8 possible configurations!).
#          A given configuration is referred to as a "flavor" (e.g. the SCF,
#          f77, debug version of the Toolkit is one flavor).
#
#----------------------------------------------------------------------------

# define shell to be used (use ksh so we can use the "." command)

SHELL=/bin/ksh

NOWINKINOPT = -V
MFLAGS2=-f makefile.cpp
# define the location of the TOOLKIT STAGE area

TK_STAGE=/ecs/formal/STAGE/TOOLKIT

# define the location of the standard Toolkit development area

TK_HOME=/ecs/formal/TOOLKIT

#define HDF version

HDF_VER=HDF4.2r3

# define the default "flavor" of the Toolkit to build

FLAVOR=

# define a list of ALL flavors

ALL_FLAVORS=		\
	scf_f77 	\
	scf_f90 	\
	scf_f77_debug 	\
	scf_f90_debug 	\
	daac_f77 	\
	daac_f90 	\
	daac_f77_debug 	\
	daac_f90_debug

# define default target

default:
	@echo "This file is for use by ECS Configuration Management processes only."

# define a target to build ALL variations (or "flavors") of the Toolkit
# for a given "brand" (i.e. architecture type)

full: $(ALL_FLAVORS)

# define a target to build the DAAC variations (or "flavors") of the Toolkit
# for a given "brand" (i.e. architecture type)

daac: daac_f77 daac_f77_debug daac_f90 daac_f90_debug

# define a target to build the SCF variations (or "flavors") of the Toolkit
# for a given "brand" (i.e. architecture type)

scf: scf_f77 scf_f77_debug scf_f90 scf_f90_debug

# define a target to build the FORTRAN 77  variations (or "flavors") of the
# Toolkit for a given "brand" (i.e. architecture type)

f77: scf_f77 scf_f77_debug daac_f77 daac_f77_debug

# define a target to build the FORTRAN 90  variations (or "flavors") of the
# Toolkit for a given "brand" (i.e. architecture type)

f90: scf_f90 scf_f90_debug daac_f90 daac_f90_debug

# define a target to build the "debug" variations (or "flavors") of the Toolkit
# for a given "brand" (i.e. architecture type) ("debug" means these libraries
# will be compiled for use in a souce code debugger)

debug: scf_f77_debug scf_f90_debug daac_f77_debug daac_f90_debug

# define a target to build the "optimized" variations (or "flavors") of the
# Toolkit for a given "brand" (i.e. architecture type) ("optimized" means these
# libraries will be optimized for execution speed by the compiler)

opt: scf_f77 scf_f90 daac_f77 daac_f90

# define a target to build the "optimized", SCF, FORTRAN 77 version of the
# Toolkit library

scf_f77:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT)  flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "optimized", SCF, FORTRAN 90 version of the
# Toolkit library

scf_f90:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "debug", SCF, FORTRAN 77 version of the
# Toolkit library

scf_f77_debug:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "debug", SCF, FORTRAN 90 version of the
# Toolkit library

scf_f90_debug:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "optimized", DAAC, FORTRAN 77 version of the
# Toolkit library

daac_f77:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "optimized", DAAC, FORTRAN 90 version of the
# Toolkit library

daac_f90:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "debug", DAAC, FORTRAN 77 version of the
# Toolkit library

daac_f77_debug:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@

# define a target to build the "debug", DAAC, FORTRAN 90 version of the
# Toolkit library

daac_f90_debug:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) flavor_dir FLAVOR=$@
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET) FLAVOR=_$@


# define standard CM targets

# NOTE: The standard targets defined by ECS CM are:
#       BuildImake, ProductHs, ProductLibs, LastPass, Deliver
#
#       Here the above targets are shadowed by the targets:
#       BuildImake2, ProductHs2, ProductLibs2, LastPass2, Deliver2
#       respectively.  The original targets only call the target
#       TK_ENV which sets up the Toolkit environment and then
#       builds the shadow target.

BuildImake:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) TK_ENV TARGET=$@2

BuildImake2:
	@echo "No Imakefile to build."

ProductHs:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) TK_ENV TARGET=$@2

ProductHs2:
	@cd $(TK_HOME)/src; $(MAKE) $(MFLAGS2) $(NOWINKINOPT) smfcompile; echo ""
	@cd $(TK_HOME)/message; $(MAKE) $(NOWINKINOPT) -f makefile.CM.cpp all

ProductLibs:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) TK_ENV TARGET=$@2

ProductLibs2:
	@if [ ! -e "$(PGSHOME)/lib/$(ARCH)" ] ; then	\
		cd $(PGSHOME)/lib;			\
		ln -s $(BRAND) $(ARCH);			\
	fi
	@cd $(TK_HOME)/src; $(MAKE) $(MFLAGS2) $(NOWINKINOPT) all
	@cd $(TK_HOME)/src/IO; $(MAKE) $(MFLAGS2) $(NOWINKINOPT) utilities

LastPass:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) TK_ENV TARGET=$@2

LastPass2:
	@if [ ! -e "$(PGSHOME)/bin/$(ARCH)" ] ; then	\
		cd $(PGSHOME)/bin;			\
		ln -s $(BRAND) $(ARCH);			\
	fi
	@cd $(TK_HOME)/src; $(MAKE) $(MFLAGS2) $(NOWINKINOPT) utilities

Deliver:
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) TK_ENV TARGET=$@2

Deliver2:
	@regexp="#define[ \t]+PGSd_TOOLKIT_MAJOR_VERSION";		       \
	version=`egrep "$$regexp" $(PGSHOME)/include/PGS_SMF.h`;	       \
	version=`echo $$version | cut -d'"' -f2`;			       \
	version=`echo $$version | cut -d'K' -f2`; 			       \
	label=SDPTK$${version}v1.00;					       \
	options="PGSTK -vob $(TK_HOME) -ws $(TK_HOME)/install -label $$label"; \
	$(TK_HOME)/bin/$(BRAND)/mkpgstar $$options

DaacDeliver:
	@if [ "$(ARCH)" = "sgi6n32" ] ; then				    \
		for arch in sgi sgi6n32 sgi6 ; do			    \
			for targ in ProductHs ProductLibs LastPass ; do	    \
				$(MAKE) $(MFLAGS2) $(NOWINKINOPT) full TARGET=$$targ ARCH=$$arch;  \
			done;						    \
		done;							    \
	else								    \
		for targ in ProductHs ProductLibs LastPass ; do		    \
			$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $$targ;				    \
		done;							    \
	fi

DaacDeliverF77:
	@if [ "$(ARCH)" = "sgi6n32" ] ; then				    \
		for arch in sgi sgi6n32 sgi6 ; do			    \
			for targ in ProductHs ProductLibs LastPass ; do	    \
				$(MAKE) $(MFLAGS2) $(NOWINKINOPT) f77 TARGET=$$targ ARCH=$$arch;   \
			done;						    \
		done;							    \
	else								    \
		for targ in ProductHs ProductLibs LastPass ; do		    \
			$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $$targ;				    \
		done;							    \
	fi

DaacDeliverF90:
	@if [ "$(ARCH)" = "sgi6n32" ] ; then				    \
		for arch in sgi sgi6n32 sgi6 ; do			    \
			for targ in ProductHs ProductLibs LastPass ; do	    \
				$(MAKE) $(MFLAGS2) $(NOWINKINOPT) f90 TARGET=$$targ ARCH=$$arch;   \
			done;						    \
		done;							    \
	else								    \
		for targ in ProductHs ProductLibs LastPass ; do		    \
			$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $$targ;				    \
		done;							    \
	fi

# this is a special target used to establish the Toolkit environment

TK_ENV:
	@case $(ARCH) in					\
		hp10)    brand=hp;      ;;			\
		sgi6n32) brand=sgi32;   ;;			\
		sgi6)    brand=sgi64;   ;;			\
		sun5.5)  brand=sun5;    ;;			\
		sun5.8)  brand=sun8;    ;;			\
		sun5.9)  brand=sun9;    ;;			\
		sun5.10)  brand=sun10;  ;;			\
		*)       brand=$(ARCH); ;;			\
	esac;							\
	. $(TK_HOME)/bin/$${brand}$(FLAVOR)/pgs-dev-env.ksh.cpp;	\
	export PGSOBJ PGSCPPO PGSLIB PGSBIN PATH;			\
	$(MAKE) $(MFLAGS2) $(NOWINKINOPT) $(TARGET)

flavor_dir:
	@sgi_mode=sgi;						\
	f90_comp=/usr/bin/f90;					\
	nag_flag=0;						\
	daac_flag=0;						\
	min_ver=`echo $(FLAVOR) | cut -f1 -d_`;			\
	if [ "$$min_ver" = "daac" ] ; then			\
		daac_flag=1;					\
	fi;							\
	opt_flag=-O;						\
	case $(FLAVOR) in					\
		*debug) opt_flag=-g; ;;				\
	esac;
	rlib_flag=0						\
	case $(ARCH) in						\
		hp10)						\
			brand=hp;				\
		;;						\
		sgi6n32)					\
			brand=sgi32;				\
			sgi_mode=n32;				\
			f90_comp="/usr/bin/f90 -n32";		\
		;;						\
		sgi6)						\
			brand=sgi64;				\
			sgi_mode=64;				\
			f90_comp="/usr/bin/f90 -64";		\
		;;						\
		sgi)						\
			brand=sgi;				\
			nag_flag=1;				\
			f90_comp=/usr/local/bin/f90;		\
		;;						\
		sun5.5)						\
			brand=sun5;				\
			rlib_flag=0				\
		;;						\
		sun5.8)						\
			brand=sun5.8;				\
			rlib_flag=0				\
		;;						\
		sun5.9)						\
			brand=sun5.9;				\
			rlib_flag=0				\
		;;						\
		sun5.10)			       		\
		        brand=sun5.10;		       		\
			rlib_flag=0				\
		;;						\
		*)						\
			brand=$(ARCH);				\
		;;						\
	esac;							\
	ftn_mode=`echo $(FLAVOR) | cut -f2 -d_`;		\
	if [ "$$ftn_mode" != "f90" ] ; then			\
		f90_comp="";					\
	fi;							\
	pgsobj=$(TK_HOME)/obj/$${brand}_$(FLAVOR);		\
	pgscppo=$(TK_HOME)/objcpp/$${brand}_$(FLAVOR);		\
	pgslib=$(TK_HOME)/lib/$${brand}_$(FLAVOR);		\
	pgsbin=$(TK_HOME)/bin/$${brand}_$(FLAVOR);		\
	if [ ! -d "$$pgscppo" ] ; then				\
		mkdir $$pgscppo;				\
		cd $(TK_HOME)/objcpp/sun5;			\
		tar -cf - . |	( cd $$pgscppo ; tar xf - );	\
	fi;							\
	if [ ! -d "$$pgsobj" ] ; then				\
		mkdir $$pgsobj;					\
		cd $(TK_HOME)/obj/sun5;				\
		tar -cf - . |	( cd $$pgsobj ; tar xf - );	\
	fi;							\
	if [ ! -d "$$pgslib" ] ; then				\
		mkdir $$pgslib;					\
	fi;							\
	if [ ! -d "$$pgsbin" ] ; then				\
		mkdir $$pgsbin;					\
	fi;							\
	cd $$pgsbin;						\
	if [ ! -f "mkpgslib.cpp" ] ; then		                    \
       	ln -s ../sun5/mkpgslib.cpp mkpgslib.cpp;			\
	fi;							\
       	hdfhome=/ecs/cots/hdf/$$brand/$(HDF_VER);		\
	hdfeoshome=/ecs/hdfeos;					\
       	tmpbin=$(TK_HOME)/bin/sun5/tmp;				\
	if [ ! -f "pgs-dev-env.csh" ] ; then			\
		echo "setenv PGSHOME $(TK_HOME)" > pgs-dev-env.csh;	    \
		echo "setenv HDFHOME $$hdfhome" >> pgs-dev-env.csh;	    \
		echo "setenv HDFEOS_HOME $$hdfeoshome" >> pgs-dev-env.csh;  \
		echo "set sgi_mode=$$sgi_mode" >> pgs-dev-env.csh;	    \
		echo "set pgs_daac=$$daac_flag" >> pgs-dev-env.csh;	    \
		echo 'set pgs_f90_comp="'$$f90_comp'"' >>  pgs-dev-env.csh; \
		echo "set pgs_nag_flag=$$nag_flag" >> pgs-dev-env.csh;	    \
		echo "set use_flavor=1" >> pgs-dev-env.csh;		    \
		echo "set flavor=$(FLAVOR)" >> pgs-dev-env.csh;		    \
		sed "s^OPT_LVL^$$opt_flag^" $$tmpbin/pgs-dev-env.csh.tmp    \
		>> pgs-dev-env.csh;					    \
	fi;			    			                    \
	if [ ! -f "pgs-dev-env.ksh" ] ; then		                    \
		echo "PGSHOME=$(TK_HOME)" > pgs-dev-env.ksh;		    \
		echo "HDFHOME=$$hdfhome" >> pgs-dev-env.ksh;		    \
		echo "HDFEOS_HOME=$$hdfeoshome" >> pgs-dev-env.ksh;	    \
		echo "sgi_mode=$$sgi_mode" >> pgs-dev-env.ksh;		    \
		echo "pgs_daac=$$daac_flag" >> pgs-dev-env.ksh;		    \
		echo 'pgs_f90_comp="'$$f90_comp'"' >>  pgs-dev-env.ksh;	    \
		echo "pgs_nag_flag=$$nag_flag" >> pgs-dev-env.ksh;	    \
		echo "use_flavor=1" >> pgs-dev-env.ksh;			    \
		echo "flavor=$(FLAVOR)" >> pgs-dev-env.ksh;		    \
		sed "s^OPT_LVL^$$opt_flag^" $$tmpbin/pgs-dev-env.ksh.tmp    \
		>> pgs-dev-env.ksh;					    \
	fi;			    			                    \
	if [ ! -f "pgs-env.csh" ] ; then		                    \
		echo "setenv PGSHOME $(TK_HOME)" > pgs-env.csh;		    \
		echo "set sgi_mode=$$sgi_mode" >> pgs-env.csh;		    \
		echo "set use_flavor=1" >> pgs-env.csh;			    \
		echo "set flavor=$(FLAVOR)" >> pgs-env.csh;		    \
		cat $$tmpbin/pgs-env.csh.tmp >> pgs-env.csh;		    \
	fi;			    			                    \
	if [ ! -f "pgs-env.ksh" ] ; then		                    \
		echo "PGSHOME=$(TK_HOME)" > pgs-env.ksh;		    \
		echo "sgi_mode=$$sgi_mode" >> pgs-env.ksh;		    \
		echo "use_flavor=1" >> pgs-env.ksh;			    \
		echo "flavor=$(FLAVOR)" >> pgs-env.ksh;			    \
		cat $$tmpbin/pgs-env.ksh.tmp >> pgs-env.ksh;		    \
	fi;			    			                    \
	if [ ! -f "mkpgslib.cpp" ] ; then		                    \
		ln -s ../sun5/mkpgslib.cpp mkpgslib.cpp;		    \
	fi;			    			                    \
	if [ ! -f "pgs-dev-env.csh.cpp" ] ; then		                    \
                echo "setenv PGSHOME $(TK_HOME)" > pgs-dev-env.csh.cpp;	    \
		echo "setenv HDFHOME $$hdfhome" >> pgs-dev-env.csh.cpp;	    \
		echo "setenv HDFEOS_HOME $$hdfeosome" >> pgs-dev-env.csh.cpp;       \
		echo "set sgi_mode=$$sgi_mode" >> pgs-dev-env.csh.cpp;	            \
		echo "set pgs_daac=$$daac_flag" >> pgs-dev-env.csh.cpp;	            \
		echo 'set pgs_f90_comp="'$$f90_comp'"' >>  pgs-dev-env.csh.cpp;     \
		echo "set pgs_nag_flag=$$nag_flag" >> pgs-dev-env.csh.cpp;	    \
		echo "set use_flavor=1" >> pgs-dev-env.csh.cpp;		            \
		echo "set flavor=$(FLAVOR)" >> pgs-dev-env.csh.cpp;		    \
		echo "set pgs_c_rlib=$rlib_flag" >> pgs-dev-env.csh.cpp;		    \
		sed "s^OPT_LVL^$$opt_flag^" $$tmpbin/pgs-dev-env.csh.cpp.tmp        \
		>> pgs-dev-env.csh.cpp;					            \
	fi;			    			                    \
	if [ ! -f "pgs-dev-env.ksh.cpp" ] ; then		                    \
		echo "PGSHOME=$(TK_HOME)" > pgs-dev-env.ksh.cpp;		    \
		echo "HDFHOME=$$hdfhome" >> pgs-dev-env.ksh.cpp;		    \
		echo "HDFEOS_HOME=$$hdfeoshome" >> pgs-dev-env.ksh.cpp;	            \
		echo "sgi_mode=$$sgi_mode" >> pgs-dev-env.ksh.cpp;		    \
		echo "pgs_daac=$$daac_flag" >> pgs-dev-env.ksh.cpp;		    \
		echo 'pgs_f90_comp="'$$f90_comp'"' >>  pgs-dev-env.ksh.cpp;	    \
		echo "pgs_nag_flag=$$nag_flag" >> pgs-dev-env.ksh.cpp;	            \
		echo "use_flavor=1" >> pgs-dev-env.ksh.cpp;			    \
		echo "flavor=$(FLAVOR)" >> pgs-dev-env.ksh.cpp;		            \
		echo "pgs_c_rlib=$rlib_flag" >> pgs-dev-env.csh.cpp;		    \
		sed "s^OPT_LVL^$$opt_flag^" $$tmpbin/pgs-dev-env.ksh.cpp.tmp        \
		>> pgs-dev-env.ksh.cpp;	                                            \
	fi;			    			                    \
	if [ ! -f "pgs-env.csh.cpp" ] ; then		                    \
		echo "setenv PGSHOME $(TK_HOME)" > pgs-env.csh.cpp;		    \
		echo "set sgi_mode=$$sgi_mode" >> pgs-env.csh.cpp;		    \
		echo "set use_flavor=1" >> pgs-env.csh.cpp;			    \
		echo "set flavor=$(FLAVOR)" >> pgs-env.csh.cpp;		            \
		cat $$tmpbin/pgs-env.csh.cpp.tmp >> pgs-env.csh.cpp;		    \
	fi;			    			                    \
	if [ ! -f "pgs-env.ksh.cpp" ] ; then		                    \
		echo "PGSHOME=$(TK_HOME)" > pgs-env.ksh.cpp;		            \
		echo "sgi_mode=$$sgi_mode" >> pgs-env.ksh.cpp;		            \
		echo "use_flavor=1" >> pgs-env.ksh.cpp;			            \
		echo "flavor=$(FLAVOR)" >> pgs-env.ksh.cpp;			    \
		cat $$tmpbin/pgs-env.ksh.cpp.tmp >> pgs-env.ksh.cpp;		            \
	fi;
