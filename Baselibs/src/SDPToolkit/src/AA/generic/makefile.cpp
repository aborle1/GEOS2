#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for AA tools 
#		(for 2 and 3 Dread and GEO tools and PeV only)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CPP CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#  
# author: Mike Sucher / Graham Bland / Guru Tej Khalsa / Megan E.D.Spencer
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target object files are built to $(PGSOBJ)/AA
# 	4) To build the PGS AA tools library, type 'make lib'
#	   (This runs the script $PGSBIN/mkpgslib to build the library).

#----------------------------------------------------------------------------

#
# Set the name of this tool group
#

GRP= AA

# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# define C preprocessor symbols 
DFLAGS  = -D$(BRAND) -DSUNCC -DPROTO


# path for #include directive
IFLAG   = -I. -I$(PGSINC) -I$(PGSINC)/FF

# Name of the remove utility and flags
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp

# object files from this sub-group needed to build library

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/PGS_AA_2DRead.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_2DReadF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3DRead.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3DReadF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_2Dgeo.o \
        $(PGSCPPO)/$(GRP)/PGS_AA_2DgeoF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3Dgeo.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3DgeoF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_2DReadGrid.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_2DReadGridF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3DReadGrid.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_3DReadGridF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_GEOGrid.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_GEOGridF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_PeV.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_PeVA.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_Map.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_Operation.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_FF_Setup.o  \
	$(PGSCPPO)/$(GRP)/PGS_AA_FF.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_dem.o \
	$(PGSCPPO)/$(GRP)/PGS_AA_demF.o 

#
# targets
#

all: message
	@case "$(BRAND)" in \
	    dec)  dflags="-DDEC_ALPHA $(DFLAGS)" ;; \
	    hp) dflags="-DHP9000 $(DFLAGS)" ;; \
	    ibm) dflags="-DIBM6000 $(DFLAGS)" ;; \
	    linux) dflags="-DLINUX $(DFLAGS)" ;; \
	    sco) dflags="-DLINUX $(DFLAGS)" ;; \
	    sgi) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi32) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi64) dflags="-DIRIX $(DFLAGS)" ;; \
	    sun4) dflags="-DSUN $(DFLAGS)" ;; \
	    sun5) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.8) dflags="-DSUN -DSUNCC -DPROTO" ;; \
            sun5.9) dflags="-DSUN -DSUNCC -DPROTO" ;; \
            sun5.10) dflags="-DSUN -DSUNCC -DPROTO" ;; \
	    *) dflags="$(DFLAGS)" ;; \
	esac ; \
	$(MAKE) $(MFLAGS) $(MFLAGS2) all2 DFLAGS="$$dflags"

all2: $(LIBOFILES)

lib:
	@${PGSBIN}/mkpgslib.cpp $(GRP)

message:
	@echo "    Makefile for AA/generic; Target: all"

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(LIBOFILES)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/PGS_AA_2DRead.o: PGS_AA_2DRead.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_2DReadF.o: PGS_AA_2DReadF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3DRead.o: PGS_AA_3DRead.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3DReadF.o: PGS_AA_3DReadF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_2Dgeo.o: PGS_AA_2Dgeo.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_2DgeoF.o: PGS_AA_2DgeoF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3Dgeo.o: PGS_AA_3Dgeo.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3DgeoF.o: PGS_AA_3DgeoF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_2DReadGrid.o: PGS_AA_2DReadGrid.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_2DReadGridF.o: PGS_AA_2DReadGridF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3DReadGrid.o: PGS_AA_3DReadGrid.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_3DReadGridF.o: PGS_AA_3DReadGridF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_GEOGrid.o: PGS_AA_GEOGrid.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_GEOGridF.o: PGS_AA_GEOGridF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_PeV.o: PGS_AA_PeV.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_PeVA.o: PGS_AA_PeVA.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_Map.o: PGS_AA_Map.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_Operation.o: PGS_AA_Operation.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_FF_Setup.o: PGS_AA_FF_Setup.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_FF.o: PGS_AA_FF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_dem.o: PGS_AA_dem.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_AA_demF.o: PGS_AA_demF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# compile C to object
.c.o:
	@case "$(BRAND)" in \
	    dec)  dflags="-DDEC_ALPHA $(DFLAGS)" ;; \
	    hp) dflags="-DHP9000 $(DFLAGS)" ;; \
	    ibm) dflags="-DIBM6000 $(DFLAGS)" ;; \
	    linux) dflags="-DLINUX $(DFLAGS)" ;; \
	    sco) dflags="-DLINUX $(DFLAGS)" ;; \
	    sgi) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi32) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi64) dflags="-DIRIX $(DFLAGS)" ;; \
	    sun4) dflags="-DSUN $(DFLAGS)" ;; \
	    sun5) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.8) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.9) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.10) dflags="-DSUN $(DFLAGS)" ;; \
	    *) dflags="$(DFLAGS)" ;; \
	esac ; \
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@ DFLAGS="$$dflags"
