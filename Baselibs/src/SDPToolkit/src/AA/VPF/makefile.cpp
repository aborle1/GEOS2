#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# file:		makefile for AA tools 
#		(for VPF only)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#  
# author: Mike Sucher / Richard Morris / Guru Tej Khalsa / MeganE.D.Spencer
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
DFLAGS  = -D$(HDFSYS) -DSUNCC  -DFPROTO -D_INCLUDE_POSIX_SOURCE

# path for #include directive
IFLAG   = -I$(PGSINC) -I$(PGSINC)/DCW 

# Name of the remove utility and flags
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp
# object files from this sub-group needed to build library  

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/coorgeom.o \
	$(PGSCPPO)/$(GRP)/linklist.o \
	$(PGSCPPO)/$(GRP)/set.o \
	$(PGSCPPO)/$(GRP)/strfunc.o \
	$(PGSCPPO)/$(GRP)/vpfprim2.o \
	$(PGSCPPO)/$(GRP)/vpfquery.o \
	$(PGSCPPO)/$(GRP)/vpfrelat.o \
	$(PGSCPPO)/$(GRP)/vpftable.o \
	$(PGSCPPO)/$(GRP)/vpftidx.o \
	$(PGSCPPO)/$(GRP)/vpfwrite.o \
	$(PGSCPPO)/$(GRP)/vpfspx.o \
	$(PGSCPPO)/$(GRP)/vpfsprel.o \
	$(PGSCPPO)/$(GRP)/vpfread.o \
	$(PGSCPPO)/$(GRP)/vvmisc.o 

#
# targets
#

all: message $(LIBOFILES)

lib:
	@${PGSBIN}/mkpgslib.cpp $(GRP)

message:
	@echo "    Makefile for AA/VPF; Target: all"

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(LIBOFILES)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/coorgeom.o: coorgeom.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/linklist.o: linklist.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/set.o: set.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/strfunc.o: strfunc.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfprim2.o: vpfprim2.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfquery.o: vpfquery.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfrelat.o: vpfrelat.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpftable.o: vpftable.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpftidx.o: vpftidx.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfwrite.o: vpfwrite.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfspx.o: vpfspx.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfsprel.o: vpfsprel.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vpfread.o: vpfread.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vvmisc.o: vvmisc.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# compile C to object
.c.o:
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
