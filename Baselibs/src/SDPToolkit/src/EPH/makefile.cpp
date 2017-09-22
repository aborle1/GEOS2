#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for EPH tool group
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77FLAGS
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC
#  
# author:  Mike Sucher / Applied Research Corporation 
#          Guru Tej S. Khalsa / Applied Research Corporation
#          Megan E.D. Spencer
#          Xin Wang / EIT Inc.
#
# history:
#	13-Apr-1994 MES  Template version
#	18-Apr-1994 MES  Pass all PGS env variables, and 
# 		 	 call mkpgslib with $PGSHOME override
#	01-Sep-1994 MES  Modified C and FORTRAN libs to use the 
# 			 Release 3 library libPGSTK.a
# 	08-Sep-1994 MES  Clean up target all - remove $(SUBGROUPS) from
# 			 target list for recursive call to 'make'.
#	03-Nov-1995 GTSK - Added $(ADD_IFLAGS) to IFLAG definition
# 			 - Added $(ADD_LFLAGS) to LFLAG definition
# 			 These allow additional directories in the include 
# 			 and lib paths by setting environment variable.
# 			 - Added $(ADD_LIBS) to LIBS definition which allows
# 			 additional libraries to be added by setting an 
# 			 environment variable.
#        10-Dec-2001  XW  Provided the actual attitude data upon request.
#        10-Feb-2002  XW  Got ephemeris data from HDF files.
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE).  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target executable files are moved to $PGSBIN.
# 	3) Target object files are moved to $(PGSOBJ)/$(GRP).
# 	4) This file does NOT build the library for this group.
#	   That is handled by the group-level makefile.
#
#----------------------------------------------------------------------------

#
# set the name of this tool group
#

GRP = EPH

# force make to use the 'sh' shell
SHELL = /bin/sh

# name of remove utility
RM = /bin/rm
RMFLAGS = -f 
MFLAGS2= -f makefile.cpp

# define C preprocessor symbols 
DFLAGS = -D$(HDFSYS)

# path for #include directive
IFLAG = -I$(PGSINC) $(ADD_IFLAGS) -I$(HDFINC) -I$(HDF5INC) 

# path for libraries linked (compiler will search in the order listed)
LFLAG = -L$(PGSLIB) $(ADD_LFLAGS) -L$(HDFLIB) -L$(HDF5LIB)

# libraries linked for C build
LIBS = -lPGSTKcpp $(ADD_LIBS) -lmfhdf -ldf -ljpeg $(HDF5LIB)/libhdf5.a -lz -lm

# for building orbsim
TARG = all

#
# object files from this sub-group needed to build library
#

C_LIBOFILES = \
	$(PGSCPPO)/$(GRP)/PGS_EPH_EphemAttit.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_EphAtt_unInterpolate.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_UnInterpEphAtt.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_GetEphMet.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_GetSpacecraftData.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_ManageMasks.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_getEphemHeaders.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_getAttitHeaders.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_getEphemRecords.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_getAttitRecords.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_getToken.o \
	$(PGSCPPO)/$(GRP)/PGS_EPH_fileHandlingStuff.o \
        $(PGSCPPO)/$(GRP)/PGS_EPH_interpolateAttitude.o \
        $(PGSCPPO)/$(GRP)/PGS_EPH_interpolatePosVel.o


SUBGROUPS = orbsim

#
# targets
#

all:	message  c_obj lib

lib:
	@$(PGSBIN)/mkpgslib.cpp $(GRP)

c_obj:  $(C_LIBOFILES) 

utilities: orbsim


message:
	@echo "    Makefile for group: $(GRP); Target: all"

clean:
	$(RM) $(RMFLAGS) core *.o

	$(RM) $(RMFLAGS) $(PGSCPPO)/$(GRP)/*.o
	@$(MAKE) $(MFLAGS) $(MFLAGS2) orbsim TARG=clean

orbsim: force_orbsim

# for backward compatibility
orb: force_orbsim

force_orbsim:
	@echo ""; cd orbsim; echo Making \`$(TARG)\' in ./orbsim; \
              $(MAKE) $(MFLAGS) $(MFLAGS2) $(TARG)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/PGS_EPH_EphemAttit.o: PGS_EPH_EphemAttit.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_EphAtt_unInterpolate.o: PGS_EPH_EphAtt_unInterpolate.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_UnInterpEphAtt.o: PGS_EPH_UnInterpEphAtt.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_GetEphMet.o: PGS_EPH_GetEphMet.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_GetSpacecraftData.o: PGS_EPH_GetSpacecraftData.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_ManageMasks.o: PGS_EPH_ManageMasks.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_getEphemHeaders.o: PGS_EPH_getEphemHeaders.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_getAttitHeaders.o: PGS_EPH_getAttitHeaders.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_getEphemRecords.o: PGS_EPH_getEphemRecords.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_getAttitRecords.o: PGS_EPH_getAttitRecords.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_getToken.o: PGS_EPH_getToken.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_fileHandlingStuff.o: PGS_EPH_fileHandlingStuff.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_interpolateAttitude.o: PGS_EPH_interpolateAttitude.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_EPH_interpolatePosVel.o: PGS_EPH_interpolatePosVel.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@


# force the default make rule to search for a specific make rule
# for a given input file (i.e. target)
.c.o:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@


