#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for Level 0 IO tools 
#		(subgroup of the PGS Toolkit IO group)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#  
# author:  Mike Sucher / Applied Research Corporation
#	   Tom W. Atwater / Applied Research Corporation 
#          Guru Tej S. Khalsa / Applied Research Corporation
#          Megan E.D.Spencer / SM&A
#          Xin Wang / EIT Inc.
#
# history:
#	18-Nov-1994 MES  Initial version (from Generic IO makefile).
#	12-Dec-1994 MES  Updated list of object files.
#	30-Jan-1995 TWA  Added function PGS_IO_L0_TRMM_HeaderInfo
#	03-Nov-1995 GTSK - Added $(ADD_IFLAGS) to IFLAG definition
# 			 - Added $(ADD_LFLAGS) to LFLAG definition
# 			 These allow additional directories in the include 
# 			 and lib paths by setting environment variable.
# 			 - Added $(ADD_LIBS) to LIBS definition which allows
# 			 additional libraries to be added by setting an 
# 			 environment variable.
#       01-Nov-2001 XW   Modified to support AURA spacecraft
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target executable files are moved to $PGSBIN
# 	3) Target object files are moved to $(PGSOBJ)/IO
# 	4) To build the PGS IO tools library, type 'make libIO.a'
#	   (This runs the script $PGSBIN/mkpgslib to build the library).
#----------------------------------------------------------------------------

#
# set the name of this tool group
#
GRP = IO

# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# Default target
TARG=all

# name of remove utility
RM = /bin/rm
RMFLAGS = -f
MFLAGS2 = -f makefile.cpp

# define C preprocessor symbols 
DFLAGS  = -D$(HDFSYS)

# path for #include directive
IFLAG   = -I$(PGSINC)  $(ADD_IFLAGS)

# path for libraries linked (compiler will search in the order listed)
LFLAG	= -L$(PGSLIB) $(ADD_LFLAGS)

# libraries linked for C build
LIBS   = -lPGSTKcpp $(ADD_LIBS)

# object files from this sub-group needed to build library

LIBOFILES	=  					\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_BYTEtoINT.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_Close.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_FileVersionInfo.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSAMfileTimes.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSPMGIISfileTimes.o     \
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSPMGIRDfileTimes.o     \
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSAURAfileTimes.o       \
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetHeader.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_GetPacket.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_ManageTable.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_MapVersions.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_NextPhysical.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_Open.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_SeekPacket.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_SetStart.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_SetStartCntPkts.o	\
        $(PGSCPPO)/$(GRP)/PGS_IO_L0_TRMM_HdrInfo.o 	\
	$(PGSCPPO)/$(GRP)/PGS_IO_L0_VersionInfoCheck.o 	


#
# targets
#

all: 	message $(LIBOFILES)

lib:
	${PGSBIN}/mkpgslib.cpp $(GRP)

# dummy target to force make
makeme:

utilities: L0sim

L0sim:	makeme
	@echo ""; cd $@; echo Making \`$(TARG)\' in ./$@; \
              $(MAKE) $(MFLAGS) $(MFLAGS2) $(TARG)

message:
	@echo "    Makefile for subgroup: L0; Target: all"

clean:
	$(RM) $(RMFLAGS) *.o;
	$(RM) $(RMFLAGS) $(LIBOFILES)
	@$(MAKE) $(MFLAGS) $(MFLAGS2) L0sim TARG=$@

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/PGS_IO_L0_BYTEtoINT.o: PGS_IO_L0_BYTEtoINT.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_Close.o: PGS_IO_L0_Close.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_FileVersionInfo.o: PGS_IO_L0_FileVersionInfo.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSAMfileTimes.o: PGS_IO_L0_GetEOSAMfileTimes.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSPMGIISfileTimes.o: PGS_IO_L0_GetEOSPMGIISfileTimes.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSPMGIRDfileTimes.o: PGS_IO_L0_GetEOSPMGIRDfileTimes.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetEOSAURAfileTimes.o: PGS_IO_L0_GetEOSAURAfileTimes.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetHeader.o: PGS_IO_L0_GetHeader.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_GetPacket.o: PGS_IO_L0_GetPacket.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_ManageTable.o: PGS_IO_L0_ManageTable.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_MapVersions.o: PGS_IO_L0_MapVersions.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_NextPhysical.o: PGS_IO_L0_NextPhysical.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_Open.o: PGS_IO_L0_Open.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_SeekPacket.o: PGS_IO_L0_SeekPacket.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_SetStart.o: PGS_IO_L0_SetStart.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_SetStartCntPkts.o: PGS_IO_L0_SetStartCntPkts.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_TRMM_HdrInfo.o: PGS_IO_L0_TRMM_HdrInfo.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_L0_VersionInfoCheck.o: PGS_IO_L0_VersionInfoCheck.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@


# force the default make rule to search for a specific make rule
# for a given input file (i.e. target)
.c.o:
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
