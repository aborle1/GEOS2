#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 1999, Raytheon Systems Company, its vendors, #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for PC tool group
#
# environment:	MPPDE, machine-independent 
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS
#   includes:  PGSINC 
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC
# 
# author:  David P. Heroux / Applied Research Corporation
#	   Michael E. Sucher / Applied Research Corporation
#	   Ray Milburn / Applied Research Corporation
#          Guru Tej S. Khalsa / Applied Research Corporation
#          Megan E. D. Spencer / SM&A
#
# history:
#	01-Apr-1994 MES Initial version
#	18-Apr-1994 DPH created target 'all'
#	30-Aug-1994 RM  added new files
#	01-Sep-1994 MES Modified C and FORTRAN libs to use the 
# 			Release 3 library libPGSTK.a
#	25-Oct-1994 DPH Added target 'clean' & removed targets for 'Put' & 
#			'Get' drivers. Also removed rules for C/Fortran 
#			executables.
#	18-Dec-1994 RM  Added new files for TK4 and added targets for 
#			PGS_PC_InitCom and PGS_PC_TermCom.
#	02-Jan-1995 DPH Added target 'commands' to build all command utilities.
#	06-Feb-1995 MES Remove 'commands' from dependency  list for 'all'.
#	05-Apr-1995 RM  Added new filesc for TK5.  Functionality to move the
#			default file locations from environment variables to
#			the PCF.
#	11-Apr-1995 RM  Added new file PGS_PC_GetReferenceType.o
#	20-Apr-1995 RM  Added new files for new marking RUNTIME functionality.
#			New files are PGS_PC_CheckFlags.o, 
#			PGS_PC_MarkRuntimeAscii.o, PGS_PC_MarkRuntimeShm.o,
#			and PGS_PC_MultiRuntimes.o.
#	30-Jun-1995 MES Revised to use a single .c rule to compile all of the 
# 			utilities.  Also check the platform type and disable 
#			the -DSHMMEM (shared memory) flag, if compilation is 
# 			on the Cray. 
#	03-Jul-1995 MES Removed the move to object dir command from .c.o rule. 
# 			Revised target 'pc' to use $LIBOFILES, renamed to 'pc_obj'.
# 			Updated target 'pctcheck' to use the new build rules.
#	07-Jul-1995 RM  Added new file PGS_PC_GetFileByAttrF.c for DR ECSed00932.
#	03-Nov-1995 GSK - Added $(ADD_IFLAGS) to IFLAG definition
# 			- Added $(ADD_LFLAGS) to LFLAG definition
# 			These allow additional directories in the include 
# 			and lib paths by setting environment variable.
# 			- Added $(ADD_LIBS) to LIBS definition which allows
# 			additional libraries to be added by setting an 
# 			environment variable.
#	15-Dec-1995 RM  Updated for TK6.  Added new file PGS_PC_GetPCFTemp.c.
#			This allows the Process Control Tools to works across
#			filesystems.
#	05-Jan-1996 RM 	Updated for TK6.  Added Universal References to
#			file entries for files.  Added new file 
#			PGS_PC_GetUniversalRef.c.
#	17-Jan-1996 GSK	Added PGS_PC_bindFORTRAN.o to list of object files
#			defined by LIBOFILES.
#       05-Mar-1997 CST Added new object file PGS_PC_PGS_PC_GetFileSize.o and 
#                       new command file PGS_PC_PGS_PC_GetFileSizeCom.
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target executable files are moved to $PGSBIN
# 	3) Target object files are moved to $(PGSOBJ)/PC
# 	4) To build the PGS PC tools library, type 'make libPC.a'
#	   (This runs the script $PGSBIN/mkpgslib to build the library).
#----------------------------------------------------------------------------

#
# set the name of this tool group
#

GRP=TSF

# force make to use the 'sh' shell
SHELL = /bin/sh

# name of remove utility
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp

# define C preprocessor symbols 
DFLAGS  = -D$(HDFSYS) -DSUNCC

# path for #include directive
IFLAG   = -I$(PGSINC) -I$(HDFINC) -I$(HDF5INC) -I$(HDFEOS_INC) $(ADD_IFLAGS)

# path for libraries linked (compiler will search in the order listed)
LFLAG	= -L$(PGSLIB) $(ADD_LFLAGS)

# libraries linked for C build
LIBS   = -lPGSTKcpp $(ADD_LIBS)

LIBOFILES	=  			\
	$(PGSOBJ)/$(GRP)/PGS_TSF_GetTSFMaster.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_HandleLock.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupAA.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupCBP.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupCSC.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupDEM.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupSMF.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupIO.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupTD.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupEPH.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupMET.o \
	$(PGSOBJ)/$(GRP)/PGS_TSF_SetupGCT.o



#
# targets
#

all: message lib

lib: $(LIBOFILES)
	@$(PGSBIN)/mkpgslib.cpp $(GRP)

message:
	@echo "    Makefile for group: $(GRP); Target: all"

clean:
	$(RM) $(RMFLAGS) *.o
	$(RM) $(RMFLAGS) $(PGSCPPO)/$(GRP)/*.o


# compile Toolkit functions (source code files to object files)
$(PGSCPPO)/$(GRP)/PGS_TSF_GetTSFMaster.o: PGS_TSF_GetTSFMaster.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_HandleLock.o: PGS_TSF_HandleLock.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupAA.o: PGS_TSF_SetupAA.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupCBP.o: PGS_TSF_SetupCBP.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupCSC.o: PGS_TSF_SetupCSC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupDEM.o: PGS_TSF_SetupDEM.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupSMF.o: PGS_TSF_SetupSMF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupIO.o: PGS_TSF_SetupIO.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupTD.o: PGS_TSF_SetupTD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupEPH.o: PGS_TSF_SetupEPH.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupMET.o: PGS_TSF_SetupMET.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TSF_SetupGCT.o: PGS_TSF_SetupGCT.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# force the default make rules to search for a specific make rule
# for a given input file (i.e. target)
.c:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@

.c.o:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
