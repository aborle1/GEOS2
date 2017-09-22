#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# file:         makefile for MET tools
#
# environment:  MPPDE, machine-independent, PGS directory structure
#
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   other:     PGSOBJ HDFSYS HDFINC HDF5INC
#
# author: Mike Sucher / Graham Bland
#         Carol S. W. Tsai / Space Applications Corporation
#         Megan E.D. Spencer / SM&A
#         Abe Taaheri/ Emergent Information Technologies, Inc.
# history:
#       01-Apr-1994 MES Initial version
#	02-JAN-1995 ANS Adopted for GCT
#	04-Apr-1995 ANS Adopted for MET
#       16-Oct-1997 CSWT Added new object file PGS_MET_ConvertToMCF.o
#       30-Mar-2001 AT  Modified for HDF5 support
# notes:
#       1) This file is intended for use in the Multi-Platform PGS Development
#          Environment (MPPDE) .  It depends on the PGS-defined toolkit
#          directory structure, and on environmental variables defined
#          by MPPDE startup files.  This is how machine independence
#          is maintained.
#       2) Target object files are moved to $(PGSOBJ)/MET
#
#----------------------------------------------------------------------------

#
# Set the name of this tool group
#

GRP= MET

# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# Name of the remove utility and flags
RM= /bin/rm
RMFLAGS= -f
MFLAGS2= -f makefile.cpp 

# define C preprocessor symbols
DFLAGS  = -D$(HDFSYS) -DPGS_MET_COMPILE

# path for #include directive
IFLAG   = -I$(PGSINC) -I$(PGSINC)/CUC -I$(HDFINC) -I$(HDF5INC)

# object files from this sub-group needed to build library

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/PGS_MET_LoadAggregate.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_ErrorMsg.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_CheckAttr.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_GetDDAttr.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_CheckAgainstDD.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_ConvertToOdl.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_GetConfigByLabel.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_RetrieveConfigData.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_HDFToODL.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_NameAndClass.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_SearchAttr.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_SearchAttrF.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_ConvertToMCF.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_GetSetAttrTD.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_ODLToXML..o \
	$(PGSCPPO)/$(GRP)/PGS_MET_WriteXML.o \
	$(PGSCPPO)/$(GRP)/PGS_MET_XslProcessor.o

#
# targets
#

all:    $(LIBOFILES)

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(LIBOFILES)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/PGS_MET_LoadAggregate.o: PGS_MET_LoadAggregate.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_ErrorMsg.o: PGS_MET_ErrorMsg.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_CheckAttr.o: PGS_MET_CheckAttr.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_GetDDAttr.o: PGS_MET_GetDDAttr.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_CheckAgainstDD.o: PGS_MET_CheckAgainstDD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_ConvertToOdl.o: PGS_MET_ConvertToOdl.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_GetConfigByLabel.o: PGS_MET_GetConfigByLabel.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_RetrieveConfigData.o: PGS_MET_RetrieveConfigData.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_HDFToODL.o: PGS_MET_HDFToODL.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_NameAndClass.o: PGS_MET_NameAndClass.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_SearchAttr.o: PGS_MET_SearchAttr.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_SearchAttrF.o: PGS_MET_SearchAttrF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_ConvertToMCF.o: PGS_MET_ConvertToMCF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_GetSetAttrTD.o: PGS_MET_GetSetAttrTD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_ODLToXML.o: PGS_MET_ODLToXML.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_WriteXML.o: PGS_MET_WriteXML.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_MET_XslProcessor.o: PGS_MET_XslProcessor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# compile C to object
.c.o:
	$(MAKE) $(MFLAGS)  $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@

