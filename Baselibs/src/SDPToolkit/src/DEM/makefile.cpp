#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:  makefile for PGS Toolkit Digital Elevation Model tools (DEM)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS
#   includes:  PGSINC HDFINC HDFEOS_INC
#   other:     PGSOBJ PGSLIB PGSBIN
#  
# author:  Mike Sucher / Applied Research Corporation
#          Guru Tej S. Khalsa / Applied Research Corporation
#          Alexis Zubrow / Applied Research Corporation
#          Megan E.D.Spencer / SM&A
#
# history:
#	20-Mar-1997 MES/GTSK/AZ  Initial version
#       15-Apr-1997 AZ           Added PGS_DEM_GetMetadata
#	24-Jun-1997 AZ		 Added PGS_DEM_GetQualityData
#	17-Jul-1997 AZ           Added PGS_DEM_RecursiveSearchBil, 
#				       PGS_DEM_AccessFile,  
#				       PGS_DEM_Interpolate, and
#				       PGS_DEM_GetBoundingPnts	 
#				       		
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE).  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.  Before using this makefile do the following:
#            %source $PGSBIN/pgs-dev-env.csh
#          (csh users only--others may have to manually set the variables
#          mentioned above to the values appropriate for their platform
#          as is done automatically in pgs-dev-env.csh)
# 	2) Target object files are moved to $(PGSOBJ)/$(GRP).
# 	3) This file builds the library for this group.
#
#----------------------------------------------------------------------------

#
# set the name of this tool group
#

GRP=DEM

# force make to use the 'sh' shell
SHELL = /bin/sh

# name of remove utility
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp

# define C preprocessor symbols 
DFLAGS  = -D$(HDFSYS)

# path for #include directive
IFLAG   = -I$(PGSINC) -I$(HDFINC) -I$(HDF5INC) -I$(HDFEOS_INC) $(ADD_IFLAGS) 

# object files from this sub-group needed to build library
#
# !!! LIST ALL OBJECT FILES HERE NEEDED FOR THE GROUP LIBRARY !!!
#
LIBOFILES = \
        $(PGSCPPO)/$(GRP)/PGS_DEM_Subset.o \
        $(PGSCPPO)/$(GRP)/PGS_DEM_Lookup.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_OrderSubset.o \
        $(PGSCPPO)/$(GRP)/PGS_DEM_Populate.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_WriteSubgridCalculator.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchPix.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchDeg.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchBil.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_OrderIndicesSumPix.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_OrderIndicesSumDeg.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_Open.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_Close.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ExtentRegion.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ExtractRegion.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPoints.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsInt8.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsInt16.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsFlt32.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetPoint.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetRegion.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetSize.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetMetadata.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetQualityData.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_GetBoundingPnts.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_DataPresent.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_SortModels.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_AccessFile.o \
	$(PGSCPPO)/$(GRP)/PGS_DEM_Interpolate.o 



all: message $(PGSLIB)/libPGSTKcpp.a

$(PGSLIB)/libPGSTKcpp.a: $(LIBOFILES) lib

lib:
	@$(PGSBIN)/mkpgslib.cpp $(GRP)

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(PGSCPPO)/$(GRP)/*.o

message:
	@echo "    Makefile for group: $(GRP); Target: all"

#
# compilation rules
#

# compile Toolkit functions (source code files to object files)
$(PGSCPPO)/$(GRP)/PGS_DEM_Subset.o: PGS_DEM_Subset.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_Lookup.o: PGS_DEM_Lookup.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_OrderSubset.o: PGS_DEM_OrderSubset.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_Populate.o: PGS_DEM_Populate.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_WriteSubgridCalculator.o: PGS_DEM_WriteSubgridCalculator.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchPix.o: PGS_DEM_RecursiveSearchPix.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchDeg.o: PGS_DEM_RecursiveSearchDeg.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_RecursiveSearchBil.o: PGS_DEM_RecursiveSearchBil.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_OrderIndicesSumPix.o: PGS_DEM_OrderIndicesSumPix.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_OrderIndicesSumDeg.o: PGS_DEM_OrderIndicesSumDeg.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_Open.o: PGS_DEM_Open.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_Close.o: PGS_DEM_Close.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ExtentRegion.o: PGS_DEM_ExtentRegion.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ExtractRegion.o: PGS_DEM_ExtractRegion.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPoints.o: PGS_DEM_ReplaceFillPoints.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsInt8.o: PGS_DEM_ReplaceFillPointsInt8.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsInt16.o: PGS_DEM_ReplaceFillPointsInt16.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_ReplaceFillPointsFlt32.o: PGS_DEM_ReplaceFillPointsFlt32.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetPoint.o: PGS_DEM_GetPoint.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetRegion.o: PGS_DEM_GetRegion.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetSize.o: PGS_DEM_GetSize.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetMetadata.o: PGS_DEM_GetMetadata.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetQualityData.o: PGS_DEM_GetQualityData.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_GetBoundingPnts.o: PGS_DEM_GetBoundingPnts.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_DataPresent.o: PGS_DEM_DataPresent.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_SortModels.o: PGS_DEM_SortModels.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_AccessFile.o: PGS_DEM_AccessFile.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_DEM_Interpolate.o: PGS_DEM_Interpolate.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@


