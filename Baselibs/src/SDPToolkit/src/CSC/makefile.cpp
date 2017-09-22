#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:  makefile for PGS Toolkit Coordinate System Conversion (CSC) tools
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFLAGS C_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSOBJ PGSBIN
#  
# author:  Mike Sucher / Applied Research Corporation
#          Guru Tej S. Khalsa / Applied Research Corporation
#	   Megan E.D.Spencer / SM&A
#
# history:
#	13-Apr-1994 MES  Template version
#	18-Apr-1994 MES  Pass all PGS env variables, and 
# 			 call mkpgslib with $PGSHOME override
#       04-Aug-1994 GTSK Specialized for building libPGSTK.a
#                        single library.  No longer builds executables.
#	03-Nov-1995 GTSK - Added $(ADD_IFLAGS) to IFLAG definition
# 			 - Added $(ADD_LFLAGS) to LFLAG definition
# 			 These allow additional directories in the include 
# 			 and lib paths by setting environment variable.
# 			 - Added $(ADD_LIBS) to LIBS definition which allows
# 			 additional libraries to be added by setting an 
# 			 environment variable.
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
# 	3) This file does builds the library for this group.
#
#----------------------------------------------------------------------------

#
# set the name of this tool group
#

GRP=CSC

# force make to use the 'sh' shell
SHELL = /bin/sh

# name of remove utility
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp

# define C preprocessor symbols 
DFLAGS = -D$(HDFSYS)

# path for #include directive
IFLAG   = -I$(PGSINC) $(ADD_IFLAGS) 

# path for libraries linked
LFLAG   = -L$(PGSLIB) $(ADD_LFLAGS)

# libraries linked for C build
LIBS   = -lPGSTKcpp  $(ADD_LIBS) -lm

# object files from this sub-group needed to build library
#
# !!! LIST ALL OBJECT FILES HERE NEEDED FOR THE GROUP LIBRARY !!!
#
LIBOFILES	=  \
     $(PGSCPPO)/$(GRP)/PGS_CSC_BorkowskiGeo.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_DayNight.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ECItoECR.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ECItoORB.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ECItoSC.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ECRtoECI.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ECRtoGEO.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_EarthOccult.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_Earthpt_FixedFOV.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_Earthpt_FOV.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_EulerToQuat.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_FOVconicalHull.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GEOtoECR.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GeoCenToRect.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GetEarthFigure.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GetFOV_Pixel.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GrazingRay.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_GreenwichHour.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_J2000toTOD.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_LookPoint.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_LookTwice.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_Norm.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ORBtoECI.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ORBtoSC.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_PointInFOVgeom.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_QuatToEuler.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_QuatToMatrix.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_RectToGeoCen.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_Rotate3or6.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_SCtoECI.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_SCtoORB.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_SpaceRefract.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_SubSatPoint.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_SubSatPointVel.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_TODtoJ2000.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_TiltYaw.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_UTC_UT1Pole.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_VecToVecAngle.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_ZenithAzimuth.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_crossProduct.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_dotProduct.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_getECItoORBquat.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_getORBtoECIquat.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_getQuats.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_nutate2000.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_precs2000.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_quatMultiply.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_quatRotate.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_quickWahr.o \
     $(PGSCPPO)/$(GRP)/PGS_CSC_wahr2.o

#
# targets
#


all: 	message lib

lib:    $(LIBOFILES)
	@$(PGSBIN)/mkpgslib.cpp $(GRP)

utilities: PGS_CSC_UT1_update update_utcpole update_utcpole_CC

update_utcpole:
	@$(MAKE) $(MFLAGS) $(MFLAGS2)  $(PGSBIN)/$@.sh

update_utcpole_CC:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@.sh

$(PGSBIN)/update_utcpole.sh: update_utcpole.sh
	cp $? $@; chmod 755 $@

$(PGSBIN)/update_utcpole_CC.sh: update_utcpole_CC.sh
	cp $? $@; chmod 755 $@

message:
	@echo "    Makefile for group: $(GRP); Target: all"

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(PGSCPPO)/$(GRP)/*.o


#
# compilation rules
#

# compile Toolkit binary utilities
$(PGSBIN)/PGS_CSC_UT1_update: PGS_CSC_UT1_update.c $(PGSLIB)/libPGSTKcpp.a
	$(CPP) $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $(LFLAG) -o $@ PGS_CSC_UT1_update.c $(LIBS)

# compile Toolkit functions (source code files to object files)
$(PGSCPPO)/$(GRP)/PGS_CSC_BorkowskiGeo.o: PGS_CSC_BorkowskiGeo.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_DayNight.o: PGS_CSC_DayNight.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ECItoECR.o: PGS_CSC_ECItoECR.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ECItoORB.o: PGS_CSC_ECItoORB.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ECItoSC.o: PGS_CSC_ECItoSC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ECRtoECI.o: PGS_CSC_ECRtoECI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ECRtoGEO.o: PGS_CSC_ECRtoGEO.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_EarthOccult.o: PGS_CSC_EarthOccult.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_Earthpt_FixedFOV.o: PGS_CSC_Earthpt_FixedFOV.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_Earthpt_FOV.o: PGS_CSC_Earthpt_FOV.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_EulerToQuat.o: PGS_CSC_EulerToQuat.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_FOVconicalHull.o: PGS_CSC_FOVconicalHull.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GEOtoECR.o: PGS_CSC_GEOtoECR.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GeoCenToRect.o: PGS_CSC_GeoCenToRect.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GetEarthFigure.o: PGS_CSC_GetEarthFigure.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GetFOV_Pixel.o: PGS_CSC_GetFOV_Pixel.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GrazingRay.o: PGS_CSC_GrazingRay.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_GreenwichHour.o: PGS_CSC_GreenwichHour.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_J2000toTOD.o: PGS_CSC_J2000toTOD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_LookTwice.o: PGS_CSC_LookTwice.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_LookPoint.o: PGS_CSC_LookPoint.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_Norm.o: PGS_CSC_Norm.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ORBtoECI.o: PGS_CSC_ORBtoECI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ORBtoSC.o: PGS_CSC_ORBtoSC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_PointInFOVgeom.o: PGS_CSC_PointInFOVgeom.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_QuatToEuler.o: PGS_CSC_QuatToEuler.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_QuatToMatrix.o: PGS_CSC_QuatToMatrix.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_RectToGeoCen.o: PGS_CSC_RectToGeoCen.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_Rotate3or6.o: PGS_CSC_Rotate3or6.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_SCtoECI.o: PGS_CSC_SCtoECI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_SCtoORB.o: PGS_CSC_SCtoORB.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_SpaceRefract.o: PGS_CSC_SpaceRefract.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_SubSatPoint.o: PGS_CSC_SubSatPoint.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_SubSatPointVel.o: PGS_CSC_SubSatPointVel.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_TODtoJ2000.o: PGS_CSC_TODtoJ2000.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_TiltYaw.o: PGS_CSC_TiltYaw.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_UTC_UT1Pole.o: PGS_CSC_UTC_UT1Pole.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_VecToVecAngle.o: PGS_CSC_VecToVecAngle.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_ZenithAzimuth.o: PGS_CSC_ZenithAzimuth.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_crossProduct.o: PGS_CSC_crossProduct.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_dotProduct.o: PGS_CSC_dotProduct.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_getECItoORBquat.o: PGS_CSC_getECItoORBquat.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_getORBtoECIquat.o: PGS_CSC_getORBtoECIquat.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_getQuats.o: PGS_CSC_getQuats.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_nutate2000.o: PGS_CSC_nutate2000.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_precs2000.o: PGS_CSC_precs2000.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_quatMultiply.o: PGS_CSC_quatMultiply.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_quatRotate.o: PGS_CSC_quatRotate.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_quickWahr.o: PGS_CSC_quickWahr.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_CSC_wahr2.o: PGS_CSC_wahr2.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# force the default make rules to search for a specific make rule
# for a given input file (i.e. target)
.c:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@

.c.o:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
