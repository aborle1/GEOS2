#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for PGS Toolkit Time and Date tools (TD)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSOBJ PGSBIN
#  
# author:  Mike Sucher / Applied Research Corporation
#          Guru Tej S. Khalsa / Applied Research Corporation
#          Megan E.D.Spencer / SM & A
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

GRP=TD

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
LIBS   = -lPGSTKcpp $(ADD_LIBS) -lm

# object files from this sub-group needed to build library
#
# !!! LIST ALL OBJECT FILES HERE NEEDED FOR THE GROUP LIBRARY !!!
#
LIBOFILES	=  \
     $(PGSCPPO)/$(GRP)/PGS_TD_ADEOSIItoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ADEOSIItoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ASCIItime_AtoB.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ASCIItime_BtoA.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSAMtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSAMtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIIStoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIRDtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIIStoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIRDtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAGIIStoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAGIRDtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_FGDCtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_GPStoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ISOinttoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ISOinttoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_JDtoMJD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_JDtoTJD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_JulianDateSplit.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_LeapSec.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_MJDtoJD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ManageTMDF.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_ManageUTCF.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_PB5CtoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_PB5toTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_PB5toUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_SCtime_to_UTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoTDTjed.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoGAST.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoISOint.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoTAIjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoUDTF.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoUT1jd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoUT1pole.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TAItoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TDBjedtoTDTjed.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TDTjedtoTAIjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TDTjedtoTDBjed.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TJDtoJD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TRMMtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TRMMtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_TimeInterval.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UDTFtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UDTFtoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UT1jdtoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTC_to_SCtime.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoISOint.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoPB5.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoPB5C.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoTAIjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUDTF.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUT1jd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUTC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoADEOSII.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAM.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPM.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPMGIIS.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPMGIRD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAURAGIIS.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAURAGIRD.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoFGDC.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoGPS.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTAI.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTAIjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTDBjed.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTDTjed.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTRMM.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUT1.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUT1jd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUTCjd.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_calday.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_gast.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_gmst.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_julday.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_sortArrayIndices.o \
     $(PGSCPPO)/$(GRP)/PGS_TD_timeCheck.o

#
# targets
#


all: 	message lib

lib: $(LIBOFILES)
	@$(PGSBIN)/mkpgslib.cpp $(GRP)

utilities: PGS_TD_NewLeap update_leapsec update_leapsec_CC

update_leapsec:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@.sh

update_leapsec_CC:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@.sh

$(PGSBIN)/update_leapsec.sh: update_leapsec.sh
	cp $? $@; chmod 755 $@

$(PGSBIN)/update_leapsec_CC.sh: update_leapsec_CC.sh
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
$(PGSBIN)/PGS_TD_NewLeap: PGS_TD_NewLeap.c $(PGSLIB)/libPGSTKcpp.a
	$(CPP) $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $(LFLAG) -o $@ PGS_TD_NewLeap.c $(LIBS)

# compile Toolkit functions (source code files to object files)
$(PGSCPPO)/$(GRP)/PGS_TD_ADEOSIItoTAI.o: PGS_TD_ADEOSIItoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ADEOSIItoUTC.o: PGS_TD_ADEOSIItoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ASCIItime_AtoB.o: PGS_TD_ASCIItime_AtoB.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ASCIItime_BtoA.o: PGS_TD_ASCIItime_BtoA.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSAMtoTAI.o: PGS_TD_EOSAMtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSAMtoUTC.o: PGS_TD_EOSAMtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMtoTAI.o: PGS_TD_EOSPMtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIIStoTAI.o: PGS_TD_EOSPMGIIStoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIRDtoTAI.o: PGS_TD_EOSPMGIRDtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMtoUTC.o: PGS_TD_EOSPMtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIIStoUTC.o: PGS_TD_EOSPMGIIStoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSPMGIRDtoUTC.o: PGS_TD_EOSPMGIRDtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@
 
$(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAGIIStoTAI.o: PGS_TD_EOSAURAGIIStoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAGIRDtoTAI.o: PGS_TD_EOSAURAGIRDtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@
 
$(PGSCPPO)/$(GRP)/PGS_TD_EOSAURAtoUTC.o: PGS_TD_EOSAURAtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_FGDCtoUTC.o: PGS_TD_FGDCtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_GPStoUTC.o: PGS_TD_GPStoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ISOinttoTAI.o: PGS_TD_ISOinttoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ISOinttoUTCjd.o: PGS_TD_ISOinttoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_JDtoMJD.o: PGS_TD_JDtoMJD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_JDtoTJD.o: PGS_TD_JDtoTJD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_JulianDateSplit.o: PGS_TD_JulianDateSplit.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_LeapSec.o: PGS_TD_LeapSec.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_MJDtoJD.o: PGS_TD_MJDtoJD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ManageTMDF.o: PGS_TD_ManageTMDF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_ManageUTCF.o: PGS_TD_ManageUTCF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_PB5CtoUTCjd.o: PGS_TD_PB5CtoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_PB5toTAI.o: PGS_TD_PB5toTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_PB5toUTCjd.o: PGS_TD_PB5toUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_SCtime_to_UTC.o: PGS_TD_SCtime_to_UTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoTAI.o: PGS_TD_TAIjdtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoTDTjed.o: PGS_TD_TAIjdtoTDTjed.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAIjdtoUTCjd.o: PGS_TD_TAIjdtoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoGAST.o: PGS_TD_TAItoGAST.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoISOint.o: PGS_TD_TAItoISOint.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoTAIjd.o: PGS_TD_TAItoTAIjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoUDTF.o: PGS_TD_TAItoUDTF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoUT1jd.o: PGS_TD_TAItoUT1jd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoUT1pole.o: PGS_TD_TAItoUT1pole.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoUTC.o: PGS_TD_TAItoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TAItoUTCjd.o: PGS_TD_TAItoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TDBjedtoTDTjed.o: PGS_TD_TDBjedtoTDTjed.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TDTjedtoTAIjd.o: PGS_TD_TDTjedtoTAIjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TDTjedtoTDBjed.o: PGS_TD_TDTjedtoTDBjed.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TJDtoJD.o: PGS_TD_TJDtoJD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TRMMtoTAI.o: PGS_TD_TRMMtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TRMMtoUTC.o: PGS_TD_TRMMtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_TimeInterval.o: PGS_TD_TimeInterval.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UDTFtoTAI.o: PGS_TD_UDTFtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UDTFtoUTCjd.o: PGS_TD_UDTFtoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UT1jdtoUTCjd.o: PGS_TD_UT1jdtoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTC_to_SCtime.o: PGS_TD_UTC_to_SCtime.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoISOint.o: PGS_TD_UTCjdtoISOint.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoPB5.o: PGS_TD_UTCjdtoPB5.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoPB5C.o: PGS_TD_UTCjdtoPB5C.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoTAIjd.o: PGS_TD_UTCjdtoTAIjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUDTF.o: PGS_TD_UTCjdtoUDTF.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUT1jd.o: PGS_TD_UTCjdtoUT1jd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCjdtoUTC.o: PGS_TD_UTCjdtoUTC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoADEOSII.o: PGS_TD_UTCtoADEOSII.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAM.o: PGS_TD_UTCtoEOSAM.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPM.o: PGS_TD_UTCtoEOSPM.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPMGIIS.o: PGS_TD_UTCtoEOSPMGIIS.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSPMGIRD.o: PGS_TD_UTCtoEOSPMGIRD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@
 
$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAURAGIIS.o: PGS_TD_UTCtoEOSAURAGIIS.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoEOSAURAGIRD.o: PGS_TD_UTCtoEOSAURAGIRD.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoFGDC.o: PGS_TD_UTCtoFGDC.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoGPS.o: PGS_TD_UTCtoGPS.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTAI.o: PGS_TD_UTCtoTAI.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTAIjd.o: PGS_TD_UTCtoTAIjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTDBjed.o: PGS_TD_UTCtoTDBjed.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTDTjed.o: PGS_TD_UTCtoTDTjed.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoTRMM.o: PGS_TD_UTCtoTRMM.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUT1.o: PGS_TD_UTCtoUT1.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUT1jd.o: PGS_TD_UTCtoUT1jd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_UTCtoUTCjd.o: PGS_TD_UTCtoUTCjd.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_calday.o: PGS_TD_calday.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_gast.o: PGS_TD_gast.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_gmst.o: PGS_TD_gmst.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_julday.o: PGS_TD_julday.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_sortArrayIndices.o: PGS_TD_sortArrayIndices.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_TD_timeCheck.o: PGS_TD_timeCheck.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# force the default make rules to search for a specific make rule
# for a given input file (i.e. target)
.c:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSBIN)/$@

.c.o:
	@$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
