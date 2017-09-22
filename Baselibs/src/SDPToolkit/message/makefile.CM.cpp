#----------------------------------------------------------------------------
# file:		CM Makefile for SMF files
#
# environment:	MPPDE, machine-independent 
# 	
# environment variables dependencies:
#   compiler:  smfcompile
#   includes:  PGSINC 
#   messages:  PGSMSG
#   other:     PGSBIN BLDDIR
# 
# author:  Guru Tej S. Khalsa
#
# history:
#	18-Nov-1997 GSK Initial version.
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) To build the PGS SMF files, type 'make all'
#----------------------------------------------------------------------------
# Define sh shell
SHELL=/bin/sh

# Define smfcompile utility
SC=smfcompile

# Define flags for use with smfcompile utility
SCFLAGS=-i
SF77FLAGS=-f77 -i

# Define CM access program (this program checks files out, if necessary)
CMACCESS=$(BLDDIR)/cmAccess

SMFFILES = 			\
	$(PGSMSG)/PGS_1		\
	$(PGSMSG)/PGS_3		\
	$(PGSMSG)/PGS_4		\
	$(PGSMSG)/PGS_5		\
	$(PGSMSG)/PGS_6		\
	$(PGSMSG)/PGS_7		\
	$(PGSMSG)/PGS_9		\
	$(PGSMSG)/PGS_10	\
	$(PGSMSG)/PGS_11	\
	$(PGSMSG)/PGS_12	\
	$(PGSMSG)/PGS_13	\
	$(PGSMSG)/PGS_14	\
	$(PGSMSG)/PGS_15

# define default target

default:
	@echo "This file is for use by ECS Configuration Management processes only."

all: message smffiles

smffiles: $(SMFFILES)

message:
	@echo "Setting up SMF include files ..."

$(PGSMSG)/PGS_1: PGS_IO_1.t
	@$(CMACCESS) PGS_1 $(PGSINC)/PGS_IO_1.h $(PGSINC)/PGS_IO_1.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_2:
	@echo SMF seed value "2" is reserved for internal SMF tool usage

$(PGSMSG)/PGS_3: PGS_TD_3.t
	@$(CMACCESS) PGS_3 $(PGSINC)/PGS_TD_3.h $(PGSINC)/PGS_TD_3.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_4: PGS_CSC_4.t
	@$(CMACCESS) PGS_4 $(PGSINC)/PGS_CSC_4.h $(PGSINC)/PGS_CSC_4.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_5: PGS_EPH_5.t
	@$(CMACCESS) PGS_5 $(PGSINC)/PGS_EPH_5.h $(PGSINC)/PGS_EPH_5.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_6: PGS_CBP_6.t
	@$(CMACCESS) PGS_6 $(PGSINC)/PGS_CBP_6.h $(PGSINC)/PGS_CBP_6.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_7: PGS_MEM_7.t
	@$(CMACCESS) PGS_7 $(PGSINC)/PGS_MEM_7.h $(PGSINC)/PGS_MEM_7.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_9: PGS_PC_9.t
	@$(CMACCESS) PGS_9 $(PGSINC)/PGS_PC_9.h $(PGSINC)/PGS_PC_9.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_10: PGS_AA_10.t
	@$(CMACCESS) PGS_10 $(PGSINC)/PGS_AA_10.h $(PGSINC)/PGS_AA_10.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_11: PGS_CUC_11.t
	@$(CMACCESS) PGS_11 $(PGSINC)/PGS_CUC_11.h $(PGSINC)/PGS_CUC_11.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_12: PGS_GCT_12.t
	@$(CMACCESS) PGS_12 $(PGSINC)/PGS_GCT_12.h $(PGSINC)/PGS_GCT_12.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_13: PGS_MET_13.t
	@$(CMACCESS) PGS_13 $(PGSINC)/PGS_MET_13.h $(PGSINC)/PGS_MET_13.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

$(PGSMSG)/PGS_14: PGS_DEM_14.t
	@$(CMACCESS) PGS_14 $(PGSINC)/PGS_DEM_14.h $(PGSINC)/PGS_DEM_14.f
	$(SC) -f $? $(SCFLAGS)
	$(SC) -f $? $(SF77FLAGS)

buildsmf:
	@echo "Building the smfcompile utility ..."
	@cd $(PGSSRC)/SMF; $(MAKE) $(MFLAGS) smfcompile
