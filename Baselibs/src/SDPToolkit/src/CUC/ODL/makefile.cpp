#-----------------------------------------------------------------------------
# file:		makefile for CUC tools 
#		(for ODL only)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#  
# author: Mike Sucher / Richard Morris / Megan E.D. Spencer
#
# history:
#	01-Apr-1994 MES Initial version
#	05-Apr-1994 MES Remove references to private directories
#	07-Apr-1994 MES Add FORTRAN compilation rule, target for test drivers
#	04-Aug-1994 TWA Delete old files from LIBOFILES; change "Tmp" to 
# 			"Temp" in filename
#	04-Aug-1994 MES Add FORTRAN to object  (.f.o) compilation rule
# 			Add FORTRAN toolkit files to object list
# 			Add FORTRAN header files dependencies
#	09-Aug-1994 TWA Add PG_IO_Gen_Temp_Ref to obj file list
#	10-Aug-1994 MES Revise rules to leave executables in local dir
#	12-Aug-1994 TWA Change PGS_IO_Gen_Temp_Ref.o to 
# 	                PGS_IO_Gen_Temp_Reference.o
#	17-Aug-1994 MES Enhance target finc: (copy in FORTRAN include files)
# 			  It now checks if files are there, sets write
# 			  permission and overwrites with versions in $PGSINC.
# 			Add finc to the target list for target 'all'.
#	26-Aug-1994 MES Remove obsolete FORTRAN SMF header files.
# 			Add target 'fobj' to make FORTRAN lib files.
#	30-Aug-1994 DPH Adjusted to refect new name of file PGS_IO_Gen_Temp_Del.c
#	30-Aug-1994 MES Modified C and FORTRAN libs to use the 
# 			Release 3 library libPGSTK.a
#	07-Sep-1994 RSM Change to use with AA tools
#	24-Oct-1994 MES Enhance target finc: Don't attempt chmod on existing
# 			files if they are already writable.
# 			Remove -g switch, obsolete targets
#       17-Jun-2002 Phuong T. Nguyen (L3 Communication Corp.)
#                       Modified for C++ on linux:  Added -D$(HDFSYS) to DFLAGS
#       
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target executable files are moved to $PGSBIN
# 	3) Target object files are moved to $(PGSLIB)/obj/CUC
# 	4) To build the PGS CUC tools library, type 'make lib'
#	   (This runs the script $PGSBIN/mkpgslib to build the library).
#----------------------------------------------------------------------------

#
# set the name of this tool group
#
GRP = CUC

# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# Default target
TARG=all

# name of remove utility
RM = /bin/rm
RMFLAGS = -f 
MFLAGS = -f makefile.cpp
# define optimization level for comilation
OPT_LEVEL = -O

# define C preprocessor symbols 
DFLAGS  = -D$(HDFSYS) -DSUN_UNIX

# path for #include directive
IFLAG   = -I. -I$(PGSINC) -I$(PGSINC)/CUC 

# object files from this sub-group needed to build library  

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/a_nodesa.o \
	$(PGSCPPO)/$(GRP)/ag_nodesag.o \
	$(PGSCPPO)/$(GRP)/ao_nodesao.o \
	$(PGSCPPO)/$(GRP)/comments.o \
	$(PGSCPPO)/$(GRP)/cvtvalue.o \
	$(PGSCPPO)/$(GRP)/fmtvalue.o \
	$(PGSCPPO)/$(GRP)/lexan.o \
	$(PGSCPPO)/$(GRP)/p_nodesp.o \
	$(PGSCPPO)/$(GRP)/parsact.o \
	$(PGSCPPO)/$(GRP)/parser.o \
	$(PGSCPPO)/$(GRP)/prtlabel.o \
	$(PGSCPPO)/$(GRP)/prtsrc.o \
	$(PGSCPPO)/$(GRP)/rdlabel.o \
	$(PGSCPPO)/$(GRP)/rdvalue.o \
	$(PGSCPPO)/$(GRP)/v_nodesv.o \
	$(PGSCPPO)/$(GRP)/wrtlabel.o \
	$(PGSCPPO)/$(GRP)/wrtsrc.o \
	$(PGSCPPO)/$(GRP)/output.o

#
# targets
#

all: $(LIBOFILES)

dbug:
	@$(MAKE)$(MFLAGS2) OPT_LEVEL=-g $(TARG)

lib:
	@${PGSBIN}/mkpgslib.cpp $(GRP)


clean:
	$(RM) $(RMFLAGS) $(PGSCPPO)/$(GRP)/core $(PGSCPPO)/$(GRP)/*.o 

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/a_nodesa.o: a_nodesa.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/ag_nodesag.o: ag_nodesag.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/ao_nodesao.o: ao_nodesao.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/comments.o: comments.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/cvtvalue.o: cvtvalue.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/fmtvalue.o: fmtvalue.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/lexan.o: lexan.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/p_nodesp.o: p_nodesp.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/parsact.o: parsact.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/parser.o: parser.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/prtlabel.o: prtlabel.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/prtsrc.o: prtsrc.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/rdlabel.o: rdlabel.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/rdvalue.o: rdvalue.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/v_nodesv.o: v_nodesv.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wrtlabel.o: wrtlabel.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wrtsrc.o: wrtsrc.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/output.o: output.c
	$(CPP) -c $(OPT_LEVEL) $(SGISTDOFF) $(DFLAGS) $(IFLAG) $? -o $@

# force the default make rule to search for a specific make rule
# for a given input file (i.e. target)
.c.o:
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@
