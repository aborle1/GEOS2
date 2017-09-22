#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#----------------------------------------------------------------------------
# file:		makefile for Generic IO tools 
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
# author:  Mike Sucher / Megan E.D.Spencer
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
#	09-Aug-1994 TWA Add PGS_IO_Gen_Temp_Ref to obj file list
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
#	12-Sep-1994 MES Enhanced target finc: Don't attempt chmod on existing
# 			files if they are already writable.
#	25-Oct-1994 DPH Enhanced target 'clean' and removed target 'test', as
#			well as rules for building C/Fortran executables
#	27-Jan-1995 MES Modified object file lists, added new targets, 
# 			rewrote target 'all'.  This was done to support 
# 			selective compilation, depending on whether f77 
# 			or f90 is used to build the FORTRAN tools, because 
# 			different files are compiled.
#	03-Mar-1995 MES Modified to skip FORTRAN build if no compiler present
#	01-Nov-1995 MES Modified to allow f90 build if $F77 has flags appended.
#	03-Nov-1995 MES Modified to allow f90 build if $F77 has path prepended.
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

# name of remove utility
RM = /bin/rm
RMFLAGS = -f
MFLAGS2= -f makefile.cpp

# define C preprocessor symbols 
DFLAGS  = -D$(HDFSYS)

# path for #include directive
IFLAG   = -I$(PGSINC)  $(ADD_IFLAGS) 

# path for libraries linked (compiler will search in the order listed)
LFLAG	= -L$(PGSLIB) $(ADD_LFLAGS)

# libraries linked for C build
LIBS   = -lPGSTKcpp $(ADD_LIBS)

# C language subset of object files from this sub-group

C_LIBOFILES	=					\
        $(PGSCPPO)/$(GRP)/PGS_IO_Gen_Close.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_Gen_Open.o 		\
        $(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Delete.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Open.o 	\
        $(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Reference.o


#
# targets
#



all: message cc_obj


cc_obj: 	$(C_LIBOFILES)

lib:
	${PGSBIN}/mkpgslib.cpp $(GRP)

message:
	@echo "    Makefile for subgroup: GEN; Target: all"

clean:
	$(RM) $(RMFLAGS) *.o

	$(RM) $(RMFLAGS) $(C_LIBOFILES)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/PGS_IO_Gen_Close.o: PGS_IO_Gen_Close.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_Gen_Open.o: PGS_IO_Gen_Open.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Delete.o: PGS_IO_Gen_Temp_Delete.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Open.o: PGS_IO_Gen_Temp_Open.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/PGS_IO_Gen_Temp_Reference.o: PGS_IO_Gen_Temp_Reference.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@



# force the default make rule to search for a specific make rule
# for a given input file (i.e. target)
.c.o:
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@

