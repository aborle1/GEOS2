#-----------------------------------------------------------------------------
# file:         makefile for GCT tools
#
# environment:  MPPDE, machine-independent, PGS directory structure
#
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#
# author: Mike Sucher / Graham Bland
#         Carol S. W. Tsai / Space Applications Corporation 
#         Megan E.D.Spencer / SM&A
# history:
#       01-Apr-1994 MES Initial version
#	02-JAN-1995 ANS Adopted for GCT
#       11-Aug-1997 CSWT Added new object files sinusfor.o, isinusinv.o,
#                        utmfor.o, and utminv.o 
# notes:
#       1) This file is intended for use in the Multi-Platform PGS Development
#          Environment (MPPDE) .  It depends on the PGS-defined toolkit
#          directory structure, and on environmental variables defined
#          by MPPDE startup files.  This is how machine independence
#          is maintained.
#       2) Target executable files are moved to $PGSBIN
#       3) Target object files are moved to $(PGSCPPO)/GCT
#       4) To build the PGS AA tools library, type 'make libGCT.a'
#          (This runs the script $PGSBIN/mkpgslib to build the library).
#----------------------------------------------------------------------------

GRP = GCT
# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# Name of the remove utility and flags
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp
# define C preprocessor symbols
DFLAGS  = -D$(HDFSYS) 

# path for #include directive
IFLAG   = -I$(PGSINC) -I$(PGSINC)/FW

# object files from this sub-group needed to build library

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/alberfor.o \
	$(PGSCPPO)/$(GRP)/alberinv.o \
	$(PGSCPPO)/$(GRP)/alconfor.o \
	$(PGSCPPO)/$(GRP)/alconinv.o \
	$(PGSCPPO)/$(GRP)/azimfor.o \
	$(PGSCPPO)/$(GRP)/aziminv.o \
	$(PGSCPPO)/$(GRP)/bceafor.o \
	$(PGSCPPO)/$(GRP)/bceainv.o \
	$(PGSCPPO)/$(GRP)/ceafor.o \
	$(PGSCPPO)/$(GRP)/ceainv.o \
	$(PGSCPPO)/$(GRP)/cproj.o \
	$(PGSCPPO)/$(GRP)/eqconfor.o \
	$(PGSCPPO)/$(GRP)/eqconinv.o \
	$(PGSCPPO)/$(GRP)/equifor.o \
	$(PGSCPPO)/$(GRP)/equiinv.o \
	$(PGSCPPO)/$(GRP)/gnomfor.o \
	$(PGSCPPO)/$(GRP)/gnominv.o \
	$(PGSCPPO)/$(GRP)/goodfor.o \
	$(PGSCPPO)/$(GRP)/goodinv.o \
	$(PGSCPPO)/$(GRP)/gvnspfor.o \
	$(PGSCPPO)/$(GRP)/gvnspinv.o \
	$(PGSCPPO)/$(GRP)/hamfor.o \
	$(PGSCPPO)/$(GRP)/haminv.o \
	$(PGSCPPO)/$(GRP)/imolwfor.o \
	$(PGSCPPO)/$(GRP)/imolwinv.o \
	$(PGSCPPO)/$(GRP)/lamazfor.o \
	$(PGSCPPO)/$(GRP)/lamazinv.o  \
	$(PGSCPPO)/$(GRP)/lamccfor.o \
	$(PGSCPPO)/$(GRP)/lamccinv.o \
	$(PGSCPPO)/$(GRP)/merfor.o \
	$(PGSCPPO)/$(GRP)/merinv.o \
	$(PGSCPPO)/$(GRP)/millfor.o \
	$(PGSCPPO)/$(GRP)/millinv.o \
	$(PGSCPPO)/$(GRP)/molwfor.o \
	$(PGSCPPO)/$(GRP)/molwinv.o \
	$(PGSCPPO)/$(GRP)/obleqfor.o \
	$(PGSCPPO)/$(GRP)/obleqinv.o \
	$(PGSCPPO)/$(GRP)/omerfor.o \
	$(PGSCPPO)/$(GRP)/omerinv.o \
	$(PGSCPPO)/$(GRP)/orthfor.o \
	$(PGSCPPO)/$(GRP)/orthinv.o \
	$(PGSCPPO)/$(GRP)/paksz.o \
	$(PGSCPPO)/$(GRP)/polyfor.o \
	$(PGSCPPO)/$(GRP)/polyinv.o \
	$(PGSCPPO)/$(GRP)/psfor.o \
	$(PGSCPPO)/$(GRP)/psinv.o \
	$(PGSCPPO)/$(GRP)/report.o \
	$(PGSCPPO)/$(GRP)/robfor.o \
	$(PGSCPPO)/$(GRP)/robinv.o \
	$(PGSCPPO)/$(GRP)/sinfor.o \
	$(PGSCPPO)/$(GRP)/sininv.o \
	$(PGSCPPO)/$(GRP)/somfor.o \
	$(PGSCPPO)/$(GRP)/sominv.o \
	$(PGSCPPO)/$(GRP)/sphdz.o \
	$(PGSCPPO)/$(GRP)/sterfor.o \
	$(PGSCPPO)/$(GRP)/sterinv.o \
	$(PGSCPPO)/$(GRP)/stplnfor.o \
	$(PGSCPPO)/$(GRP)/stplninv.o \
	$(PGSCPPO)/$(GRP)/tmfor.o \
	$(PGSCPPO)/$(GRP)/tminv.o \
	$(PGSCPPO)/$(GRP)/untfz.o \
	$(PGSCPPO)/$(GRP)/vandgfor.o \
	$(PGSCPPO)/$(GRP)/vandginv.o \
	$(PGSCPPO)/$(GRP)/wivfor.o \
	$(PGSCPPO)/$(GRP)/wivinv.o\
	$(PGSCPPO)/$(GRP)/wviifor.o \
	$(PGSCPPO)/$(GRP)/wviiinv.o \
	$(PGSCPPO)/$(GRP)/isinusfor.o \
	$(PGSCPPO)/$(GRP)/isinusinv.o \
	$(PGSCPPO)/$(GRP)/utmfor.o \
	$(PGSCPPO)/$(GRP)/utminv.o 

INIT_FUNCS = \
	$(PGSCPPO)/$(GRP)/for_init.o \
	$(PGSCPPO)/$(GRP)/inv_init.o

#
# targets
#

all:    $(LIBOFILES)

lib:
	@$(PGSBIN)/mkpgslib.cpp GCT

clean:
	/bin/rm -f *.o core
	$(RM) $(RMFLAGS) $(LIBOFILES)

# The following two targets are for use in building the HDF-EOS version of this
# code.  This is not part of the SDP Toolkit.

gctp:
	@$(MAKE) $(MFLAGS) PGSCPPO=$(HDFEOS_OBJ) \
	 GRP="" DFLAGS=-D$(HDFSYS) $(HDFEOS_LIB)/libGctp.a
gctp5:
	@$(MAKE) $(MFLAGS) PGSCPPO=$(HDFEOS5_OBJ) \
	 GRP="" DFLAGS=-D$(HDFSYS) $(HDFEOS5_LIB)/libGctp.a

$(HDFEOS_LIB)/libGctp.a: $(INIT_FUNCS) $(LIBOFILES)
	ar r $@ $?
$(HDFEOS5_LIB)/libGctp.a: $(INIT_FUNCS) $(LIBOFILES)
	ar r $@ $?

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/alberfor.o: alberfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/alberinv.o: alberinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/alconfor.o: alconfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/alconinv.o: alconinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/azimfor.o: azimfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/aziminv.o: aziminv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/bceafor.o: bceafor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/bceainv.o: bceainv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/ceafor.o: ceafor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/ceainv.o: ceainv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/cproj.o: cproj.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/eqconfor.o: eqconfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/eqconinv.o: eqconinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/equifor.o: equifor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/equiinv.o: equiinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/for_init.o: for_init.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/gnomfor.o: gnomfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/gnominv.o: gnominv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/goodfor.o: goodfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/goodinv.o: goodinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/gvnspfor.o: gvnspfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/gvnspinv.o: gvnspinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/hamfor.o: hamfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/haminv.o: haminv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/imolwfor.o: imolwfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/imolwinv.o: imolwinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/inv_init.o: inv_init.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/lamazfor.o: lamazfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/lamazinv.o: lamazinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/lamccfor.o: lamccfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/lamccinv.o: lamccinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/merfor.o: merfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/merinv.o: merinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/millfor.o: millfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/millinv.o: millinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/molwfor.o: molwfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/molwinv.o: molwinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/obleqfor.o: obleqfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/obleqinv.o: obleqinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/omerfor.o: omerfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/omerinv.o: omerinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/orthfor.o: orthfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/orthinv.o: orthinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/paksz.o: paksz.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/polyfor.o: polyfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/polyinv.o: polyinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/psfor.o: psfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/psinv.o: psinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/report.o: report.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS)-DTOOLKIT_COMPILE $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/robfor.o: robfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/robinv.o: robinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sinfor.o: sinfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sininv.o: sininv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/somfor.o: somfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sominv.o: sominv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sphdz.o: sphdz.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sterfor.o: sterfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/sterinv.o: sterinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/stplnfor.o: stplnfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/stplninv.o: stplninv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/tmfor.o: tmfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/tminv.o: tminv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/untfz.o: untfz.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vandgfor.o: vandgfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/vandginv.o: vandginv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wivfor.o: wivfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wivinv.o: wivinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wviifor.o: wviifor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/wviiinv.o: wviiinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/isinusfor.o: isinusfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/isinusinv.o: isinusinv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/utmfor.o: utmfor.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/utminv.o: utminv.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# compile C to object
.c.o:
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@





