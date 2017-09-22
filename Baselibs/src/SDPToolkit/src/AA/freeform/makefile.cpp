#-------------------------------------------------------------------------#
#                                                                         #
#  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  #
#  and suppliers.  ALL RIGHTS RESERVED.                                   #
#                                                                         #
#-------------------------------------------------------------------------#
#-----------------------------------------------------------------------------
# file:		makefile for AA tools 
#		(for freeform only)
#
# environment:	MPPDE, machine-independent, PGS directory structure
# 	
# environment variables dependencies:
#   compiler:  CC CFHFLAGS F77 F77_CFH
#   includes:  PGSINC
#   libraries: PGSLIB
#   other:     PGSBIN PGSSRC HDFSYS
#  
# author: Mike Sucher / Richard Morris / Guru Tej Khalsa
#
# notes:
# 	1) This file is intended for use in the Multi-Platform PGS Development
# 	   Environment (MPPDE) .  It depends on the PGS-defined toolkit 
# 	   directory structure, and on environmental variables defined
# 	   by MPPDE startup files.  This is how machine independence
# 	   is maintained.
# 	2) Target object files are built to $(PGSOBJ)/AA
# 	4) To build the PGS AA tools library, type 'make lib'
#	   (This runs the script $PGSBIN/mkpgslib to build the library).
#----------------------------------------------------------------------------

#
# Set the name of this tool group
#

GRP= AA

# define sh shell (needed for embedded shell scripts)
SHELL=/bin/sh

# define C preprocessor symbols 
OPSYS = SUNCC
DFLAGS = -D$(OPSYS) -DPROTO

# path for #include directive
IFLAG   = -I$(HDFINC) -I$(PGSINC) -I$(PGSINC)/FF

# Name of the remove utility and flags
RM= /bin/rm
RMFLAGS= -f 
MFLAGS2= -f makefile.cpp

# object files from this sub-group needed to build library  

LIBOFILES = \
	$(PGSCPPO)/$(GRP)/afm2bfm.o \
        $(PGSCPPO)/$(GRP)/byteswap.o \
        $(PGSCPPO)/$(GRP)/cv_units.o \
        $(PGSCPPO)/$(GRP)/dbevents.o \
        $(PGSCPPO)/$(GRP)/dbfree.o \
        $(PGSCPPO)/$(GRP)/dbhdlen.o \
        $(PGSCPPO)/$(GRP)/dl_lists.o \
        $(PGSCPPO)/$(GRP)/dupform.o \
        $(PGSCPPO)/$(GRP)/dvevents.o \
        $(PGSCPPO)/$(GRP)/eqn_util.o \
        $(PGSCPPO)/$(GRP)/error.o\
        $(PGSCPPO)/$(GRP)/eval_eqn.o \
        $(PGSCPPO)/$(GRP)/fflookup.o \
        $(PGSCPPO)/$(GRP)/file2buf.o \
        $(PGSCPPO)/$(GRP)/findfile.o \
        $(PGSCPPO)/$(GRP)/findvar.o \
        $(PGSCPPO)/$(GRP)/formlist.o \
        $(PGSCPPO)/$(GRP)/freeform.o \
        $(PGSCPPO)/$(GRP)/freeview.o \
        $(PGSCPPO)/$(GRP)/frm2vlst.o \
        $(PGSCPPO)/$(GRP)/geo44tim.o \
        $(PGSCPPO)/$(GRP)/get_doub.o \
        $(PGSCPPO)/$(GRP)/get_str.o \
        $(PGSCPPO)/$(GRP)/get_val.o \
        $(PGSCPPO)/$(GRP)/getcount.o \
        $(PGSCPPO)/$(GRP)/latlon.o \
        $(PGSCPPO)/$(GRP)/makedbin.o \
        $(PGSCPPO)/$(GRP)/makeform.o \
        $(PGSCPPO)/$(GRP)/makeindx.o \
        $(PGSCPPO)/$(GRP)/makeview.o \
	$(PGSCPPO)/$(GRP)/memtrack.o \
        $(PGSCPPO)/$(GRP)/mkformat.o \
        $(PGSCPPO)/$(GRP)/mkplist.o \
	$(PGSCPPO)/$(GRP)/mkstdbin.o \
        $(PGSCPPO)/$(GRP)/name_tab.o \
        $(PGSCPPO)/$(GRP)/os_utils.o \
        $(PGSCPPO)/$(GRP)/proclist.o \
        $(PGSCPPO)/$(GRP)/qstack.o \
	$(PGSCPPO)/$(GRP)/seismic.o \
        $(PGSCPPO)/$(GRP)/set_var.o \
        $(PGSCPPO)/$(GRP)/setdbin.o \
        $(PGSCPPO)/$(GRP)/setview.o \
        $(PGSCPPO)/$(GRP)/showdbin.o \
        $(PGSCPPO)/$(GRP)/showform.o \
        $(PGSCPPO)/$(GRP)/showvars.o \
        $(PGSCPPO)/$(GRP)/showview.o \
        $(PGSCPPO)/$(GRP)/skiphead.o \
	$(PGSCPPO)/$(GRP)/str2bin.o \
        $(PGSCPPO)/$(GRP)/strnstr.o \
        $(PGSCPPO)/$(GRP)/time.o \
        $(PGSCPPO)/$(GRP)/tytoty.o \
        $(PGSCPPO)/$(GRP)/viewsize.o \
        $(PGSCPPO)/$(GRP)/writform.o

#
# targets
#
all: message
	@case "$(HDFINC)" in	 \
	    "") \
	        echo "No HDF include directory specified - AA/freeform will NOT be built" ; \
	        echo "" ; \
	        ;; \
	    *) \
		case "$(BRAND)" in \
		   dec)  dflags="-DDEC_ALPHA $(DFLAGS)" ;; \
		   hp) dflags="-DHP9000 $(DFLAGS)" ;; \
		   ibm) dflags="-DIBM6000 $(DFLAGS)" ;; \
		   linux) dflags="-DLINUX $(DFLAGS)" ;; \
		   sco) dflags="-DLINUX $(DFLAGS)" ;; \
		   sgi) dflags="-DIRIS4 $(DFLAGS)" ;; \
		   sgi32) dflags="-DIRIS4 $(DFLAGS)" ;; \
		   sgi64) dflags="-DIRIX $(DFLAGS)" ;; \
		   sun4) dflags="-DSUN $(DFLAGS)" ;; \
		   sun5) dflags="-DSUN $(DFLAGS)" ;; \
                   sun5.8) dflags="-DSUN $(DFLAGS)" ;; \
                   sun5.9) dflags="-DSUN $(DFLAGS)" ;; \
                   sun5.10) dflags="-DSUN $(DFLAGS)" ;; \
		   *) dflags="$(DFLAGS)" ;; \
		esac ; \
		$(MAKE) $(MFLAGS) $(MFLAGS2)  DFLAGS="$$dflags" ff_all; \
	        ;; \
	esac                                

ff_all:	$(LIBOFILES)

lib:
	@${PGSBIN}/mkpgslib.cpp $(GRP)

message:
	@echo "    Makefile for AA/freeform; Target: all"

clean:
	$(RM) $(RMFLAGS) core *.o
	$(RM) $(RMFLAGS) $(LIBOFILES)

#
# compilation rules
#

$(PGSCPPO)/$(GRP)/afm2bfm.o: afm2bfm.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/byteswap.o: byteswap.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/cv_units.o: cv_units.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dbevents.o: dbevents.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dbfree.o: dbfree.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dbhdlen.o: dbhdlen.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dl_lists.o: dl_lists.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dupform.o: dupform.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/dvevents.o: dvevents.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/eqn_util.o: eqn_util.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/error.o: error.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/eval_eqn.o: eval_eqn.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/fflookup.o: fflookup.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/file2buf.o: file2buf.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/findfile.o: findfile.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/findvar.o: findvar.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/formlist.o: formlist.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/freeform.o: freeform.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/freeview.o: freeview.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/frm2vlst.o: frm2vlst.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/geo44tim.o: geo44tim.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/get_doub.o: get_doub.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/get_str.o: get_str.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/get_val.o: get_val.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/getcount.o: getcount.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/latlon.o: latlon.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/makedbin.o: makedbin.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/makeform.o: makeform.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/makeindx.o: makeindx.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/makeview.o: makeview.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/memtrack.o: memtrack.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/mkformat.o: mkformat.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/mkplist.o: mkplist.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/mkstdbin.o: mkstdbin.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/name_tab.o: name_tab.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/os_utils.o: os_utils.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/proclist.o: proclist.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/qstack.o: qstack.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/seismic.o: seismic.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/set_var.o: set_var.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/setdbin.o: setdbin.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/setview.o: setview.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/showdbin.o: showdbin.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/showform.o: showform.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/showvars.o: showvars.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/showview.o: showview.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/skiphead.o: skiphead.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/str2bin.o: str2bin.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/strnstr.o: strnstr.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/time.o: time.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/tytoty.o: tytoty.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/viewsize.o: viewsize.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

$(PGSCPPO)/$(GRP)/writform.o: writform.c
	$(CPP) -c $(CPPFHFLAGS) $(DFLAGS) $(IFLAG) $? -o $@

# compile C to object
.c.o:
	@case "$(BRAND)" in \
	    dec)  dflags="-DDEC_ALPHA $(DFLAGS)" ;; \
	    hp) dflags="-DHP9000 $(DFLAGS)" ;; \
	    ibm) dflags="-DIBM6000 $(DFLAGS)" ;; \
	    linux) dflags="-DLINUX $(DFLAGS)" ;; \
	    sco) dflags="-DLINUX $(DFLAGS)" ;; \
	    sgi) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi32) dflags="-DIRIS4 $(DFLAGS)" ;; \
	    sgi64) dflags="-DIRIX $(DFLAGS)" ;; \
	    sun4) dflags="-DSUN $(DFLAGS)" ;; \
	    sun5) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.8) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.9) dflags="-DSUN $(DFLAGS)" ;; \
            sun5.10) dflags="-DSUN $(DFLAGS)" ;; \
	    *) dflags="$(DFLAGS)" ;; \
	esac ; \
	$(MAKE) $(MFLAGS) $(MFLAGS2) $(PGSCPPO)/$(GRP)/$@ DFLAGS="$$dflags"
