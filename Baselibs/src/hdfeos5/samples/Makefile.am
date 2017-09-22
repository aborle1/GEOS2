# samples Makefile.am
# Sample programs for HDF-EOS5

# Boilerplate definitions file
include $(top_srcdir)/config/include.am

# Link against GCTP library and HDF-EOS5
INCLUDES=-I$(top_srcdir)/include -I$(top_srcdir)/gctp/include

LDADD=$(LIBHDFEOS5) $(LIBGCTP)

# The sample programs to be built when the user types 'make check'
check_PROGRAMS=he5_gd_datainfo he5_gd_defexternalfld he5_gd_definefields     \
      he5_gd_defunlimfld he5_gd_info he5_gd_rdexternaldata he5_gd_readdata   \
      he5_gd_writedata he5_gd_setup he5_gd_subset he5_gd_wrexternaldata he5_gd_wrunlimfld     \
      he5_pt_datainfo he5_pt_definelevels he5_pt_readattrs he5_pt_readdata   \
      he5_pt_setup he5_pt_updatelevels he5_pt_writeattrs he5_pt_writedata    \
      he5_sw_datainfo he5_sw_defexternalfld he5_sw_definefields              \
      he5_sw_defunlimfld he5_sw_info he5_sw_rdexternaldata he5_sw_readdata   \
      he5_sw_setup he5_sw_subset he5_sw_wrextendrd he5_sw_wrexternaldata     \
      he5_sw_writedata he5_sw_wrunlimfld he5_za_datainfo                     \
      he5_za_defexternalfld he5_za_definefields he5_za_info                  \
      he5_za_rdexternaldata he5_za_readdata he5_za_setup                     \
      he5_za_wrexternaldata he5_za_writedata
 
# Tests consist of several programs run in succession.  Each of these
# names is the name of such a "sequence" of tests.
TEST_SEQUENCES=gd_write_test gd_unlim_test pt_write_test sw_write_test \
       sw_unlim_test sw_extend_test za_write_test

# 'make check' should run each of the test sequences above
check-local: $(TEST_SEQUENCES)

# Manually list the tests to be run in order for each test sequence.
gd_write_test: $(check_PROGRAMS)
	./he5_gd_setup
	./he5_gd_definefields
	./he5_gd_writedata
	./he5_gd_readdata
	./he5_gd_info
	./he5_gd_datainfo
	./he5_gd_subset

gd_unlim_test: $(check_PROGRAMS)
	./he5_gd_setup
	./he5_gd_defunlimfld
	./he5_gd_wrunlimfld

pt_write_test: $(check_PROGRAMS)
	./he5_pt_setup
	./he5_pt_definelevels
	srcdir=$(srcdir) ./he5_pt_writedata
	./he5_pt_writeattrs
	./he5_pt_readdata
	./he5_pt_readattrs
	./he5_pt_updatelevels
	./he5_pt_datainfo

sw_write_test: $(check_PROGRAMS)
	./he5_sw_setup
	./he5_sw_definefields
	./he5_sw_writedata
	./he5_sw_readdata
	./he5_sw_info
	./he5_sw_datainfo
	./he5_sw_subset

sw_unlim_test: $(check_PROGRAMS)
	./he5_sw_setup
	./he5_sw_defunlimfld
	./he5_sw_wrunlimfld
	./he5_sw_subset

sw_extend_test: $(check_PROGRAMS)
	./he5_sw_setup
	./he5_sw_definefields
	./he5_sw_wrextendrd

za_write_test: $(check_PROGRAMS)
	./he5_za_setup
	./he5_za_definefields
	./he5_za_writedata
	./he5_za_readdata
	./he5_za_info
	./he5_za_datainfo

# This avoids any confusion between the names of the sequences and
# files with the same names.
.PHONY: $(TEST_SEQUENCES)
