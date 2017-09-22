#
#  Implement esmf specific targets
#

#                          -----------
#                          Environment
#                          -----------

hdfeos.config config: 
	@touch hdfeos.config

hdfeos.all all hdfeos.lib lib: hdfeos.config
	@echo "Customized HDFEOS build..."
	@(cd hdfeos; \
          bin/INSTALL-HDFEOS \
                                -i $(prefix)/include/hdf \
                                -l $(prefix)/lib \
				-cc_flag "$(CFLAGS)" \
         )

hdfeos.install install: hdfeos.lib
	@echo "Customized HDFEOS build..."
	@mkdir -p $(prefix)/lib $(prefix)/include/hdfeos
	@cp -p hdfeos/lib/*/lib*.a  $(prefix)/lib
	@cp -p hdfeos/include/*.h     $(prefix)/include/hdfeos
	@chmod +w $(prefix)/include/hdfeos/*
	@touch hdfeos.install

hdfeos.clean clean hdfeos.distclean distclean:
	@echo "Customized HDFEOS build..."
	@/bin/rm -rf  hdfeos/lib/*/lib*.a	
	@/bin/rm -rf  hdfeos/bin/*/.hdf_env
	@/bin/rm -rf  hdfeos/bin/*/hdfeos_env
	@/bin/rm -rf  hdfeos/bin/*/hdfeos_env.ksh
	@/bin/rm -rf  hdfeos/bin/*/hdfeos_env.csh

%: 
	@echo "Customized HDFEOS build ... nothing to do for $@"
