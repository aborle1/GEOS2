#
#  Implement esmf specific targets
#

#                          -----------
#                          Environment
#                          -----------

INC_HDF5 = $(prefix)/include/hdf5
LIB_HDF5 = $(wildcard $(foreach lib, hdf5_hl hdf5 z sz curl,\
           $(prefix)/lib/lib$(lib).a) )
hdfeos5.config config: 
	@echo "Configuring HDFEOS5 build..."
	@(cd hdfeos5; \
          export LIBS="-L$(prefix)/lib $(LIB_HDF5) -lmfhdf -ldf -lsz  -ljpeg $(LINK_GPFS) -ldl -lm" ;\
          ./configure --prefix=$(prefix) \
                      --with-hdf5=$(prefix)/include/hdf5,$(prefix)/lib \
                      --with-zlib=$(prefix)/include/zlib/\,$(prefix)/lib \
                      --with-szlib=$(prefix)/include/szlib/\,$(prefix)/lib; \
        )
	@touch hdfeos5.config

hdfeos5.all all hdfeos5.lib lib: hdfeos5.config
	@echo "Customized HDFEOS5 build..."
	@(cd hdfeos5; \
          export LINUX_BRAND=linux64; \
	  export CFLAGS=-DH5_USE_16_API; \
          $(MAKE) install; \
        )

hdfeos5.lib lib: hdfeos5.config
	@echo "Customized HDFEOS5 build..."
	@(cd hdfeos5; \
          export LINUX_BRAND=linux64; \
	  export CFLAGS=-DH5_USE_16_API; \
          $(MAKE) install; \
        )

hdfeos5.install install: hdfeos5.lib
	@echo "Customized HDFEOS5 build..."
	@(cd hdfeos5; \
          export LINUX_BRAND=linux64; \
	  export CFLAGS=-DH5_USE_16_API; \
          $(MAKE) install; \
        )
	@touch hdfeos5.install

hdfeos5.clean clean: 
	@echo "Customized HDFEOS5 build..."
	@(cd hdfeos5; \
          export LINUX_BRAND=linux64; \
	  export CFLAGS=-DH5_USE_16_API; \
          $(MAKE) clean; \
        )

hdfeos5.distclean distclean: 
	@echo "Customized HDFEOS5 build..."
	@(cd hdfeos5; \
          export LINUX_BRAND=linux64; \
	  export CFLAGS=-DH5_USE_16_API; \
          $(MAKE) distclean; \
        )

%: 
	@echo "Customized HDFEOS5 build ... nothing to do for $@"
