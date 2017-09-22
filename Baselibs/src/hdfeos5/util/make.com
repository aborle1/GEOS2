INCDIR = -I$(HDFINC) -I$(HDF5INC) -I$(HDFEOS5_INC) -I$(SZIPINC)
LIBDIR = -L$(HDFLIB) -L$(HDF5LIB) -L$(HDFEOS5_LIB) -lhe5_hdfeos -lGctp $(HDF5LIB)/libhdf5.a -ljpeg -lz $(SZIPLIB)/libsz.a -lm
#LIBDIR = -L$(HDFLIB) -L$(HDFEOS_LIB) -lhe5_hdfeos -lGctp $(HDF5LIB)/libhdf5.a -ljpeg -lz $(SZIPLIB)/libsz.a -lm /usr/lib/librpc.a
default all:
	@echo " "; echo " "; \
	if [ "$(HDFINC)" = "" ] || [ "$(HDF5INC)" = "" ] || [ "$(HDFEOS5_INC)" = "" ] || [ "$(SZIPINC)" = "" ] || [ "$(HDFLIB)" = "" ] || [ "$(HDF5LIB)" = "" ] || [ "$(HDFEOS5_LIB)" = "" ] || [ "$(SZIPLIB)" = "" ] ; then \
		echo " --- ERROR: One or more of the environment variables HDFINC,"; \
		echo " --- HDF5INC, HDFEOS5_INC, SZIPINC, HDFLIB, HDF5LIB, HDFEOS5_LIB,"; \
		echo " --- SZIPLIB has not been set. Failed building utility executable."; \
	else \
		echo " ---- Making executable for HE5_GDconvert_ij2ll grid convertor ----"; \
		echo "$(CC) $(CFLAGS) -o HE5_GDconvert_ij2ll.o $(INCDIR) -c HE5_GDconvert_ij2ll.c"; \
		$(CC) $(CFLAGS) -o HE5_GDconvert_ij2ll.o $(INCDIR) -c HE5_GDconvert_ij2ll.c; \
		echo "$(CC) $(CFLAGS) -o $(HDFEOS5_BIN)/HE5_GDconvert_ij2ll HE5_GDconvert_ij2ll.o $(LIBDIR) $(CEXTRAL)"; \
		$(CC) $(CFLAGS) -o $(HDFEOS5_BIN)/HE5_GDconvert_ij2ll HE5_GDconvert_ij2ll.o $(LIBDIR) $(CEXTRAL); \
		$(RM) $(RMFLAGS) *.o;  \
	fi; \
	echo " ";


