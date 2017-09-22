# ESMF application makefile fragment
#
# Use the following ESMF_ variables to compile and link
# your ESMF application against this ESMF build.
#
# !!! VERY IMPORTANT: If the location of this ESMF build is   !!!
# !!! changed, e.g. libesmf.a is copied to another directory, !!!
# !!! this file - esmf.mk - must be edited to adjust to the   !!!
# !!! correct new path                                        !!!
#
# Please see end of file for options used on this ESMF build
#


ESMF_VERSION_STRING=5.2.0rp3

ESMF_VERSION_MAJOR=5
ESMF_VERSION_MINOR=2
ESMF_VERSION_REVISION=0
ESMF_VERSION_PATCHLEVEL=3
ESMF_VERSION_PUBLIC='T'
ESMF_VERSION_BETASNAPSHOT='F'


ESMF_APPSDIR=/home/aborle1/Baselibs/src/esmf/apps/appsO/Linux.gfortran.32.openmpi.default


ESMF_F90COMPILER=mpifort
ESMF_F90LINKER=mpifort

ESMF_F90COMPILEOPTS=-O -fPIC -fno-second-underscore -pthread -ffree-line-length-none
ESMF_F90COMPILEPATHS=-I/home/aborle1/Baselibs/src/esmf/mod/modO/Linux.gfortran.32.openmpi.default -I/home/aborle1/Baselibs/src/esmf/src/include 
ESMF_F90COMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMF_MPIIO -DESMF_NO_OPENMP -DS32=1 -DESMF_OS_Linux=1
ESMF_F90COMPILEFREECPP=
ESMF_F90COMPILEFREENOCPP=-ffree-form
ESMF_F90COMPILEFIXCPP=-cpp -ffixed-form
ESMF_F90COMPILEFIXNOCPP=

ESMF_F90LINKOPTS= -fno-second-underscore -pthread
ESMF_F90LINKPATHS=-L/home/aborle1/Baselibs/src/esmf/lib/libO/Linux.gfortran.32.openmpi.default  -L/usr/lib/gcc/powerpc64le-linux-gnu/5/
ESMF_F90LINKRPATHS=-Wl,-rpath,/home/aborle1/Baselibs/src/esmf/lib/libO/Linux.gfortran.32.openmpi.default  -Wl,-rpath,/usr/lib/gcc/powerpc64le-linux-gnu/5/
ESMF_F90LINKLIBS= -lmpi_cxx -lrt -lstdc++ -ldl
ESMF_F90ESMFLINKLIBS=-lesmf  -lmpi_cxx -lrt -lstdc++ -ldl

ESMF_CXXCOMPILER=mpicxx
ESMF_CXXLINKER=mpicxx

ESMF_CXXCOMPILEOPTS=-O -DNDEBUG -fPIC -DESMF_LOWERCASE_SINGLEUNDERSCORE -pthread
ESMF_CXXCOMPILEPATHS= -I/home/aborle1/Baselibs/src/esmf/src/include  
ESMF_CXXCOMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMF_MPIIO -DESMF_NO_OPENMP -DS32=1 -DESMF_OS_Linux=1 -D__SDIR__='' -DESMF_NO_SIGUSR2

ESMF_CXXLINKOPTS= -pthread
ESMF_CXXLINKPATHS=-L/home/aborle1/Baselibs/src/esmf/lib/libO/Linux.gfortran.32.openmpi.default  -L/usr/lib/gcc/powerpc64le-linux-gnu/5/
ESMF_CXXLINKRPATHS=-Wl,-rpath,/home/aborle1/Baselibs/src/esmf/lib/libO/Linux.gfortran.32.openmpi.default  -Wl,-rpath,/usr/lib/gcc/powerpc64le-linux-gnu/5/
ESMF_CXXLINKLIBS= -lmpi_mpifh -lrt -lgfortran -ldl
ESMF_CXXESMFLINKLIBS=-lesmf  -lmpi_mpifh -lrt -lgfortran -ldl

ESMF_SO_F90COMPILEOPTS=-fPIC
ESMF_SO_F90LINKOPTS=-shared
ESMF_SO_F90LINKOPTSEXE=-Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS=-fPIC
ESMF_SO_CXXLINKOPTS=-shared
ESMF_SO_CXXLINKOPTSEXE=-Wl,-export-dynamic

ESMF_OPENMP_F90COMPILEOPTS= -fopenmp
ESMF_OPENMP_F90LINKOPTS= -fopenmp
ESMF_OPENMP_CXXCOMPILEOPTS= -fopenmp
ESMF_OPENMP_CXXLINKOPTS= -fopenmp

# Internal ESMF variables, do NOT depend on these!

ESMF_INTERNAL_DIR=/home/aborle1/Baselibs/src/esmf

#
# !!! The following options were used on this ESMF build !!!
#
# ESMF_DIR: /home/aborle1/Baselibs/src/esmf
# ESMF_OS: Linux
# ESMF_MACHINE: ppc64le
# ESMF_ABI: 32
# ESMF_COMPILER: gfortran
# ESMF_BOPT: O
# ESMF_COMM: openmpi
# ESMF_SITE: default
# ESMF_PTHREADS: ON
# ESMF_OPENMP: OFF
# ESMF_ARRAY_LITE: FALSE
# ESMF_NO_INTEGER_1_BYTE: TRUE
# ESMF_NO_INTEGER_2_BYTE: TRUE
# ESMF_FORTRANSYMBOLS: default
# ESMF_DEFER_LIB_BUILD:   ON
# 
# ESMF environment variables pointing to 3rd party software:
