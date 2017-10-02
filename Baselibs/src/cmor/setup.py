import numpy
from numpy.distutils.core import setup, Extension
#from numpy.distutils.ccompiler import CCompiler
import os,sys,string

include_dirs = [numpy.lib.utils.get_include(),"include","include/cdTime"]

library_dirs = [ os.path.join("/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux","lib") ,'.']
include_dirs.append(os.path.join("/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux","include"))
libraries = []

for st in ["-L/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/lib -lnetcdf -ljpeg -lmfhdf -ldf -lhdf5_hl -lhdf5 -ldl -lm -lmfhdf -ldf -lsz -ljpeg -lcurl -lssl -lcrypto -lssl -lcrypto -lz -lm -ldl -lm", "-I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include -DgFortran -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/ -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/zlib -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/szlib -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/jpeg -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/hdf5 -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/hdf -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/uuid -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/netcdf -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/udunits2", " -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/udunits2", " -L/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/lib  -Wl,-rpath=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/lib -ludunits2 -lexpat", " -I/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/include/uuid", " -L/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/lib  -Wl,-rpath=/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu/gfortran_5.4.0-openmpi_1.10.2/Linux/lib -luuid"]:
   sp = st.strip().split()
   for s in sp:
      if s[:2]=='-L':
        library_dirs.append(s[2:])
      if s[:2]=='-l':
        libraries.append(s[2:])
      if s[:2]=='-I':
        include_dirs.append(s[2:])

srcfiles = "Src/cmor.c Src/cmor_variables.c Src/cmor_axes.c Src/cmor_tables.c Src/cmor_grids.c Src/cdTime/cdTimeConv.c Src/cdTime/cdUtil.c Src/cdTime/timeConv.c Src/cdTime/timeArith.c Src/cmor_cfortran_interface.c Src/cmor_md5.c".split()
srcfiles.insert(0,os.path.join("Src","_cmormodule.c"))

macros=[]
for m in " -DCOLOREDOUTPUT".split():
   macros.append((m[2:],None))
   
ld =[]
for p in library_dirs:
   if os.path.exists(p):
      ld.append(p)
library_dirs=ld

ld =[]
for p in include_dirs:
   if os.path.exists(p):
      ld.append(p)
include_dirs=ld

print 'Setting up python module with:'
print 'libraries:',libraries
print 'libdir:',library_dirs
print 'incdir',include_dirs
print 'src:',srcfiles
print 'macros:',macros

setup (name = "CMOR",
       version='2.0',
       author='Charles Doutriaux, PCMDI',
       description = "Python Interface to CMOR output library",
       url = "http://www-pcmdi.llnl.gov/cmor",
       packages = ['cmor'],
       package_dir = {'cmor': 'Lib'},
       ext_modules = [
    Extension('cmor._cmor',
              srcfiles,
	      include_dirs = include_dirs,
              library_dirs = library_dirs,
              libraries = libraries,
              define_macros = macros,
              extra_compile_args = [ "-g", ]
              ),
    
    ]
      )

