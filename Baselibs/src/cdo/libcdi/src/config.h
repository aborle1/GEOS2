/* src/config.h.  Generated from config.h.in by configure.  */
/* src/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* CDI version */
#define CDI "1.7.2"

/* Compiler */
#define COMPILER "mpicc -fPIC  "

/* Compiler version */
#define COMP_VERSION "gcc: error: unrecognized command line option '-V'"

/* Defined if backtrace() could be fully identified. */
#define HAVE_BACKTRACE 1

/* Defined to 1 if C / Fortran interface cfortran.h works */
#define HAVE_CF_INTERFACE 1

/* Define to 1 if you have the declaration of `isnan', and to 0 if you don't.
   */
#define HAVE_DECL_ISNAN 1

/* Define to 1 if you have the declaration of `PAGESIZE', and to 0 if you
   don't. */
#define HAVE_DECL_PAGESIZE 0

/* Define to 1 if you have the declaration of `PAGE_SIZE', and to 0 if you
   don't. */
#define HAVE_DECL_PAGE_SIZE 0

/* Define to 1 if you have the declaration of `POSIX_REC_XFER_ALIGN', and to 0
   if you don't. */
#define HAVE_DECL_POSIX_REC_XFER_ALIGN 0

/* Define to 1 if you have the declaration of `uuid_create', and to 0 if you
   don't. */
/* #undef HAVE_DECL_UUID_CREATE */

/* Define to 1 if you have the declaration of `uuid_generate', and to 0 if you
   don't. */
/* #undef HAVE_DECL_UUID_GENERATE */

/* Define to 1 if you have the declaration of `_PC_REC_XFER_ALIGN', and to 0
   if you don't. */
#define HAVE_DECL__PC_REC_XFER_ALIGN 1

/* Define to 1 if you have the declaration of `_SC_LARGE_PAGESIZE', and to 0
   if you don't. */
#define HAVE_DECL__SC_LARGE_PAGESIZE 0

/* Define to 1 if you have the declaration of `_SC_PAGESIZE', and to 0 if you
   don't. */
#define HAVE_DECL__SC_PAGESIZE 1

/* Define to 1 if you have the declaration of `_SC_PAGE_SIZE', and to 0 if you
   don't. */
#define HAVE_DECL__SC_PAGE_SIZE 1

/* Define to 1 if __builtin_ctz is available, 0 if not */
#define HAVE_DECL___BUILTIN_CTZ 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <execinfo.h> header file. */
#define HAVE_EXECINFO_H 1

/* Define to 1 if you have the `getline' function. */
#define HAVE_GETLINE 1

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the <grib_api.h> header file. */
/* #undef HAVE_GRIB_API_H */

/* Define to 1 if you have the `grib_get_length' function. */
/* #undef HAVE_GRIB_GET_LENGTH */

/* Define to 1 for H5get_libversion support */
#define HAVE_H5GET_LIBVERSION 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <jasper.h> header file. */
/* #undef HAVE_JASPER_H */

/* Define to 1 for GRIB1 decoding/encoding with cgribex */
#define HAVE_LIBCGRIBEX 1

/* Define to 1 for EXTRA interface */
#define HAVE_LIBEXTRA 1

/* Define to 1 for GRIB support */
/* #undef HAVE_LIBGRIB */

/* GRIB_API library is present if defined to 1 */
/* #undef HAVE_LIBGRIB_API */

/* Define to 1 for IEG interface */
#define HAVE_LIBIEG 1

/* Define to 1 for JPEG compression for GRIB2 */
/* #undef HAVE_LIBJASPER */

/* Define to 1 for PNG compression for GRIB2 */
/* #undef HAVE_LIBLIBPNG */

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `malloc' library (-lmalloc). */
/* #undef HAVE_LIBMALLOC */

/* Define to 1 for NetCDF OpenDAP */
#define HAVE_LIBNC_DAP 1

/* Define to 1 for NetCDF support */
#define HAVE_LIBNETCDF 1

/* Define to 1 for JPEG compression for GRIB2 */
/* #undef HAVE_LIBOPENJPEG */

/* Define to 1 if you have the `pthread' library (-lpthread). */
#define HAVE_LIBPTHREAD 1

/* Define to 1 for SERVICE interface */
#define HAVE_LIBSERVICE 1

/* Define to 1 for SZIP support */
#define HAVE_LIBSZ 1

/* Define to 1 if you have the `mallinfo' function. */
#define HAVE_MALLINFO 1

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have a working `mmap' system call. */
#define HAVE_MMAP 1

/* Define to 1 for NetCDF4/HDF5 support */
#define HAVE_NC4HDF5 1

/* Define to 1 for NetCDF4/HDF5 threadsafe support */
/* #undef HAVE_NC4HDF5_THREADSAFE */

/* Define to 1 for NetCDF2 support */
#define HAVE_NETCDF2 1

/* Define to 1 for NetCDF4 support */
#define HAVE_NETCDF4 1

/* Define to 1 if you have the <netcdf.h> header file. */
#define HAVE_NETCDF_H 1

/* Define to 1 if you have the <netcdf_par.h> header file. */
/* #undef HAVE_NETCDF_PAR_H */

/* Define to 1 if you have the <openjpeg.h> header file. */
/* #undef HAVE_OPENJPEG_H */

/* netCDF library does support MPI parallel invocations */
/* #undef HAVE_PARALLEL_NC4 */

/* Define to 1 if you have the <png.h> header file. */
/* #undef HAVE_PNG_H */

/* ScalES PPM C core library is available */
/* #undef HAVE_PPM_CORE */

/* Define to 1 if you have the <pthread.h> header file. */
/* #undef HAVE_PTHREAD_H */

/* Have PTHREAD_PRIO_INHERIT. */
#define HAVE_PTHREAD_PRIO_INHERIT 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if `st_blksize' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <szlib.h> header file. */
#define HAVE_SZLIB_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `uuid_create' function. */
/* #undef HAVE_UUID_CREATE */

/* Define to 1 if you have the <uuid.h> header file. */
/* #undef HAVE_UUID_H */

/* Define to 1 if you have the <uuid/uuid.h> header file. */
/* #undef HAVE_UUID_UUID_H */

/* yaxt library is available */
/* #undef HAVE_YAXT */

/* Host name */
#define HOST_NAME "p8umbc1"

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "cdi"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "http://mpimet.mpg.de/cdi"

/* Define to the full name of this package. */
#define PACKAGE_NAME "cdi"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "cdi 1.7.2"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "cdi"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.7.2"

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* System type */
#define SYSTEM_TYPE "/home/aborle1/Baselibs/powerpc64le-unknown-linux-gnu"

/* User name */
#define USER_NAME "aborle1"

/* parallel I/O requested and available */
/* #undef USE_MPI */

/* Version number of package */
#define VERSION "1.7.2"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Defined to return type of backtrace(). */
#define backtrace_size_t int

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#define restrict __restrict
/* Work around a bug in Sun C++: it does not support _Restrict or
   __restrict__, even though the corresponding Sun C compiler ends up with
   "#define restrict _Restrict" or "#define restrict __restrict__" in the
   previous line.  Perhaps some future version of Sun C++ will work with
   restrict; if so, hopefully it defines __RESTRICT like Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
# define __restrict__
#endif
