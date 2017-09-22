#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Prototype configure.ac for HDF-EOS
# Created 5/2/06 by James Laird (jlaird@ncsa.uiuc.edu)

AC_PREREQ(2.59)
AC_INIT("hdf-eos5", 1.8, null@bogus.email)
AM_CONFIG_HEADER([include/HE5_config.h])

AC_CONFIG_AUX_DIR([config])

AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE

# Disable shared libraries (for now)
AC_DISABLE_SHARED

AM_PROG_LIBTOOL

dnl ----------------------------------------------------------------------
dnl Set prefix default (install directory) to a directory in the build area.
AC_PREFIX_DEFAULT([`pwd`/hdfeos5])

# Check for the math library that HDF5 needs
AC_CHECK_LIB([m], [ceil])

# Check whether zlib is enabled.
# HDF-EOS5 doesn't use zlib, but HDF5 might.  If HDF5 does, it needs
# to be given to the linker or there will be undefined symbols in HDF5.
# Using h5cc avoids the need to specify zlib at configure time.
AC_ARG_WITH([zlib],
            [AC_HELP_STRING([--with-zlib=DIR],
                            [Specify path to external zlib library.
                             Linker must be able to find zlib if HDF5
                             was built with zlib.
                             [default=yes]])],,
            withval=yes)

case $withval in
  yes)
    HAVE_ZLIB="yes"
    AC_CHECK_HEADERS([zlib.h],)
    AC_CHECK_LIB([z], [compress2],, [unset HAVE_ZLIB])

    if test -z "$HAVE_ZLIB"; then
      AC_MSG_ERROR([couldn't find zlib library])
    fi
    ;;
  no)
    HAVE_ZLIB="no"
    AC_MSG_CHECKING([for GNU zlib])
    AC_MSG_RESULT([suppressed])
    ;;
  *)
    HAVE_ZLIB="yes"
    case "$withval" in
      *,*)
        zlib_inc="`echo $withval |cut -f1 -d,`"
        zlib_lib="`echo $withval |cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          zlib_inc="$withval/include"
          zlib_lib="$withval/lib"
        fi
        ;;
    esac

    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$zlib_inc" = "X/usr/include"; then
      zlib_inc=""
    fi
    if test "X$zlib_lib" = "X/usr/lib"; then
      zlib_lib=""
    fi

    saved_CPPFLAGS="$CPPFLAGS"
    saved_LDFLAGS="$LDFLAGS"

    if test -n "$zlib_inc"; then
      CPPFLAGS="$CPPFLAGS -I$zlib_inc"
    fi

    AC_CHECK_HEADERS([zlib.h], ,
                     [CPPFLAGS="$saved_CPPFLAGS"])

    if test -n "$zlib_lib"; then
      LDFLAGS="$LDFLAGS -L$zlib_lib"
    fi

    AC_CHECK_LIB([z], [compress2],,
                 [LDFLAGS="$saved_LDFLAGS"; unset HAVE_ZLIB])

    if test -z "$HAVE_ZLIB"; then
      AC_MSG_ERROR([couldn't find zlib library])
    fi
    ;;
esac


# Check whether szlib (szip) is enabled.
# HDF-EOS5 doesn't use szlib, but HDF5 might.  If HDF5 does, it needs
# to be given to the linker or there will be undefined symbols in HDF5.
# Using h5cc should avoid the need to specify szlib at configure time.
AC_ARG_WITH([szlib],
            [AC_HELP_STRING([--with-szlib=DIR],
                            [Use szlib library for external szlib I/O
                             filter.  Linker must be able to find szlib
                             if HDF5 was build with szip. [default=no]])],,
            withval=no)

case $withval in
  yes)
    HAVE_SZLIB="yes"
    AC_CHECK_HEADERS([szlib.h], )
    AC_CHECK_LIB([sz], [SZ_BufftoBuffCompress],, [unset HAVE_SZLIB])

    if test -z "$HAVE_SZLIB"; then
      AC_MSG_ERROR([couldn't find szlib library])
    fi
    ;;
  no)
    HAVE_SZLIB="no"
    AC_MSG_CHECKING([for szlib])
    AC_MSG_RESULT([suppressed])
    ;;
  *)
    HAVE_SZLIB="yes"
    case "$withval" in
      *,*)
        szlib_inc="`echo $withval |cut -f1 -d,`"
        szlib_lib="`echo $withval |cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          szlib_inc="$withval/include"
          szlib_lib="$withval/lib"
        fi
        ;;
    esac

    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$szlib_inc" = "X/usr/include"; then
      szlib_inc=""
    fi
    if test "X$szlib_lib" = "X/usr/lib"; then
      szlib_lib=""
    fi

    saved_CPPFLAGS="$CPPFLAGS"
    saved_LDFLAGS="$LDFLAGS"

    if test -n "$szlib_inc"; then
      CPPFLAGS="$CPPFLAGS -I$szlib_inc"
    fi

    AC_CHECK_HEADERS([szlib.h],
                     [HAVE_SZLIB_H="yes"],
                     [CPPFLAGS="$saved_CPPFLAGS"])

    if test -n "$szlib_lib"; then
      LDFLAGS="$LDFLAGS -L$szlib_lib"
    fi

    AC_CHECK_LIB([sz], [SZ_BufftoBuffCompress],,
                 [LDFLAGS="$saved_LDFLAGS"; unset HAVE_SZLIB])

    if test -z "$HAVE_SZLIB" -a -n "$HDF5_CONFIG_ABORT"; then
      AC_MSG_ERROR([couldn't find szlib library])
    fi
    ;;
esac


# Check for the HDF5 library using the --with argument
# Of course HDF5 is required, but the user may be using h5cc, in which case
# they don't need to specify --with-hdf5.  They can also use
# --with-hdf5=/path/to/hdf5 to give a specific path.
HAVE_HDF5="yes"
AC_ARG_WITH([hdf5],
            [AC_HELP_STRING([--with-hdf5=DIR],
                            [Specify path to HDF5 library to use while building
                            [default=yes]])],,
            withval=yes)

case $withval in
  yes)
    ;;
  no)
    AC_MSG_ERROR([HDF5 disabled in confugre, but is required to build HDF-EOS5.])
    ;;
  *)
    case "$withval" in
      *,*)
        hdf5_inc="`echo $withval |cut -f1 -d,`"
        hdf5_lib="`echo $withval |cut -f2 -d, -s`"
        ;;
      *)
        if test -n "$withval"; then
          hdf5_inc="$withval/include"
          hdf5_lib="$withval/lib"
        fi
        ;;
    esac
    
    dnl Trying to include -I/usr/include and -L/usr/lib is redundant and
    dnl can mess some compilers up.
    if test "X$hdf5_inc" = "X/usr/include"; then
      hdf5_inc=""
    fi
    if test "X$hdf5_lib" = "X/usr/lib"; then
      hdf5_lib=""
    fi

    if test -n "$hdf5_inc"; then
      CPPFLAGS="$CPPFLAGS -I$hdf5_inc"
    fi

    if test -n "$hdf5_lib"; then
      LDFLAGS="$LDFLAGS -L$hdf5_lib"
    fi
    ;;
esac

dnl See if we have a valid linking path to the HDF5 library, either because
dnl it's in the user's path or because they provided the correct path using
dnl --with-hdf5
AC_CHECK_HEADERS([hdf5.h], , [HAVE_HDF5="no"])
AC_CHECK_LIB(hdf5, H5Fcreate,, [HAVE_HDF5="no"])

if test "X$HAVE_HDF5" = "Xno"; then
  AC_MSG_ERROR([can't link against HDF5 library])
fi


# Check whether HDF5 threadsafety is enabled
AC_MSG_CHECKING([if HDF5 threadsafe mode is enabled])
AC_TRY_RUN([
#include "hdf5.h"

int main(void) {
#ifdef H5_HAVE_THREADSAFE
  return 0;
#else
  return 1;
#endif
} ], 
  [ AC_MSG_RESULT([yes])
    THREADSAFE="yes"],
  [AC_MSG_RESULT([no])
    THREADSAFE="no"])

# Record threadsafe status in config.h and for Makefiles
if test "X$THREADSAFE" = "Xyes"; then
  AC_DEFINE([_HDFEOS5_THREADSAFE], [1],
            [Define if threadsafe HDF-EOS is enabled])
fi

AM_CONDITIONAL([THREADSAFE_CONDITIONAL], [test "X$THREADSAFE" = "Xyes"])

# Set CFLAGS to prevent AC_PROG_CC from setting it
CFLAGS=$CFLAGS

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([malloc.h stdlib.h string.h limits.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_ERROR_AT_LINE
AC_CHECK_FUNCS([memchr memmove memset strchr strstr pow sqrt])

# Check whether testdrivers directory is present.  The user might have
# downloaded HDF-EOS5 but not the tests.
AC_MSG_CHECKING([for testdrivers directory])
if test -d "${srcdir}/testdrivers"; then
  TESTDRIVERS_DIR="yes"
  AC_MSG_RESULT([present])

  # If the testdrivers directory is present, see if it contains a file named
  # 'makefile'.  If so, rename the file so it doesn't cause confusion later
  # on.
  if test -e "testdrivers/makefile"; then
    mv testdrivers/makefile testdrivers/makefile_orig
  fi

else
  TESTDRIVERS_DIR="no"
  AC_MSG_RESULT([not found])
  AC_MSG_WARN([testdrivers directory is not present. Tests will not be run])
fi

AC_ARG_ENABLE([install-include],
              [AC_HELP_STRING([--enable-install-include],
                              [HDF-EOS5 normally only installs libraries,
                               assuming that users will use the headers in
                               the include directories.
                               Users who want the standard automake
                               behavior of installing both libraries and
                               header files should enable this option.])],
              [INSTALL_INCLUDE=$enableval])

AM_CONDITIONAL([TESTDRIVERS_CONDITIONAL], [test "X$TESTDRIVERS_DIR" = "Xyes"])
AM_CONDITIONAL([INSTALL_INCLUDE_CONDITIONAL], [test "X$INSTALL_INCLUDE" = "Xyes"])

AC_CONFIG_FILES([Makefile
                 include/Makefile
                 src/Makefile
                 gctp/Makefile
                 gctp/include/Makefile
                 gctp/src/Makefile
                 samples/Makefile])

if test "X$TESTDRIVERS_DIR" = "Xyes"; then
  AC_CONFIG_FILES([ testdrivers/Makefile
                 testdrivers/grid/Makefile
                 testdrivers/point/Makefile
                 testdrivers/swath/Makefile
                 testdrivers/threads/Makefile
                 testdrivers/za/Makefile])
fi

AC_OUTPUT
