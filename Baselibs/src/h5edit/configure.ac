#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of the HDF h5edit tool. The full h5edit tool
# copyright notice, including terms governing use, modification,
# and redistribution, is contained in the file COPYING, which can
# be found at the root of the source code distribution tree. If
# you do not have access to this file, you may request a copy
# from help@hdfgroup.org.

AC_PREREQ([2.67])
AC_INIT([h5edit], [1.3.1], [help@hdfgroup.org], [HDF H5Edit])
AC_CONFIG_SRCDIR([src/h5edit.h])
AM_CONFIG_HEADER([src/h5edit_config.h])

AC_CONFIG_AUX_DIR([bin])

dnl AM_INIT_AUTOMAKE takes a list of options that should be applied to
dnl every Makefile.am when automake is run.
AM_INIT_AUTOMAKE([foreign])

dnl AM_MAINTAINER_MODE turns off "rebuild rules" that contain dependencies
dnl for Makefiles, configure, src/h5edit_config.h, etc.  If AM_MAINTAINER_MODE
dnl is *not* included here, these files will be rebuilt if out of date.
dnl This is a problem because if users try to build on a machine with
dnl the wrong versions of autoconf and automake, these files will be
dnl rebuilt with the wrong versions and bad things can happen.
dnl Also, CVS doesn't preserve dependencies between timestamps, so
dnl Makefiles will often think rebuilding needs to occur when it doesn't.
dnl Developers should './configure --enable-maintainer-mode' to turn on
dnl rebuild rules.
AM_MAINTAINER_MODE

dnl Set prefix default (install directory) to a directory in the build area.
dnl This allows multiple src-dir builds within one host.
AC_PREFIX_DEFAULT([`pwd`/h5edit])

# Checks for programs.
dnl Make sure they are set to something.
CC="${CC:-h5cc}"

AC_PROG_CC

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

dnl We don't need to say when we're entering directories if we're using
dnl GNU make because make does it for us.
if test "X$GMAKE" = "Xyes"; then
  AC_SUBST([SETX]) SETX=":"
else
  AC_SUBST([SETX]) SETX="set -x"
fi


AC_CONFIG_FILES([Makefile
                 src/Makefile
                 test/Makefile])
AC_OUTPUT
