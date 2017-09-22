/*
  This is part of ncgen, a utility which is part of netCDF. Copyright
  2010, UCAR/Unidata. See COPYRIGHT file for copying and
  redistribution conditions.
 */

/* $Id$ */
/* $Header$ */

#ifndef NCGEN_INCLUDES_H
#define NCGEN_INCLUDES_H

#undef USE_NOFILL

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>	/* for isprint() */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __SunOS
#include <strings.h>
#endif

#ifdef __hpux
#include <locale.h>
#endif

#include "list.h"
#include "bytebuffer.h"

/* Local Configuration flags*/
#define ENABLE_BINARY
#define ENABLE_C
#define ENABLE_F77
#define ENABLE_JAVA

#include "netcdf.h"
#include "data.h"
#include "ncgen.h"
#include "genlib.h"
#include "util.h"
#include "debug.h"
#include "nc.h"

#ifdef USE_NETCDF4
#include "nc4internal.h"
#endif

extern int specialconstants;

#undef ITERBUG
#undef CHARBUG

#endif /* NCGEN_INCLUDES_H */
