/* -*-Mode: C;-*-
 * Module:      CDMS utility functions
 *
 * Copyright:	1996, Regents of the University of California
 *		This software may not be distributed to others without
 *		permission of the author.
 *
 * Author:      Bob Drach, Lawrence Livermore National Laboratory
 *              drach@llnl.gov
 *
 * Version:     $Id$
 *
 * Revision History:
 *
 * $Log$
 * Revision 1.1.1.1  2010/02/17 20:21:20  dnadeau
 * dnadeau import c-more release CMOR-2.0rc6 from 20100209
 *
 * Revision 1.1.1.1  1997/12/09 18:57:40  drach
 * Copied from cirrus
 *
 * Revision 1.1  1997/10/24  18:28:26  drach
 * - Initial repository version
 *
 *
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "cdmsint.h"

int cuErrorOccurred = 0;
void cdError(char *fmt, ...){
	va_list args;
	
	cuErrorOccurred = 1;
	if(cuErrOpts & CU_VERBOSE){
		va_start(args,fmt);
		fprintf(stderr, "CDMS error: ");
		vfprintf(stderr, fmt, args);
		fprintf(stderr, "\n");
		va_end(args);
	}
	if(cuErrOpts & CU_FATAL)
		exit(1);
	return;
}

