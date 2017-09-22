 /*********************************************************************
  *   Copyright 1993, UCAR/Unidata
  *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
  *   $Header$
  *********************************************************************/
#ifndef CONSTRAINTS4_H
#define CONSTRAINTS4_H 1

extern NCerror choosetactic4(NCDAP4* drno, Getvara* getvar,
		             DCEprojection* varaprojection);
extern NCerror buildvaraprojection4(Getvara* getvar,
		     const size_t* startp, const size_t* countp, const ptrdiff_t* stridep,
		     DCEprojection** projectionp);

#endif /*CONSTRAINTS4_H*/


