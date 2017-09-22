#ifndef NCGEN_DUMP_H
#define NCGEN_DUMP_H

/*#define F*/

/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/

extern void dumptransform(Datalist*);
extern void dumpdatalist(Datalist*,char*);
extern void dumpconstant(NCConstant*,char*);
extern void bufdump(Datalist*,Bytebuffer*);
extern void dumpgroup(Symbol* g);
extern void dumpsrc(Datasrc*,char*);

#ifdef F
#define DUMPSRC(src,tag) dumpsrc(src,tag)
#else
#define DUMPSRC(src,tag)
#endif

#endif
