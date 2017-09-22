/*********************************************************************
 *   Copyright 2009, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *********************************************************************/
/* $Id$ */
/* $Header$ */

#ifndef GENERR_H
#define GENERR_H

extern int error_count;

#ifndef NO_STDARG
#define vastart(argv,fmt) va_start(argv,fmt)
#else
#define vastart(argv,fmt) va_start(argv)
#endif

#ifndef NO_STDARG
#include <stdarg.h>
extern void vderror(const char *fmt, va_list argv);
extern void vdwarn(const char *fmt, va_list argv);
extern void derror(const char *fmt, ...);
extern int panic(const char* fmt, ...);
extern void nprintf(char* buffer, size_t size, const char *fmt, ...);
extern  void semerror(const int, const char *fmt, ...);
extern  void semwarn(const int, const char *fmt, ...);
#else
#include <varargs.h>
/* Technically illegal; va_alist should be only arg */
extern void vderror(fmt,va_alist) const char* fmt; va_dcl;
extern void vdwarn(fmt,va_alist) const char* fmt; va_dcl;
extern void derror(fmt,va_alist) const char* fmt; va_dcl;
extern void panic(fmt,va_alist) const char* fmt; va_dcl;
extern void nprintf(buffer,size,fmt)
	char* buffer; size_t size; const char* fmt; va_dcl;
extern  void semerror(lno,fmt,va_alist) const int lno; const char* fmt; va_dcl;
extern  void semwarnlno,fmt,va_alist) const int lno; const char* fmt; va_dcl;
#endif

#endif /*GENERR_H*/
