/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/
#ifndef _CDL_H_
#define _CDL_H_

/* Names of special performance-related virtual attributes for
 * netCDF-4, displayed with "ncdump -s".  ncdump and ncgen need to
 * know these, as they are used in CDL. */
#define NC_ATT_FORMAT      "_Format"
#define NC_ATT_CHECKSUM    "_Fletcher32"
#define NC_ATT_CHUNKING    "_ChunkSizes"
#define NC_ATT_ENDIANNESS  "_Endianness"
#define NC_ATT_DEFLATE     "_DeflateLevel"
#define NC_ATT_SHUFFLE     "_Shuffle"
#define NC_ATT_STORAGE     "_Storage"
#define NC_ATT_NOFILL      "_NoFill"
#define NC_ATT_NETCDF4     "_NetCDF4"

#endif	/*_CDL_H_ */
