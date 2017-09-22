#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#ifdef HAVE_LIBNETCDF

//#define TEST_GROUPS 1

#include <limits.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#ifdef HAVE_MMAP
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif
#ifdef HAVE_LIBPTHREAD
#include <pthread.h>
#endif

#include <netcdf.h>

#include "dmemory.h"
#include "cdi.h"
#include "basetime.h"
#include "gaussgrid.h"
#include "cdi_int.h"
#include "cdi_uuid.h"
#include "stream_cdf.h"
#include "cdf.h"
#include "cdf_int.h"
#include "varscan.h"
#include "vlist.h"
#include "zaxis.h"

//#define PROJECTION_TEST

#undef  UNDEFID
#define UNDEFID  CDI_UNDEFID

static const char bndsName[] = "bnds";

#define  X_AXIS  1
#define  Y_AXIS  2
#define  Z_AXIS  3
#define  T_AXIS  4

#define  POSITIVE_UP    1
#define  POSITIVE_DOWN  2

typedef struct {
  int     ncvarid;
  int     dimtype;
  size_t  len;
  char    name[CDI_MAX_NAME];
}
ncdim_t;
#define  MAX_COORDVARS  4
#define  MAX_AUXVARS    4

typedef struct {
  int      ncid;
  int      ignore;
  short    isvar;
  short    islon;
  int      islat;
  int      islev;
  int      istime;
  int      warn;
  int      tsteptype;
  int      param;
  int      code;
  int      tabnum;
  int      climatology;
  int      bounds;
  int      lformula;
  int      lformulaterms;
  int      gridID;
  int      zaxisID;
  int      gridtype;
  int      zaxistype;
  int      xdim;
  int      ydim;
  int      zdim;
  int      xvarid;
  int      yvarid;
  int      zvarid;
  int      tvarid;
  int      psvarid;
  int      p0varid;
  int      ncoordvars;
  int      coordvarids[MAX_COORDVARS];
  int      nauxvars;
  int      auxvarids[MAX_AUXVARS];
  int      cellarea;
  int      calendar;
  int      tableID;
  int      truncation;
  int      position;
  int      defmissval;
  int      deffillval;
  int      xtype;
  int      ndims;
  int      gmapid;
  int      positive;
  int      dimids[8];
  int      dimtype[8];
  int      chunks[8];
  int      chunked;
  int      chunktype;
  int      natts;
  int      deflate;
  int      lunsigned;
  int      lvalidrange;
  int     *atts;
  size_t   vctsize;
  double  *vct;
  double   missval;
  double   fillval;
  double   addoffset;
  double   scalefactor;
  double   validrange[2];
  char     name[CDI_MAX_NAME];
  char     longname[CDI_MAX_NAME];
  char     stdname[CDI_MAX_NAME];
  char     units[CDI_MAX_NAME];
  char     extra[CDI_MAX_NAME];
  ensinfo_t   *ensdata;    /* Ensemble information */
}
ncvar_t;

static
void strtolower(char *str)
{
  if ( str )
    for (size_t i = 0; str[i]; ++i)
      str[i] = (char)tolower((int)str[i]);
}

static
int get_timeunit(size_t len, const char *ptu)
{
  int timeunit = -1;

  if ( len > 2 )
    {
      if      ( memcmp(ptu, "sec",    3) == 0 )          timeunit = TUNIT_SECOND;
      else if ( memcmp(ptu, "minute", 6) == 0 )          timeunit = TUNIT_MINUTE;
      else if ( memcmp(ptu, "hour",   4) == 0 )          timeunit = TUNIT_HOUR;
      else if ( memcmp(ptu, "day",    3) == 0 )          timeunit = TUNIT_DAY;
      else if ( memcmp(ptu, "month",  5) == 0 )          timeunit = TUNIT_MONTH;
      else if ( memcmp(ptu, "calendar_month", 14) == 0 ) timeunit = TUNIT_MONTH;
      else if ( memcmp(ptu, "year",   4) == 0 )          timeunit = TUNIT_YEAR;
    }
  else if ( len == 1 )
    {
      if ( ptu[0] == 's' ) timeunit = TUNIT_SECOND;
    }

  return timeunit;
}

static
bool isTimeUnits(const char *timeunits)
{
  bool status = strncmp(timeunits, "sec",    3) == 0
    || strncmp(timeunits, "minute", 6) == 0
    || strncmp(timeunits, "hour",   4) == 0
    || strncmp(timeunits, "day",    3) == 0
    || strncmp(timeunits, "month",  5) == 0;
  return status;
}

static
bool isTimeAxisUnits(const char *timeunits)
{
  bool status = false;

  size_t len = strlen(timeunits);
  char *tu = (char *) Malloc((len+1)*sizeof(char));
  memcpy(tu, timeunits, (len+1) * sizeof(char));
  char *ptu = tu;

  for (size_t i = 0; i < len; i++ ) ptu[i] = (char)tolower((int)ptu[i]);

  int timeunit = get_timeunit(len, ptu);
  if ( timeunit != -1 )
    {

      while ( ! isspace(*ptu) && *ptu != 0 ) ptu++;
      if ( *ptu )
        {
          while ( isspace(*ptu) ) ptu++;

          int timetype = memcmp(ptu, "as", 2) == 0 ? TAXIS_ABSOLUTE :
            memcmp(ptu, "since", 5) == 0 ? TAXIS_RELATIVE : -1;

          status = timetype != -1;
        }
    }

  Free(tu);

  return status;
}

static
void scanTimeString(const char *ptu, int *rdate, int *rtime)
{
  int year = 1, month = 1, day = 1;
  int hour = 0, minute = 0, second = 0;
  int v1 = 1, v2 = 1, v3 = 1;

  *rdate = 0;
  *rtime = 0;

  if ( *ptu )
    {
      v1 = atoi(ptu);
      if ( v1 < 0 ) ptu++;
      while ( isdigit((int) *ptu) ) ptu++;
      if ( *ptu )
        {
          v2 = atoi(++ptu);
          while ( isdigit((int) *ptu) ) ptu++;
          if ( *ptu )
            {
              v3 = atoi(++ptu);
              while ( isdigit((int) *ptu) ) ptu++;
            }
        }
    }

  if ( v3 > 999 && v1 < 32 )
    { year = v3; month = v2; day = v1; }
  else
    { year = v1; month = v2; day = v3; }

  while ( isspace((int) *ptu) ) ptu++;

  if ( *ptu )
    {
      while ( ! isdigit((int) *ptu) ) ptu++;

      hour = atoi(ptu);
      while ( isdigit((int) *ptu) ) ptu++;
      if ( *ptu == ':' )
        {
          ptu++;
          minute = atoi(ptu);
          while ( isdigit((int) *ptu) ) ptu++;
          if ( *ptu == ':' )
            {
              ptu++;
              second = atoi(ptu);
            }
        }
    }

  *rdate = cdiEncodeDate(year, month, day);
  *rtime = cdiEncodeTime(hour, minute, second);
}

static
int scanTimeUnit(const char *unitstr)
{
  size_t len = strlen(unitstr);
  int timeunit = get_timeunit(len, unitstr);
  if ( timeunit == -1 )
    Message("Unsupported TIMEUNIT: %s!", unitstr);

  return timeunit;
}

static
void setForecastTime(const char *timestr, taxis_t *taxis)
{
  (*taxis).fdate = 0;
  (*taxis).ftime = 0;

  int len = (int) strlen(timestr);
  if ( len == 0 ) return;

  int fdate = 0, ftime = 0;
  scanTimeString(timestr, &fdate, &ftime);

  (*taxis).fdate = fdate;
  (*taxis).ftime = ftime;
}

static
int setBaseTime(const char *timeunits, taxis_t *taxis)
{
  int timetype = TAXIS_ABSOLUTE;
  int rdate = -1, rtime = -1;

  size_t len = strlen(timeunits);
  char *tu = (char *) Malloc((len+1) * sizeof (char));
  memcpy(tu, timeunits, (len+1) * sizeof (char));
  char *ptu = tu;

  for ( size_t i = 0; i < len; i++ ) ptu[i] = (char)tolower((int) ptu[i]);

  int timeunit = get_timeunit(len, ptu);
  if ( timeunit == -1 )
    {
      Message("Unsupported TIMEUNIT: %s!", timeunits);
      return (1);
    }

  while ( ! isspace(*ptu) && *ptu != 0 ) ptu++;
  if ( *ptu )
    {
      while ( isspace(*ptu) ) ptu++;

      if ( memcmp(ptu, "as", 2) == 0 )
        timetype = TAXIS_ABSOLUTE;
      else if ( memcmp(ptu, "since", 5) == 0 )
        timetype = TAXIS_RELATIVE;

      while ( ! isspace(*ptu) && *ptu != 0 ) ptu++;
      if ( *ptu )
        {
          while ( isspace(*ptu) ) ptu++;

          if ( timetype == TAXIS_ABSOLUTE )
            {
              if ( memcmp(ptu, "%y%m%d.%f", 9) != 0 && timeunit == TUNIT_DAY )
                {
                  Message("Unsupported format %s for TIMEUNIT day!", ptu);
                  timeunit = -1;
                }
              else if ( memcmp(ptu, "%y%m.%f", 7) != 0 && timeunit == TUNIT_MONTH )
                {
                  Message("Unsupported format %s for TIMEUNIT month!", ptu);
                  timeunit = -1;
                }
            }
          else if ( timetype == TAXIS_RELATIVE )
            {
              scanTimeString(ptu, &rdate, &rtime);

              (*taxis).rdate = rdate;
              (*taxis).rtime = rtime;

              if ( CDI_Debug )
                Message("rdate = %d  rtime = %d", rdate, rtime);
            }
        }
    }

  (*taxis).type = timetype;
  (*taxis).unit = timeunit;

  Free(tu);

  if ( CDI_Debug )
    Message("timetype = %d  unit = %d", timetype, timeunit);

  return 0;
}

static
void cdfGetAttInt(int fileID, int ncvarid, const char *attname, int attlen, int *attint)
{
  nc_type atttype;
  size_t nc_attlen;

  *attint = 0;

  cdf_inq_atttype(fileID, ncvarid, attname, &atttype);
  cdf_inq_attlen(fileID, ncvarid, attname, &nc_attlen);

  if ( atttype != NC_CHAR )
    {
      int *pintatt = (int)nc_attlen > attlen
        ? (int *)(Malloc(nc_attlen * sizeof (int))) : attint;

      cdf_get_att_int(fileID, ncvarid, attname, pintatt);

      if ( (int)nc_attlen > attlen )
        {
          memcpy(attint, pintatt, (size_t)attlen * sizeof (int));
          Free(pintatt);
        }
    }
}

static
void cdfGetAttDouble(int fileID, int ncvarid, char *attname, int attlen, double *attdouble)
{
  nc_type atttype;
  size_t nc_attlen;

  *attdouble = 0;

  cdf_inq_atttype(fileID, ncvarid, attname, &atttype);
  cdf_inq_attlen(fileID, ncvarid, attname, &nc_attlen);

  if ( atttype != NC_CHAR )
    {
      double *pdoubleatt = NULL;

      if ( (int)nc_attlen > attlen )
        pdoubleatt = (double *) Malloc(nc_attlen * sizeof (double));
      else
        pdoubleatt = attdouble;

      cdf_get_att_double(fileID, ncvarid, attname, pdoubleatt);

      if ( (int)nc_attlen > attlen )
        {
          memcpy(attdouble, pdoubleatt, (size_t)attlen * sizeof (double));
          Free(pdoubleatt);
        }
    }
}

static
void cdfGetAttText(int fileID, int ncvarid,const char *attname, int attlen, char *atttext)
{
  nc_type atttype;
  size_t nc_attlen;

  cdf_inq_atttype(fileID, ncvarid, attname, &atttype);
  cdf_inq_attlen(fileID, ncvarid, attname, &nc_attlen);

  if ( atttype == NC_CHAR )
    {
      char attbuf[65636];
      if ( nc_attlen < sizeof(attbuf) )
        {
          cdf_get_att_text(fileID, ncvarid, attname, attbuf);

          if ( (int) nc_attlen > (attlen-1) ) nc_attlen = (size_t)(attlen-1);

          attbuf[nc_attlen++] = 0;
          memcpy(atttext, attbuf, nc_attlen);
        }
      else
        {
          atttext[0] = 0;
        }
    }
#if  defined  (HAVE_NETCDF4)
  else if ( atttype == NC_STRING )
    {
      if ( nc_attlen == 1 )
        {
          char *attbuf = NULL;
          cdf_get_att_string(fileID, ncvarid, attname, &attbuf);

          size_t ssize = strlen(attbuf) + 1;

          if ( ssize > (size_t)attlen ) ssize = (size_t)attlen;
          memcpy(atttext, attbuf, ssize);
          atttext[ssize - 1] = 0;
          Free(attbuf);
        }
      else
        {
          atttext[0] = 0;
        }
    }
#endif
}

static
int xtypeIsText(int xtype)
{
  int isText = FALSE;

  if ( xtype == NC_CHAR )
    isText = TRUE;
#if  defined  (HAVE_NETCDF4)
  else if ( xtype == NC_STRING )
    isText = TRUE;
#endif

  return isText;
}

static
int xtypeIsFloat(int xtype)
{
  int isFloat = xtype == NC_FLOAT || xtype == NC_DOUBLE;
  return isFloat;
}

static
int cdfInqDatatype(int xtype, int lunsigned)
{
  int datatype = -1;

#if  defined  (HAVE_NETCDF4)
  if ( xtype == NC_BYTE && lunsigned ) xtype = NC_UBYTE;
#endif

  if      ( xtype == NC_BYTE   )  datatype = DATATYPE_INT8;
  /* else if ( xtype == NC_CHAR   )  datatype = DATATYPE_UINT8; */
  else if ( xtype == NC_SHORT  )  datatype = DATATYPE_INT16;
  else if ( xtype == NC_INT    )  datatype = DATATYPE_INT32;
  else if ( xtype == NC_FLOAT  )  datatype = DATATYPE_FLT32;
  else if ( xtype == NC_DOUBLE )  datatype = DATATYPE_FLT64;
#if  defined  (HAVE_NETCDF4)
  else if ( xtype == NC_UBYTE  )  datatype = DATATYPE_UINT8;
  else if ( xtype == NC_LONG   )  datatype = DATATYPE_INT32;
  else if ( xtype == NC_USHORT )  datatype = DATATYPE_UINT16;
  else if ( xtype == NC_UINT   )  datatype = DATATYPE_UINT32;
  else if ( xtype == NC_INT64  )  datatype = DATATYPE_FLT64;
  else if ( xtype == NC_UINT64 )  datatype = DATATYPE_FLT64;
#endif

  return datatype;
}


void cdfCopyRecord(stream_t *streamptr2, stream_t *streamptr1)
{
  int vlistID1 = streamptr1->vlistID;
  int tsID     = streamptr1->curTsID;
  int vrecID   = streamptr1->tsteps[tsID].curRecID;
  int recID    = streamptr1->tsteps[tsID].recIDs[vrecID];
  int ivarID   = streamptr1->tsteps[tsID].records[recID].varID;
  int gridID   = vlistInqVarGrid(vlistID1, ivarID);
  int datasize = gridInqSize(gridID);
  int datatype = vlistInqVarDatatype(vlistID1, ivarID);
  int memtype  = datatype != DATATYPE_FLT32 ? MEMTYPE_DOUBLE : MEMTYPE_FLOAT;

  void *data
    = Malloc((size_t)datasize
             * (memtype == MEMTYPE_DOUBLE ? sizeof(double) : sizeof(float)));

  int nmiss;
  cdf_read_record(streamptr1, memtype, data, &nmiss);
  cdf_write_record(streamptr2, memtype, data, nmiss);

  Free(data);
}

/* not used
int cdfInqRecord(stream_t *streamptr, int *varID, int *levelID)
{
  int tsID, recID;

  recID = streamptr->tsteps[0].curRecID++;
  printf("cdfInqRecord recID %d %d\n", recID, streamptr->tsteps[0].curRecID);
  printf("cdfInqRecord tsID %d\n", streamptr->curTsID);

  if ( streamptr->tsteps[0].curRecID >= streamptr->tsteps[0].nrecs )
    {
      streamptr->tsteps[0].curRecID = 0;
    }

  *varID   = streamptr->tsteps[0].records[recID].varID;
  *levelID = streamptr->tsteps[0].records[recID].levelID;

  streamptr->record->varID   = *varID;
  streamptr->record->levelID = *levelID;

  if ( CDI_Debug )
    Message("recID = %d  varID = %d  levelID = %d", recID, *varID, *levelID);

  return (recID+1);
}
*/


void cdfDefRecord(stream_t *streamptr)
{
  (void)streamptr;
}

#if defined(NC_SZIP_NN_OPTION_MASK)
static
void cdfDefVarSzip(int ncid, int ncvarid)
{
  int retval;
  /* Set options_mask and bits_per_pixel. */
  int options_mask = NC_SZIP_NN_OPTION_MASK;
  int bits_per_pixel = 16;

  if ((retval = nc_def_var_szip(ncid, ncvarid, options_mask, bits_per_pixel)))
    {
      if ( retval == NC_EINVAL )
        {
          static int lwarn = TRUE;

          if ( lwarn )
            {
              lwarn = FALSE;
              Warning("NetCDF4/Szip compression not compiled in!");
            }
        }
      else
        Error("nc_def_var_szip failed, status = %d", retval);
    }
}
#endif

static
void cdfDefTimeValue(stream_t *streamptr, int tsID)
{
  int fileID = streamptr->fileID;

  if ( CDI_Debug )
    Message("streamID = %d, fileID = %d", streamptr->self, fileID);

  taxis_t *taxis = &streamptr->tsteps[tsID].taxis;

  if ( streamptr->ncmode == 1 )
    {
      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  size_t index = (size_t)tsID;

  double timevalue = cdiEncodeTimeval(taxis->vdate, taxis->vtime, &streamptr->tsteps[0].taxis);
  if ( CDI_Debug ) Message("tsID = %d  timevalue = %f", tsID, timevalue);

  int ncvarid = streamptr->basetime.ncvarid;
  cdf_put_var1_double(fileID, ncvarid, &index, &timevalue);

  if ( taxis->has_bounds )
    {
      size_t start[2], count[2];

      ncvarid = streamptr->basetime.ncvarboundsid;

      timevalue = cdiEncodeTimeval(taxis->vdate_lb, taxis->vtime_lb, &streamptr->tsteps[0].taxis);
      start[0] = (size_t)tsID; count[0] = 1; start[1] = 0; count[1] = 1;
      cdf_put_vara_double(fileID, ncvarid, start, count, &timevalue);

      timevalue = cdiEncodeTimeval(taxis->vdate_ub, taxis->vtime_ub, &streamptr->tsteps[0].taxis);
      start[0] = (size_t)tsID; count[0] = 1; start[1] = 1; count[1] = 1;
      cdf_put_vara_double(fileID, ncvarid, start, count, &timevalue);
    }

  ncvarid = streamptr->basetime.leadtimeid;
  if ( taxis->type == TAXIS_FORECAST && ncvarid != UNDEFID )
    {
      timevalue = taxis->fc_period;
      cdf_put_var1_double(fileID, ncvarid, &index, &timevalue);
    }

  /*
printf("fileID = %d %d %d %f\n", fileID, time_varid, index, timevalue);
  */
}

static
int cdfDefTimeBounds(int fileID, int nctimevarid, int nctimedimid, const char *taxis_name, taxis_t* taxis)
{
  int time_bndsid = -1;
  int dims[2];

  dims[0] = nctimedimid;

  /* fprintf(stderr, "time has bounds\n"); */

  if ( nc_inq_dimid(fileID, bndsName, &dims[1]) != NC_NOERR )
    cdf_def_dim(fileID, bndsName, 2, &dims[1]);

  const char *bndsAttName, *bndsAttVal;
  size_t bndsAttValLen;
  char tmpstr[CDI_MAX_NAME];
  if ( taxis->climatology )
    {
      static const char climatology_bndsName[] = "climatology_bnds",
        climatology_bndsAttName[] = "climatology";
      bndsAttName = climatology_bndsAttName;
      bndsAttValLen = sizeof (climatology_bndsName) - 1;
      bndsAttVal = climatology_bndsName;
    }
  else
    {
      size_t taxisnameLen = strlen(taxis_name);
      memcpy(tmpstr, taxis_name, taxisnameLen);
      tmpstr[taxisnameLen] = '_';
      memcpy(tmpstr + taxisnameLen + 1, bndsName, sizeof (bndsName));
      size_t tmpstrLen = taxisnameLen + sizeof (bndsName);
      static const char generic_bndsAttName[] = "bounds";
      bndsAttName = generic_bndsAttName;
      bndsAttValLen = tmpstrLen;
      bndsAttVal = tmpstr;
    }
  cdf_def_var(fileID, bndsAttVal, NC_DOUBLE, 2, dims, &time_bndsid);
  cdf_put_att_text(fileID, nctimevarid, bndsAttName, bndsAttValLen, bndsAttVal);

  return (time_bndsid);
}

static
void cdfDefTimeUnits(char *unitstr, taxis_t* taxis0, taxis_t* taxis)
{
  unitstr[0] = 0;

  if ( taxis0->type == TAXIS_ABSOLUTE )
    {
      if ( taxis0->unit == TUNIT_YEAR )
        sprintf(unitstr, "year as %s", "%Y.%f");
      else if ( taxis0->unit == TUNIT_MONTH )
        sprintf(unitstr, "month as %s", "%Y%m.%f");
      else
        sprintf(unitstr, "day as %s", "%Y%m%d.%f");
    }
  else
    {
      int timeunit = taxis->unit != -1 ? taxis->unit : TUNIT_HOUR;
      int rdate    = taxis->rdate;
      int rtime    = taxis->rtime;
      if ( rdate == -1 )
        {
          rdate  = taxis->vdate;
          rtime  = taxis->vtime;
        }

      int year, month, day, hour, minute, second;
      cdiDecodeDate(rdate, &year, &month, &day);
      cdiDecodeTime(rtime, &hour, &minute, &second);

      if ( timeunit == TUNIT_QUARTER   ) timeunit = TUNIT_MINUTE;
      if ( timeunit == TUNIT_30MINUTES ) timeunit = TUNIT_MINUTE;
      if ( timeunit == TUNIT_3HOURS  ||
	   timeunit == TUNIT_6HOURS  ||
	   timeunit == TUNIT_12HOURS ) timeunit = TUNIT_HOUR;

      sprintf(unitstr, "%s since %d-%d-%d %02d:%02d:%02d",
              tunitNamePtr(timeunit), year, month, day, hour, minute, second);
    }
}

static
void cdfDefForecastTimeUnits(char *unitstr, int timeunit)
{
  unitstr[0] = 0;

  if ( timeunit == -1 ) timeunit = TUNIT_HOUR;

  if ( timeunit == TUNIT_QUARTER   ) timeunit = TUNIT_MINUTE;
  if ( timeunit == TUNIT_30MINUTES ) timeunit = TUNIT_MINUTE;
  if ( timeunit == TUNIT_3HOURS  ||
       timeunit == TUNIT_6HOURS  ||
       timeunit == TUNIT_12HOURS ) timeunit = TUNIT_HOUR;

  strcpy(unitstr, tunitNamePtr(timeunit));
}

static
void cdfDefCalendar(int fileID, int ncvarid, int calendar)
{
  static const struct { int calCode; const char *calStr; } calTab[] = {
    { CALENDAR_STANDARD, "standard" },
    { CALENDAR_PROLEPTIC, "proleptic_gregorian" },
    { CALENDAR_NONE, "none" },
    { CALENDAR_360DAYS, "360_day" },
    { CALENDAR_365DAYS, "365_day" },
    { CALENDAR_366DAYS, "366_day" },
  };
  enum { calTabSize = sizeof calTab / sizeof calTab[0] };

  for (size_t i = 0; i < calTabSize; ++i)
    if (calTab[i].calCode == calendar)
      {
        const char *calstr = calTab[i].calStr;
        size_t len = strlen(calstr);
        cdf_put_att_text(fileID, ncvarid, "calendar", len, calstr);
        break;
      }
}


void cdfDefTime(stream_t* streamptr)
{
  int time_varid;
  int time_dimid;
  int time_bndsid = -1;
  static const char default_name[] = "time";

  if ( streamptr->basetime.ncvarid != UNDEFID ) return;

  int fileID = streamptr->fileID;

  if ( streamptr->ncmode == 0 ) streamptr->ncmode = 1;
  if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

  taxis_t *taxis = &streamptr->tsteps[0].taxis;

  const char *taxis_name = (taxis->name && taxis->name[0]) ? taxis->name : default_name ;

  cdf_def_dim(fileID, taxis_name, NC_UNLIMITED, &time_dimid);
  streamptr->basetime.ncdimid = time_dimid;

  cdf_def_var(fileID, taxis_name, NC_DOUBLE, 1, &time_dimid, &time_varid);

  streamptr->basetime.ncvarid = time_varid;

  {
    static const char timeStr[] = "time";
    cdf_put_att_text(fileID, time_varid, "standard_name", sizeof(timeStr) - 1, timeStr);
  }

  if ( taxis->longname && taxis->longname[0] )
    cdf_put_att_text(fileID, time_varid, "long_name", strlen(taxis->longname), taxis->longname);

  if ( taxis->has_bounds )
    {
      time_bndsid = cdfDefTimeBounds(fileID, time_varid, time_dimid, taxis_name, taxis);
      streamptr->basetime.ncvarboundsid = time_bndsid;
    }

  {
    char unitstr[CDI_MAX_NAME];
    cdfDefTimeUnits(unitstr, &streamptr->tsteps[0].taxis, taxis);
    size_t len = strlen(unitstr);
    if ( len )
      {
        cdf_put_att_text(fileID, time_varid, "units", len, unitstr);
        /*
          if ( taxis->has_bounds )
          cdf_put_att_text(fileID, time_bndsid, "units", len, unitstr);
        */
      }
  }

  if ( taxis->calendar != -1 )
    {
      cdfDefCalendar(fileID, time_varid, taxis->calendar);
      /*
      if ( taxis->has_bounds )
        cdfDefCalendar(fileID, time_bndsid, taxis->calendar);
      */
    }

  if ( taxis->type == TAXIS_FORECAST )
    {
      int leadtimeid;

      cdf_def_var(fileID, "leadtime", NC_DOUBLE, 1, &time_dimid, &leadtimeid);

      streamptr->basetime.leadtimeid = leadtimeid;

      {
        static const char stdname[] = "forecast_period";
        cdf_put_att_text(fileID, leadtimeid, "standard_name", sizeof(stdname) - 1, stdname);
      }

      {
        static const char lname[] = "Time elapsed since the start of the forecast";
        cdf_put_att_text(fileID, leadtimeid, "long_name", sizeof(lname) - 1, lname);
      }

      {
          char unitstr[CDI_MAX_NAME];
          cdfDefForecastTimeUnits(unitstr, taxis->fc_unit);
          size_t len = strlen(unitstr);
          if ( len )
            cdf_put_att_text(fileID, leadtimeid, "units", len, unitstr);
      }
    }

  cdf_put_att_text(fileID, time_varid, "axis", 1, "T");

  if ( streamptr->ncmode == 2 ) cdf_enddef(fileID);
}


void cdfDefTimestep(stream_t *streamptr, int tsID)
{
  int vlistID = streamptr->vlistID;

  if ( vlistHasTime(vlistID) ) cdfDefTime(streamptr);

  cdfDefTimeValue(streamptr, tsID);
}

static
void cdfDefComplex(stream_t *streamptr, int gridID)
{
  static const char axisname[] = "nc2";
  int dimID = UNDEFID;
  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  int ngrids = vlistNgrids(vlistID);

  for ( int index = 0; index < ngrids; index++ )
    {
      if ( streamptr->xdimID[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == GRID_SPECTRAL || gridtype0 == GRID_FOURIER )
            {
              dimID = streamptr->xdimID[index];
              break;
            }
        }
    }

  if ( dimID == UNDEFID )
    {
      size_t dimlen = 2;

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      cdf_def_dim(fileID, axisname, dimlen, &dimID);

      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  int gridindex = vlistGridIndex(vlistID, gridID);
  streamptr->xdimID[gridindex] = dimID;
}

static void
cdfDefSPorFC(stream_t *streamptr, int gridID,
             char *restrict axisname, int gridRefType)
{
  int index, iz = 0;
  int dimID = UNDEFID;

  int vlistID = streamptr->vlistID;

  int ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridInqSize(gridID)/2;

  for ( index = 0; index < ngrids; index++ )
    {
      if ( streamptr->ydimID[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == gridRefType )
            {
              size_t dimlen0 = (size_t)gridInqSize(gridID0)/2;
              if ( dimlen == dimlen0 )
                {
                  dimID = streamptr->ydimID[index];
                  break;
                }
              else
                iz++;
            }
        }
    }

  if ( dimID == UNDEFID )
    {
      int fileID  = streamptr->fileID;
      if ( iz == 0 ) axisname[3] = '\0';
      else           sprintf(&axisname[3], "%1d", iz+1);

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      cdf_def_dim(fileID, axisname, dimlen, &dimID);

      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  int gridindex = vlistGridIndex(vlistID, gridID);
  streamptr->ydimID[gridindex] = dimID;
}

static
void cdfDefSP(stream_t *streamptr, int gridID)
{
  /*
  char longname[] = "Spherical harmonic coefficient";
  */
  char axisname[5] = "nspX";
  cdfDefSPorFC(streamptr, gridID, axisname, GRID_SPECTRAL);
}


static
void cdfDefFC(stream_t *streamptr, int gridID)
{
  char axisname[5] = "nfcX";
  cdfDefSPorFC(streamptr, gridID, axisname, GRID_FOURIER);
}

static const struct cdfDefGridAxisInqs {
  int (*axisSize)(int gridID);
  void (*axisName)(int gridID, char *dimname);
  const char *(*axisNamePtr)(int gridID);
  void (*axisStdname)(int gridID, char *dimstdname);
  void (*axisLongname)(int gridID, char *dimlongname);
  void (*axisUnits)(int gridID, char *dimunits);
  double (*axisVal)(int gridID, int index);
  const double *(*axisValsPtr)(int gridID);
  const double *(*axisBoundsPtr)(int gridID);
} gridInqsX = {
  .axisSize = gridInqXsize,
  .axisName = gridInqXname,
  .axisNamePtr = gridInqXnamePtr,
  .axisStdname = gridInqXstdname,
  .axisLongname = gridInqXlongname,
  .axisUnits = gridInqXunits,
  .axisVal = gridInqXval,
  .axisValsPtr = gridInqXvalsPtr,
  .axisBoundsPtr = gridInqXboundsPtr,
}, gridInqsY = {
  .axisSize = gridInqYsize,
  .axisName = gridInqYname,
  .axisNamePtr = gridInqYnamePtr,
  .axisStdname = gridInqYstdname,
  .axisLongname = gridInqYlongname,
  .axisUnits = gridInqYunits,
  .axisVal = gridInqYval,
  .axisValsPtr = gridInqYvalsPtr,
  .axisBoundsPtr = gridInqYboundsPtr,
}, gridInqsZ = {
  .axisStdname = zaxisInqStdname,
  .axisLongname = zaxisInqLongname,
  .axisUnits = zaxisInqUnits,
};

static void
cdfPutGridStdAtts(int fileID, int ncvarid,
                  int gridID, const struct cdfDefGridAxisInqs *inqs)
{
  size_t len;
  {
    char stdname[CDI_MAX_NAME];
    inqs->axisStdname(gridID, stdname);
    if ( (len = strlen(stdname)) )
      cdf_put_att_text(fileID, ncvarid, "standard_name", len, stdname);
  }
  {
    char longname[CDI_MAX_NAME];
    inqs->axisLongname(gridID, longname);
    if ( (len = strlen(longname)) )
      cdf_put_att_text(fileID, ncvarid, "long_name", len, longname);
  }
  {
    char units[CDI_MAX_NAME];
    inqs->axisUnits(gridID, units);
    if ( (len = strlen(units)) )
      cdf_put_att_text(fileID, ncvarid, "units", len, units);
  }
}

static void
cdfDefTrajLatLon(stream_t *streamptr, int gridID,
                 const struct cdfDefGridAxisInqs *inqs,
                 int *dimID, const char *sizeName)
{
  nc_type xtype = gridInqPrec(gridID) == DATATYPE_FLT32 ? NC_FLOAT : NC_DOUBLE;

  int vlistID = streamptr->vlistID;
  int dimlen = inqs->axisSize(gridID);
  if ( dimlen != 1 )
    Error("%s isn't 1 for %s grid!", sizeName, gridNamePtr(gridInqType(gridID)));

  int gridindex = vlistGridIndex(vlistID, gridID);
  int ncvarid = dimID[gridindex];

  if ( ncvarid == UNDEFID )
    {
      int dimNcID = streamptr->basetime.ncvarid;
      int fileID  = streamptr->fileID;
      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      const char *axisname = inqs->axisNamePtr(gridID);
      cdf_def_var(fileID, axisname, xtype, 1, &dimNcID, &ncvarid);
      cdfPutGridStdAtts(fileID, ncvarid, gridID, inqs);
      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  dimID[gridindex] = ncvarid; /* var ID for trajectory !!! */
}

static
void cdfDefTrajLon(stream_t *streamptr, int gridID)
{
  cdfDefTrajLatLon(streamptr, gridID, &gridInqsX, streamptr->xdimID, "Xsize");
}


static
void cdfDefTrajLat(stream_t *streamptr, int gridID)
{
  cdfDefTrajLatLon(streamptr, gridID, &gridInqsY, streamptr->ydimID, "Ysize");
}

static
int checkDimName(int fileID, size_t dimlen, char *dimname)
{
  /* check whether the dimenion name is already defined with the same length */
  unsigned iz = 0;
  int dimid = UNDEFID;
  char name[CDI_MAX_NAME];

  size_t len = strlen(dimname);
  memcpy(name, dimname, len + 1);

  do
    {
      if ( iz ) sprintf(name + len, "_%u", iz+1);

      int dimid0, status = nc_inq_dimid(fileID, name, &dimid0);
      if ( status != NC_NOERR )
        break;
      size_t dimlen0;
      cdf_inq_dimlen(fileID, dimid0, &dimlen0);
      if ( dimlen0 == dimlen )
        {
          dimid = dimid0;
          break;
        }
      iz++;
    }
  while ( iz <= 99 );


  if ( iz ) sprintf(dimname + len, "_%u", iz+1);

  return dimid;
}

static
void checkGridName(char *axisname, int fileID, int vlistID, int gridID, int ngrids, int mode)
{
  int ncdimid;
  char axisname2[CDI_MAX_NAME];

  /* check that the name is not already defined */
  unsigned iz = 0;

  size_t axisnameLen = strlen(axisname);
  memcpy(axisname2, axisname, axisnameLen + 1);
  do
    {
      if ( iz ) sprintf(axisname2 + axisnameLen, "_%u", iz+1);

      int status = nc_inq_varid(fileID, axisname2, &ncdimid);

      if ( status != NC_NOERR )
        {
          if ( iz )
            {
              /* check that the name does not exist for other grids */
              for ( int index = 0; index < ngrids; index++ )
                {
                  int gridID0 = vlistGrid(vlistID, index);
                  if ( gridID != gridID0 )
                    {
                       /* mode X or Y */
                      const char *(*query)(int)
                        = mode == 'X' ? gridInqXnamePtr : gridInqYnamePtr;
                      const char *axisname0 = query(gridID0);
                      if ( strcmp(axisname0, axisname2) == 0 ) goto nextSuffix;
                    }
                }
            }
          break;
        }
      nextSuffix:
      ++iz;
    }
  while ( iz <= 99 );


  if ( iz ) sprintf(axisname + axisnameLen, "_%u", iz+1);
}

static
int checkZaxisName(char *axisname, int fileID, int vlistID, int zaxisID, int nzaxis)
{
  char axisname2[CDI_MAX_NAME];

  /* check that the name is not already defined */
  unsigned iz = 0;

  size_t axisnameLen = strlen(axisname);
  memcpy(axisname2, axisname, axisnameLen + 1);
  do
    {
      if ( iz ) sprintf(axisname2 + axisnameLen, "_%u", iz+1);

      int ncdimid, status = nc_inq_varid(fileID, axisname2, &ncdimid);

      if ( status != NC_NOERR )
        {
          if ( iz )
            {
              /* check that the name does not exist for other zaxes */
              for ( int index = 0; index < nzaxis; index++ )
                {
                  int zaxisID0 = vlistZaxis(vlistID, index);
                  if ( zaxisID != zaxisID0 )
                    {
                      const char *axisname0 = zaxisInqNamePtr(zaxisID0);
                      if ( strcmp(axisname0, axisname2) == 0 ) goto nextSuffix;
                    }
                }
            }
          break;
        }
      nextSuffix:
      ++iz;
    }
  while (iz <= 99);


  if ( iz ) sprintf(axisname + axisnameLen, "_%u", iz+1);

  return (int)iz;
}

static void
cdfDefAxisCommon(stream_t *streamptr, int gridID, int ndims,
                 const struct cdfDefGridAxisInqs *gridAxisInq,
                 int *axisDimIDs, int dimKey, char axisLetter,
                 void (*finishCyclicBounds)(double *pbounds, size_t dimlen,
                                            const double *pvals),
                 int *ncAxisVarIDs)
{
  int dimID = UNDEFID;
  int ngrids = 0;
  int ncvarid = UNDEFID, ncbvarid = UNDEFID;
  int nvdimID = UNDEFID;
  nc_type xtype = gridInqPrec(gridID) == DATATYPE_FLT32 ? NC_FLOAT : NC_DOUBLE;

  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  if ( ndims ) ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridAxisInq->axisSize(gridID);
  int gridindex = vlistGridIndex(vlistID, gridID);

  const char *axisname = gridAxisInq->axisNamePtr(gridID);
  size_t axisnameLen = strlen(axisname);

  if ( axisname[0] == 0 ) Error("axis name undefined!");

  for ( int index = 0; index < ngrids; index++ )
    {
      if ( axisDimIDs[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == GRID_GAUSSIAN    ||
               gridtype0 == GRID_LONLAT      ||
               gridtype0 == GRID_CURVILINEAR ||
               gridtype0 == GRID_GENERIC )
            {
              size_t dimlen0 = (size_t)gridAxisInq->axisSize(gridID0);
              if ( dimlen == dimlen0 )
                {
                  double (*inqVal)(int gridID, int index)
                    = gridAxisInq->axisVal;
                  if ( IS_EQUAL(inqVal(gridID0, 0), inqVal(gridID, 0)) &&
                       IS_EQUAL(inqVal(gridID0, (int)dimlen-1), inqVal(gridID, (int)dimlen-1)) )
                  {
                    dimID = axisDimIDs[index];
                    break;
                  }
                }
            }
        }
    }

  if ( dimID == UNDEFID )
    {
      const double *pvals = gridAxisInq->axisValsPtr(gridID);

      /* enough to append _ plus up to 100 decimal and trailing \0 */
      char extendedAxisname[axisnameLen + 4 + 1];
      memcpy(extendedAxisname, axisname, axisnameLen + 1);
      checkGridName(extendedAxisname, fileID, vlistID, gridID, ngrids, axisLetter);
      size_t extendedAxisnameLen
        = axisnameLen + strlen(extendedAxisname + axisnameLen);

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      if ( ndims )
        {
          char dimname[CDI_MAX_NAME+3];
          dimname[0] = 0;

          if ( pvals == NULL )
            cdiGridInqString(gridID, dimKey, CDI_MAX_NAME, dimname);

          if ( dimname[0] == 0 ) strcpy(dimname, extendedAxisname);
          dimID = checkDimName(fileID, dimlen, dimname);

          if ( dimID == UNDEFID ) cdf_def_dim(fileID, dimname, dimlen, &dimID);
        }

      bool gen_bounds = false;
      int grid_is_cyclic = gridIsCircular(gridID);
      double *pbounds = NULL;
      if ( pvals )
        {
          cdf_def_var(fileID, extendedAxisname, xtype, ndims, &dimID, &ncvarid);

          cdfPutGridStdAtts(fileID, ncvarid, gridID, gridAxisInq);
          {
            char axisStr[2] = { axisLetter, '\0' };
            cdf_put_att_text(fileID, ncvarid, "axis", 1, axisStr);
          }

          pbounds = (double *)gridAxisInq->axisBoundsPtr(gridID);

          if ( CDI_cmor_mode && grid_is_cyclic && !pbounds )
            {
              gen_bounds = true;
              pbounds = (double*) Malloc(2*dimlen*sizeof(double));
              for ( size_t i = 0; i < dimlen-1; ++i )
                {
                  pbounds[i*2+1]   = (pvals[i] + pvals[i+1])/2;
                  pbounds[(i+1)*2] = (pvals[i] + pvals[i+1])/2;
                }
              finishCyclicBounds(pbounds, dimlen, pvals);
            }
          if ( pbounds )
            {
              size_t nvertex = 2;
              if ( nc_inq_dimid(fileID, bndsName, &nvdimID) != NC_NOERR )
                cdf_def_dim(fileID, bndsName, nvertex, &nvdimID);
            }
          if ( pbounds && nvdimID != UNDEFID )
            {
              char boundsname[extendedAxisnameLen + 1 + sizeof (bndsName)];
              memcpy(boundsname, axisname, extendedAxisnameLen);
              boundsname[extendedAxisnameLen] = '_';
              memcpy(boundsname + extendedAxisnameLen + 1, bndsName, sizeof bndsName);
              int dimIDs[2] = { dimID, nvdimID };
              cdf_def_var(fileID, boundsname, xtype, 2, dimIDs, &ncbvarid);
              cdf_put_att_text(fileID, ncvarid, "bounds", extendedAxisnameLen + sizeof (bndsName), boundsname);
            }
        }

      cdf_enddef(fileID);
      streamptr->ncmode = 2;

      if ( ncvarid  != UNDEFID ) cdf_put_var_double(fileID, ncvarid, pvals);
      if ( ncbvarid != UNDEFID ) cdf_put_var_double(fileID, ncbvarid, pbounds);
      if ( gen_bounds ) Free(pbounds);

      if ( ndims == 0 ) ncAxisVarIDs[gridindex] = ncvarid;
    }

  axisDimIDs[gridindex] = dimID;
}

static void
finishCyclicXBounds(double *pbounds, size_t dimlen, const double *pvals)
{
  pbounds[0] = (pvals[0] + pvals[dimlen-1]-360)*0.5;
  pbounds[2*dimlen-1] = (pvals[dimlen-1] + pvals[0]+360)*0.5;
}

static
void cdfDefXaxis(stream_t *streamptr, int gridID, int ndims)
{
  cdfDefAxisCommon(streamptr, gridID, ndims, &gridInqsX, streamptr->xdimID,
                   CDI_GRID_XDIMNAME, 'X', finishCyclicXBounds,
                   streamptr->ncxvarID);
}

static void
finishCyclicYBounds(double *pbounds, size_t dimlen, const double *pvals)
{
  pbounds[0] = copysign(90.0, pvals[0]);
  pbounds[2*dimlen-1] = copysign(90.0, pvals[dimlen-1]);
}

static
void cdfDefYaxis(stream_t *streamptr, int gridID, int ndims)
{
  cdfDefAxisCommon(streamptr, gridID, ndims, &gridInqsY, streamptr->ydimID,
                   CDI_GRID_YDIMNAME, 'Y', finishCyclicYBounds,
                   streamptr->ncyvarID);
}

static
void cdfGridCompress(int fileID, int ncvarid, int gridsize, int filetype, int comptype)
{
#if  defined  (HAVE_NETCDF4)
  if ( gridsize > 1 && comptype == COMPRESS_ZIP && (filetype == FILETYPE_NC4 || filetype == FILETYPE_NC4C) )
    {
      nc_def_var_chunking(fileID, ncvarid, NC_CHUNKED, NULL);
      cdfDefVarDeflate(fileID, ncvarid, 1);
    }
#endif
}

static
void cdfDefCurvilinear(stream_t *streamptr, int gridID)
{
  int xdimID = UNDEFID;
  int ydimID = UNDEFID;
  int ncxvarid = UNDEFID, ncyvarid = UNDEFID;
  int ncbxvarid = UNDEFID, ncbyvarid = UNDEFID, ncavarid = UNDEFID;
  nc_type xtype = gridInqPrec(gridID) == DATATYPE_FLT32 ? NC_FLOAT : NC_DOUBLE;

  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  int ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridInqSize(gridID);
  size_t xdimlen = (size_t)gridInqXsize(gridID);
  size_t ydimlen = (size_t)gridInqYsize(gridID);
  int gridindex = vlistGridIndex(vlistID, gridID);

  for ( int index = 0; index < ngrids; index++ )
    {
      if ( streamptr->xdimID[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == GRID_CURVILINEAR )
            {
              size_t dimlen0 = (size_t)gridInqSize(gridID0);
              if ( dimlen == dimlen0 )
                if ( IS_EQUAL(gridInqXval(gridID0, 0), gridInqXval(gridID, 0)) &&
                     IS_EQUAL(gridInqXval(gridID0, (int)dimlen-1), gridInqXval(gridID, (int)dimlen-1)) &&
                     IS_EQUAL(gridInqYval(gridID0, 0), gridInqYval(gridID, 0)) &&
                     IS_EQUAL(gridInqYval(gridID0, (int)dimlen-1), gridInqYval(gridID, (int)dimlen-1)) )
                  {
                    xdimID = streamptr->xdimID[index];
                    ydimID = streamptr->ydimID[index];
                    ncxvarid = streamptr->ncxvarID[index];
                    ncyvarid = streamptr->ncyvarID[index];
                    break;
                  }
            }
        }
    }

  if ( xdimID == UNDEFID || ydimID == UNDEFID )
    {
      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);
      {
        char xdimname[CDI_MAX_NAME+3];
        xdimname[0] = 0;
        cdiGridInqString(gridID, CDI_GRID_XDIMNAME, CDI_MAX_NAME, xdimname);
        if ( xdimname[0] == 0 ) { xdimname[0] = 'x'; xdimname[1] = 0; }
        xdimID = checkDimName(fileID, xdimlen, xdimname);
        if ( xdimID == UNDEFID ) cdf_def_dim(fileID, xdimname, xdimlen, &xdimID);
      }
      {
        char ydimname[CDI_MAX_NAME+3];
        ydimname[0] = 0;
        cdiGridInqString(gridID, CDI_GRID_YDIMNAME, CDI_MAX_NAME, ydimname);
        if ( ydimname[0] == 0 ) { ydimname[0] = 'y'; ydimname[1] = 0; }
        ydimID = checkDimName(fileID, ydimlen, ydimname);
        if ( ydimID == UNDEFID ) cdf_def_dim(fileID, ydimname, ydimlen, &ydimID);
      }

      int nvdimID = UNDEFID;
      int dimIDs[3];
      if ( gridInqXboundsPtr(gridID) || gridInqYboundsPtr(gridID) )
        {
          char vdimname[CDI_MAX_NAME+3];
          vdimname[0] = 0;
          cdiGridInqString(gridID, CDI_GRID_VDIMNAME, CDI_MAX_NAME, vdimname);
          if ( vdimname[0] == 0 ) strcpy(vdimname, "nv4");
          size_t nvertex = 4;
          nvdimID = checkDimName(fileID, nvertex, vdimname);
          if ( nvdimID == UNDEFID ) cdf_def_dim(fileID, vdimname, nvertex, &nvdimID);
        }

      dimIDs[0] = ydimID;
      dimIDs[1] = xdimID;
      dimIDs[2] = nvdimID;

      if ( gridInqXvalsPtr(gridID) )
        {
          char xaxisname[CDI_MAX_NAME];
          gridInqXname(gridID, xaxisname);
          checkGridName(xaxisname, fileID, vlistID, gridID, ngrids, 'X');

          cdf_def_var(fileID, xaxisname, xtype, 2, dimIDs, &ncxvarid);
          cdfGridCompress(fileID, ncxvarid, (int)(xdimlen*ydimlen), streamptr->filetype, streamptr->comptype);

          cdfPutGridStdAtts(fileID, ncxvarid, gridID, &gridInqsX);

          /* attribute for Panoply */
          cdf_put_att_text(fileID, ncxvarid, "_CoordinateAxisType", 3, "Lon");

          if ( gridInqXboundsPtr(gridID) && nvdimID != UNDEFID )
            {
              size_t xaxisnameLen = strlen(xaxisname);
              xaxisname[xaxisnameLen] = '_';
              memcpy(xaxisname + xaxisnameLen + 1, bndsName, sizeof (bndsName));
              cdf_def_var(fileID, xaxisname, xtype, 3, dimIDs, &ncbxvarid);
              cdfGridCompress(fileID, ncbxvarid, (int)(xdimlen*ydimlen), streamptr->filetype, streamptr->comptype);

              cdf_put_att_text(fileID, ncxvarid, "bounds", xaxisnameLen + sizeof (bndsName), xaxisname);
            }
        }

      if ( gridInqYvalsPtr(gridID) )
        {
          char yaxisname[CDI_MAX_NAME];
          gridInqYname(gridID, yaxisname);
          checkGridName(yaxisname, fileID, vlistID, gridID, ngrids, 'Y');

          cdf_def_var(fileID, yaxisname, xtype, 2, dimIDs, &ncyvarid);
          cdfGridCompress(fileID, ncyvarid, (int)(xdimlen*ydimlen), streamptr->filetype, streamptr->comptype);

          cdfPutGridStdAtts(fileID, ncyvarid, gridID, &gridInqsY);

          /* attribute for Panoply */
          cdf_put_att_text(fileID, ncyvarid, "_CoordinateAxisType", 3, "Lat");

          if ( gridInqYboundsPtr(gridID) && nvdimID != UNDEFID )
            {
              size_t yaxisnameLen = strlen(yaxisname);
              yaxisname[yaxisnameLen] = '_';
              memcpy(yaxisname + yaxisnameLen + 1, bndsName, sizeof (bndsName));
              cdf_def_var(fileID, yaxisname, xtype, 3, dimIDs, &ncbyvarid);
              cdfGridCompress(fileID, ncbyvarid, (int)(xdimlen*ydimlen), streamptr->filetype, streamptr->comptype);

              cdf_put_att_text(fileID, ncyvarid, "bounds", yaxisnameLen + sizeof (bndsName), yaxisname);
            }
        }

      if ( gridInqAreaPtr(gridID) )
        {
          static const char yaxisname_[] = "cell_area";
          static const char units[] = "m2";
          static const char longname[] = "area of grid cell";
          static const char stdname[] = "cell_area";

          cdf_def_var(fileID, yaxisname_, xtype, 2, dimIDs, &ncavarid);

          cdf_put_att_text(fileID, ncavarid, "standard_name", sizeof (stdname) - 1, stdname);
          cdf_put_att_text(fileID, ncavarid, "long_name", sizeof (longname) - 1, longname);
          cdf_put_att_text(fileID, ncavarid, "units", sizeof (units) - 1, units);
        }

      cdf_enddef(fileID);
      streamptr->ncmode = 2;

      if ( ncxvarid  != UNDEFID ) cdf_put_var_double(fileID, ncxvarid,  gridInqXvalsPtr(gridID));
      if ( ncbxvarid != UNDEFID ) cdf_put_var_double(fileID, ncbxvarid, gridInqXboundsPtr(gridID));
      if ( ncyvarid  != UNDEFID ) cdf_put_var_double(fileID, ncyvarid,  gridInqYvalsPtr(gridID));
      if ( ncbyvarid != UNDEFID ) cdf_put_var_double(fileID, ncbyvarid, gridInqYboundsPtr(gridID));
      if ( ncavarid  != UNDEFID ) cdf_put_var_double(fileID, ncavarid,  gridInqAreaPtr(gridID));
    }

  streamptr->xdimID[gridindex] = xdimID;
  streamptr->ydimID[gridindex] = ydimID;
  streamptr->ncxvarID[gridindex] = ncxvarid;
  streamptr->ncyvarID[gridindex] = ncyvarid;
  streamptr->ncavarID[gridindex] = ncavarid;
}

static
void cdfDefRgrid(stream_t *streamptr, int gridID)
{
  int dimID = UNDEFID;

  int vlistID = streamptr->vlistID;
  int ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridInqSize(gridID);

  int iz = 0;
  for ( int index = 0; index < ngrids; index++ )
    {
      if ( streamptr->xdimID[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == GRID_GAUSSIAN_REDUCED )
            {
              size_t dimlen0 = (size_t)gridInqSize(gridID0);

              if ( dimlen == dimlen0 )
                {
                  dimID = streamptr->xdimID[index];
                  break;
                }
              iz++;
            }
        }
    }

  if ( dimID == UNDEFID )
    {
      int fileID  = streamptr->fileID;
      static bool lwarn = true;
      if ( lwarn )
        {
          Warning("Creating a NetCDF file with data on a gaussian reduced grid.");
          Warning("The further processing of the resulting file is unsupported!");
          lwarn = false;
        }

      char axisname[7] = "rgridX";
      if ( iz == 0 ) axisname[5] = '\0';
      else           sprintf(&axisname[5], "%1d", iz+1);

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      cdf_def_dim(fileID, axisname, dimlen, &dimID);

      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  int gridindex = vlistGridIndex(vlistID, gridID);
  streamptr->xdimID[gridindex] = dimID;
}

static
void cdfDefGdim(stream_t *streamptr, int gridID)
{
  int iz = 0;
  int dimID = UNDEFID;

  int vlistID = streamptr->vlistID;
  int ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridInqSize(gridID);

  if ( gridInqYsize(gridID) == 0 )
    for ( int index = 0; index < ngrids; index++ )
      {
        if ( streamptr->xdimID[index] != UNDEFID )
          {
            int gridID0 = vlistGrid(vlistID, index);
            int gridtype0 = gridInqType(gridID0);
            if ( gridtype0 == GRID_GENERIC )
              {
                size_t dimlen0 = (size_t)gridInqSize(gridID0);
                if ( dimlen == dimlen0 )
                  {
                    dimID = streamptr->xdimID[index];
                    break;
                  }
                else
                  iz++;
              }
          }
      }

  if ( gridInqXsize(gridID) == 0 )
    for ( int index = 0; index < ngrids; index++ )
      {
        if ( streamptr->ydimID[index] != UNDEFID )
          {
            int gridID0 = vlistGrid(vlistID, index);
            int gridtype0 = gridInqType(gridID0);
            if ( gridtype0 == GRID_GENERIC )
              {
                size_t dimlen0 = (size_t)gridInqSize(gridID0);
                if ( dimlen == dimlen0 )
                  {
                    dimID = streamptr->ydimID[index];
                    break;
                  }
                else
                  iz++;
              }
          }
      }

  if ( dimID == UNDEFID )
    {
      int fileID  = streamptr->fileID;
      char dimname[CDI_MAX_NAME];
      strcpy(dimname, "gsize");

      dimID = checkDimName(fileID, dimlen, dimname);

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      if ( dimID == UNDEFID ) cdf_def_dim(fileID, dimname, dimlen, &dimID);

      cdf_enddef(fileID);
      streamptr->ncmode = 2;
    }

  int gridindex = vlistGridIndex(vlistID, gridID);
  streamptr->xdimID[gridindex] = dimID;
}

static
void cdfDefGridReference(stream_t *streamptr, int gridID)
{
  int fileID  = streamptr->fileID;
  int number = gridInqNumber(gridID);

  if ( number > 0 )
    {
      cdf_put_att_int(fileID, NC_GLOBAL, "number_of_grid_used", NC_INT, 1, &number);
    }

  const char *gridfile = gridInqReferencePtr(gridID);
  if ( gridfile && gridfile[0] != 0 )
    cdf_put_att_text(fileID, NC_GLOBAL, "grid_file_uri", strlen(gridfile), gridfile);
}

static
void cdfDefGridUUID(stream_t *streamptr, int gridID)
{
  unsigned char uuidOfHGrid[CDI_UUID_SIZE];

  gridInqUUID(gridID, uuidOfHGrid);
  if ( !cdiUUIDIsNull(uuidOfHGrid) )
    {
      char uuidOfHGridStr[37];
      cdiUUID2Str(uuidOfHGrid, uuidOfHGridStr);
      if ( uuidOfHGridStr[0] != 0 && strlen(uuidOfHGridStr) == 36 )
        {
          int fileID  = streamptr->fileID;
          //if ( streamptr->ncmode == 2 ) cdf_redef(fileID);
          cdf_put_att_text(fileID, NC_GLOBAL, "uuidOfHGrid", 36, uuidOfHGridStr);
          //if ( streamptr->ncmode == 2 ) cdf_enddef(fileID);
        }
    }
}

static
void cdfDefZaxisUUID(stream_t *streamptr, int zaxisID)
{
  unsigned char uuidOfVGrid[CDI_UUID_SIZE];
  zaxisInqUUID(zaxisID, uuidOfVGrid);

  if ( uuidOfVGrid[0] != 0 )
    {
      char uuidOfVGridStr[37];
      cdiUUID2Str(uuidOfVGrid, uuidOfVGridStr);
      if ( uuidOfVGridStr[0] != 0 && strlen(uuidOfVGridStr) == 36 )
        {
          int fileID  = streamptr->fileID;
          if ( streamptr->ncmode == 2 ) cdf_redef(fileID);
          cdf_put_att_text(fileID, NC_GLOBAL, "uuidOfVGrid", 36, uuidOfVGridStr);
          if ( streamptr->ncmode == 2 ) cdf_enddef(fileID);
        }
    }
}

static
void cdfDefUnstructured(stream_t *streamptr, int gridID)
{
  int dimID = UNDEFID;
  int ncxvarid = UNDEFID, ncyvarid = UNDEFID;
  int ncbxvarid = UNDEFID, ncbyvarid = UNDEFID, ncavarid = UNDEFID;
  int nvdimID = UNDEFID;
  nc_type xtype = NC_DOUBLE;

  if ( gridInqPrec(gridID) == DATATYPE_FLT32 ) xtype = NC_FLOAT;

  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  int ngrids = vlistNgrids(vlistID);

  size_t dimlen = (size_t)gridInqSize(gridID);
  int gridindex = vlistGridIndex(vlistID, gridID);

  for ( int index = 0; index < ngrids; index++ )
    {
      if ( streamptr->xdimID[index] != UNDEFID )
        {
          int gridID0 = vlistGrid(vlistID, index);
          int gridtype0 = gridInqType(gridID0);
          if ( gridtype0 == GRID_UNSTRUCTURED )
            {
              size_t dimlen0 = (size_t)gridInqSize(gridID0);
              if ( dimlen == dimlen0 )
		if ( gridInqNvertex(gridID0) == gridInqNvertex(gridID) &&
		     IS_EQUAL(gridInqXval(gridID0, 0), gridInqXval(gridID, 0)) &&
                     IS_EQUAL(gridInqXval(gridID0, (int)dimlen-1), gridInqXval(gridID, (int)dimlen-1)) &&
		     IS_EQUAL(gridInqYval(gridID0, 0), gridInqYval(gridID, 0)) &&
                     IS_EQUAL(gridInqYval(gridID0, (int)dimlen-1), gridInqYval(gridID, (int)dimlen-1)) )
		  {
		    dimID = streamptr->xdimID[index];
                    ncxvarid = streamptr->ncxvarID[index];
                    ncyvarid = streamptr->ncyvarID[index];
                    ncavarid = streamptr->ncavarID[index];
		    break;
		  }
            }
        }
    }

  if ( dimID == UNDEFID )
    {
      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);
      {
        char xdimname[CDI_MAX_NAME+3];
        xdimname[0] = 0;
        cdiGridInqString(gridID, CDI_GRID_XDIMNAME, CDI_MAX_NAME, xdimname);
        if ( xdimname[0] == 0 ) strcpy(xdimname, "ncells");
        dimID = checkDimName(fileID, dimlen, xdimname);
        if ( dimID == UNDEFID ) cdf_def_dim(fileID, xdimname, dimlen, &dimID);
      }

      size_t nvertex = (size_t)gridInqNvertex(gridID);
      if ( nvertex > 0 )
        {
          char vdimname[CDI_MAX_NAME+3];
          vdimname[0] = 0;
          cdiGridInqString(gridID, CDI_GRID_VDIMNAME, CDI_MAX_NAME, vdimname);
          if ( vdimname[0] == 0 ) strcpy(vdimname, "vertices");
          nvdimID = checkDimName(fileID, nvertex, vdimname);
          if ( nvdimID == UNDEFID ) cdf_def_dim(fileID, vdimname, nvertex, &nvdimID);
        }

      cdfDefGridReference(streamptr, gridID);

      cdfDefGridUUID(streamptr, gridID);

      if ( gridInqXvalsPtr(gridID) )
        {
          char xaxisname[CDI_MAX_NAME];
          gridInqXname(gridID, xaxisname);
          checkGridName(xaxisname, fileID, vlistID, gridID, ngrids, 'X');
          cdf_def_var(fileID, xaxisname, xtype, 1, &dimID, &ncxvarid);
          cdfGridCompress(fileID, ncxvarid, (int)dimlen, streamptr->filetype, streamptr->comptype);

          cdfPutGridStdAtts(fileID, ncxvarid, gridID, &gridInqsX);

          if ( gridInqXboundsPtr(gridID) && nvdimID != UNDEFID )
            {
              int dimIDs[2] = { dimID, nvdimID };
              size_t xaxisnameLen = strlen(xaxisname);
              xaxisname[xaxisnameLen] = '_';
              memcpy(xaxisname + xaxisnameLen + 1, bndsName, sizeof (bndsName));
              cdf_def_var(fileID, xaxisname, xtype, 2, dimIDs, &ncbxvarid);
              cdfGridCompress(fileID, ncbxvarid, (int)dimlen, streamptr->filetype, streamptr->comptype);

              cdf_put_att_text(fileID, ncxvarid, "bounds", xaxisnameLen + sizeof (bndsName), xaxisname);
            }
        }

      if ( gridInqYvalsPtr(gridID) )
        {
          char yaxisname[CDI_MAX_NAME];
          gridInqYname(gridID, yaxisname);
          checkGridName(yaxisname, fileID, vlistID, gridID, ngrids, 'Y');
          cdf_def_var(fileID, yaxisname, xtype, 1, &dimID, &ncyvarid);
          cdfGridCompress(fileID, ncyvarid, (int)dimlen, streamptr->filetype, streamptr->comptype);

          cdfPutGridStdAtts(fileID, ncyvarid, gridID, &gridInqsY);

          if ( gridInqYboundsPtr(gridID) && nvdimID != UNDEFID )
            {
              int dimIDs[2] = { dimID, nvdimID };
              size_t yaxisnameLen = strlen(yaxisname);
              yaxisname[yaxisnameLen] = '_';
              memcpy(yaxisname + yaxisnameLen + 1, bndsName, sizeof (bndsName));
              cdf_def_var(fileID, yaxisname, xtype, 2, dimIDs, &ncbyvarid);
              cdfGridCompress(fileID, ncbyvarid, (int)dimlen, streamptr->filetype, streamptr->comptype);

              cdf_put_att_text(fileID, ncyvarid, "bounds", yaxisnameLen + sizeof (bndsName), yaxisname);
            }
        }

      if ( gridInqAreaPtr(gridID) )
        {
          static const char yaxisname_[] = "cell_area";
          static const char units[] = "m2";
          static const char longname[] = "area of grid cell";
          static const char stdname[] = "cell_area";

          cdf_def_var(fileID, yaxisname_, xtype, 1, &dimID, &ncavarid);

          cdf_put_att_text(fileID, ncavarid, "standard_name", sizeof (stdname) - 1, stdname);
          cdf_put_att_text(fileID, ncavarid, "long_name", sizeof (longname) - 1, longname);
          cdf_put_att_text(fileID, ncavarid, "units", sizeof (units) - 1, units);
        }

      cdf_enddef(fileID);
      streamptr->ncmode = 2;

      if ( ncxvarid  != UNDEFID ) cdf_put_var_double(fileID, ncxvarid,  gridInqXvalsPtr(gridID));
      if ( ncbxvarid != UNDEFID ) cdf_put_var_double(fileID, ncbxvarid, gridInqXboundsPtr(gridID));
      if ( ncyvarid  != UNDEFID ) cdf_put_var_double(fileID, ncyvarid,  gridInqYvalsPtr(gridID));
      if ( ncbyvarid != UNDEFID ) cdf_put_var_double(fileID, ncbyvarid, gridInqYboundsPtr(gridID));
      if ( ncavarid  != UNDEFID ) cdf_put_var_double(fileID, ncavarid,  gridInqAreaPtr(gridID));
    }

  streamptr->xdimID[gridindex] = dimID;
  streamptr->ncxvarID[gridindex] = ncxvarid;
  streamptr->ncyvarID[gridindex] = ncyvarid;
  streamptr->ncavarID[gridindex] = ncavarid;
}

struct attTxtTab2
{
  const char *attName, *attVal;
  size_t valLen;
};

static
void cdf_def_vct_echam(stream_t *streamptr, int zaxisID)
{
  int type = zaxisInqType(zaxisID);

  if ( type == ZAXIS_HYBRID || type == ZAXIS_HYBRID_HALF )
    {
      int ilev = zaxisInqVctSize(zaxisID)/2;
      if ( ilev == 0 ) return;

      int mlev = ilev - 1;
      size_t start;
      size_t count = 1;
      int ncdimid, ncdimid2;
      int hyaiid, hybiid, hyamid, hybmid;
      double mval;

      if ( streamptr->vct.ilev > 0 )
        {
          if ( streamptr->vct.ilev != ilev )
            Error("more than one VCT for each file unsupported!");
          return;
        }

      int fileID = streamptr->fileID;

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      cdf_def_dim(fileID, "nhym", (size_t)mlev, &ncdimid);
      cdf_def_dim(fileID, "nhyi", (size_t)ilev, &ncdimid2);

      streamptr->vct.mlev   = mlev;
      streamptr->vct.ilev   = ilev;
      streamptr->vct.mlevID = ncdimid;
      streamptr->vct.ilevID = ncdimid2;

      cdf_def_var(fileID, "hyai", NC_DOUBLE, 1, &ncdimid2, &hyaiid);
      cdf_def_var(fileID, "hybi", NC_DOUBLE, 1, &ncdimid2, &hybiid);
      cdf_def_var(fileID, "hyam", NC_DOUBLE, 1, &ncdimid,  &hyamid);
      cdf_def_var(fileID, "hybm", NC_DOUBLE, 1, &ncdimid,  &hybmid);

      {
        static const char lname_n[] = "long_name",
          lname_v_ai[] = "hybrid A coefficient at layer interfaces",
          units_n[] = "units",
          units_v_ai[] = "Pa",
          lname_v_bi[] = "hybrid B coefficient at layer interfaces",
          units_v_bi[] = "1",
          lname_v_am[] = "hybrid A coefficient at layer midpoints",
          units_v_am[] = "Pa",
          lname_v_bm[] = "hybrid B coefficient at layer midpoints",
          units_v_bm[] = "1";
        static const struct attTxtTab2 tab[]
          = {
          { lname_n, lname_v_ai, sizeof (lname_v_ai) - 1 },
          { units_n, units_v_ai, sizeof (units_v_ai) - 1 },
          { lname_n, lname_v_bi, sizeof (lname_v_bi) - 1 },
          { units_n, units_v_bi, sizeof (units_v_bi) - 1 },
          { lname_n, lname_v_am, sizeof (lname_v_am) - 1 },
          { units_n, units_v_am, sizeof (units_v_am) - 1 },
          { lname_n, lname_v_bm, sizeof (lname_v_bm) - 1 },
          { units_n, units_v_bm, sizeof (units_v_bm) - 1 },
        };
        enum { tabLen = sizeof (tab) / sizeof (tab[0]) };
        int ids[tabLen] = { hyaiid, hyaiid, hybiid, hybiid,
                            hyamid, hyamid, hybmid, hybmid };
        for ( size_t i = 0; i < tabLen; ++i )
          cdf_put_att_text(fileID, ids[i], tab[i].attName, tab[i].valLen, tab[i].attVal);
      }

      cdf_enddef(fileID);
      streamptr->ncmode = 2;

      const double *vctptr = zaxisInqVctPtr(zaxisID);

      cdf_put_var_double(fileID, hyaiid, vctptr);
      cdf_put_var_double(fileID, hybiid, vctptr+ilev);

      for ( int i = 0; i < mlev; i++ )
        {
          start = (size_t)i;
          mval = (vctptr[i] + vctptr[i+1]) * 0.5;
          cdf_put_vara_double(fileID, hyamid, &start, &count, &mval);
          mval = (vctptr[ilev+i] + vctptr[ilev+i+1]) * 0.5;
          cdf_put_vara_double(fileID, hybmid, &start, &count, &mval);
        }
    }
}

static
void cdf_def_vct_cf(stream_t *streamptr, int zaxisID, int nclevID, int ncbndsID)
{
  int type = zaxisInqType(zaxisID);

  if ( type == ZAXIS_HYBRID || type == ZAXIS_HYBRID_HALF )
    {
      int ilev = zaxisInqVctSize(zaxisID)/2;
      if ( ilev == 0 ) return;

      int mlev = ilev - 1;
      int hyaiid = 0, hybiid = 0, hyamid, hybmid;

      if ( streamptr->vct.ilev > 0 )
        {
          if ( streamptr->vct.ilev != ilev )
            Error("more than one VCT for each file unsupported!");
          return;
        }

      int fileID = streamptr->fileID;

      if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

      int dimIDs[2];
      dimIDs[0] = nclevID;
      dimIDs[1] = ncbndsID;

      streamptr->vct.mlev   = mlev;
      streamptr->vct.ilev   = ilev;
      streamptr->vct.mlevID = nclevID;
      streamptr->vct.ilevID = nclevID;

      cdf_def_var(fileID, "ap", NC_DOUBLE, 1, dimIDs,  &hyamid);
      cdf_def_var(fileID, "b",  NC_DOUBLE, 1, dimIDs,  &hybmid);

      {
        static const char lname[] = "vertical coordinate formula term: ap(k)";
        cdf_put_att_text(fileID, hyamid, "long_name", sizeof (lname) - 1, lname);
      }
      {
        static const char units[] = "Pa";
        cdf_put_att_text(fileID, hyamid, "units", sizeof (units) - 1, units);
      }
      {
        static const char lname[] = "vertical coordinate formula term: b(k)";
        cdf_put_att_text(fileID, hybmid, "long_name", sizeof (lname) - 1, lname);
      }
      {
        static const char units[] = "1";
        cdf_put_att_text(fileID, hybmid, "units", sizeof (units) - 1, units);
      }

      if ( ncbndsID != -1 )
        {
          cdf_def_var(fileID, "ap_bnds", NC_DOUBLE, 2, dimIDs, &hyaiid);
          cdf_def_var(fileID, "b_bnds",  NC_DOUBLE, 2, dimIDs, &hybiid);
          {
            static const char lname[] = "vertical coordinate formula term: ap(k+1/2)";
            cdf_put_att_text(fileID, hyaiid, "long_name", sizeof (lname) - 1, lname);
          }
          {
            static const char units[] = "Pa";
            cdf_put_att_text(fileID, hyaiid, "units", sizeof (units) - 1, units);
          }
          {
            static const char lname[] = "vertical coordinate formula term: b(k+1/2)";
            cdf_put_att_text(fileID, hybiid, "long_name", sizeof (lname) - 1, lname);
          }
          {
            static const char units[] = "1";
            cdf_put_att_text(fileID, hybiid, "units", sizeof (units) - 1, units);
          }
        }

      cdf_enddef(fileID);
      streamptr->ncmode = 2;

      const double *vctptr = zaxisInqVctPtr(zaxisID);
      double tarray[ilev*2];

      if ( ncbndsID != -1 )
        {
          for ( int i = 0; i < mlev; ++i )
            {
              tarray[2*i  ] = vctptr[i];
              tarray[2*i+1] = vctptr[i+1];
            }
          cdf_put_var_double(fileID, hyaiid, tarray);

          for ( int i = 0; i < mlev; ++i )
            {
              tarray[2*i  ] = vctptr[ilev+i];
              tarray[2*i+1] = vctptr[ilev+i+1];
            }
          cdf_put_var_double(fileID, hybiid, tarray);
        }

      for ( int i = 0; i < mlev; ++i )
        tarray[i] = (vctptr[i] + vctptr[i+1]) * 0.5;
      cdf_put_var_double(fileID, hyamid, tarray);

      for ( int i = 0; i < mlev; ++i )
        tarray[i] = (vctptr[ilev+i] + vctptr[ilev+i+1]) * 0.5;
      cdf_put_var_double(fileID, hybmid, tarray);
    }
}

struct attTxtTab { const char *txt; size_t txtLen; };

static
void cdf_def_zaxis_hybrid_echam(stream_t *streamptr, int type, int ncvarid, int zaxisID, int zaxisindex, int xtype, size_t dimlen, int *dimID, char *axisname)
{
  int fileID  = streamptr->fileID;

  if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

  cdf_def_dim(fileID, axisname, dimlen, dimID);
  cdf_def_var(fileID, axisname, (nc_type) xtype, 1, dimID,  &ncvarid);

  {
    static const char sname[] = "hybrid_sigma_pressure";
    cdf_put_att_text(fileID, ncvarid, "standard_name", sizeof (sname) - 1, sname);
  }
  {
    static const char *attName[] = {
      "long_name",
      "formula",
      "formula_terms"
    };
    enum { nAtt = sizeof (attName) / sizeof (attName[0]) };
    static const char lname_m[] = "hybrid level at layer midpoints",
      formula_m[] = "hyam hybm (mlev=hyam+hybm*aps)",
      fterms_m[] = "ap: hyam b: hybm ps: aps",
      lname_i[] = "hybrid level at layer interfaces",
      formula_i[] = "hyai hybi (ilev=hyai+hybi*aps)",
      fterms_i[] = "ap: hyai b: hybi ps: aps";
    static const struct attTxtTab tab[2][nAtt] = {
      {
        { lname_i, sizeof (lname_i) - 1 },
        { formula_i, sizeof (formula_i) - 1 },
        { fterms_i, sizeof (fterms_i) - 1 }
      },
      {
        { lname_m, sizeof (lname_m) - 1 },
        { formula_m, sizeof (formula_m) - 1 },
        { fterms_m, sizeof (fterms_m) - 1 }
      }
    };

    size_t tabSelect = type == ZAXIS_HYBRID;
    for (size_t i = 0; i < nAtt; ++i)
      cdf_put_att_text(fileID, ncvarid, attName[i],
                       tab[tabSelect][i].txtLen, tab[tabSelect][i].txt);
  }

  {
    static const char units[] = "level";
    cdf_put_att_text(fileID, ncvarid, "units", sizeof (units) - 1, units);
  }
  {
    static const char direction[] = "down";
    cdf_put_att_text(fileID, ncvarid, "positive", sizeof (direction) - 1, direction);
  }

  cdf_enddef(fileID);
  streamptr->ncmode = 2;

  cdf_put_var_double(fileID, ncvarid, zaxisInqLevelsPtr(zaxisID));

  cdf_def_vct_echam(streamptr, zaxisID);

  if ( *dimID == UNDEFID )
    {
      if ( type == ZAXIS_HYBRID )
        streamptr->zaxisID[zaxisindex] = streamptr->vct.mlevID;
      else
        streamptr->zaxisID[zaxisindex] = streamptr->vct.ilevID;
    }
}

static
void cdf_def_zaxis_hybrid_cf(stream_t *streamptr, int type, int ncvarid, int zaxisID, int zaxisindex, int xtype, size_t dimlen, int *dimID, char *axisname)
{
  char psname[CDI_MAX_NAME];
  psname[0] = 0;
  zaxisInqPsName(zaxisID, psname);
  if ( psname[0] == 0 ) strcpy(psname, "ps");

  int fileID = streamptr->fileID;
  if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

  strcpy(axisname, "lev");

  cdf_def_dim(fileID, axisname, dimlen, dimID);
  cdf_def_var(fileID, axisname, (nc_type) xtype, 1, dimID,  &ncvarid);

  {
    static const char sname[] = "standard_name",
      sname_v[] = "atmosphere_hybrid_sigma_pressure_coordinate",
      lname[] = "long_name",
      lname_v[] = "hybrid sigma pressure coordinate",
      formula[] = "formula",
      formula_v[] = "p = ap + b*ps",
      fterms[] = "formula_terms",
      fterms_v[] = "ap: ap b: b ps: ",
      units[] = "units",
      units_v[] = "1",
      axis[] = "axis",
      axis_v[] = "Z",
      direction[] = "positive",
      direction_v[] = "down";
    struct attTxtTab2 tab[] = {
      { sname, sname_v, sizeof (sname_v) - 1 },
      { lname, lname_v, sizeof (lname_v) - 1 },
      { formula, formula_v, sizeof (formula_v) - 1 },
      { fterms, fterms_v, sizeof (fterms_v) - 1 },
      { units, units_v, sizeof (units_v) - 1 },
      { axis, axis_v, sizeof (axis_v) - 1 },
      { direction, direction_v, sizeof (direction_v) - 1 },
    };
    enum { nAtt = sizeof (tab) / sizeof (tab[0]) };
    for (size_t i = 0; i < nAtt; ++i)
      cdf_put_att_text(fileID, ncvarid, tab[i].attName, tab[i].valLen, tab[i].attVal);
  }

  int ncbvarid = UNDEFID;
  int nvdimID = UNDEFID;

  double lbounds[dimlen], ubounds[dimlen], levels[dimlen];

  zaxisInqLevels(zaxisID, levels);

  if ( zaxisInqLbounds(zaxisID, NULL) && zaxisInqUbounds(zaxisID, NULL) )
    {
      zaxisInqLbounds(zaxisID, lbounds);
      zaxisInqUbounds(zaxisID, ubounds);
    }
  else
    {
      for ( size_t i = 0; i < dimlen; ++i ) lbounds[i] = levels[i];
      for ( size_t i = 0; i < dimlen-1; ++i ) ubounds[i] = levels[i+1];
      ubounds[dimlen-1] = levels[dimlen-1] + 1;
    }

  //if ( zaxisInqLbounds(zaxisID, NULL) && zaxisInqUbounds(zaxisID, NULL) )
    {
      size_t nvertex = 2;
      if ( nc_inq_dimid(fileID, bndsName, &nvdimID) != NC_NOERR )
        cdf_def_dim(fileID, bndsName, nvertex, &nvdimID);

      if ( nvdimID != UNDEFID )
        {
          size_t axisnameLen = strlen(axisname);
          axisname[axisnameLen] = '_';
          memcpy(axisname + axisnameLen + 1, bndsName, sizeof (bndsName));
          axisnameLen += sizeof (bndsName);
          int dimIDs[2] = { *dimID, nvdimID };
          cdf_def_var(fileID, axisname, (nc_type) xtype, 2, dimIDs, &ncbvarid);
          cdf_put_att_text(fileID, ncvarid, "bounds", axisnameLen, axisname);
          {
            static const char sname[] = "standard_name",
              sname_v[] = "atmosphere_hybrid_sigma_pressure_coordinate",
              formula[] = "formula",
              formula_v[] = "p = ap + b*ps";
            struct attTxtTab2 tab[] = {
              { sname, sname_v, sizeof (sname_v) - 1 },
              { formula, formula_v, sizeof (formula_v) - 1 },
            };
            enum { nAtt = sizeof (tab) / sizeof (tab[0]) };
            for (size_t i = 0; i < nAtt; ++i)
              cdf_put_att_text(fileID, ncbvarid, tab[i].attName, tab[i].valLen, tab[i].attVal);
          }
          {
            char txt[CDI_MAX_NAME];
            size_t len = (size_t)(sprintf(txt, "%s%s", "ap: ap_bnds b: b_bnds ps: ", psname));
            cdf_put_att_text(fileID, ncbvarid, "formula_terms", len, txt);
          }
          {
            static const char units[] = "1";
            cdf_put_att_text(fileID, ncbvarid, "units", sizeof (units) - 1, units);
          }
        }
    }

  cdf_enddef(fileID);
  streamptr->ncmode = 2;

  cdf_put_var_double(fileID, ncvarid, levels);

  if ( ncbvarid != UNDEFID )
    {
      double zbounds[2*dimlen];
      for ( size_t i = 0; i < dimlen; ++i )
        {
          zbounds[2*i  ] = lbounds[i];
          zbounds[2*i+1] = ubounds[i];
        }
      cdf_put_var_double(fileID, ncbvarid, zbounds);
    }

  cdf_def_vct_cf(streamptr, zaxisID, *dimID, nvdimID);

  if ( *dimID == UNDEFID )
    {
      if ( type == ZAXIS_HYBRID )
        streamptr->zaxisID[zaxisindex] = streamptr->vct.mlevID;
      else
        streamptr->zaxisID[zaxisindex] = streamptr->vct.ilevID;
    }
}

static
void cdf_def_zaxis_hybrid(stream_t *streamptr, int type, int ncvarid, int zaxisID, int zaxisindex, int xtype, size_t dimlen, int *dimID, char *axisname)
{
  if ( (!CDI_cmor_mode && cdiConvention == CDI_CONVENTION_ECHAM) || type == ZAXIS_HYBRID_HALF )
    cdf_def_zaxis_hybrid_echam(streamptr, type, ncvarid, zaxisID, zaxisindex, xtype, dimlen, dimID, axisname);
  else
    cdf_def_zaxis_hybrid_cf(streamptr, type, ncvarid, zaxisID, zaxisindex, xtype, dimlen, dimID, axisname);
}

static
void cdfDefZaxis(stream_t *streamptr, int zaxisID)
{
  /*  char zaxisname0[CDI_MAX_NAME]; */
  char axisname[CDI_MAX_NAME];
  int dimID = UNDEFID;
  int dimIDs[2];
  int ncvarid = UNDEFID, ncbvarid = UNDEFID;
  int nvdimID = UNDEFID;
  int xtype = NC_DOUBLE;

  if ( zaxisInqPrec(zaxisID) == DATATYPE_FLT32 ) xtype = NC_FLOAT;

  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  int zaxisindex = vlistZaxisIndex(vlistID, zaxisID);

  int nzaxis = vlistNzaxis(vlistID);

  size_t dimlen = (size_t)zaxisInqSize(zaxisID);
  int type   = zaxisInqType(zaxisID);

  int is_scalar = FALSE;
  if ( dimlen == 1 )
    {
      is_scalar = zaxisInqScalar(zaxisID);
      if ( !is_scalar && CDI_cmor_mode )
        {
          is_scalar = TRUE;
          zaxisDefScalar(zaxisID);
        }
    }

  int ndims = 1;
  if ( is_scalar ) ndims = 0;

  if ( dimlen == 1 )
    switch (type)
      {
      case ZAXIS_SURFACE:
      case ZAXIS_CLOUD_BASE:
      case ZAXIS_CLOUD_TOP:
      case ZAXIS_ISOTHERM_ZERO:
      case ZAXIS_TOA:
      case ZAXIS_SEA_BOTTOM:
      case ZAXIS_ATMOSPHERE:
      case ZAXIS_MEANSEA:
      case ZAXIS_LAKE_BOTTOM:
      case ZAXIS_SEDIMENT_BOTTOM:
      case ZAXIS_SEDIMENT_BOTTOM_TA:
      case ZAXIS_SEDIMENT_BOTTOM_TW:
      case ZAXIS_MIX_LAYER:
        return;
      }

  zaxisInqName(zaxisID, axisname);

  if ( dimID == UNDEFID )
    {
      checkZaxisName(axisname, fileID, vlistID, zaxisID, nzaxis);

      char dimname[CDI_MAX_NAME+3];
      dimname[0] = 0;
      //cdiZaxisInqString(zaxisID, CDI_ZAXIS_DIMNAME, CDI_MAX_NAME, dimname);
      if ( dimname[0] == 0 ) strcpy(dimname, axisname);

      if ( type == ZAXIS_REFERENCE ) cdfDefZaxisUUID(streamptr, zaxisID);

      if ( type == ZAXIS_HYBRID || type == ZAXIS_HYBRID_HALF )
        {
          cdf_def_zaxis_hybrid(streamptr, type, ncvarid, zaxisID, zaxisindex, xtype, dimlen, &dimID, axisname);
        }
      else
        {
          dimID = checkDimName(fileID, dimlen, dimname);

          if ( streamptr->ncmode == 2 ) cdf_redef(fileID);

          if ( ndims && dimID == UNDEFID ) cdf_def_dim(fileID, dimname, dimlen, &dimID);

          cdf_def_var(fileID, axisname, (nc_type) xtype, ndims, &dimID, &ncvarid);

          cdfPutGridStdAtts(fileID, ncvarid, zaxisID, &gridInqsZ);

          {
            int positive = zaxisInqPositive(zaxisID);
            static const char positive_up[] = "up",
              positive_down[] = "down";
            static const struct attTxtTab tab[2] = {
              { positive_up, sizeof (positive_up) - 1 },
              { positive_down, sizeof (positive_down) - 1 },
            };
            if ( positive == POSITIVE_UP || positive == POSITIVE_DOWN )
              {
                size_t select = positive == POSITIVE_DOWN;
                cdf_put_att_text(fileID, ncvarid, "positive",
                                 tab[select].txtLen, tab[select].txt);
              }
          }
          cdf_put_att_text(fileID, ncvarid, "axis", 1, "Z");

	  if ( zaxisInqLbounds(zaxisID, NULL) && zaxisInqUbounds(zaxisID, NULL) )
            {
              size_t nvertex = 2;
	      if ( nc_inq_dimid(fileID, bndsName, &nvdimID) != NC_NOERR )
		cdf_def_dim(fileID, bndsName, nvertex, &nvdimID);

	      if ( nvdimID != UNDEFID )
		{
                  size_t axisnameLen = strlen(axisname);
                  axisname[axisnameLen] = '_';
                  memcpy(axisname + axisnameLen + 1, bndsName, sizeof (bndsName));
		  dimIDs[0] = dimID;
		  dimIDs[ndims] = nvdimID;
		  cdf_def_var(fileID, axisname, (nc_type) xtype, ndims+1, dimIDs, &ncbvarid);
		  cdf_put_att_text(fileID, ncvarid, "bounds", strlen(axisname), axisname);
		}
	    }

          cdf_enddef(fileID);
          streamptr->ncmode = 2;

          cdf_put_var_double(fileID, ncvarid, zaxisInqLevelsPtr(zaxisID));

          if ( ncbvarid != UNDEFID )
	    {
              double lbounds[dimlen], ubounds[dimlen], zbounds[2*dimlen];
	      zaxisInqLbounds(zaxisID, lbounds);
	      zaxisInqUbounds(zaxisID, ubounds);
	      for ( size_t i = 0; i < dimlen; ++i )
		{
		  zbounds[2*i  ] = lbounds[i];
		  zbounds[2*i+1] = ubounds[i];
		}

	      cdf_put_var_double(fileID, ncbvarid, zbounds);
	    }

          if ( ndims == 0 ) streamptr->nczvarID[zaxisindex] = ncvarid;
        }
    }

  if ( dimID != UNDEFID )
    streamptr->zaxisID[zaxisindex] = dimID;
}

static
void cdfDefPole(stream_t *streamptr, int gridID)
{
  int ncvarid = UNDEFID;
  static const char varname[] = "rotated_pole";
  static const char mapname[] = "rotated_latitude_longitude";

  int fileID  = streamptr->fileID;

  double ypole = gridInqYpole(gridID);
  double xpole = gridInqXpole(gridID);
  double angle = gridInqAngle(gridID);

  cdf_redef(fileID);

  int ncerrcode = nc_def_var(fileID, varname, (nc_type) NC_CHAR, 0, NULL, &ncvarid);
  if ( ncerrcode == NC_NOERR )
    {
      cdf_put_att_text(fileID, ncvarid, "grid_mapping_name", sizeof (mapname) - 1, mapname);
      cdf_put_att_double(fileID, ncvarid, "grid_north_pole_latitude", NC_DOUBLE, 1, &ypole);
      cdf_put_att_double(fileID, ncvarid, "grid_north_pole_longitude", NC_DOUBLE, 1, &xpole);
      if ( IS_NOT_EQUAL(angle, 0) )
        cdf_put_att_double(fileID, ncvarid, "north_pole_grid_longitude", NC_DOUBLE, 1, &angle);
    }

  cdf_enddef(fileID);
}


static
void cdfDefMapping(stream_t *streamptr, int gridID)
{
  int ncvarid = UNDEFID;
  int fileID  = streamptr->fileID;

  if ( gridInqType(gridID) == GRID_SINUSOIDAL )
    {
      static const char varname[] = "sinusoidal";
      static const char mapname[] = "sinusoidal";

      cdf_redef(fileID);

      int ncerrcode = nc_def_var(fileID, varname, (nc_type) NC_CHAR, 0, NULL, &ncvarid);
      if ( ncerrcode == NC_NOERR )
        {
          cdf_put_att_text(fileID, ncvarid, "grid_mapping_name", strlen(mapname), mapname);
          /*
          cdf_put_att_double(fileID, ncvarid, "grid_north_pole_latitude", NC_DOUBLE, 1, &ypole);
          cdf_put_att_double(fileID, ncvarid, "grid_north_pole_longitude", NC_DOUBLE, 1, &xpole);
          */
        }

      cdf_enddef(fileID);
    }
  else if ( gridInqType(gridID) == GRID_LAEA )
    {
      static const char varname[] = "laea";
      static const char mapname[] = "lambert_azimuthal_equal_area";

      cdf_redef(fileID);

      int ncerrcode = nc_def_var(fileID, varname, (nc_type) NC_CHAR, 0, NULL, &ncvarid);
      if ( ncerrcode == NC_NOERR )
        {
          double a, lon_0, lat_0;

          gridInqLaea(gridID, &a, &lon_0, &lat_0);

          cdf_put_att_text(fileID, ncvarid, "grid_mapping_name", strlen(mapname), mapname);
          cdf_put_att_double(fileID, ncvarid, "earth_radius", NC_DOUBLE, 1, &a);
          cdf_put_att_double(fileID, ncvarid, "longitude_of_projection_origin", NC_DOUBLE, 1, &lon_0);
          cdf_put_att_double(fileID, ncvarid, "latitude_of_projection_origin", NC_DOUBLE, 1, &lat_0);
        }

      cdf_enddef(fileID);
    }
  else if ( gridInqType(gridID) == GRID_LCC2 )
    {
      static const char varname[] = "Lambert_Conformal";
      static const char mapname[] = "lambert_conformal_conic";

      cdf_redef(fileID);

      int ncerrcode = nc_def_var(fileID, varname, (nc_type) NC_CHAR, 0, NULL, &ncvarid);
      if ( ncerrcode == NC_NOERR )
        {
          double radius, lon_0, lat_0, lat_1, lat_2;

          gridInqLcc2(gridID, &radius, &lon_0, &lat_0, &lat_1, &lat_2);

          cdf_put_att_text(fileID, ncvarid, "grid_mapping_name", strlen(mapname), mapname);
          if ( radius > 0 )
            cdf_put_att_double(fileID, ncvarid, "earth_radius", NC_DOUBLE, 1, &radius);
          cdf_put_att_double(fileID, ncvarid, "longitude_of_central_meridian", NC_DOUBLE, 1, &lon_0);
          cdf_put_att_double(fileID, ncvarid, "latitude_of_projection_origin", NC_DOUBLE, 1, &lat_0);
          if ( IS_EQUAL(lat_1, lat_2) )
            cdf_put_att_double(fileID, ncvarid, "standard_parallel", NC_DOUBLE, 1, &lat_1);
          else
            {
              double lat_1_2[2];
              lat_1_2[0] = lat_1;
              lat_1_2[1] = lat_2;
              cdf_put_att_double(fileID, ncvarid, "standard_parallel", NC_DOUBLE, 2, lat_1_2);
            }
        }

      cdf_enddef(fileID);
    }
}


static
void cdfDefGrid(stream_t *streamptr, int gridID)
{
  int vlistID = streamptr->vlistID;
  int gridindex = vlistGridIndex(vlistID, gridID);
  if ( streamptr->xdimID[gridindex] != UNDEFID ) return;

  int gridtype = gridInqType(gridID);
  int size     = gridInqSize(gridID);

  if ( CDI_Debug )
    Message("gridtype = %d  size = %d", gridtype, size);

  if ( gridtype == GRID_GAUSSIAN ||
       gridtype == GRID_LONLAT   ||
       gridtype == GRID_GENERIC )
    {
      if ( gridtype == GRID_GENERIC )
        {
          if ( size == 1 && gridInqXsize(gridID) == 0 && gridInqYsize(gridID) == 0 )
            {
              /* no grid information */
            }
          else
            {
              int lx = 0, ly = 0;
              if ( gridInqXsize(gridID) > 0 /*&& gridInqXvals(gridID, NULL) > 0*/ )
                {
                  cdfDefXaxis(streamptr, gridID, 1);
                  lx = 1;
                }

              if ( gridInqYsize(gridID) > 0 /*&& gridInqYvals(gridID, NULL) > 0*/ )
                {
                  cdfDefYaxis(streamptr, gridID, 1);
                  ly = 1;
                }

              if ( lx == 0 && ly == 0 ) cdfDefGdim(streamptr, gridID);
            }
        }
      else
        {
          int ndims = 1;
          if ( gridtype == GRID_LONLAT && size == 1 && gridInqHasDims(gridID) == FALSE )
            ndims = 0;

          if ( gridInqXsize(gridID) > 0 ) cdfDefXaxis(streamptr, gridID, ndims);
          if ( gridInqYsize(gridID) > 0 ) cdfDefYaxis(streamptr, gridID, ndims);
        }

      if ( gridIsRotated(gridID) ) cdfDefPole(streamptr, gridID);
    }
  else if ( gridtype == GRID_CURVILINEAR )
    {
      cdfDefCurvilinear(streamptr, gridID);
    }
  else if ( gridtype == GRID_UNSTRUCTURED )
    {
      cdfDefUnstructured(streamptr, gridID);
    }
  else if ( gridtype == GRID_GAUSSIAN_REDUCED )
    {
      cdfDefRgrid(streamptr, gridID);
    }
  else if ( gridtype == GRID_SPECTRAL )
    {
      cdfDefComplex(streamptr, gridID);
      cdfDefSP(streamptr, gridID);
    }
  else if ( gridtype == GRID_FOURIER )
    {
      cdfDefComplex(streamptr, gridID);
      cdfDefFC(streamptr, gridID);
    }
  else if ( gridtype == GRID_TRAJECTORY )
    {
      cdfDefTrajLon(streamptr, gridID);
      cdfDefTrajLat(streamptr, gridID);
    }
  else if ( gridtype == GRID_SINUSOIDAL || gridtype == GRID_LAEA || gridtype == GRID_LCC2 )
    {
      cdfDefXaxis(streamptr, gridID, 1);
      cdfDefYaxis(streamptr, gridID, 1);

      cdfDefMapping(streamptr, gridID);
    }
  /*
  else if ( gridtype == GRID_LCC )
    {
      cdfDefLcc(streamptr, gridID);
    }
  */
  else
    {
      Error("Unsupported grid type: %s", gridNamePtr(gridtype));
    }
}

static
void scale_add(size_t size, double *data, double addoffset, double scalefactor)
{
  int laddoffset;
  int lscalefactor;

  laddoffset   = IS_NOT_EQUAL(addoffset, 0);
  lscalefactor = IS_NOT_EQUAL(scalefactor, 1);

  if ( laddoffset || lscalefactor )
    {
      for (size_t i = 0; i < size; ++i )
        {
          if ( lscalefactor ) data[i] *= scalefactor;
          if ( laddoffset )   data[i] += addoffset;
        }
    }
}

static
void cdfCreateRecords(stream_t *streamptr, int tsID)
{
  if ( tsID < 0 || (tsID >= streamptr->ntsteps && tsID > 0) ) return;

  if ( streamptr->tsteps[tsID].nallrecs > 0 ) return;

  int vlistID  = streamptr->vlistID;

  tsteps_t* sourceTstep = streamptr->tsteps;
  tsteps_t* destTstep = sourceTstep + tsID;

  int nvars = vlistNvars(vlistID);
  int nrecs = vlistNrecs(vlistID);

  if ( nrecs <= 0 ) return;

  if ( tsID == 0 )
    {
      int nvrecs = nrecs; /* use all records at first timestep */

      streamptr->nrecs += nrecs;

      destTstep->records    = (record_t *) Malloc((size_t)nrecs*sizeof(record_t));
      destTstep->nrecs      = nrecs;
      destTstep->nallrecs   = nrecs;
      destTstep->recordSize = nrecs;
      destTstep->curRecID   = UNDEFID;
      destTstep->recIDs     = (int *) Malloc((size_t)nvrecs*sizeof (int));;
      for ( int recID = 0; recID < nvrecs; recID++ ) destTstep->recIDs[recID] = recID;

      record_t *records = destTstep->records;

      for ( int varID = 0, recID = 0; varID < nvars; varID++ )
        {
          int zaxisID = vlistInqVarZaxis(vlistID, varID);
          int nlev    = zaxisInqSize(zaxisID);
          for ( int levelID = 0; levelID < nlev; levelID++ )
            {
              recordInitEntry(&records[recID]);
              records[recID].varID   = (short)varID;
              records[recID].levelID = (short)levelID;
              recID++;
            }
        }
    }
  else if ( tsID == 1 )
    {
      int nvrecs = 0;
      for ( int varID = 0; varID < nvars; varID++ )
        {
          if ( vlistInqVarTsteptype(vlistID, varID) != TSTEP_CONSTANT )
            {
              int zaxisID = vlistInqVarZaxis(vlistID, varID);
              nvrecs += zaxisInqSize(zaxisID);
            }
        }

      streamptr->nrecs += nvrecs;

      destTstep->records    = (record_t *) Malloc((size_t)nrecs*sizeof(record_t));
      destTstep->nrecs      = nvrecs;
      destTstep->nallrecs   = nrecs;
      destTstep->recordSize = nrecs;
      destTstep->curRecID   = UNDEFID;

      memcpy(destTstep->records, sourceTstep->records, (size_t)nrecs*sizeof(record_t));

      if ( nvrecs )
        {
          destTstep->recIDs = (int *) Malloc((size_t)nvrecs * sizeof (int));
          for ( int recID = 0, vrecID = 0; recID < nrecs; recID++ )
            {
              int varID = destTstep->records[recID].varID;
              if ( vlistInqVarTsteptype(vlistID, varID) != TSTEP_CONSTANT )
                {
                  destTstep->recIDs[vrecID++] = recID;
                }
            }
        }
    }
  else
    {
      if ( streamptr->tsteps[1].records == 0 ) cdfCreateRecords(streamptr, 1);

      int nvrecs = streamptr->tsteps[1].nrecs;

      streamptr->nrecs += nvrecs;

      destTstep->records    = (record_t *) Malloc((size_t)nrecs*sizeof(record_t));
      destTstep->nrecs      = nvrecs;
      destTstep->nallrecs   = nrecs;
      destTstep->recordSize = nrecs;
      destTstep->curRecID   = UNDEFID;

      memcpy(destTstep->records, sourceTstep->records, (size_t)nrecs*sizeof(record_t));

      destTstep->recIDs     = (int *) Malloc((size_t)nvrecs * sizeof(int));

      memcpy(destTstep->recIDs, streamptr->tsteps[1].recIDs, (size_t)nvrecs*sizeof(int));
    }
}


static
int cdfTimeDimID(int fileID, int ndims, int nvars)
{
  for ( int dimid = 0; dimid < ndims; dimid++ )
    {
      char dimname[80];
      cdf_inq_dimname(fileID, dimid, dimname);
      if ( memcmp(dimname, "time", 4) == 0 )
        return dimid;
    }


  for ( int varid = 0; varid < nvars; varid++ )
    {
      int nvdims, nvatts, dimids[9];
      cdf_inq_var(fileID, varid, NULL, NULL, &nvdims, dimids, &nvatts);
      if ( nvdims == 1 )
        {
          for ( int iatt = 0; iatt < nvatts; iatt++ )
            {
              char sbuf[CDI_MAX_NAME];
              cdf_inq_attname(fileID, varid, iatt, sbuf);
              if ( strncmp(sbuf, "units", 5) == 0 )
                {
                  cdfGetAttText(fileID, varid, "units", sizeof(sbuf), sbuf);
                  strtolower(sbuf);

                  if ( isTimeUnits(sbuf) )
                    return dimids[0];
                }
            }
        }
    }

  return UNDEFID;
}

static
void init_ncdims(long ndims, ncdim_t *ncdims)
{
  for ( long ncdimid = 0; ncdimid < ndims; ncdimid++ )
    {
      ncdims[ncdimid].ncvarid      = UNDEFID;
      ncdims[ncdimid].dimtype      = UNDEFID;
      ncdims[ncdimid].len          = 0;
      ncdims[ncdimid].name[0]      = 0;
    }
}

static
void init_ncvars(long nvars, ncvar_t *ncvars)
{
  for ( long ncvarid = 0; ncvarid < nvars; ++ncvarid )
    {
      ncvars[ncvarid].ncid            = UNDEFID;
      ncvars[ncvarid].ignore          = FALSE;
      ncvars[ncvarid].isvar           = UNDEFID;
      ncvars[ncvarid].islon           = FALSE;
      ncvars[ncvarid].islat           = FALSE;
      ncvars[ncvarid].islev           = FALSE;
      ncvars[ncvarid].istime          = FALSE;
      ncvars[ncvarid].warn            = FALSE;
      ncvars[ncvarid].tsteptype       = TSTEP_CONSTANT;
      ncvars[ncvarid].param           = UNDEFID;
      ncvars[ncvarid].code            = UNDEFID;
      ncvars[ncvarid].tabnum          = 0;
      ncvars[ncvarid].calendar        = FALSE;
      ncvars[ncvarid].climatology     = FALSE;
      ncvars[ncvarid].bounds          = UNDEFID;
      ncvars[ncvarid].lformula        = FALSE;
      ncvars[ncvarid].lformulaterms   = FALSE;
      ncvars[ncvarid].gridID          = UNDEFID;
      ncvars[ncvarid].zaxisID         = UNDEFID;
      ncvars[ncvarid].gridtype        = UNDEFID;
      ncvars[ncvarid].zaxistype       = UNDEFID;
      ncvars[ncvarid].xdim            = UNDEFID;
      ncvars[ncvarid].ydim            = UNDEFID;
      ncvars[ncvarid].zdim            = UNDEFID;
      ncvars[ncvarid].xvarid          = UNDEFID;
      ncvars[ncvarid].yvarid          = UNDEFID;
      ncvars[ncvarid].zvarid          = UNDEFID;
      ncvars[ncvarid].tvarid          = UNDEFID;
      ncvars[ncvarid].psvarid         = UNDEFID;
      ncvars[ncvarid].p0varid         = UNDEFID;
      ncvars[ncvarid].ncoordvars      = 0;
      for ( int i = 0; i < MAX_COORDVARS; ++i )
        ncvars[ncvarid].coordvarids[i]  = UNDEFID;
      ncvars[ncvarid].nauxvars      = 0;
      for ( int i = 0; i < MAX_AUXVARS; ++i )
        ncvars[ncvarid].auxvarids[i]  = UNDEFID;
      ncvars[ncvarid].cellarea        = UNDEFID;
      ncvars[ncvarid].tableID         = UNDEFID;
      ncvars[ncvarid].xtype           = 0;
      ncvars[ncvarid].ndims           = 0;
      ncvars[ncvarid].gmapid          = UNDEFID;
      ncvars[ncvarid].vctsize         = 0;
      ncvars[ncvarid].vct             = NULL;
      ncvars[ncvarid].truncation      = 0;
      ncvars[ncvarid].position        = 0;
      ncvars[ncvarid].positive        = 0;
      ncvars[ncvarid].chunked         = 0;
      ncvars[ncvarid].chunktype       = UNDEFID;
      ncvars[ncvarid].defmissval      = 0;
      ncvars[ncvarid].deffillval      = 0;
      ncvars[ncvarid].missval         = 0;
      ncvars[ncvarid].fillval         = 0;
      ncvars[ncvarid].addoffset       = 0;
      ncvars[ncvarid].scalefactor     = 1;
      ncvars[ncvarid].name[0]         = 0;
      ncvars[ncvarid].longname[0]     = 0;
      ncvars[ncvarid].stdname[0]      = 0;
      ncvars[ncvarid].units[0]        = 0;
      ncvars[ncvarid].extra[0]        = 0;
      ncvars[ncvarid].natts           = 0;
      ncvars[ncvarid].atts            = NULL;
      ncvars[ncvarid].deflate         = 0;
      ncvars[ncvarid].lunsigned       = 0;
      ncvars[ncvarid].lvalidrange     = 0;
      ncvars[ncvarid].validrange[0]   = VALIDMISS;
      ncvars[ncvarid].validrange[1]   = VALIDMISS;
      ncvars[ncvarid].ensdata         = NULL;
    }
}

static
void cdfSetVar(ncvar_t *ncvars, int ncvarid, short isvar)
{
  if ( ncvars[ncvarid].isvar != UNDEFID &&
       ncvars[ncvarid].isvar != isvar   &&
       ncvars[ncvarid].warn  == FALSE )
    {
      if ( ! ncvars[ncvarid].ignore )
        Warning("Inconsistent variable definition for %s!", ncvars[ncvarid].name);

      ncvars[ncvarid].warn = TRUE;
      isvar = FALSE;
    }

  ncvars[ncvarid].isvar = isvar;
}

static
void cdfSetDim(ncvar_t *ncvars, int ncvarid, int dimid, int dimtype)
{
  if ( ncvars[ncvarid].dimtype[dimid] != UNDEFID &&
       ncvars[ncvarid].dimtype[dimid] != dimtype )
    {
      Warning("Inconsistent dimension definition for %s! dimid = %d;  type = %d;  newtype = %d",
              ncvars[ncvarid].name, dimid, ncvars[ncvarid].dimtype[dimid], dimtype);
    }

  ncvars[ncvarid].dimtype[dimid] = dimtype;
}

static
bool isLonAxis(const char *units, const char *stdname)
{
  bool status = false;
  char lc_units[16];

  memcpy(lc_units, units, 15);
  lc_units[15] = 0;
  strtolower(lc_units);

  if ( ((memcmp(lc_units, "degree", 6) == 0 || memcmp(lc_units, "radian", 6) == 0) &&
        (memcmp(stdname, "grid_longitude", 14) == 0 || memcmp(stdname, "longitude", 9) == 0)) )
    {
      status = true;
    }

  if ( status == false &&
       memcmp(stdname, "grid_latitude", 13) && memcmp(stdname, "latitude", 8) &&
       memcmp(lc_units, "degree", 6) == 0 )
    {
      int ioff = 6;
      if ( lc_units[ioff] == 's' ) ioff++;
      if ( lc_units[ioff] == '_' ) ioff++;
      if ( lc_units[ioff] == 'e' ) status = true;
    }

  return status;
}

static
bool isLatAxis(const char *units, const char *stdname)
{
  bool status = false;
  char lc_units[16];

  memcpy(lc_units, units, 15);
  lc_units[15] = 0;
  strtolower(lc_units);

  if ( ((memcmp(lc_units, "degree", 6) == 0 || memcmp(lc_units, "radian", 6) == 0) &&
        (memcmp(stdname, "grid_latitude", 13) == 0 || memcmp(stdname, "latitude", 8) == 0)) )
    {
      status = true;
    }

  if ( status == false &&
       memcmp(stdname, "grid_longitude", 14) && memcmp(stdname, "longitude", 9) &&
       memcmp(lc_units, "degree", 6) == 0 )
    {
      int ioff = 6;
      if ( lc_units[ioff] == 's' ) ioff++;
      if ( lc_units[ioff] == '_' ) ioff++;
      if ( lc_units[ioff] == 'n' || lc_units[ioff] == 's' ) status = true;
    }

  return status;
}

static
bool isDBLAxis(/*const char *units,*/ const char *longname)
{
  bool status = false;

  if ( strcmp(longname, "depth below land")         == 0 ||
       strcmp(longname, "depth_below_land")         == 0 ||
       strcmp(longname, "levels below the surface") == 0 )
    {
      /*
      if ( strcmp(ncvars[ncvarid].units, "cm") == 0 ||
           strcmp(ncvars[ncvarid].units, "dm") == 0 ||
           strcmp(ncvars[ncvarid].units, "m")  == 0 )
      */
        status = true;
    }

  return status;
}

static
bool unitsIsHeight(const char *units)
{
  bool status = false;
  int u0 = units[0];

  if ( (u0=='m' && (!units[1] || strncmp(units, "meter", 5) == 0)) ||
       (!units[2] && units[1]=='m' && (u0=='c' || u0=='d' || u0=='k')) )
    {
      status = true;
    }

  return status;
}

static
bool isDepthAxis(const char *stdname, const char *longname)
{
  bool status = false;

  if ( strcmp(stdname, "depth") == 0 )
    status = true;
  else
    if ( strcmp(longname, "depth_below_sea") == 0 ||
         strcmp(longname, "depth below sea") == 0 )
      {
        status = true;
      }

  return status;
}

static
bool isHeightAxis(const char *stdname, const char *longname)
{
  bool status = false;

  if ( strcmp(stdname, "height") == 0 )
    status = true;
  else
    if ( strcmp(longname, "height") == 0 ||
         strcmp(longname, "height above the surface") == 0 )
      {
        status = true;
      }

  return status;
}

static
bool unitsIsPressure(const char *units)
{
  bool status = false;

  if ( strncmp(units, "millibar", 8) == 0 ||
       strncmp(units, "mb", 2)       == 0 ||
       strncmp(units, "hectopas", 8) == 0 ||
       strncmp(units, "hPa", 3)      == 0 ||
       strncmp(units, "Pa", 2)       == 0 )
    {
      status = true;
    }

  return status;
}

static
int scan_hybrid_formula(int ncid, int ncfvarid, int *apvarid, int *bvarid, int *psvarid, int *avarid, int *p0varid)
{
  int status = 0;
  *apvarid = -1;
  *bvarid  = -1;
  *psvarid = -1;
  *avarid  = -1;
  *p0varid = -1;
  enum { attstringlen = 8192 }; char attstring[attstringlen];
  cdfGetAttText(ncid, ncfvarid, "formula", attstringlen, attstring);
  if ( strcmp(attstring, "p = ap + b*ps") == 0 )
    {
      status = 1;
      int lstop = FALSE;
      int dimvarid;
      cdfGetAttText(ncid, ncfvarid, "formula_terms", attstringlen, attstring);
      char *pstring = attstring;

      for ( int i = 0; i < 3; i++ )
        {
          while ( isspace((int) *pstring) ) pstring++;
          if ( *pstring == 0 ) break;
          char *tagname = pstring;
          while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
          if ( *pstring == 0 ) lstop = TRUE;
          *pstring++ = 0;

          while ( isspace((int) *pstring) ) pstring++;
          if ( *pstring == 0 ) break;
          char *varname = pstring;
          while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
          if ( *pstring == 0 ) lstop = TRUE;
          *pstring++ = 0;

          int status_nc = nc_inq_varid(ncid, varname, &dimvarid);
          if ( status_nc == NC_NOERR )
            {
              if      ( strcmp(tagname, "ap:") == 0 ) *apvarid = dimvarid;
              else if ( strcmp(tagname, "b:")  == 0 ) *bvarid  = dimvarid;
              else if ( strcmp(tagname, "ps:") == 0 ) *psvarid = dimvarid;
            }
          else if ( strcmp(tagname, "ps:") != 0 )
            {
              Warning("%s - %s", nc_strerror(status_nc), varname);
            }

          if ( lstop ) break;
        }
    }
  else if ( strcmp(attstring, "xxxp = a*p0 + b*ps") == 0 )
    {
      status = 2;
      int lstop = FALSE;
      int dimvarid;
      cdfGetAttText(ncid, ncfvarid, "formula_terms", attstringlen, attstring);
      char *pstring = attstring;

      for ( int i = 0; i < 4; i++ )
        {
          while ( isspace((int) *pstring) ) pstring++;
          if ( *pstring == 0 ) break;
          char *tagname = pstring;
          while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
          if ( *pstring == 0 ) lstop = TRUE;
          *pstring++ = 0;

          while ( isspace((int) *pstring) ) pstring++;
          if ( *pstring == 0 ) break;
          char *varname = pstring;
          while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
          if ( *pstring == 0 ) lstop = TRUE;
          *pstring++ = 0;

          int status_nc = nc_inq_varid(ncid, varname, &dimvarid);
          if ( status_nc == NC_NOERR )
            {
              if      ( strcmp(tagname, "a:")  == 0 ) *avarid  = dimvarid;
              else if ( strcmp(tagname, "b:")  == 0 ) *bvarid  = dimvarid;
              else if ( strcmp(tagname, "ps:") == 0 ) *psvarid = dimvarid;
              else if ( strcmp(tagname, "p0:") == 0 ) *p0varid = dimvarid;
            }
          else if ( strcmp(tagname, "ps:") != 0 )
            {
              Warning("%s - %s", nc_strerror(status_nc), varname);
            }

          if ( lstop ) break;
        }
    }

  return status;
}

static
bool isHybridSigmaPressureCoordinate(int ncid, int ncvarid, ncvar_t *ncvars, const ncdim_t *ncdims)
{
  bool status = false;
  int ncfvarid = ncvarid;
  ncvar_t *ncvar = &ncvars[ncvarid];

  if ( strcmp(ncvar->stdname, "atmosphere_hybrid_sigma_pressure_coordinate") == 0 )
    {
      cdiConvention = CDI_CONVENTION_CF;

      status = true;
      ncvar->zaxistype = ZAXIS_HYBRID;
      int dimid = ncvar->dimids[0];
      size_t dimlen = ncdims[dimid].len;

      int ret;
      int apvarid1 = -1, bvarid1 = -1, psvarid1 = -1, avarid1 = -1, p0varid1 = -1;
      if ( ncvars[ncfvarid].lformula && ncvars[ncfvarid].lformulaterms )
        ret = scan_hybrid_formula(ncid, ncfvarid, &apvarid1, &bvarid1, &psvarid1, &avarid1, &p0varid1);
      if ( apvarid1 != -1 ) ncvars[apvarid1].isvar = FALSE;
      if ( bvarid1  != -1 ) ncvars[bvarid1].isvar  = FALSE;
      if ( psvarid1 != -1 ) ncvar->psvarid = psvarid1;
      if ( avarid1  != -1 ) ncvars[avarid1].isvar = FALSE;
      if ( p0varid1 != -1 ) ncvar->p0varid = p0varid1;

      if ( ncvar->bounds != UNDEFID && ncvars[ncvar->bounds].lformula && ncvars[ncvar->bounds].lformulaterms )
        {
          ncfvarid = ncvar->bounds;
          int apvarid2 = -1, bvarid2 = -1, psvarid2 = -1, avarid2 = -1, p0varid2 = -1;
          ret = 0;
          if ( ncvars[ncfvarid].lformula && ncvars[ncfvarid].lformulaterms )
            ret = scan_hybrid_formula(ncid, ncfvarid, &apvarid2, &bvarid2, &psvarid2, &avarid2, &p0varid2);
          if ( ret == 1 ) avarid2 = apvarid2;
          if ( avarid2 != -1 && bvarid2 != -1 )
            {
              ncvars[avarid2].isvar = FALSE;
              ncvars[bvarid2].isvar  = FALSE;

              if ( dimid == ncvars[avarid2].dimids[0] && ncdims[ncvars[avarid2].dimids[1]].len == 2 )
                {
                  double px = 1;
                  if ( ret == 2 && p0varid1 == p0varid2 )
                    cdf_get_var_double(ncid, p0varid2, &px);

                  double abuf[dimlen*2], bbuf[dimlen*2];
                  cdf_get_var_double(ncid, avarid2, abuf);
                  cdf_get_var_double(ncid, bvarid2, bbuf);
                  /*
                  for ( int i = 0; i < dimlen; ++i )
                    printf("%d  %g %g    %g %g\n", i, abuf[i*2], abuf[i*2+1], bbuf[i*2], bbuf[i*2+1]);
                  */
                  size_t vctsize = (dimlen+1)*2;
                  double *vct = (double *) Malloc(vctsize * sizeof(double));
                  for ( size_t i = 0; i < dimlen; ++i )
                    {
                      vct[i] = abuf[i*2];
                      vct[i+dimlen+1] = bbuf[i*2];
                    }
                  vct[dimlen]     = abuf[dimlen*2-1];
                  vct[dimlen*2+1] = bbuf[dimlen*2-1];

                  if ( ret == 2 && IS_NOT_EQUAL(px, 1) )
                    for ( size_t i = 0; i < dimlen+1; ++i ) vct[i] *= px;

                  ncvar->vct = vct;
                  ncvar->vctsize = vctsize;
                }
            }
        }
    }

  return status;
}


static
int isGaussGrid(size_t ysize, double yinc, const double *yvals)
{
  int lgauss = FALSE;
  double *yv, *yw;

  if ( IS_EQUAL(yinc, 0) && ysize > 2 ) /* check if gaussian */
    {
      size_t i;
      yv = (double *) Malloc(ysize*sizeof(double));
      yw = (double *) Malloc(ysize*sizeof(double));
      gaussaw(yv, yw, ysize);
      Free(yw);
      for ( i = 0; i < ysize; i++ )
        yv[i] = asin(yv[i])/M_PI*180.0;

      for ( i = 0; i < ysize; i++ )
        if ( fabs(yv[i] - yvals[i]) >
             ((yv[0] - yv[1])/500) ) break;

      if ( i == ysize ) lgauss = TRUE;

      /* check S->N */
      if ( lgauss == FALSE )
        {
          for ( i = 0; i < ysize; i++ )
            if ( fabs(yv[i] - yvals[ysize-i-1]) >
                 ((yv[0] - yv[1])/500) ) break;

          if ( i == ysize ) lgauss = TRUE;
        }

      Free(yv);
    }

  return (lgauss);
}

static
void printNCvars(const ncvar_t *ncvars, int nvars, const char *oname)
{
  char axis[7];
  int ncvarid, i;
  int ndim;
  static const char iaxis[] = {'t', 'z', 'y', 'x'};

  fprintf(stderr, "%s:\n", oname);

  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      ndim = 0;
      if ( ncvars[ncvarid].isvar )
        {
          axis[ndim++] = 'v';
          axis[ndim++] = ':';
          for ( i = 0; i < ncvars[ncvarid].ndims; i++ )
            {/*
              if      ( ncvars[ncvarid].tvarid != -1 ) axis[ndim++] = iaxis[0];
              else if ( ncvars[ncvarid].zvarid != -1 ) axis[ndim++] = iaxis[1];
              else if ( ncvars[ncvarid].yvarid != -1 ) axis[ndim++] = iaxis[2];
              else if ( ncvars[ncvarid].xvarid != -1 ) axis[ndim++] = iaxis[3];
              else
             */
              if      ( ncvars[ncvarid].dimtype[i] == T_AXIS ) axis[ndim++] = iaxis[0];
              else if ( ncvars[ncvarid].dimtype[i] == Z_AXIS ) axis[ndim++] = iaxis[1];
              else if ( ncvars[ncvarid].dimtype[i] == Y_AXIS ) axis[ndim++] = iaxis[2];
              else if ( ncvars[ncvarid].dimtype[i] == X_AXIS ) axis[ndim++] = iaxis[3];
              else                                             axis[ndim++] = '?';
            }
        }
      else
        {
          axis[ndim++] = 'c';
          axis[ndim++] = ':';
          if      ( ncvars[ncvarid].istime ) axis[ndim++] = iaxis[0];
          else if ( ncvars[ncvarid].islev  ) axis[ndim++] = iaxis[1];
          else if ( ncvars[ncvarid].islat  ) axis[ndim++] = iaxis[2];
          else if ( ncvars[ncvarid].islon  ) axis[ndim++] = iaxis[3];
          else                               axis[ndim++] = '?';
        }

      axis[ndim++] = 0;

      fprintf(stderr, "%3d %3d  %-6s %s\n", ncvarid, ndim-3, axis, ncvars[ncvarid].name);
    }
}

static
void cdfScanVarAttributes(int nvars, ncvar_t *ncvars, ncdim_t *ncdims,
                          int timedimid, int modelID, int format)
{
  int ncid;
  int ncdimid;
  int nvdims, nvatts;
  int *dimidsp;
  int iatt;
  nc_type xtype, atttype;
  size_t attlen;
  char name[CDI_MAX_NAME];
  char attname[CDI_MAX_NAME];
  const int attstringlen = 8192; char attstring[8192];

  int nchecked_vars = 0;
  enum { max_check_vars = 9 };
  char *checked_vars[max_check_vars];
  for ( int i = 0; i < max_check_vars; ++i ) checked_vars[i] = NULL;

  for ( int ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      ncid    = ncvars[ncvarid].ncid;
      dimidsp = ncvars[ncvarid].dimids;

      cdf_inq_var(ncid, ncvarid, name, &xtype, &nvdims, dimidsp, &nvatts);
      strcpy(ncvars[ncvarid].name, name);

      for ( ncdimid = 0; ncdimid < nvdims; ncdimid++ )
        ncvars[ncvarid].dimtype[ncdimid] = -1;

      ncvars[ncvarid].xtype = xtype;
      ncvars[ncvarid].ndims = nvdims;

#if  defined  (HAVE_NETCDF4)
      if ( format == NC_FORMAT_NETCDF4_CLASSIC || format == NC_FORMAT_NETCDF4 )
        {
          int shuffle, deflate, deflate_level;
          size_t chunks[nvdims];
          int storage_in;
          nc_inq_var_deflate(ncid, ncvarid, &shuffle, &deflate, &deflate_level);
          if ( deflate > 0 ) ncvars[ncvarid].deflate = 1;

          if ( nc_inq_var_chunking(ncid, ncvarid, &storage_in, chunks) == NC_NOERR )
            {
              if ( storage_in == NC_CHUNKED )
                {
                  ncvars[ncvarid].chunked = 1;
                  for ( int i = 0; i < nvdims; ++i ) ncvars[ncvarid].chunks[i] = (int)chunks[i];
                  if ( CDI_Debug )
                    {
                      fprintf(stderr, "%s: chunking %d %d %d  chunks ", name, storage_in, NC_CONTIGUOUS, NC_CHUNKED);
                      for ( int i = 0; i < nvdims; ++i ) fprintf(stderr, "%ld ", chunks[i]);
                      fprintf(stderr, "\n");
                    }
                  {
                    char *buf = ncvars[ncvarid].extra;
                    size_t pos = strlen(buf);
                    static const char prefix[] = "chunks=";
                    memcpy(buf + pos, prefix, sizeof (prefix));
                    pos += sizeof (prefix) - 1;
                    for ( int i = nvdims-1; i >= 0; --i )
                      {
                        pos += (size_t)(sprintf(buf + pos, "%zu%s", chunks[i],
                                                i > 0 ? "x" : ""));
                      }
                    buf[pos] = ' '; buf[pos + 1] = 0;
                  }
                }
            }
        }
#endif

      if ( nvdims > 0 )
        {
          if ( timedimid == dimidsp[0] )
            {
              ncvars[ncvarid].tsteptype = TSTEP_INSTANT;
              cdfSetDim(ncvars, ncvarid, 0, T_AXIS);
            }
          else
            {
              for ( ncdimid = 1; ncdimid < nvdims; ncdimid++ )
                {
                  if ( timedimid == dimidsp[ncdimid] )
                    {
                      Warning("Time must be the first dimension! Unsupported array structure, skipped variable %s!", ncvars[ncvarid].name);
                      ncvars[ncvarid].isvar = FALSE;
                    }
                }
            }
        }

      for ( iatt = 0; iatt < nvatts; iatt++ )
        {
          cdf_inq_attname(ncid, ncvarid, iatt, attname);
          cdf_inq_atttype(ncid, ncvarid, attname, &atttype);
          cdf_inq_attlen(ncid, ncvarid, attname, &attlen);

          if ( strcmp(attname, "long_name") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, CDI_MAX_NAME, ncvars[ncvarid].longname);
            }
          else if ( strcmp(attname, "standard_name") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, CDI_MAX_NAME, ncvars[ncvarid].stdname);
            }
          else if ( strcmp(attname, "units") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, CDI_MAX_NAME, ncvars[ncvarid].units);
            }
          else if ( strcmp(attname, "calendar") == 0 )
            {
              ncvars[ncvarid].calendar = TRUE;
            }
          else if ( strcmp(attname, "param") == 0 && xtypeIsText(atttype) )
            {
	      char paramstr[32];
	      int pnum = 0, pcat = 255, pdis = 255;
              cdfGetAttText(ncid, ncvarid, attname, sizeof(paramstr), paramstr);
	      sscanf(paramstr, "%d.%d.%d", &pnum, &pcat, &pdis);
	      ncvars[ncvarid].param = cdiEncodeParam(pnum, pcat, pdis);
              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "code") == 0 && !xtypeIsText(atttype) )
            {
              cdfGetAttInt(ncid, ncvarid, attname, 1, &ncvars[ncvarid].code);
              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "table") == 0 && !xtypeIsText(atttype) )
            {
              int tablenum;
              cdfGetAttInt(ncid, ncvarid, attname, 1, &tablenum);
              if ( tablenum > 0 )
                {
                  ncvars[ncvarid].tabnum = tablenum;
                  ncvars[ncvarid].tableID = tableInq(modelID, tablenum, NULL);
                  if ( ncvars[ncvarid].tableID == CDI_UNDEFID )
                    ncvars[ncvarid].tableID = tableDef(modelID, tablenum, NULL);
                }
              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "trunc_type") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              if ( memcmp(attstring, "Triangular", attlen) == 0 )
                ncvars[ncvarid].gridtype = GRID_SPECTRAL;
            }
          else if ( strcmp(attname, "grid_type") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              strtolower(attstring);

              if      ( strcmp(attstring, "gaussian reduced") == 0 )
                ncvars[ncvarid].gridtype = GRID_GAUSSIAN_REDUCED;
              else if ( strcmp(attstring, "gaussian") == 0 )
                ncvars[ncvarid].gridtype = GRID_GAUSSIAN;
              else if ( strncmp(attstring, "spectral", 8) == 0 )
                ncvars[ncvarid].gridtype = GRID_SPECTRAL;
              else if ( strncmp(attstring, "fourier", 7) == 0 )
                ncvars[ncvarid].gridtype = GRID_FOURIER;
              else if ( strcmp(attstring, "trajectory") == 0 )
                ncvars[ncvarid].gridtype = GRID_TRAJECTORY;
              else if ( strcmp(attstring, "generic") == 0 )
                ncvars[ncvarid].gridtype = GRID_GENERIC;
              else if ( strcmp(attstring, "cell") == 0 )
                ncvars[ncvarid].gridtype = GRID_UNSTRUCTURED;
              else if ( strcmp(attstring, "unstructured") == 0 )
                ncvars[ncvarid].gridtype = GRID_UNSTRUCTURED;
              else if ( strcmp(attstring, "curvilinear") == 0 )
                ncvars[ncvarid].gridtype = GRID_CURVILINEAR;
              else if ( strcmp(attstring, "sinusoidal") == 0 )
                ;
              else if ( strcmp(attstring, "laea") == 0 )
                ;
              else if ( strcmp(attstring, "lcc2") == 0 )
                ;
              else if ( strcmp(attstring, "linear") == 0 ) // ignore grid type linear
                ;
              else
                {
                  static int warn = TRUE;
                  if ( warn )
                    {
                      warn = FALSE;
                      Warning("NetCDF attribute grid_type='%s' unsupported!", attstring);
                    }
                }

              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "level_type") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              strtolower(attstring);

              if      ( strcmp(attstring, "toa") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_TOA;
              else if ( strcmp(attstring, "cloudbase") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_CLOUD_BASE;
              else if ( strcmp(attstring, "cloudtop") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_CLOUD_TOP;
              else if ( strcmp(attstring, "isotherm0") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_ISOTHERM_ZERO;
              else if ( strcmp(attstring, "seabottom") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_SEA_BOTTOM;
              else if ( strcmp(attstring, "lakebottom") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_LAKE_BOTTOM;
              else if ( strcmp(attstring, "sedimentbottom") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_SEDIMENT_BOTTOM;
              else if ( strcmp(attstring, "sedimentbottomta") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_SEDIMENT_BOTTOM_TA;
              else if ( strcmp(attstring, "sedimentbottomtw") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_SEDIMENT_BOTTOM_TW;
              else if ( strcmp(attstring, "mixlayer") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_MIX_LAYER;
              else if ( strcmp(attstring, "atmosphere") == 0 )
                ncvars[ncvarid].zaxistype = ZAXIS_ATMOSPHERE;
              else
                {
                  static int warn = TRUE;
                  if ( warn )
                    {
                      warn = FALSE;
                      Warning("NetCDF attribute level_type='%s' unsupported!", attstring);
                    }
                }

              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "trunc_count") == 0 && !xtypeIsText(atttype) )
            {
              cdfGetAttInt(ncid, ncvarid, attname, 1, &ncvars[ncvarid].truncation);
            }
          else if ( strcmp(attname, "truncation") == 0 && !xtypeIsText(atttype) )
            {
              cdfGetAttInt(ncid, ncvarid, attname, 1, &ncvars[ncvarid].truncation);
            }
          else if ( strcmp(attname, "number_of_grid_in_reference") == 0 && !xtypeIsText(atttype) )
            {
              cdfGetAttInt(ncid, ncvarid, attname, 1, &ncvars[ncvarid].position);
            }
          else if ( strcmp(attname, "add_offset") == 0 && !xtypeIsText(atttype) )
            {
	      cdfGetAttDouble(ncid, ncvarid, attname, 1, &ncvars[ncvarid].addoffset);
	      /*
		if ( atttype != NC_BYTE && atttype != NC_SHORT && atttype != NC_INT )
		if ( ncvars[ncvarid].addoffset != 0 )
		Warning("attribute add_offset not supported for atttype %d", atttype);
	      */
	      /* (also used for lon/lat) cdfSetVar(ncvars, ncvarid, TRUE); */
            }
          else if ( strcmp(attname, "scale_factor") == 0 && !xtypeIsText(atttype) )
            {
	      cdfGetAttDouble(ncid, ncvarid, attname, 1, &ncvars[ncvarid].scalefactor);
	      /*
		if ( atttype != NC_BYTE && atttype != NC_SHORT && atttype != NC_INT )
		if ( ncvars[ncvarid].scalefactor != 1 )
		Warning("attribute scale_factor not supported for atttype %d", atttype);
	      */
	      /* (also used for lon/lat) cdfSetVar(ncvars, ncvarid, TRUE); */
            }
          else if ( strcmp(attname, "climatology") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              int ncboundsid;
              int status = nc_inq_varid(ncid, attstring, &ncboundsid);
              if ( status == NC_NOERR )
                {
                  ncvars[ncvarid].climatology = TRUE;
                  ncvars[ncvarid].bounds = ncboundsid;
                  cdfSetVar(ncvars, ncvars[ncvarid].bounds, FALSE);
                  cdfSetVar(ncvars, ncvarid, FALSE);
                }
              else
                Warning("%s - %s", nc_strerror(status), attstring);
            }
          else if ( xtypeIsText(atttype) && strcmp(attname, "bounds") == 0 )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              int ncboundsid;
              int status = nc_inq_varid(ncid, attstring, &ncboundsid);
              if ( status == NC_NOERR )
                {
                  ncvars[ncvarid].bounds = ncboundsid;
                  cdfSetVar(ncvars, ncvars[ncvarid].bounds, FALSE);
                  cdfSetVar(ncvars, ncvarid, FALSE);
                }
              else
                Warning("%s - %s", nc_strerror(status), attstring);
            }
          else if ( xtypeIsText(atttype) && strcmp(attname, "formula_terms") == 0 )
            {
              ncvars[ncvarid].lformulaterms = TRUE;
            }
          else if ( xtypeIsText(atttype) && strcmp(attname, "formula") == 0 )
            {
              ncvars[ncvarid].lformula = TRUE;
            }
          else if ( strcmp(attname, "cell_measures") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              char *pstring = attstring;

              while ( isspace((int) *pstring) ) pstring++;
              char *cell_measures = pstring;
              while ( isalnum((int) *pstring) ) pstring++;
              *pstring++ = 0;
              while ( isspace((int) *pstring) ) pstring++;
              char *cell_var = pstring;
              while ( ! isspace((int) *pstring) && *pstring != 0 ) pstring++;
              *pstring++ = 0;
              /*
              printf("cell_measures >%s<\n", cell_measures);
              printf("cell_var >%s<\n", cell_var);
              */
              if ( memcmp(cell_measures, "area", 4) == 0 )
                {
                  int nc_cell_id;
                  int status = nc_inq_varid(ncid, cell_var, &nc_cell_id);
                  if ( status == NC_NOERR )
                    {
                      ncvars[ncvarid].cellarea = nc_cell_id;
                      /* ncvars[nc_cell_id].isvar = UNDEFID; */
                      cdfSetVar(ncvars, nc_cell_id, FALSE);
                    }
                  else
                    Warning("%s - %s", nc_strerror(status), cell_var);
                }
              else
                {
                  Warning("%s has an unexpected contents: %s", attname, cell_measures);
                }
              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          /*
          else if ( strcmp(attname, "coordinates") == 0 )
            {
              char *pstring, *xvarname = NULL, *yvarname = NULL;

              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              pstring = attstring;

              while ( isspace((int) *pstring) ) pstring++;
              xvarname = pstring;
              while ( isgraph((int) *pstring) ) pstring++;
              *pstring++ = 0;
              while ( isspace((int) *pstring) ) pstring++;
              yvarname = pstring;
              while ( isgraph((int) *pstring) ) pstring++;
              *pstring++ = 0;

              cdf_inq_varid(ncid, xvarname, &ncvars[ncvarid].xvarid);
              cdf_inq_varid(ncid, yvarname, &ncvars[ncvarid].yvarid);

              cdfSetVar(ncvars, ncvars[ncvarid].xvarid, FALSE);
              cdfSetVar(ncvars, ncvars[ncvarid].yvarid, FALSE);
              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          */
          else if ( (strcmp(attname, "associate")  == 0 || strcmp(attname, "coordinates") == 0) && xtypeIsText(atttype) )
            {
              int lstop = FALSE;
              int dimvarid;

              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              char *pstring = attstring;

              for ( int i = 0; i < MAX_COORDVARS; i++ )
                {
                  while ( isspace((int) *pstring) ) pstring++;
                  if ( *pstring == 0 ) break;
                  char *varname = pstring;
                  while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
                  if ( *pstring == 0 ) lstop = TRUE;
                  *pstring++ = 0;

                  int status = nc_inq_varid(ncid, varname, &dimvarid);
                  if ( status == NC_NOERR )
                    {
                      cdfSetVar(ncvars, dimvarid, FALSE);
                      if ( cdiIgnoreAttCoordinates == FALSE )
                        {
                          ncvars[ncvarid].coordvarids[i] = dimvarid;
                          ncvars[ncvarid].ncoordvars++;
                        }
                    }
                  else
                    {
                      int k;
                      for ( k = 0; k < nchecked_vars; ++k )
                        if ( strcmp(checked_vars[k], varname) == 0 ) break;

                      if ( k == nchecked_vars )
                        {
                          if ( nchecked_vars < max_check_vars ) checked_vars[nchecked_vars++] = strdup(varname);
                          Warning("%s - %s", nc_strerror(status), varname);
                        }
                    }

                  if ( lstop ) break;
                }

              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( (strcmp(attname, "auxiliary_variable") == 0) && xtypeIsText(atttype) )
            {
              int lstop = FALSE;
              int dimvarid;

              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              char *pstring = attstring;

              for ( int i = 0; i < MAX_AUXVARS; i++ )
                {
                  while ( isspace((int) *pstring) ) pstring++;
                  if ( *pstring == 0 ) break;
                  char *varname = pstring;
                  while ( !isspace((int) *pstring) && *pstring != 0 ) pstring++;
                  if ( *pstring == 0 ) lstop = TRUE;
                  *pstring++ = 0;

                  int status = nc_inq_varid(ncid, varname, &dimvarid);
                  if ( status == NC_NOERR )
                    {
                      cdfSetVar(ncvars, dimvarid, FALSE);
                      //  if ( cdiIgnoreAttCoordinates == FALSE )
                        {
                          ncvars[ncvarid].auxvarids[i] = dimvarid;
                          ncvars[ncvarid].nauxvars++;
                        }
                    }
                  else
                    Warning("%s - %s", nc_strerror(status), varname);

                  if ( lstop ) break;
                }

              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "grid_mapping") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              int nc_gmap_id;
              int status = nc_inq_varid(ncid, attstring, &nc_gmap_id);
              if ( status == NC_NOERR )
                {
                  ncvars[ncvarid].gmapid = nc_gmap_id;
                  cdfSetVar(ncvars, ncvars[ncvarid].gmapid, FALSE);
                }
              else
                Warning("%s - %s", nc_strerror(status), attstring);

              cdfSetVar(ncvars, ncvarid, TRUE);
            }
          else if ( strcmp(attname, "positive") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              strtolower(attstring);

              if    ( memcmp(attstring, "down", 4) == 0 ) ncvars[ncvarid].positive = POSITIVE_DOWN;
              else if ( memcmp(attstring, "up", 2) == 0 ) ncvars[ncvarid].positive = POSITIVE_UP;

              if ( ncvars[ncvarid].ndims == 1 )
                {
                  cdfSetVar(ncvars, ncvarid, FALSE);
                  cdfSetDim(ncvars, ncvarid, 0, Z_AXIS);
                  ncdims[ncvars[ncvarid].dimids[0]].dimtype = Z_AXIS;
                }
            }
          else if ( strcmp(attname, "_FillValue") == 0 && !xtypeIsText(atttype) )
            {
	      cdfGetAttDouble(ncid, ncvarid, attname, 1, &ncvars[ncvarid].fillval);
	      ncvars[ncvarid].deffillval = TRUE;
	      /* cdfSetVar(ncvars, ncvarid, TRUE); */
            }
          else if ( strcmp(attname, "missing_value") == 0 && !xtypeIsText(atttype) )
            {
	      cdfGetAttDouble(ncid, ncvarid, attname, 1, &ncvars[ncvarid].missval);
	      ncvars[ncvarid].defmissval = TRUE;
	      /* cdfSetVar(ncvars, ncvarid, TRUE); */
            }
          else if ( strcmp(attname, "valid_range") == 0 && attlen == 2 )
            {
              if ( ncvars[ncvarid].lvalidrange == FALSE )
                {
                  extern int cdiIgnoreValidRange;
                  int lignore = xtypeIsFloat(atttype) != xtypeIsFloat(xtype);
                  if ( cdiIgnoreValidRange == FALSE && lignore == FALSE )
                    {
                      cdfGetAttDouble(ncid, ncvarid, attname, 2, ncvars[ncvarid].validrange);
                      ncvars[ncvarid].lvalidrange = TRUE;
                      if ( ((int)ncvars[ncvarid].validrange[0]) == 0 && ((int)ncvars[ncvarid].validrange[1]) == 255 )
                        ncvars[ncvarid].lunsigned = TRUE;
                      /* cdfSetVar(ncvars, ncvarid, TRUE); */
                    }
                  else if ( lignore )
                    {
                      Warning("Inconsistent data type for attribute %s:valid_range, ignored!", name);
                    }
                }
            }
          else if ( strcmp(attname, "valid_min") == 0 && attlen == 1 )
            {
              if ( ncvars[ncvarid].lvalidrange == FALSE )
                {
                  extern int cdiIgnoreValidRange;
                  int lignore = xtypeIsFloat(atttype) != xtypeIsFloat(xtype);
                  if ( cdiIgnoreValidRange == FALSE && lignore == FALSE )
                    {
                      cdfGetAttDouble(ncid, ncvarid, attname, 1, &(ncvars[ncvarid].validrange)[0]);
                      ncvars[ncvarid].lvalidrange = TRUE;
                    }
                  else if ( lignore )
                    {
                      Warning("Inconsistent data type for attribute %s:valid_min, ignored!", name);
                    }
                }
            }
          else if ( strcmp(attname, "valid_max") == 0 && attlen == 1 )
            {
              if ( ncvars[ncvarid].lvalidrange == FALSE )
                {
                  extern int cdiIgnoreValidRange;
                  int lignore = xtypeIsFloat(atttype) != xtypeIsFloat(xtype);
                  if ( cdiIgnoreValidRange == FALSE && lignore == FALSE )
                    {
                      cdfGetAttDouble(ncid, ncvarid, attname, 1, &(ncvars[ncvarid].validrange)[1]);
                      ncvars[ncvarid].lvalidrange = TRUE;
                    }
                  else if ( lignore )
                    {
                      Warning("Inconsistent data type for attribute %s:valid_max, ignored!", name);
                    }
                }
            }
          else if ( strcmp(attname, "_Unsigned") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
              strtolower(attstring);

              if ( memcmp(attstring, "true", 4) == 0 )
                {
                  ncvars[ncvarid].lunsigned = TRUE;
                  /*
                  ncvars[ncvarid].lvalidrange = TRUE;
                  ncvars[ncvarid].validrange[0] = 0;
                  ncvars[ncvarid].validrange[1] = 255;
                  */
                }
	      /* cdfSetVar(ncvars, ncvarid, TRUE); */
            }
          else if ( strcmp(attname, "cdi") == 0 && xtypeIsText(atttype) )
            {
	      cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
	      strtolower(attstring);

	      if ( memcmp(attstring, "ignore", 6) == 0 )
		{
		  ncvars[ncvarid].ignore = TRUE;
		  cdfSetVar(ncvars, ncvarid, FALSE);
		}
            }
          else if ( strcmp(attname, "axis") == 0 && xtypeIsText(atttype) )
            {
              cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
	      attlen = strlen(attstring);

	      if ( (int) attlen > nvdims && nvdims > 0 && attlen > 1 )
		{
		    Warning("Unexpected axis attribute length for %s, ignored!", name);
		}
              else if ( nvdims == 0 && attlen == 1 )
                {
                  if ( attstring[0] == 'z' || attstring[0] == 'Z' )
                    {
                      cdfSetVar(ncvars, ncvarid, FALSE);
                      ncvars[ncvarid].islev = TRUE;
                    }
                }
	      else
		{
		  strtolower(attstring);
                  int i;
		  for ( i = 0; i < (int)attlen; ++i )
		    {
		      if ( attstring[i] != '-' && attstring[i] != 't' && attstring[i] != 'z' &&
			   attstring[i] != 'y' && attstring[i] != 'x' )
			{
			  Warning("Unexpected character in axis attribute for %s, ignored!", name);
			  break;
			}
		    }

		  if ( i == (int) attlen && (int) attlen == nvdims )
		    {
		      while ( attlen-- )
			{
			  if ( (int) attstring[attlen] == 't' )
			    {
			      if ( attlen != 0 ) Warning("axis attribute 't' not on first position");
			      cdfSetDim(ncvars, ncvarid, (int)attlen, T_AXIS);
			    }
			  else if ( (int) attstring[attlen] == 'z' )
			    {
                              ncvars[ncvarid].zdim = dimidsp[attlen];
                              cdfSetDim(ncvars, ncvarid, (int)attlen, Z_AXIS);

                              if ( ncvars[ncvarid].ndims == 1 )
                                {
                                  cdfSetVar(ncvars, ncvarid, FALSE);
                                  ncdims[ncvars[ncvarid].dimids[0]].dimtype = Z_AXIS;
                                }
			    }
			  else if ( (int) attstring[attlen] == 'y' )
			    {
			      ncvars[ncvarid].ydim = dimidsp[attlen];
			      cdfSetDim(ncvars, ncvarid, (int)attlen, Y_AXIS);

			      if ( ncvars[ncvarid].ndims == 1 )
				{
				  cdfSetVar(ncvars, ncvarid, FALSE);
				  ncdims[ncvars[ncvarid].dimids[0]].dimtype = Y_AXIS;
				}
			    }
			  else if ( (int) attstring[attlen] == 'x' )
			    {
			      ncvars[ncvarid].xdim = dimidsp[attlen];
			      cdfSetDim(ncvars, ncvarid, (int)attlen, X_AXIS);

			      if ( ncvars[ncvarid].ndims == 1 )
				{
				  cdfSetVar(ncvars, ncvarid, FALSE);
				  ncdims[ncvars[ncvarid].dimids[0]].dimtype = X_AXIS;
				}
			    }
			}
		    }
		}
	    }
	  else if ( ( strcmp(attname, "realization") == 0 )         ||
	            ( strcmp(attname, "ensemble_members") == 0 )    ||
	            ( strcmp(attname, "forecast_init_type") == 0 )    )
	    {
	      int temp;

	      if( ncvars[ncvarid].ensdata == NULL )
		ncvars[ncvarid].ensdata = (ensinfo_t *) Malloc( sizeof( ensinfo_t ) );

	      cdfGetAttInt(ncid, ncvarid, attname, 1, &temp);

	      if( strcmp(attname, "realization") == 0 )
		ncvars[ncvarid].ensdata->ens_index = temp;
	      else if( strcmp(attname, "ensemble_members") == 0 )
		ncvars[ncvarid].ensdata->ens_count = temp;
	      else if( strcmp(attname, "forecast_init_type") == 0 )
		ncvars[ncvarid].ensdata->forecast_init_type = temp;

	      cdfSetVar(ncvars, ncvarid, TRUE);
	    }
	  else
	    {
	      if ( ncvars[ncvarid].natts == 0 )
		ncvars[ncvarid].atts
                  = (int *) Malloc((size_t)nvatts * sizeof (int));

	      ncvars[ncvarid].atts[ncvars[ncvarid].natts++] = iatt;
	      /*
	      int attrint;
	      double attrflt;
	      nc_type attrtype;
	      cdf_inq_attlen(ncid, ncvarid, attname, &attlen);
	      cdf_inq_atttype(ncid, ncvarid, attname, &attrtype);
	      if ( attlen == 1 && (attrtype == NC_INT || attrtype == NC_SHORT) )
		{
		  cdfGetAttInt(ncid, ncvarid, attname, 1, &attrint);
		  printf("int: %s.%s = %d\n", ncvars[ncvarid].name, attname, attrint);
		}
	      else if ( attlen == 1 && (attrtype == NC_FLOAT || attrtype == NC_DOUBLE) )
		{
		  cdfGetAttDouble(ncid, ncvarid, attname, 1, &attrflt);
		  printf("flt: %s.%s = %g\n", ncvars[ncvarid].name, attname, attrflt);
		}
	      else if ( attrtype == NC_CHAR )
		{
		  cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
		  attstring[attlen] = 0;
		  printf("txt: %s.%s = %s\n", ncvars[ncvarid].name, attname, attstring);
		}
	      else
		printf("att: %s.%s = unknown\n", ncvars[ncvarid].name, attname);
	      */
	    }
	}
    }

  for ( int i = 0; i < max_check_vars; ++i ) if ( checked_vars[i] ) Free(checked_vars[i]);
}

static
void setDimType(int nvars, ncvar_t *ncvars, ncdim_t *ncdims)
{
  int ndims;
  int ncvarid, ncdimid;
  int i;

  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].isvar == TRUE )
	{
	  int lxdim = 0, lydim = 0, lzdim = 0/* , ltdim = 0 */;
	  ndims = ncvars[ncvarid].ndims;
	  for ( i = 0; i < ndims; i++ )
	    {
	      ncdimid = ncvars[ncvarid].dimids[i];
	      if      ( ncdims[ncdimid].dimtype == X_AXIS ) cdfSetDim(ncvars, ncvarid, i, X_AXIS);
	      else if ( ncdims[ncdimid].dimtype == Y_AXIS ) cdfSetDim(ncvars, ncvarid, i, Y_AXIS);
	      else if ( ncdims[ncdimid].dimtype == Z_AXIS ) cdfSetDim(ncvars, ncvarid, i, Z_AXIS);
	      else if ( ncdims[ncdimid].dimtype == T_AXIS ) cdfSetDim(ncvars, ncvarid, i, T_AXIS);
	    }

	  if ( CDI_Debug )
	    {
	      Message("var %d %s", ncvarid, ncvars[ncvarid].name);
	      for ( i = 0; i < ndims; i++ )
		printf("  dim%d type=%d  ", i, ncvars[ncvarid].dimtype[i]);
	      printf("\n");
	    }

	  for ( i = 0; i < ndims; i++ )
	    {
	      if      ( ncvars[ncvarid].dimtype[i] == X_AXIS ) lxdim = TRUE;
	      else if ( ncvars[ncvarid].dimtype[i] == Y_AXIS ) lydim = TRUE;
	      else if ( ncvars[ncvarid].dimtype[i] == Z_AXIS ) lzdim = TRUE;
	      /* else if ( ncvars[ncvarid].dimtype[i] == T_AXIS ) ltdim = TRUE; */
	    }

          if ( lxdim == FALSE && ncvars[ncvarid].xvarid != UNDEFID )
            {
              if (  ncvars[ncvars[ncvarid].xvarid].ndims == 0 ) lxdim = TRUE;
            }

          if ( lydim == FALSE && ncvars[ncvarid].yvarid != UNDEFID )
            {
              if (  ncvars[ncvars[ncvarid].yvarid].ndims == 0 ) lydim = TRUE;
            }

          //   if ( ndims > 1 )
            for ( i = ndims-1; i >= 0; i-- )
              {
                if ( ncvars[ncvarid].dimtype[i] == -1 )
                  {
                    if ( lxdim == FALSE )
                      {
                        cdfSetDim(ncvars, ncvarid, i, X_AXIS);
                        lxdim = TRUE;
                      }
                    else if ( lydim == FALSE && ncvars[ncvarid].gridtype != GRID_UNSTRUCTURED )
                      {
                        cdfSetDim(ncvars, ncvarid, i, Y_AXIS);
                        lydim = TRUE;
                      }
                    else if ( lzdim == FALSE )
                      {
                        cdfSetDim(ncvars, ncvarid, i, Z_AXIS);
                        lzdim = TRUE;
                      }
                  }
              }
	}
    }
}

/* verify coordinate vars - first scan (dimname == varname) */
static
void verify_coordinate_vars_1(int ncid, int ndims, ncdim_t *ncdims, ncvar_t *ncvars, int timedimid)
{
  int ncdimid, ncvarid;

  for ( ncdimid = 0; ncdimid < ndims; ncdimid++ )
    {
      ncvarid = ncdims[ncdimid].ncvarid;
      if ( ncvarid != -1 )
	{
	  if ( ncvars[ncvarid].dimids[0] == timedimid )
	    {
              ncvars[ncvarid].istime = TRUE;
	      ncdims[ncdimid].dimtype = T_AXIS;
	      continue;
	    }

          if ( isHybridSigmaPressureCoordinate(ncid, ncvarid, ncvars, ncdims) ) continue;

	  if ( ncvars[ncvarid].units[0] != 0 )
	    {
	      if ( isLonAxis(ncvars[ncvarid].units, ncvars[ncvarid].stdname) )
		{
		  ncvars[ncvarid].islon = TRUE;
		  cdfSetVar(ncvars, ncvarid, FALSE);
		  cdfSetDim(ncvars, ncvarid, 0, X_AXIS);
		  ncdims[ncdimid].dimtype = X_AXIS;
		}
	      else if ( isLatAxis(ncvars[ncvarid].units, ncvars[ncvarid].stdname) )
		{
		  ncvars[ncvarid].islat = TRUE;
		  cdfSetVar(ncvars, ncvarid, FALSE);
		  cdfSetDim(ncvars, ncvarid, 0, Y_AXIS);
		  ncdims[ncdimid].dimtype = Y_AXIS;
		}
	      else if ( unitsIsPressure(ncvars[ncvarid].units) )
		{
		  ncvars[ncvarid].zaxistype = ZAXIS_PRESSURE;
		}
	      else if ( strcmp(ncvars[ncvarid].units, "level") == 0 || strcmp(ncvars[ncvarid].units, "1") == 0 )
		{
		  if      ( strcmp(ncvars[ncvarid].longname, "hybrid level at layer midpoints") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID;
		  else if ( strncmp(ncvars[ncvarid].longname, "hybrid level at midpoints", 25) == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID;
		  else if ( strcmp(ncvars[ncvarid].longname, "hybrid level at layer interfaces") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID_HALF;
		  else if ( strncmp(ncvars[ncvarid].longname, "hybrid level at interfaces", 26) == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID_HALF;
		  else if ( strcmp(ncvars[ncvarid].units, "level") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_GENERIC;
		}
	      else if ( isDBLAxis(ncvars[ncvarid].longname) )
                {
                  ncvars[ncvarid].zaxistype = ZAXIS_DEPTH_BELOW_LAND;
		}
	      else if ( unitsIsHeight(ncvars[ncvarid].units) )
		{
		  if ( isDepthAxis(ncvars[ncvarid].stdname, ncvars[ncvarid].longname) )
		    ncvars[ncvarid].zaxistype = ZAXIS_DEPTH_BELOW_SEA;
		  else if ( isHeightAxis(ncvars[ncvarid].stdname, ncvars[ncvarid].longname) )
		    ncvars[ncvarid].zaxistype = ZAXIS_HEIGHT;
		}
	    }
          else
            {
              if ( (strcmp(ncvars[ncvarid].longname, "generalized_height") == 0 ||
                    strcmp(ncvars[ncvarid].longname, "generalized height") == 0) &&
                   strcmp(ncvars[ncvarid].stdname, "height") == 0 )
                  ncvars[ncvarid].zaxistype = ZAXIS_REFERENCE;
            }

	  if ( ncvars[ncvarid].islon == FALSE && ncvars[ncvarid].longname[0] != 0 &&
               ncvars[ncvarid].islat == FALSE && ncvars[ncvarid].longname[1] != 0 )
	    {
	      if ( memcmp(ncvars[ncvarid].longname+1, "ongitude", 8) == 0 )
		{
		  ncvars[ncvarid].islon = TRUE;
		  cdfSetVar(ncvars, ncvarid, FALSE);
		  cdfSetDim(ncvars, ncvarid, 0, X_AXIS);
		  ncdims[ncdimid].dimtype = X_AXIS;
		  continue;
		}
	      else if ( memcmp(ncvars[ncvarid].longname+1, "atitude", 7) == 0 )
		{
		  ncvars[ncvarid].islat = TRUE;
		  cdfSetVar(ncvars, ncvarid, FALSE);
		  cdfSetDim(ncvars, ncvarid, 0, Y_AXIS);
		  ncdims[ncdimid].dimtype = Y_AXIS;
		  continue;
		}
	    }

	  if ( ncvars[ncvarid].zaxistype != UNDEFID )
	    {
              ncvars[ncvarid].islev = TRUE;
	      cdfSetVar(ncvars, ncvarid, FALSE);
	      cdfSetDim(ncvars, ncvarid, 0, Z_AXIS);
	      ncdims[ncdimid].dimtype = Z_AXIS;
	    }
	}
    }
}

/* verify coordinate vars - second scan (all other variables) */
static
void verify_coordinate_vars_2(int nvars, ncvar_t *ncvars)
{
  int ncvarid;

  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].isvar == 0 )
	{
	  if ( ncvars[ncvarid].units[0] != 0 )
	    {
	      if ( isLonAxis(ncvars[ncvarid].units, ncvars[ncvarid].stdname) )
		{
		  ncvars[ncvarid].islon = TRUE;
		  continue;
		}
	      else if ( isLatAxis(ncvars[ncvarid].units, ncvars[ncvarid].stdname) )
		{
		  ncvars[ncvarid].islat = TRUE;
		  continue;
		}
	      else if ( unitsIsPressure(ncvars[ncvarid].units) )
		{
		  ncvars[ncvarid].zaxistype = ZAXIS_PRESSURE;
		  continue;
		}
	      else if ( strcmp(ncvars[ncvarid].units, "level") == 0 || strcmp(ncvars[ncvarid].units, "1") == 0 )
		{
		  if      ( strcmp(ncvars[ncvarid].longname, "hybrid level at layer midpoints") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID;
		  else if ( strncmp(ncvars[ncvarid].longname, "hybrid level at midpoints", 25) == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID;
		  else if ( strcmp(ncvars[ncvarid].longname, "hybrid level at layer interfaces") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID_HALF;
		  else if ( strncmp(ncvars[ncvarid].longname, "hybrid level at interfaces", 26) == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_HYBRID_HALF;
		  else if ( strcmp(ncvars[ncvarid].units, "level") == 0 )
		    ncvars[ncvarid].zaxistype = ZAXIS_GENERIC;
		  continue;
		}
	      else if ( isDBLAxis(ncvars[ncvarid].longname) )
		{
                  ncvars[ncvarid].zaxistype = ZAXIS_DEPTH_BELOW_LAND;
		  continue;
		}
	      else if ( unitsIsHeight(ncvars[ncvarid].units) )
		{
		  if ( isDepthAxis(ncvars[ncvarid].stdname, ncvars[ncvarid].longname) )
		    ncvars[ncvarid].zaxistype = ZAXIS_DEPTH_BELOW_SEA;
		  else if ( isHeightAxis(ncvars[ncvarid].stdname, ncvars[ncvarid].longname) )
		    ncvars[ncvarid].zaxistype = ZAXIS_HEIGHT;
		  continue;
		}
            }

	  /* not needed anymore for rotated grids */
	  if ( ncvars[ncvarid].islon == FALSE && ncvars[ncvarid].longname[0] != 0 &&
               ncvars[ncvarid].islat == FALSE && ncvars[ncvarid].longname[1] != 0 )
	    {
	      if ( memcmp(ncvars[ncvarid].longname+1, "ongitude", 8) == 0 )
		{
		  ncvars[ncvarid].islon = TRUE;
		  continue;
		}
	      else if ( memcmp(ncvars[ncvarid].longname+1, "atitude", 7) == 0 )
		{
		  ncvars[ncvarid].islat = TRUE;
		  continue;
		}
	    }
	}
    }
}

#if defined (PROJECTION_TEST)
static
void copy_numeric_projatts(int gridID, int ncvarID, int ncfileID)
{
  int iatt, nvatts;
  size_t attlen;
  char attname[CDI_MAX_NAME];
  nc_type xtype;

  cdf_inq_varnatts(ncfileID, ncvarID, &nvatts);

  for ( iatt = 0; iatt < nvatts; iatt++ )
    {
      cdf_inq_attname(ncfileID, ncvarID, iatt, attname);
      cdf_inq_atttype(ncfileID, ncvarID, attname, &xtype);
      cdf_inq_attlen(ncfileID, ncvarID, attname, &attlen);

      //  printf("%s %d\n", attname, (int)attlen);
    }

}
#endif

static
void grid_set_chunktype(grid_t *grid, ncvar_t *ncvar)
{
  if ( ncvar->chunked )
    {
      int ndims = ncvar->ndims;

      if ( grid->type == GRID_UNSTRUCTURED )
        {
          if ( ncvar->chunks[ndims-1] == grid->size )
            ncvar->chunktype = CHUNK_GRID;
          else
            ncvar->chunktype = CHUNK_AUTO;
        }
      else
        {
          if ( grid->xsize > 1 && grid->ysize > 1 && ndims > 1 &&
               grid->xsize == ncvar->chunks[ndims-1] &&
               grid->ysize == ncvar->chunks[ndims-2] )
            ncvar->chunktype = CHUNK_GRID;
          else if ( grid->xsize > 1 && grid->xsize == ncvar->chunks[ndims-1] )
            ncvar->chunktype = CHUNK_LINES;
          else
            ncvar->chunktype = CHUNK_AUTO;
        }
    }
}

static struct gridVirtTable cdfLazyGridVtable;
static double *cdfPendingLoad;
#ifdef HAVE_LIBPTHREAD
static pthread_once_t cdfLazyInitialized = PTHREAD_ONCE_INIT;
#else
static bool cdfLazyInitialized;
#endif

struct cdfLazyGrid
{
  grid_t base;
  const struct gridVirtTable *baseVtable;
  struct {
    int datasetNCId, varNCId;
  } cellAreaGet, xBoundsGet, yBoundsGet;
  struct xyValGet {
    double scalefactor, addoffset;
    size_t start[3], count[3], size, dimsize;
    int datasetNCId, varNCId;
    short ndims;
  } xValsGet, yValsGet;
#ifdef HAVE_LIBPTHREAD
  pthread_mutex_t loadSerialize;
#endif
};

#ifdef HAVE_LIBPTHREAD
#define lock_lazy_load(plGrid) pthread_mutex_lock(&((plGrid)->loadSerialize))
#define unlock_lazy_load(plGrid) pthread_mutex_unlock(&((plGrid)->loadSerialize))
#define destroy_lazy_load_lock(plGrid) pthread_mutex_destroy(&((plGrid)->loadSerialize))
#define init_lazy_load_lock(plGrid) pthread_mutex_init(&((plGrid)->loadSerialize), NULL)
#else
#define lock_lazy_load(plGrid)
#define unlock_lazy_load(plGrid)
#define destroy_lazy_load_lock(plGrid)
#define init_lazy_load_lock(plGrid)
#endif

static void cdfLazyGridDestroy(struct cdfLazyGrid *lazyGrid)
{
  lazyGrid->base.extraData = NULL;
  if (lazyGrid->base.area == cdfPendingLoad)
    lazyGrid->base.area = NULL;
  if (lazyGrid->base.xvals == cdfPendingLoad)
    lazyGrid->base.xvals = NULL;
  if (lazyGrid->base.yvals == cdfPendingLoad)
    lazyGrid->base.yvals = NULL;
  if (lazyGrid->base.xbounds == cdfPendingLoad)
    lazyGrid->base.xbounds = NULL;
  if (lazyGrid->base.ybounds == cdfPendingLoad)
    lazyGrid->base.ybounds = NULL;
  destroy_lazy_load_lock(lazyGrid);
}

static void cdfLazyGridDelete(grid_t *grid)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  void (*baseDestroy)(grid_t *grid) = cdfGrid->baseVtable->destroy;
  cdfLazyGridDestroy(cdfGrid);
  baseDestroy(grid);
}

static void cdfLazyGridDestroyOnce(void)
{
  /*
#ifdef HAVE_MMAP
  size_t pgSize = cdiGetPageSize(false);
  munmap(cdfPendingLoad, pgSize);
#endif
  */
}

static void
cdfLazyGridDefArea(grid_t *grid, const double *area)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(cdfGrid);
  if (grid->area == cdfPendingLoad)
    grid->area = NULL;
  cdfGrid->cellAreaGet.datasetNCId = -1;
  cdfGrid->cellAreaGet.varNCId = -1;
  cdfGrid->baseVtable->defArea(grid, area);
  unlock_lazy_load(cdfGrid);
}


static const double *
cdfLazyGridInqAreaPtr(grid_t *grid)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  if (grid->area == cdfPendingLoad)
    {
      grid->area = (double *)Malloc((size_t)grid->size * sizeof(double));
      cdf_get_var_double(lazyGrid->cellAreaGet.datasetNCId,
                         lazyGrid->cellAreaGet.varNCId, grid->area);
    }
  unlock_lazy_load(lazyGrid);
  return lazyGrid->baseVtable->inqAreaPtr(grid);
}

static void
cdfLazyGridInqArea(grid_t *grid, double *area)
{
  grid->vtable->inqAreaPtr(grid);
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lazyGrid->baseVtable->inqArea(grid, area);
}


static void
cdfLazyLoadXYVals(struct xyValGet *valsGet, double **valsp)
{
  double *grid_vals
    = (double *)Malloc(valsGet->size * sizeof (double));
  *valsp = grid_vals;
  if ( valsGet->ndims == 3 )
    cdf_get_vara_double(valsGet->datasetNCId, valsGet->varNCId,
                        valsGet->start, valsGet->count, grid_vals);
  else
    cdf_get_var_double(valsGet->datasetNCId, valsGet->varNCId, grid_vals);
  scale_add(valsGet->size, grid_vals, valsGet->addoffset, valsGet->scalefactor);
}

static const double *
cdfLazyGridInqXValsPtr(grid_t *grid)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  if (grid->xvals == cdfPendingLoad)
    cdfLazyLoadXYVals(&lazyGrid->xValsGet, &grid->xvals);
  unlock_lazy_load(lazyGrid);
  return lazyGrid->baseVtable->inqXValsPtr(grid);
}

static const double *
cdfLazyGridInqYValsPtr(grid_t *grid)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  if (grid->yvals == cdfPendingLoad)
    cdfLazyLoadXYVals(&lazyGrid->yValsGet, &grid->yvals);
  unlock_lazy_load(lazyGrid);
  return lazyGrid->baseVtable->inqYValsPtr(grid);
}

static double
cdfLazyGridInqXYVal(grid_t *grid, size_t index,
                    const struct xyValGet *valsGet, double *vals,
                    const double *(*inqValsPtr)(grid_t *gridptr))
{
  size_t size = valsGet->size;
  double v;
  if ( vals == cdfPendingLoad )
    {
      /* prevent full load if only first/last values get inspected */
      if ( index == 0 || index == size - 1 )
        {
          size_t indexND[3];
          if ( valsGet->ndims == 3 )
            {
              indexND[0] = 0;
              indexND[1] = index / valsGet->count[2];
              indexND[2] = index % valsGet->count[2];
            }
          else if ( valsGet->ndims == 2)
            {
              indexND[0] = index / (size_t)grid->xsize;
              indexND[1] = index % (size_t)grid->xsize;
            }
          else
            indexND[0] = index;
          cdf_get_var1_double(valsGet->datasetNCId, valsGet->varNCId,
                              indexND, &v);
        }
      else
        {
          const double *grid_vals = inqValsPtr(grid);
          v = grid_vals[index];
        }
    }
  else if ( vals )
    v = vals[index];
  else
    v = 0.0;
  return v;
}

static void
cdfLazyGridDefXVals(grid_t *grid, const double *vals)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(cdfGrid);
  if (grid->xvals == cdfPendingLoad)
    grid->xvals = NULL;
  cdfGrid->xValsGet.datasetNCId = -1;
  cdfGrid->xValsGet.varNCId = -1;
  cdfGrid->baseVtable->defXVals(grid, vals);
  unlock_lazy_load(cdfGrid);
}

static void
cdfLazyGridDefYVals(grid_t *grid, const double *vals)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(cdfGrid);
  if (grid->yvals == cdfPendingLoad)
    grid->yvals = NULL;
  cdfGrid->yValsGet.datasetNCId = -1;
  cdfGrid->yValsGet.varNCId = -1;
  cdfGrid->baseVtable->defYVals(grid, vals);
  unlock_lazy_load(cdfGrid);
}

static double
cdfLazyGridInqXVal(grid_t *grid, int index)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  double rv = cdfLazyGridInqXYVal(grid, (size_t)index, &lazyGrid->xValsGet,
                                  grid->xvals, grid->vtable->inqXValsPtr);
  unlock_lazy_load(lazyGrid);
  return rv;
}

static double
cdfLazyGridInqYVal(grid_t *grid, int index)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  double rv = cdfLazyGridInqXYVal(grid, (size_t)index, &lazyGrid->yValsGet,
                                  grid->yvals, grid->vtable->inqYValsPtr);
  unlock_lazy_load(lazyGrid);
  return rv;
}

static bool
cdfLazyXYValGetCompare(struct cdfLazyGrid *lazyGridRef,
                       struct cdfLazyGrid *lazyGridTest)
{
  struct xyValGet *valsGetXRef = &lazyGridRef->xValsGet,
    *valsGetYRef = &lazyGridRef->yValsGet,
    *valsGetXTest = &lazyGridTest->xValsGet,
    *valsGetYTest = &lazyGridTest->yValsGet;
  if (valsGetXRef->datasetNCId == -1
      || valsGetXTest->datasetNCId == -1
      || valsGetYRef->datasetNCId == -1
      || valsGetYTest->datasetNCId == -1)
    return lazyGridRef->baseVtable->compareXYFull(&lazyGridRef->base,
                                                  &lazyGridTest->base);
  return valsGetXRef->datasetNCId != valsGetXTest->datasetNCId
    ||   valsGetXRef->varNCId     != valsGetXTest->varNCId
    ||   valsGetYRef->datasetNCId != valsGetYTest->datasetNCId
    ||   valsGetYRef->varNCId     != valsGetYTest->varNCId;
}

static bool
cdfLazyCompareXYFull(grid_t *gridRef, grid_t *gridTest)
{
  bool diff;
  struct cdfLazyGrid *lazyGridRef = (struct cdfLazyGrid *)gridRef;
  if (gridTest->vtable == &cdfLazyGridVtable)
    diff = cdfLazyXYValGetCompare(lazyGridRef, (struct cdfLazyGrid *)gridTest);
  else
    diff = lazyGridRef->baseVtable->compareXYFull(gridRef, gridTest);
  return diff;
}

static bool
cdfLazyCompareXYAO(grid_t *gridRef, grid_t *gridTest)
{
  bool diff;
  struct cdfLazyGrid *lazyGridRef = (struct cdfLazyGrid *)gridRef;
  if (gridTest->vtable == &cdfLazyGridVtable)
    diff = cdfLazyXYValGetCompare(lazyGridRef, (struct cdfLazyGrid *)gridTest);
  else
    diff = lazyGridRef->baseVtable->compareXYAO(gridRef, gridTest);
  return diff;
}


static const double *
cdfLazyGridInqXBoundsPtr(grid_t *grid)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  if (grid->xbounds == cdfPendingLoad)
    {
      grid->xbounds = (double *)Malloc((size_t)grid->nvertex
                                       * (size_t)grid->size * sizeof(double));
      cdf_get_var_double(lazyGrid->xBoundsGet.datasetNCId,
                         lazyGrid->xBoundsGet.varNCId, grid->xbounds);
    }
  unlock_lazy_load(lazyGrid);
  return lazyGrid->baseVtable->inqXBoundsPtr(grid);
}

static void
cdfLazyGridDefXBounds(grid_t *grid, const double *xbounds)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(cdfGrid);
  if (grid->xbounds == cdfPendingLoad)
    grid->xbounds = NULL;
  cdfGrid->xBoundsGet.datasetNCId = -1;
  cdfGrid->xBoundsGet.varNCId = -1;
  cdfGrid->baseVtable->defXBounds(grid, xbounds);
  unlock_lazy_load(cdfGrid);
}

static void
cdfLazyGridDefYBounds(grid_t *grid, const double *ybounds)
{
  struct cdfLazyGrid *cdfGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(cdfGrid);
  if (grid->ybounds == cdfPendingLoad)
    grid->ybounds = NULL;
  cdfGrid->yBoundsGet.datasetNCId = -1;
  cdfGrid->yBoundsGet.varNCId = -1;
  cdfGrid->baseVtable->defYBounds(grid, ybounds);
  unlock_lazy_load(cdfGrid);
}

static const double *
cdfLazyGridInqYBoundsPtr(grid_t *grid)
{
  struct cdfLazyGrid *lazyGrid = (struct cdfLazyGrid *)grid;
  lock_lazy_load(lazyGrid);
  if (grid->ybounds == cdfPendingLoad)
    {
      grid->ybounds = (double *)Malloc((size_t)grid->nvertex
                                       * (size_t)grid->size * sizeof(double));
      cdf_get_var_double(lazyGrid->yBoundsGet.datasetNCId,
                         lazyGrid->yBoundsGet.varNCId, grid->ybounds);
    }
  unlock_lazy_load(lazyGrid);
  return lazyGrid->baseVtable->inqYBoundsPtr(grid);
}

static void
cdfLazyGridCopyScalarFields(grid_t *gridptrOrig, grid_t *gridptrDup)
{
  struct cdfLazyGrid *lazyGridDup = (struct cdfLazyGrid *)gridptrDup,
    *lazyGridOrig = (struct cdfLazyGrid *)gridptrOrig;
  lazyGridOrig->baseVtable->copyScalarFields(gridptrOrig, &lazyGridDup->base);
  lazyGridDup->baseVtable = lazyGridOrig->baseVtable;
  lazyGridDup->cellAreaGet = lazyGridOrig->cellAreaGet;
  lazyGridDup->xBoundsGet = lazyGridOrig->xBoundsGet;
  lazyGridDup->yBoundsGet = lazyGridOrig->yBoundsGet;
  lazyGridDup->xValsGet = lazyGridOrig->xValsGet;
  lazyGridDup->yValsGet = lazyGridOrig->yValsGet;
  init_lazy_load_lock(lazyGridDup);
}

static void
cdfLazyGridCopyArrayFields(grid_t *gridptrOrig, grid_t *gridptrDup)
{
  size_t nrowlon = (size_t)gridptrOrig->nrowlon;
  size_t gridsize = (size_t)gridptrOrig->size;
  int gridtype = gridptrOrig->type;
  int irregular = gridtype == GRID_CURVILINEAR || gridtype == GRID_UNSTRUCTURED;
  if ( nrowlon )
    {
      gridptrDup->rowlon = (int *)Malloc(nrowlon * sizeof (int));
      memcpy(gridptrDup->rowlon, gridptrOrig->rowlon, nrowlon * sizeof(int));
    }

  if ( gridptrOrig->xvals != NULL && gridptrOrig->xvals != cdfPendingLoad )
    {
      size_t size  = irregular ? gridsize : (size_t)gridptrOrig->xsize;

      gridptrDup->xvals = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->xvals, gridptrOrig->xvals, size * sizeof (double));
    }

  if ( gridptrOrig->yvals != NULL && gridptrOrig->yvals != cdfPendingLoad )
    {
      size_t size  = irregular ? gridsize : (size_t)gridptrOrig->ysize;

      gridptrDup->yvals = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->yvals, gridptrOrig->yvals, size * sizeof (double));
    }

  if ( gridptrOrig->xbounds != NULL && gridptrOrig->xbounds != cdfPendingLoad )
    {
      size_t size  = (irregular ? gridsize : (size_t)gridptrOrig->xsize)
        * (size_t)gridptrOrig->nvertex;

      gridptrDup->xbounds = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->xbounds, gridptrOrig->xbounds, size * sizeof (double));
    }

  if ( gridptrOrig->ybounds != NULL && gridptrOrig->ybounds != cdfPendingLoad )
    {
      size_t size = (irregular ? gridsize : (size_t)gridptrOrig->ysize)
        * (size_t)gridptrOrig->nvertex;

      gridptrDup->ybounds = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->ybounds, gridptrOrig->ybounds, size * sizeof (double));
    }

  {
    if ( gridptrOrig->area != NULL && gridptrOrig->area != cdfPendingLoad )
      {
        size_t size = gridsize;

        gridptrDup->area = (double *)Malloc(size * sizeof (double));
        memcpy(gridptrDup->area, gridptrOrig->area, size * sizeof (double));
      }
  }

  if ( gridptrOrig->mask != NULL )
    {
      size_t size = gridsize;

      gridptrDup->mask = (mask_t *)Malloc(size * sizeof(mask_t));
      memcpy(gridptrDup->mask, gridptrOrig->mask, size * sizeof (mask_t));
    }

  if ( gridptrOrig->mask_gme != NULL )
    {
      size_t size = gridsize;

      gridptrDup->mask_gme = (mask_t *)Malloc(size * sizeof (mask_t));
      memcpy(gridptrDup->mask_gme, gridptrOrig->mask_gme, size * sizeof(mask_t));
    }
}

static grid_t *
cdfLazyGridCopy(grid_t *gridptrOrig)
{
  struct cdfLazyGrid *lazyGridDup
    = (struct cdfLazyGrid *)Malloc(sizeof (*lazyGridDup));
  gridptrOrig->vtable->copyScalarFields(gridptrOrig, &lazyGridDup->base);
  gridptrOrig->vtable->copyArrayFields(gridptrOrig, &lazyGridDup->base);
  return &lazyGridDup->base;
}

static void
cdfLazyGridInitOnce(void)
{
  cdfLazyGridVtable = cdiGridVtable;
  cdfLazyGridVtable.destroy = cdfLazyGridDelete;
  cdfLazyGridVtable.copy = cdfLazyGridCopy;
  cdfLazyGridVtable.copyScalarFields = cdfLazyGridCopyScalarFields;
  cdfLazyGridVtable.copyArrayFields = cdfLazyGridCopyArrayFields;
  cdfLazyGridVtable.defArea = cdfLazyGridDefArea;
  cdfLazyGridVtable.inqAreaPtr = cdfLazyGridInqAreaPtr;
  cdfLazyGridVtable.inqArea = cdfLazyGridInqArea;
  cdfLazyGridVtable.inqXValsPtr = cdfLazyGridInqXValsPtr;
  cdfLazyGridVtable.inqYValsPtr = cdfLazyGridInqYValsPtr;
  cdfLazyGridVtable.inqXVal = cdfLazyGridInqXVal;
  cdfLazyGridVtable.inqYVal = cdfLazyGridInqYVal;
  cdfLazyGridVtable.defXVals = cdfLazyGridDefXVals;
  cdfLazyGridVtable.defYVals = cdfLazyGridDefYVals;
  cdfLazyGridVtable.compareXYFull = cdfLazyCompareXYFull;
  cdfLazyGridVtable.compareXYAO = cdfLazyCompareXYAO;
  cdfLazyGridVtable.defXBounds = cdfLazyGridDefXBounds;
  cdfLazyGridVtable.defYBounds = cdfLazyGridDefYBounds;
  cdfLazyGridVtable.inqXBoundsPtr = cdfLazyGridInqXBoundsPtr;
  cdfLazyGridVtable.inqYBoundsPtr = cdfLazyGridInqYBoundsPtr;
  /* create inaccessible memory area, if possible, this serves as
   * dummy value for pointers to data not yet loaded */
  /*
#ifdef HAVE_MMAP
  {
    size_t pgSize = cdiGetPageSize(false);
    static const char devZero[] = "/dev/zero";
    int fd = open(devZero, O_RDWR);
    if (fd == -1)
      SysError("Could not open %s to map anonymous memory", devZero);
    void *cdfInvalid = mmap(NULL, pgSize, PROT_NONE, MAP_PRIVATE, fd, 0);
    if (cdfInvalid == MAP_FAILED)
      SysError("Could not mmap anonymous memory");
    cdfPendingLoad = cdfInvalid;
    int rc = close(fd);
    if (rc == -1)
      SysError("Could not close %s file handle %d after mapping anonymous"
               " memory", devZero, fd);
  }
#else
  */
  cdfPendingLoad = (double *)&cdfPendingLoad;
  //#endif
  atexit(cdfLazyGridDestroyOnce);
#ifndef HAVE_LIBPTHREAD
  cdfLazyInitialized = true;
#endif
}

static void
cdfBaseGridInit(grid_t *grid, int gridtype)
{
  grid_init(grid);
  cdiGridTypeInit(grid, gridtype, 0);
}

static void
cdfLazyGridInit(struct cdfLazyGrid *grid, int gridtype)
{
#ifdef HAVE_LIBPTHREAD
  pthread_once(&cdfLazyInitialized, cdfLazyGridInitOnce);
#else
  if (cdfLazyInitialized) ; else cdfLazyGridInitOnce();
#endif
  cdfBaseGridInit(&grid->base, gridtype);
  grid->baseVtable = grid->base.vtable;
  grid->cellAreaGet.datasetNCId = -1;
  grid->cellAreaGet.varNCId = -1;
  grid->xValsGet.datasetNCId = -1;
  grid->xValsGet.varNCId = -1;
  grid->yValsGet.datasetNCId = -1;
  grid->yValsGet.varNCId = -1;
  grid->xBoundsGet.datasetNCId = -1;
  grid->xBoundsGet.varNCId = -1;
  grid->yBoundsGet.datasetNCId = -1;
  grid->yBoundsGet.varNCId = -1;
  grid->base.vtable = &cdfLazyGridVtable;
  init_lazy_load_lock(grid);
}

static void
cdfLazyGridRenew(struct cdfLazyGrid *restrict *restrict gridpptr, int gridtype)
{
  struct cdfLazyGrid *restrict grid = *gridpptr;
  if (!grid)
    *gridpptr = grid = (struct cdfLazyGrid *)Malloc(sizeof (*grid));
  cdfLazyGridInit(grid, gridtype);
}

static void
cdfBaseGridRenew(struct cdfLazyGrid *restrict *restrict gridpptr, int gridtype)
{
  struct cdfLazyGrid *restrict grid = *gridpptr;
  if (!grid)
    *gridpptr = grid = (struct cdfLazyGrid *)Malloc(sizeof (grid_t));
  cdfBaseGridInit((grid_t*)grid, gridtype);
}


/* define all input grids */
static
void define_all_grids(stream_t *streamptr, int vlistID, ncdim_t *ncdims, int nvars, ncvar_t *ncvars, int timedimid, unsigned char *uuidOfHGrid, char *gridfile, int number_of_grid_used)
{
  int ltwarn = TRUE;
  struct cdfLazyGrid *restrict lazyGrid = NULL, *restrict lazyProj = NULL;
#define grid (&lazyGrid->base)
#define proj (&lazyProj->base)

  for ( int ncvarid = 0; ncvarid < nvars; ++ncvarid )
    {
      if ( ncvars[ncvarid].isvar && ncvars[ncvarid].gridID == UNDEFID )
	{
          int xdimids[2] = {-1,-1}, ydimids[2] = {-1,-1};
	  int xdimid = -1, ydimid = -1;
          int vdimid = -1;
	  int islon = 0, islat = 0;
	  int nxdims = 0, nydims = 0;
          size_t size = 0;
          size_t xsize = 0, ysize = 0;
	  double yinc = 0;
          struct addIffNewRes projAdded = { .Id = CDI_UNDEFID, .isNew = 0 },
            gridAdded  = { .Id = CDI_UNDEFID, .isNew = 0 };

	  int ndims = ncvars[ncvarid].ndims;
	  for ( int i = 0; i < ndims; i++ )
	    {
	      if ( ncvars[ncvarid].dimtype[i] == X_AXIS && nxdims < 2 )
		{
		  xdimids[nxdims] = ncvars[ncvarid].dimids[i];
		  nxdims++;
		}
	      else if ( ncvars[ncvarid].dimtype[i] == Y_AXIS && nydims < 2 )
		{
		  ydimids[nydims] = ncvars[ncvarid].dimids[i];
		  nydims++;
		}
	    }

	  if ( nxdims == 2 )
	    {
	      xdimid = xdimids[1];
	      ydimid = xdimids[0];
	    }
	  else if ( nydims == 2 )
	    {
	      xdimid = ydimids[1];
	      ydimid = ydimids[0];
	    }
	  else
	    {
	      xdimid = xdimids[0];
	      ydimid = ydimids[0];
	    }

	  int xvarid = ncvars[ncvarid].xvarid != UNDEFID
	    ? ncvars[ncvarid].xvarid
            : (xdimid != UNDEFID ? ncdims[xdimid].ncvarid : -1);
          int yvarid = ncvars[ncvarid].yvarid != UNDEFID
            ? ncvars[ncvarid].yvarid
            : (ydimid != UNDEFID ? ncdims[ydimid].ncvarid : -1);

	  /*
	  if ( xdimid != UNDEFID )
	    xvarid = ncdims[xdimid].ncvarid;
	  if ( xvarid == UNDEFID && ncvars[ncvarid].xvarid != UNDEFID )
	    xvarid = ncvars[ncvarid].xvarid;

	  if ( ydimid != UNDEFID )
	    yvarid = ncdims[ydimid].ncvarid;
	  if ( yvarid == UNDEFID && ncvars[ncvarid].yvarid != UNDEFID )
	    yvarid = ncvars[ncvarid].yvarid;
	  */

	  if ( xdimid != UNDEFID ) xsize = ncdims[xdimid].len;
	  if ( ydimid != UNDEFID ) ysize = ncdims[ydimid].len;

	  if ( ydimid == UNDEFID && yvarid != UNDEFID )
	    {
	      if ( ncvars[yvarid].ndims == 1 )
		{
		  ydimid = ncvars[yvarid].dimids[0];
		  ysize  = ncdims[ydimid].len;
		}
	    }

	  if ( ncvars[ncvarid].gridtype == UNDEFID || ncvars[ncvarid].gridtype == GRID_GENERIC )
	    if ( xdimid != UNDEFID && xdimid == ydimid && nydims == 0 ) ncvars[ncvarid].gridtype = GRID_UNSTRUCTURED;

          if (CDI_netcdf_lazy_grid_load)
            {
              cdfLazyGridRenew(&lazyGrid, ncvars[ncvarid].gridtype);
              cdfLazyGridRenew(&lazyProj, GRID_PROJECTION);
            }
          else
            {
              cdfBaseGridRenew(&lazyGrid, ncvars[ncvarid].gridtype);
              cdfBaseGridRenew(&lazyProj, GRID_PROJECTION);
            }

	  grid->prec  = DATATYPE_FLT64;
	  grid->trunc = ncvars[ncvarid].truncation;

	  if ( ncvars[ncvarid].gridtype == GRID_TRAJECTORY )
	    {
	      if ( ncvars[ncvarid].xvarid == UNDEFID )
		Error("Longitude coordinate undefined for %s!", ncvars[ncvarid].name);
	      if ( ncvars[ncvarid].yvarid == UNDEFID )
		Error("Latitude coordinate undefined for %s!", ncvars[ncvarid].name);
	    }
	  else
	    {
	      size_t start[3], count[3];
	      int ltgrid = FALSE;

	      if ( xvarid != UNDEFID && yvarid != UNDEFID )
		{
		  if ( ncvars[xvarid].ndims != ncvars[yvarid].ndims )
		    {
		      Warning("Inconsistent grid structure for variable %s!", ncvars[ncvarid].name);
		      ncvars[ncvarid].xvarid = UNDEFID;
		      ncvars[ncvarid].yvarid = UNDEFID;
		      xvarid = UNDEFID;
		      yvarid = UNDEFID;
		    }

		  if ( ncvars[xvarid].ndims > 2 || ncvars[yvarid].ndims > 2 )
		    {
		      if ( ncvars[xvarid].ndims == 3 && ncvars[xvarid].dimids[0] == timedimid &&
			   ncvars[yvarid].ndims == 3 && ncvars[yvarid].dimids[0] == timedimid )
			{
			  if ( ltwarn )
			    Warning("Time varying grids unsupported, using grid at time step 1!");
			  ltgrid = TRUE;
			  ltwarn = FALSE;
			  start[0] = start[1] = start[2] = 0;
			  count[0] = 1; count[1] = ysize; count[2] = xsize;
			}
		      else
			{
			  Warning("Unsupported grid structure for variable %s (grid dims > 2)!", ncvars[ncvarid].name);
			  ncvars[ncvarid].xvarid = UNDEFID;
			  ncvars[ncvarid].yvarid = UNDEFID;
			  xvarid = UNDEFID;
			  yvarid = UNDEFID;
			}
		    }
		}

              if ( xvarid != UNDEFID )
                {
                  if ( ncvars[xvarid].ndims > 3 || (ncvars[xvarid].ndims == 3 && ltgrid == FALSE) )
                    {
                      Warning("Coordinate variable %s has to many dimensions (%d), skipped!", ncvars[xvarid].name, ncvars[xvarid].ndims);
                      //ncvars[ncvarid].xvarid = UNDEFID;
                      xvarid = UNDEFID;
                    }
                }

              if ( yvarid != UNDEFID )
                {
                  if ( ncvars[yvarid].ndims > 3 || (ncvars[yvarid].ndims == 3 && ltgrid == FALSE) )
                    {
                      Warning("Coordinate variable %s has to many dimensions (%d), skipped!", ncvars[yvarid].name, ncvars[yvarid].ndims);
                      //ncvars[ncvarid].yvarid = UNDEFID;
                      yvarid = UNDEFID;
                    }
                }

              if ( xvarid != UNDEFID )
		{
                  bool skipvar = true;
		  islon = ncvars[xvarid].islon;
		  ndims = ncvars[xvarid].ndims;
		  if ( ndims == 2 || ndims == 3 )
		    {
		      ncvars[ncvarid].gridtype = GRID_CURVILINEAR;
		      size = xsize*ysize;
		      /* Check size of 2 dimensional coordinate variables */
                      int dimid = ncvars[xvarid].dimids[ndims-2];
                      size_t dimsize1 = ncdims[dimid].len;
                      dimid = ncvars[xvarid].dimids[ndims-1];
                      size_t dimsize2 = ncdims[dimid].len;
                      skipvar = dimsize1*dimsize2 != size;
		    }
		  else if ( ndims == 1 )
		    {
		      size = xsize;
		      /* Check size of 1 dimensional coordinate variables */
                      int dimid = ncvars[xvarid].dimids[0];
                      size_t dimsize = ncdims[dimid].len;
                      skipvar = dimsize != size;
		    }
		  else if ( ndims == 0 && xsize == 0 )
		    {
                      size = xsize = 1;
                      skipvar = false;
		    }

                  if ( skipvar )
                    {
                      Warning("Unsupported array structure, skipped variable %s!", ncvars[ncvarid].name);
                      ncvars[ncvarid].isvar = -1;
                      continue;
                    }

		  if ( ncvars[xvarid].xtype == NC_FLOAT ) grid->prec = DATATYPE_FLT32;
                  if (CDI_netcdf_lazy_grid_load)
                    {
                      lazyGrid->xValsGet = (struct xyValGet){
                        .scalefactor = ncvars[xvarid].scalefactor,
                        .addoffset = ncvars[xvarid].addoffset,
                        .start = { start[0], start[1], start[2] },
                        .count = { count[0], count[1], count[2] },
                        .size = size,
                        .datasetNCId = ncvars[xvarid].ncid,
                        .varNCId = xvarid,
                        .ndims = (short)ndims,
                      };
                      grid->xvals = cdfPendingLoad;
                    }
                  else
                    {
                      grid->xvals = (double *) Malloc(size*sizeof(double));
                      if ( ltgrid )
                        cdf_get_vara_double(ncvars[xvarid].ncid, xvarid,
                                            start, count, grid->xvals);
                      else
                        cdf_get_var_double(ncvars[xvarid].ncid, xvarid,
                                           grid->xvals);
                      scale_add(size, grid->xvals,
                                ncvars[xvarid].addoffset,
                                ncvars[xvarid].scalefactor);
                    }
		  strcpy(grid->xname, ncvars[xvarid].name);
		  strcpy(grid->xlongname, ncvars[xvarid].longname);
		  strcpy(grid->xunits, ncvars[xvarid].units);
		  /* don't change the name !!! */
		  /*
		  if ( (len = strlen(grid->xname)) > 2 )
		    if ( grid->xname[len-2] == '_' && isdigit((int) grid->xname[len-1]) )
		      grid->xname[len-2] = 0;
		  */
		}

	      if ( yvarid != UNDEFID )
		{
                  bool skipvar = true;
		  islat = ncvars[yvarid].islat;
		  ndims = ncvars[yvarid].ndims;
		  if ( ndims == 2 || ndims == 3 )
		    {
		      ncvars[ncvarid].gridtype = GRID_CURVILINEAR;
		      size = xsize*ysize;
		      /* Check size of 2 dimensional coordinate variables */
		      {
			int dimid;
			size_t dimsize1, dimsize2;
			dimid = ncvars[yvarid].dimids[ndims-2];
			dimsize1 = ncdims[dimid].len;
			dimid = ncvars[yvarid].dimids[ndims-1];
			dimsize2 = ncdims[dimid].len;
			skipvar = dimsize1*dimsize2 != size;
		      }
		    }
		  else if ( ndims == 1 )
		    {
		      if ( (int) ysize == 0 ) size = xsize;
		      else                    size = ysize;

		      /* Check size of 1 dimensional coordinate variables */
		      {
			int dimid;
			size_t dimsize;
			dimid = ncvars[yvarid].dimids[0];
			dimsize = ncdims[dimid].len;
			skipvar = dimsize != size;
		      }
		    }
		  else if ( ndims == 0 && ysize == 0 )
		    {
                      size = ysize = 1;
                      skipvar = false;
		    }

                  if ( skipvar )
                    {
                      Warning("Unsupported array structure, skipped variable %s!", ncvars[ncvarid].name);
                      ncvars[ncvarid].isvar = -1;
                      continue;
                    }

		  if ( ncvars[yvarid].xtype == NC_FLOAT ) grid->prec = DATATYPE_FLT32;
                  /* see below for when it's impossible to operate
                   * without y values */
                  if ( !CDI_netcdf_lazy_grid_load
                       || ((ncvars[ncvarid].gridtype == UNDEFID ||
                            ncvars[ncvarid].gridtype == GRID_GENERIC)
                           && islat && (islon || xsize == 0)) )
                    {
                      grid->yvals = (double *) Malloc(size*sizeof(double));

                      if ( ltgrid )
                        cdf_get_vara_double(ncvars[yvarid].ncid, yvarid, start, count, grid->yvals);
                      else
                        cdf_get_var_double(ncvars[yvarid].ncid, yvarid, grid->yvals);

                      scale_add(size, grid->yvals, ncvars[yvarid].addoffset, ncvars[yvarid].scalefactor);

                      /* don't change the name !!! */
                      /*
                        if ( (len = strlen(grid->yname)) > 2 )
                        if ( grid->yname[len-2] == '_' && isdigit((int) grid->yname[len-1]) )
                        grid->yname[len-2] = 0;
                      */
                      if ( islon && (int) ysize > 1 )
                        {
                          yinc = fabs(grid->yvals[0] - grid->yvals[1]);
                          for ( size_t i = 2; i < ysize; i++ )
                            if ( (fabs(grid->yvals[i-1] - grid->yvals[i]) - yinc) > (yinc/1000) )
                              {
                                yinc = 0;
                                break;
                              }
                        }
                    }
                  else
                    {
                      lazyGrid->yValsGet = (struct xyValGet){
                        .scalefactor = ncvars[yvarid].scalefactor,
                        .addoffset = ncvars[yvarid].addoffset,
                        .start = { start[0], start[1], start[2] },
                        .count = { count[0], count[1], count[2] },
                        .size = size,
                        .datasetNCId = ncvars[yvarid].ncid,
                        .varNCId = yvarid,
                        .ndims = (short)ndims,
                      };
                      grid->yvals = cdfPendingLoad;
                    }
                  strcpy(grid->yname, ncvars[yvarid].name);
                  strcpy(grid->ylongname, ncvars[yvarid].longname);
                  strcpy(grid->yunits, ncvars[yvarid].units);
		}

	      if      ( (int) ysize == 0 ) size = xsize;
	      else if ( (int) xsize == 0 ) size = ysize;
	      else if ( ncvars[ncvarid].gridtype == GRID_UNSTRUCTURED ) size = xsize;
	      else                         size = xsize*ysize;
	    }

	  if ( ncvars[ncvarid].gridtype == UNDEFID ||
	       ncvars[ncvarid].gridtype == GRID_GENERIC )
	    {
	      if ( islat && (islon || xsize == 0) )
		{
		  if ( isGaussGrid(ysize, yinc, grid->yvals) )
                    {
                      ncvars[ncvarid].gridtype = GRID_GAUSSIAN;
                      grid->np = (int)(ysize/2);
                    }
                  else
		    ncvars[ncvarid].gridtype = GRID_LONLAT;
		}
	      else if ( islon && !islat && ysize == 0 )
		{
		  ncvars[ncvarid].gridtype = GRID_LONLAT;
		}
	      else
		ncvars[ncvarid].gridtype = GRID_GENERIC;
	    }

	  switch (ncvars[ncvarid].gridtype)
	    {
	    case GRID_GENERIC:
	    case GRID_LONLAT:
	    case GRID_GAUSSIAN:
	    case GRID_UNSTRUCTURED:
	    case GRID_CURVILINEAR:
	      {
		grid->size  = (int)size;
		grid->xsize = (int)xsize;
		grid->ysize = (int)ysize;
		if ( xvarid != UNDEFID )
		  {
		    grid->xdef  = 1;
		    if ( ncvars[xvarid].bounds != UNDEFID )
		      {
			int nbdims = ncvars[ncvars[xvarid].bounds].ndims;
			if ( nbdims == 2 || nbdims == 3 )
			  {
                            vdimid = ncvars[ncvars[xvarid].bounds].dimids[nbdims-1];
			    size_t nvertex = ncdims[vdimid].len;
			    grid->nvertex = (int)nvertex;
                            if (CDI_netcdf_lazy_grid_load)
                              {
                                lazyGrid->xBoundsGet.datasetNCId
                                  = ncvars[xvarid].ncid;
                                lazyGrid->xBoundsGet.varNCId
                                  = ncvars[xvarid].bounds;
                                grid->xbounds = cdfPendingLoad;
                              }
                            else
                              {
                                grid->xbounds
                                  = (double *)Malloc(nvertex * size
                                                     * sizeof(double));
                                cdf_get_var_double(ncvars[xvarid].ncid,
                                                   ncvars[xvarid].bounds,
                                                   grid->xbounds);
                              }
			  }
		      }
		  }
		if ( yvarid != UNDEFID )
		  {
		    grid->ydef  = 1;
		    if ( ncvars[yvarid].bounds != UNDEFID )
		      {
			int nbdims = ncvars[ncvars[yvarid].bounds].ndims;
			if ( nbdims == 2 || nbdims == 3 )
			  {
			    /* size_t nvertex = ncdims[ncvars[ncvars[yvarid].bounds].dimids[nbdims-1]].len;
			    if ( nvertex != grid->nvertex )
			      Warning("nvertex problem! nvertex x %d, nvertex y %d",
				      grid->nvertex, (int) nvertex);
			    */
                            if (CDI_netcdf_lazy_grid_load)
                              {
                                lazyGrid->yBoundsGet.datasetNCId
                                  = ncvars[yvarid].ncid;
                                lazyGrid->yBoundsGet.varNCId
                                  = ncvars[yvarid].bounds;
                                grid->ybounds = cdfPendingLoad;
                              }
                            else
                              {
                                vdimid = ncvars[ncvars[yvarid].bounds].dimids[nbdims-1];
                                size_t nvertex = ncdims[vdimid].len;
                                /*
                                  if ( nvertex != grid->nvertex )
                                  Warning("nvertex problem! nvertex x %d, nvertex y %d",
                                  grid->nvertex, (int) nvertex);
                                */
                                grid->ybounds
                                  = (double *)Malloc(nvertex * size
                                                     * sizeof(double));
                                cdf_get_var_double(ncvars[yvarid].ncid,
                                                   ncvars[yvarid].bounds,
                                                   grid->ybounds);
                              }
			  }
		      }
		  }

		if ( ncvars[ncvarid].cellarea != UNDEFID )
                  {
                    if (CDI_netcdf_lazy_grid_load)
                      {
                        grid->area = cdfPendingLoad;
                        lazyGrid->cellAreaGet.datasetNCId
                          = ncvars[ncvarid].ncid;
                        lazyGrid->cellAreaGet.varNCId
                          = ncvars[ncvarid].cellarea;
                      }
                    else
                      {
                        grid->area = (double *) Malloc(size*sizeof(double));
                        cdf_get_var_double(ncvars[ncvarid].ncid,
                                           ncvars[ncvarid].cellarea,
                                           grid->area);
                      }
                  }

		break;
	      }
	    case GRID_SPECTRAL:
	      {
		grid->size = (int)size;
		grid->lcomplex = 1;
		break;
	      }
	    case GRID_FOURIER:
	      {
		grid->size = (int)size;
		break;
	      }
	    case GRID_TRAJECTORY:
	      {
		grid->size = 1;
		break;
	      }
	    }

          if ( grid->type != ncvars[ncvarid].gridtype )
            {
              int gridtype = ncvars[ncvarid].gridtype;
              grid->type = gridtype;
              cdiGridTypeInit(grid, gridtype, grid->size);
            }

	  if ( grid->size == 0 )
	    {
	      if ( (ncvars[ncvarid].ndims == 1 && ncvars[ncvarid].dimtype[0] == T_AXIS) ||
		   (ncvars[ncvarid].ndims == 1 && ncvars[ncvarid].dimtype[0] == Z_AXIS) ||
		   (ncvars[ncvarid].ndims == 2 && ncvars[ncvarid].dimtype[0] == T_AXIS && ncvars[ncvarid].dimtype[1] == Z_AXIS) )
		{
		  grid->type  = GRID_GENERIC;
		  grid->size  = 1;
		  grid->xsize = 0;
		  grid->ysize = 0;
		}
	      else
		{
		  Warning("Variable %s has an unsupported grid, skipped!", ncvars[ncvarid].name);
		  ncvars[ncvarid].isvar = -1;
		  continue;
		}
	    }

	  if ( number_of_grid_used != UNDEFID && (grid->type == UNDEFID || grid->type == GRID_GENERIC) )
            grid->type   = GRID_UNSTRUCTURED;

	  if ( number_of_grid_used != UNDEFID && grid->type == GRID_UNSTRUCTURED )
            grid->number = number_of_grid_used;

	  if ( ncvars[ncvarid].gmapid >= 0 && ncvars[ncvarid].gridtype != GRID_CURVILINEAR )
	    {
              int nvatts;
	      cdf_inq_varnatts(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, &nvatts);

	      for ( int iatt = 0; iatt < nvatts; iatt++ )
		{
                  size_t attlen;
                  char attname[CDI_MAX_NAME];
		  cdf_inq_attname(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, iatt, attname);
		  cdf_inq_attlen(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, &attlen);

		  if ( strcmp(attname, "grid_mapping_name") == 0 )
		    {
                      enum {
                        attstringlen = 8192,
                      };
                      char attstring[attstringlen];

		      cdfGetAttText(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, attstringlen, attstring);
		      strtolower(attstring);

		      if ( strcmp(attstring, "rotated_latitude_longitude") == 0 )
			grid->isRotated = TRUE;
		      else if ( strcmp(attstring, "sinusoidal") == 0 )
			grid->type = GRID_SINUSOIDAL;
		      else if ( strcmp(attstring, "lambert_azimuthal_equal_area") == 0 )
			grid->type = GRID_LAEA;
		      else if ( strcmp(attstring, "lambert_conformal_conic") == 0 )
			grid->type = GRID_LCC2;
		      else if ( strcmp(attstring, "lambert_cylindrical_equal_area") == 0 )
			{
			  proj->type = GRID_PROJECTION;
			  proj->name = strdup(attstring);
			}
		    }
		  else if ( strcmp(attname, "earth_radius") == 0 )
		    {
                      double datt;
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &datt);
		      grid->laea_a = datt;
		      grid->lcc2_a = datt;
		    }
		  else if ( strcmp(attname, "longitude_of_projection_origin") == 0 )
		    {
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &grid->laea_lon_0);
		    }
		  else if ( strcmp(attname, "longitude_of_central_meridian") == 0 )
		    {
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &grid->lcc2_lon_0);
		    }
		  else if ( strcmp(attname, "latitude_of_projection_origin") == 0 )
		    {
                      double datt;
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &datt);
		      grid->laea_lat_0 = datt;
		      grid->lcc2_lat_0 = datt;
		    }
		  else if ( strcmp(attname, "standard_parallel") == 0 )
		    {
		      if ( attlen == 1 )
			{
                          double datt;
			  cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &datt);
			  grid->lcc2_lat_1 = datt;
			  grid->lcc2_lat_2 = datt;
			}
		      else
			{
			  double datt2[2];
			  cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 2, datt2);
			  grid->lcc2_lat_1 = datt2[0];
			  grid->lcc2_lat_2 = datt2[1];
			}
		    }
		  else if ( strcmp(attname, "grid_north_pole_latitude") == 0 )
		    {
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &grid->ypole);
		    }
		  else if ( strcmp(attname, "grid_north_pole_longitude") == 0 )
		    {
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &grid->xpole);
		    }
		  else if ( strcmp(attname, "north_pole_grid_longitude") == 0 )
		    {
		      cdfGetAttDouble(ncvars[ncvarid].ncid, ncvars[ncvarid].gmapid, attname, 1, &grid->angle);
		    }
		}
	    }

          if ( grid->type == GRID_UNSTRUCTURED )
            {
              int zdimid = UNDEFID;
              int xdimidx = -1, ydimidx = -1;

              for ( int i = 0; i < ndims; i++ )
                {
                  if      ( ncvars[ncvarid].dimtype[i] == X_AXIS ) xdimidx = i;
                  else if ( ncvars[ncvarid].dimtype[i] == Y_AXIS ) ydimidx = i;
                  else if ( ncvars[ncvarid].dimtype[i] == Z_AXIS ) zdimid = ncvars[ncvarid].dimids[i];
                }

              if ( xdimid != UNDEFID && ydimid != UNDEFID && zdimid == UNDEFID )
                {
                  if ( grid->xsize > grid->ysize && grid->ysize < 1000 )
                    {
                      ncvars[ncvarid].dimtype[ydimidx] = Z_AXIS;
                      ydimid = UNDEFID;
                      grid->size  = grid->xsize;
                      grid->ysize = 0;
                    }
                  else if ( grid->ysize > grid->xsize && grid->xsize < 1000 )
                    {
                      ncvars[ncvarid].dimtype[xdimidx] = Z_AXIS;
                      xdimid = ydimid;
                      ydimid = UNDEFID;
                      grid->size  = grid->ysize;
                      grid->xsize = grid->ysize;
                      grid->ysize = 0;
                    }
                }

              if ( grid->size != grid->xsize )
                {
                  Warning("Unsupported array structure, skipped variable %s!", ncvars[ncvarid].name);
                  ncvars[ncvarid].isvar = -1;
                  continue;
                }

              if ( ncvars[ncvarid].position > 0 ) grid->position = ncvars[ncvarid].position;
              if ( uuidOfHGrid[0] != 0 ) memcpy(grid->uuid, uuidOfHGrid, 16);
            }

#if defined (PROJECTION_TEST)
	  if ( proj->type == GRID_PROJECTION )
	    {
	      if ( grid->type == GRID_GENERIC )
		{
		  grid->type = GRID_CURVILINEAR;
		}

	      if ( grid->type == GRID_CURVILINEAR )
		{
                  proj->size  = grid->size;
                  proj->xsize = grid->xsize;
                  proj->ysize = grid->ysize;
		}

	      //  grid->proj = gridGenerate(proj);
	    }
#endif

	  if ( CDI_Debug )
	    {
	      Message("grid: type = %d, size = %d, nx = %d, ny %d",
		      grid->type, grid->size, grid->xsize, grid->ysize);
	      Message("proj: type = %d, size = %d, nx = %d, ny %d",
		      proj->type, proj->size, proj->xsize, proj->ysize);
	    }

#if defined (PROJECTION_TEST)
	  if ( proj->type == GRID_PROJECTION )
	    {
              projAdded = cdiVlistAddGridIfNew(vlistID, proj, 1);
              ncvars[ncvarid].gridID = projAdded.Id;
	      copy_numeric_projatts(ncvars[ncvarid].gridID, ncvars[ncvarid].gmapid, ncvars[ncvarid].ncid);
	    }
	  else
#endif
            {
              gridAdded = cdiVlistAddGridIfNew(vlistID, grid, 1);
              ncvars[ncvarid].gridID = gridAdded.Id;
            }

          if ( grid->type == GRID_UNSTRUCTURED )
            {
              if ( gridfile[0] != 0 ) gridDefReference(ncvars[ncvarid].gridID, gridfile);
            }

          if ( ncvars[ncvarid].chunked ) grid_set_chunktype(grid, &ncvars[ncvarid]);

	  int gridindex = vlistGridIndex(vlistID, ncvars[ncvarid].gridID);
	  streamptr->xdimID[gridindex] = xdimid;
	  streamptr->ydimID[gridindex] = ydimid;
          if ( xdimid == -1 && ydimid == -1 && grid->size == 1 )
            gridDefHasDims(ncvars[ncvarid].gridID, FALSE);

          if ( xdimid != -1 )
            cdiGridDefString(ncvars[ncvarid].gridID, CDI_GRID_XDIMNAME, (int)(strlen(ncdims[xdimid].name)+1), ncdims[xdimid].name);
          if ( ydimid != -1 )
            cdiGridDefString(ncvars[ncvarid].gridID, CDI_GRID_YDIMNAME, (int)(strlen(ncdims[ydimid].name)+1), ncdims[ydimid].name);
          if ( vdimid != -1 )
            cdiGridDefString(ncvars[ncvarid].gridID, CDI_GRID_VDIMNAME, (int)(strlen(ncdims[vdimid].name)+1), ncdims[vdimid].name);

	  if ( CDI_Debug )
	    Message("gridID %d %d %s", ncvars[ncvarid].gridID, ncvarid, ncvars[ncvarid].name);

	  for ( int ncvarid2 = ncvarid+1; ncvarid2 < nvars; ncvarid2++ )
	    if ( ncvars[ncvarid2].isvar == TRUE && ncvars[ncvarid2].gridID == UNDEFID )
	      {
		int xdimid2 = UNDEFID, ydimid2 = UNDEFID, zdimid2 = UNDEFID;
                int xdimidx = -1, ydimidx = -1;
		int ndims2 = ncvars[ncvarid2].ndims;

		for ( int i = 0; i < ndims2; i++ )
		  {
		    if ( ncvars[ncvarid2].dimtype[i] == X_AXIS )
		      { xdimid2 = ncvars[ncvarid2].dimids[i]; xdimidx = i; }
		    else if ( ncvars[ncvarid2].dimtype[i] == Y_AXIS )
		      { ydimid2 = ncvars[ncvarid2].dimids[i]; ydimidx = i; }
		    else if ( ncvars[ncvarid2].dimtype[i] == Z_AXIS )
		      { zdimid2 = ncvars[ncvarid2].dimids[i]; }
		  }

                if ( ncvars[ncvarid2].gridtype == UNDEFID && grid->type == GRID_UNSTRUCTURED )
                  {
                    if ( xdimid == xdimid2 && ydimid2 != UNDEFID && zdimid2 == UNDEFID )
                      {
                        ncvars[ncvarid2].dimtype[ydimidx] = Z_AXIS;
                        ydimid2 = UNDEFID;
                      }

                    if ( xdimid == ydimid2 && xdimid2 != UNDEFID && zdimid2 == UNDEFID )
                      {
                        ncvars[ncvarid2].dimtype[xdimidx] = Z_AXIS;
                        xdimid2 = ydimid2;
                        ydimid2 = UNDEFID;
                      }
                  }

                if ( xdimid == xdimid2 &&
		    (ydimid == ydimid2 || (xdimid == ydimid && ydimid2 == UNDEFID)) )
		  {
		    int same_grid = ncvars[ncvarid].xvarid == ncvars[ncvarid2].xvarid
                      && ncvars[ncvarid].yvarid == ncvars[ncvarid2].yvarid
                      && ncvars[ncvarid].position == ncvars[ncvarid2].position;
                    /*
		    if ( xvarid != -1 && ncvars[ncvarid2].xvarid != UNDEFID &&
			 xvarid != ncvars[ncvarid2].xvarid ) same_grid = FALSE;

		    if ( yvarid != -1 && ncvars[ncvarid2].yvarid != UNDEFID &&
			 yvarid != ncvars[ncvarid2].yvarid ) same_grid = FALSE;
                    */

		    if ( same_grid )
		      {
			if ( CDI_Debug )
			  Message("Same gridID %d %d %s", ncvars[ncvarid].gridID, ncvarid2, ncvars[ncvarid2].name);
			ncvars[ncvarid2].gridID = ncvars[ncvarid].gridID;
			ncvars[ncvarid2].chunktype = ncvars[ncvarid].chunktype;
		      }
		  }
	      }

          if (gridAdded.isNew)
            lazyGrid = NULL;
          if (projAdded.isNew)
            lazyProj = NULL;
	}
    }
  if (lazyGrid)
    {
      if (CDI_netcdf_lazy_grid_load) cdfLazyGridDestroy(lazyGrid);
      grid_free(grid);
      Free(grid);
    }
  if (lazyProj)
    {
      if (CDI_netcdf_lazy_grid_load) cdfLazyGridDestroy(lazyProj);
      grid_free(proj);
      Free(proj);
    }
#undef proj
#undef grid
}

/* define all input zaxes */
static
void define_all_zaxes(stream_t *streamptr, int vlistID, ncdim_t *ncdims, int nvars, ncvar_t *ncvars,
		      size_t vctsize_echam, double *vct_echam, unsigned char *uuidOfVGrid)
{
  int ncvarid, ncvarid2;
  int i, ilev;
  int zaxisindex;
  int nbdims, nvertex, nlevel;
  int psvarid = -1;
  char *pname, *plongname, *punits;
  size_t vctsize = vctsize_echam;
  double *vct = vct_echam;

  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].isvar == TRUE && ncvars[ncvarid].zaxisID == UNDEFID )
	{
          int is_scalar = FALSE;
	  int with_bounds = FALSE;
	  int zdimid = UNDEFID;
	  int zvarid = UNDEFID;
	  int zsize = 1;
	  double *lbounds = NULL;
	  double *ubounds = NULL;

          int positive = 0;
	  int ndims = ncvars[ncvarid].ndims;

          if ( ncvars[ncvarid].zvarid != -1 && ncvars[ncvars[ncvarid].zvarid].ndims == 0 )
            {
              zvarid = ncvars[ncvarid].zvarid;
              is_scalar = TRUE;
            }
          else
            {
              for ( i = 0; i < ndims; i++ )
                {
                  if ( ncvars[ncvarid].dimtype[i] == Z_AXIS )
                    zdimid = ncvars[ncvarid].dimids[i];
                }

              if ( zdimid != UNDEFID )
                {
                  zvarid = ncdims[zdimid].ncvarid;
                  zsize  = (int)ncdims[zdimid].len;
                }
            }

	  if ( CDI_Debug ) Message("nlevs = %d", zsize);

	  double *zvar = (double *) Malloc((size_t)zsize * sizeof (double));

	  int zaxisType = UNDEFID;
	  if ( zvarid != UNDEFID ) zaxisType = ncvars[zvarid].zaxistype;
	  if ( zaxisType == UNDEFID )  zaxisType = ZAXIS_GENERIC;

	  int zprec = DATATYPE_FLT64;

	  if ( zvarid != UNDEFID )
	    {
	      positive  = ncvars[zvarid].positive;
	      pname     = ncvars[zvarid].name;
	      plongname = ncvars[zvarid].longname;
	      punits    = ncvars[zvarid].units;
	      if ( ncvars[zvarid].xtype == NC_FLOAT ) zprec = DATATYPE_FLT32;
	      /* don't change the name !!! */
	      /*
	      if ( (len = strlen(pname)) > 2 )
		if ( pname[len-2] == '_' && isdigit((int) pname[len-1]) )
		  pname[len-2] = 0;
	      */
              psvarid = -1;
              if ( zaxisType == ZAXIS_HYBRID && ncvars[zvarid].vct )
                {
                  vct = ncvars[zvarid].vct;
                  vctsize = ncvars[zvarid].vctsize;

                  if ( ncvars[zvarid].psvarid != -1 ) psvarid = ncvars[zvarid].psvarid;
                }

	      cdf_get_var_double(ncvars[zvarid].ncid, zvarid, zvar);

	      if ( ncvars[zvarid].bounds != UNDEFID )
		{
		  nbdims = ncvars[ncvars[zvarid].bounds].ndims;
		  if ( nbdims == 2 )
		    {
		      nlevel  = (int)ncdims[ncvars[ncvars[zvarid].bounds].dimids[0]].len;
		      nvertex = (int)ncdims[ncvars[ncvars[zvarid].bounds].dimids[1]].len;
		      if ( nlevel == zsize && nvertex == 2 )
			{
			  with_bounds = TRUE;
			  lbounds = (double *) Malloc((size_t)nlevel*sizeof(double));
			  ubounds = (double *) Malloc((size_t)nlevel*sizeof(double));
			  double zbounds[2*nlevel];
			  cdf_get_var_double(ncvars[zvarid].ncid, ncvars[zvarid].bounds, zbounds);
			  for ( i = 0; i < nlevel; ++i )
			    {
			      lbounds[i] = zbounds[i*2];
			      ubounds[i] = zbounds[i*2+1];
			    }
			}
		    }
		}
	    }
	  else
	    {
	      pname     = NULL;
	      plongname = NULL;
	      punits    = NULL;

	      if ( zsize == 1 )
		{
                  if ( ncvars[ncvarid].zaxistype != UNDEFID )
                    zaxisType = ncvars[ncvarid].zaxistype;
                  else
                    zaxisType = ZAXIS_SURFACE;

		  zvar[0] = 0;
		  /*
		  if ( zdimid == UNDEFID )
		    zvar[0] = 9999;
		  else
		    zvar[0] = 0;
		  */
		}
	      else
		{
		  for ( ilev = 0; ilev < zsize; ilev++ ) zvar[ilev] = ilev + 1;
		}
	    }

      	  ncvars[ncvarid].zaxisID = varDefZaxis(vlistID, zaxisType, (int) zsize, zvar, with_bounds, lbounds, ubounds,
						(int)vctsize, vct, pname, plongname, punits, zprec, 1, 0);

	  if ( uuidOfVGrid[0] != 0 )
            {
              // printf("uuidOfVGrid: defined\n");
              zaxisDefUUID(ncvars[ncvarid].zaxisID, uuidOfVGrid);
            }

          if ( zaxisType == ZAXIS_HYBRID && psvarid != -1 ) zaxisDefPsName(ncvars[ncvarid].zaxisID, ncvars[psvarid].name);

          if ( positive > 0 ) zaxisDefPositive(ncvars[ncvarid].zaxisID, positive);
          if ( is_scalar ) zaxisDefScalar(ncvars[ncvarid].zaxisID);

          if ( zdimid != -1 )
            cdiZaxisDefString(ncvars[ncvarid].zaxisID, CDI_ZAXIS_DIMNAME, (int)(strlen(ncdims[zdimid].name)+1), ncdims[zdimid].name);
          /*
          if ( vdimid != -1 )
            cdiZaxisDefString(ncvars[ncvarid].zaxisID, CDI_ZAXIS_VDIMNAME, strlen(ncdims[vdimid].name)+1, ncdims[vdimid].name);
          */
	  Free(zvar);
	  Free(lbounds);
	  Free(ubounds);

	  zaxisindex = vlistZaxisIndex(vlistID, ncvars[ncvarid].zaxisID);
	  streamptr->zaxisID[zaxisindex]  = zdimid;

	  if ( CDI_Debug )
	    Message("zaxisID %d %d %s", ncvars[ncvarid].zaxisID, ncvarid, ncvars[ncvarid].name);

	  for ( ncvarid2 = ncvarid+1; ncvarid2 < nvars; ncvarid2++ )
	    if ( ncvars[ncvarid2].isvar == TRUE && ncvars[ncvarid2].zaxisID == UNDEFID /*&& ncvars[ncvarid2].zaxistype == UNDEFID*/ )
	      {
                int zvarid2 = UNDEFID;
                if ( ncvars[ncvarid2].zvarid != UNDEFID && ncvars[ncvars[ncvarid2].zvarid].ndims == 0 )
                  zvarid2 = ncvars[ncvarid2].zvarid;

		int zdimid2 = UNDEFID;
		ndims = ncvars[ncvarid2].ndims;
		for ( i = 0; i < ndims; i++ )
		  {
		    if ( ncvars[ncvarid2].dimtype[i] == Z_AXIS )
		      zdimid2 = ncvars[ncvarid2].dimids[i];
		  }

		if ( zdimid == zdimid2 /* && zvarid == zvarid2 */)
		  {
                    if ( (zdimid != UNDEFID && ncvars[ncvarid2].zaxistype == UNDEFID) ||
                         (zdimid == UNDEFID && zvarid != UNDEFID && zvarid == zvarid2) ||
                         (zdimid == UNDEFID && zaxisType == ncvars[ncvarid2].zaxistype) ||
                         (zdimid == UNDEFID && zvarid2 == UNDEFID && ncvars[ncvarid2].zaxistype == UNDEFID) )
                      {
                        if ( CDI_Debug )
                          Message("zaxisID %d %d %s", ncvars[ncvarid].zaxisID, ncvarid2, ncvars[ncvarid2].name);
                        ncvars[ncvarid2].zaxisID = ncvars[ncvarid].zaxisID;
                      }
                  }
	      }
	}
    }
}

struct varinfo
{
  int      ncvarid;
  const char *name;
};

static
int cmpvarname(const void *s1, const void *s2)
{
  const struct varinfo *x = (const struct varinfo *)s1,
    *y = (const struct varinfo *)s2;
  return (strcmp(x->name, y->name));
}

/* define all input data variables */
static
void define_all_vars(stream_t *streamptr, int vlistID, int instID, int modelID, int *varids, int nvars, int num_ncvars, ncvar_t *ncvars)
{
  if ( CDI_Debug )
    {
      for(int i = 0; i < nvars; i++) Message("varids[%d] = %d", i, varids[i]);
    }
  if ( streamptr->sortname )
    {
      struct varinfo *varInfo
        = (struct varinfo *) Malloc((size_t)nvars * sizeof (struct varinfo));

      for ( int varID = 0; varID < nvars; varID++ )
	{
	  int ncvarid = varids[varID];
	  varInfo[varID].ncvarid = ncvarid;
	  varInfo[varID].name = ncvars[ncvarid].name;
	}
      qsort(varInfo, (size_t)nvars, sizeof(varInfo[0]), cmpvarname);
      for ( int varID = 0; varID < nvars; varID++ )
	{
	  varids[varID] = varInfo[varID].ncvarid;
	}
      Free(varInfo);
      if ( CDI_Debug )
        {
          for(int i = 0; i < nvars; i++) Message("sorted varids[%d] = %d", i, varids[i]);
        }
    }

  for ( int varID1 = 0; varID1 < nvars; varID1++ )
    {
      int ncvarid = varids[varID1];
      int gridID  = ncvars[ncvarid].gridID;
      int zaxisID = ncvars[ncvarid].zaxisID;

      stream_new_var(streamptr, gridID, zaxisID, CDI_UNDEFID);
      int varID = vlistDefVar(vlistID, gridID, zaxisID, ncvars[ncvarid].tsteptype);

#if  defined  (HAVE_NETCDF4)
      if ( ncvars[ncvarid].deflate )
	vlistDefVarCompType(vlistID, varID, COMPRESS_ZIP);

      if ( ncvars[ncvarid].chunked && ncvars[ncvarid].chunktype != UNDEFID )
        vlistDefVarChunkType(vlistID, varID, ncvars[ncvarid].chunktype);
#endif

      streamptr->vars[varID1].defmiss = 0;
      streamptr->vars[varID1].ncvarid = ncvarid;

      vlistDefVarName(vlistID, varID, ncvars[ncvarid].name);
      if ( ncvars[ncvarid].param != UNDEFID ) vlistDefVarParam(vlistID, varID, ncvars[ncvarid].param);
      if ( ncvars[ncvarid].code != UNDEFID )  vlistDefVarCode(vlistID, varID, ncvars[ncvarid].code);
      if ( ncvars[ncvarid].code != UNDEFID )
	{
	  int param = cdiEncodeParam(ncvars[ncvarid].code, ncvars[ncvarid].tabnum, 255);
	  vlistDefVarParam(vlistID, varID, param);
	}
      if ( ncvars[ncvarid].longname[0] )  vlistDefVarLongname(vlistID, varID, ncvars[ncvarid].longname);
      if ( ncvars[ncvarid].stdname[0] )   vlistDefVarStdname(vlistID, varID, ncvars[ncvarid].stdname);
      if ( ncvars[ncvarid].units[0] )     vlistDefVarUnits(vlistID, varID, ncvars[ncvarid].units);

      if ( ncvars[ncvarid].lvalidrange )
        vlistDefVarValidrange(vlistID, varID, ncvars[ncvarid].validrange);

      if ( IS_NOT_EQUAL(ncvars[ncvarid].addoffset, 0) )
	vlistDefVarAddoffset(vlistID, varID, ncvars[ncvarid].addoffset);
      if ( IS_NOT_EQUAL(ncvars[ncvarid].scalefactor, 1) )
	vlistDefVarScalefactor(vlistID, varID, ncvars[ncvarid].scalefactor);

      vlistDefVarDatatype(vlistID, varID, cdfInqDatatype(ncvars[ncvarid].xtype, ncvars[ncvarid].lunsigned));

      vlistDefVarInstitut(vlistID, varID, instID);
      vlistDefVarModel(vlistID, varID, modelID);
      if ( ncvars[ncvarid].tableID != UNDEFID )
	vlistDefVarTable(vlistID, varID, ncvars[ncvarid].tableID);

      if ( ncvars[ncvarid].deffillval == FALSE && ncvars[ncvarid].defmissval == TRUE )
        {
          ncvars[ncvarid].deffillval = TRUE;
          ncvars[ncvarid].fillval    = ncvars[ncvarid].missval;
        }

      if ( ncvars[ncvarid].deffillval == TRUE )
        vlistDefVarMissval(vlistID, varID, ncvars[ncvarid].fillval);

      if ( CDI_Debug )
	Message("varID = %d  gridID = %d  zaxisID = %d", varID,
		vlistInqVarGrid(vlistID, varID), vlistInqVarZaxis(vlistID, varID));

      int gridindex = vlistGridIndex(vlistID, gridID);
      int xdimid = streamptr->xdimID[gridindex];
      int ydimid = streamptr->ydimID[gridindex];

      int zaxisindex = vlistZaxisIndex(vlistID, zaxisID);
      int zdimid = streamptr->zaxisID[zaxisindex];

      int ndims = ncvars[ncvarid].ndims;
      int iodim = 0;
      int ixyz = 0;
      int ipow10[4] = {1, 10, 100, 1000};

      if ( ncvars[ncvarid].tsteptype != TSTEP_CONSTANT ) iodim++;

      if ( gridInqType(gridID) == GRID_UNSTRUCTURED && ndims-iodim <= 2 && ydimid == xdimid )
        {
          if ( xdimid == ncvars[ncvarid].dimids[ndims-1] )
            {
              ixyz = 321;
            }
          else
            {
              ixyz = 213;
            }
        }
      else
        {
          for ( int idim = iodim; idim < ndims; idim++ )
            {
              if      ( xdimid == ncvars[ncvarid].dimids[idim] )
                ixyz += 1*ipow10[ndims-idim-1];
              else if ( ydimid == ncvars[ncvarid].dimids[idim] )
                ixyz += 2*ipow10[ndims-idim-1];
              else if ( zdimid == ncvars[ncvarid].dimids[idim] )
                ixyz += 3*ipow10[ndims-idim-1];
            }
        }

      vlistDefVarXYZ(vlistID, varID, ixyz);
      /*
      printf("ixyz %d\n", ixyz);
      printf("ndims %d\n", ncvars[ncvarid].ndims);
      for ( int i = 0; i < ncvars[ncvarid].ndims; ++i )
        printf("dimids: %d %d\n", i, ncvars[ncvarid].dimids[i]);
      printf("xdimid, ydimid %d %d\n", xdimid, ydimid);
      */
      if ( ncvars[ncvarid].ensdata != NULL )
        {
          vlistDefVarEnsemble( vlistID, varID, ncvars[ncvarid].ensdata->ens_index,
                               ncvars[ncvarid].ensdata->ens_count,
                               ncvars[ncvarid].ensdata->forecast_init_type );
          Free(ncvars[ncvarid].ensdata);
          ncvars[ncvarid].ensdata = NULL;
        }

      if ( ncvars[ncvarid].extra[0] != 0 )
        {
          vlistDefVarExtra(vlistID, varID, ncvars[ncvarid].extra);
        }
    }

  for ( int varID = 0; varID < nvars; varID++ )
    {
      int ncvarid = varids[varID];
      int ncid = ncvars[ncvarid].ncid;

      if ( ncvars[ncvarid].natts )
	{
	  int attnum;
	  int iatt;
	  nc_type attrtype;
	  size_t attlen;
	  char attname[CDI_MAX_NAME];
	  const int attstringlen = 8192; char attstring[8192];
	  int nvatts = ncvars[ncvarid].natts;

	  for ( iatt = 0; iatt < nvatts; iatt++ )
	    {
	      attnum = ncvars[ncvarid].atts[iatt];
	      cdf_inq_attname(ncid, ncvarid, attnum, attname);
	      cdf_inq_attlen(ncid, ncvarid, attname, &attlen);
	      cdf_inq_atttype(ncid, ncvarid, attname, &attrtype);

	      if ( attrtype == NC_SHORT || attrtype == NC_INT )
		{
		  int attint[attlen];
		  cdfGetAttInt(ncid, ncvarid, attname, (int)attlen, attint);
		  if ( attrtype == NC_SHORT )
		    vlistDefAttInt(vlistID, varID, attname, DATATYPE_INT16, (int)attlen, attint);
		  else
		    vlistDefAttInt(vlistID, varID, attname, DATATYPE_INT32, (int)attlen, attint);
		}
	      else if ( attrtype == NC_FLOAT || attrtype == NC_DOUBLE )
		{
		  double attflt[attlen];
		  cdfGetAttDouble(ncid, ncvarid, attname, (int)attlen, attflt);
		  if ( attrtype == NC_FLOAT )
		    vlistDefAttFlt(vlistID, varID, attname, DATATYPE_FLT32, (int)attlen, attflt);
		  else
		    vlistDefAttFlt(vlistID, varID, attname, DATATYPE_FLT64, (int)attlen, attflt);
		}
	      else if ( xtypeIsText(attrtype) )
		{
		  cdfGetAttText(ncid, ncvarid, attname, attstringlen, attstring);
		  vlistDefAttTxt(vlistID, varID, attname, (int)attlen, attstring);
		}
	      else
		{
		  if ( CDI_Debug ) printf("att: %s.%s = unknown\n", ncvars[ncvarid].name, attname);
		}
	    }

	  if (ncvars[ncvarid].vct) Free(ncvars[ncvarid].vct);
	  if (ncvars[ncvarid].atts) Free(ncvars[ncvarid].atts);
          ncvars[ncvarid].vct = NULL;
          ncvars[ncvarid].atts = NULL;
	}
    }

  /* release mem of not freed attributes */
  for ( int ncvarid = 0; ncvarid < num_ncvars; ncvarid++ )
    if ( ncvars[ncvarid].atts ) Free(ncvars[ncvarid].atts);

  if ( varids ) Free(varids);

  for ( int varID = 0; varID < nvars; varID++ )
    {
      if ( vlistInqVarCode(vlistID, varID) == -varID-1 )
	{
	  const char *pname = vlistInqVarNamePtr(vlistID, varID);
	  size_t len = strlen(pname);
	  if ( len > 3 && isdigit((int) pname[3]) )
	    {
	      if ( memcmp("var", pname, 3) == 0 )
		{
		  vlistDefVarCode(vlistID, varID, atoi(pname+3));
                  // vlistDestroyVarName(vlistID, varID);
		}
	    }
	  else if ( len > 4 && isdigit((int) pname[4]) )
	    {
	      if ( memcmp("code", pname, 4) == 0 )
		{
		  vlistDefVarCode(vlistID, varID, atoi(pname+4));
		  // vlistDestroyVarName(vlistID, varID);
		}
	    }
	  else if ( len > 5 && isdigit((int) pname[5]) )
	    {
	      if ( memcmp("param", pname, 5) == 0 )
		{
		  int pnum = -1, pcat = 255, pdis = 255;
		  sscanf(pname+5, "%d.%d.%d", &pnum, &pcat, &pdis);
		  vlistDefVarParam(vlistID, varID, cdiEncodeParam(pnum, pcat, pdis));
                  // vlistDestroyVarName(vlistID, varID);
		}
	    }
	}
    }

  for ( int varID = 0; varID < nvars; varID++ )
    {
      int varInstID  = vlistInqVarInstitut(vlistID, varID);
      int varModelID = vlistInqVarModel(vlistID, varID);
      int varTableID = vlistInqVarTable(vlistID, varID);
      int code = vlistInqVarCode(vlistID, varID);
      if ( cdiDefaultTableID != UNDEFID )
	{
	  if ( tableInqParNamePtr(cdiDefaultTableID, code) )
	    {
	      vlistDestroyVarName(vlistID, varID);
	      vlistDestroyVarLongname(vlistID, varID);
	      vlistDestroyVarUnits(vlistID, varID);

	      if ( varTableID != UNDEFID )
		{
		  vlistDefVarName(vlistID, varID, tableInqParNamePtr(cdiDefaultTableID, code));
		  if ( tableInqParLongnamePtr(cdiDefaultTableID, code) )
		    vlistDefVarLongname(vlistID, varID, tableInqParLongnamePtr(cdiDefaultTableID, code));
		  if ( tableInqParUnitsPtr(cdiDefaultTableID, code) )
		    vlistDefVarUnits(vlistID, varID, tableInqParUnitsPtr(cdiDefaultTableID, code));
		}
	      else
		{
		  varTableID = cdiDefaultTableID;
		}
	    }

	  if ( cdiDefaultModelID != UNDEFID ) varModelID = cdiDefaultModelID;
	  if ( cdiDefaultInstID  != UNDEFID ) varInstID  = cdiDefaultInstID;
	}
      if ( varInstID  != UNDEFID ) vlistDefVarInstitut(vlistID, varID, varInstID);
      if ( varModelID != UNDEFID ) vlistDefVarModel(vlistID, varID, varModelID);
      if ( varTableID != UNDEFID ) vlistDefVarTable(vlistID, varID, varTableID);
    }
}

static
void scan_global_attributes(int fileID, int vlistID, stream_t *streamptr, int ngatts, int *instID, int *modelID, int *ucla_les, unsigned char *uuidOfHGrid, unsigned char *uuidOfVGrid, char *gridfile, int *number_of_grid_used)
{
  nc_type xtype;
  size_t attlen;
  char attname[CDI_MAX_NAME];
  enum { attstringlen = 65636 };
  char attstring[attstringlen];
  int iatt;

  for ( iatt = 0; iatt < ngatts; iatt++ )
    {
      cdf_inq_attname(fileID, NC_GLOBAL, iatt, attname);
      cdf_inq_atttype(fileID, NC_GLOBAL, attname, &xtype);
      cdf_inq_attlen(fileID, NC_GLOBAL, attname, &attlen);

      if ( xtypeIsText(xtype) )
	{
	  cdfGetAttText(fileID, NC_GLOBAL, attname, attstringlen, attstring);

          size_t attstrlen = strlen(attstring);

	  if ( attlen > 0 && attstring[0] != 0 )
	    {
	      if ( strcmp(attname, "history") == 0 )
		{
		  streamptr->historyID = iatt;
		}
	      else if ( strcmp(attname, "institution") == 0 )
		{
		  *instID = institutInq(0, 0, NULL, attstring);
		  if ( *instID == UNDEFID )
		    *instID = institutDef(0, 0, NULL, attstring);
		}
	      else if ( strcmp(attname, "source") == 0 )
		{
		  *modelID = modelInq(-1, 0, attstring);
		  if ( *modelID == UNDEFID )
		    *modelID = modelDef(-1, 0, attstring);
		}
	      else if ( strcmp(attname, "Source") == 0 )
		{
		  if ( strncmp(attstring, "UCLA-LES", 8) == 0 )
		    *ucla_les = TRUE;
		}
	      /*
	      else if ( strcmp(attname, "Conventions") == 0 )
		{
		}
	      */
	      else if ( strcmp(attname, "CDI") == 0 )
		{
		}
	      else if ( strcmp(attname, "CDO") == 0 )
		{
		}
              /*
	      else if ( strcmp(attname, "forecast_reference_time") == 0 )
		{
                  memcpy(fcreftime, attstring, attstrlen+1);
		}
              */
	      else if ( strcmp(attname, "grid_file_uri") == 0 )
		{
                  memcpy(gridfile, attstring, attstrlen+1);
		}
	      else if ( strcmp(attname, "uuidOfHGrid") == 0 && attstrlen == 36 )
		{
                  attstring[36] = 0;
                  cdiStr2UUID(attstring, uuidOfHGrid);
                  //   printf("uuid: %d %s\n", attlen, attstring);
		}
	      else if ( strcmp(attname, "uuidOfVGrid") == 0 && attstrlen == 36 )
		{
                  attstring[36] = 0;
                  cdiStr2UUID(attstring, uuidOfVGrid);
		}
	      else
		{
                  if ( strcmp(attname, "ICON_grid_file_uri") == 0 && gridfile[0] == 0 )
                    {
                      memcpy(gridfile, attstring, attstrlen+1);
                    }

		  vlistDefAttTxt(vlistID, CDI_GLOBAL, attname, (int)attstrlen, attstring);
		}
	    }
	}
      else if ( xtype == NC_SHORT || xtype == NC_INT )
	{
	  if ( strcmp(attname, "number_of_grid_used") == 0 )
	    {
	      (*number_of_grid_used) = UNDEFID;
	      cdfGetAttInt(fileID, NC_GLOBAL, attname, 1, number_of_grid_used);
	    }
 	  else
            {
              int attint[attlen];
              cdfGetAttInt(fileID, NC_GLOBAL, attname, (int)attlen, attint);
              if ( xtype == NC_SHORT )
                vlistDefAttInt(vlistID, CDI_GLOBAL, attname, DATATYPE_INT16, (int)attlen, attint);
              else
                vlistDefAttInt(vlistID, CDI_GLOBAL, attname, DATATYPE_INT32, (int)attlen, attint);
            }
        }
      else if ( xtype == NC_FLOAT || xtype == NC_DOUBLE )
	{
	  double attflt[attlen];
	  cdfGetAttDouble(fileID, NC_GLOBAL, attname, (int)attlen, attflt);
	  if ( xtype == NC_FLOAT )
	    vlistDefAttFlt(vlistID, CDI_GLOBAL, attname, DATATYPE_FLT32, (int)attlen, attflt);
	  else
	    vlistDefAttFlt(vlistID, CDI_GLOBAL, attname, DATATYPE_FLT64, (int)attlen, attflt);
	}
    }
}

static
int find_leadtime(int nvars, ncvar_t *ncvars)
{
  int leadtime_id = UNDEFID;

  for ( int ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].stdname[0] )
        {
          if ( strcmp(ncvars[ncvarid].stdname, "forecast_period") == 0 )
            {
              leadtime_id = ncvarid;
              break;
            }
        }
    }

  return (leadtime_id);
}

static
void find_time_vars(int nvars, ncvar_t *ncvars, ncdim_t *ncdims, int timedimid, stream_t *streamptr,
                    int *time_has_units, int *time_has_bounds, int *time_climatology)
{
  int ncvarid;

  if ( timedimid == UNDEFID )
    {
      char timeunits[CDI_MAX_NAME];

      for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
        {
          if ( ncvars[ncvarid].ndims == 0 && strcmp(ncvars[ncvarid].name, "time") == 0 )
            {
              if ( ncvars[ncvarid].units[0] )
                {
                  strcpy(timeunits, ncvars[ncvarid].units);
                  strtolower(timeunits);

                  if ( isTimeUnits(timeunits) )
                    {
                      streamptr->basetime.ncvarid = ncvarid;
                      break;
                    }
                }
            }
        }
    }
  else
    {
      int ltimevar = FALSE;

      if ( ncdims[timedimid].ncvarid != UNDEFID )
        {
          streamptr->basetime.ncvarid = ncdims[timedimid].ncvarid;
          ltimevar = TRUE;
        }

      for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
        if ( ncvarid != streamptr->basetime.ncvarid &&
             ncvars[ncvarid].ndims == 1 &&
             timedimid == ncvars[ncvarid].dimids[0] &&
             !xtypeIsText(ncvars[ncvarid].xtype) &&
             isTimeAxisUnits(ncvars[ncvarid].units) )
          {
            ncvars[ncvarid].isvar = FALSE;

            if ( !ltimevar )
              {
                streamptr->basetime.ncvarid = ncvarid;
                ltimevar = TRUE;
                if ( CDI_Debug )
                  fprintf(stderr, "timevar %s\n", ncvars[ncvarid].name);
              }
            else
              {
                Warning("Found more than one time variable, skipped variable %s!", ncvars[ncvarid].name);
              }
          }

      if ( ltimevar == FALSE ) /* search for WRF time description */
        {
          for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
            if ( ncvarid != streamptr->basetime.ncvarid &&
                 ncvars[ncvarid].ndims == 2 &&
                 timedimid == ncvars[ncvarid].dimids[0] &&
                 xtypeIsText(ncvars[ncvarid].xtype) &&
                 ncdims[ncvars[ncvarid].dimids[1]].len == 19 )
              {
                streamptr->basetime.ncvarid = ncvarid;
                streamptr->basetime.lwrf    = TRUE;
                break;
              }
        }

      /* time varID */
      ncvarid = streamptr->basetime.ncvarid;

      if ( ncvarid == UNDEFID )
        {
          Warning("Time variable >%s< not found!", ncdims[timedimid].name);
        }
    }

  /* time varID */
  ncvarid = streamptr->basetime.ncvarid;

  if ( ncvarid != UNDEFID && streamptr->basetime.lwrf == FALSE )
    {
      if ( ncvars[ncvarid].units[0] != 0 ) *time_has_units = TRUE;

      if ( ncvars[ncvarid].bounds != UNDEFID )
        {
          int nbdims = ncvars[ncvars[ncvarid].bounds].ndims;
          if ( nbdims == 2 )
            {
              int len = (int) ncdims[ncvars[ncvars[ncvarid].bounds].dimids[nbdims-1]].len;
              if ( len == 2 && timedimid == ncvars[ncvars[ncvarid].bounds].dimids[0] )
                {
                  *time_has_bounds = TRUE;
                  streamptr->basetime.ncvarboundsid = ncvars[ncvarid].bounds;
                  if ( ncvars[ncvarid].climatology ) *time_climatology = TRUE;
                }
            }
        }
    }
}

static
void read_vct_echam(int fileID, int nvars, ncvar_t *ncvars, ncdim_t *ncdims, double **vct, size_t *pvctsize)
{
  /* find ECHAM VCT */
  int nvcth_id = UNDEFID, vcta_id = UNDEFID, vctb_id = UNDEFID;

  for ( int ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].ndims == 1 )
        {
          size_t len = strlen(ncvars[ncvarid].name);
          if ( len == 4 && ncvars[ncvarid].name[0] == 'h' && ncvars[ncvarid].name[1] == 'y' )
            {
              if ( ncvars[ncvarid].name[2] == 'a' && ncvars[ncvarid].name[3] == 'i' ) // hyai
                {
                  vcta_id = ncvarid;
                  nvcth_id = ncvars[ncvarid].dimids[0];
                  ncvars[ncvarid].isvar = FALSE;
                }
              else if ( ncvars[ncvarid].name[2] == 'b' && ncvars[ncvarid].name[3] == 'i' ) //hybi
                {
                  vctb_id = ncvarid;
                  nvcth_id = ncvars[ncvarid].dimids[0];
                  ncvars[ncvarid].isvar = FALSE;
                }
              else if ( (ncvars[ncvarid].name[2] == 'a' || ncvars[ncvarid].name[2] == 'b') && ncvars[ncvarid].name[3] == 'm' )
                {
                  ncvars[ncvarid].isvar = FALSE; // hyam or hybm
                }
            }
	}
    }

  /* read VCT */
  if ( nvcth_id != UNDEFID && vcta_id != UNDEFID && vctb_id != UNDEFID )
    {
      size_t vctsize = ncdims[nvcth_id].len;
      vctsize *= 2;
      *vct = (double *) Malloc(vctsize*sizeof(double));
      cdf_get_var_double(fileID, vcta_id, *vct);
      cdf_get_var_double(fileID, vctb_id, *vct+vctsize/2);
      *pvctsize = vctsize;
    }
}


int cdfInqContents(stream_t *streamptr)
{
  int ndims, nvars, ngatts, unlimdimid;
  int ncvarid;
  int ncdimid;
  size_t ntsteps;
  int timedimid = -1;
  int *varids;
  int nvarids;
  int time_has_units = FALSE;
  int time_has_bounds = FALSE;
  int time_climatology = FALSE;
  int leadtime_id = UNDEFID;
  int nvars_data;
  int instID  = UNDEFID;
  int modelID = UNDEFID;
  int taxisID;
  int i;
  int calendar = UNDEFID;
  ncdim_t *ncdims;
  ncvar_t *ncvars = NULL;
  int format = 0;
  int ucla_les = FALSE;
  unsigned char uuidOfHGrid[CDI_UUID_SIZE];
  unsigned char uuidOfVGrid[CDI_UUID_SIZE];
  char gridfile[8912];
  char fcreftime[CDI_MAX_NAME];
  int number_of_grid_used = UNDEFID;

  memset(uuidOfHGrid, 0, CDI_UUID_SIZE);
  memset(uuidOfVGrid, 0, CDI_UUID_SIZE);
  gridfile[0] = 0;
  fcreftime[0] = 0;

  int vlistID = streamptr->vlistID;
  int fileID  = streamptr->fileID;

  if ( CDI_Debug ) Message("streamID = %d, fileID = %d", streamptr->self, fileID);

#if  defined  (HAVE_NETCDF4)
  nc_inq_format(fileID, &format);
#endif

  cdf_inq(fileID, &ndims , &nvars, &ngatts, &unlimdimid);

  if ( CDI_Debug )
    Message("root: ndims %d, nvars %d, ngatts %d", ndims, nvars, ngatts);

  if ( ndims == 0 )
    {
      Warning("ndims = %d", ndims);
      return (CDI_EUFSTRUCT);
    }

  /* alloc ncdims */
  ncdims = (ncdim_t *) Malloc((size_t)ndims * sizeof (ncdim_t));
  init_ncdims(ndims, ncdims);

  if ( nvars > 0 )
    {
      /* alloc ncvars */
      ncvars = (ncvar_t *) Malloc((size_t)nvars * sizeof (ncvar_t));
      init_ncvars(nvars, ncvars);

      for ( ncvarid = 0; ncvarid < nvars; ++ncvarid )
        ncvars[ncvarid].ncid = fileID;
    }

#if  defined  (TEST_GROUPS)
#if  defined  (HAVE_NETCDF4)
  if ( format == NC_FORMAT_NETCDF4 )
    {
      int ncid;
      int numgrps;
      int ncids[NC_MAX_VARS];
      char name1[CDI_MAX_NAME];
      int gndims, gnvars, gngatts, gunlimdimid;
      nc_inq_grps(fileID, &numgrps, ncids);
      for ( int i = 0; i < numgrps; ++i )
        {
          ncid = ncids[i];
          nc_inq_grpname (ncid, name1);
          cdf_inq(ncid, &gndims , &gnvars, &gngatts, &gunlimdimid);

          if ( CDI_Debug )
            Message("%s: ndims %d, nvars %d, ngatts %d", name1, gndims, gnvars, gngatts);

          if ( gndims == 0 )
            {
            }
        }
    }
#endif
#endif

  if ( nvars == 0 )
    {
      Warning("nvars = %d", nvars);
      return (CDI_EUFSTRUCT);
    }

  /* scan global attributes */
  scan_global_attributes(fileID, vlistID, streamptr, ngatts, &instID, &modelID, &ucla_les,
                         uuidOfHGrid, uuidOfVGrid, gridfile, &number_of_grid_used);

  /* find time dim */
  if ( unlimdimid >= 0 )
    timedimid = unlimdimid;
  else
    timedimid = cdfTimeDimID(fileID, ndims, nvars);

  streamptr->basetime.ncdimid = timedimid;

  if ( timedimid != UNDEFID )
    cdf_inq_dimlen(fileID, timedimid, &ntsteps);
  else
    ntsteps = 0;

  if ( CDI_Debug ) Message("Number of timesteps = %d", ntsteps);
  if ( CDI_Debug ) Message("Time dimid = %d", streamptr->basetime.ncdimid);

  /* read ncdims */
  for ( ncdimid = 0; ncdimid < ndims; ncdimid++ )
    {
      cdf_inq_dimlen(fileID, ncdimid, &ncdims[ncdimid].len);
      cdf_inq_dimname(fileID, ncdimid, ncdims[ncdimid].name);
      if ( timedimid == ncdimid )
	ncdims[ncdimid].dimtype = T_AXIS;
    }

  if ( CDI_Debug ) printNCvars(ncvars, nvars, "cdfScanVarAttributes");

  /* scan attributes of all variables */
  cdfScanVarAttributes(nvars, ncvars, ncdims, timedimid, modelID, format);


  if ( CDI_Debug ) printNCvars(ncvars, nvars, "find coordinate vars");

  /* find coordinate vars */
  for ( ncdimid = 0; ncdimid < ndims; ncdimid++ )
    {
      for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
	{
	  if ( ncvars[ncvarid].ndims == 1 )
	    {
	      if ( timedimid != UNDEFID && timedimid == ncvars[ncvarid].dimids[0] )
		{
		  if ( ncvars[ncvarid].isvar != FALSE ) cdfSetVar(ncvars, ncvarid, TRUE);
		}
	      else
		{
                  //  if ( ncvars[ncvarid].isvar != TRUE ) cdfSetVar(ncvars, ncvarid, FALSE);
		}
	      // if ( ncvars[ncvarid].isvar != TRUE ) cdfSetVar(ncvars, ncvarid, FALSE);

	      if ( ncdimid == ncvars[ncvarid].dimids[0] && ncdims[ncdimid].ncvarid == UNDEFID )
		if ( strcmp(ncvars[ncvarid].name, ncdims[ncdimid].name) == 0 )
		  {
		    ncdims[ncdimid].ncvarid = ncvarid;
		    ncvars[ncvarid].isvar = FALSE;
		  }
	    }
	}
    }

  /* find time vars */
  find_time_vars(nvars, ncvars, ncdims, timedimid, streamptr, &time_has_units, &time_has_bounds, &time_climatology);

  leadtime_id = find_leadtime(nvars, ncvars);
  if ( leadtime_id != UNDEFID ) ncvars[leadtime_id].isvar = FALSE;

  /* check ncvars */
  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( timedimid != UNDEFID )
	if ( ncvars[ncvarid].isvar == -1 &&
	     ncvars[ncvarid].ndims > 1   &&
	     timedimid == ncvars[ncvarid].dimids[0] )
	  cdfSetVar(ncvars, ncvarid, TRUE);

      if ( ncvars[ncvarid].isvar == -1 && ncvars[ncvarid].ndims == 0 )
	cdfSetVar(ncvars, ncvarid, FALSE);

      //if ( ncvars[ncvarid].isvar == -1 && ncvars[ncvarid].ndims > 1 )
      if ( ncvars[ncvarid].isvar == -1 && ncvars[ncvarid].ndims >= 1 )
	cdfSetVar(ncvars, ncvarid, TRUE);

      if ( ncvars[ncvarid].isvar == -1 )
	{
	  ncvars[ncvarid].isvar = 0;
	  Warning("Variable %s has an unknown type, skipped!", ncvars[ncvarid].name);
	  continue;
	}

      if ( ncvars[ncvarid].ndims > 4 )
	{
	  ncvars[ncvarid].isvar = 0;
	  Warning("%d dimensional variables are not supported, skipped variable %s!",
		ncvars[ncvarid].ndims, ncvars[ncvarid].name);
	  continue;
	}

      if ( ncvars[ncvarid].ndims == 4 && timedimid == UNDEFID )
	{
	  ncvars[ncvarid].isvar = 0;
	  Warning("%d dimensional variables without time dimension are not supported, skipped variable %s!",
		ncvars[ncvarid].ndims, ncvars[ncvarid].name);
	  continue;
	}

      if ( xtypeIsText(ncvars[ncvarid].xtype) )
	{
	  ncvars[ncvarid].isvar = 0;
	  continue;
	}

      if ( cdfInqDatatype(ncvars[ncvarid].xtype, ncvars[ncvarid].lunsigned) == -1 )
	{
	  ncvars[ncvarid].isvar = 0;
	  Warning("Variable %s has an unsupported data type, skipped!", ncvars[ncvarid].name);
	  continue;
	}

      if ( timedimid != UNDEFID && ntsteps == 0 && ncvars[ncvarid].ndims > 0 )
	{
	  if ( timedimid == ncvars[ncvarid].dimids[0] )
	    {
	      ncvars[ncvarid].isvar = 0;
	      Warning("Number of time steps undefined, skipped variable %s!", ncvars[ncvarid].name);
	      continue;
	    }
	}
    }

  /* verify coordinate vars - first scan (dimname == varname) */
  verify_coordinate_vars_1(fileID, ndims, ncdims, ncvars, timedimid);

  /* verify coordinate vars - second scan (all other variables) */
  verify_coordinate_vars_2(nvars, ncvars);

  if ( CDI_Debug ) printNCvars(ncvars, nvars, "verify_coordinate_vars");

  if ( ucla_les == TRUE )
    {
      for ( ncdimid = 0; ncdimid < ndims; ncdimid++ )
	{
	  ncvarid = ncdims[ncdimid].ncvarid;
	  if ( ncvarid != -1 )
	    {
	      if ( ncdims[ncdimid].dimtype == UNDEFID && ncvars[ncvarid].units[0] == 'm' )
		{
		  if      ( ncvars[ncvarid].name[0] == 'x' ) ncdims[ncdimid].dimtype = X_AXIS;
		  else if ( ncvars[ncvarid].name[0] == 'y' ) ncdims[ncdimid].dimtype = Y_AXIS;
		  else if ( ncvars[ncvarid].name[0] == 'z' ) ncdims[ncdimid].dimtype = Z_AXIS;
		}
	    }
	}
    }
  /*
  for ( ncdimid = 0; ncdimid < ndims; ncdimid++ )
    {
      ncvarid = ncdims[ncdimid].ncvarid;
      if ( ncvarid != -1 )
	{
	  printf("coord var %d %s %s\n", ncvarid, ncvars[ncvarid].name, ncvars[ncvarid].units);
	  if ( ncdims[ncdimid].dimtype == X_AXIS )
	    printf("coord var %d %s is x dim\n", ncvarid, ncvars[ncvarid].name);
	  if ( ncdims[ncdimid].dimtype == Y_AXIS )
	    printf("coord var %d %s is y dim\n", ncvarid, ncvars[ncvarid].name);
	  if ( ncdims[ncdimid].dimtype == Z_AXIS )
	    printf("coord var %d %s is z dim\n", ncvarid, ncvars[ncvarid].name);
	  if ( ncdims[ncdimid].dimtype == T_AXIS )
	    printf("coord var %d %s is t dim\n", ncvarid, ncvars[ncvarid].name);

	  if ( ncvars[ncvarid].islon )
	    printf("coord var %d %s is lon\n", ncvarid, ncvars[ncvarid].name);
	  if ( ncvars[ncvarid].islat )
	    printf("coord var %d %s is lat\n", ncvarid, ncvars[ncvarid].name);
	  if ( ncvars[ncvarid].islev )
	    printf("coord var %d %s is lev\n", ncvarid, ncvars[ncvarid].name);
	}
    }
  */

  /* Set coordinate varids (att: associate)  */
  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    {
      if ( ncvars[ncvarid].isvar == TRUE && ncvars[ncvarid].ncoordvars )
	{
	  /* ndims = ncvars[ncvarid].ndims; */
	  ndims = ncvars[ncvarid].ncoordvars;
	  for ( i = 0; i < ndims; i++ )
	    {
	      if ( ncvars[ncvars[ncvarid].coordvarids[i]].islon )
		ncvars[ncvarid].xvarid = ncvars[ncvarid].coordvarids[i];
	      else if ( ncvars[ncvars[ncvarid].coordvarids[i]].islat )
		ncvars[ncvarid].yvarid = ncvars[ncvarid].coordvarids[i];
	      else if ( ncvars[ncvars[ncvarid].coordvarids[i]].islev )
		ncvars[ncvarid].zvarid = ncvars[ncvarid].coordvarids[i];
	    }
	}
    }

  /* set dim type */
  setDimType(nvars, ncvars, ncdims);

  /* read ECHAM VCT if present */
  size_t vctsize = 0;
  double *vct = NULL;
  read_vct_echam(fileID, nvars, ncvars, ncdims, &vct, &vctsize);


  if ( CDI_Debug ) printNCvars(ncvars, nvars, "define_all_grids");

  /* define all grids */
  define_all_grids(streamptr, vlistID, ncdims, nvars, ncvars, timedimid, uuidOfHGrid, gridfile, number_of_grid_used);


  /* define all zaxes */
  define_all_zaxes(streamptr, vlistID, ncdims, nvars, ncvars, vctsize, vct, uuidOfVGrid);
  if ( vct ) Free(vct);


  /* select vars */
  varids = (int *) Malloc((size_t)nvars * sizeof (int));
  nvarids = 0;
  for ( ncvarid = 0; ncvarid < nvars; ncvarid++ )
    if ( ncvars[ncvarid].isvar == TRUE ) varids[nvarids++] = ncvarid;

  nvars_data = nvarids;

  if ( CDI_Debug ) Message("time varid = %d", streamptr->basetime.ncvarid);
  if ( CDI_Debug ) Message("ntsteps = %d", ntsteps);
  if ( CDI_Debug ) Message("nvars_data = %d", nvars_data);


  if ( nvars_data == 0 )
    {
      streamptr->ntsteps = 0;
      return (CDI_EUFSTRUCT);
    }

  if ( ntsteps == 0 && streamptr->basetime.ncdimid == UNDEFID && streamptr->basetime.ncvarid != UNDEFID )
    ntsteps = 1;

  streamptr->ntsteps = (long)ntsteps;

  /* define all data variables */
  define_all_vars(streamptr, vlistID, instID, modelID, varids, nvars_data, nvars, ncvars);


  cdiCreateTimesteps(streamptr);

  /* time varID */
  int nctimevarid = streamptr->basetime.ncvarid;

  if ( time_has_units )
    {
      taxis_t *taxis = &streamptr->tsteps[0].taxis;

      if ( setBaseTime(ncvars[nctimevarid].units, taxis) == 1 )
        {
          nctimevarid = UNDEFID;
          streamptr->basetime.ncvarid = UNDEFID;
        }

      if ( leadtime_id != UNDEFID && taxis->type == TAXIS_RELATIVE )
        {
          streamptr->basetime.leadtimeid = leadtime_id;
          taxis->type = TAXIS_FORECAST;

          int timeunit = -1;
          if ( ncvars[leadtime_id].units[0] != 0 ) timeunit = scanTimeUnit(ncvars[leadtime_id].units);
          if ( timeunit == -1 ) timeunit = taxis->unit;
          taxis->fc_unit = timeunit;

          setForecastTime(fcreftime, taxis);
        }
    }

  if ( time_has_bounds )
    {
      streamptr->tsteps[0].taxis.has_bounds = TRUE;
      if ( time_climatology ) streamptr->tsteps[0].taxis.climatology = TRUE;
    }

  if ( nctimevarid != UNDEFID )
    {
      taxis_t *taxis = &streamptr->tsteps[0].taxis;
      ptaxisDefName(taxis, ncvars[nctimevarid].name);
      if ( ncvars[nctimevarid].longname[0] )
        ptaxisDefLongname(taxis, ncvars[nctimevarid].longname);
    }

  if ( nctimevarid != UNDEFID )
    if ( ncvars[nctimevarid].calendar == TRUE )
      {
        enum {attstringlen = 8192};
        char attstring[attstringlen];

	cdfGetAttText(fileID, nctimevarid, "calendar", attstringlen, attstring);
	strtolower(attstring);

	if ( memcmp(attstring, "standard", 8)  == 0 ||
	     memcmp(attstring, "gregorian", 9) == 0 )
	  calendar = CALENDAR_STANDARD;
	else if ( memcmp(attstring, "none", 4) == 0 )
	  calendar = CALENDAR_NONE;
	else if ( memcmp(attstring, "proleptic", 9) == 0 )
	  calendar = CALENDAR_PROLEPTIC;
	else if ( memcmp(attstring, "360", 3) == 0 )
	  calendar = CALENDAR_360DAYS;
	else if ( memcmp(attstring, "365", 3) == 0 ||
		  memcmp(attstring, "noleap", 6)  == 0 )
	  calendar = CALENDAR_365DAYS;
	else if ( memcmp(attstring, "366", 3)  == 0 ||
		  memcmp(attstring, "all_leap", 8) == 0 )
	  calendar = CALENDAR_366DAYS;
	else
	  Warning("calendar >%s< unsupported!", attstring);
      }

  if ( streamptr->tsteps[0].taxis.type == TAXIS_FORECAST )
    {
      taxisID = taxisCreate(TAXIS_FORECAST);
    }
  else if ( streamptr->tsteps[0].taxis.type == TAXIS_RELATIVE )
    {
      taxisID = taxisCreate(TAXIS_RELATIVE);
    }
  else
    {
      taxisID = taxisCreate(TAXIS_ABSOLUTE);
      if ( !time_has_units )
	{
	  taxisDefTunit(taxisID, TUNIT_DAY);
	  streamptr->tsteps[0].taxis.unit = TUNIT_DAY;
	}
    }


  if ( calendar == UNDEFID && streamptr->tsteps[0].taxis.type != TAXIS_ABSOLUTE )
    {
      calendar = CALENDAR_STANDARD;
    }

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 5)
#pragma GCC diagnostic push
#pragma GCC diagnostic warning "-Wstrict-overflow"
#endif
  if ( calendar != UNDEFID )
    {
      taxis_t *taxis = &streamptr->tsteps[0].taxis;
      taxis->calendar = calendar;
      taxisDefCalendar(taxisID, calendar);
    }
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ > 5)
#pragma GCC diagnostic pop
#endif

  vlistDefTaxis(vlistID, taxisID);

  streamptr->curTsID = 0;
  streamptr->rtsteps = 1;

  (void) cdfInqTimestep(streamptr, 0);

  cdfCreateRecords(streamptr, 0);

  /* free ncdims */
  Free(ncdims);

  /* free ncvars */
  Free(ncvars);

  return (0);
}

static
void wrf_read_timestep(int fileID, int nctimevarid, int tsID, taxis_t *taxis)
{
  size_t start[2], count[2];
  char stvalue[32];
  start[0] = (size_t) tsID; start[1] = 0;
  count[0] = 1; count[1] = 19;
  stvalue[0] = 0;
  cdf_get_vara_text(fileID, nctimevarid, start, count, stvalue);
  stvalue[19] = 0;
  {
    int year = 1, month = 1, day = 1 , hour = 0, minute = 0, second = 0;
    if ( strlen(stvalue) == 19 )
      sscanf(stvalue, "%d-%d-%d_%d:%d:%d", &year, &month, &day, &hour, &minute, &second);
    taxis->vdate = cdiEncodeDate(year, month, day);
    taxis->vtime = cdiEncodeTime(hour, minute, second);
    taxis->type = TAXIS_ABSOLUTE;
  }
}

static
double get_timevalue(int fileID, int nctimevarid, int tsID, timecache_t *tcache)
{
  double timevalue = 0;
  size_t index = (size_t) tsID;

  if ( tcache )
    {
      if ( tcache->size == 0 || (tsID < tcache->startid || tsID > (tcache->startid+tcache->size-1)) )
        {
          int maxvals = MAX_TIMECACHE_SIZE;
          tcache->startid = (tsID/MAX_TIMECACHE_SIZE)*MAX_TIMECACHE_SIZE;
          if ( (tcache->startid + maxvals) > tcache->maxvals ) maxvals = (tcache->maxvals)%MAX_TIMECACHE_SIZE;
          tcache->size = maxvals;
          index = (size_t) tcache->startid;
          // fprintf(stderr, "fill time cache: %d %d %d %d %d\n", tcache->maxvals, tsID, tcache->startid, tcache->startid+maxvals-1, maxvals);
          for ( int ival = 0; ival < maxvals; ++ival )
            {
              cdf_get_var1_double(fileID, nctimevarid, &index, &timevalue);
              if ( timevalue >= NC_FILL_DOUBLE || timevalue < -NC_FILL_DOUBLE ) timevalue = 0;
              tcache->cache[ival] = timevalue;
              index++;
            }
        }

      timevalue = tcache->cache[tsID%MAX_TIMECACHE_SIZE];
    }
  else
    {
      cdf_get_var1_double(fileID, nctimevarid, &index, &timevalue);
      if ( timevalue >= NC_FILL_DOUBLE || timevalue < -NC_FILL_DOUBLE ) timevalue = 0;
    }

  return timevalue;
}


int cdfInqTimestep(stream_t * streamptr, int tsID)
{
  long nrecs = 0;
  double timevalue;
  int fileID;
  taxis_t *taxis;

  if ( CDI_Debug ) Message("streamID = %d  tsID = %d", streamptr->self, tsID);

  if ( tsID < 0 ) Error("unexpected tsID = %d", tsID);

  if ( tsID < streamptr->ntsteps && streamptr->ntsteps > 0 )
    {
      cdfCreateRecords(streamptr, tsID);

      taxis = &streamptr->tsteps[tsID].taxis;
      if ( tsID > 0 )
	ptaxisCopy(taxis, &streamptr->tsteps[0].taxis);

      timevalue = tsID;

      int nctimevarid = streamptr->basetime.ncvarid;
      if ( nctimevarid != UNDEFID )
	{
	  fileID = streamptr->fileID;
	  size_t index  = (size_t)tsID;

	  if ( streamptr->basetime.lwrf )
	    {
              wrf_read_timestep(fileID, nctimevarid, tsID, taxis);
	    }
	  else
	    {
#if defined (USE_TIMECACHE)
              if ( streamptr->basetime.timevar_cache == NULL )
                {
                  streamptr->basetime.timevar_cache = (timecache_t *) Malloc(MAX_TIMECACHE_SIZE*sizeof(timecache_t));
                  streamptr->basetime.timevar_cache->size = 0;
                  streamptr->basetime.timevar_cache->maxvals = streamptr->ntsteps;
                }
#endif
              timevalue = get_timevalue(fileID, nctimevarid, tsID, streamptr->basetime.timevar_cache);
	      cdiDecodeTimeval(timevalue, taxis, &taxis->vdate, &taxis->vtime);
	    }

	  int nctimeboundsid = streamptr->basetime.ncvarboundsid;
	  if ( nctimeboundsid != UNDEFID )
	    {
	      size_t start[2], count[2];
              start[0] = index; count[0] = 1; start[1] = 0; count[1] = 1;
	      cdf_get_vara_double(fileID, nctimeboundsid, start, count, &timevalue);
              if ( timevalue >= NC_FILL_DOUBLE || timevalue < -NC_FILL_DOUBLE ) timevalue = 0;

	      cdiDecodeTimeval(timevalue, taxis, &taxis->vdate_lb, &taxis->vtime_lb);

              start[0] = index; count[0] = 1; start[1] = 1; count[1] = 1;
	      cdf_get_vara_double(fileID, nctimeboundsid, start, count, &timevalue);
              if ( timevalue >= NC_FILL_DOUBLE || timevalue < -NC_FILL_DOUBLE ) timevalue = 0;

	      cdiDecodeTimeval(timevalue, taxis, &taxis->vdate_ub, &taxis->vtime_ub);
	    }

          int leadtimeid = streamptr->basetime.leadtimeid;
          if ( leadtimeid != UNDEFID )
            {
              timevalue = get_timevalue(fileID, leadtimeid, tsID, NULL);
              cdiSetForecastPeriod(timevalue, taxis);
            }
	}
    }

  streamptr->curTsID = tsID;
  nrecs = streamptr->tsteps[tsID].nrecs;

  return ((int) nrecs);
}


void cdfDefHistory(stream_t *streamptr, int size, const char *history)
{
  int ncid = streamptr->fileID;
  cdf_put_att_text(ncid, NC_GLOBAL, "history", (size_t) size, history);
}


int cdfInqHistorySize(stream_t *streamptr)
{
  size_t size = 0;
  int ncid = streamptr->fileID;
  if ( streamptr->historyID != UNDEFID )
    cdf_inq_attlen(ncid, NC_GLOBAL, "history", &size);

  return ((int) size);
}


void cdfInqHistoryString(stream_t *streamptr, char *history)
{
  int ncid = streamptr->fileID;
  if ( streamptr->historyID != UNDEFID )
    {
      nc_type atttype;
      cdf_inq_atttype(ncid, NC_GLOBAL, "history", &atttype);

      if ( atttype == NC_CHAR )
        {
          cdf_get_att_text(ncid, NC_GLOBAL, "history", history);
        }
#if  defined  (HAVE_NETCDF4)
      else if ( atttype == NC_STRING )
        {
          // ToDo
          Warning("History attribute with type NC_STRING unsupported!");
        }
#endif
    }
}


void cdfDefVars(stream_t *streamptr)
{
  int vlistID = streamptr->vlistID;
  if ( vlistID == UNDEFID )
    Error("Internal problem! vlist undefined for streamptr %p", streamptr);

  int ngrids = vlistNgrids(vlistID);
  int nzaxis = vlistNzaxis(vlistID);
  /*
  if ( vlistHasTime(vlistID) ) cdfDefTime(streamptr);
  */
  if ( ngrids > 0 )
    for ( int index = 0; index < ngrids; index++ )
      {
        int gridID = vlistGrid(vlistID, index);
        cdfDefGrid(streamptr, gridID);
      }

  if ( nzaxis > 0 )
    for ( int index = 0; index < nzaxis; index++ )
      {
        int zaxisID = vlistZaxis(vlistID, index);
        if ( streamptr->zaxisID[index] == UNDEFID ) cdfDefZaxis(streamptr, zaxisID);
      }

  /* define time first!!!
    int nvars  = vlistNvars(vlistID);
  for ( int varID = 0; varID < nvars; varID++ )
    {
      int ncvarid = cdfDefVar(streamptr, varID);
    }
  */
}
#endif
/*
 * Local Variables:
 * c-file-style: "Java"
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * require-trailing-newline: t
 * End:
 */
