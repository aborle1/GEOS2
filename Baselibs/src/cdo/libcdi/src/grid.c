#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <assert.h>
#include <string.h>
#include <float.h>  /* FLT_EPSILON */
#include <limits.h> /* INT_MAX     */

#include "dmemory.h"
#include "cdi.h"
#include "cdi_cksum.h"
#include "cdi_int.h"
#include "cdi_uuid.h"
#include "grid.h"
#include "gaussgrid.h"
#include "resource_handle.h"
#include "resource_unpack.h"
#include "namespace.h"
#include "serialize.h"
#include "vlist.h"

#undef  UNDEFID
#define UNDEFID -1

/* the value in the second pair of brackets must match the length of
 * the longest string (including terminating NUL) */
static const char Grids[][17] = {
  /*  0 */  "undefined",
  /*  1 */  "generic",
  /*  2 */  "gaussian",
  /*  3 */  "gaussian reduced",
  /*  4 */  "lonlat",
  /*  5 */  "spectral",
  /*  6 */  "fourier",
  /*  7 */  "gme",
  /*  8 */  "trajectory",
  /*  9 */  "unstructured",
  /* 10 */  "curvilinear",
  /* 11 */  "lcc",
  /* 12 */  "lcc2",
  /* 13 */  "laea",
  /* 14 */  "sinusoidal",
  /* 15 */  "projection",
};

/* must match table below */
enum xystdname_idx {
  grid_xystdname_grid_latlon,
  grid_xystdname_latlon,
  grid_xystdname_projection,
};
static const char xystdname_tab[][2][24] = {
  [grid_xystdname_grid_latlon] = { "grid_longitude",
                                   "grid_latitude" },
  [grid_xystdname_latlon] = { "longitude",
                              "latitude" },
  [grid_xystdname_projection] = { "projection_x_coordinate",
                                  "projection_y_coordinate" },

};



static int    gridCompareP    ( void * gridptr1, void * gridptr2 );
static void   gridDestroyP    ( void * gridptr );
static void   gridPrintP      ( void * gridptr, FILE * fp );
static int    gridGetPackSize ( void * gridptr, void *context);
static void   gridPack        ( void * gridptr, void * buff, int size,
				int *position, void *context);
static int    gridTxCode      ( void );

static const resOps gridOps = {
  gridCompareP,
  gridDestroyP,
  gridPrintP,
  gridGetPackSize,
  gridPack,
  gridTxCode
};

static int  GRID_Debug = 0;   /* If set to 1, debugging */

grid_t *gridID2Ptr(int gridID)
{
  return (grid_t *)reshGetVal(gridID, &gridOps);
}
#define gridID2Ptr(gridID) (grid_t *)reshGetVal(gridID, &gridOps)
#define gridMark4Update(gridID) reshSetStatus(gridID, &gridOps, RESH_DESYNC_IN_USE)


void grid_init(grid_t *gridptr)
{
  gridptr->self          = CDI_UNDEFID;
  gridptr->type          = CDI_UNDEFID;
  gridptr->proj          = CDI_UNDEFID;
  gridptr->mask          = NULL;
  gridptr->mask_gme      = NULL;
  gridptr->xvals         = NULL;
  gridptr->yvals         = NULL;
  gridptr->area          = NULL;
  gridptr->xbounds       = NULL;
  gridptr->ybounds       = NULL;
  gridptr->rowlon        = NULL;
  gridptr->nrowlon       = 0;
  gridptr->xfirst        = 0.0;
  gridptr->xlast         = 0.0;
  gridptr->xinc          = 0.0;
  gridptr->yfirst        = 0.0;
  gridptr->ylast         = 0.0;
  gridptr->yinc          = 0.0;
  gridptr->lcc_originLon = 0.0;
  gridptr->lcc_originLat = 0.0;
  gridptr->lcc_lonParY   = 0.0;
  gridptr->lcc_lat1      = 0.0;
  gridptr->lcc_lat2      = 0.0;
  gridptr->lcc_xinc      = 0.0;
  gridptr->lcc_yinc      = 0.0;
  gridptr->lcc_projflag  = 0;
  gridptr->lcc_scanflag  = 0;
  gridptr->lcc_defined   = FALSE;
  gridptr->lcc2_lon_0    = 0.0;
  gridptr->lcc2_lat_0    = 0.0;
  gridptr->lcc2_lat_1    = 0.0;
  gridptr->lcc2_lat_2    = 0.0;
  gridptr->lcc2_a        = 0.0;
  gridptr->lcc2_defined  = FALSE;
  gridptr->laea_lon_0    = 0.0;
  gridptr->laea_lat_0    = 0.0;
  gridptr->laea_a        = 0.0;
  gridptr->laea_defined  = FALSE;
  gridptr->trunc         = 0;
  gridptr->nvertex       = 0;
  gridptr->nd            = 0;
  gridptr->ni            = 0;
  gridptr->ni2           = 0;
  gridptr->ni3           = 0;
  gridptr->number        = 0;
  gridptr->position      = 0;
  gridptr->reference     = NULL;
  gridptr->prec          = 0;
  gridptr->size          = 0;
  gridptr->xsize         = 0;
  gridptr->ysize         = 0;
  gridptr->np            = 0;
  gridptr->xdef          = 0;
  gridptr->ydef          = 0;
  gridptr->isCyclic      = CDI_UNDEFID;
  gridptr->isRotated     = FALSE;
  gridptr->xpole         = 0.0;
  gridptr->ypole         = 0.0;
  gridptr->angle         = 0.0;
  gridptr->lcomplex      = 0;
  gridptr->hasdims       = TRUE;
  gridptr->xdimname[0]   = 0;
  gridptr->ydimname[0]   = 0;
  gridptr->vdimname[0]   = 0;
  gridptr->xname[0]      = 0;
  gridptr->yname[0]      = 0;
  gridptr->xlongname[0]  = 0;
  gridptr->ylongname[0]  = 0;
  gridptr->xunits[0]     = 0;
  gridptr->yunits[0]     = 0;
  gridptr->xstdname      = NULL;
  gridptr->ystdname      = NULL;
  memset(gridptr->uuid, 0, CDI_UUID_SIZE);
  gridptr->name          = NULL;
  gridptr->vtable        = &cdiGridVtable;
  gridptr->extraData     = NULL;
}


static void
grid_free_components(grid_t *gridptr)
{
  void *p2free[] = { gridptr->mask, gridptr->mask_gme,
                   gridptr->xvals, gridptr->yvals,
                   gridptr->xbounds, gridptr->ybounds,
                   gridptr->rowlon, gridptr->area,
                   gridptr->reference, gridptr->name };
  for (size_t i = 0; i < sizeof (p2free) / sizeof (p2free[0]); ++i)
    if (p2free[i]) Free(p2free[i]);
}

void grid_free(grid_t *gridptr)
{
  grid_free_components(gridptr);
  grid_init(gridptr);
}

static grid_t *
gridNewEntry(cdiResH resH)
{
  grid_t *gridptr = (grid_t*) Malloc(sizeof(grid_t));
  grid_init(gridptr);
  if (resH == CDI_UNDEFID)
    gridptr->self = reshPut(gridptr, &gridOps);
  else
    {
      gridptr->self = resH;
      reshReplace(resH, gridptr, &gridOps);
    }
  return gridptr;
}

static
void gridInit(void)
{
  static int gridInitialized = 0;

  if ( gridInitialized ) return;

  gridInitialized = 1;

  const char *env = getenv("GRID_DEBUG");
  if ( env ) GRID_Debug = atoi(env);
}

static void
grid_copy_base_scalar_fields(grid_t *gridptrOrig, grid_t *gridptrDup)
{
  memcpy(gridptrDup, gridptrOrig, sizeof(grid_t));
  gridptrDup->self = CDI_UNDEFID;
  if (gridptrOrig->reference)
    gridptrDup->reference = strdupx(gridptrOrig->reference);
}


static grid_t *
grid_copy_base(grid_t *gridptrOrig)
{
  grid_t *gridptrDup = (grid_t *)Malloc(sizeof (*gridptrDup));
  gridptrOrig->vtable->copyScalarFields(gridptrOrig, gridptrDup);
  gridptrOrig->vtable->copyArrayFields(gridptrOrig, gridptrDup);
  return gridptrDup;
}

unsigned cdiGridCount(void)
{
  return reshCountType(&gridOps);
}

static inline
void gridSetString(char *gridstrname, const char *name, size_t len)
{
  if ( len > CDI_MAX_NAME ) len = CDI_MAX_NAME;
  strncpy(gridstrname, name, len);
  gridstrname[len - 1] = 0;
}

static inline
void gridGetString(char *name, const char *gridstrname, size_t len)
{
  if ( len > CDI_MAX_NAME ) len = CDI_MAX_NAME;
  strncpy(name, gridstrname, len);
  name[len - 1] = 0;
}

static inline void
gridSetName(char *gridstrname, const char *name)
{
  strncpy(gridstrname, name, CDI_MAX_NAME);
  gridstrname[CDI_MAX_NAME - 1] = 0;
}

void
cdiGridTypeInit(grid_t *gridptr, int gridtype, int size)
{
  gridptr->type = gridtype;
  gridptr->size = size;

  switch (gridtype)
    {
    case GRID_CURVILINEAR:
      gridptr->nvertex = 4;
      /* Fall through */
    case GRID_LONLAT:
    case GRID_GAUSSIAN:
    case GRID_GAUSSIAN_REDUCED:
    case GRID_TRAJECTORY:
      {
        if ( gridtype == GRID_TRAJECTORY )
          {
            if ( gridptr->xname[0] == 0 ) gridSetName(gridptr->xname, "tlon");
            if ( gridptr->yname[0] == 0 ) gridSetName(gridptr->yname, "tlat");
          }
        else
          {
            if ( gridptr->xname[0] == 0 ) gridSetName(gridptr->xname, "lon");
            if ( gridptr->yname[0] == 0 ) gridSetName(gridptr->yname, "lat");
          }
        gridSetName(gridptr->xlongname, "longitude");
        gridSetName(gridptr->ylongname, "latitude");

        /*
        if ( gridtype == GRID_CURVILINEAR )
          {
            gridptr->xstdname = xystdname_tab[grid_xystdname_grid_latlon][0];
            gridptr->ystdname = xystdname_tab[grid_xystdname_grid_latlon][1];
            gridDefXunits(gridID, "degrees");
            gridDefYunits(gridID, "degrees");
          }
        else
        */
          {
            gridptr->xstdname = xystdname_tab[grid_xystdname_latlon][0];
            gridptr->ystdname = xystdname_tab[grid_xystdname_latlon][1];
            gridSetName(gridptr->xunits, "degrees_east");
            gridSetName(gridptr->yunits, "degrees_north");
          }

        break;
      }
    case GRID_UNSTRUCTURED:
      gridptr->xsize = size;
      /* Fall through */
    case GRID_GME:
      {
        if ( gridptr->xname[0] == 0 ) gridSetName(gridptr->xname, "lon");
        if ( gridptr->yname[0] == 0 ) gridSetName(gridptr->yname, "lat");
        gridptr->xstdname = xystdname_tab[grid_xystdname_latlon][0];
        gridptr->ystdname = xystdname_tab[grid_xystdname_latlon][1];
        gridSetName(gridptr->xunits, "degrees_east");
        gridSetName(gridptr->yunits, "degrees_north");
        break;
      }
    case GRID_GENERIC:
      {

        /* gridptr->xsize = size; */
        if ( gridptr->xname[0] == 0 ) gridSetName(gridptr->xname, "x");
        if ( gridptr->yname[0] == 0 ) gridSetName(gridptr->yname, "y");
        /*
        strcpy(gridptr->xstdname, "grid_longitude");
        strcpy(gridptr->ystdname, "grid_latitude");
        gridptr->xstdname = xystdname_tab[grid_xystdname_grid_latlon][0];
        gridptr->ystdname = xystdname_tab[grid_xystdname_grid_latlon][1];
        gridDefXunits(gridID, "degrees");
        gridDefYunits(gridID, "degrees");
        */
        break;
      }
    case GRID_LCC2:
    case GRID_SINUSOIDAL:
    case GRID_LAEA:
      {
        if ( gridptr->xname[0] == 0 ) gridSetName(gridptr->xname, "x");
        if ( gridptr->yname[0] == 0 ) gridSetName(gridptr->yname, "y");
        gridptr->xstdname = xystdname_tab[grid_xystdname_projection][0];
        gridptr->ystdname = xystdname_tab[grid_xystdname_projection][1];
        gridSetName(gridptr->xunits, "m");
        gridSetName(gridptr->yunits, "m");
        break;
      }
    }
}


// used also in CDO
void gridGenXvals(int xsize, double xfirst, double xlast, double xinc, double *restrict xvals)
{
  if ( (! (fabs(xinc) > 0)) && xsize > 1 )
    {
      if ( xfirst >= xlast )
        {
          while ( xfirst >= xlast ) xlast += 360;
          xinc = (xlast-xfirst)/(xsize);
        }
      else
        {
          xinc = (xlast-xfirst)/(xsize-1);
        }
    }

  for ( int i = 0; i < xsize; ++i )
    xvals[i] = xfirst + i*xinc;
}

static
void calc_gaussgrid(double *restrict yvals, int ysize, double yfirst, double ylast)
{
  double *restrict yw = (double *) Malloc((size_t)ysize * sizeof(double));
  gaussaw(yvals, yw, (size_t)ysize);
  Free(yw);
  for (int i = 0; i < ysize; i++ )
    yvals[i] = asin(yvals[i])/M_PI*180.0;

  if ( yfirst < ylast && yfirst > -90.0 && ylast < 90.0 )
    {
      int yhsize = ysize/2;
      for (int i = 0; i < yhsize; i++ )
        {
          double ytmp = yvals[i];
          yvals[i] = yvals[ysize-i-1];
          yvals[ysize-i-1] = ytmp;
        }
    }
}

// used also in CDO
void gridGenYvals(int gridtype, int ysize, double yfirst, double ylast, double yinc, double *restrict yvals)
{
  const double deleps = 0.002;

  if ( gridtype == GRID_GAUSSIAN || gridtype == GRID_GAUSSIAN_REDUCED )
    {
      if ( ysize > 2 )
	{
	  calc_gaussgrid(yvals, ysize, yfirst, ylast);

	  if ( ! (IS_EQUAL(yfirst, 0) && IS_EQUAL(ylast, 0)) )
	    if ( fabs(yvals[0] - yfirst) > deleps || fabs(yvals[ysize-1] - ylast) > deleps )
	      {
		double *restrict ytmp = NULL;
		int nstart, lfound = 0;
		int ny = (int) (180./(fabs(ylast-yfirst)/(ysize-1)) + 0.5);
		ny -= ny%2;
		if ( ny > ysize && ny < 4096 )
		  {
		    ytmp = (double *) Malloc((size_t)ny * sizeof (double));
		    calc_gaussgrid(ytmp, ny, yfirst, ylast);
                    {
                      int i;
                      for ( i = 0; i < (ny-ysize); i++ )
                        if ( fabs(ytmp[i] - yfirst) < deleps ) break;
                      nstart = i;
                    }

		    lfound = (nstart+ysize-1) < ny
                      && fabs(ytmp[nstart+ysize-1] - ylast) < deleps;
                    if ( lfound )
                      {
                        for (int i = 0; i < ysize; i++) yvals[i] = ytmp[i+nstart];
                      }
		  }

		if ( !lfound )
		  {
		    Warning("Cannot calculate gaussian latitudes for lat1 = %g latn = %g!", yfirst, ylast);
		    for (int i = 0; i < ysize; i++ ) yvals[i] = 0;
		    yvals[0] = yfirst;
		    yvals[ysize-1] = ylast;
		  }

		if ( ytmp ) Free(ytmp);
	      }
	}
      else
        {
          yvals[0] = yfirst;
          yvals[ysize-1] = ylast;
        }
    }
  /*     else if ( gridtype == GRID_LONLAT || gridtype == GRID_GENERIC ) */
  else
    {
      if ( (! (fabs(yinc) > 0)) && ysize > 1 )
        {
          if ( IS_EQUAL(yfirst, ylast) && IS_NOT_EQUAL(yfirst, 0) ) ylast *= -1;

          if ( yfirst > ylast )
            yinc = (yfirst-ylast)/(ysize-1);
          else if ( yfirst < ylast )
            yinc = (ylast-yfirst)/(ysize-1);
          else
            {
              if ( ysize%2 != 0 )
                {
                  yinc = 180.0/(ysize-1);
                  yfirst = -90;
                }
              else
                {
                  yinc = 180.0/ysize;
                  yfirst = -90 + yinc/2;
                }
            }
        }

      if ( yfirst > ylast && yinc > 0 ) yinc = -yinc;

      for (int i = 0; i < ysize; i++ )
        yvals[i] = yfirst + i*yinc;
    }
  /*
    else
    Error("unable to calculate values for %s grid!", gridNamePtr(gridtype));
  */
}

/*
@Function  gridCreate
@Title     Create a horizontal Grid

@Prototype int gridCreate(int gridtype, int size)
@Parameter
    @Item  gridtype  The type of the grid, one of the set of predefined CDI grid types.
                     The valid CDI grid types are @func{GRID_GENERIC}, @func{GRID_GAUSSIAN},
                     @func{GRID_LONLAT}, @func{GRID_LCC}, @func{GRID_SPECTRAL},
                     @func{GRID_GME}, @func{GRID_CURVILINEAR} and @func{GRID_UNSTRUCTURED}.
    @Item  size      Number of gridpoints.

@Description
The function @func{gridCreate} creates a horizontal Grid.

@Result
@func{gridCreate} returns an identifier to the Grid.

@Example
Here is an example using @func{gridCreate} to create a regular lon/lat Grid:

@Source
#include "cdi.h"
   ...
#define  nlon  12
#define  nlat   6
   ...
double lons[nlon] = {0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330};
double lats[nlat] = {-75, -45, -15, 15, 45, 75};
int gridID;
   ...
gridID = gridCreate(GRID_LONLAT, nlon*nlat);
gridDefXsize(gridID, nlon);
gridDefYsize(gridID, nlat);
gridDefXvals(gridID, lons);
gridDefYvals(gridID, lats);
   ...
@EndSource
@EndFunction
*/
int gridCreate(int gridtype, int size)
{
  if ( CDI_Debug ) Message("gridtype=%s  size=%d", gridNamePtr(gridtype), size);

  if ( size < 0 || size > INT_MAX ) Error("Grid size (%d) out of bounds (0 - %d)!", size, INT_MAX);

  gridInit();

  grid_t *gridptr = gridNewEntry(CDI_UNDEFID);
  if ( ! gridptr ) Error("No memory");

  int gridID = gridptr->self;

  if ( CDI_Debug ) Message("gridID: %d", gridID);

  cdiGridTypeInit(gridptr, gridtype, size);

  return gridID;
}

static
void gridDestroyKernel( grid_t * gridptr )
{
  int id;

  xassert ( gridptr );

  id = gridptr->self;

  grid_free_components(gridptr);
  Free( gridptr );

  reshRemove ( id, &gridOps );
}

/*
@Function  gridDestroy
@Title     Destroy a horizontal Grid

@Prototype void gridDestroy(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.

@EndFunction
*/
void gridDestroy(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->destroy(gridptr);
}

void gridDestroyP ( void * gridptr )
{
  ((grid_t *)gridptr)->vtable->destroy((grid_t *)gridptr);
}


const char *gridNamePtr(int gridtype)
{
  int size = (int) (sizeof(Grids)/sizeof(Grids[0]));

  const char *name = gridtype >= 0 && gridtype < size ? Grids[gridtype] : Grids[GRID_GENERIC];

  return name;
}


void gridName(int gridtype, char *gridname)
{
  strcpy(gridname, gridNamePtr(gridtype));
}

static
char *grid_key_to_string(grid_t *gridptr, int key)
{
  char *gridstring = NULL;

  switch (key)
    {
    case CDI_GRID_XDIMNAME: gridstring = gridptr->xdimname; break;
    case CDI_GRID_YDIMNAME: gridstring = gridptr->ydimname; break;
    case CDI_GRID_VDIMNAME: gridstring = gridptr->vdimname; break;
    }

  return gridstring;
}

/*
@Function  cdiGridDefString
@Title     Define a CDI grid string value from a key

@Prototype int cdiGridDefString(int gridID, int key, int size, const char *mesg)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  key      The key to be searched
    @Item  size     The allocated length of the string on input
    @Item  mesg     The address of a string where the data will be read

@Description
The function @func{cdiGridDefString} defines a CDI grid string value from a key.

@Result
@func{cdiGridDefString} returns 0 if OK and integer value on error.

@EndFunction
*/
int cdiGridDefString(int gridID, int key, int size, const char *mesg)
{
  if ( size < 1 || mesg == NULL || *mesg == 0 ) return -1;

  grid_t *gridptr = gridID2Ptr(gridID);

  char *gridstring = grid_key_to_string(gridptr, key);
  if ( gridstring == NULL )
    {
      Warning("CDI grid string key %d not supported!", key);
      return -1;
    }

  gridSetString(gridstring, mesg, (size_t)size);
  gridMark4Update(gridID);

  return 0;
}

/*
@Function  cdiGridInqString
@Title     Get a CDI grid string value from a key

@Prototype int cdiGridInqString(int gridID, int key, int size, char *mesg)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  key      The key to be searched.
    @Item  size     The allocated length of the string on input.
    @Item  mesg     The address of a string where the data will be retrieved.
                    The caller must allocate space for the returned string.
                    The maximum possible length, in characters, of the string
                    is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{cdiGridInqString} return a CDI grid string value from a key.

@Result
@func{cdiGridInqString} returns 0 if OK and integer value on error.

@EndFunction
*/
int cdiGridInqString(int gridID, int key, int size, char *mesg)
{
  if ( size < 1 || mesg == NULL ) return -1;

  grid_t *gridptr = gridID2Ptr(gridID);
  const char *gridstring = grid_key_to_string(gridptr, key);
  if ( gridstring == NULL)
    {
      Warning("CDI grid string key %d not supported!", key);
      return -1;
    }

  gridGetString(mesg, gridstring, (size_t)size);

  return 0;
}

/*
@Function  gridDefXname
@Title     Define the name of a X-axis

@Prototype void gridDefXname(int gridID, const char *name)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  name     Name of the X-axis.

@Description
The function @func{gridDefXname} defines the name of a X-axis.

@EndFunction
*/
void gridDefXname(int gridID, const char *xname)
{
  if ( xname && *xname )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->xname, xname);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridDefXlongname
@Title     Define the longname of a X-axis

@Prototype void gridDefXlongname(int gridID, const char *longname)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  longname Longname of the X-axis.

@Description
The function @func{gridDefXlongname} defines the longname of a X-axis.

@EndFunction
*/
void gridDefXlongname(int gridID, const char *xlongname)
{
  if ( xlongname )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->xlongname, xlongname);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridDefXunits
@Title     Define the units of a X-axis

@Prototype void gridDefXunits(int gridID, const char *units)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  units    Units of the X-axis.

@Description
The function @func{gridDefXunits} defines the units of a X-axis.

@EndFunction
*/
void gridDefXunits(int gridID, const char *xunits)
{
  if ( xunits )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->xunits, xunits);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridDefYname
@Title     Define the name of a Y-axis

@Prototype void gridDefYname(int gridID, const char *name)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  name     Name of the Y-axis.

@Description
The function @func{gridDefYname} defines the name of a Y-axis.

@EndFunction
*/
void gridDefYname(int gridID, const char *yname)
{
  if ( yname && *yname )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->yname, yname);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridDefYlongname
@Title     Define the longname of a Y-axis

@Prototype void gridDefYlongname(int gridID, const char *longname)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  longname Longname of the Y-axis.

@Description
The function @func{gridDefYlongname} defines the longname of a Y-axis.

@EndFunction
*/
void gridDefYlongname(int gridID, const char *ylongname)
{
  if ( ylongname )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->ylongname, ylongname);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridDefYunits
@Title     Define the units of a Y-axis

@Prototype void gridDefYunits(int gridID, const char *units)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  units    Units of the Y-axis.

@Description
The function @func{gridDefYunits} defines the units of a Y-axis.

@EndFunction
*/
void gridDefYunits(int gridID, const char *yunits)
{
  if ( yunits )
    {
      grid_t *gridptr = gridID2Ptr(gridID);
      gridSetName(gridptr->yunits, yunits);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridInqXname
@Title     Get the name of a X-axis

@Prototype void gridInqXname(int gridID, char *name)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  name     Name of the X-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqXname} returns the name of a X-axis.

@Result
@func{gridInqXname} returns the name of the X-axis to the parameter name.

@EndFunction
*/
void gridInqXname(int gridID, char *xname)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(xname, gridptr->xname);
}

const char *gridInqXnamePtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->xname;
}

/*
@Function  gridInqXlongname
@Title     Get the longname of a X-axis

@Prototype void gridInqXlongname(int gridID, char *longname)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  longname Longname of the X-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqXlongname} returns the longname of a X-axis.

@Result
@func{gridInqXlongname} returns the longname of the X-axis to the parameter longname.

@EndFunction
*/
void gridInqXlongname(int gridID, char *xlongname)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(xlongname, gridptr->xlongname);
}

/*
@Function  gridInqXunits
@Title     Get the units of a X-axis

@Prototype void gridInqXunits(int gridID, char *units)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  units    Units of the X-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqXunits} returns the units of a X-axis.

@Result
@func{gridInqXunits} returns the units of the X-axis to the parameter units.

@EndFunction
*/
void gridInqXunits(int gridID, char *xunits)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(xunits, gridptr->xunits);
}


void gridInqXstdname(int gridID, char *xstdname)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  if ( gridptr->xstdname )
    strcpy(xstdname, gridptr->xstdname);
  else
    xstdname[0] = 0;
}

/*
@Function  gridInqYname
@Title     Get the name of a Y-axis

@Prototype void gridInqYname(int gridID, char *name)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  name     Name of the Y-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqYname} returns the name of a Y-axis.

@Result
@func{gridInqYname} returns the name of the Y-axis to the parameter name.

@EndFunction
*/
void gridInqYname(int gridID, char *yname)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(yname, gridptr->yname);
}

const char *gridInqYnamePtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->yname;
}

/*
@Function  gridInqYlongname
@Title     Get the longname of a Y-axis

@Prototype void gridInqXlongname(int gridID, char *longname)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  longname Longname of the Y-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqYlongname} returns the longname of a Y-axis.

@Result
@func{gridInqYlongname} returns the longname of the Y-axis to the parameter longname.

@EndFunction
*/
void gridInqYlongname(int gridID, char *ylongname)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(ylongname, gridptr->ylongname);
}

/*
@Function  gridInqYunits
@Title     Get the units of a Y-axis

@Prototype void gridInqYunits(int gridID, char *units)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  units    Units of the Y-axis. The caller must allocate space for the
                    returned string. The maximum possible length, in characters, of
                    the string is given by the predefined constant @func{CDI_MAX_NAME}.

@Description
The function @func{gridInqYunits} returns the units of a Y-axis.

@Result
@func{gridInqYunits} returns the units of the Y-axis to the parameter units.

@EndFunction
*/
void gridInqYunits(int gridID, char *yunits)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  strcpy(yunits, gridptr->yunits);
}

void gridInqYstdname(int gridID, char *ystdname)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  if ( gridptr->ystdname )
    strcpy(ystdname, gridptr->ystdname);
  else
    ystdname[0] = 0;
}

/*
@Function  gridInqType
@Title     Get the type of a Grid

@Prototype int gridInqType(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqType} returns the type of a Grid.

@Result
@func{gridInqType} returns the type of the grid,
one of the set of predefined CDI grid types.
The valid CDI grid types are @func{GRID_GENERIC}, @func{GRID_GAUSSIAN},
@func{GRID_LONLAT}, @func{GRID_LCC}, @func{GRID_SPECTRAL}, @func{GRID_GME},
@func{GRID_CURVILINEAR} and @func{GRID_UNSTRUCTURED}.

@EndFunction
*/
int gridInqType(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->type;
}


/*
@Function  gridInqSize
@Title     Get the size of a Grid

@Prototype int gridInqSize(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqSize} returns the size of a Grid.

@Result
@func{gridInqSize} returns the number of grid points of a Grid.

@EndFunction
*/
int gridInqSize(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  int size = gridptr->size;

  if ( size == 0 )
    {
      int xsize = gridptr->xsize;
      int ysize = gridptr->ysize;

      if ( ysize )
        size = xsize * ysize;
      else
        size = xsize;

      gridptr->size = size;
    }

  return size;
}

static
int nsp2trunc(int nsp)
{
  /*  nsp = (trunc+1)*(trunc+1)              */
  /*      => trunc^2 + 3*trunc - (x-2) = 0   */
  /*                                         */
  /*  with:  y^2 + p*y + q = 0               */
  /*         y = -p/2 +- sqrt((p/2)^2 - q)   */
  /*         p = 3 and q = - (x-2)           */
  int trunc = (int) (sqrt(nsp*4 + 1.) - 3) / 2;
  return trunc;
}


int gridInqTrunc(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->trunc == 0 )
    {
      if ( gridptr->type == GRID_SPECTRAL )
        gridptr->trunc = nsp2trunc(gridptr->size);
      /*
      else if      ( gridptr->type == GRID_GAUSSIAN )
        gridptr->trunc = nlat2trunc(gridptr->ysize);
      */
    }

  return gridptr->trunc;
}


void gridDefTrunc(int gridID, int trunc)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->trunc != trunc )
    {
      gridMark4Update(gridID);
      gridptr->trunc = trunc;
    }
}

/*
@Function  gridDefXsize
@Title     Define the number of values of a X-axis

@Prototype void gridDefXsize(int gridID, int xsize)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  xsize    Number of values of a X-axis.

@Description
The function @func{gridDefXsize} defines the number of values of a X-axis.

@EndFunction
*/
void gridDefXsize(int gridID, int xsize)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  int gridSize = gridInqSize(gridID);
  if ( xsize > gridSize )
    Error("xsize %d is greater then gridsize %d", xsize, gridSize);

  int gridType = gridInqType(gridID);
  if ( gridType == GRID_UNSTRUCTURED && xsize != gridSize )
    Error("xsize %d must be equal to gridsize %d for gridtype: UNSTRUCTURED", xsize, gridSize);

  if ( gridptr->xsize != xsize )
    {
      gridMark4Update(gridID);
      gridptr->xsize = xsize;
    }

  if ( gridType != GRID_UNSTRUCTURED )
    {
      long axisproduct = gridptr->xsize*gridptr->ysize;
      if ( axisproduct > 0 && axisproduct != gridSize )
        Error("Inconsistent grid declaration! (xsize=%d ysize=%d gridsize=%d)",
              gridptr->xsize, gridptr->ysize, gridSize);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefPrec(int gridID, int prec)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->prec != prec )
    {
      gridMark4Update(gridID);
      gridptr->prec = prec;
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
int gridInqPrec(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->prec;
}

/*
@Function  gridInqXsize
@Title     Get the number of values of a X-axis

@Prototype int gridInqXsize(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqXsize} returns the number of values of a X-axis.

@Result
@func{gridInqXsize} returns the number of values of a X-axis.

@EndFunction
*/
int gridInqXsize(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->xsize;
}

/*
@Function  gridDefYsize
@Title     Define the number of values of a Y-axis

@Prototype void gridDefYsize(int gridID, int ysize)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  ysize    Number of values of a Y-axis.

@Description
The function @func{gridDefYsize} defines the number of values of a Y-axis.

@EndFunction
*/
void gridDefYsize(int gridID, int ysize)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  int gridSize = gridInqSize(gridID);

  if ( ysize > gridSize )
    Error("ysize %d is greater then gridsize %d", ysize, gridSize);

  if ( gridInqType(gridID) == GRID_UNSTRUCTURED && ysize != gridSize )
    Error("ysize %d must be equal gridsize %d for gridtype: UNSTRUCTURED", ysize, gridSize);

  if ( gridptr->ysize != ysize )
    {
      gridMark4Update(gridID);
      gridptr->ysize = ysize;
    }

  if ( gridInqType(gridID) != GRID_UNSTRUCTURED )
    {
      long axisproduct = gridptr->xsize*gridptr->ysize;
      if ( axisproduct > 0 && axisproduct != gridSize )
        Error("Inconsistent grid declaration! (xsize=%d ysize=%d gridsize=%d)",
              gridptr->xsize, gridptr->ysize, gridSize);
    }
}

/*
@Function  gridInqYsize
@Title     Get the number of values of a Y-axis

@Prototype int gridInqYsize(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqYsize} returns the number of values of a Y-axis.

@Result
@func{gridInqYsize} returns the number of values of a Y-axis.

@EndFunction
*/
int gridInqYsize(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->ysize;
}

/*
@Function  gridDefNP
@Title     Define the number of parallels between a pole and the equator

@Prototype void gridDefNP(int gridID, int np)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  np       Number of parallels between a pole and the equator.

@Description
The function @func{gridDefNP} defines the number of parallels between a pole and the equator
of a Gaussian grid.

@EndFunction
*/
void gridDefNP(int gridID, int np)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->np != np )
    {
      gridMark4Update(gridID);
      gridptr->np = np;
    }
}

/*
@Function  gridInqNP
@Title     Get the number of parallels between a pole and the equator

@Prototype int gridInqNP(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqNP} returns the number of parallels between a pole and the equator
of a Gaussian grid.

@Result
@func{gridInqNP} returns the number of parallels between a pole and the equator.

@EndFunction
*/
int gridInqNP(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->np;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefRowlon(int gridID, int nrowlon, const int rowlon[])
{
  grid_t *gridptr = gridID2Ptr(gridID);

  gridptr->rowlon = (int *) Malloc((size_t)nrowlon * sizeof(int));
  gridptr->nrowlon = nrowlon;
  memcpy(gridptr->rowlon, rowlon, (size_t)nrowlon * sizeof(int));
  gridMark4Update(gridID);
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridInqRowlon(int gridID, int *rowlon)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->rowlon == 0 )  Error("undefined pointer!");

  memcpy(rowlon, gridptr->rowlon, (size_t)gridptr->nrowlon * sizeof(int));
}

static int
gridInqMaskSerialGeneric(grid_t *gridptr, mask_t **internalMask,
                        int *restrict mask)
{
  long size = gridptr->size;

  if ( CDI_Debug && size == 0 )
    Warning("Size undefined for gridID = %d", gridptr->self);

  const mask_t *restrict mask_src = *internalMask;
  if ( mask_src )
    {
      if (mask && size > 0)
        for (size_t i = 0; i < (size_t)size; ++i)
          mask[i] = (int)mask_src[i];
    }
  else
    size = 0;

  return (int)size;
}

static int
gridInqMaskSerial(grid_t *gridptr, int *mask)
{
  return gridInqMaskSerialGeneric(gridptr, &gridptr->mask, mask);
}


int gridInqMask(int gridID, int *mask)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqMask(gridptr, mask);
}

static void
gridDefMaskSerial(grid_t *gridptr, const int *mask)
{
  long size = gridptr->size;

  if ( size == 0 )
    Error("Size undefined for gridID = %d", gridptr->self);

  if ( mask == NULL )
    {
      if ( gridptr->mask )
	{
	  Free(gridptr->mask);
	  gridptr->mask = NULL;
	}
    }
  else
    {
      if ( gridptr->mask == NULL )
	gridptr->mask = (mask_t *) Malloc((size_t)size*sizeof(mask_t));
      else if ( CDI_Debug )
	Warning("grid mask already defined!");

      for (long i = 0; i < size; ++i )
	gridptr->mask[i] = (mask_t)(mask[i] != 0);
    }
}

void gridDefMask(int gridID, const int *mask)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defMask(gridptr, mask);
  gridMark4Update(gridID);
}

static int
gridInqMaskGMESerial(grid_t *gridptr, int *mask_gme)
{
  return gridInqMaskSerialGeneric(gridptr, &gridptr->mask_gme, mask_gme);
}

int gridInqMaskGME(int gridID, int *mask)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqMaskGME(gridptr, mask);
}

static void
gridDefMaskGMESerial(grid_t *gridptr, const int *mask)
{
  long size = gridptr->size;

  if ( size == 0 )
    Error("Size undefined for gridID = %d", gridptr->self);

  if ( gridptr->mask_gme == NULL )
    gridptr->mask_gme = (mask_t *) Malloc((size_t)size * sizeof (mask_t));
  else if ( CDI_Debug )
    Warning("mask already defined!");

  for (long i = 0; i < size; ++i)
    gridptr->mask_gme[i] = (mask_t)(mask[i] != 0);
}

void gridDefMaskGME(int gridID, const int *mask)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defMaskGME(gridptr, mask);
  gridMark4Update(gridID);
}


static int
gridInqXValsSerial(grid_t *gridptr, double *xvals)
{
  long size;
  if ( gridptr->type == GRID_CURVILINEAR || gridptr->type == GRID_UNSTRUCTURED )
    size = gridptr->size;
  else if ( gridptr->type == GRID_GAUSSIAN_REDUCED )
    size = 2;
  else
    size = gridptr->xsize;

  if ( CDI_Debug && size == 0 )
    Warning("size undefined for gridID = %d", gridptr->self);

  if ( gridptr->xvals )
    {
      if ( size && xvals )
        {
          const double *gridptr_xvals = gridptr->vtable->inqXValsPtr(gridptr);
          memcpy(xvals, gridptr_xvals, (size_t)size * sizeof (double));
        }
    }
  else
    size = 0;

  return (int)size;
}

/*
@Function  gridInqXvals
@Title     Get all values of a X-axis

@Prototype int gridInqXvals(int gridID, double *xvals)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  xvals    Pointer to the location into which the X-values are read.
                    The caller must allocate space for the returned values.

@Description
The function @func{gridInqXvals} returns all values of the X-axis.

@Result
Upon successful completion @func{gridInqXvals} returns the number of values and
the values are stored in @func{xvals}.
Otherwise, 0 is returned and @func{xvals} is empty.

@EndFunction
*/
int gridInqXvals(int gridID, double *xvals)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqXVals(gridptr, xvals);
}


static void
gridDefXValsSerial(grid_t *gridptr, const double *xvals)
{
  int gridtype = gridptr->type;

  long size;
  if ( gridtype == GRID_UNSTRUCTURED || gridtype == GRID_CURVILINEAR )
    size = gridptr->size;
  else if ( gridtype == GRID_GAUSSIAN_REDUCED )
    size = 2;
  else
    size = gridptr->xsize;

  if ( size == 0 )
    Error("Size undefined for gridID = %d", gridptr->self);

  if (gridptr->xvals && CDI_Debug)
    Warning("values already defined!");
  gridptr->xvals = (double *)Realloc(gridptr->xvals,
                                      (size_t)size * sizeof(double));
  memcpy(gridptr->xvals, xvals, (size_t)size * sizeof (double));
}

/*
@Function  gridDefXvals
@Title     Define the values of a X-axis

@Prototype void gridDefXvals(int gridID, const double *xvals)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  xvals    X-values of the grid.

@Description
The function @func{gridDefXvals} defines all values of the X-axis.

@EndFunction
*/
void gridDefXvals(int gridID, const double *xvals)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defXVals(gridptr, xvals);
  gridMark4Update(gridID);
}

static int
gridInqYValsSerial(grid_t *gridptr, double *yvals)
{
  int gridtype = gridptr->type;
  long size
    = (gridtype == GRID_CURVILINEAR || gridtype == GRID_UNSTRUCTURED)
    ? gridptr->size : gridptr->ysize;

  if ( CDI_Debug && size == 0 )
    Warning("size undefined for gridID = %d!", gridptr->self);

  if ( gridptr->yvals )
    {
      if ( size && yvals )
        {
          const double *gridptr_yvals = gridptr->vtable->inqYValsPtr(gridptr);
          memcpy(yvals, gridptr_yvals, (size_t)size * sizeof (double));
        }
    }
  else
    size = 0;

  return (int)size;
}

/*
@Function  gridInqYvals
@Title     Get all values of a Y-axis

@Prototype int gridInqYvals(int gridID, double *yvals)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  yvals    Pointer to the location into which the Y-values are read.
                    The caller must allocate space for the returned values.

@Description
The function @func{gridInqYvals} returns all values of the Y-axis.

@Result
Upon successful completion @func{gridInqYvals} returns the number of values and
the values are stored in @func{yvals}.
Otherwise, 0 is returned and @func{yvals} is empty.

@EndFunction
*/
int gridInqYvals(int gridID, double *yvals)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqYVals(gridptr, yvals);
}

static void
gridDefYValsSerial(grid_t *gridptr, const double *yvals)
{
  int gridtype = gridptr->type;
  long size
    = (gridtype == GRID_CURVILINEAR || gridtype == GRID_UNSTRUCTURED)
    ? gridptr->size : gridptr->ysize;

  if ( size == 0 )
    Error("Size undefined for gridID = %d!", gridptr->self);

  if (gridptr->yvals && CDI_Debug)
    Warning("Values already defined!");

  gridptr->yvals = (double *)Realloc(gridptr->yvals, (size_t)size * sizeof (double));
  memcpy(gridptr->yvals, yvals, (size_t)size * sizeof (double));
}


/*
@Function  gridDefYvals
@Title     Define the values of a Y-axis

@Prototype void gridDefYvals(int gridID, const double *yvals)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  yvals    Y-values of the grid.

@Description
The function @func{gridDefYvals} defines all values of the Y-axis.

@EndFunction
*/
void gridDefYvals(int gridID, const double *yvals)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defYVals(gridptr, yvals);
  gridMark4Update(gridID);
}

static double
gridInqXValSerial(grid_t *gridptr, int index)
{
  double xval = gridptr->xvals ? gridptr->xvals[index] : 0;
  return xval;
}


double gridInqXval(int gridID, int index)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqXVal(gridptr, index);
}

static double
gridInqYValSerial(grid_t *gridptr, int index)
{
  double yval = gridptr->yvals ? gridptr->yvals[index] : 0;
  return yval;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqYval(int gridID, int index)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqYVal(gridptr, index);
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqXinc(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  double xinc = gridptr->xinc;
  const double *restrict xvals = gridptr->vtable->inqXValsPtr(gridptr);

  if ( (! (fabs(xinc) > 0)) && xvals )
    {
      int xsize = gridptr->xsize;
      if ( xsize > 1 )
        {
          xinc = fabs(xvals[xsize-1] - xvals[0])/(xsize-1);
          for (size_t i = 2; i < (size_t)xsize; i++ )
            if ( fabs(fabs(xvals[i-1] - xvals[i]) - xinc) > 0.01*xinc )
              {
                xinc = 0;
                break;
              }

          gridptr->xinc = xinc;
        }
    }

  return xinc;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqYinc(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  double yinc = gridptr->yinc;
  const double *yvals = gridptr->vtable->inqYValsPtr(gridptr);

  if ( (! (fabs(yinc) > 0)) && yvals )
    {
      int ysize = gridptr->ysize;
      if ( ysize > 1 )
        {
          yinc = yvals[1] - yvals[0];
          double abs_yinc = fabs(yinc);
          for ( size_t i = 2; i < (size_t)ysize; i++ )
            if ( fabs(fabs(yvals[i] - yvals[i-1]) - abs_yinc) > (0.01*abs_yinc) )
              {
                yinc = 0;
                break;
              }

          gridptr->yinc = yinc;
        }
    }

  return yinc;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqXpole(int gridID)
{
  // Xpole -> grid_north_pole_longitude
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->xpole;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefXpole(int gridID, double xpole)
{
  // Xpole -> grid_north_pole_longitude
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->xstdname && memcmp(gridptr->xstdname, "grid", 4) != 0 )
    gridptr->xstdname = xystdname_tab[grid_xystdname_grid_latlon][0];

  if ( gridptr->isRotated != TRUE || IS_NOT_EQUAL(gridptr->xpole, xpole) )
    {
      gridptr->isRotated = TRUE;
      gridptr->xpole = xpole;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqYpole(int gridID)
{
  // Ypole -> grid_north_pole_latitude
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->ypole;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefYpole(int gridID, double ypole)
{
  // Ypole -> grid_north_pole_latitude
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->ystdname && memcmp(gridptr->ystdname, "grid", 4) != 0 )
    gridptr->ystdname = xystdname_tab[grid_xystdname_grid_latlon][1];

  if ( gridptr->isRotated != TRUE || IS_NOT_EQUAL(gridptr->ypole, ypole) )
    {
      gridptr->isRotated = TRUE;
      gridptr->ypole = ypole;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
double gridInqAngle(int gridID)
{
  // Angle -> north_pole_grid_longitude
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->angle;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction<
*/
void gridDefAngle(int gridID, double angle)
{
  // Angle -> north_pole_grid_longitude
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->isRotated != TRUE || IS_NOT_EQUAL(gridptr->angle, angle) )
    {
      gridptr->isRotated = TRUE;
      gridptr->angle = angle;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
int gridInqGMEnd(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->nd;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefGMEnd(int gridID, int nd)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if (gridptr->nd != nd)
    {
      gridptr->nd = nd;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
int gridInqGMEni(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->ni;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefGMEni(int gridID, int ni)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->ni != ni )
    {
      gridptr->ni = ni;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
int gridInqGMEni2(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->ni2;
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridDefGMEni2(int gridID, int ni2)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->ni2 != ni2 )
    {
      gridptr->ni2 = ni2;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
int gridInqGMEni3(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->ni3;
}

void gridDefGMEni3(int gridID, int ni3)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->ni3 != ni3 )
    {
      gridptr->ni3 = ni3;
      gridMark4Update(gridID);
    }
}

/*
@Function
@Title

@Prototype
@Parameter
    @Item  Grid identifier

@EndFunction
*/
void gridChangeType(int gridID, int gridtype)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( CDI_Debug )
    Message("Changed grid type from %s to %s", gridNamePtr(gridptr->type), gridNamePtr(gridtype));

  if (gridptr->type != gridtype)
    {
      gridptr->type = gridtype;
      gridMark4Update(gridID);
    }
}

static
void grid_check_cyclic(grid_t *gridptr)
{
  gridptr->isCyclic = FALSE;
  enum { numVertices = 4 };
  size_t xsize = gridptr->xsize >= 0 ? (size_t)gridptr->xsize : 0,
    ysize = gridptr->ysize >= 0 ? (size_t)gridptr->ysize : 0;
  const double *xvals = gridptr->vtable->inqXValsPtr(gridptr),
    (*xbounds)[numVertices]
    = (const double (*)[numVertices])gridptr->vtable->inqXBoundsPtr(gridptr);

  if ( gridptr->type == GRID_GAUSSIAN || gridptr->type == GRID_LONLAT )
    {
      if ( xvals && xsize > 1 )
        {
          double xinc = xvals[1] - xvals[0];
          if ( IS_EQUAL(xinc, 0) )
            xinc = (xvals[xsize-1] - xvals[0])/(double)(xsize-1);

          double x0 = 2*xvals[xsize-1]-xvals[xsize-2]-360;

          if ( IS_NOT_EQUAL(xvals[0], xvals[xsize-1]) )
            if ( fabs(x0 - xvals[0]) < 0.01*xinc ) gridptr->isCyclic = TRUE;
        }
    }
  else if ( gridptr->type == GRID_CURVILINEAR )
    {
      if ( xvals && xsize > 1 )
        {
          size_t nc = 0;
          for ( size_t j = 0; j < ysize; ++j )
            {
              size_t i1 = j*xsize,
                i2 = j*xsize+1,
                in = j*xsize+(xsize-1);
              double val1 = xvals[i1],
                val2 = xvals[i2],
                valn = xvals[in];

              double xinc = fabs(val2-val1);

	      if ( val1 <    1 && valn > 300 ) val1 += 360;
	      if ( valn <    1 && val1 > 300 ) valn += 360;
	      if ( val1 < -179 && valn > 120 ) val1 += 360;
	      if ( valn < -179 && val1 > 120 ) valn += 360;
              if ( fabs(valn-val1) > 180 ) val1 += 360;

              double x0 = valn + copysign(xinc, val1 - valn);

              nc += fabs(x0-val1) < 0.5*xinc;
            }
          gridptr->isCyclic = nc > ysize/2 ? TRUE : FALSE;
        }

      if ( xbounds && xsize > 1 )
	{
          bool isCyclic = true;
	  for ( size_t j = 0; j < ysize; ++j )
	    {
	      size_t i1 = j*xsize,
                i2 = j*xsize+(xsize-1);
	      for (size_t k1 = 0; k1 < numVertices; ++k1 )
		{
		  double val1 = xbounds[i1][k1];
		  for (size_t k2 = 0; k2 < numVertices; ++k2 )
		    {
		      double val2 = xbounds[i2][k2];

		      if ( val1 <    1 && val2 > 300 ) val1 += 360;
		      if ( val2 <    1 && val1 > 300 ) val2 += 360;
		      if ( val1 < -179 && val2 > 120 ) val1 += 360;
		      if ( val2 < -179 && val1 > 120 ) val2 += 360;
                      if ( fabs(val2-val1) > 180 ) val1 += 360;

		      if ( fabs(val1-val2) < 0.001 )
                        goto foundCloseVertices;
		    }
		}
              /* all vertices more than 0.001 degrees apart */
              isCyclic = false;
              break;
              foundCloseVertices:
              ;
	    }
          gridptr->isCyclic = (int) isCyclic;
	}
    }
}


int gridIsCircular(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->isCyclic == CDI_UNDEFID ) grid_check_cyclic(gridptr);

  return gridptr->isCyclic;
}


int gridIsRotated(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->isRotated;
}

static
bool compareXYvals(grid_t *gridRef, grid_t *gridTest)
{
  bool differ = false;

  int xsizeTest = gridTest->xsize, ysizeTest = gridTest->ysize;
  if ( !differ && xsizeTest > 0 && xsizeTest == gridRef->vtable->inqXVals(gridRef, NULL) )
    {
      const double *restrict xvalsRef = gridRef->vtable->inqXValsPtr(gridRef),
        *restrict xvalsTest = gridTest->vtable->inqXValsPtr(gridTest);

      for ( size_t i = 0; i < (size_t)xsizeTest; ++i )
	if ( fabs(xvalsTest[i] - xvalsRef[i]) > 1.e-10 )
	  {
	    differ = true;
	    break;
	  }
    }

  if ( !differ && ysizeTest > 0 && ysizeTest == gridRef->vtable->inqYVals(gridRef, NULL) )
    {
      const double *restrict yvalsRef = gridRef->vtable->inqYValsPtr(gridRef),
        *restrict yvalsTest = gridTest->vtable->inqYValsPtr(gridTest);
      for ( size_t i = 0; i < (size_t)ysizeTest; ++i )
	if ( fabs(yvalsTest[i] - yvalsRef[i]) > 1.e-10 )
	  {
	    differ = true;
	    break;
	  }
    }

  return differ;
}

static
bool compareXYvals2(grid_t *gridRef, grid_t *gridTest)
{
  int gridsize = gridTest->size;
  bool differ
    = ((gridTest->xvals == NULL) ^ (gridRef->xvals == NULL))
    || ((gridTest->yvals == NULL) ^ (gridRef->yvals == NULL));

  typedef double (*inqVal)(grid_t *grid, int index);
  inqVal inqXValRef = gridRef->vtable->inqXVal,
    inqYValRef = gridRef->vtable->inqXVal,
    inqXValTest = gridTest->vtable->inqXVal,
    inqYValTest = gridTest->vtable->inqYVal;

  if ( !differ && gridTest->xvals )
    differ = fabs(inqXValTest(gridTest, 0) - inqXValRef(gridRef, 0)) > 1.e-9
      || fabs(inqXValTest(gridTest, gridsize-1) - inqXValRef(gridRef, gridsize-1)) > 1.e-9;

  if ( !differ && gridTest->yvals )
    differ = fabs(inqYValTest(gridTest, 0) - inqYValRef(gridRef, 0)) > 1.e-9
      || fabs(inqYValTest(gridTest, gridsize-1) - inqYValRef(gridRef, gridsize-1)) > 1.e-9;

  return differ;
}

static
bool gridCompare(int gridID, const grid_t *grid)
{
  bool differ = true;
  grid_t *gridRef = gridID2Ptr(gridID);

  if ( grid->type == gridRef->type || grid->type == GRID_GENERIC )
    {
      if ( grid->size == gridRef->size )
	{
	  differ = false;
	  if ( grid->type == GRID_LONLAT )
	    {
	      /*
	      printf("gridID      %d\n", gridID);
	      printf("grid.xdef   %d\n", grid->xdef);
	      printf("grid.ydef   %d\n", grid->ydef);
	      printf("grid.xsize  %d\n", grid->xsize);
	      printf("grid.ysize  %d\n", grid->ysize);
	      printf("grid.xfirst %f\n", grid->xfirst);
	      printf("grid.yfirst %f\n", grid->yfirst);
	      printf("grid.xfirst %f\n", gridInqXval(gridID, 0));
	      printf("grid.yfirst %f\n", gridInqYval(gridID, 0));
	      printf("grid.xinc   %f\n", grid->xinc);
	      printf("grid.yinc   %f\n", grid->yinc);
	      printf("grid.xinc   %f\n", gridInqXinc(gridID));
	      printf("grid.yinc   %f\n", gridInqYinc(gridID));
	      */
	      if ( grid->xsize == gridRef->xsize && grid->ysize == gridRef->ysize )
		{
		  if ( grid->xdef == 2 && grid->ydef == 2 )
		    {
		      if ( ! (IS_EQUAL(grid->xfirst, 0) && IS_EQUAL(grid->xlast, 0) && IS_EQUAL(grid->xinc, 0)) &&
			   ! (IS_EQUAL(grid->yfirst, 0) && IS_EQUAL(grid->ylast, 0) && IS_EQUAL(grid->yinc, 0)) &&
			   IS_NOT_EQUAL(grid->xfirst, grid->xlast) && IS_NOT_EQUAL(grid->yfirst, grid->ylast) )
			{
			  if ( IS_NOT_EQUAL(grid->xfirst, gridInqXval(gridID, 0)) ||
			       IS_NOT_EQUAL(grid->yfirst, gridInqYval(gridID, 0)))
			    {
			      differ = true;
			    }
			  if ( !differ && fabs(grid->xinc) > 0 &&
			       fabs(fabs(grid->xinc) - fabs(gridRef->xinc)) > fabs(grid->xinc/1000))
			    {
			      differ = true;
			    }
			  if ( !differ && fabs(grid->yinc) > 0 &&
			       fabs(fabs(grid->yinc) - fabs(gridRef->yinc)) > fabs(grid->yinc/1000))
			    {
			      differ = true;
			    }
			}
		    }
		  else if ( grid->xvals && grid->yvals )
                    differ = gridRef->vtable->compareXYFull(gridRef, (grid_t *)grid);
		}
	      else
		differ = true;
	    }
	  else if ( grid->type == GRID_GENERIC )
	    {
	      if ( grid->xsize == gridRef->xsize && grid->ysize == gridRef->ysize )
		{
		  if ( grid->xdef == 1 && grid->ydef == 1
                       && grid->xvals && grid->yvals )
                    differ = gridRef->vtable->compareXYFull(gridRef, (grid_t *)grid);
		}
	      else if ( (grid->ysize == 0 || grid->ysize == 1) &&
			grid->xsize == gridRef->xsize*gridRef->ysize )
		{
		}
	      else
		differ = true;
	    }
	  else if ( grid->type == GRID_GAUSSIAN )
	    {
	      if ( grid->xsize == gridRef->xsize && grid->ysize == gridRef->ysize )
		{
		  if ( grid->xdef == 2 && grid->ydef == 2 )
		    {
		      if ( ! (IS_EQUAL(grid->xfirst, 0) && IS_EQUAL(grid->xlast, 0) && IS_EQUAL(grid->xinc, 0)) &&
			   ! (IS_EQUAL(grid->yfirst, 0) && IS_EQUAL(grid->ylast, 0)) )
			if ( fabs(grid->xfirst - gridInqXval(gridID, 0)) > 0.0015 ||
			     fabs(grid->yfirst - gridInqYval(gridID, 0)) > 0.0015 ||
			     (fabs(grid->xinc)>0 && fabs(fabs(grid->xinc) - fabs(gridRef->xinc)) > fabs(grid->xinc/1000)) )
			  {
			    differ = true;
			  }
		    }
		  else if ( grid->xvals && grid->yvals )
                    differ = gridRef->vtable->compareXYFull(gridRef, (grid_t *)grid);
		}
	      else
		differ = true;
	    }
	  else if ( grid->type == GRID_CURVILINEAR )
	    {
	      /*
	      printf("gridID      %d\n", gridID);
	      printf("grid.xsize  %d\n", grid->xsize);
	      printf("grid.ysize  %d\n", grid->ysize);
	      printf("grid.xfirst %f\n", grid->xvals[0]);
	      printf("grid.yfirst %f\n", grid->yvals[0]);
	      printf("grid xfirst %f\n", gridInqXval(gridID, 0));
	      printf("grid yfirst %f\n", gridInqYval(gridID, 0));
	      printf("grid.xlast  %f\n", grid->xvals[grid->size-1]);
	      printf("grid.ylast  %f\n", grid->yvals[grid->size-1]);
	      printf("grid xlast  %f\n", gridInqXval(gridID, grid->size-1));
	      printf("grid ylast  %f\n", gridInqYval(gridID, grid->size-1));
	      printf("grid.nv     %d\n", grid->nvertex);
	      printf("grid nv     %d\n", gridInqNvertex(gridID));
	      */
	      if ( grid->xsize == gridRef->xsize && grid->ysize == gridRef->ysize )
                differ = gridRef->vtable->compareXYAO(gridRef, (grid_t *)grid);
	    }
	  else if ( grid->type == GRID_UNSTRUCTURED )
	    {
              /* FIXME: not octet 0 but octet 7 is guaranteed  non-zero
               * for any non-NULL UUID */
              differ = differ || ( gridRef->uuid[0] && grid->uuid[0] && memcmp(gridRef->uuid, grid->uuid, CDI_UUID_SIZE) != 0 );

              if ( !differ &&
                   ((grid->xvals == NULL) ^ (gridRef->xvals == NULL)) &&
                   ((grid->yvals == NULL) ^ (gridRef->yvals == NULL)) )
                {
                  int nvertexA, nvertexB, numberA, numberB;
                  differ = ( (nvertexA = grid->nvertex)
                             && (nvertexB = gridRef->nvertex)
                             && (nvertexA != nvertexB) )
                    || (numberA = grid->number, numberB = gridRef->number,
                        ( (numberA)
                          && numberB
                          && (numberA != numberB) )
                        || ( (numberA && numberB)
                             && (grid->position) != (gridRef->position) ) );
                }
              else if ( !differ )
                {
                  differ = grid->nvertex != gridRef->nvertex
                    || grid->number != gridRef->number
                    || (grid->number > 0 && grid->position != gridRef->position)
                    || gridRef->vtable->compareXYAO(gridRef, (grid_t *)grid);
                }
              }
	}
    }

  return differ;
}


int gridCompareP ( void * gridptr1, void * gridptr2 )
{
  grid_t * g1 = ( grid_t * ) gridptr1;
  grid_t * g2 = ( grid_t * ) gridptr2;
  enum { equal = 0,
         differ = -1 };
  int i, size;

  xassert ( g1 );
  xassert ( g2 );

  if ( g1->type          != g2->type         ) return differ;
  if ( g1->prec          != g2->prec         ) return differ;
  if ( g1->lcc_projflag  != g2->lcc_projflag ) return differ;
  if ( g1->lcc_scanflag  != g2->lcc_scanflag ) return differ;
  if ( g1->lcc_defined   != g2->lcc_defined  ) return differ;
  if ( g1->lcc2_defined  != g2->lcc2_defined ) return differ;
  if ( g1->laea_defined  != g2->laea_defined ) return differ;
  if ( g1->isCyclic      != g2->isCyclic     ) return differ;
  if ( g1->isRotated     != g2->isRotated    ) return differ;
  if ( g1->xdef          != g2->xdef         ) return differ;
  if ( g1->ydef          != g2->ydef         ) return differ;
  if ( g1->nd            != g2->nd           ) return differ;
  if ( g1->ni            != g2->ni           ) return differ;
  if ( g1->ni2           != g2->ni2          ) return differ;
  if ( g1->ni3           != g2->ni3          ) return differ;
  if ( g1->number        != g2->number       ) return differ;
  if ( g1->position      != g2->position     ) return differ;
  if ( g1->trunc         != g2->trunc        ) return differ;
  if ( g1->nvertex       != g2->nvertex      ) return differ;
  if ( g1->nrowlon       != g2->nrowlon      ) return differ;
  if ( g1->size          != g2->size         ) return differ;
  if ( g1->xsize         != g2->xsize        ) return differ;
  if ( g1->ysize         != g2->ysize        ) return differ;
  if ( g1->lcomplex      != g2->lcomplex     ) return differ;

  if ( IS_NOT_EQUAL(g1->xfirst        , g2->xfirst)        ) return differ;
  if ( IS_NOT_EQUAL(g1->yfirst	      , g2->yfirst)        ) return differ;
  if ( IS_NOT_EQUAL(g1->xlast         , g2->xlast)         ) return differ;
  if ( IS_NOT_EQUAL(g1->ylast         , g2->ylast)         ) return differ;
  if ( IS_NOT_EQUAL(g1->xinc	      , g2->xinc)          ) return differ;
  if ( IS_NOT_EQUAL(g1->yinc	      , g2->yinc)          ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_originLon , g2->lcc_originLon) ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_originLat , g2->lcc_originLat) ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_lonParY   , g2->lcc_lonParY)   ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_lat1      , g2->lcc_lat1)      ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_lat2      , g2->lcc_lat2)      ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_xinc      , g2->lcc_xinc)      ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc_yinc      , g2->lcc_yinc)      ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc2_lon_0    , g2->lcc2_lon_0)    ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc2_lat_0    , g2->lcc2_lat_0)    ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc2_lat_1    , g2->lcc2_lat_1)    ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc2_lat_2    , g2->lcc2_lat_2)    ) return differ;
  if ( IS_NOT_EQUAL(g1->lcc2_a        , g2->lcc2_a)        ) return differ;
  if ( IS_NOT_EQUAL(g1->laea_lon_0    , g2->laea_lon_0)    ) return differ;
  if ( IS_NOT_EQUAL(g1->laea_lat_0    , g2->laea_lat_0)    ) return differ;
  if ( IS_NOT_EQUAL(g1->laea_a        , g2->laea_a)        ) return differ;
  if ( IS_NOT_EQUAL(g1->xpole         , g2->xpole)         ) return differ;
  if ( IS_NOT_EQUAL(g1->ypole         , g2->ypole)         ) return differ;
  if ( IS_NOT_EQUAL(g1->angle         , g2->angle)         ) return differ;

  const double *restrict g1_xvals = g1->vtable->inqXValsPtr(g1),
    *restrict g2_xvals = g2->vtable->inqXValsPtr(g2);
  if ( g1_xvals )
    {
      if ( g1->type == GRID_UNSTRUCTURED || g1->type == GRID_CURVILINEAR )
        size = g1->size;
      else
        size = g1->xsize;
      xassert ( size );

      if ( !g2_xvals ) return differ;

      for ( i = 0; i < size; i++ )
        if ( IS_NOT_EQUAL(g1_xvals[i], g2_xvals[i]) ) return differ;
    }
  else if ( g2_xvals )
    return differ;

  const double *restrict g1_yvals = g1->vtable->inqYValsPtr(g1),
    *restrict g2_yvals = g2->vtable->inqYValsPtr(g2);
  if ( g1_yvals )
    {
      if ( g1->type == GRID_UNSTRUCTURED || g1->type == GRID_CURVILINEAR )
	size = g1->size;
      else
	size = g1->ysize;
      xassert ( size );

      if ( !g2_yvals ) return differ;

      for ( i = 0; i < size; i++ )
        if ( IS_NOT_EQUAL(g1_yvals[i], g2_yvals[i]) ) return differ;
    }
  else if ( g2_yvals )
    return differ;

  const double *restrict g1_area = g1->vtable->inqAreaPtr(g1),
    *restrict g2_area = g2->vtable->inqAreaPtr(g2);
  if ( g1_area )
    {
      xassert ( g1->size );

      if ( !g2_area ) return differ;

      for ( i = 0; i < g1->size; i++ )
	if ( IS_NOT_EQUAL(g1_area[i], g2_area[i]) ) return differ;
    }
  else if ( g2_area )
    return differ;

  {
    const double *restrict g1_xbounds, *restrict g2_xbounds;
    if ( (g1_xbounds = g1->vtable->inqXBoundsPtr(g1)) )
      {
        xassert ( g1->nvertex );
        if ( g1->type == GRID_CURVILINEAR || g1->type == GRID_UNSTRUCTURED )
          size = g1->nvertex * g1->size;
        else
          size = g1->nvertex * g1->xsize;
        xassert ( size );

        if ( !(g2_xbounds = g2->vtable->inqXBoundsPtr(g2)) ) return differ;

        for ( i = 0; i < size; i++ )
          if ( IS_NOT_EQUAL(g1_xbounds[i], g2_xbounds[i]) ) return differ;
      }
    else if ( g2->vtable->inqXBoundsPtr(g2) )
      return differ;
  }

  {
    const double *restrict g1_ybounds, *restrict g2_ybounds;
    if ( (g1_ybounds = g1->vtable->inqYBoundsPtr(g1)) )
      {
        xassert ( g1->nvertex );
        if ( g1->type == GRID_CURVILINEAR || g1->type == GRID_UNSTRUCTURED )
          size = g1->nvertex * g1->size;
        else
          size = g1->nvertex * g1->ysize;
        xassert ( size );

        if ( ! (g2_ybounds = g2->vtable->inqYBoundsPtr(g2)) ) return differ;

        for ( i = 0; i < size; i++ )
          if ( IS_NOT_EQUAL(g1->ybounds[i], g2->ybounds[i]) ) return differ;
      }
    else if ( g2->vtable->inqYBoundsPtr(g2) )
      return differ;
  }

  if (strcmp(g1->xname, g2->xname)) return differ;
  if (strcmp(g1->yname, g2->yname)) return differ;
  if (strcmp(g1->xlongname, g2->xlongname)) return differ;
  if (strcmp(g1->ylongname, g2->ylongname)) return differ;
  if (g1->xstdname != g2->xstdname) return differ;
  if (g1->ystdname != g2->ystdname) return differ;
  if (strcmp(g1->xunits, g2->xunits)) return differ;
  if (strcmp(g1->yunits, g2->yunits)) return differ;

  if ( g1->reference )
    {
      if ( !g2->reference ) return differ;
      if ( strcmp(g1->reference, g2->reference) ) return differ;
    }
  else if ( g2->reference )
    return differ;

  if ( g1->mask )
    {
      xassert ( g1->size );
      if ( !g2->mask ) return differ;
      if ( memcmp ( g1->mask, g2->mask, (size_t)g1->size * sizeof(mask_t)) ) return differ;
    }
  else if ( g2->mask )
    return differ;

  if ( g1->mask_gme )
    {
      xassert ( g1->size );
      if ( !g2->mask_gme ) return differ;
      if ( memcmp ( g1->mask_gme, g2->mask_gme, (size_t)g1->size * sizeof(mask_t)) ) return differ;
    }
  else if ( g2->mask_gme )
    return differ;

  if (memcmp(g1->uuid, g2->uuid, CDI_UUID_SIZE))
    return differ;

  return equal;
}

static void gridComplete(grid_t *grid)
{
  int gridID = grid->self;
  gridDefPrec(gridID, grid->prec);

  int gridtype = grid->type;
  switch (gridtype)
    {
    case GRID_LONLAT:
    case GRID_GAUSSIAN:
    case GRID_UNSTRUCTURED:
    case GRID_CURVILINEAR:
    case GRID_GENERIC:
    case GRID_LCC:
    case GRID_LCC2:
    case GRID_SINUSOIDAL:
    case GRID_LAEA:
    case GRID_PROJECTION:
      {
	if ( grid->xsize > 0 ) gridDefXsize(gridID, grid->xsize);
	if ( grid->ysize > 0 ) gridDefYsize(gridID, grid->ysize);

        if ( gridtype == GRID_GAUSSIAN ) gridDefNP(gridID, grid->np);

	if ( grid->nvertex > 0 )
	  gridDefNvertex(gridID, grid->nvertex);

	if ( grid->xdef == 2 )
	  {
            assert(gridtype != GRID_UNSTRUCTURED
                   && gridtype != GRID_CURVILINEAR);
	    double *xvals
              = (double *) Malloc((size_t)grid->xsize * sizeof (double));
	    gridGenXvals(grid->xsize, grid->xfirst, grid->xlast, grid->xinc, xvals);
	    grid->xvals = xvals;
	    /*
	    gridDefXinc(gridID, grid->xinc);
	    */
	  }

	if ( grid->ydef == 2 )
	  {
            assert(gridtype != GRID_UNSTRUCTURED
                   && gridtype != GRID_CURVILINEAR);
	    double *yvals
              = (double *) Malloc((size_t)grid->ysize * sizeof (double));
	    gridGenYvals(gridtype, grid->ysize, grid->yfirst, grid->ylast, grid->yinc, yvals);
	    grid->yvals = yvals;
	    /*
	    gridDefYinc(gridID, grid->yinc);
	    */
	  }

	if ( grid->isRotated )
	  {
	    gridDefXname(gridID, "rlon");
	    gridDefYname(gridID, "rlat");
	    gridDefXlongname(gridID, "longitude in rotated pole grid");
	    gridDefYlongname(gridID, "latitude in rotated pole grid");
            grid->xstdname = xystdname_tab[grid_xystdname_grid_latlon][0];
            grid->ystdname = xystdname_tab[grid_xystdname_grid_latlon][1];
	    gridDefXunits(gridID, "degrees");
	    gridDefYunits(gridID, "degrees");

	    gridDefXpole(gridID, grid->xpole);
	    gridDefYpole(gridID, grid->ypole);
	    gridDefAngle(gridID, grid->angle);
	  }

        switch (gridtype)
          {
          case GRID_LAEA:
            gridDefLaea(gridID, grid->laea_a, grid->laea_lon_0, grid->laea_lat_0);
            break;
          case GRID_LCC2:
            gridDefLcc2(gridID, grid->lcc2_a, grid->lcc2_lon_0, grid->lcc2_lat_0, grid->lcc2_lat_1, grid->lcc2_lat_2);
            break;
          case GRID_LCC:
            gridDefLCC(gridID, grid->lcc_originLon, grid->lcc_originLat, grid->lcc_lonParY,
                       grid->lcc_lat1, grid->lcc_lat2, grid->lcc_xinc, grid->lcc_yinc,
                       grid->lcc_projflag, grid->lcc_scanflag);
            break;
          case GRID_UNSTRUCTURED:
            {
              int number = grid->number;
              int position = grid->position >= 0 ? grid->position : 0;
              if ( number > 0 )
                {
                  gridDefNumber(gridID, number);
                  gridDefPosition(gridID, position);
                }
            }
            break;
	  }

	break;
      }
    case GRID_GAUSSIAN_REDUCED:
      {
	gridDefNP(gridID, grid->np);
	gridDefYsize(gridID, grid->ysize);
        if ( grid->xdef == 2 )
          {
            double xvals[2] = { grid->xfirst, grid->xlast };
            gridDefXvals(gridID, xvals);
          }

        if ( grid->ydef == 2 )
	  {
	    double *yvals
              = (double *) Malloc((size_t)grid->ysize * sizeof (double));
	    gridGenYvals(gridtype, grid->ysize, grid->yfirst, grid->ylast, grid->yinc, yvals);
            grid->yvals = yvals;
	    /*
	    gridDefYinc(gridID, grid->yinc);
	    */
	  }
	break;
      }
    case GRID_SPECTRAL:
      {
        gridDefTrunc(gridID, grid->trunc);
        if ( grid->lcomplex ) gridDefComplexPacking(gridID, 1);
        break;
      }
    case GRID_FOURIER:
      {
	gridDefTrunc(gridID, grid->trunc);
	break;
      }
    case GRID_GME:
      {
        gridDefGMEnd(gridID, grid->nd);
        gridDefGMEni(gridID, grid->ni);
        gridDefGMEni2(gridID, grid->ni2);
        gridDefGMEni3(gridID, grid->ni3);
        break;
      }
      /*
    case GRID_GENERIC:
      {
        if ( grid->xsize > 0 && grid->ysize > 0 )
          {
            gridDefXsize(gridID, grid->xsize);
            gridDefYsize(gridID, grid->ysize);
            if ( grid->xvals ) gridDefXvals(gridID, grid->xvals);
            if ( grid->yvals ) gridDefYvals(gridID, grid->yvals);
          }
        break;
      }
      */
    case GRID_TRAJECTORY:
      {
        gridDefXsize(gridID, 1);
        gridDefYsize(gridID, 1);
        break;
      }
    default:
      {
	Error("Gridtype %s unsupported!", gridNamePtr(gridtype));
	break;
      }
    }

  grid->xname[CDI_MAX_NAME - 1] = 0;
  grid->xlongname[CDI_MAX_NAME - 1] = 0;
  grid->xunits[CDI_MAX_NAME - 1] = 0;
  grid->yname[CDI_MAX_NAME - 1] = 0;
  grid->ylongname[CDI_MAX_NAME - 1] = 0;
  grid->yunits[CDI_MAX_NAME - 1] = 0;
}

#define GRID_STR_SERIALIZE(gridP) { gridP->xdimname, gridP->ydimname,  \
    gridP->vdimname, gridP->xname, gridP->yname,  \
    gridP->xlongname, gridP->ylongname, \
    gridP->xunits, gridP->yunits }

int gridGenerate(const grid_t *grid)
{
  int gridtype = grid->type;
  int gridID = gridCreate(gridtype, grid->size);
  grid_t *restrict gridptr = gridID2Ptr(gridID);
  gridptr->prec = grid->prec;
  gridptr->xsize = grid->xsize;
  gridptr->ysize = grid->ysize;
  gridptr->np = grid->np;
  gridptr->nvertex = grid->nvertex;
  gridptr->xdef = grid->xdef;
  int valdef_group1 = 0;
  static const int valdef_group1_tab[] = {
    GRID_LONLAT, GRID_GAUSSIAN, GRID_UNSTRUCTURED, GRID_CURVILINEAR,
    GRID_GENERIC, GRID_LCC, GRID_LCC2, GRID_SINUSOIDAL, GRID_LAEA,
    GRID_PROJECTION
  };
  for ( size_t i = 0; i < sizeof (valdef_group1_tab) / sizeof (valdef_group1_tab[0]); ++i)
    valdef_group1 |= (gridtype == valdef_group1_tab[i]);
  if ( valdef_group1 && grid->xdef == 1 )
    {
      gridDefXvals(gridID, grid->xvals);
      if ( grid->xbounds )
        gridDefXbounds(gridID, grid->xbounds);
    }
  gridptr->xfirst = grid->xfirst;
  gridptr->xlast = grid->xlast;
  gridptr->xinc = grid->xinc;
  gridptr->ydef = grid->ydef;
  if ( (valdef_group1 || gridtype == GRID_GAUSSIAN_REDUCED) && grid->ydef == 1)
    {
      gridDefYvals(gridID, grid->yvals);
      if ( grid->ybounds )
        gridDefYbounds(gridID, grid->ybounds);
    }
  gridptr->yfirst = grid->yfirst;
  gridptr->ylast = grid->ylast;
  gridptr->yinc = grid->yinc;
  gridptr->isRotated = grid->isRotated;
  gridptr->xpole = grid->xpole;
  gridptr->ypole = grid->ypole;
  gridptr->angle = grid->angle;
  if ( valdef_group1 && grid->area)
    gridDefArea(gridID, grid->area);
  gridptr->laea_a = grid->laea_a;
  gridptr->laea_lon_0 = grid->laea_lon_0;
  gridptr->laea_lat_0 = grid->laea_lat_0;
  gridptr->lcc2_a = grid->lcc2_a;
  gridptr->lcc2_lon_0 = grid->lcc2_lon_0;
  gridptr->lcc2_lat_0 = grid->lcc2_lat_0;
  gridptr->lcc2_lat_1 = grid->lcc2_lat_1;
  gridptr->lcc2_lat_2 = grid->lcc2_lat_2;
  gridptr->lcc_originLon = grid->lcc_originLon;
  gridptr->lcc_originLat = grid->lcc_originLat;
  gridptr->lcc_lonParY = grid->lcc_lonParY;
  gridptr->lcc_lat1 = grid->lcc_lat1;
  gridptr->lcc_lat2 = grid->lcc_lat2;
  gridptr->lcc_xinc = grid->lcc_xinc;
  gridptr->lcc_yinc = grid->lcc_yinc;
  gridptr->lcc_projflag = grid->lcc_projflag;
  gridptr->lcc_scanflag = grid->lcc_scanflag;
  gridptr->number = grid->number;
  gridptr->position = grid->position;
  memcpy(gridptr->uuid, grid->uuid, CDI_UUID_SIZE);
  if ( gridtype == GRID_UNSTRUCTURED && grid->reference )
    gridDefReference(gridID, grid->reference);
  if ( gridtype == GRID_PROJECTION )
    gridptr->name = strdup(grid->name);
  if ( gridtype == GRID_GAUSSIAN_REDUCED )
    gridDefRowlon(gridID, grid->ysize, grid->rowlon);
  gridptr->trunc = grid->trunc;
  gridptr->lcomplex = grid->lcomplex;
  gridptr->nd = grid->nd;
  gridptr->ni = grid->ni;
  gridptr->ni2 = grid->ni2;
  gridptr->ni3 = grid->ni3;
  const char *grid_str_tab[] = GRID_STR_SERIALIZE(grid);
  char *gridptr_str_tab[] = GRID_STR_SERIALIZE(gridptr);
  for (size_t i = 0; i < sizeof (grid_str_tab) / sizeof (grid_str_tab[0]); ++i)
    if ( grid_str_tab[i][0] )
      memcpy(gridptr_str_tab[i], grid_str_tab[i], CDI_MAX_NAME);
  gridComplete(gridptr);
  return gridID;
}

static void
grid_copy_base_array_fields(grid_t *gridptrOrig, grid_t *gridptrDup)
{
  size_t nrowlon = (size_t)gridptrOrig->nrowlon;
  size_t gridsize = (size_t)gridptrOrig->size;
  int gridtype = gridptrOrig->type;
  int irregular = gridtype == GRID_CURVILINEAR || gridtype == GRID_UNSTRUCTURED;
  if ( nrowlon )
    {
      gridptrDup->rowlon = (int*) Malloc(nrowlon * sizeof(int));
      memcpy(gridptrDup->rowlon, gridptrOrig->rowlon, nrowlon * sizeof(int));
    }

  if ( gridptrOrig->xvals != NULL )
    {
      size_t size  = irregular ? gridsize : (size_t)gridptrOrig->xsize;

      gridptrDup->xvals = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->xvals, gridptrOrig->xvals, size * sizeof (double));
    }

  if ( gridptrOrig->yvals != NULL )
    {
      size_t size  = irregular ? gridsize : (size_t)gridptrOrig->ysize;

      gridptrDup->yvals = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->yvals, gridptrOrig->yvals, size * sizeof (double));
    }

  if ( gridptrOrig->xbounds != NULL )
    {
      size_t size  = (irregular ? gridsize : (size_t)gridptrOrig->xsize)
        * (size_t)gridptrOrig->nvertex;

      gridptrDup->xbounds = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->xbounds, gridptrOrig->xbounds, size * sizeof (double));
    }

  if ( gridptrOrig->ybounds != NULL )
    {
      size_t size = (irregular ? gridsize : (size_t)gridptrOrig->ysize)
        * (size_t)gridptrOrig->nvertex;

      gridptrDup->ybounds = (double *)Malloc(size * sizeof (double));
      memcpy(gridptrDup->ybounds, gridptrOrig->ybounds, size * sizeof (double));
    }

  {
    const double *gridptrOrig_area
      = gridptrOrig->vtable->inqAreaPtr(gridptrOrig);
    if ( gridptrOrig_area != NULL )
      {
        size_t size = gridsize;

        gridptrDup->area = (double *)Malloc(size * sizeof (double));
        memcpy(gridptrDup->area, gridptrOrig_area, size * sizeof (double));
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


/*
@Function  gridDuplicate
@Title     Duplicate a horizontal Grid

@Prototype int gridDuplicate(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridDuplicate} duplicates a horizontal Grid.

@Result
@func{gridDuplicate} returns an identifier to the duplicated Grid.

@EndFunction
*/
int gridDuplicate(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  grid_t *gridptrnew = gridptr->vtable->copy(gridptr);
  int gridIDnew = reshPut(gridptrnew, &gridOps);
  gridptrnew->self = gridIDnew;
  return gridIDnew;
}


void gridCompress(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  int gridtype = gridInqType(gridID);
  if ( gridtype == GRID_UNSTRUCTURED )
    {
      if ( gridptr->mask_gme != NULL )
	{
          size_t gridsize = (size_t)gridInqSize(gridID);
	  size_t nv = (size_t)gridptr->nvertex;
          double *restrict area
            = (double *)gridptr->vtable->inqAreaPtr(gridptr),
            *restrict xvals = (double *)gridptr->vtable->inqXValsPtr((grid_t *)gridptr),
            *restrict yvals = (double *)gridptr->vtable->inqYValsPtr((grid_t *)gridptr),
            *restrict xbounds = (double *)gridptr->vtable->inqXBoundsPtr(gridptr),
            *restrict ybounds = (double *)gridptr->vtable->inqYBoundsPtr(gridptr);
          mask_t *restrict mask_gme = gridptr->mask_gme;
          size_t *restrict selection = (size_t *)Malloc(gridsize * sizeof (selection[0]));
          size_t nselect;
          {
            size_t j = 0;
            for (size_t i = 0; i < gridsize; i++ )
              selection[j] = i, j += (mask_gme[i] != 0);
            nselect = j;
          }
          selection = (size_t *)Realloc(selection, nselect * sizeof (selection[0]));
          if (xvals)
            for (size_t i = 0; i < nselect; i++ )
	      xvals[i] = xvals[selection[i]];
          if (yvals)
            for (size_t i = 0; i < nselect; i++ )
              yvals[i] = yvals[selection[i]];
          if (area)
            for (size_t i = 0; i < nselect; i++ )
              area[i] = area[selection[i]];
          if (xbounds)
            for (size_t i = 0; i < nselect; i++ )
              for (size_t iv = 0; iv < nv; iv++)
                xbounds[i * nv + iv] = xbounds[selection[i] * nv + iv];
          if (ybounds)
            for (size_t i = 0; i < nselect; i++ )
              for (size_t iv = 0; iv < nv; iv++)
                ybounds[i * nv + iv] = ybounds[selection[i] * nv + iv];
          Free(selection);

	  /* fprintf(stderr, "grid compress %d %d %d\n", i, j, gridsize); */
	  gridsize = nselect;
	  gridptr->size  = (int)gridsize;
	  gridptr->xsize = (int)gridsize;
	  gridptr->ysize = (int)gridsize;

          double **resizeP[] = { &gridptr->xvals, &gridptr->yvals,
                                 &gridptr->area,
                                 &gridptr->xbounds, &gridptr->ybounds };
          size_t newSize[] = { gridsize, gridsize, gridsize, nv*gridsize,
                               nv*gridsize };
          for ( size_t i = 0; i < sizeof (resizeP) / sizeof (resizeP[0]); ++i)
            if ( *(resizeP[i]) )
              *(resizeP[i]) = (double *)Realloc(*(resizeP[i]), newSize[i]*sizeof(double));

	  Free(gridptr->mask_gme);
	  gridptr->mask_gme = NULL;
          gridMark4Update(gridID);
	}
    }
  else
    Warning("Unsupported grid type: %s", gridNamePtr(gridtype));
}

static void
gridDefAreaSerial(grid_t *gridptr, const double *area)
{
  size_t size = (size_t)gridptr->size;

  if ( size == 0 )
    Error("size undefined for gridID = %d", gridptr->self);

  if ( gridptr->area == NULL )
    gridptr->area = (double *) Malloc(size*sizeof(double));
  else if ( CDI_Debug )
    Warning("values already defined!");

  memcpy(gridptr->area, area, size * sizeof(double));
}


void gridDefArea(int gridID, const double *area)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defArea(gridptr, area);
  gridMark4Update(gridID);
}

static void
gridInqAreaSerial(grid_t *gridptr, double *area)
{
  if (gridptr->area)
    memcpy(area, gridptr->area, (size_t)gridptr->size * sizeof (double));
}


void gridInqArea(int gridID, double *area)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->inqArea(gridptr, area);
}

static int
gridHasAreaBase(grid_t *gridptr)
{
  return gridptr->area != NULL;
}

int gridHasArea(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->hasArea(gridptr);
}


static const double *gridInqAreaPtrBase(grid_t *gridptr)
{
  return gridptr->area;
}

const double *gridInqAreaPtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqAreaPtr(gridptr);
}


void gridDefNvertex(int gridID, int nvertex)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if (gridptr->nvertex != nvertex)
    {
      gridptr->nvertex = nvertex;
      gridMark4Update(gridID);
    }
}


int gridInqNvertex(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->nvertex;
}

static void
gridDefBoundsGeneric(grid_t *gridptr, const double *bounds, int regularSize,
                     double **field)
{
  int irregular = gridptr->type == GRID_CURVILINEAR
    || gridptr->type == GRID_UNSTRUCTURED;
  size_t nvertex = (size_t)gridptr->nvertex;
  if ( nvertex == 0 )
    {
      Warning("nvertex undefined for gridID = %d. Cannot define bounds!",
              gridptr->self);
      return;
    }
  size_t size = nvertex * (size_t)(irregular ? gridptr->size : regularSize);
  if ( size == 0 )
    Error("size undefined for gridID = %d", gridptr->self);

  if (*field == NULL)
    *field = (double *)Malloc(size * sizeof (double));
  else if ( CDI_Debug )
    Warning("values already defined!");

  memcpy(*field, bounds, size * sizeof (double));
}


static void
gridDefXBoundsSerial(grid_t *gridptr, const double *xbounds)
{
  gridDefBoundsGeneric(gridptr, xbounds, gridptr->xsize, &gridptr->xbounds);
}

/*
@Function  gridDefXbounds
@Title     Define the bounds of a X-axis

@Prototype void gridDefXbounds(int gridID, const double *xbounds)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  xbounds  X-bounds of the grid.

@Description
The function @func{gridDefXbounds} defines all bounds of the X-axis.

@EndFunction
*/
void gridDefXbounds(int gridID, const double *xbounds)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defXBounds(gridptr, xbounds);
  gridMark4Update(gridID);
}

static int
gridInqXBoundsSerial(grid_t *gridptr, double *xbounds)
{
  size_t nvertex = (size_t)gridptr->nvertex;

  int irregular = gridptr->type == GRID_CURVILINEAR
    || gridptr->type == GRID_UNSTRUCTURED;
  size_t size = nvertex * (size_t)(irregular ? gridptr->size : gridptr->xsize);

  const double *gridptr_xbounds = gridptr->vtable->inqXBoundsPtr(gridptr);
  if ( gridptr_xbounds )
    {
      if ( size && xbounds )
        memcpy(xbounds, gridptr_xbounds, size * sizeof (double));
    }
  else
    size = 0;

  return (int)size;
}

/*
@Function  gridInqXbounds
@Title     Get the bounds of a X-axis

@Prototype int gridInqXbounds(int gridID, double *xbounds)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  xbounds  Pointer to the location into which the X-bounds are read.
                    The caller must allocate space for the returned values.

@Description
The function @func{gridInqXbounds} returns the bounds of the X-axis.

@Result
Upon successful completion @func{gridInqXbounds} returns the number of bounds and
the bounds are stored in @func{xbounds}.
Otherwise, 0 is returned and @func{xbounds} is empty.

@EndFunction
*/
int gridInqXbounds(int gridID, double *xbounds)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqXBounds(gridptr, xbounds);
}

static const double *
gridInqXBoundsPtrSerial(grid_t *gridptr)
{
  return gridptr->xbounds;
}


const double *gridInqXboundsPtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqXBoundsPtr(gridptr);
}

static void
gridDefYBoundsSerial(grid_t *gridptr, const double *ybounds)
{
  gridDefBoundsGeneric(gridptr, ybounds, gridptr->ysize, &gridptr->ybounds);
}

/*
@Function  gridDefYbounds
@Title     Define the bounds of a Y-axis

@Prototype void gridDefYbounds(int gridID, const double *ybounds)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  ybounds  Y-bounds of the grid.

@Description
The function @func{gridDefYbounds} defines all bounds of the Y-axis.

@EndFunction
*/
void gridDefYbounds(int gridID, const double *ybounds)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  gridptr->vtable->defYBounds(gridptr, ybounds);
  gridMark4Update(gridID);
}

static int
gridInqYBoundsSerial(grid_t *gridptr, double *ybounds)
{
  size_t nvertex = (size_t)gridptr->nvertex;

  int irregular = gridptr->type == GRID_CURVILINEAR
    || gridptr->type == GRID_UNSTRUCTURED;
  size_t size = nvertex * (size_t)(irregular ? gridptr->size : gridptr->ysize);

  const double *gridptr_ybounds = gridptr->vtable->inqYBoundsPtr(gridptr);
  if ( gridptr_ybounds )
    {
      if ( size && ybounds )
        memcpy(ybounds, gridptr_ybounds, size * sizeof (double));
    }
  else
    size = 0;

  return (int)size;
}


/*
@Function  gridInqYbounds
@Title     Get the bounds of a Y-axis

@Prototype int gridInqYbounds(int gridID, double *ybounds)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  ybounds  Pointer to the location into which the Y-bounds are read.
                    The caller must allocate space for the returned values.

@Description
The function @func{gridInqYbounds} returns the bounds of the Y-axis.

@Result
Upon successful completion @func{gridInqYbounds} returns the number of bounds and
the bounds are stored in @func{ybounds}.
Otherwise, 0 is returned and @func{ybounds} is empty.

@EndFunction
*/
int gridInqYbounds(int gridID, double *ybounds)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqYBounds(gridptr, ybounds);
}

static const double *
gridInqYBoundsPtrSerial(grid_t *gridptr)
{
  return gridptr->ybounds;
}


const double *gridInqYboundsPtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqYBoundsPtr(gridptr);
}

static void
printDblsPrefixAutoBrk(FILE *fp, int dig, const char prefix[], size_t nbyte0,
                       size_t n, const double vals[])
{
  fputs(prefix, fp);
  size_t nbyte = nbyte0;
  for ( size_t i = 0; i < n; i++ )
    {
      if ( nbyte > 80 )
        {
          fprintf(fp, "\n%*s", (int)nbyte0, "");
          nbyte = nbyte0;
        }
      nbyte += (size_t)fprintf(fp, "%.*g ", dig, vals[i]);
    }
  fputs("\n", fp);
}

static void
printIntsPrefixAutoBrk(FILE *fp, const char prefix[], size_t nbyte0,
                       size_t n, const int vals[])
{
  fputs(prefix, fp);
  size_t nbyte = nbyte0;
  for ( size_t i = 0; i < n; i++ )
    {
      if ( nbyte > 80 )
        {
          fprintf(fp, "\n%*s", (int)nbyte0, "");
          nbyte = nbyte0;
        }
      nbyte += (size_t)fprintf(fp, "%d ", vals[i]);
    }
  fputs("\n", fp);
}

static void
printBounds(FILE *fp, int dig, const char prefix[], size_t nbyte0,
            size_t n, size_t nvertex, const double bounds[])
{
  fputs(prefix, fp);
  if ( n > 0 )
    {
      for ( size_t iv = 0; iv < nvertex; iv++ )
        fprintf(fp, "%.*g ", dig, bounds[iv]);
      for ( size_t i = 1; i < (size_t)n; i++ )
        {
          fprintf(fp, "\n%*s", (int)nbyte0, "");
          for ( size_t iv = 0; iv < nvertex; iv++ )
            fprintf(fp, "%.*g ", dig, bounds[i*nvertex+iv]);
        }
      fputs("\n", fp);
    }
}

static void
printMask(FILE *fp, const char prefix[], size_t nbyte0,
          size_t n, const mask_t mask[])
{
  fputs(prefix, fp);
  size_t nbyte = nbyte0;
  for ( size_t i = 0; i < n; i++ )
    {
      if ( nbyte > 80 )
        {
          fprintf(fp, "\n%*s", (int)nbyte0, "");
          nbyte = nbyte0;
        }
      nbyte += (size_t)fprintf(fp, "%d ", (int)mask[i]);
    }
  fputs("\n", fp);
}

static void gridPrintKernel(grid_t * gridptr, int index, int opt, FILE *fp)
{
  int xdim, ydim;
  unsigned char uuidOfHGrid[CDI_UUID_SIZE];
  int gridID = gridptr->self;
  const double *area    = gridInqAreaPtr(gridID);
  const double *xvals   = gridInqXvalsPtr(gridID);
  const double *yvals   = gridInqYvalsPtr(gridID);
  const double *xbounds = gridInqXboundsPtr(gridID);
  const double *ybounds = gridInqYboundsPtr(gridID);

  int type     = gridInqType(gridID);
  int trunc    = gridInqTrunc(gridID);
  int gridsize = gridInqSize(gridID);
  int xsize    = gridInqXsize(gridID);
  int ysize    = gridInqYsize(gridID);
  int nvertex  = gridInqNvertex(gridID);
  int prec     = gridInqPrec(gridID);

  int dig = (prec == DATATYPE_FLT64) ? 15 : 7;

  fprintf(fp, "#\n"
          "# gridID %d\n"
          "#\n"
          "gridtype  = %s\n"
          "gridsize  = %d\n", index, gridNamePtr(type), gridsize);

  if ( type != GRID_GME )
    {
      if ( xvals )
        {
          if ( gridptr->xname[0]     )     fprintf(fp, "xname     = %s\n", gridptr->xname);
          if ( gridptr->xlongname[0] )     fprintf(fp, "xlongname = %s\n", gridptr->xlongname);
          if ( gridptr->xunits[0]    )     fprintf(fp, "xunits    = %s\n", gridptr->xunits);
        }
      if ( yvals )
        {
          if ( gridptr->yname[0]     )     fprintf(fp, "yname     = %s\n", gridptr->yname);
          if ( gridptr->ylongname[0] )     fprintf(fp, "ylongname = %s\n", gridptr->ylongname);
          if ( gridptr->yunits[0]    )     fprintf(fp, "yunits    = %s\n", gridptr->yunits);
        }
      if ( type == GRID_UNSTRUCTURED && nvertex > 0 ) fprintf(fp, "nvertex   = %d\n", nvertex);
    }

  switch (type)
    {
    case GRID_LONLAT:
    case GRID_GAUSSIAN:
    case GRID_GAUSSIAN_REDUCED:
    case GRID_GENERIC:
    case GRID_LCC2:
    case GRID_SINUSOIDAL:
    case GRID_LAEA:
    case GRID_CURVILINEAR:
    case GRID_UNSTRUCTURED:
      {
        if ( type == GRID_GAUSSIAN || type == GRID_GAUSSIAN_REDUCED ) fprintf(fp, "np        = %d\n", gridptr->np);

	if ( type == GRID_CURVILINEAR || type == GRID_UNSTRUCTURED )
	  {
	    xdim = gridsize;
	    ydim = gridsize;
	  }
        else if ( type == GRID_GAUSSIAN_REDUCED )
          {
	    xdim = 2;
	    ydim = ysize;
          }
	else
	  {
	    xdim = xsize;
	    ydim = ysize;
	  }

	if ( type != GRID_UNSTRUCTURED )
	  {
	    if ( xsize > 0 ) fprintf(fp, "xsize     = %d\n", xsize);
	    if ( ysize > 0 ) fprintf(fp, "ysize     = %d\n", ysize);
	  }

	if ( type == GRID_UNSTRUCTURED )
          {
            int number = gridInqNumber(gridID);
            int position = gridInqPosition(gridID);
            // const unsigned char *d;
            if ( number > 0 )
              {
                fprintf(fp, "number    = %d\n", number);
                if ( position >= 0 ) fprintf(fp, "position  = %d\n", position);
              }
            /*
              gridInqUUID(gridID, uuidOfHGrid);
              d = (unsigned char *) &uuidOfHGrid;
              fprintf(fp, "uuid      = %02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x\n",
              d[0], d[1], d[2], d[3], d[4], d[5], d[6], d[7],
              d[8], d[9], d[10], d[11], d[12], d[13], d[14], d[15]);
            */
            if ( gridInqReference(gridID, NULL) )
              {
                char reference_link[8192];
                gridInqReference(gridID, reference_link);
                fprintf(fp, "uri       = %s\n", reference_link);
              }
          }

	if ( type == GRID_LAEA )
	  {
	    double a = 0, lon_0 = 0, lat_0 = 0;
	    gridInqLaea(gridID, &a, &lon_0, &lat_0);
	    fprintf(fp, "a         = %.*g\n"
                    "lon_0     = %.*g\n"
                    "lat_0     = %.*g\n", dig, a, dig, lon_0, dig, lat_0);
	  }

	if ( type == GRID_LCC2 )
	  {
	    double a = 0, lon_0 = 0, lat_0 = 0, lat_1 = 0, lat_2 = 0;
	    gridInqLcc2(gridID, &a, &lon_0, &lat_0, &lat_1, &lat_2);
	    fprintf(fp, "a         = %.*g\n"
                    "lon_0     = %.*g\n"
                    "lat_0     = %.*g\n"
                    "lat_1     = %.*g\n"
                    "lat_2     = %.*g\n",
                    dig, a, dig, lon_0, dig, lat_0, dig, lat_1, dig, lat_2);
	  }

	if ( gridptr->isRotated )
	  {
	    if ( xsize > 0 ) fprintf(fp, "xnpole    = %.*g\n", dig, gridptr->xpole);
	    if ( ysize > 0 ) fprintf(fp, "ynpole    = %.*g\n", dig, gridptr->ypole);
	    if ( IS_NOT_EQUAL(gridptr->angle, 0) ) fprintf(fp, "angle     = %.*g\n", dig, gridptr->angle);
 	  }

	if ( xvals )
	  {
	    double xfirst = 0.0, xinc = 0.0;

	    if ( type == GRID_LONLAT     || type == GRID_GAUSSIAN ||
		 type == GRID_GENERIC    || type == GRID_LCC2     ||
                 type == GRID_SINUSOIDAL || type == GRID_LAEA )
	      {
		xfirst = gridInqXval(gridID, 0);
		xinc   = gridInqXinc(gridID);
	      }

	    if ( IS_NOT_EQUAL(xinc, 0) && opt )
	      {
                fprintf(fp, "xfirst    = %.*g\n"
                        "xinc      = %.*g\n", dig, xfirst, dig, xinc);
	      }
	    else
	      {
                static const char prefix[] = "xvals     = ";
                printDblsPrefixAutoBrk(fp, dig, prefix, sizeof(prefix)-1,
                                       (size_t)(xdim > 0 ? xdim : 0), xvals);
	      }
	  }

	if ( xbounds )
	  {
            static const char prefix[] = "xbounds   = ";
            printBounds(fp, dig, prefix, sizeof(prefix)-1,
                        (size_t)(xdim > 0 ? xdim : 0),
                        (size_t)(nvertex > 0 ? nvertex : 0), xbounds);
	  }

	if ( yvals )
	  {
	    double yfirst = 0.0, yinc = 0.0;

	    if ( type == GRID_LONLAT || type == GRID_GENERIC || type == GRID_LCC2 ||
		 type == GRID_SINUSOIDAL || type == GRID_LAEA )
	      {
		yfirst = gridInqYval(gridID, 0);
		yinc   = gridInqYinc(gridID);
	      }

	    if ( IS_NOT_EQUAL(yinc, 0) && opt )
	      {
	  	fprintf(fp, "yfirst    = %.*g\n"
                        "yinc      = %.*g\n", dig, yfirst, dig, yinc);
	      }
	    else
	      {
                static const char prefix[] = "yvals     = ";
                printDblsPrefixAutoBrk(fp, dig, prefix, sizeof(prefix)-1,
                                       (size_t)(ydim > 0 ? ydim : 0), yvals);
	      }
	  }

	if ( ybounds )
	  {
            static const char prefix[] = "ybounds   = ";
            printBounds(fp, dig, prefix, sizeof(prefix)-1,
                        (size_t)(ydim > 0 ? ydim : 0),
                        (size_t)(nvertex > 0 ? nvertex : 0), ybounds);
	  }

	if ( area )
	  {
            static const char prefix[] = "area      = ";
            printDblsPrefixAutoBrk(fp, dig, prefix, sizeof(prefix)-1,
                                   (size_t)(gridsize > 0 ? gridsize : 0), area);
	  }

        if ( type == GRID_GAUSSIAN_REDUCED )
          {
            static const char prefix[] = "rowlon    = ";
            int *rowlon = (int *)Malloc((size_t)ysize*sizeof(int));
            gridInqRowlon(gridID, rowlon);
            printIntsPrefixAutoBrk(fp, prefix, sizeof(prefix)-1,
                                   (size_t)(ysize > 0 ? ysize : 0), rowlon);
            Free(rowlon);
          }

	break;
      }
    case GRID_LCC:
      {
	double originLon = 0, originLat = 0, lonParY = 0, lat1 = 0, lat2 = 0, xincm = 0, yincm = 0;
	int projflag = 0, scanflag = 0;
	gridInqLCC(gridID, &originLon, &originLat, &lonParY, &lat1, &lat2, &xincm, &yincm,
		   &projflag, &scanflag);

	fprintf(fp,
                "xsize     = %d\n"
                "ysize     = %d\n"
                "originLon = %.*g\n"
                "originLat = %.*g\n"
                "lonParY   = %.*g\n"
                "lat1      = %.*g\n"
                "lat2      = %.*g\n"
                "xinc      = %.*g\n"
                "yinc      = %.*g\n"
                "projection = %s\n",
                xsize, ysize, dig, originLon, dig, originLat, dig, lonParY,
                dig, lat1, dig, lat2, dig, xincm, dig, yincm,
                (projflag & 128) == 0 ? "northpole" : "southpole");
	break;
      }
    case GRID_SPECTRAL:
      {
        fprintf(fp, "truncation = %d\n"
                "complexpacking = %d\n", trunc, gridptr->lcomplex );
        break;
      }
    case GRID_FOURIER:
      {
	fprintf(fp, "truncation = %d\n", trunc);
	break;
      }
    case GRID_GME:
      {
        fprintf(fp, "ni        = %d\n", gridptr->ni );
        break;
      }
   default:
      {
	fprintf(stderr, "Unsupported grid type: %s\n", gridNamePtr(type));
        break;
      }
    }

  gridInqUUID(gridID, uuidOfHGrid);
  if ( !cdiUUIDIsNull(uuidOfHGrid) )
    {
      char uuidOfHGridStr[37];
      cdiUUID2Str(uuidOfHGrid, uuidOfHGridStr);
      if ( uuidOfHGridStr[0] != 0 && strlen(uuidOfHGridStr) == 36 )
        fprintf(fp, "uuid      = %s\n", uuidOfHGridStr);
    }

  if ( gridptr->mask )
    {
      static const char prefix[] = "mask      = ";
      printMask(fp, prefix, sizeof(prefix)-1,
                (size_t)(gridsize > 0 ? gridsize : 0), gridptr->mask);
    }
}

void gridPrint ( int gridID, int index, int opt )
{
  grid_t *gridptr = gridID2Ptr(gridID);

  gridPrintKernel ( gridptr, index, opt, stdout );
}



void gridPrintP ( void * voidptr, FILE * fp )
{
  grid_t * gridptr = ( grid_t * ) voidptr;

  xassert ( gridptr );

  gridPrintKernel ( gridptr , gridptr->self, 0, fp );

  fprintf(fp,
          "precision = %d\n"
          "nd        = %d\n"
          "ni        = %d\n"
          "ni2       = %d\n"
          "ni3       = %d\n"
          "number    = %d\n"
          "position  = %d\n"
          "trunc     = %d\n"
          "lcomplex  = %d\n"
          "nrowlon   = %d\n",
          gridptr->prec, gridptr->nd, gridptr->ni, gridptr->ni2,
          gridptr->ni3, gridptr->number, gridptr->position, gridptr->trunc,
          gridptr->lcomplex, gridptr->nrowlon );

  if ( gridptr->rowlon )
    {
      static const char prefix[] = "rowlon    = ";
      printIntsPrefixAutoBrk(fp, prefix, sizeof(prefix)-1,
                             (size_t)(gridptr->nrowlon > 0
                                      ? gridptr->nrowlon : 0), gridptr->rowlon);
    }

  if ( gridptr->mask_gme )
    {
      static const char prefix[] = "mask_gme  = ";
      printMask(fp, prefix, sizeof(prefix)-1,
                (size_t)(gridptr->size > 0 ? gridptr->size : 0),
                gridptr->mask_gme);
    }
}

static const double *gridInqXValsPtrSerial(grid_t *gridptr)
{
  return gridptr->xvals;
}

const double *gridInqXvalsPtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqXValsPtr(gridptr);
}


static const double *gridInqYValsPtrSerial(grid_t *gridptr)
{
  return gridptr->yvals;
}

const double *gridInqYvalsPtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->vtable->inqYValsPtr(gridptr);
}

/*
@Function  gridDefLCC
@Title     Define the parameter of a Lambert Conformal Conic grid

@Prototype void gridDefLCC(int gridID, double originLon, double originLat, double lonParY, double lat1, double lat2, double xinc, double yinc, int projflag, int scanflag)
@Parameter
    @Item  gridID    Grid ID, from a previous call to @fref{gridCreate}.
    @Item  originLon Longitude of the first grid point.
    @Item  originLat Latitude of the first grid point.
    @Item  lonParY   The East longitude of the meridian which is parallel to the Y-axis.
    @Item  lat1      First latitude from the pole at which the secant cone cuts the sphere.
    @Item  lat2      Second latitude at which the secant cone cuts the sphere.
    @Item  xinc      X-direction grid lenght in meter.
    @Item  yinc      Y-direction grid lenght in meter.
    @Item  projflag  Projection centre flag.
    @Item  scanflag  Scanning mode flag.

@Description
The function @func{gridDefLCC} defines the parameter of a Lambert Conformal Conic grid.

@EndFunction
*/
void gridDefLCC(int gridID, double originLon, double originLat, double lonParY,
                double lat1, double lat2, double xinc, double yinc,
                int projflag, int scanflag)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LCC )
    Warning("Definition of LCC grid for %s grid not allowed!",
	    gridNamePtr(gridptr->type));
  else
    {
      gridptr->lcc_originLon = originLon;
      gridptr->lcc_originLat = originLat;
      gridptr->lcc_lonParY   = lonParY;
      gridptr->lcc_lat1      = lat1;
      gridptr->lcc_lat2      = lat2;
      gridptr->lcc_xinc      = xinc;
      gridptr->lcc_yinc      = yinc;
      gridptr->lcc_projflag  = projflag;
      gridptr->lcc_scanflag  = scanflag;
      gridptr->lcc_defined   = TRUE;
      gridMark4Update(gridID);
    }
}

/*
@Function  gridInqLCC
@Title     Get the parameter of a Lambert Conformal Conic grid

@Prototype void gridInqLCC(int gridID, double *originLon, double *originLat, double *lonParY, double *lat1, double *lat2, double *xinc, double *yinc, int *projflag, int *scanflag)
@Parameter
    @Item  gridID    Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.
    @Item  originLon Longitude of the first grid point.
    @Item  originLat Latitude of the first grid point.
    @Item  lonParY   The East longitude of the meridian which is parallel to the Y-axis.
    @Item  lat1      First latitude from the pole at which the secant cone cuts the sphere.
    @Item  lat2      Second latitude at which the secant cone cuts the sphere.
    @Item  xinc      X-direction grid lenght in meter.
    @Item  yinc      Y-direction grid lenght in meter.
    @Item  projflag  Projection centre flag.
    @Item  scanflag  Scanning mode flag.
 
@Description
The function @func{gridInqLCC} returns the parameter of a Lambert Conformal Conic grid.

@EndFunction
*/
void gridInqLCC(int gridID, double *originLon, double *originLat, double *lonParY,
                double *lat1, double *lat2, double *xinc, double *yinc,
                int *projflag, int *scanflag)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LCC )
    Warning("Inquire of LCC grid definition for %s grid not allowed!",
	    gridNamePtr(gridptr->type));
  else
    {
      if ( gridptr->lcc_defined )
        {
          *originLon = gridptr->lcc_originLon;
          *originLat = gridptr->lcc_originLat;
          *lonParY   = gridptr->lcc_lonParY;
          *lat1      = gridptr->lcc_lat1;
          *lat2      = gridptr->lcc_lat2;
          *xinc      = gridptr->lcc_xinc;
          *yinc      = gridptr->lcc_yinc;
          *projflag  = gridptr->lcc_projflag;
          *scanflag  = gridptr->lcc_scanflag;
        }
      else
	Warning("Lambert Conformal grid undefined (gridID = %d)", gridID);
    }
}

void gridDefLcc2(int gridID, double earth_radius, double lon_0, double lat_0, double lat_1, double lat_2)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LCC2 )
    Warning("Definition of LCC2 grid for %s grid not allowed!",
	    gridNamePtr(gridptr->type));
  else
    {
      gridptr->lcc2_a       = earth_radius;
      gridptr->lcc2_lon_0   = lon_0;
      gridptr->lcc2_lat_0   = lat_0;
      gridptr->lcc2_lat_1   = lat_1;
      gridptr->lcc2_lat_2   = lat_2;
      gridptr->lcc2_defined = TRUE;
      gridMark4Update(gridID);
    }
}


void gridInqLcc2(int gridID, double *earth_radius, double *lon_0, double *lat_0, double *lat_1, double *lat_2)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LCC2 )
    Warning("Inquire of LCC2 grid definition for %s grid not allowed!",
	    gridNamePtr(gridptr->type));
  else
    {
      if ( gridptr->lcc2_defined )
        {
          *earth_radius = gridptr->lcc2_a;
          *lon_0        = gridptr->lcc2_lon_0;
          *lat_0        = gridptr->lcc2_lat_0;
          *lat_1        = gridptr->lcc2_lat_1;
          *lat_2        = gridptr->lcc2_lat_2;
        }
      else
        Warning("LCC2 grid undefined (gridID = %d)", gridID);
    }
}

void gridDefLaea(int gridID, double earth_radius, double lon_0, double lat_0)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LAEA )
    Warning("Definition of LAEA grid for %s grid not allowed!",
            gridNamePtr(gridptr->type));
  else
    {
      gridptr->laea_a       = earth_radius;
      gridptr->laea_lon_0   = lon_0;
      gridptr->laea_lat_0   = lat_0;
      gridptr->laea_defined = TRUE;
      gridMark4Update(gridID);
    }
}


void gridInqLaea(int gridID, double *earth_radius, double *lon_0, double *lat_0)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  if ( gridptr->type != GRID_LAEA )
    Warning("Inquire of LAEA grid definition for %s grid not allowed!",
            gridNamePtr(gridptr->type));
  else
    {
      if ( gridptr->laea_defined )
        {
          *earth_radius = gridptr->laea_a;
          *lon_0        = gridptr->laea_lon_0;
          *lat_0        = gridptr->laea_lat_0;
        }
      else
        Warning("LAEA grid undefined (gridID = %d)", gridID);
    }
}


void gridDefComplexPacking(int gridID, int lcomplex)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if (gridptr->lcomplex != lcomplex)
    {
      gridptr->lcomplex = (short)(lcomplex != 0);
      gridMark4Update(gridID);
    }
}


int gridInqComplexPacking(int gridID)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  return gridptr->lcomplex;
}


void gridDefHasDims(int gridID, int hasdims)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  if ( gridptr->hasdims != (hasdims != 0) )
    {
      gridptr->hasdims = hasdims != 0;
      gridMark4Update(gridID);
    }
}


int gridInqHasDims(int gridID)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  return gridptr->hasdims;
}

/*
@Function  gridDefNumber
@Title     Define the reference number for an unstructured grid

@Prototype void gridDefNumber(int gridID, const int number)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  number   Reference number for an unstructured grid.

@Description
The function @func{gridDefNumber} defines the reference number for an unstructured grid.

@EndFunction
*/
void gridDefNumber(int gridID, const int number)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  if ( gridptr->number != number )
    {
      gridptr->number = number;
      gridMark4Update(gridID);
    }
}

/*
@Function  gridInqNumber
@Title     Get the reference number to an unstructured grid

@Prototype int gridInqNumber(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqNumber} returns the reference number to an unstructured grid.

@Result
@func{gridInqNumber} returns the reference number to an unstructured grid.
@EndFunction
*/
int gridInqNumber(int gridID)
{
  grid_t* gridptr = gridID2Ptr(gridID);
  return gridptr->number;
}

/*
@Function  gridDefPosition
@Title     Define the position of grid in the reference file

@Prototype void gridDefPosition(int gridID, const int position)
@Parameter
    @Item  gridID     Grid ID, from a previous call to @fref{gridCreate}.
    @Item  position   Position of grid in the reference file.

@Description
The function @func{gridDefPosition} defines the position of grid in the reference file.

@EndFunction
*/
void gridDefPosition(int gridID, int position)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  if ( gridptr->position != position )
    {
      gridptr->position = position;
      gridMark4Update(gridID);
    }
}

/*
@Function  gridInqPosition
@Title     Get the position of grid in the reference file

@Prototype int gridInqPosition(int gridID)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqPosition} returns the position of grid in the reference file.

@Result
@func{gridInqPosition} returns the position of grid in the reference file.
@EndFunction
*/
int gridInqPosition(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);

  return gridptr->position;
}

/*
@Function  gridDefReference
@Title     Define the reference URI for an unstructured grid

@Prototype void gridDefReference(int gridID, const char *reference)
@Parameter
    @Item  gridID      Grid ID, from a previous call to @fref{gridCreate}.
    @Item  reference   Reference URI for an unstructured grid.

@Description
The function @func{gridDefReference} defines the reference URI for an unstructured grid.

@EndFunction
*/
void gridDefReference(int gridID, const char *reference)
{
  grid_t* gridptr = gridID2Ptr(gridID);

  if ( reference )
    {
      if ( gridptr->reference )
        {
          Free(gridptr->reference);
          gridptr->reference = NULL;
        }

      gridptr->reference = strdupx(reference);
      gridMark4Update(gridID);
    }
}

/*
@Function  gridInqReference
@Title     Get the reference URI to an unstructured grid

@Prototype char *gridInqReference(int gridID, char *reference)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqReference} returns the reference URI to an unstructured grid.

@Result
@func{gridInqReference} returns the reference URI to an unstructured grid.
@EndFunction
*/
int gridInqReference(int gridID, char *reference)
{
  size_t len = 0;
  grid_t* gridptr = gridID2Ptr(gridID);

  if ( gridptr->reference )
    {
      len = strlen(gridptr->reference);
      if ( reference )
        strcpy(reference, gridptr->reference);
    }

  return (int)len;
}

const char *gridInqReferencePtr(int gridID)
{
  grid_t *gridptr = gridID2Ptr(gridID);
  return gridptr->reference;
}

/*
@Function  gridDefUUID
@Title     Define the UUID for an unstructured grid

@Prototype void gridDefUUID(int gridID, const char *uuid)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate}.
    @Item  uuid     UUID for an unstructured grid.

@Description
The function @func{gridDefUUID} defines the UUID for an unstructured grid.

@EndFunction
*/
void gridDefUUID(int gridID, const unsigned char uuid[CDI_UUID_SIZE])
{
  grid_t* gridptr = gridID2Ptr(gridID);

  memcpy(gridptr->uuid, uuid, CDI_UUID_SIZE);
  gridMark4Update(gridID);
}

/*
@Function  gridInqUUID
@Title     Get the UUID to an unstructured grid

@Prototype void gridInqUUID(int gridID, char *uuid)
@Parameter
    @Item  gridID   Grid ID, from a previous call to @fref{gridCreate} or @fref{vlistInqVarGrid}.

@Description
The function @func{gridInqUUID} returns the UUID to an unstructured grid.

@Result
@func{gridInqUUID} returns the UUID to an unstructured grid to the parameter uuid.
@EndFunction
*/
void gridInqUUID(int gridID, unsigned char uuid[CDI_UUID_SIZE])
{
  grid_t *gridptr = gridID2Ptr(gridID);

  memcpy(uuid, gridptr->uuid, CDI_UUID_SIZE);
}


void cdiGridGetIndexList(unsigned ngrids, int * gridIndexList)
{
  reshGetResHListOfType(ngrids, gridIndexList, &gridOps);
}


static int
gridTxCode ()
{
  return GRID;
}

enum { gridNint    = 28,
       gridNdouble = 24,
       gridHasMaskFlag = 1 << 0,
       gridHasGMEMaskFlag = 1 << 1,
       gridHasXValsFlag = 1 << 2,
       gridHasYValsFlag = 1 << 3,
       gridHasAreaFlag = 1 << 4,
       gridHasXBoundsFlag = 1 << 5,
       gridHasYBoundsFlag = 1 << 6,
       gridHasReferenceFlag = 1 << 7,
       gridHasRowLonFlag = 1 << 8,
       gridHasUUIDFlag = 1 << 9,
};


static int gridGetComponentFlags(const grid_t * gridP)
{
  int flags = (gridHasMaskFlag & (int)((unsigned)(gridP->mask == NULL) - 1U))
    | (gridHasGMEMaskFlag & (int)((unsigned)(gridP->mask_gme == NULL) - 1U))
    | (gridHasXValsFlag
       & (int)((unsigned)(gridP->vtable->inqXValsPtr((grid_t *)gridP) == NULL) - 1U))
    | (gridHasYValsFlag
       & (int)((unsigned)(gridP->vtable->inqYValsPtr((grid_t *)gridP) == NULL) - 1U))
    | (gridHasAreaFlag
       & (int)((unsigned)(gridP->vtable->inqAreaPtr((grid_t *)gridP) == NULL)
               - 1U))
    | (gridHasXBoundsFlag & (int)((unsigned)(gridP->xbounds == NULL) - 1U))
    | (gridHasYBoundsFlag & (int)((unsigned)(gridP->ybounds == NULL) - 1U))
    | (gridHasReferenceFlag & (int)((unsigned)(gridP->reference == NULL) - 1U))
    | (gridHasRowLonFlag & (int)((unsigned)(gridP->rowlon == NULL) - 1U))
    | (gridHasUUIDFlag & (int)((unsigned)cdiUUIDIsNull(gridP->uuid) - 1U));
  return flags;
}

static int
gridGetPackSize(void * voidP, void *context)
{
  grid_t * gridP = ( grid_t * ) voidP;
  int packBuffSize = 0, count;

  packBuffSize += serializeGetSize(gridNint, DATATYPE_INT, context)
    + serializeGetSize(1, DATATYPE_UINT32, context);

  if (gridP->rowlon)
    {
      xassert(gridP->nrowlon);
      packBuffSize += serializeGetSize(gridP->nrowlon, DATATYPE_INT, context)
        + serializeGetSize( 1, DATATYPE_UINT32, context);
    }

  packBuffSize += serializeGetSize(gridNdouble, DATATYPE_FLT64, context);

  if (gridP->vtable->inqXValsPtr(gridP))
    {
      if (gridP->type == GRID_UNSTRUCTURED || gridP->type == GRID_CURVILINEAR)
	count = gridP->size;
      else
	count = gridP->xsize;
      xassert(count);
      packBuffSize += serializeGetSize(count, DATATYPE_FLT64, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (gridP->vtable->inqYValsPtr(gridP))
    {
      if (gridP->type == GRID_UNSTRUCTURED || gridP->type == GRID_CURVILINEAR)
	count = gridP->size;
      else
	count = gridP->ysize;
      xassert(count);
      packBuffSize += serializeGetSize(count, DATATYPE_FLT64, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (gridP->vtable->inqAreaPtr(gridP))
    {
      xassert(gridP->size);
      packBuffSize +=
        serializeGetSize(gridP->size, DATATYPE_FLT64, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (gridP->xbounds)
    {
      xassert(gridP->nvertex);
      if (gridP->type == GRID_CURVILINEAR || gridP->type == GRID_UNSTRUCTURED)
	count = gridP->size;
      else
	count = gridP->xsize;
      xassert(count);
      packBuffSize
        += (serializeGetSize(gridP->nvertex * count, DATATYPE_FLT64, context)
            + serializeGetSize(1, DATATYPE_UINT32, context));
    }

  if (gridP->ybounds)
    {
      xassert(gridP->nvertex);
      if (gridP->type == GRID_CURVILINEAR || gridP->type == GRID_UNSTRUCTURED)
	count = gridP->size;
      else
	count = gridP->ysize;
      xassert(count);
      packBuffSize
        += (serializeGetSize(gridP->nvertex * count, DATATYPE_FLT64, context)
            + serializeGetSize(1, DATATYPE_UINT32, context));
    }

  {
    const char *strTab[] = GRID_STR_SERIALIZE(gridP);
    int numStr = (int)(sizeof (strTab) / sizeof (strTab[0]));
    packBuffSize
      += serializeStrTabGetPackSize(strTab, numStr, context);
  }

  if (gridP->reference)
    {
      size_t len = strlen(gridP->reference);
      packBuffSize += serializeGetSize(1, DATATYPE_INT, context)
        + serializeGetSize((int)len + 1, DATATYPE_TXT, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (gridP->mask)
    {
      xassert(gridP->size);
      packBuffSize
        += serializeGetSize(gridP->size, DATATYPE_UCHAR, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (gridP->mask_gme)
    {
      xassert(gridP->size);
      packBuffSize += serializeGetSize(gridP->size, DATATYPE_UCHAR, context)
        + serializeGetSize(1, DATATYPE_UINT32, context);
    }

  if (!cdiUUIDIsNull(gridP->uuid))
    packBuffSize += serializeGetSize(CDI_UUID_SIZE, DATATYPE_UCHAR, context);

  return packBuffSize;
}

void
gridUnpack(char * unpackBuffer, int unpackBufferSize,
           int * unpackBufferPos, int originNamespace, void *context,
           int force_id)
{
  grid_t * gridP;
  uint32_t d;
  int memberMask, size;

  gridInit();

  {
    int intBuffer[gridNint];
    serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                    intBuffer, gridNint, DATATYPE_INT, context);
    serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                    &d, 1, DATATYPE_UINT32, context);

    xassert(cdiCheckSum(DATATYPE_INT, gridNint, intBuffer) == d);
    int targetID = namespaceAdaptKey(intBuffer[0], originNamespace);
    gridP = gridNewEntry(force_id?targetID:CDI_UNDEFID);

    xassert(!force_id || targetID == gridP->self);

    gridP->type          =   intBuffer[1];
    gridP->prec          =   intBuffer[2];
    gridP->lcc_projflag  =   intBuffer[3];
    gridP->lcc_scanflag  =   intBuffer[4];
    gridP->lcc_defined   =   (short)intBuffer[5];
    gridP->lcc2_defined  =   (short)intBuffer[6];
    gridP->laea_defined  =   intBuffer[7];
    gridP->isCyclic      =   (short)intBuffer[8];
    gridP->isRotated     =   (short)intBuffer[9];
    gridP->xdef          =   (short)intBuffer[10];
    gridP->ydef          =   (short)intBuffer[11];
    gridP->nd            =   intBuffer[12];
    gridP->ni            =   intBuffer[13];
    gridP->ni2           =   intBuffer[14];
    gridP->ni3           =   intBuffer[15];
    gridP->number        =   intBuffer[16];
    gridP->position      =   intBuffer[17];
    gridP->trunc         =   intBuffer[18];
    gridP->nvertex       =   intBuffer[19];
    gridP->nrowlon       =   intBuffer[20];
    gridP->size          =   intBuffer[21];
    gridP->xsize         =   intBuffer[22];
    gridP->ysize         =   intBuffer[23];
    gridP->lcomplex      =   (short)intBuffer[24];
    memberMask           =   intBuffer[25];
    gridP->xstdname      =   xystdname_tab[intBuffer[26]][0];
    gridP->ystdname      =   xystdname_tab[intBuffer[27]][1];
  }

  if (memberMask & gridHasRowLonFlag)
    {
      xassert(gridP->nrowlon);
      gridP->rowlon = (int *) Malloc((size_t)gridP->nrowlon * sizeof (int));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->rowlon, gridP->nrowlon , DATATYPE_INT, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_INT, gridP->nrowlon, gridP->rowlon) == d);
    }

  {
    double doubleBuffer[gridNdouble];
    serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                    doubleBuffer, gridNdouble, DATATYPE_FLT64, context);
    serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                    &d, 1, DATATYPE_UINT32, context);
    xassert(d == cdiCheckSum(DATATYPE_FLT, gridNdouble, doubleBuffer));

    gridP->xfirst = doubleBuffer[0];
    gridP->yfirst = doubleBuffer[1];
    gridP->xlast = doubleBuffer[2];
    gridP->ylast = doubleBuffer[3];
    gridP->xinc = doubleBuffer[4];
    gridP->yinc = doubleBuffer[5];
    gridP->lcc_originLon = doubleBuffer[6];
    gridP->lcc_originLat = doubleBuffer[7];
    gridP->lcc_lonParY = doubleBuffer[8];
    gridP->lcc_lat1 = doubleBuffer[9];
    gridP->lcc_lat2 = doubleBuffer[10];
    gridP->lcc_xinc = doubleBuffer[11];
    gridP->lcc_yinc = doubleBuffer[12];
    gridP->lcc2_lon_0 = doubleBuffer[13];
    gridP->lcc2_lat_0 = doubleBuffer[14];
    gridP->lcc2_lat_1 = doubleBuffer[15];
    gridP->lcc2_lat_2 = doubleBuffer[16];
    gridP->lcc2_a = doubleBuffer[17];
    gridP->laea_lon_0 = doubleBuffer[18];
    gridP->laea_lat_0 = doubleBuffer[19];
    gridP->laea_a = doubleBuffer[20];
    gridP->xpole = doubleBuffer[21];
    gridP->ypole = doubleBuffer[22];
    gridP->angle = doubleBuffer[23];
  }

  int irregular = gridP->type == GRID_UNSTRUCTURED
    || gridP->type == GRID_CURVILINEAR;
  if (memberMask & gridHasXValsFlag)
    {
      size = irregular ? gridP->size : gridP->xsize;

      gridP->xvals = (double *) Malloc((size_t)size * sizeof (double));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->xvals, size, DATATYPE_FLT64, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_FLT, size, gridP->xvals) == d );
    }

  if (memberMask & gridHasYValsFlag)
    {
      size = irregular ? gridP->size : gridP->ysize;

      gridP->yvals = (double *) Malloc((size_t)size * sizeof (double));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->yvals, size, DATATYPE_FLT64, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_FLT, size, gridP->yvals) == d);
    }

  if (memberMask & gridHasAreaFlag)
    {
      size = gridP->size;
      xassert(size);
      gridP->area = (double *) Malloc((size_t)size * sizeof (double));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->area, size, DATATYPE_FLT64, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_FLT, size, gridP->area) == d);
    }

  if (memberMask & gridHasXBoundsFlag)
    {
      size = gridP->nvertex * (irregular ? gridP->size : gridP->xsize);
      xassert(size);

      gridP->xbounds = (double *) Malloc((size_t)size * sizeof (double));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->xbounds, size, DATATYPE_FLT64, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_FLT, size, gridP->xbounds) == d);
    }

  if (memberMask & gridHasYBoundsFlag)
    {
      size = gridP->nvertex * (irregular ? gridP->size : gridP->ysize);
      xassert(size);

      gridP->ybounds = (double *) Malloc((size_t)size * sizeof (double));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
			  gridP->ybounds, size, DATATYPE_FLT64, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_FLT, size, gridP->ybounds) == d);
    }

  {
    char *strTab[] = GRID_STR_SERIALIZE(gridP);
    int numStr = sizeof (strTab) / sizeof (strTab[0]);
    serializeStrTabUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                          strTab, numStr, context);
  }

  if (memberMask & gridHasReferenceFlag)
    {
      int referenceSize;
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &referenceSize, 1, DATATYPE_INT, context);
      gridP->reference = (char *) Malloc((size_t)referenceSize);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->reference, referenceSize, DATATYPE_TXT, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_TXT, referenceSize, gridP->reference) == d);
    }

  if (memberMask & gridHasMaskFlag)
    {
      xassert((size = gridP->size));
      gridP->mask = (mask_t *) Malloc((size_t)size * sizeof (mask_t));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->mask, gridP->size, DATATYPE_UCHAR, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_UCHAR, gridP->size, gridP->mask) == d);
    }

  if (memberMask & gridHasGMEMaskFlag)
    {
      xassert((size = gridP->size));
      gridP->mask_gme = (mask_t *) Malloc((size_t)size * sizeof (mask_t));
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->mask_gme, gridP->size, DATATYPE_UCHAR, context);
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      &d, 1, DATATYPE_UINT32, context);
      xassert(cdiCheckSum(DATATYPE_UCHAR, gridP->size, gridP->mask_gme) == d);
    }
  if (memberMask & gridHasUUIDFlag)
    {
      serializeUnpack(unpackBuffer, unpackBufferSize, unpackBufferPos,
                      gridP->uuid, CDI_UUID_SIZE, DATATYPE_UCHAR, context);
    }

  reshSetStatus(gridP->self, &gridOps,
                reshGetStatus(gridP->self, &gridOps) & ~RESH_SYNC_BIT);
}


static void
gridPack(void * voidP, void * packBuffer, int packBufferSize,
         int * packBufferPos, void *context)
{
  grid_t   * gridP = ( grid_t * )   voidP;
  int size;
  uint32_t d;
  int memberMask;

  {
    int intBuffer[gridNint];

    intBuffer[0]  = gridP->self;
    intBuffer[1]  = gridP->type;
    intBuffer[2]  = gridP->prec;
    intBuffer[3]  = gridP->lcc_projflag;
    intBuffer[4]  = gridP->lcc_scanflag;
    intBuffer[5]  = gridP->lcc_defined;
    intBuffer[6]  = gridP->lcc2_defined;
    intBuffer[7]  = gridP->laea_defined;
    intBuffer[8]  = gridP->isCyclic;
    intBuffer[9]  = gridP->isRotated;
    intBuffer[10] = gridP->xdef;
    intBuffer[11] = gridP->ydef;
    intBuffer[12] = gridP->nd;
    intBuffer[13] = gridP->ni;
    intBuffer[14] = gridP->ni2;
    intBuffer[15] = gridP->ni3;
    intBuffer[16] = gridP->number;
    intBuffer[17] = gridP->position;
    intBuffer[18] = gridP->trunc;
    intBuffer[19] = gridP->nvertex;
    intBuffer[20] = gridP->nrowlon;
    intBuffer[21] = gridP->size;
    intBuffer[22] = gridP->xsize;
    intBuffer[23] = gridP->ysize;
    intBuffer[24] = gridP->lcomplex;
    intBuffer[25] = memberMask = gridGetComponentFlags(gridP);
    intBuffer[26] = (int)((const char (*)[2][24])gridP->xstdname
                          - xystdname_tab);
    intBuffer[27] = (int)((const char (*)[2][24])gridP->ystdname
                          - (const char (*)[2][24])xystdname_tab[0][1]);

    serializePack(intBuffer, gridNint, DATATYPE_INT,
                  packBuffer, packBufferSize, packBufferPos, context);
    d = cdiCheckSum(DATATYPE_INT, gridNint, intBuffer);
    serializePack(&d, 1, DATATYPE_UINT32,
                  packBuffer, packBufferSize, packBufferPos, context);
  }

  if (memberMask & gridHasRowLonFlag)
    {
      size = gridP->nrowlon;
      xassert(size > 0);
      serializePack(gridP->rowlon, size, DATATYPE_INT,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_INT , size, gridP->rowlon);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  {
    double doubleBuffer[gridNdouble];

    doubleBuffer[0]  = gridP->xfirst;
    doubleBuffer[1]  = gridP->yfirst;
    doubleBuffer[2]  = gridP->xlast;
    doubleBuffer[3]  = gridP->ylast;
    doubleBuffer[4]  = gridP->xinc;
    doubleBuffer[5]  = gridP->yinc;
    doubleBuffer[6]  = gridP->lcc_originLon;
    doubleBuffer[7]  = gridP->lcc_originLat;
    doubleBuffer[8]  = gridP->lcc_lonParY;
    doubleBuffer[9]  = gridP->lcc_lat1;
    doubleBuffer[10] = gridP->lcc_lat2;
    doubleBuffer[11] = gridP->lcc_xinc;
    doubleBuffer[12] = gridP->lcc_yinc;
    doubleBuffer[13] = gridP->lcc2_lon_0;
    doubleBuffer[14] = gridP->lcc2_lat_0;
    doubleBuffer[15] = gridP->lcc2_lat_1;
    doubleBuffer[16] = gridP->lcc2_lat_2;
    doubleBuffer[17] = gridP->lcc2_a;
    doubleBuffer[18] = gridP->laea_lon_0;
    doubleBuffer[19] = gridP->laea_lat_0;
    doubleBuffer[20] = gridP->laea_a;
    doubleBuffer[21] = gridP->xpole;
    doubleBuffer[22] = gridP->ypole;
    doubleBuffer[23] = gridP->angle;

    serializePack(doubleBuffer, gridNdouble, DATATYPE_FLT64,
                  packBuffer, packBufferSize, packBufferPos, context);
    d = cdiCheckSum(DATATYPE_FLT, gridNdouble, doubleBuffer);
    serializePack(&d, 1, DATATYPE_UINT32,
                  packBuffer, packBufferSize, packBufferPos, context);
  }

  if (memberMask & gridHasXValsFlag)
    {
      if (gridP->type == GRID_UNSTRUCTURED || gridP->type == GRID_CURVILINEAR)
	size = gridP->size;
      else
	size = gridP->xsize;
      xassert(size);

      const double *gridP_xvals = gridP->vtable->inqXValsPtr(gridP);
      serializePack(gridP_xvals, size, DATATYPE_FLT64,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_FLT, size, gridP_xvals);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasYValsFlag)
    {
      if (gridP->type == GRID_UNSTRUCTURED || gridP->type == GRID_CURVILINEAR )
	size = gridP->size;
      else
	size = gridP->ysize;
      xassert(size);
      const double *gridP_yvals = gridP->vtable->inqYValsPtr(gridP);
      serializePack(gridP_yvals, size, DATATYPE_FLT64,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_FLT, size, gridP_yvals);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasAreaFlag)
    {
      xassert(gridP->size);

      serializePack(gridP->area, gridP->size, DATATYPE_FLT64,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_FLT, gridP->size, gridP->area);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasXBoundsFlag)
    {
      xassert ( gridP->nvertex );
      if (gridP->type == GRID_CURVILINEAR || gridP->type == GRID_UNSTRUCTURED)
	size = gridP->nvertex * gridP->size;
      else
	size = gridP->nvertex * gridP->xsize;
      xassert ( size );

      serializePack(gridP->xbounds, size, DATATYPE_FLT64,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_FLT, size, gridP->xbounds);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasYBoundsFlag)
    {
      xassert(gridP->nvertex);
      if (gridP->type == GRID_CURVILINEAR || gridP->type == GRID_UNSTRUCTURED)
	size = gridP->nvertex * gridP->size;
      else
	size = gridP->nvertex * gridP->ysize;
      xassert ( size );

      serializePack(gridP->ybounds, size, DATATYPE_FLT64,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_FLT, size, gridP->ybounds);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  {
    const char *strTab[] = GRID_STR_SERIALIZE(gridP);
    int numStr = sizeof (strTab) / sizeof (strTab[0]);
    serializeStrTabPack(strTab, numStr,
                        packBuffer, packBufferSize, packBufferPos, context);
  }

  if (memberMask & gridHasReferenceFlag)
    {
      size = (int)strlen(gridP->reference) + 1;
      serializePack(&size, 1, DATATYPE_INT,
                    packBuffer, packBufferSize, packBufferPos, context);
      serializePack(gridP->reference, size, DATATYPE_TXT,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_TXT, size, gridP->reference);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasMaskFlag)
    {
      xassert((size = gridP->size));
      serializePack(gridP->mask, size, DATATYPE_UCHAR,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_UCHAR, size, gridP->mask);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasGMEMaskFlag)
    {
      xassert((size = gridP->size));

      serializePack(gridP->mask_gme, size, DATATYPE_UCHAR,
                    packBuffer, packBufferSize, packBufferPos, context);
      d = cdiCheckSum(DATATYPE_UCHAR, size, gridP->mask_gme);
      serializePack(&d, 1, DATATYPE_UINT32,
                    packBuffer, packBufferSize, packBufferPos, context);
    }

  if (memberMask & gridHasUUIDFlag)
    serializePack(gridP->uuid, CDI_UUID_SIZE, DATATYPE_UCHAR,
                  packBuffer, packBufferSize, packBufferPos, context);
}

#undef GRID_STR_SERIALIZE


struct gridCompareSearchState
{
  int resIDValue;
  const grid_t *queryKey;
};

static enum cdiApplyRet
gridCompareSearch(int id, void *res, void *data)
{
  struct gridCompareSearchState *state = (struct gridCompareSearchState*)data;
  (void)res;
  if ( gridCompare(id, state->queryKey) == false )
    {
      state->resIDValue = id;
      return CDI_APPLY_STOP;
    }
  else
    return CDI_APPLY_GO_ON;
}

/* Add grid (which must be Malloc'ed to vlist if not already found */
struct addIffNewRes cdiVlistAddGridIfNew(int vlistID, grid_t *grid, int mode)
{
  /*
    mode: 0 search in vlist and grid table
          1 search in grid table only
   */
  bool gridglobdefined = false;
  bool griddefined = false;
  int gridID = CDI_UNDEFID;
  vlist_t *vlistptr = vlist_to_pointer(vlistID);

  unsigned ngrids = (unsigned)vlistptr->ngrids;

  if ( mode == 0 )
    for ( unsigned index = 0; index < ngrids; index++ )
      {
	if ( (gridID = vlistptr->gridIDs[index]) != UNDEFID )
          {
            if ( gridCompare(gridID, grid) == false )
              {
                griddefined = true;
                break;
              }
          }
        else
          Error("Internal problem: undefined gridID in vlist "
                "%d, position %u!", vlistID, index);

      }

  if ( ! griddefined )
    {
      struct gridCompareSearchState query;
      query.queryKey = grid;// = { .queryKey = grid };
      if ((gridglobdefined
           = (cdiResHFilterApply(&gridOps, gridCompareSearch, &query)
              == CDI_APPLY_STOP)))
        gridID = query.resIDValue;

      if ( mode == 1 && gridglobdefined )
	for (unsigned index = 0; index < ngrids; index++ )
	  if ( vlistptr->gridIDs[index] == gridID )
	    {
	      gridglobdefined = false;
	      break;
	    }
    }

  if ( ! griddefined )
    {
      if ( ! gridglobdefined )
        {
          grid->self = gridID = reshPut(grid, &gridOps);
          gridComplete(grid);
        }
      vlistptr->gridIDs[ngrids] = gridID;
      vlistptr->ngrids++;
    }

  return (struct addIffNewRes){ .Id = gridID,
      .isNew = !griddefined && !gridglobdefined };
}

const struct gridVirtTable cdiGridVtable
  = {
  .destroy = gridDestroyKernel,
  .copy = grid_copy_base,
  .copyScalarFields = grid_copy_base_scalar_fields,
  .copyArrayFields = grid_copy_base_array_fields,
  .defXVals = gridDefXValsSerial,
  .defYVals = gridDefYValsSerial,
  .defMask = gridDefMaskSerial,
  .defMaskGME = gridDefMaskGMESerial,
  .defXBounds = gridDefXBoundsSerial,
  .defYBounds = gridDefYBoundsSerial,
  .defArea = gridDefAreaSerial,
  .inqXVal = gridInqXValSerial,
  .inqYVal = gridInqYValSerial,
  .inqXVals = gridInqXValsSerial,
  .inqYVals = gridInqYValsSerial,
  .inqXValsPtr = gridInqXValsPtrSerial,
  .inqYValsPtr = gridInqYValsPtrSerial,
  .compareXYFull = compareXYvals,
  .compareXYAO = compareXYvals2,
  .inqArea = gridInqAreaSerial,
  .inqAreaPtr = gridInqAreaPtrBase,
  .hasArea = gridHasAreaBase,
  .inqMask = gridInqMaskSerial,
  .inqMaskGME = gridInqMaskGMESerial,
  .inqXBounds = gridInqXBoundsSerial,
  .inqYBounds = gridInqYBoundsSerial,
  .inqXBoundsPtr = gridInqXBoundsPtrSerial,
  .inqYBoundsPtr = gridInqYBoundsPtrSerial,
};

/*
 * Local Variables:
 * c-file-style: "Java"
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * require-trailing-newline: t
 * End:
 */
