#ifndef _GRID_H
#define _GRID_H

#include "cdi.h"
#include <stdbool.h>

typedef unsigned char mask_t;

typedef struct grid_t grid_t;

struct gridVirtTable
{
  void (*destroy)(grid_t *gridptr);
  grid_t *(*copy)(grid_t *gridptr);
  void (*copyScalarFields)(grid_t *gridptrOrig, grid_t *gridptrDup);
  void (*copyArrayFields)(grid_t *gridptrOrig, grid_t *gridptrDup);
  void (*defXVals)(grid_t *gridptr, const double *xvals);
  void (*defYVals)(grid_t *gridptr, const double *yvals);
  void (*defMask)(grid_t *gridptr, const int *mask);
  void (*defMaskGME)(grid_t *gridptr, const int *mask);
  void (*defXBounds)(grid_t *gridptr, const double *xbounds);
  void (*defYBounds)(grid_t *gridptr, const double *ybounds);
  void (*defArea)(grid_t *gridptr, const double *area);
  double (*inqXVal)(grid_t *gridptr, int index);
  double (*inqYVal)(grid_t *gridptr, int index);
  int (*inqXVals)(grid_t *gridptr, double *xvals);
  int (*inqYVals)(grid_t *gridptr, double *yvals);
  const double *(*inqXValsPtr)(grid_t *gridptr);
  const double *(*inqYValsPtr)(grid_t *gridptr);
  /* return if for both grids, all xval and all yval are equal */
  bool (*compareXYFull)(grid_t *gridRef, grid_t *gridTest);
  /* return if for both grids, x[0], y[0], x[size-1] and y[size-1] are
   * respectively equal */
  bool (*compareXYAO)(grid_t *gridRef, grid_t *gridTest);
  void (*inqArea)(grid_t *gridptr, double *area);
  const double *(*inqAreaPtr)(grid_t *gridptr);
  int (*hasArea)(grid_t *gridptr);
  int (*inqMask)(grid_t *gridptr, int *mask);
  int (*inqMaskGME)(grid_t *gridptr, int *mask_gme);
  int (*inqXBounds)(grid_t *gridptr, double *xbounds);
  int (*inqYBounds)(grid_t *gridptr, double *ybounds);
  const double *(*inqXBoundsPtr)(grid_t *gridptr);
  const double *(*inqYBoundsPtr)(grid_t *gridptr);
};

struct grid_t {
  int     self;
  int     type;                   /* grid type                      */
  int     prec;                   /* grid precision                 */
  int     proj;                   /* grid projection                */
  mask_t *mask;
  mask_t *mask_gme;
  double *xvals;
  double *yvals;
  double *area;
  double *xbounds;
  double *ybounds;
  double  xfirst, yfirst;
  double  xlast, ylast;
  double  xinc, yinc;
  double  lcc_originLon;          /* Lambert Conformal Conic        */
  double  lcc_originLat;
  double  lcc_lonParY;
  double  lcc_lat1;
  double  lcc_lat2;
  double  lcc_xinc;
  double  lcc_yinc;
  int     lcc_projflag;
  int     lcc_scanflag;
  short   lcc_defined;
  short   lcc2_defined;
  int     laea_defined;
  double  lcc2_lon_0;             /* Lambert Conformal Conic 2      */
  double  lcc2_lat_0;
  double  lcc2_lat_1;
  double  lcc2_lat_2;
  double  lcc2_a;
  double  laea_lon_0;             /* Lambert Azimuthal Equal Area   */
  double  laea_lat_0;
  double  laea_a;
  double  xpole, ypole, angle;    /* rotated north pole             */
  short   isCyclic;               /* TRUE for global cyclic grids   */
  short   isRotated;              /* TRUE for rotated grids         */
  short   xdef;                   /* 0: undefined 1:xvals 2:x0+xinc */
  short   ydef;                   /* 0: undefined 1:yvals 2:y0+yinc */
  int     nd, ni, ni2, ni3;       /* parameter for GRID_GME         */
  int     number, position;       /* parameter for GRID_REFERENCE   */
  int     trunc;                  /* parameter for GRID_SPECTEAL    */
  int     nvertex;
  char   *reference;
  unsigned char uuid[CDI_UUID_SIZE]; /* uuid for grid reference        */
  int    *rowlon;
  int     nrowlon;
  int     size;
  int     xsize;                  /* number of values along X */
  int     ysize;                  /* number of values along Y */
  int     np;                     /* number of parallels between a pole and the equator */
  short   lcomplex;
  short   hasdims;
  const char *xstdname;
  const char *ystdname;
  char    xdimname[CDI_MAX_NAME];
  char    ydimname[CDI_MAX_NAME];
  char    vdimname[CDI_MAX_NAME];
  char    xname[CDI_MAX_NAME];
  char    yname[CDI_MAX_NAME];
  char    xlongname[CDI_MAX_NAME];
  char    ylongname[CDI_MAX_NAME];
  char    xunits[CDI_MAX_NAME];
  char    yunits[CDI_MAX_NAME];
  char   *name;
  const struct gridVirtTable *vtable;
  void *extraData;
};


void grid_init(grid_t *gridptr);
void
cdiGridTypeInit(grid_t *gridptr, int gridtype, int size);
void grid_free(grid_t *gridptr);
grid_t *gridID2Ptr(int gridID);
extern const struct gridVirtTable cdiGridVtable;

unsigned cdiGridCount(void);

const double *gridInqXvalsPtr(int gridID);
const double *gridInqYvalsPtr(int gridID);

const double *gridInqXboundsPtr(int gridID);
const double *gridInqYboundsPtr(int gridID);
const double *gridInqAreaPtr(int gridID);

const char *gridInqXnamePtr(int gridID);
const char *gridInqYnamePtr(int gridID);

const char *gridInqReferencePtr(int gridID);

int gridGenerate(const grid_t *grid);

void cdiGridGetIndexList(unsigned, int * );

void
gridUnpack(char * unpackBuffer, int unpackBufferSize,
           int * unpackBufferPos, int originNamespace, void *context,
           int force_id);

struct addIffNewRes
{
  int Id;
  int isNew;
};

struct addIffNewRes cdiVlistAddGridIfNew(int vlistID, grid_t *grid, int mode);

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
