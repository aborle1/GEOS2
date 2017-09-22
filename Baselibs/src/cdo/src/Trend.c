/*
  This file is part of CDO. CDO is a collection of Operators to
  manipulate and analyse Climate model Data.

  Copyright (C) 2003-2016 Uwe Schulzweida, <uwe.schulzweida AT mpimet.mpg.de>
  See COPYING file for copying and redistribution conditions.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
*/

/*
   This module contains the following operators:

      Trend      trend           Trend
*/


#include <cdi.h>
#include "cdo.h"
#include "cdo_int.h"
#include "pstream.h"


void *Trend(void *argument)
{
  int gridsize;
  int vdate = 0, vtime = 0;
  int nrecs, nrecords;
  int gridID, varID, levelID, recID;
  int tsID;
  int i, w;
  int streamID1, streamID2, streamID3;
  int vlistID1, vlistID2, taxisID1, taxisID2;
  int nmiss;
  int nvars;
  int *recVarID, *recLevelID;
  int nwork = 5;
  double zj;
  double temp1, temp2;
  double missval, missval1, missval2;
  field_t **work[5];
  field_t field1, field2;

  cdoInitialize(argument);

  streamID1 = streamOpenRead(cdoStreamName(0));

  vlistID1 = streamInqVlist(streamID1);
  vlistID2 = vlistDuplicate(vlistID1);

  vlistDefNtsteps(vlistID2, 1);

  taxisID1 = vlistInqTaxis(vlistID1);
  taxisID2 = taxisDuplicate(taxisID1);
  vlistDefTaxis(vlistID2, taxisID2);

  nvars    = vlistNvars(vlistID1);
  nrecords = vlistNrecs(vlistID1);

  for ( varID = 0; varID < nvars; varID++ )
    vlistDefVarDatatype(vlistID2, varID, DATATYPE_FLT64);

  streamID2 = streamOpenWrite(cdoStreamName(1), cdoFiletype());
  streamID3 = streamOpenWrite(cdoStreamName(2), cdoFiletype());

  streamDefVlist(streamID2, vlistID2);
  streamDefVlist(streamID3, vlistID2);

  recVarID   = (int*) Malloc(nrecords*sizeof(int));
  recLevelID = (int*) Malloc(nrecords*sizeof(int));

  gridsize = vlistGridsizeMax(vlistID1);

  field_init(&field1);
  field_init(&field2);

  field1.ptr = (double*) Malloc(gridsize*sizeof(double));
  field2.ptr = (double*) Malloc(gridsize*sizeof(double));

  for ( w = 0; w < nwork; w++ )
    work[w] = field_calloc(vlistID1, FIELD_PTR);

  tsID = 0;
  while ( (nrecs = streamInqTimestep(streamID1, tsID)) )
    {
      vdate = taxisInqVdate(taxisID1);
      vtime = taxisInqVtime(taxisID1);

      zj = tsID;
      
      for ( recID = 0; recID < nrecs; recID++ )
	{
	  streamInqRecord(streamID1, &varID, &levelID);

	  if ( tsID == 0 )
	    {
	      recVarID[recID]   = varID;
	      recLevelID[recID] = levelID;
	    }

	  streamReadRecord(streamID1, field1.ptr, &nmiss);

	  missval  = vlistInqVarMissval(vlistID1, varID);
	  gridID   = vlistInqVarGrid(vlistID1, varID);
	  gridsize = gridInqSize(gridID);

	  for ( i = 0; i < gridsize; i++ )
	    if ( !DBL_IS_EQUAL(field1.ptr[i], missval) )
	      {
		work[0][varID][levelID].ptr[i] += zj;
		work[1][varID][levelID].ptr[i] += zj * zj;
		work[2][varID][levelID].ptr[i] += zj * field1.ptr[i];
		work[3][varID][levelID].ptr[i] += field1.ptr[i];
		work[4][varID][levelID].ptr[i]++;
	      }      
	}

      tsID++;
    }
	  

  taxisDefVdate(taxisID2, vdate);
  taxisDefVtime(taxisID2, vtime);
  streamDefTimestep(streamID2, 0);
  streamDefTimestep(streamID3, 0);

  for ( recID = 0; recID < nrecords; recID++ )
    {
      varID   = recVarID[recID];
      levelID = recLevelID[recID];

      missval  = vlistInqVarMissval(vlistID1, varID);
      gridID   = vlistInqVarGrid(vlistID1, varID);
      gridsize = gridInqSize(gridID);

      missval1  = missval;
      missval2  = missval;

      for ( i = 0; i < gridsize; i++ )
	{
	  temp1 = SUBMN(work[2][varID][levelID].ptr[i],
		      DIVMN( MULMN(work[0][varID][levelID].ptr[i], work[3][varID][levelID].ptr[i]), work[4][varID][levelID].ptr[i]));
	  temp2 = SUBMN(work[1][varID][levelID].ptr[i],
		      DIVMN( MULMN(work[0][varID][levelID].ptr[i], work[0][varID][levelID].ptr[i]), work[4][varID][levelID].ptr[i]));

	  field2.ptr[i] = DIVMN(temp1, temp2);
	  field1.ptr[i] = SUBMN( DIVMN(work[3][varID][levelID].ptr[i], work[4][varID][levelID].ptr[i]),
			      MULMN( DIVMN(work[0][varID][levelID].ptr[i], work[4][varID][levelID].ptr[i]), field2.ptr[i]));
	}

      nmiss = 0;
      for ( i = 0; i < gridsize; i++ )
	if ( DBL_IS_EQUAL(field1.ptr[i], missval) ) nmiss++;

      streamDefRecord(streamID2, varID, levelID);
      streamWriteRecord(streamID2, field1.ptr, nmiss);

      nmiss = 0;
      for ( i = 0; i < gridsize; i++ )
	if ( DBL_IS_EQUAL(field2.ptr[i], missval) ) nmiss++;

      streamDefRecord(streamID3, varID, levelID);
      streamWriteRecord(streamID3, field2.ptr, nmiss);
    }


  for ( w = 0; w < nwork; w++ ) field_free(work[w], vlistID1);

  if ( field1.ptr ) Free(field1.ptr);
  if ( field2.ptr ) Free(field2.ptr);

  if ( recVarID   ) Free(recVarID);
  if ( recLevelID ) Free(recLevelID);

  streamClose(streamID3);
  streamClose(streamID2);
  streamClose(streamID1);

  cdoFinish();

  return 0;
}
