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

      Split      splitcode       Split codes
      Split      splitparam      Split parameters
      Split      splitname       Split variables
      Split      splitlevel      Split levels
      Split      splitgrid       Split grids
      Split      splitzaxis      Split zaxis
      Split      splittabnum     Split table numbers
*/


#include <cdi.h>
#include "cdo.h"
#include "cdo_int.h"
#include "pstream.h"


static
void gen_filename(char *filename, int swap_obase, const char *obase, const char *suffix)
{
  if ( swap_obase ) strcat(filename, obase);
  if ( suffix[0] ) strcat(filename, suffix);
}


void *Split(void *argument)
{
  int nchars = 0;
  int varID;
  int code, tabnum, param;
  int nlevs;
  int recID, levelID, zaxisID, levID;
  int varID2, levelID2;
  int vlistID2;
  int *vlistIDs = NULL, *streamIDs = NULL;
  int  itmp[999];
  double ftmp[999];
  char filesuffix[32];
  char filename[8192];
  const char *refname;
  int nsplit = 0;
  int index;
  int i;
  int gridsize;
  int nmiss;
  int swap_obase = FALSE;
  const char *uuid_attribute = NULL;

  cdoInitialize(argument);

  if ( processSelf() != 0 ) cdoAbort("This operator can't be combined with other operators!");

  bool lcopy = UNCHANGED_RECORD;

  int SPLITCODE   = cdoOperatorAdd("splitcode",   0, 0, NULL);
  int SPLITPARAM  = cdoOperatorAdd("splitparam",  0, 0, NULL);
  int SPLITNAME   = cdoOperatorAdd("splitname",   0, 0, NULL);
  int SPLITLEVEL  = cdoOperatorAdd("splitlevel",  0, 0, NULL);
  int SPLITGRID   = cdoOperatorAdd("splitgrid",   0, 0, NULL);
  int SPLITZAXIS  = cdoOperatorAdd("splitzaxis",  0, 0, NULL);
  int SPLITTABNUM = cdoOperatorAdd("splittabnum", 0, 0, NULL);

  int operatorID = cdoOperatorID();

  for( i = 0; i < operatorArgc(); ++i )
    {
      if ( strcmp("swap", operatorArgv()[i]) == 0 )
          swap_obase = TRUE;
      else if ( strncmp("uuid=", operatorArgv()[i], 5 ) == 0 )
          uuid_attribute = operatorArgv()[i] + 5;
      else cdoAbort("Unknown parameter: >%s<", operatorArgv()[0]); 
    }

  int streamID1 = streamOpenRead(cdoStreamName(0));

  int vlistID1 = streamInqVlist(streamID1);

  int nvars  = vlistNvars(vlistID1);

  if ( swap_obase == 0 )
    {
      strcpy(filename, cdoStreamName(1)->args);
      nchars = strlen(filename);
    }

  refname = cdoStreamName(0)->argv[cdoStreamName(0)->argc-1];
  filesuffix[0] = 0;
  cdoGenFileSuffix(filesuffix, sizeof(filesuffix), streamInqFiletype(streamID1), vlistID1, refname);
  
  if ( operatorID == SPLITCODE )
    {
      nsplit = 0;
      for ( varID = 0; varID < nvars; varID++ )
	{
	  code = vlistInqVarCode(vlistID1, varID);
	  for ( index = 0; index < varID; index++ )
	    if ( code == vlistInqVarCode(vlistID1, index) ) break;

	  if ( index == varID )
	    {
	      itmp[nsplit] = code;
	      nsplit++;
	    }
	}

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      int codes[nsplit];
      memcpy(codes, itmp, nsplit*sizeof(int));

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      code    = vlistInqVarCode(vlistID1, varID);
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      if ( codes[index] == code )
		{
		  for ( levID = 0; levID < nlevs; levID++ )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }

	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  if ( codes[index] > 9999 )
	    {
	      sprintf(filename+nchars, "%05d", codes[index]);
	      gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);
	    }
	  else if ( codes[index] > 999 )
	    {
	      sprintf(filename+nchars, "%04d", codes[index]);
	      gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);
	    }
	  else
	    {
	      sprintf(filename+nchars, "%03d", codes[index]);
	      gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);
	    }

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITPARAM )
    {
      char paramstr[32];
      nsplit = 0;
      for ( varID = 0; varID < nvars; varID++ )
	{
	  param = vlistInqVarParam(vlistID1, varID);
	  for ( index = 0; index < varID; index++ )
	    if ( param == vlistInqVarParam(vlistID1, index) ) break;

	  if ( index == varID )
	    {
	      itmp[nsplit] = param;
	      nsplit++;
	    }
	}

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      int params[nsplit];
      memcpy(params, itmp, nsplit*sizeof(int));

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      param   = vlistInqVarParam(vlistID1, varID);
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      if ( params[index] == param )
		{
		  for ( levID = 0; levID < nlevs; levID++ )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }

	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  cdiParamToString(params[index], paramstr, sizeof(paramstr));

	  filename[nchars] = '\0';
	  strcat(filename, paramstr);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITTABNUM )
    {
      nsplit = 0;
      for ( varID = 0; varID < nvars; varID++ )
	{
	  tabnum  = tableInqNum(vlistInqVarTable(vlistID1, varID));
	  for ( index = 0; index < varID; index++ )
	    if ( tabnum == tableInqNum(vlistInqVarTable(vlistID1, index)) ) break;

	  if ( index == varID )
	    {
	      itmp[nsplit] = tabnum;
	      nsplit++;
	    }
	}

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      int tabnums[nsplit];
      memcpy(tabnums, itmp, nsplit*sizeof(int));

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      tabnum  = tableInqNum(vlistInqVarTable(vlistID1, varID));
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      if ( tabnums[index] == tabnum )
		{
		  for ( levID = 0; levID < nlevs; levID++ )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }
	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  sprintf(filename+nchars, "%03d", tabnums[index]);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITNAME )
    {
      char varname[CDI_MAX_NAME];
      nsplit = nvars;

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  varID = index;
	  zaxisID = vlistInqVarZaxis(vlistID1, varID);
	  nlevs   = zaxisInqSize(zaxisID);
	  for ( levID = 0; levID < nlevs; levID++ )
	    {
	      vlistDefIndex(vlistID1, varID, levID, index);
	      vlistDefFlag(vlistID1, varID, levID, TRUE);
	    }

	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  filename[nchars] = '\0';
	  vlistInqVarName(vlistID1, varID, varname);
	  strcat(filename, varname);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITLEVEL )
    {
      double level;
      int nzaxis = vlistNzaxis(vlistID1);
      nsplit = 0;
      for ( index = 0; index < nzaxis; index++ )
	{
	  zaxisID = vlistZaxis(vlistID1, index);
	  nlevs   = zaxisInqSize(zaxisID);
	  for ( levID = 0; levID < nlevs; levID++ )
	    {
	      level = zaxisInqLevel(zaxisID, levID);
	      for ( i = 0; i < nsplit; i++ )
		if ( IS_EQUAL(level, ftmp[i]) ) break;
	      if ( i == nsplit )
		ftmp[nsplit++] = level;
	    }
	}

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      double levels[nsplit];
      memcpy(levels, ftmp, nsplit*sizeof(double));

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      for ( levID = 0; levID < nlevs; levID++ )
		{
		  level = zaxisInqLevel(zaxisID, levID);
		  if ( IS_EQUAL(levels[index], level) )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }
	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  sprintf(filename+nchars, "%06g", levels[index]);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);
   
	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITGRID )
    {
      int gridID;

      nsplit = vlistNgrids(vlistID1);

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      int gridIDs[nsplit];
      for ( index = 0; index < nsplit; index++ )
	gridIDs[index] = vlistGrid(vlistID1, index);

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      gridID  = vlistInqVarGrid(vlistID1, varID);
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      if ( gridIDs[index] == gridID )
		{
		  for ( levID = 0; levID < nlevs; levID++ )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }
	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  sprintf(filename+nchars, "%02d", vlistGridIndex(vlistID1, gridIDs[index])+1);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else if ( operatorID == SPLITZAXIS )
    {
      int zaxisID;

      nsplit = vlistNzaxis(vlistID1);

      vlistIDs  = (int*) Malloc(nsplit*sizeof(int));
      streamIDs = (int*) Malloc(nsplit*sizeof(int));
      int zaxisIDs[nsplit];
      for ( index = 0; index < nsplit; index++ )
	zaxisIDs[index] = vlistZaxis(vlistID1, index);

      for ( index = 0; index < nsplit; index++ )
	{
	  vlistClearFlag(vlistID1);
	  for ( varID = 0; varID < nvars; varID++ )
	    {
	      zaxisID = vlistInqVarZaxis(vlistID1, varID);
	      nlevs   = zaxisInqSize(zaxisID);
	      if ( zaxisIDs[index] == zaxisID )
		{
		  for ( levID = 0; levID < nlevs; levID++ )
		    {
		      vlistDefIndex(vlistID1, varID, levID, index);
		      vlistDefFlag(vlistID1, varID, levID, TRUE);
		    }
		}
	    }
	  vlistID2 = vlistCreate();
	  vlistCopyFlag(vlistID2, vlistID1);
	  vlistIDs[index] = vlistID2;

	  sprintf(filename+nchars, "%02d", vlistZaxisIndex(vlistID1, zaxisIDs[index])+1);
	  gen_filename(filename, swap_obase, cdoStreamName(1)->args, filesuffix);

	  argument_t *fileargument = file_argument_new(filename);
	  streamIDs[index] = streamOpenWrite(fileargument, cdoFiletype());
	  file_argument_free(fileargument);
	}
    }
  else
    {
      cdoAbort("not implemented!");
    }

  for ( index = 0; index < nsplit; index++ )
    {
      if ( uuid_attribute ) cdo_def_tracking_id(vlistIDs[index], uuid_attribute);

      streamDefVlist(streamIDs[index], vlistIDs[index]);
    }

  double *array = NULL;
  if ( ! lcopy )
    {
      gridsize = vlistGridsizeMax(vlistID1);
      if ( vlistNumber(vlistID1) != CDI_REAL ) gridsize *= 2;
      array = (double *) Malloc(gridsize*sizeof(double));
    }

  int nrecs;
  int tsID = 0;
  while ( (nrecs = streamInqTimestep(streamID1, tsID)) )
    {
      for ( index = 0; index < nsplit; index++ )
	streamDefTimestep(streamIDs[index], tsID);

      for ( recID = 0; recID < nrecs; recID++ )
	{
	  streamInqRecord(streamID1, &varID, &levelID);

	  index    = vlistInqIndex(vlistID1, varID, levelID);
	  vlistID2 = vlistIDs[index];
	  varID2   = vlistFindVar(vlistID2, varID);
	  levelID2 = vlistFindLevel(vlistID2, varID, levelID);
	  /*
	    printf("%d %d %d %d %d %d\n", index, vlistID2, varID, levelID, varID2, levelID2);
	  */
	  streamDefRecord(streamIDs[index], varID2, levelID2);
	  if ( lcopy )
	    {
	      streamCopyRecord(streamIDs[index], streamID1);
	    }
	  else
	    {
	      streamReadRecord(streamID1, array, &nmiss);
	      streamWriteRecord(streamIDs[index], array, nmiss);
	    }
	}

      tsID++;
    }

  streamClose(streamID1);

  for ( index = 0; index < nsplit; index++ )
    {
      streamClose(streamIDs[index]);
      vlistDestroy(vlistIDs[index]);
    }
 
  if ( ! lcopy )
    if ( array ) Free(array);

  if ( vlistIDs  ) Free(vlistIDs);
  if ( streamIDs ) Free(streamIDs);

  cdoFinish();

  return 0;
}
