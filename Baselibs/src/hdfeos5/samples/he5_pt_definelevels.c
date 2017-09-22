/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |  
 ----------------------------------------------------------------------------  
 */


#include <HE5_HdfEosDef.h>

/* File name: he5_pt_definelevels.c */
/* -------------------------------- */

/*
 * In this example we will open the "Point.h5" HDF-EOS file 
 *             and define the point levels.
 */


int main()
{
  herr_t            status = FAIL;

  int               i;

  hid_t             ptfid = FAIL, PTid = FAIL;

  HE5_CmpDTSinfo    *dtsinfo;

  typedef struct
	{
	  double          time;
	  float           concentr[4];
	  char            spec[8];

	} InputData1;

  typedef struct
	{
	  char        label[8];
	  double      lon;
	  double      lat;
	  int         date;
	  char        id[8];
	} InputData2;


  typedef struct
	{
	  double      time;
	  float       rain;
	  float       temp;
	  char        id[8];
	} InputData3;


  typedef struct
	{
	  char        label[10];
	  int         date;
	  int         weight;
	  char        id[8];
	} InputData4;


  typedef struct
	{
	  double      time;
	  double      lon;
	  double      lat;
	  float       rain;
	  float       temp;
	  char        id[8];
	} InputData5;


  /*
   * We first open the HDF-EOS point file, "Point.h5".  Because this file
   * already exist and we wish to write to it, we use the H5F_ACC_RDWR access
   * code in the open statement.  The PTopen routine returns the point file
   * id, ptfid, which is used to identify the file in subsequent routines.
   */

  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDWR);
  if (ptfid != FAIL)
    {
	  /* Simple Point */
	  /* ------------ */
	  PTid = HE5_PTattach(ptfid, "Simple Point");

      dtsinfo = (HE5_CmpDTSinfo *)calloc(1, sizeof(HE5_CmpDTSinfo));

	  /* Set up level data structure */
	  /* --------------------------- */
	  dtsinfo->nfields = 3;

	  dtsinfo->rank[0] = 1;
	  dtsinfo->rank[1] = 1;
	  dtsinfo->rank[2] = 1;

	  dtsinfo->offset[0] = HOFFSET(InputData1, time);
	  dtsinfo->offset[1] = HOFFSET(InputData1, concentr);
	  dtsinfo->offset[2] = HOFFSET(InputData1, spec);
	  
	  dtsinfo->dtype[0] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[1] = H5T_NATIVE_FLOAT;
	  dtsinfo->dtype[2] = H5T_NATIVE_CHAR;

	  dtsinfo->array[0] = 0;
	  dtsinfo->array[1] = 1;
	  dtsinfo->array[2] = 1;

	  for (i = 0; i < 3; i++)
		dtsinfo->fieldname[i] = (char *)calloc(64, sizeof(char));

	  strcpy(dtsinfo->fieldname[0],"Time");
	  strcpy(dtsinfo->fieldname[1],"Concentration");
	  strcpy(dtsinfo->fieldname[2],"Species");
	  
	  dtsinfo->dims[0][0] = 1;
	  dtsinfo->dims[1][0] = 4;
	  dtsinfo->dims[2][0] = 8;

      dtsinfo->datasize = (size_t)sizeof(InputData1);
	
	  status = HE5_PTdeflevel(PTid, "Sensor", dtsinfo);
	  printf("Status returned by HE5_PTdeflevel() :   %d \n", status);
	  
	  for (i = 0; i < 3; i++)
		{
		  free(dtsinfo->fieldname[i]);
		  dtsinfo->fieldname[i] = NULL;
		}

	  free(dtsinfo);


	  HE5_PTdetach(PTid);

	  /* Fixed Buoy Point */
	  /* ---------------- */
	  PTid = HE5_PTattach(ptfid, "FixedBuoy Point");

      dtsinfo = (HE5_CmpDTSinfo *)calloc(1, sizeof(HE5_CmpDTSinfo));

	  /* Set up level data struvcture */
	  /* ---------------------------- */
	  dtsinfo->nfields = 5;

	  dtsinfo->rank[0] = 1;
	  dtsinfo->rank[1] = 1;
	  dtsinfo->rank[2] = 1;
	  dtsinfo->rank[3] = 1;
	  dtsinfo->rank[4] = 1;

	  dtsinfo->array[0] = 1;
	  dtsinfo->array[1] = 0;
	  dtsinfo->array[2] = 0;
	  dtsinfo->array[3] = 0;
	  dtsinfo->array[4] = 1;

	  dtsinfo->offset[0] = HOFFSET(InputData2, label);
	  dtsinfo->offset[1] = HOFFSET(InputData2, lon);
	  dtsinfo->offset[2] = HOFFSET(InputData2, lat);
	  dtsinfo->offset[3] = HOFFSET(InputData2, date);
	  dtsinfo->offset[4] = HOFFSET(InputData2, id);
	  
	  dtsinfo->dtype[0] = H5T_NATIVE_CHAR;
	  dtsinfo->dtype[1] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[2] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[3] = H5T_NATIVE_INT;
	  dtsinfo->dtype[4] = H5T_NATIVE_CHAR;

	  for (i = 0; i < 5; i++)
		dtsinfo->fieldname[i] = (char *)calloc(64, sizeof(char));

	  strcpy(dtsinfo->fieldname[0],"Label");
	  strcpy(dtsinfo->fieldname[1],"Longitude");
	  strcpy(dtsinfo->fieldname[2],"Latitude");
	  strcpy(dtsinfo->fieldname[3],"DeployDate");
	  strcpy(dtsinfo->fieldname[4],"ID");
	  
	  dtsinfo->dims[0][0] = 8;
	  dtsinfo->dims[1][0] = 1;
	  dtsinfo->dims[2][0] = 1;
	  dtsinfo->dims[3][0] = 1;
	  dtsinfo->dims[4][0] = 8;

      dtsinfo->datasize = (size_t)sizeof(InputData2);

	  status = HE5_PTdeflevel(PTid, "Desc-Loc", dtsinfo);
	  printf("Status returned by HE5_PTdeflevel() :   %d \n", status);

	  for (i = 0; i < 5; i++)
		free(dtsinfo->fieldname[i]);

	  free(dtsinfo);

	  dtsinfo = (HE5_CmpDTSinfo *)calloc(1, sizeof(HE5_CmpDTSinfo));
	  
	  /* Set up level data structure */
	  /* --------------------------- */
	  dtsinfo->nfields = 4;

	  dtsinfo->rank[0] = 1;
	  dtsinfo->rank[1] = 1;
	  dtsinfo->rank[2] = 1;
	  dtsinfo->rank[3] = 1;

	  dtsinfo->array[0] = 0;
	  dtsinfo->array[1] = 0;
	  dtsinfo->array[2] = 0;
	  dtsinfo->array[3] = 1;

	  dtsinfo->offset[0] = HOFFSET(InputData3, time);
	  dtsinfo->offset[1] = HOFFSET(InputData3, rain);
	  dtsinfo->offset[2] = HOFFSET(InputData3, temp);
	  dtsinfo->offset[3] = HOFFSET(InputData3, id);
	  
	  dtsinfo->dtype[0] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[1] = H5T_NATIVE_FLOAT;
	  dtsinfo->dtype[2] = H5T_NATIVE_FLOAT;
	  dtsinfo->dtype[3] = H5T_NATIVE_CHAR;

	  for (i = 0; i < 4; i++)
		dtsinfo->fieldname[i] = (char *)calloc(64, sizeof(char));

	  strcpy(dtsinfo->fieldname[0],"Time");
	  strcpy(dtsinfo->fieldname[1],"Rainfall");
	  strcpy(dtsinfo->fieldname[2],"Temperature");
	  strcpy(dtsinfo->fieldname[3],"ID");
	  
	  dtsinfo->dims[0][0] = 1;
	  dtsinfo->dims[1][0] = 1;
	  dtsinfo->dims[2][0] = 1;
	  dtsinfo->dims[3][0] = 8;

      dtsinfo->datasize = (size_t)sizeof(InputData3);

	  status = HE5_PTdeflevel(PTid, "Observations", dtsinfo);
	  printf("Status returned by HE5_PTdeflevel() :   %d \n", status);

	  for (i = 0; i < 4; i++)
		free(dtsinfo->fieldname[i]);

	  free(dtsinfo);


	  status = HE5_PTdeflinkage(PTid, "Desc-Loc", "Observations", "ID");
	  printf("Status returned by HE5_PTdeflinkage() : %d \n", status);

	  status = HE5_PTdetach(PTid);
	  printf("Status returned by HE5_PTdetach() :     %d\n", status);
	
	  /* Floating Buoy Point */
	  /* ------------------- */
	  PTid = HE5_PTattach(ptfid, "FloatBuoy Point");


	  dtsinfo = (HE5_CmpDTSinfo *)calloc(1, sizeof(HE5_CmpDTSinfo));

	  /* Set up level data structure */
	  /* --------------------------- */
	  dtsinfo->nfields = 4;

	  dtsinfo->rank[0] = 1;
	  dtsinfo->rank[1] = 1;
	  dtsinfo->rank[2] = 1;
	  dtsinfo->rank[3] = 1;

	  dtsinfo->array[0] = 1;
	  dtsinfo->array[1] = 0;
	  dtsinfo->array[2] = 0;
	  dtsinfo->array[3] = 1;

	  dtsinfo->offset[0] = HOFFSET(InputData4, label);
	  dtsinfo->offset[1] = HOFFSET(InputData4, date);
	  dtsinfo->offset[2] = HOFFSET(InputData4, weight);
	  dtsinfo->offset[3] = HOFFSET(InputData4, id);
	  
	  dtsinfo->dtype[0] = H5T_NATIVE_CHAR;
	  dtsinfo->dtype[1] = H5T_NATIVE_INT;
	  dtsinfo->dtype[2] = H5T_NATIVE_INT;
	  dtsinfo->dtype[3] = H5T_NATIVE_CHAR;

	  for (i = 0; i < 4; i++)
		dtsinfo->fieldname[i] = (char *)calloc(64, sizeof(char));

	  strcpy(dtsinfo->fieldname[0],"Label");
	  strcpy(dtsinfo->fieldname[1],"DeployDate");
	  strcpy(dtsinfo->fieldname[2],"Weight");
	  strcpy(dtsinfo->fieldname[3],"ID");
	  
	  dtsinfo->dims[0][0] = 8;
	  dtsinfo->dims[1][0] = 1;
	  dtsinfo->dims[2][0] = 1;
	  dtsinfo->dims[3][0] = 8;

      dtsinfo->datasize = (size_t)sizeof(InputData4);

	  status = HE5_PTdeflevel(PTid, "Description",dtsinfo );
	  printf("Status returned by HE5_PTdeflevel() :   %d \n", status);

	  for (i = 0; i < 4; i++)
		free(dtsinfo->fieldname[i]);

	  free(dtsinfo);


	  /* Define Data Level */

	  dtsinfo = (HE5_CmpDTSinfo *)calloc(1, sizeof(HE5_CmpDTSinfo));
	  
	  /* Set up level data structure */
	  /* --------------------------- */
	  dtsinfo->nfields = 6;

	  dtsinfo->rank[0] = 1;
	  dtsinfo->rank[1] = 1;
	  dtsinfo->rank[2] = 1;
	  dtsinfo->rank[3] = 1;
	  dtsinfo->rank[4] = 1;
	  dtsinfo->rank[5] = 1;

	  dtsinfo->array[0] = 0;
	  dtsinfo->array[1] = 0;
	  dtsinfo->array[2] = 0;
	  dtsinfo->array[3] = 0;
	  dtsinfo->array[4] = 0;
	  dtsinfo->array[5] = 1;

	  dtsinfo->offset[0] = HOFFSET(InputData5, time);
	  dtsinfo->offset[1] = HOFFSET(InputData5, lon);
	  dtsinfo->offset[2] = HOFFSET(InputData5, lat);
	  dtsinfo->offset[3] = HOFFSET(InputData5, rain);
	  dtsinfo->offset[4] = HOFFSET(InputData5, temp);
	  dtsinfo->offset[5] = HOFFSET(InputData5, id);
	  
	  dtsinfo->dtype[0] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[1] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[2] = H5T_NATIVE_DOUBLE;
	  dtsinfo->dtype[3] = H5T_NATIVE_FLOAT;
	  dtsinfo->dtype[4] = H5T_NATIVE_FLOAT;
	  dtsinfo->dtype[5] = H5T_NATIVE_CHAR;

	  for (i = 0; i < 6; i++)
		dtsinfo->fieldname[i] = (char *)calloc(64, sizeof(char));

	  strcpy(dtsinfo->fieldname[0],"Time");
	  strcpy(dtsinfo->fieldname[1],"Longitude");
	  strcpy(dtsinfo->fieldname[2],"Latitude");
	  strcpy(dtsinfo->fieldname[3],"Rainfall");
	  strcpy(dtsinfo->fieldname[4],"Temperature");
	  strcpy(dtsinfo->fieldname[5],"ID");
	  
	  dtsinfo->dims[0][0] = 1;
	  dtsinfo->dims[1][0] = 1;
	  dtsinfo->dims[2][0] = 1;
	  dtsinfo->dims[3][0] = 1;
	  dtsinfo->dims[4][0] = 1;
	  dtsinfo->dims[5][0] = 8;

      dtsinfo->datasize = (size_t)sizeof(InputData5);

	  status = HE5_PTdeflevel(PTid, "Measurements", dtsinfo);
	  printf("Status returned by HE5_PTdeflevel() :   %d \n", status);

	  for (i = 0; i < 6; i++)
		free(dtsinfo->fieldname[i]);	  

	  free(dtsinfo);

	  status = HE5_PTdeflinkage(PTid, "Description", "Measurements", "ID");
	  printf("Status returned by HE5_PTdeflinkage() : %d \n", status);

		  
	  HE5_PTdetach(PTid);
	  HE5_PTclose(ptfid);
	}
  else 
    {
	  printf("File ID returned by HE5_PTopen() : %d \n", ptfid);
    }
  
  return 0;
}
  









