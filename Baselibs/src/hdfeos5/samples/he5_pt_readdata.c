/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include <HE5_HdfEosDef.h>

int main()
{
  herr_t          status = FAIL;/* return status variable         */

  hid_t           ptfid = FAIL; /* HDFEOS Point file ID           */
  hid_t           PTid  = FAIL; /* Point structure ID             */

  int             i, j;        /* Loop indices                    */
  int             nflds = FAIL;/* Number of level fields          */
  int             IntAttr;     /* Integer attribute value         */

  long            nattr;       /* Number of attributes            */
  long            strbufsize;  /* Size of attribute list buffer   */

  hsize_t         nrecs = 0;   /* Number of records in a level    */

  size_t          datasize = 0;/* Size (in bytes) of data to read */

  HE5_CmpDTSinfo  level;      /* Level information data structure */
  HE5_CmpDTSinfo  inInfo;     /* Input information data structure */
  HE5_CmpDTSinfo  inInfo2;    /* Input information data structure */


  /* User-defined structure to read level data to */
  /* -------------------------------------------- */
  typedef struct {
          double   time;
          float    con[4];
          char     spec[8];
        } Sensor;

  Sensor   s[50];
  
  double   s1[50];


  /* Open the HDF-EOS file, "Point.h5" */
  /* --------------------------------- */
  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDONLY);
  printf("File ID returned by HE5_PTopen() :           %d \n", ptfid);

  /* Read Simple Point */
  /* ----------------- */
  PTid = HE5_PTattach(ptfid, "Simple Point");
  printf("Point ID returned by HE5_PTattach() :        %d \n", PTid);

  /* Get level information */
  /* --------------------- */
  status = HE5_PTlevelinfo(PTid, 0, &level);
  printf("Status returned by HE5_PTlevelinfo() :       %d \n", status);
  
  nflds = level.nfields;
  printf("Number of fields in level:   %d \n", nflds);
  for (i = 0; i < nflds; i++)
	{
	  printf("Field name:                  %s \n", level.fieldname[i]);
	  printf("Field rank:                  %d \n", level.rank[i]);
	  printf("Field type ID:               %d \n", level.dtype[i]);
	  for (j = 0; j < level.rank[i]; j++)
		printf("Field dims:                  %d \n", (int)level.dims[i][j]);
	  printf("Field class:                 %d \n", level.dclass[i]);
	}

  /* Get the number of records in level */
  /* ---------------------------------- */
  nrecs = HE5_PTnrecs(PTid, 0);
  printf("Number of records in level:  %lu \n", (unsigned long)nrecs);

  /* Set the data size */
  /* ----------------- */
  datasize = (size_t)sizeof(Sensor);

  /* Populate input information structure */
  /* ------------------------------------ */
  inInfo.nfields    = nflds;
  inInfo.datasize   = (size_t)sizeof(Sensor);

  inInfo.rank[0]    = 1;
  inInfo.rank[1]    = 1;
  inInfo.rank[2]    = 1;

  inInfo.offset[0]  = HOFFSET(Sensor, time);
  inInfo.offset[1]  = HOFFSET(Sensor, con);
  inInfo.offset[2]  = HOFFSET(Sensor, spec);

  inInfo.dtype[0]   = H5T_NATIVE_DOUBLE;
  inInfo.dtype[1]   = H5T_NATIVE_FLOAT;
  inInfo.dtype[2]   = H5T_NATIVE_CHAR;

  inInfo.dclass[0]  = H5T_NO_CLASS;
  inInfo.dclass[1]  = H5T_NO_CLASS;
  inInfo.dclass[2]  = H5T_NO_CLASS;

  inInfo.dims[0][0] = 1;
  inInfo.dims[1][0] = 4;
  inInfo.dims[2][0] = 8;

  inInfo.array[0] = 0;
  inInfo.array[1] = 1;
  inInfo.array[2] = 1;

  for( i = 0; i < nflds; i++)
  {
	inInfo.fieldname[i] = (char *)calloc(64, sizeof(char));
	strcpy(inInfo.fieldname[i], level.fieldname[i]);
  }

  /* Read the level data */
  /* ------------------- */
  status = HE5_PTreadlevel(PTid, 0, &inInfo, &datasize, s);
  printf("Status returned by HE5_PTreadlevel() :       %d \n", status);
  for (i = 0; i < nrecs; i++)
	printf("%lf  %f  %f  %f  %f  %s\n", s[i].time, s[i].con[0], s[i].con[1], s[i].con[2], s[i].con[3], s[i].spec);


  /* Release memory */
  /* -------------- */
  for (i = 0; i < nflds; i++)
	if (inInfo.fieldname[i] != NULL) free(inInfo.fieldname[i]);

  /* Set the data size */
  /* ----------------- */
  datasize = (size_t)sizeof(double);

  /* Populate input information structure */
  /* ------------------------------------ */
  inInfo2.nfields    = 1;
  inInfo2.datasize   = (size_t)sizeof(double);

  inInfo2.rank[0]    = 1;

  inInfo2.offset[0]  = 0;

  inInfo2.dtype[0]   = H5T_NATIVE_DOUBLE;

  inInfo2.dclass[0]  = H5T_FLOAT;

  inInfo2.dims[0][0] = 1;

  inInfo2.array[0]   = 0;

  inInfo2.fieldname[0] = (char *)calloc(64, sizeof(char));
  strcpy(inInfo2.fieldname[0], level.fieldname[0]);

  /* Read the level data */
  /* ------------------- */
  status = HE5_PTreadlevel(PTid, 0, &inInfo2, &datasize, s1);
  printf("Status returned by HE5_PTreadlevel() :       %d \n", status);
  for (i = 0; i < nrecs; i++)
	printf("%lf \n", s1[i]);
  
  /* Release memory */
  /* -------------- */
  if (inInfo2.fieldname[0] != NULL) 
	free(inInfo2.fieldname[0]);
  
  for (i = 0; i < nflds; i++)
	if(level.fieldname[i] != NULL) free(level.fieldname[i]);

  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :          %d \n", status);
    
  PTid = HE5_PTattach(ptfid, "FixedBuoy Point");
  printf("Point ID returned by HE5_PTattach() :        %d \n", PTid);

  /* Read Fixed Buoy Point Attributes */
  /* -------------------------------- */
  nattr  = HE5_PTinqattrs(PTid, NULL, &strbufsize);
  status = HE5_PTreadattr(PTid, "GlobalAttr_Integer", &IntAttr);
  printf("Status returned by HE5_PTreadattr() :        %d \n", status);
  printf(" \n");
  printf("Integer attribute value:                     %d\n", IntAttr);
	
  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :          %d \n", status);

  status = HE5_PTclose(ptfid);
  printf("Status returned by HE5_PTclose() :           %d \n", status);

  return 0;
}







