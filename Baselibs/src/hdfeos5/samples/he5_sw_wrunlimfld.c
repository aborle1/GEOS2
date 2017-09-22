/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include <HE5_HdfEosDef.h>

/* In this program we (1) open the "Swath.h5" HDF-EOS file, (2) attach to  */
/* the "Swath1" swath, and (3) write data to the "Longitude", "Latitude",  */
/*      and (appendable) "Spectra" fields                                  */
/* ----------------------------------------------------------------------- */


int main()
{
  herr_t          status = FAIL;

  int             i, j, k;
  int             track, xtrack;
  int             rank = FAIL;

  hid_t           swfid = FAIL;
  hid_t           SWid  = FAIL;

  hid_t     *ntype;

  hssize_t        start[3] = {0, 0, 0};
    
  hsize_t         count[3];
  hsize_t         dims[8] = {0, 0, 0, 0, 0, 0, 0, 0};

  long            attr[4] = {3, 5, 7, 11};

  float           lng[20][10], latcnt;  
  float           lat[20][10], loncnt;

  double          plane[15][40][20];
  double          tme[20];

  char            dimlist[80];
  char            maxdimlist[80];   

  hvl_t           buffer[4];

  size_t          datasize;


  /* Populate lon/lat data arrays */
  /* ---------------------------- */
  latcnt = 1.;
  loncnt = 1.;
  track  = 0;
  xtrack = 0;
 
  while(track < 20) {
	while(xtrack < 10) {
	  lat[track][xtrack] = latcnt;
	  lng[track][xtrack] = loncnt;
	  loncnt = loncnt + 1.;
	  xtrack++;
	}
	latcnt = latcnt + 1.;
	loncnt = 1.;
	track++;
	xtrack = 0;
  }
    

  /* Popolate spectra data arrays */
  /* ---------------------------- */
  for (i = 0; i < 15; i++){
	for (j = 0; j < 40; j++)
	  for (k = 0; k < 20; k++)
		plane[i][j][k] = (double)(j*100 + i);
  }


  /* Allocate memory for and populate data buffer */
  /* -------------------------------------------- */
  datasize = 0;
  for (i = 0; i < 4; i++)
	{
	  buffer[i].p = (hvl_t *)calloc( 25 *(i+1), sizeof(unsigned int));
	  buffer[i].len  =  25 * (i+1);
	  /* calculate the data buffer size (bytes) */
	  datasize += buffer[i].len * sizeof(unsigned int);
	  for ( j = 0; j < 25 * (i+1); j++)
		((unsigned int *)buffer[i].p)[j] = (i+1)*1000 + j;
	}


  /* Open the HDF swath file, "Swath.h5" */
  /* ----------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
    {
	  /* Attach the "Swath1" swath */
	  /* ------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  count[0] = 20;
		  count[1] = 10;

		  /* Write "Longitute" field */
		  /* ----------------------- */
		  status = HE5_SWwritefield(SWid, "Longitude", start, NULL, count, lng);
		  printf("status returned by HE5_SWwritefield(\"Longitude\"):         %d\n", status);
	    
		  /* Write "Latitude" field */
		  /* ---------------------- */
		  status = HE5_SWwritefield(SWid, "Latitude", start, NULL, count, lat);
		  printf("status returned by HE5_SWwritefield(\"Latitude\"):          %d\n", status);

		  /*  Write "Time" field */
		  /* ------------------- */
		  for (i = 0; i < 20; i++) 
			tme[i] = 34574087.3 + 84893.2*i;

		  count[0] = 20;
		  status = HE5_SWwritefield(SWid, "Time", start, NULL, count, tme);
		  printf("status returned by HE5_SWwritefield(\"Time\"):              %d\n", status);

		  /*  Write "Spectra" field  1st time */
		  /* -------------------------------- */

		  count[0] = 15;  
		  count[1] = 40;  
		  count[2] = 20;

		  status = HE5_SWwritefield(SWid, "Spectra", start, NULL, count, plane);
		  printf("status returned by HE5_SWwritefield(\"Spectra\"):           %d\n", status);

		  /* Retrieve information about "Spectra" field */
		  /* ------------------------------------------ */

		  ntype = (hid_t *)calloc(3,sizeof(hid_t));

		  status = HE5_SWfieldinfo(SWid, "Spectra", &rank, dims, ntype, dimlist, maxdimlist);
		  printf("Number of elements after first write: \n");
		  for ( i = 0; i < rank; i++)
			printf("\t\t %lu\n",(unsigned long)dims[i]);

		  printf("DimList string:        %s \n", dimlist);
		  printf("MaxdimList string:     %s \n", maxdimlist);

		  /*  Write Spectra Field  2d time */
		  /* ----------------------------- */

		  start[0] = 10;  
		  start[1] = 50;  
		  start[2] = 30;

		  count[0] = 15;  
		  count[1] = 40;  
		  count[2] = 20;

		  status = HE5_SWwritefield(SWid, "Spectra", start, NULL, count, plane);
		  printf("status returned by HE5_SWwritefield(\"Spectra\"):           %d\n", status);

		  /* Retrieve information about "Spectra" field */
		  /* ------------------------------------------ */

		  dims[0] = 0;  
		  dims[1] = 0; 
		  dims[2] = 0;
		  status = HE5_SWfieldinfo(SWid, "Spectra", &rank, dims, ntype, dimlist, maxdimlist);
		  printf("Number of elements after second write: \n");
		  for ( i = 0; i < rank; i++)
			{
			  printf("\t\t %lu\n",(unsigned long)dims[i]);
			}
		  printf("DimList string:        %s \n", dimlist);
		  printf("MaxdimList string:     %s \n", maxdimlist);            

		  free(ntype);

		  /* Write data to the profile */
		  /* ------------------------- */
		  start[0] = 0;  count[0] = 4;
		  status = HE5_PRwrite(SWid, "Profile-2000", start, NULL, count, datasize, buffer);
		  printf("Status returned by HE5_PRwrite(\"Profile-2000\"):           %d \n", status);
   
		  /* Write User-defined Attribute */
		  /* ---------------------------- */
		  count[0] = 4;
		  status = HE5_SWwriteattr(SWid, "TestAttr", H5T_NATIVE_INT, count, attr);
		  printf("status returned by HE5_SWwriteattr(\"TestAttr\"):           %d\n", status);
	    
		}
    }

  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);

  return 0;
}






