/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include    <HE5_HdfEosDef.h>

#define  DIMIN    100
#define  DIMOUT   60

/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "UTMGrid", (3) read external data
 */


int main()
{
  FILE            *in_1, *in_2, *in_3;

  herr_t          status = FAIL;

  int             i;
  int             data_in_1[DIMIN];
  int             data_in_2[DIMIN];
  int             data_in_3[DIMIN];
  int             data_out[DIMOUT];
  int             nfiles = FAIL;

  hid_t           gdfid  = FAIL;
  hid_t           GDid   = FAIL;

  size_t          namelength = 48;
 
  off_t           offset[3];

  char            filelist[256];

  hssize_t        start[2];

  hsize_t         count[2];
  hsize_t         size[3];



  /* Create the external data sets */
  /* ----------------------------- */
  for (i = 0; i < DIMIN; i++)
	{
	  data_in_1[i] = 1000 + i + 1;
	  data_in_2[i] = 2000 + i + 1;
	  data_in_3[i] = 3000 + i + 1;
	}

  /* Open the external data files */
  /* ---------------------------- */
  in_1 = fopen("external_1g.data", "w");
  in_2 = fopen("external_2g.data", "w");
  in_3 = fopen("external_3g.data", "w");


  /* Write data buffers to the external data files */
  /* --------------------------------------------- */
  fwrite(data_in_1, sizeof(int), DIMIN, in_1);
  fwrite(data_in_2, sizeof(int), DIMIN, in_2);
  fwrite(data_in_3, sizeof(int), DIMIN, in_3);

  /* Close the external data files */
  /* ----------------------------- */
  fclose(in_1);
  fclose(in_2);
  fclose(in_3);
  

  /*
   * Open the HDF grid file, "Grid.h5".
   */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDWR);
  if (gdfid != FAIL)
    {
	  /*
	   * Attach the "UTMGrid".
	   */
	  GDid = HE5_GDattach(gdfid, "UTMGrid");
	  if (GDid != FAIL)
		{
		  /* Read the external data field */
		  /* ---------------------------- */
		  start[0] = 0;
		  count[0] = DIMOUT;
		  status = HE5_GDreadfield(GDid, "ExtData", start, NULL, count, data_out);
		  printf("Status returned by HE5_GDreadfield() :    %d \n", status);
	    
		  /* Display external data set */
		  /* ------------------------- */
		  printf(" \n");
		  for (i = 0; i < DIMOUT; i++)
			printf("%d ", data_out[i]);
		  
		  /* Get the number of external files */
		  /* -------------------------------- */
		  nfiles = HE5_GDgetextdata(GDid, "ExtData", namelength, filelist, offset, size);
		  printf(" \n");
		  printf(" \n");
		  printf("Number of external files returned by HE5_GDgetextdata() :     %d \n", nfiles);
		  if (nfiles > 0)
			{
			  printf(" \n");
			  printf("External files: \"%s\"  \n", filelist);
			  printf(" \n");
			  printf("offsets:  ");
			  for (i = 0; i < nfiles; i++)
				printf("%d ", (int)offset[i]);

			  printf(" \n");
			  printf("sizes:    ");
			  for (i = 0; i < nfiles; i++)
				printf("%lu ", (unsigned long)size[i]);
			  printf(" \n");
			  printf(" \n");
			}
		}
	}
  
  /* Detach from the Grid */
  /* -------------------- */
  status = HE5_GDdetach(GDid);
  printf("Status returned by HE5_GDdetach() :       %d \n", status);
  
  /* Close the file */
  /* -------------- */
  status = HE5_GDclose(gdfid);
  printf("Status returned by HE5_GDclose() :        %d \n", status);

  return 0;
}








