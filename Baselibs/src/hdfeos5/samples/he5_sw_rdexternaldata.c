/*
 ---------------------------------------------------------------------------- 
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include    <HE5_HdfEosDef.h>

#define  DIMIN    100
#define  DIMOUT   60

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

  hid_t           swfid  = FAIL;
  hid_t           SWid   = FAIL;

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
  in_1 = fopen("external_1.data", "w");
  in_2 = fopen("external_2.data", "w");
  in_3 = fopen("external_3.data", "w");


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
  


  /* Open the HDF-EOS swath file, "Swath.h5" */
  /* --------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
	{
	  /* Attach the "Swath1" swath */
	  /* ------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  /* Read the external data field */
		  /* ---------------------------- */
		  start[0] = 0;
		  count[0] = DIMOUT;
		  status = HE5_SWreadfield(SWid, "ExtData", start, NULL, count, data_out);
		  printf("Status returned by HE5_SWreadfield() :    %d \n", status);
	    
		  /* Display external data set */
		  /* ------------------------- */
		  printf(" \n");
		  for (i = 0; i < DIMOUT; i++)
			printf("%d ", data_out[i]);
		  
		  /* Get the number of external files */
		  /* -------------------------------- */
		  nfiles = HE5_SWgetextdata(SWid, "ExtData", namelength, filelist, offset, size);
		  printf(" \n");
		  printf(" \n");
		  printf("Number of external files returned by HE5_SWgetextdata() :     %d \n", nfiles);
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
  
  /* Detach from the swath */
  /* --------------------- */
  status = HE5_SWdetach(SWid);
  printf("Status returned by HE5_SWdetach() :       %d \n", status);
  
  /* Close the file */
  /* -------------- */
  status = HE5_SWclose(swfid);
  printf("Status returned by HE5_SWclose() :        %d \n", status);

  return 0;
}








