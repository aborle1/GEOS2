/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |   
----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>

#define  DIM      100
#define  DIMTOT   60

int main()
{
  FILE            *in_1, *in_2, *in_3;

  herr_t          status = FAIL;

  int             i;
  int             data_out_1[DIM];
  int             data_out_2[DIM];
  int             data_out_3[DIM];
  int             data[DIMTOT];

  hid_t           zafid = FAIL;
  hid_t           ZAid  = FAIL;

  hssize_t        start[2];
  hsize_t         count[2];


  
  /* Create new data buffer */
  /* ---------------------- */
  for (i = 0; i < DIMTOT; i++)
	data[ i ] = 9999;

  /* Open the HDF-EOS za file, "ZA.he5" */
  /* --------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDWR);
  if (zafid != FAIL)
	{
	  /* Attach the "ZA1" za */
	  /* ------------------- */
	  ZAid = HE5_ZAattach(zafid, "ZA1");
	  if (ZAid != FAIL)
		{
		  /* Write new data to the external files */
		  /* ------------------------------------ */
		  start[0] = 0;
		  count[0] = DIMTOT;
		  status = HE5_ZAwrite(ZAid, "ExtData", start, NULL, count, data);
		  printf("Status returned by HE5_ZAwrite() :    %d \n", status);
	    

		  /* Open the external data files */
		  /* ---------------------------- */
		  in_1 = fopen("external_1.data", "r");
		  in_2 = fopen("external_2.data", "r");
		  in_3 = fopen("external_3.data", "r");

		  /* Read data from external data files */
		  /* ---------------------------------- */
		  fread(data_out_1, sizeof(int), DIM, in_1);
		  fread(data_out_2, sizeof(int), DIM, in_2);
		  fread(data_out_3, sizeof(int), DIM, in_3);
		  
		  /* Close the external data files */
		  /* ----------------------------- */
		  fclose(in_1);
		  fclose(in_2);
		  fclose(in_3);
  
		  /* Check the contents of external files */
		  /* ------------------------------------ */
		  printf(" \n");
		  printf("Reading data from 1st file: \n");
		  printf(" \n");
		  for (i = 0; i < DIM; i++)
			printf("%d ", data_out_1[i]);

		  printf(" \n");
		  printf(" \n");
		  printf("Reading data from 2d file: \n");
		  printf(" \n");
		  for (i = 0; i < DIM; i++)
			printf("%d ", data_out_2[i]);
		  
		  printf(" \n");
		  printf(" \n");
		  printf("Reading data from 3d file: \n");
		  printf(" \n"); 
		  for (i = 0; i < DIM; i++)
			printf("%d ", data_out_3[i]);
		  printf(" \n");
		  printf(" \n");

		}
	}

  /* Detach from the za */
  /* ------------------ */
  status = HE5_ZAdetach(ZAid);
  printf("Status returned by HE5_ZAdetach() :        %d \n", status);

  /* Close the file */
  /* -------------- */
  status = HE5_ZAclose(zafid);
  printf("Status returned by HE5_ZAclose() :         %d \n", status); 
  
  return 0;
}








