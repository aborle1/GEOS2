/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
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

  hid_t           gdfid = FAIL;
  hid_t           GDid  = FAIL;

  hssize_t        start[2];
  hsize_t         count[2];


  
  /* Create new data buffer */
  /* ---------------------- */
  for (i = 0; i < DIMTOT; i++)
	data[ i ] = 9999;

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
		  /* Write new data to the external files */
		  /* ------------------------------------ */
		  start[0] = 0;
		  count[0] = DIMTOT;
		  status = HE5_GDwritefield(GDid, "ExtData", start, NULL, count, data);
		  printf("Status returned by HE5_GDwritefield() :    %d \n", status);
	    
		  /* Open the external data files */
		  /* ---------------------------- */
		  in_1 = fopen("external_1g.data", "r");
		  in_2 = fopen("external_2g.data", "r");
		  in_3 = fopen("external_3g.data", "r");

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

  /* Detach from the grid  */
  /* --------------------- */
  status = HE5_GDdetach(GDid);
  printf("Status returned by HE5_GDdetach() :        %d \n", status);

  /* Close the file */
  /* -------------- */
  status = HE5_GDclose(gdfid);
  printf("Status returned by HE5_GDclose() :         %d \n", status); 
  
  return 0;
}








