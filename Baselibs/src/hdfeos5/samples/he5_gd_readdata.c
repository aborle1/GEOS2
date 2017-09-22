/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include      <HE5_HdfEosDef.h>


/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "UTMGrid", (3) read data from the "Vegetation" field. and (4) read 
 * global, group, and  local attributes.
 */


int main()
{
  herr_t          status = FAIL;

  int             i, j;
  int             grpattr[3] = {-9,-9,-9};         /* group attribute  */

  hid_t           gdfid = FAIL;
  hid_t           GDid  = FAIL;

  float           flt     = -999.;	               /* global attribute */
  float           attr[4] = {-9.9,-9.9,-9.9,-9.9}; /* local attribute  */
  float           veg[200][120];
      
  hssize_t        start[2] ={0, 0};
    
  hsize_t         edge[2] = {200, 100};

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
		  status = HE5_GDreadfield(GDid, "Vegetation", start, NULL, edge, veg);
		  printf("Status returned by HE5_GDreadfield() :    %d \n", status );
		  for (i = 0; i < 5; i++)
			for (j = 0; j < 10; j++)
			  printf("\t\t %f \n", veg[i][j]);
	  
		  status = HE5_GDreadattr(GDid, "GlobalAttribute", &flt);
		  printf("Status returned by HE5_GDreadattr() :     %d \n", status );
		  printf("\tGlobal attribute reads: \n");
		  printf("\t\t %f \n", flt);

		  status = HE5_GDreadgrpattr(GDid, "GroupAttribute", grpattr);
		  printf("Status returned by HE5_GDreadgrpattr() :  %d \n", status );
		  printf("\tGroup attribute reads: \n");
		  for (i = 0; i < 3; i++)
			printf("\t\t %d \n", grpattr[i]);

		  status = HE5_GDreadlocattr(GDid, "Vegetation","LocalAttribute", attr);
		  printf("Status returned by HE5_GDreadlocattr() :  %d \n", status );
		  printf("\tLocal attribute reads: \n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %f \n", attr[i]);
		  
		}
    }

  status = HE5_GDdetach(GDid);
  status = HE5_GDclose(gdfid);

  return 0;
}

