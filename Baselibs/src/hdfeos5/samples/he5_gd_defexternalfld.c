/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>

/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "UTMGrid" grid, and (3) define the external data field "ExtData".
 */

int main()
{  
  herr_t          status = FAIL;

  hid_t           gdfid  = FAIL;
  hid_t           GDid   = FAIL;

  off_t           offset[3];
 
  hsize_t         size[3];
  
  
  /* Open the file, "Grid.h5", using the H5F_ACC_RDWR access code */
  /* ------------------------------------------------------------- */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDWR);
  if (gdfid != FAIL)
    {	  
	  /* Attach to the "UTMgrid" grid */
	  /* ---------------------------- */
	  GDid = HE5_GDattach(gdfid, "UTMGrid");
	  if (GDid != FAIL)
		{
		  /* Set the data sizes and offsets in external files */
		  /* ------------------------------------------------ */
		  size[0] = 10 * sizeof(int);       offset[0] = 0;
		  size[1] = 20 * sizeof(int);       offset[1] = 40;
		  size[2] = 30 * sizeof(int);       offset[2] = 80;
		  
		  /* Set external data files first */
		  /* ----------------------------- */
		  status = HE5_GDsetextdata(GDid, "external_1g.data,external_2g.data,external_3g.data", offset, size);
		  printf("Status returned by HE5_GDsetextdata(...) :             %d\n",status);
 
		  /* Define field containing external data */
		  /* ------------------------------------- */
		  status = HE5_GDdeffield(GDid, "ExtData", "ExtDim", NULL, H5T_NATIVE_INT, 0);
		  printf("Status returned by HE5_GDdeffield(...\"ExtData\",..) :   %d\n",status);              
		}
	}
  
  /* Detach from the swath */
  /* --------------------- */
  status = HE5_GDdetach(GDid);
  printf("Status returned by HE5_GDdetach(...) :                 %d\n",status);

  /* Close the file */
  /* -------------- */
  status = HE5_GDclose(gdfid);
  printf("Status returned by HE5_GDclose(...) :                  %d\n",status);
  

  return 0;
}



