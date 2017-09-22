/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>


/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "Grid1" grid, and (3) define the grid appendable field.
 */


int main()
{
  herr_t          status = FAIL;

  int             tilerank, compcode = 4;    
  int             compparm[5] = {0, 0, 0, 0, 0};

  hid_t           gdfid = FAIL;
  hid_t           GDid1 = FAIL;
  hid_t           GDid2 = FAIL;

  hsize_t         tiledims[8];
    
  float           fillval1 = -7., fillval2 = -9999.;
 

  /*
   * We first open the HDF-EOS grid file, "Grid.h5".  Because this file
   * already exist and we wish to write to it, we use the H5F_ACC_RDWR access
   * code in the open statement.  The HE5_GDopen routine returns the grid file
   * id, gdfid, which is used to identify the file in subsequent routines.
   */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDWR);

  /*
   * If the grid file cannot be found, HE5_GDopen will return -1 for the file
   * handle (gdfid).  We there check that this is not the case before
   * proceeding with the other routines.
   * 
   * The HE5_GDattach routine returns the handle to the existing grid "Grid1",
   * GDid.  If the grid is not found, HE5_GDattach returns -1 for the handle.
   */
  if (gdfid != FAIL)
    {
	  GDid1 = HE5_GDattach(gdfid, "UTMGrid");
	  printf("Grid ID returned by HE5_GDattach(,\"UTMGrid\"):      %d\n", GDid1);
	
	  /* Define appendable field "Vegetation"  */
	  tilerank    = 2;
	  tiledims[0] = 100;
	  tiledims[1] = 60;
	  compparm[0] = 6;
        
	  status = HE5_GDdeftile(GDid1,HE5_HDFE_TILE,tilerank,tiledims);
	  printf("Status returned by HE5_GDdeftile():                %d\n", status);

	  status = HE5_GDdefcomp(GDid1,compcode, compparm);
	  printf("Status returned by HE5_GDdefcomp():                %d\n", status);
	
	  status = HE5_GDdeffield(GDid1, "Vegetation", "YDim,XDim","Unlim,Unlim",H5T_NATIVE_FLOAT, 0);
	  printf("Status returned by HE5_GDdeffield(\"Vegetation\"):   %d\n", status);

	  GDid2 = HE5_GDattach(gdfid, "PolarGrid");
	  printf("Grid ID returned by HE5_GDattach(\"PolarGrid\"):     %d\n", GDid2);

	  status = HE5_GDdeffield(GDid2, "Temperature", "YDim,XDim", NULL, H5T_NATIVE_FLOAT, 0);
	  printf("Status returned by HE5_GDdeffield(\"Temperature\"):  %d\n", status);

	  status = HE5_GDdeffield(GDid2, "Pressure", "YDim,XDim", NULL, H5T_NATIVE_FLOAT, 0);
	  printf("Status returned by HE5_GDdeffield(\"Pressure\"):     %d\n", status);

	  status = HE5_GDsetfillvalue(GDid2, "Pressure", H5T_NATIVE_FLOAT, &fillval2);
	  printf("Status returned by HE5_GDsetfillvalue(\"Pressure\'): %d\n", status);

	  status = HE5_GDdetach(GDid1);
	  status = HE5_GDdetach(GDid2);
	
    }

  status = HE5_GDclose(gdfid);
  
  return 0;
}







































