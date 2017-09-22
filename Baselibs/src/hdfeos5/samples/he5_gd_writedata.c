/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>

/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "UTMGrid", and (3) write data to the "Vegetation" field.  We will
 * then attach to the "PolarGrid" and write to the "Temperature" field.
 */


int main()
{
  herr_t          status = FAIL;

  int             i, j;
  int             grpattr[3] = {3,7,11};       /* group attr  */

  hid_t           gdfid = FAIL;
  hid_t           GDid  = FAIL;

  float           flt = 3.1415;	               /* global attr */
  float           attr[4] = {1.1,2.2,3.3,4.4}; /* local attr  */
  float           veg[200][120];
  float           temp[100][100];
  
  hssize_t        start[3];
  
  hsize_t         edge[3];
  
  
  /* Fill veg array */
  for (i = 0; i < 200; i++)
	for (j = 0; j < 120; j++)
	  veg[i][j] = (float)(10+i);
	
  /* Fill temp array */
  for (i = 0; i < 100; i++)
	for (j = 0; j < 100; j++)
	  temp[i][j] = (float)(100*i+j);
  
  
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
          /* Data Field "Vegetation" */
          /* ----------------------  */
		  start[0] = 0;     start[1] = 0;
		  edge[0]  = 200;   edge[1]  = 120;
		  status = HE5_GDwritefield(GDid, "Vegetation", start, NULL, edge, veg);
		  printf("Status returned by HE5_GDwritefield() :    %d \n", status);
		  
		  /* Global attribute */
		  /* ---------------- */
		  edge[0] = 1;
		  status = HE5_GDwriteattr(GDid, "GlobalAttribute", H5T_NATIVE_FLOAT, edge, &flt);
		  printf("Status returned by HE5_GDwriteattr() :     %d \n", status);
		  
		  /* Group attribute */
		  /* --------------- */
		  edge[0] = 3;
		  status = HE5_GDwritegrpattr(GDid, "GroupAttribute", H5T_NATIVE_INT, edge, grpattr);
		  printf("Status returned by HE5_GDwritegrpattr() :  %d \n", status);

		  /* Local attribute */
		  /* --------------- */
		  edge[0] = 4;
		  status = HE5_GDwritelocattr(GDid, "Vegetation", "LocalAttribute", H5T_NATIVE_FLOAT, edge, attr);
		  printf("Status returned by HE5_GDwritelocattr() :  %d \n", status);         
		}
	  
	  status = HE5_GDdetach(GDid);
	  
	  GDid = HE5_GDattach(gdfid, "PolarGrid");
	  if (GDid != FAIL)
		{
          /* Data field "Temperature" */
          /* -----------------------  */
		  start[0] = 0;    start[1] = 0;
		  edge[0]  = 100;  edge[1]  = 100;
		  status = HE5_GDwritefield(GDid, "Temperature", start, NULL, edge, temp);
		  printf("Status returned by HE5_GDwritefield() :    %d \n", status);
		  
		}
	  status = HE5_GDdetach(GDid);
    }
  
  status = HE5_GDclose(gdfid);
  
  return 0;
}





