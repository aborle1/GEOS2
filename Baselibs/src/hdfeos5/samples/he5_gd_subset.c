/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include    <HE5_HdfEosDef.h>


/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "PolarGrid", and (3) subset data from the "Temperature" field.
 */


int main()
{
  herr_t          status   = FAIL;

  int             rank     = FAIL;
  int             i;

  hid_t           gdfid    = FAIL;
  hid_t           GDid     = FAIL;
  hid_t           regionID = FAIL;

  hid_t           *ntype;
    
  long            size;

  hsize_t         dims[8];

  float           *datbuf;
    
  double          cornerlon[2], cornerlat[2];
  double          upleft[2], lowright[2];
    

  /*
   * Open the HDF-EOS grid file, "Grid.h5".
   */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDWR);
  if (gdfid != FAIL)
    {
	  GDid = HE5_GDattach(gdfid, "PolarGrid");
	  if (GDid != FAIL)
		{
		  cornerlon[0] = 57.;  cornerlat[0] = 23.;
		  cornerlon[1] = 59.;  cornerlat[1] = 35.;
		  cornerlon[0] = 0.;   cornerlat[0] = 90.;
		  cornerlon[1] = 90.;  cornerlat[1] = 0.;

		  regionID = HE5_GDdefboxregion(GDid, cornerlon, cornerlat);
		  printf("Region ID returned by HE5_GDdefboxregion() :    %d \n", regionID);

		  ntype = (hid_t *)calloc(1, sizeof(hid_t));    
		  status = HE5_GDregioninfo(GDid, regionID, "Temperature", ntype,&rank, dims, &size, upleft, lowright);
		  printf("Status returned by HE5_GDregioninfo() :         %d \n", status);
		  printf("Byte size of region data buffer:            %d\n",(int)size);
      
		  datbuf = (float *)malloc(size);    
		  status = HE5_GDextractregion(GDid, regionID, "Temperature", datbuf);
		  printf("Status returned by HE5_GDextractregion() :      %d \n", status);
		  printf("First 20 values of data buffer: \n");
		  for (i = 0; i < 20; i++)
			printf("\t\t %f \n", datbuf[ i ]);
		  
		  free(datbuf);
		  free(ntype);   
		}
    }
  status = HE5_GDdetach(GDid);
  printf("Status returned by HE5_GDdetach() :             %d \n", status);
  
  status = HE5_GDclose(gdfid);
  printf("Status returned by HE5_GDclose() :              %d \n", status);

  return 0;
}



