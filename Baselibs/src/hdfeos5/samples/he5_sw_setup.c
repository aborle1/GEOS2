/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>


/*   In this program we (1) open an HDF-EOS file, (2) create the swath    */
/*  interface within the file, and (3) define the swath field dimensions  */
/*  --------------------------------------------------------------------  */

int main()
{
  herr_t         status = FAIL; 

  int            i, j;

  hid_t          swfid = FAIL;
  hid_t          SWid  = FAIL;

  long           indx[12] = {0,1,3,6,7,8,11,12,14,24,32,39};

  
  /* Open a new HDF-EOS swath file, "Swath.h5" */
  /* ----------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_TRUNC);
  printf("File ID returned by HE5_SWopen():       %d \n", swfid);

  /* Create the swath, "Swath1", within the file */
  /* ------------------------------------------- */
  SWid = HE5_SWcreate(swfid, "Swath1");
  printf("Swath ID returned by HE5_SWcreate():    %d \n", SWid);

  /* Define dimensions and specify their sizes */
  /* ----------------------------------------- */
  status = HE5_SWdefdim(SWid, "GeoTrack", 20);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "GeoXtrack", 10);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "Res2tr", 40);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "Res2xtr", 20);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "Bands", 15);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "IndxTrack", 12);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  status = HE5_SWdefdim(SWid, "ProfDim", 4);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  /* Define "External" Dimension */
  /* --------------------------- */
  status = HE5_SWdefdim(SWid, "ExtDim", 60);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);
     
  /* Define Unlimited Dimension */
  /* -------------------------- */
  status = HE5_SWdefdim(SWid, "Unlim", H5S_UNLIMITED);
  printf("Status returned by HE5_SWdefdim():      %d \n", status);

  /*
   * Once the dimensions are defined, the relationship (mapping) between the
   * geolocation dimensions, such as track and cross track, and the data
   * dimensions, must be established.  This is done through the HE5_SWdefdimmap
   * routine.  It takes as input the swath id, the names of the dimensions
   * designating the geolocation and data dimensions, respectively, and the
   * offset and increment defining the relation.
   * 
   * In the first example we relate the "GeoTrack" and "Res2tr" dimensions
   * with an offset of 0 and an increment of 2.  Thus the ith element of
   * "Geotrack" corresponds to the 2 * ith element of "Res2tr".
   * 
   * In the second example, the ith element of "GeoXtrack" corresponds to the
   * 2 * ith + 1 element of "Res2xtr".
   *
   * Note that there is no relationship between the geolocation dimensions
   * and the "Bands" dimension.
   */

  /* Define Dimension Mapping */
  /* ------------------------ */
  status = HE5_SWdefdimmap(SWid, "GeoTrack", "Res2tr", 0, 2);
  printf("Status returned by HE5_SWdefdimmap():   %d \n", status);

  status = HE5_SWdefdimmap(SWid, "GeoXtrack", "Res2xtr", 1, 2);
  printf("Status returned by HE5_SWdefdimmap():   %d \n", status);

  /* Define Indexed Mapping */
  /* ---------------------- */
  status = HE5_SWdefidxmap(SWid, "IndxTrack", "Res2tr", indx);
  printf("Status returned by HE5_SWdefidxmap():   %d \n", status);

  /* Close the swath interface */
  /* ------------------------- */
  status = HE5_SWdetach(SWid);
  printf("Status returned by HE5_SWdetach():      %d \n", status);

  /* Close the swath file */
  /* -------------------- */
  status = HE5_SWclose(swfid);
  printf("Status returned by HE5_SWclose():       %d \n", status);


  return 0;

}





