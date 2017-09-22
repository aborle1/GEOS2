/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include    <HE5_HdfEosDef.h>


/*  In this program we (1) open an HDF-EOS file, (2) create the ZA    */
/*  interface within the file, and (3) define the za field dimensions */
/*  ----------------------------------------------------------------- */

int main()
{
  herr_t         status = FAIL; 

  hid_t          zafid = FAIL;
  hid_t          ZAid  = FAIL;


  
  /* Open a new HDF-EOS za file, "ZA.he5" */
  /* ----------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_TRUNC);
  printf("File ID returned by HE5_ZAopen():       %d \n", zafid);
  
  /* Create the ZA, "ZA1", within the file */
  /* ------------------------------------- */
  ZAid = HE5_ZAcreate(zafid, "ZA1");
  printf("ZA ID returned by HE5_ZAcreate():    %d \n", ZAid);

  /* Define dimensions and specify their sizes */
  /* ----------------------------------------- */
  status = HE5_ZAdefdim(ZAid, "MyTrack1", 20);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  status = HE5_ZAdefdim(ZAid, "MyTrack2", 10);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  status = HE5_ZAdefdim(ZAid, "Res2tr", 40);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  status = HE5_ZAdefdim(ZAid, "Res2xtr", 20);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  status = HE5_ZAdefdim(ZAid, "Bands", 15);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  status = HE5_ZAdefdim(ZAid, "IndxTrack", 12);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  /* Define "External" Dimension */
  /* --------------------------- */
  status = HE5_ZAdefdim(ZAid, "ExtDim", 60);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);

  /* Define Unlimited Dimension */
  /* -------------------------- */
  status = HE5_ZAdefdim(ZAid, "Unlim", H5S_UNLIMITED);
  printf("Status returned by HE5_ZAdefdim():      %d \n", status);


  /* Close the za interface */
  /* ---------------------- */
  status = HE5_ZAdetach(ZAid);
  printf("Status returned by HE5_ZAdetach():      %d \n", status);

  /* Close the za file */
  /* ----------------- */
  status = HE5_ZAclose(zafid);
  printf("Status returned by HE5_ZAclose():       %d \n", status);


  return 0;

}





