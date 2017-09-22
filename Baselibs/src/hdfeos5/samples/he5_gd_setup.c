/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include     <HE5_HdfEosDef.h>

/*
 * In this example we will open an HDF-EOS file and create UTM and Polar
 * Stereographic grid structures within the file.
 */


int main()
{

  herr_t    status = FAIL;

  hid_t     gdfid  = FAIL;
  hid_t     GDid   = FAIL;
  hid_t     GDid2  = FAIL;
  hid_t     GDid3  = FAIL;

  int       i, j;
  int       zonecode, projcode, spherecode, dummy = 0;

  long      xdim, ydim;

  double    projparm[16], uplft[2], lowrgt[2];
    
  /*
   * We first open the HDF grid file, "Grid.h5".  Because this file
   * does not already exist, we use the H5F_ACC_TRUNC access code in the
   * open statement.  The GDopen routine returns the grid file id, gdfid,
   * which is used to identify the file in subsequent routines in the
   * library.
   */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_TRUNC);

  /*
   * Create UTM Grid
   * 
   * Region is bounded by 54 E and 60 E longitude and 20 N and 30 N latitude.
   * UTM Zone 40
   * 
   * Use default spheriod (Clarke 1866 - spherecode = 0)
   * 
   * Grid into 120 bins along x-axis and 200 bins along y-axis
   *                   (approx 3' by 3' bins)
   */

  zonecode   = 40;
  spherecode = 0;

  /* Upper Left and Lower Right points in meters */
  /* ------------------------------------------- */
  uplft[0]   =   210584.50041;
  uplft[1]   =  3322395.95445;
  lowrgt[0]  =  813931.10959;
  lowrgt[1]  = 2214162.53278;

  xdim = 120;
  ydim = 200;
    
  GDid   = HE5_GDcreate(gdfid, "UTMGrid", xdim, ydim, uplft, lowrgt);
  printf("Grid ID returned by HE5_GDcreate :                    %d \n", GDid);

  status = HE5_GDdefproj(GDid, HE5_GCTP_UTM, zonecode, spherecode, projparm);
  printf("status returned by HE5_GDdefproj(...\"HE5_GCTP_UTM\"...) :  %d \n", status);

  /* Define "Time" Dimension */
  status = HE5_GDdefdim(GDid, "Time", 10);
  printf("status returned by HE5_GDdefdim(...\"Time\"...) :       %d \n", status);

  /* Define "External" Dimension */
  /* --------------------------- */
  status = HE5_GDdefdim(GDid, "ExtDim", 60);
  printf("status returned by HE5_GDdefdim(...\"ExtDim\"...):      %d \n", status);

  /* Define "Unlim" Dimension */
  status = HE5_GDdefdim(GDid, "Unlim", H5S_UNLIMITED);
  printf("status returned by HE5_GDdefdim(...\"Unlim\"...) :      %d \n", status);

  /*
   * Create polar stereographic grid
   * 
   * Northern Hemisphere  (True Scale at 90 N, 0 Longitude below pole)
   * 
   * Use International 1967 spheriod (spherecode = 3)
   * 
   * Grid into 100 bins along x-axis and y-axis
   */

  spherecode = 3;

  /* Define GCTP Projection Parameters */
  /* --------------------------------- */
  for (i = 0; i < 16; i++)
	projparm[i] = 0;

  /* Set Longitude below pole & true scale in DDDMMMSSS.SSS format) */
  projparm[4] = 0.0;
  projparm[5] = 90000000.00;
    

  xdim = 100;
  ydim = 100;

  GDid2  = HE5_GDcreate(gdfid, "PolarGrid", xdim, ydim, NULL, NULL);
  printf("Grid ID returned by HE5_GDcreate() :                  %d \n", GDid2);

  status = HE5_GDdefproj(GDid2, HE5_GCTP_PS, dummy, spherecode, projparm);
  printf("status returned by HE5_GDdefproj(...\"HE5_GCTP_PS\"...) :   %d \n", status);

  status = HE5_GDdeforigin(GDid2, HE5_HDFE_GD_LR);
  printf("status returned by HE5_GDdeforigin() :                %d \n", status);

    
  /* Define "Bands" Dimension */
  status = HE5_GDdefdim(GDid2, "Bands", 3);
  printf("status returned by HE5_GDdefdim(...\"Bands\"...) :      %d \n", status);

  /*
   * Create geographic (linear scale) grid
   * 
   * 0 - 15 degrees longitude,  20 - 30 degrees latitude
   *
   */
    
  xdim = 60;
  ydim = 40;

  uplft[0]  = HE5_EHconvAng(0.,  HE5_HDFE_DEG_DMS);
  uplft[1]  = HE5_EHconvAng(30., HE5_HDFE_DEG_DMS);
  lowrgt[0] = HE5_EHconvAng(15., HE5_HDFE_DEG_DMS);
  lowrgt[1] = HE5_EHconvAng(20., HE5_HDFE_DEG_DMS);
    
  GDid3  = HE5_GDcreate(gdfid, "GEOGrid", xdim, ydim, uplft, lowrgt);
  printf("Grid ID returned by HE5_GDcreate() :                  %d \n", GDid3);

  status = HE5_GDdefproj(GDid3, HE5_GCTP_GEO, dummy, dummy, NULL);
  printf("status returned by HE5_GDdefproj(...\"HE5_GCTP_GEO\"...) :  %d \n", status);

  /*
   * We now close the grid interface with the GDdetach routine.  This step
   * is necessary to properly store the grid information within the file
   * AND SHOULD BE DONE BEFORE WRITING OR READING DATA TO OR FROM THE FIELD.
   */    
  status = HE5_GDdetach(GDid);
  status = HE5_GDdetach(GDid2);
  status = HE5_GDdetach(GDid3);

  /*
   * Finally, we close the grid file using the HE5_GDclose routine.  This will
   * release the grid file handles established by HE5_GDopen.
   */
  status = HE5_GDclose(gdfid);

  return 0;
}








