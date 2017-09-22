/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include     <HE5_HdfEosDef.h>


/*  In this program we  (1) open the "Swath.h5" HDF-EOS file,   */
/*  (2) attach to the "Swath1" swath, and (3) define the swath  */
/*  fields, including appendable field "Spectra".               */
/* ------------------------------------------------------------ */


#define RANK 3

int main()
{

  herr_t      status = FAIL;

  hid_t       swfid  = FAIL;
  hid_t       SWid   = FAIL;

  int         comp_level[ 5 ] = { 0, 0, 0, 0, 0 };
  int         comp_code;

  hsize_t     chunk_dims[ 3 ];


  /* Open the file, "Swath.h5", using the H5F_ACC_RDWR access code */
  /* ------------------------------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
    {
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{	  
		  /*
		   * We define seven fields.  The first three, "Time", "Longitude"
		   * and "Latitude" are geolocation fields and thus we use the
		   * geolocation dimensions "GeoTrack" and "GeoXtrack" in the field
		   * definitions.
		   * 
		   * The next four fields are data fields.  Note that either
		   * geolocation or data dimensions can be used.
		   */
		  status = HE5_SWdefgeofield(SWid, "Time", "GeoTrack", NULL, H5T_NATIVE_DOUBLE, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Time\",...) :         %d\n",status);
	    
		  status = HE5_SWdefgeofield(SWid, "Longitude", "GeoTrack,GeoXtrack", NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Longitude\",...) :    %d\n",status);

		  status = HE5_SWdefgeofield(SWid, "Latitude", "GeoTrack,GeoXtrack", NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Latitude\",...) :     %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Density", "GeoTrack", NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Density\",...) :     %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Temperature", "GeoTrack,GeoXtrack",NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Temperature\",...) : %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Pressure", "Res2tr,Res2xtr", NULL, H5T_NATIVE_DOUBLE, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Pressure\",...) :    %d\n",status);

		  /* Define Appendable Field  */
		  /* ------------------------ */

		  /* ----------------------------------------  */
		  /*           First, define chunking          */
		  /* (the appendable dataset must be chunked)  */
		  /* ----------------------------------------  */
		  chunk_dims[0] = 15;
		  chunk_dims[1] = 40;
		  chunk_dims[2] = 20;
	    
             
		  status = HE5_SWdefchunk(SWid, RANK, chunk_dims);
		  printf("\tStatus returned by HE5_SWdefchunk() :                 %d\n",status);

		  comp_code     = 4;
                  /*comp_code     = 0;*/
		  comp_level[0] = 6;
                  /*comp_level[0] = 0;*/

		  status = HE5_SWdefcomp(SWid,comp_code, comp_level);
		  printf("\tStatus returned by HE5_SWdefcomp() :                  %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Spectra", "Bands,Res2tr,Res2xtr", "Unlim,Unlim,Unlim", H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Spectra\",...) :     %d\n",status);

                                    /* Define Profile field */
                  /* -------------------- */
                  status = HE5_PRdefine(SWid, "Profile-2000", "ProfDim", NULL, H5T_NATIVE_UINT);
                  printf("Status returned by HE5_PRdefine(...\"Profile-2000\",...) :      %d\n",status);

		}
    }

  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);
  
  return 0;
}





