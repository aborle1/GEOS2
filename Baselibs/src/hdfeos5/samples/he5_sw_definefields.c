/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include     <HE5_HdfEosDef.h>

#define  RANK   3
#define  rank   1

/*   In this program we (1) open the "Swath.h5" HDF-EOS file,  */
/* (2) attach to the "Swath1" swath, and (3) define the fields */
/* ----------------------------------------------------------- */

int main()
{
  
  herr_t      status = FAIL;

  int         comp_level[ 5 ] = {0,0,0,0,0};
  int         comp_code;
  int         i, j;  
  
  hid_t       swfid = FAIL;
  hid_t       SWid  = FAIL;

  hsize_t     chunk_dims[ 3 ];
  char        fillvalue[100] = "charname";

  double      fillvalue1 = -1.11111;
  int         fillvalue2 = 6;

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

                  status = HE5_SWsetfillvalue(SWid,"Time",H5T_NATIVE_DOUBLE, &fillvalue1);
                  printf("Status returned by HE5_SWsetfillvalue(...\"Time\",...) :         %d\n",status);

		  status = HE5_SWdefgeofield(SWid, "Time", "GeoTrack", NULL, H5T_NATIVE_DOUBLE, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Time\",...) :         %d\n",status);
	    
		  status = HE5_SWdefgeofield(SWid, "Longitude", "GeoTrack,GeoXtrack", NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Longitude\",...) :    %d\n",status);

		  status = HE5_SWdefgeofield(SWid, "Latitude", "GeoTrack,GeoXtrack", NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefgeofield(...\"Latitude\",...) :     %d\n",status);

                  status = HE5_SWsetfillvalue(SWid, "Test_string", HE5T_CHARSTRING, fillvalue);
                  printf("Status returned by HE5_SWsetfillvalue(...\"Test_string\",...) :     %d\n",status);

                  status = HE5_SWdefdatafield(SWid, "Test_string", "GeoXtrack", NULL, HE5T_CHARSTRING, 0);
                  printf("Status returned by HE5_SWdefdatafield(...\"Test_string\",...) :     %d\n",status);

                  status = HE5_SWsetfillvalue(SWid,"Density",H5T_NATIVE_INT8, &fillvalue2);
                  printf("Status returned by HE5_SWsetfillvalue(...\"Density\",...) :         %d\n",status);

                  status = HE5_SWdefdatafield(SWid, "Density", "GeoTrack", NULL, H5T_NATIVE_INT8, 0);
                  printf("Status returned by HE5_SWdefdatafield(...\"Density\",...) :     %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Temperature", "GeoTrack,GeoXtrack",NULL, H5T_NATIVE_FLOAT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Temperature\",...) : %d\n",status);

                  comp_level[0] =0;

                  chunk_dims[0] = 40;

                  status = HE5_SWdefchunk(SWid, rank, chunk_dims);
                  printf("\tStatus returned by HE5_SWdefchunk() :                 %d\n",status);

                  status = HE5_SWdefcomp(SWid,HE5_HDFE_COMP_NONE,comp_level);
                  status = HE5_SWdefdatafield(SWid, "Pressure", "Res2tr", NULL, H5T_NATIVE_FLOAT, 0);
                  printf("Status returned by HE5_SWdefdatafield(...\"Pressure\",...):    %d\n",status);

		  /* Define Appendable Field */
		  /* ----------------------- */

		  /*           First, define chunking          */
		  /* (the appendable dataset must be chunked)  */
		  /* ----------------------------------------  */
		  chunk_dims[0] = 15;
		  chunk_dims[1] = 40;
		  chunk_dims[2] = 20;

		  status = HE5_SWdefchunk(SWid, RANK, chunk_dims);
		  printf("\tStatus returned by HE5_SWdefchunk() :                 %d\n",status);

		  /* Second, define compression scheme */
		  /* --------------------------------- */ 
		  
		  /* set the value of compression code: */
		  /*  HDFE_COMP_NONE                  0 */
		  /*  HDFE_COMP_RLE                   1 */   
		  /*  HDFE_COMP_NBIT                  2 */   
		  /*  HDFE_COMP_SKPHUFF               3 */   
		  /*  HDFE_COMP_DEFLATE               4 */           
                  /*  HDFE_COMP_SZIP_CHIP             5 */
                  /*  HDFE_COMP_SZIP_K13              6 */
                  /*  HDFE_COMP_SZIP_EC               7 */
                  /*  HDFE_COMP_SZIP_NN               8 */
                  /*  HDFE_COMP_SZIP_K13orEC          9 */
                  /*  HDFE_COMP_SZIP_K13orNN          10 */
                  /*  HDFE_COMP_SHUF_DEFLATE          11 */
                  /*  HDFE_COMP_SHUF_SZIP_CHIP        12 */
                  /*  HDFE_COMP_SHUF_SZIP_K13         13 */
                  /*  HDFE_COMP_SHUF_SZIP_EC          14 */
                  /*  HDFE_COMP_SHUF_SZIP_NN          15 */
                  /*  HDFE_COMP_SHUF_SZIP_K13orEC     16 */
                  /*  HDFE_COMP_SHUF_SZIP_K13orNN     17 */

		  comp_code = 16;
                  /*comp_code = 0;*/

		  /* Set compression level: value 0,1,2,3,4,5,6,7,8,9,10,16, or 32 */
		  /* ------------------------------------------------------------- */
		  comp_level[0] = 8;
                  /*comp_level[0] = 0;*/
		  
		  status = HE5_SWdefcomp(SWid,comp_code, comp_level);
		  printf("\tStatus returned by HE5_SWdefcomp() :                  %d\n",status);

                  status = HE5_SWdefdatafield(SWid, "Spectra", "Bands,Res2tr,Res2xtr", NULL, H5T_NATIVE_FLOAT, 0);
                  printf("Status returned by HE5_SWdefdatafield(...\"Spectra\",...) :     %d\n",status);
	    

                  /* Define Appendable Field */
                  /* ----------------------- */
 
                  /*           First, define chunking          */
                  /* (the appendable dataset must be chunked)  */
                  /* ----------------------------------------  */
                  chunk_dims[0] = 20;
 
                  status = HE5_SWdefchunk(SWid, rank, chunk_dims);
                  printf("\tStatus returned by HE5_SWdefchunk() :                 %d\n",status);
 
                  /* Second, define compression scheme */
                  /* --------------------------------- */
 
                  /* set the value of compression code: */
                  /*  HDFE_COMP_NONE                  0 */
                  /*  HDFE_COMP_RLE                   1 */
                  /*  HDFE_COMP_NBIT                  2 */
                  /*  HDFE_COMP_SKPHUFF               3 */
                  /*  HDFE_COMP_DEFLATE               4 */
                  /*  HDFE_COMP_SZIP_CHIP             5 */
                  /*  HDFE_COMP_SZIP_K13              6 */
                  /*  HDFE_COMP_SZIP_EC               7 */
                  /*  HDFE_COMP_SZIP_NN               8 */
                  /*  HDFE_COMP_SZIP_K13orEC          9 */
                  /*  HDFE_COMP_SZIP_K13orNN          10 */
                  /*  HDFE_COMP_SHUF_DEFLATE          11 */
                  /*  HDFE_COMP_SHUF_SZIP_CHIP        12 */
                  /*  HDFE_COMP_SHUF_SZIP_K13         13 */
                  /*  HDFE_COMP_SHUF_SZIP_EC          14 */
                  /*  HDFE_COMP_SHUF_SZIP_NN          15 */
                  /*  HDFE_COMP_SHUF_SZIP_K13orEC     16 */
                  /*  HDFE_COMP_SHUF_SZIP_K13orNN     17 */
                  comp_code = 10;
                  /*comp_code = 0;*/
 
                  /* Set compression level: value 0,1,2,3,4,5,6,7,8,9,10,16, or 32 */
                  /* ------------------------------------------------------------- */
                  comp_level[0] = 8;
                  /*comp_level[0] = 0;*/
 
                  status = HE5_SWdefcomp(SWid,comp_code, comp_level);
                  printf("\tStatus returned by HE5_SWdefcomp() :                  %d\n",status);

		  status = HE5_SWdefdatafield(SWid, "Count", "GeoTrack", "Unlim", H5T_NATIVE_INT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"Count\",...) :       %d\n",status);


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





