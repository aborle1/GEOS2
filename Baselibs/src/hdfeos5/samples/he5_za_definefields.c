/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include     <HE5_HdfEosDef.h>

#define  RANK   3
#define  rank   1

/* In this program we (1) open the "ZA.he5" HDF-EOS file, */
/* (2) attach to the "ZA1" za, and (3) define the fields */
/* ----------------------------------------------------- */

int main()
{
  
  herr_t      status = FAIL;

  int         comp_level[ 5 ] = {0,0,0,0,0};
  int         comp_code;
  
  hid_t       zafid = FAIL;
  hid_t       ZAid  = FAIL;

  hsize_t     chunk_dims[ 3 ];
  char        fillvalue[100] = "charname";


  /* Open the file, "ZA.he5", using the H5F_ACC_RDWR access code */
  /* ---------------------------------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDWR);
  if (zafid != FAIL)
    {
	  ZAid = HE5_ZAattach(zafid, "ZA1"); 
	  if (ZAid != FAIL)
		{
		  status = HE5_ZAdefine(ZAid, "Density", "MyTrack1", NULL, H5T_NATIVE_FLOAT);
		  printf("Status returned by HE5_ZAdefine(...\"Density\",...) :     %d\n",status);
		  
		  status = HE5_ZAdefine(ZAid, "Temperature", "MyTrack1,MyTrack2",NULL, H5T_NATIVE_FLOAT);
		  printf("Status returned by HE5_ZAdefine(...\"Temperature\",...) : %d\n",status);

		  status = HE5_ZAdefine(ZAid, "Presure", "Res2tr,Res2xtr", NULL, H5T_NATIVE_DOUBLE);
		  printf("Status returned by HE5_ZAdefine(...\"Presure\",...) :    %d\n",status);

                  status = HE5_ZAsetfillvalue(ZAid, "Test_string", HE5T_CHARSTRING, fillvalue);
                  printf("Status returned by HE5_ZAsetfillvalue(...\"Test_string\",...) :     %d\n",status);
 
                  status = HE5_ZAdefine(ZAid, "Test_string", "MyTrack2", NULL, HE5T_CHARSTRING);
                  printf("Status returned by HE5_ZAdefine(...\"Test_string\",...) :     %d\n",status);


		  /* Define Appendable Field */
		  /* ----------------------- */

		  /*           First, define chunking          */
		  /* (the appendable dataset must be chunked)  */
		  /* ----------------------------------------  */
		  chunk_dims[0] = 15;
		  chunk_dims[1] = 40;
		  chunk_dims[2] = 20;

		  status = HE5_ZAdefchunk(ZAid, RANK, chunk_dims);
		  printf("\tStatus returned by HE5_ZAdefchunk() :                 %d\n",status);

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
		  comp_code = 9;
                  /*comp_code = 0;*/

		  /* Set compression level: value 0,1,2,3,4,5,6,7,8,9,10,16, or 32 */
		  /* ------------------------------------------------------------- */
		  comp_level[0] = 16;
                  /*comp_level[0] = 0;*/
		  
		  status = HE5_ZAdefcomp(ZAid,comp_code, comp_level);
		  printf("\tStatus returned by HE5_ZAdefcomp() :                  %d\n",status);

                  status = HE5_ZAdefine(ZAid, "Spectra", "Bands,Res2tr,Res2xtr", NULL, H5T_NATIVE_FLOAT);
                  printf("Status returned by HE5_ZAdefine(...\"Spectra\",...) :     %d\n",status);

                  /* Define Appendable Field */
                  /* ----------------------- */
 
                  /*           First, define chunking          */
                  /* (the appendable dataset must be chunked)  */
                  /* ----------------------------------------  */
                  chunk_dims[0] = 20;
 
                  status = HE5_ZAdefchunk(ZAid, rank, chunk_dims);
                  printf("\tStatus returned by HE5_ZAdefchunk() :                 %d\n",status);
 
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
                  /*  HDFE_COMP_SHUF_SZIP_K13orEC     16  */
                  /*  HDFE_COMP_SHUF_SZIP_K13orNN     17  */
                  comp_code = 16;
                  /*comp_code = 0;*/
 
                  /* Set compression level: value 0,1,2,3,4,5,6,7,8,9,10,16, or 32 */
                  /* ------------------------------------------------------------- */
                  comp_level[0] = 2;
                  /*comp_level[0] = 0;*/
 
                  status = HE5_ZAdefcomp(ZAid,comp_code, comp_level);
                  printf("\tStatus returned by HE5_ZAdefcomp() :                  %d\n",status);
 
                  status = HE5_ZAdefine(ZAid, "Count", "MyTrack1", "Unlim", H5T_NATIVE_INT);
                  printf("Status returned by HE5_ZAdefine(...\"Count\",...) :       %d\n",status);

		}
    }
  
  status = HE5_ZAdetach(ZAid);
  status = HE5_ZAclose(zafid);
  
  return 0;
}





