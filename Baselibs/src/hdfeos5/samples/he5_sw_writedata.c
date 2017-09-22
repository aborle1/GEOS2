/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include  <HE5_HdfEosDef.h>


/*  In this program we (1) open the "Swath.h5" file, (2) attach to the       */
/*  "Swath1" swath, and (3) write data to the "Longitude", "Latitude", and   */
/*  "Spectra" fields. Also, set up the global, group, and local attributes   */
/*  -----------------------------------------------------------------------  */


int main()
{
  herr_t          status = FAIL;

  int             i, j, k;
  int             track, xtrack;
  int             attr1[4] = {1, 2, 3, 4};         /* global attribute */
  int             attr2[4] = {10, 20, 30, 40};     /* group attribute  */
  int             attr3[4] = {100, 200, 300, 400}; /* local attribute  */

  hid_t           swfid = FAIL;
  hid_t           SWid  = FAIL;

  char            attr4[20]; /* Global 'char' attribute */
  char            attr8[20]; /* Global 'char' attribute */
  char            attr9[20]; /* Global 'char' attribute */

  long            attr5[4] = {1111111L,2222222L,3333333L,4444444L};/* Global 'long' attribute   */ 

  double          attr6[4] = {1.111111,2.222222,3.333333,4.444444};/* Global 'double' attribute */

  float           attr7[4] = {1.111111,2.222222,3.333333,4.444444};/* Local 'float'   attribute */  

  hssize_t        start[3];
    
  hsize_t         count[3];

  size_t          datasize;

  float           lng[20][10], latcnt;  
  float           lat[20][10], loncnt;

  double          plane[15][40][20], tme[20];

  hvl_t           buffer[4];/* Data buffer for the profile */

  const char *charname[10] = {
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 
        "BBBBBBBBBBBBBBBB", 
        "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC", 
        "DDDDDDDDDDDDDDDDDDDD", 
        "EEEEEEEE", 
        "FFFFFFFFFFFFF", 
        "GGGGGGGGGGGGG", 
        "HHHHHHHHHH",
        "IIIIIII", 
        "JJJJJJJJJJJJJ"
      };


  /* Populate lon/lat data arrays */
  /* ---------------------------- */
  latcnt = 1.;
  loncnt = 1.;
  track  = 0 ;
  xtrack = 0 ;
  while(track < 20) {
	while(xtrack < 10) {
	  lat[track][xtrack] = latcnt;
	  lng[track][xtrack] = loncnt;
	  loncnt = loncnt + 1.;
	  xtrack++;
	}
	latcnt = latcnt + 1.;
	loncnt = 1.;
	track++;
	xtrack = 0;
  }
    
	
  /* Popolate spectra data array. Value = 100*(track index)+(band index) */
  /* ------------------------------------------------------------------- */
  for (i = 0; i < 15; i++)
	{
	  for (j = 0; j < 40; j++)
		for (k = 0; k < 20; k++)
		  plane[i][j][k] = (double)(j*100 + i);
    }
	
	
  /* Allocate memory for and populate data buffer */
  /* -------------------------------------------- */
  datasize = 0;
  for (i = 0; i < 4; i++)
	{
	  buffer[i].p = (hvl_t *)calloc( 25 *(i+1),  sizeof(unsigned int));
	  buffer[i].len  =  25 * (i+1);
	  /* calculate the data buffer size (bytes) */
	  datasize += buffer[i].len * sizeof(unsigned int);
	  for ( j = 0; j < 25 * (i+1); j++)
		((unsigned int *)buffer[i].p)[j] = (i+1)*1000 + j;
	}
  
	
  /* Open the HDF swath file, "Swath.h5" */
  /* ----------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
	{
	  /* Attach the "Swath1" swath */
	  /* ------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  start[0] = 0;
		  start[1] = 0;
		  count[0] = 20;
		  count[1] = 10;
			
		  /*  Write longitute field */
		  /* ---------------------- */
		  status = HE5_SWwritefield(SWid, "Longitude", start, NULL, count, lng);
		  printf("status returned by HE5_SWwritefield(\"Longitude\"):         %d\n", status);
	    
		  /*  Write latitude field */
		  /* --------------------- */
		  status = HE5_SWwritefield(SWid, "Latitude", start, NULL, count, lat);
		  printf("status returned by HE5_SWwritefield(\"Latitude\"):          %d\n", status);
			
		  /* Write Time Field */
		  /* ---------------- */
		  for (i = 0; i < 20; i++) 
			tme[i] = 34574087.3 + 84893.2*i;
		  
		  start[0] = 0;
		  count[0] = 20;
		  status = HE5_SWwritefield(SWid, "Time", start, NULL, count, tme);
		  printf("status returned by HE5_SWwritefield(\"Time\"):              %d\n", status);

            start[0] = 0;
            count[0] = 10;
                  status = HE5_SWwritefield(SWid, "Test_string", start, NULL, count, charname);
                  printf("status returned by HE5_SWwritefield(\"Test_string\"):              %d\n", status);
			
		  /* Write Spectra Field */
		  /* ------------------- */
		  start[0] = 0;      count[0] = 15;
		  start[1] = 0;      count[1] = 40;
		  start[2] = 0;      count[2] = 20;
			
		  status = HE5_SWwritefield(SWid, "Spectra", start, NULL, count, plane);
		  printf("status returned by HE5_SWwritefield(\"Spectra\"):           %d\n", status);

		  /* Write data to the profile */
		  /* ------------------------- */
		  start[0] = 0;      count[0] = 4;
		  status = HE5_PRwrite(SWid, "Profile-2000", start, NULL, count, datasize, buffer);
		  printf("Status returned by HE5_PRwrite(\"Profile-2000\"):           %d \n", status);
            
		  /* Write Global 'int' Attribute */
		  /* ---------------------------- */
		  count[0] = 4;
		  status = HE5_SWwriteattr(SWid, "GlobalAttribute", H5T_NATIVE_INT, count, attr1);
		  printf("status returned by HE5_SWwriteattr(\"GlobalAttribute\"):    %d\n", status);

                  status = HE5_EHwriteglbattr(swfid, "GlobalAttribute_1", H5T_NATIVE_FLOAT, count, attr7);
                  printf("status returned by HE5_EHwriteglbattr(\"GlobalAttribute_1\"):    %d\n", status);

          strcpy(attr8,"AAAAAA");
          count[0] = 6;
                  status = HE5_EHwriteglbattr(swfid, "GlobalAttribute_2222", H5T_NATIVE_CHAR, count, attr8);
                  printf("status returned by HE5_EHwriteglbattr(\"GlobalAttribute_2222\"):    %d\n", status);

		  /* Write Global 'char' Attribute */
		  /* ----------------------------- */
          strcpy(attr4,"ABCDEFGH");
		  count[0] = 8;
		  status = HE5_SWwriteattr(SWid, "GLOBAL_CHAR_ATTR", H5T_NATIVE_CHAR, count, attr4);
		  printf("status returned by HE5_SWwriteattr(\"GLOBAL_CHAR_ATTR\"):   %d\n", status);
			
          strcpy(attr9,"abc");
                  count[0] = 3;
                  status = HE5_SWwriteattr(SWid, "GLOBAL_CHAR_ATTR_11", H5T_NATIVE_CHAR, count, attr9);
                  printf("status returned by HE5_SWwriteattr(\"GLOBAL_CHAR_ATTR_11\"):   %d\n", status);

		  /* Write Global 'long' Attribute */
		  /* ----------------------------- */
		  count[0] = 4;
		  status = HE5_SWwriteattr(SWid, "GLOBAL_LONG_ATTR", H5T_NATIVE_LONG, count, attr5);
		  printf("status returned by HE5_SWwriteattr(\"GLOBAL_LONG_ATTR\"):   %d\n", status);

		  /* Write Global 'double' Attribute */
		  /* ------------------------------- */
		  count[0] = 4;
		  status = HE5_SWwriteattr(SWid, "GLOBAL_DOUBLE_ATTR", H5T_NATIVE_DOUBLE, count, attr6);
		  printf("status returned by HE5_SWwriteattr(\"GLOBAL_DOUBLE_ATTR\"): %d\n", status);

                  status = HE5_EHwriteglbattr(swfid, "GLOBAL_DOUBLE_ATTR_1", H5T_NATIVE_DOUBLE, count, attr6);
                  printf("status returned by HE5_EHwriteglbattr(\"GLOBAL_DOUBLE_ATTR_1\"): %d\n", status);

		  /* Write Group Attribute */
		  /* --------------------- */
		  status = HE5_SWwritegrpattr(SWid, "GroupAttribute", H5T_NATIVE_INT, count, attr2);
		  printf("status returned by HE5_SWwritegrpattr(\"GroupAttribute\"):  %d\n", status);

		  /* Write Local Attribute */
		  /* --------------------- */
          strcpy(attr4,"ababababababababab");
                  count[0] = 18;
                  status = HE5_SWwritelocattr(SWid, "Time", "LOCAL_CHAR_ATTR", H5T_NATIVE_CHAR, count, attr4);
                  printf("status returned by HE5_SWwritelocattr(\"LOCAL_CHAR_ATTR\"):   %d\n", status);
 
                  count[0] = 4;

		  status = HE5_SWwritelocattr(SWid, "Density", "LocalAttribute_1", H5T_NATIVE_INT, count, attr3);
		  printf("status returned by HE5_SWwritelocattr(\"LocalAttribute_1\"):%d\n", status);

		  /* Write Local Attribute */
		  /* --------------------- */
		  status = HE5_SWwritelocattr(SWid, "Longitude", "LocalAttribute_2", H5T_NATIVE_FLOAT, count, attr7);
		  printf("status returned by HE5_SWwritelocattr(\"LocalAttribute_2\"):%d\n", status);
		}
	}
	
  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);

  return 0;
}









