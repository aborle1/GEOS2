/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include  <HE5_HdfEosDef.h>


/*  In this program we (1) open the "ZA.he5" file, (2) attach to the    */
/*  "ZA1" za, and (3) write data to the "Spectra" fields. Also, set up */
/*  the global, group, and local attributes                            */
/*  ------------------------------------------------------------------ */


int main()
{
  herr_t          status = FAIL;

  int             i, j, k;
  int             attr1[4] = {1, 2, 3, 4};         /* global attribute */
  int             attr2[4] = {10, 20, 30, 40};     /* group attribute  */
  int             attr3[4] = {100, 200, 300, 400}; /* local attribute  */

  hid_t           zafid = FAIL;
  hid_t           ZAid  = FAIL;

  char            attr4[7]; /* Global 'char' attribute */
 
  long            attr5[4] = {1111111L,2222222L,3333333L,4444444L};/* Global 'long' attribute   */ 

  double          attr6[4] = {1.111111,2.222222,3.333333,4.444444};/* Global 'double' attribute */

  hssize_t        start[3];
    
  hsize_t         count[3];

  double          plane[15][40][20];

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


  /* Populate spectra data array. Value = 100*(track index)+(band index) */
  /* ------------------------------------------------------------------- */
  for (i = 0; i < 15; i++)
  {
	  for (j = 0; j < 40; j++)
		for (k = 0; k < 20; k++)
		  plane[i][j][k] = (double)(j*100 + i);
  }
	
	
  /* Open the HDF za file, "ZA.he5" */
  /* ----------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDWR);
  if (zafid != FAIL)
	{
	  /* Attach the "ZA1" za */
	  /* ------------------- */
	  ZAid = HE5_ZAattach(zafid, "ZA1");
	  if (ZAid != FAIL)
		{
		  /* Write Spectra Field */
		  /* ------------------- */
		  start[0] = 0;      count[0] = 15;
		  start[1] = 0;      count[1] = 40;
		  start[2] = 0;      count[2] = 20;
			
		  status = HE5_ZAwrite(ZAid, "Spectra", start, NULL, count, plane);
		  printf("status returned by HE5_ZAwrite(\"Spectra\"):           %d\n", status);

            start[0] = 0;
            count[0] = 10;
                  status = HE5_ZAwrite(ZAid, "Test_string", start, NULL, count, charname);
                  printf("status returned by HE5_ZAwrite(\"Test_string\"):              %d\n", status);
            
		  /* Write Global 'int' Attribute */
		  /* ---------------------------- */
		  count[0] = 4;
		  status = HE5_ZAwriteattr(ZAid, "GlobalAttribute", H5T_NATIVE_INT, count, attr1);
		  printf("status returned by HE5_ZAwriteattr(\"GlobalAttribute\"):    %d\n", status);

		  /* Write Global 'char' Attribute */
		  /* ----------------------------- */
                  strcpy(attr4,"ABCDEF");
		  count[0] = 6;
		  status = HE5_ZAwriteattr(ZAid, "GLOBAL_CHAR_ATTR", H5T_NATIVE_CHAR, count, attr4);
		  printf("status returned by HE5_ZAwriteattr(\"GLOBAL_CHAR_ATTR\"):   %d\n", status);
			
		  /* Write Global 'long' Attribute */
		  /* ----------------------------- */
		  count[0] = 4;
		  status = HE5_ZAwriteattr(ZAid, "GLOBAL_LONG_ATTR", H5T_NATIVE_LONG, count, attr5);
		  printf("status returned by HE5_ZAwriteattr(\"GLOBAL_LONG_ATTR\"):   %d\n", status);

		  /* Write Global 'double' Attribute */
		  /* ------------------------------- */
		  count[0] = 4;
		  status = HE5_ZAwriteattr(ZAid, "GLOBAL_DOUBLE_ATTR", H5T_NATIVE_DOUBLE, count, attr6);
		  printf("status returned by HE5_ZAwriteattr(\"GLOBAL_DOUBLE_ATTR\"): %d\n", status);

		  /* Write Group Attribute */
		  /* --------------------- */
		  status = HE5_ZAwritegrpattr(ZAid, "GroupAttribute", H5T_NATIVE_INT, count, attr2);
		  printf("status returned by HE5_ZAwritegrpattr(\"GroupAttribute\"):  %d\n", status);

		  /* Write Local Attribute */
		  /* --------------------- */
		  status = HE5_ZAwritelocattr(ZAid, "Density", "LocalAttribute", H5T_NATIVE_INT, count, attr3);
		  printf("status returned by HE5_ZAwritelocattr(\"LocalAttribute\"):%d\n", status);

		}
	}
	
  status = HE5_ZAdetach(ZAid);
  status = HE5_ZAclose(zafid);

  return 0;
}









