/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */




#include    <HE5_HdfEosDef.h>

/* ------------------------------------------------------------------- */
/* In this program we (1) open the "ZA.he5" HDF-EOS file, (2) attach to */
/* the "ZA1" za, and (3) read data from the "Spectra" field. Also, we  */ 
/* read the global/group/local attributes                              */
/* ------------------------------------------------------------------- */


int main()
{
  herr_t          status = FAIL;

  int             i, j, k;
  int             attr1[4];		/* data buffer for global attribute     */
  int             attr2[4];		/* ....  for group attribute            */
  int             attr3[4];		/* ....  for local attribute            */

  hid_t           zafid = FAIL;
  hid_t           ZAid  = FAIL;

  char            attr4[7] = {0};     /* ...  for global 'char' attribute     */

  long            attr5[4];     /* ...  for global 'long' attribute     */

  double          attr6[4];     /* ... for global 'double' attribute    */

  hssize_t        start[3];
  hsize_t         count[3];

  double          plane[15][40][20];
  char            *charname[10] = {0};


  /* Populate spectra data array. Value = 100*(track index)+(band index) */
  /* ------------------------------------------------------------------- */
  for (i = 0; i < 15; i++)
        {
          for (j = 0; j < 40; j++)
                for (k = 0; k < 20; k++)
                  plane[i][j][k] = (double)(j*100 + i);
    }


  /* Open the HDF-EOS za file, "ZA.he5" */
  /* --------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDONLY);
  if (zafid != FAIL)
	{
	  /* Attach the "ZA1" za */
	  /* ------------------- */
	  ZAid = HE5_ZAattach(zafid, "ZA1");
	  if (ZAid != FAIL)
		{
		  /* Read the entire Spectra field */
		  /* ----------------------------- */
		  start[0] = 0;    start[1] = 0;   start[2] = 0;
		  count[0] = 15;   count[1] = 40;  count[2] = 20;
		  status = HE5_ZAread(ZAid, "Spectra", start, NULL, count, plane);
		  printf("Status returned by HE5_ZAread() :    %d \n", status);

                  start[0] = 0;
                  count[0] = 10;
                  for (i = 0; i < 10; i++)
                      charname[i] = (char *)malloc(95*sizeof(char));
                  status = HE5_ZAread(ZAid, "Test_string", start, NULL, count, charname);
                  printf("Status returned by HE5_ZAread() :    %d \n", status);
 
                  /* Display Test_string data */
                  /* ---------------------- */
                  for (i = 0; i < 10; i++)
                          printf("i = %d        string =  %s\n", i, charname[i]);

                  for (i = 0; i < 10; i++)
                       free(charname[i]);

		  /* Read Global 'int' Attribute */
		  /* --------------------------- */
		  status = HE5_ZAreadattr(ZAid, "GlobalAttribute", attr1);
		  printf("Status returned by HE5_ZAreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr1[i]);
			
		  /* Read Group Attribute */
		  /* -------------------- */
		  status = HE5_ZAreadgrpattr(ZAid, "GroupAttribute", attr2);
		  printf("Status returned by HE5_ZAreadgrpattr() :  %d \n", status);
		  printf("Group attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr2[i]);
		  
		  /* Read Local Attribute */
		  /* -------------------- */
		  status = HE5_ZAreadlocattr(ZAid, "Density", "LocalAttribute", attr3);
		  printf("Status returned by HE5_ZAreadlocattr() :  %d \n", status);
		  printf("Local attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr3[i]);

		  /* Read Global 'char' Attribute */
		  /* ---------------------------- */
		  status = HE5_ZAreadattr(ZAid, "GLOBAL_CHAR_ATTR", attr4);
		  printf("Status returned by HE5_ZAreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  printf("\t\t %s \n",attr4);

		  /* Read Global 'long' Attribute */
		  /* ---------------------------- */
		  status = HE5_ZAreadattr(ZAid, "GLOBAL_LONG_ATTR", attr5);
		  printf("Status returned by HE5_ZAreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %li \n",attr5[i]);

		  /* Read Global 'double' Attribute */
		  /* ------------------------------ */
		  status = HE5_ZAreadattr(ZAid, "GLOBAL_DOUBLE_ATTR", attr6);
		  printf("Status returned by HE5_ZAreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %f \n",attr6[i]);
		  
		}
	}
  
  status = HE5_ZAdetach(ZAid);
  status = HE5_ZAclose(zafid);
  
  return 0;
}


