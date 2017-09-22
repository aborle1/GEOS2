/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */




#include    <HE5_HdfEosDef.h>


/* In this program we (1) open the "Swath.h5" HDF-EOS file, (2) attach to  */
/* the "Swath1" swath, and (3) read data from the "Longitude" field. Also, */ 
/*   we read the global/group/local attributes                             */
/* ----------------------------------------------------------------------- */


int main()
{
  herr_t          status = FAIL;

  int             i, j, k;
  int             attr1[4];		/* data buffer for global attribute     */
  int             attr2[4];		/* ....  for group attribute            */
  int             attr3[4];		/* ....  for local attribute            */

  hid_t           swfid = FAIL;
  hid_t           SWid  = FAIL;

  char            attr4[20] = {0};    /* ...  for global 'char' attribute     */

  long            attr5[4];     /* ...  for global 'long' attribute     */

  double          attr6[4];     /* ... for global 'double' attribute    */

  float           attr7[4];     /* ... for local 'float'   attribute    */

  char            attr8[20] = {0};   /* ... for local 'char' attribute    */

  hssize_t        start[2];
  hsize_t         count[2];

  float           lng[20][10];

  hvl_t           buffer_out[4];/* Buffer to read out data from profile */
  char            *charname[10] = {0};


  /* Open the HDF-EOS swath file, "Swath.h5" */
  /* --------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDONLY);
  if (swfid != FAIL)
	{
	  /* Attach the "Swath1" swath */
	  /* ------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  /* Read the entire longitude field */
		  /* ------------------------------- */
		  start[0] = 0;    start[1] = 0;
		  count[0] = 20;   count[1] = 10;
		  status = HE5_SWreadfield(SWid, "Longitude", start, NULL, count, lng);
		  printf("Status returned by HE5_SWreadfield() :    %d \n", status);
	    
		  /* Display longitude data */
		  /* ---------------------- */
		  for (i = 0; i < 20; i++)
			for (j = 0; j < 10; j++)
			  printf("i j Longitude: %d %d %f\n", i, j, lng[i][j]);

                  start[0] = 0;    
                  count[0] = 10;   
                  for (i = 0; i < 10; i++)
                      charname[i] = (char *)malloc(95*sizeof(char));
                  status = HE5_SWreadfield(SWid, "Test_string", start, NULL, count, charname);
                  printf("Status returned by HE5_SWreadfield() :    %d \n", status);
 
                  /* Display Test_string data */
                  /* ---------------------- */
                  for (i = 0; i < 10; i++)
                          printf("i = %d	string =  %s\n", i, charname[i]);

                  for (i = 0; i < 10; i++)
                       free(charname[i]);

		  /* Read data from the Profile */
		  /* -------------------------- */
		  start[0] = 0;    count[0] = 4;
		  status = HE5_PRread(SWid, "Profile-2000", start, NULL, count, buffer_out);
		  printf("Status returned by HE5_PRread() :         %d \n", status);
		  
		  /* Display the profile data */
		  /* ------------------------ */
		  for (i = 0; i < 4; i++)
			{                      
			  printf("\tThe %d-th element length is %d \n", i, (unsigned)buffer_out[i].len);
			  for (j = 0; j < buffer_out[i].len; j++)
				printf("\t\t %d \n", ((unsigned int *)buffer_out[i].p)[j]);
			}
		  
		  /* Release IDs and memory */
		  /* ---------------------- */
		  status = HE5_PRreclaimspace(SWid, "Profile-2000", buffer_out);
		  printf("Status returned by HE5_PRreclaimspace() : %d \n", status);
		  
		  /* Read Global 'int' Attribute */
		  /* --------------------------- */
		  status = HE5_SWreadattr(SWid, "GlobalAttribute", attr1);
		  printf("Status returned by HE5_SWreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr1[i]);
			
		  /* Read Group Attribute */
		  /* -------------------- */
		  status = HE5_SWreadgrpattr(SWid, "GroupAttribute", attr2);
		  printf("Status returned by HE5_SWreadgrpattr() :  %d \n", status);
		  printf("Group attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr2[i]);
		  
		  /* Read Local Attribute */
		  /* -------------------- */
		  status = HE5_SWreadlocattr(SWid, "Density", "LocalAttribute_1", attr3);
		  printf("Status returned by HE5_SWreadlocattr() :  %d \n", status);
		  printf("Local attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %d \n",attr3[i]);

		  /* Read Local Attribute */
		  /* -------------------- */
		  status = HE5_SWreadlocattr(SWid, "Longitude", "LocalAttribute_2", attr7);
		  printf("Status returned by HE5_SWreadlocattr() :  %d \n", status);
		  printf("Local attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %f \n",attr7[i]);

                  status = HE5_SWreadlocattr(SWid, "Time", "LOCAL_CHAR_ATTR", attr8);
                  printf("Status returned by HE5_SWreadlocattr() :  %d \n", status);
                  printf("Local attribute values:\n");
                  printf("\t\t %s \n",attr8);

		  /* Read Global 'char' Attribute */
		  /* ---------------------------- */
		  status = HE5_SWreadattr(SWid, "GLOBAL_CHAR_ATTR", attr4);
		  printf("Status returned by HE5_SWreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  printf("\t\t %s \n",attr4);

		  /* Read Global 'long' Attribute */
		  /* ---------------------------- */
		  status = HE5_SWreadattr(SWid, "GLOBAL_LONG_ATTR", attr5);
		  printf("Status returned by HE5_SWreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %li \n",attr5[i]);

		  /* Read Global 'double' Attribute */
		  /* ------------------------------ */
		  status = HE5_SWreadattr(SWid, "GLOBAL_DOUBLE_ATTR", attr6);
		  printf("Status returned by HE5_SWreadattr() :     %d \n", status);
		  printf("Global attribute values:\n");
		  for (i = 0; i < 4; i++)
			printf("\t\t %f \n",attr6[i]);
		  
		}
	}
  
  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);
  
  return 0;
}


