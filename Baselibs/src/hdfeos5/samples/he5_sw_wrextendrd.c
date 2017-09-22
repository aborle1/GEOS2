/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include <HE5_HdfEosDef.h>

/* This program demonstrates the use of the unlimited dimension   */
/*               in creating an appendable field                  */
/* -------------------------------------------------------------- */


int main()
{
  herr_t          status = FAIL;

  int             i, rank = FAIL;
  int             inarray[20] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
  int             outarray[40];

  hid_t           swfid = FAIL;
  hid_t           SWid  = FAIL;

  hid_t     *ntype;

  hssize_t        start[1];

  hsize_t         count[1], dims[1];

  char            dimlist[100];
  char            maxdimlist[100];
 

  /* Open the swath file */
  /* ------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
	{
	  /* Attach to the swath "Swath1" */
	  /* ---------------------------- */
	  SWid  = HE5_SWattach(swfid, "Swath1");
	  if (SWid != NULL)
		{
		  /* Inquire information about "Count" field */
		  /* --------------------------------------- */
		  ntype = (hid_t *)calloc(1,sizeof(hid_t));
		  
		  status = HE5_SWfieldinfo(SWid, "Count", &rank, dims, ntype, dimlist, maxdimlist);
		  if (status != FAIL)
			{
			  printf("No. of elements in \"Count\":  %lu\n", (unsigned long)dims[0]);
			  printf("\"DimList\" :                  %s \n", dimlist);
			  printf("\"MaxdimList\" :               %s \n", maxdimlist);
			}
		  
		  /* Write 20 records to the "Count" field */
		  /* ------------------------------------- */
		  start[0] = 0;
		  count[0] = 20;
		  status = HE5_SWwritefield(SWid, "Count", start, NULL, count, inarray);
		  printf("Status returned by HE5_SWwritefield:  %d \n", status);
		  

		  /* Inquire information about "Count" field again */
		  /* --------------------------------------------- */  
		  dims[0] = 0;
		  status = HE5_SWfieldinfo(SWid, "Count", &rank, dims, ntype, dimlist, maxdimlist);
		  if (status != FAIL)
			{
			  printf("No. of records written:        %lu \n",(unsigned long)dims[0]);
			  printf("\"DimList\" :                  %s \n", dimlist);
			  printf("\"MaxdimList\" :               %s \n", maxdimlist);
			}
		  
		  /* Append 10 records to the "Count" field */
		  /* -------------------------------------- */
		  start[0] = 22;
		  count[0] = 10;
		  status = HE5_SWwritefield(SWid, "Count", start, NULL, count, inarray);
		  printf("Status returned by HE5_SWwritefield:  %d \n", status);
		  
		  /* Inquire information about "Count" field one more time */
		  /* ----------------------------------------------------- */  
		  dims[0] = 0;
		  status = HE5_SWfieldinfo(SWid, "Count", &rank, dims, ntype, dimlist, maxdimlist);
		  if (status != FAIL)
			{
			  printf("No. of records written:        %lu\n",(unsigned long)dims[0]);
			  printf("\"DimList\" :                  %s \n", dimlist);
			  printf("\"MaxdimList\" :               %s \n", maxdimlist);
			  free(ntype);   
			}

		  /* Display the "Count" field data */
		  /* ------------------------------ */
		  start[0] = 0;
		  status = HE5_SWreadfield(SWid, "Count", start, NULL, dims, outarray);
		  if (status != FAIL )
			{
			  for (i = 0; i < (unsigned long)dims[0]; i++) 
				printf("\t\t element #%d:  value: %d \n", i+1, outarray[i]);
			}
		  
		}
	}
  
  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);
  
  return 0;
}








