/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include   <HE5_HdfEosDef.h>


/*
 * In this example we will (1) open the "Grid.h5" HDF-EOS file, (2) attach to
 * the "UTMGrid", and (3) write data to the appendable "Vegetation" field. 
 */

int main()
{
  herr_t          status = FAIL;

  int             i, j;
  int             rank = FAIL;

  hid_t           gdfid = FAIL;
  hid_t           GDid  = FAIL;

  hid_t           *ntype;    
  
  hssize_t        start[3];

  hsize_t         edge[3], dims[8] = {0,0,0,0,0,0,0,0};

  float           veg[200][120];
   
  char            buf1[256], buf2[256];
    


  /* Fill veg array */
  /* -------------- */
  for (i = 0; i < 200; i++)
	for (j = 0; j < 120; j++)
	  veg[i][j] = (float)(10+i);
    
  /* Open the HDF grid file "Grid.h5" */
  /* -------------------------------  */
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDWR);
  if (gdfid != FAIL)
    {
	  /* Attach the "UTMGrid" */
      /* -------------------  */
	  GDid = HE5_GDattach(gdfid, "UTMGrid");
	  if (GDid != FAIL)
		{
          /* Write data to the appendable field */
          /* ---------------------------------- */  
		  start[0] = 0;     start[1] = 0;
		  edge[0]  = 200;   edge[1]  = 120;
		  status = HE5_GDwritefield(GDid, "Vegetation", start, NULL, edge, veg);
          printf("Status returned by HE5_GDwritefield():  %d\n", status);

          strcpy(buf1, "");
          strcpy(buf2, "");
          dims[0] = 0;
		  dims[1] = 0;

          ntype = (hid_t *)calloc(1, sizeof(hid_t));
          status = HE5_GDfieldinfo(GDid, "Vegetation", &rank, dims, ntype, buf1, buf2);
          printf("Status returned by HE5_GDfieldinfo():   %d\n", status);
          if (status != FAIL)
			{
			  printf("\tThe dimension list:         %s \n", buf1);
			  printf("\tThe maximum dimension list: %s \n", buf2);
			  printf("The sizes of dataset dimensions before appending: \n");
			  for (i = 0; i < rank; i++) 
				printf("\t                            %lu \n", (unsigned long)dims[i]);
			}
          free(ntype);
		}

	  /* Detach from "UTMGrid" */
      /* --------------------  */
	  status = HE5_GDdetach(GDid);
	  
	  /* Attach the "UTMGrid" */
      /* -------------------  */	
	  GDid = HE5_GDattach(gdfid, "UTMGrid");
	  if (GDid != FAIL)
		{  
          /* Write new data to the appendable field */
          /* -------------------------------------- */   
		  start[0] = 180;   start[1] = 100;
		  edge[0]  = 100;   edge[1]  = 80;
		  status = HE5_GDwritefield(GDid, "Vegetation", start, NULL, edge, veg);
          printf("Status returned by HE5_GDwritefield():  %d\n", status);

          strcpy(buf1, "");
          strcpy(buf2, "");
          dims[0] = 0;    dims[1] = 0;

          ntype = (hid_t *)calloc(1, sizeof(hid_t));	  
          status = HE5_GDfieldinfo(GDid, "Vegetation", &rank, dims, ntype, buf1, buf2);
          printf("Status returned by HE5_GDfieldinfo():   %d\n", status);
          if (status != FAIL)
			{
			  printf("\tThe dimension list:         %s \n", buf1);
			  printf("\tThe maximum dimension list: %s \n", buf2);
			  printf("The sizes of dataset dimensions after appending: \n");
			  for (i = 0; i < rank; i++) 
				printf("\t                            %lu \n", (unsigned long)dims[i]);
			}	  
          free(ntype);
		}
	  status = HE5_GDdetach(GDid);
    }
  status = HE5_GDclose(gdfid);
  
  return 0;
}
