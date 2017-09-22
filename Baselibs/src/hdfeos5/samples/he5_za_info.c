/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include      <HE5_HdfEosDef.h>

/* ------------------------------------------------------------------- */
/*  In this program we retrieve information about (1) dimensions, (2)  */
/*  za fields, and (3) the global/group/local attributes               */
/* ------------------------------------------------------------------- */

int main()
{
  herr_t          status = FAIL;

  int             i, *rank, chunk_rank;

  hid_t           zafid = FAIL, ZAid = FAIL;

  hid_t           ntype[10];
  hid_t           dtype = FAIL;

  long            ndims, strbufsize, nflds, nattr;

  hsize_t         dimsize;
  hsize_t         *dims, chunk_dim[8];
  hsize_t         n, nelem = 0;
    
  char            version[80] = {0};
  char            *dimname, *fieldlist; 
  char            attrlist[80];

  /* Open the ZA HDF-EOS File "ZA.he5" for reading only */
  /* ------------------------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDONLY);
  if (zafid != FAIL)
    {
	  HE5_EHgetversion(zafid, version);
	  printf("HDF-EOS library version: \"%s\" \n", version);
	
	  /* Attach the ZA "ZA1" */
	  /* ------------------- */
	  ZAid = HE5_ZAattach(zafid, "ZA1");
	  if (ZAid != FAIL)
		{
		  /* Inquire Dimensions */
		  /* ------------------ */
		  ndims = HE5_ZAnentries(ZAid, HE5_HDFE_NENTDIM, &strbufsize);
		  dims = (hsize_t *) calloc(ndims, sizeof(hsize_t));
		  dimname = (char *) calloc(strbufsize + 1, 1);
	    
		  ndims = HE5_ZAinqdims(ZAid, dimname, dims);

		  printf("Dimension list: %s\n", dimname);
		  for (i = 0; i < ndims; i++)
			printf("dim size: %li \n", (long)dims[i]);
	    
		  free(dims);
		  free(dimname);


		  /* Inquire Data Fields */
		  /* ------------------- */
		  nflds = HE5_ZAnentries(ZAid, HE5_HDFE_NENTDFLD, &strbufsize);
		  rank = (int *) calloc(nflds, sizeof(int));
		  fieldlist = (char *) calloc(strbufsize + 1, 1);
		  nflds = HE5_ZAinquire(ZAid, fieldlist, rank, ntype);

		  printf("data fields: %s\n", fieldlist);
		  for (i = 0; i < nflds; i++)
			printf("Rank:  %d   Data type:  %d\n", rank[i], ntype[i]);

		  free(rank);
		  free(fieldlist);


		  /* Get info on "MyTrack1" dim */
		  /* -------------------------- */
		  dimsize = HE5_ZAdiminfo(ZAid, "MyTrack1");
		  printf("Size of MyTrack1: %lu\n", (unsigned long)dimsize);


                  dtype = FAIL;
		  /* Get info about Global Attributes */
		  /* -------------------------------- */
		  printf("Global Attribute:\n");
		  status = HE5_ZAattrinfo(ZAid,"GlobalAttribute",&dtype, &nelem);
		  printf("\t\t Data type:          %d\n", dtype);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    

		  nelem = 0;
                  dtype = FAIL;
		  /* Get info about Group Attributes */
		  /* ------------------------------- */
		  printf("Group Attribute:\n");
		  status = HE5_ZAgrpattrinfo(ZAid,"GroupAttribute",&dtype,&nelem);
		  printf("\t\t Data type:          %d\n", dtype);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    

		  nelem = 777;
                  dtype = FAIL;
		  /* Get info about Local Attributes */
		  /* ------------------------------- */
		  printf("Local Attribute:\n");
		  status = HE5_ZAlocattrinfo(ZAid,"Density", "LocalAttribute",&dtype,&nelem);
		  printf("\t\t Data type:          %d\n", dtype);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    


		  /* Inquire Global Attributes */
		  /* ------------------------- */
		  printf("Global Attributes:\n");
		  nattr = HE5_ZAinqattrs(ZAid, NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  n = HE5_ZAinqattrs(ZAid, attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);
           
		  /* Inquire Group Attributes */
		  /* ------------------------ */
		  strbufsize = 0;
		  printf("\n");
		  printf("Group Attributes:\n");
		  nattr = HE5_ZAinqgrpattrs(ZAid, NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  strcpy(attrlist,"");
		  nattr = HE5_ZAinqgrpattrs(ZAid, attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);

		  /* Inquire Local Attributes */
		  /* ------------------------ */
		  strbufsize = 0;
		  printf("\n");
		  printf("Local Attributes:\n");
		  nattr = HE5_ZAinqlocattrs(ZAid, "Density", NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  strcpy(attrlist,"");
		  nattr = HE5_ZAinqlocattrs(ZAid, "Density", attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);

                  status = HE5_ZAchunkinfo(ZAid,"Spectra",&chunk_rank,chunk_dim);
                  printf("\n");
                  printf("\t\t Spectra chunk_rank: %d\n", chunk_rank);
                  for (i = 0; i < chunk_rank; i++)
                        printf("\t\t Spectra chunk_dim dimension %d: %lu\n",i+1, (unsigned long)chunk_dim[i]);

	    
		}
    }

  status = HE5_ZAdetach(ZAid);
  status = HE5_ZAclose(zafid);

  return 0;
}

