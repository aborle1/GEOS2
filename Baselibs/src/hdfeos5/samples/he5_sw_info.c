/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include      <HE5_HdfEosDef.h>

/*  In this program we retrieve information about (1) dimensions, (2)  */
/*   dimension mappings (geolocation relations), (3) swath fields,     */
/*     and (4) the global/group/local attributes                       */
/* ------------------------------------------------------------------- */

int main()
{
  herr_t          status = FAIL;

  int             i, rk, *rank, rk1, chunk_rank;

  hid_t           swfid = FAIL, SWid = FAIL;

  hid_t           ntype[10];
  hid_t           dtype = FAIL;
  hid_t           dtype_char = FAIL;
  hid_t           dtype1 = FAIL;

  long            ndims, strbufsize, nmaps, nflds, nattr;
  long            *off, *inc, *indx, offset, incr;

  hsize_t         *sizes, dimsize;
  hsize_t         dim[8], *dims, dim1[8], chunk_dim[8];
  hsize_t         n, nelem = 0;
    
  char            version[80] = {0};
  char            *dimname, *dimmap, *fieldlist; 
  char            dimlist[80], attrlist[80], dimlist1[80];

   int         level[ 5 ]      = { 0, 0, 0, 0, 0 };
   int         code            = 0;

  /* Open the Swath HDF-EOS File "Swath.h5" for reading only */
  /* ------------------------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDONLY);
  if (swfid != FAIL)
    {
	  HE5_EHgetversion(swfid, version);
	  printf("HDF-EOS library version: \"%s\" \n", version);
	
	  /* Attach the swath "Swath1" */
	  /* ------------------------  */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  /* Inquire Dimensions */
		  /* ------------------ */
		  ndims = HE5_SWnentries(SWid, HE5_HDFE_NENTDIM, &strbufsize);
		  dims = (hsize_t *) calloc(ndims, sizeof(hsize_t));
		  dimname = (char *) calloc(strbufsize + 1, 1);
	    
		  ndims = HE5_SWinqdims(SWid, dimname, dims);

		  printf("Dimension list: %s\n", dimname);
		  for (i = 0; i < ndims; i++)
			printf("dim size: %li\n", (long)dims[i]);
	    
		  free(dims);
		  free(dimname);

		  /* Inquire Dimension Mappings */
		  /* -------------------------- */
		  nmaps = HE5_SWnentries(SWid, HE5_HDFE_NENTMAP, &strbufsize);
	    
		  off = (long *)calloc(nmaps, sizeof(long));
		  inc = (long *)calloc(nmaps, sizeof(long));
		  dimmap = (char *)calloc(strbufsize + 1, 1);
	    
		  nmaps = HE5_SWinqmaps(SWid, dimmap, off, inc);
		  printf("Dimension map: %s\n", dimmap);
		  for (i = 0; i < nmaps; i++)
			printf("offset increment: %li %li\n",
				   off[i], inc[i]);
		  free(off);
		  free(inc);
		  free(dimmap);


		  /* Inquire Indexed Dimension Mappings */
		  /* ---------------------------------- */
		  nmaps = HE5_SWnentries(SWid, HE5_HDFE_NENTIMAP, &strbufsize);
		  sizes = (hsize_t *) calloc(nmaps, sizeof(hsize_t));
		  dimmap = (char *) calloc(strbufsize + 1, 1);
		  nmaps = HE5_SWinqidxmaps(SWid, dimmap, sizes);

		  printf("Index Dimension map: %s\n", dimmap);
		  for (i = 0; i < nmaps; i++)
			printf("sizes: %lu\n", (unsigned long)sizes[i]);

		  free(sizes);
		  free(dimmap);


		  /* Inquire Geolocation Fields */
		  /* -------------------------- */
		  nflds = HE5_SWnentries(SWid, HE5_HDFE_NENTGFLD, &strbufsize);
		  rank = (int *)calloc(nflds, sizeof(int));
		  fieldlist = (char *) calloc(strbufsize + 1, 1);
		  nflds = HE5_SWinqgeofields(SWid, fieldlist, rank, ntype);

		  printf("geo fields: %s\n", fieldlist);
		  for (i = 0; i < nflds; i++)
			printf("Rank:  %d   Data type:  %d\n", rank[i], ntype[i]);

		  free(rank);
		  free(fieldlist);

		  /* Inquire Data Fields */
		  /* ------------------- */
		  nflds = HE5_SWnentries(SWid, HE5_HDFE_NENTDFLD, &strbufsize);
		  rank = (int *) calloc(nflds, sizeof(int));
		  fieldlist = (char *) calloc(strbufsize + 1, 1);
		  nflds = HE5_SWinqdatafields(SWid, fieldlist, rank, ntype);

		  printf("data fields: %s\n", fieldlist);
		  for (i = 0; i < nflds; i++)
			printf("Rank:  %d   Data type:  %d\n", rank[i], ntype[i]);

		  free(rank);
		  free(fieldlist);


		  /* Get info on "GeoTrack" dim */
		  /* -------------------------- */
		  dimsize = HE5_SWdiminfo(SWid, "GeoTrack");
		  printf("Size of GeoTrack: %lu\n", (unsigned long)dimsize);

                  dimsize = HE5_SWdiminfo(SWid, "Unlim");
                  printf("Size of Unlim: %li\n", (long)dimsize);

		  /* Get info on "GeoTrack/Res2tr" mapping */
		  /* ------------------------------------- */
		  status = HE5_SWmapinfo(SWid, "GeoTrack", "Res2tr", &offset, &incr);
		  printf("Mapping Offset: %li\n", offset);
		  printf("Mapping Increment: %li\n", incr);


		  /* Get info on "IndxTrack/Res2tr" indexed mapping */
		  /* ---------------------------------------------- */
		  dimsize = HE5_SWdiminfo(SWid, "IndxTrack");
		  indx = (long *) calloc(dimsize, sizeof(long));
		  n = HE5_SWidxmapinfo(SWid, "IndxTrack", "Res2tr", indx);
		  for (i = 0; i < n; i++)
			printf("Index Mapping Entry %d: %li\n", i+1, indx[i]);
		  free(indx);

		  /* Get info on "Longitude" Field */
		  /* ----------------------------- */
		  status = HE5_SWfieldinfo(SWid, "Longitude", &rk, dim, &dtype, dimlist, NULL);
		  printf("Longitude Rank: %d\n", rk);
		  printf("Longitude NumberType: %d\n", dtype);
		  printf("Longitude Dimension List: %s\n", dimlist);	    
		  for (i = 0; i < rk; i++)
			printf("Dimension %d: %lu\n",i+1,(unsigned long)dim[i]);

                  status = HE5_SWfieldinfo(SWid, "Test_string", &rk1, dim1, &dtype_char, dimlist1, NULL);
                  printf("Test_string Rank: %d\n", rk1);
                  printf("Test_string NumberType: %d\n", dtype_char);
                  printf("Test_string Dimension List: %s\n", dimlist1);
                  for (i = 0; i < rk1; i++)
                        printf("Dimension %d: %lu\n",i+1,(unsigned long)dim1[i]);

          dtype1 = FAIL;
		  /* Get info about Global Attributes */
		  /* -------------------------------- */
		  printf("Global Attribute:\n");
		  status = HE5_SWattrinfo(SWid,"GlobalAttribute",&dtype1, &nelem);
		  printf("\t\t Data type:          %d\n", dtype1);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    
 
                  nelem = 0;
                  status = HE5_SWattrinfo(SWid,"GLOBAL_CHAR_ATTR",&dtype1, &nelem);
                  printf("\t\t Data type:          %d\n", dtype1);
                  printf("\t\t The size of string: %lu \n", (unsigned long)nelem);

                  nelem = 0;
                  status = HE5_SWattrinfo(SWid,"GLOBAL_DOUBLE_ATTR",&dtype1, &nelem);
                  printf("\t\t Data type:          %d\n", dtype1);
                  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);

		  nelem = 0;
          dtype1 = FAIL;
		  /* Get info about Group Attributes */
		  /* ------------------------------- */
		  printf("Group Attribute:\n");
		  status = HE5_SWgrpattrinfo(SWid,"GroupAttribute",&dtype1,&nelem);
		  printf("\t\t Data type:          %d\n", dtype1);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    

		  nelem = 777;
          dtype1 = FAIL;
		  /* Get info about Local Attributes */
		  /* ------------------------------- */
		  printf("Local Attribute:\n");
		  status = HE5_SWlocattrinfo(SWid,"Density", "LocalAttribute_1",&dtype1,&nelem);
		  printf("\t\t Data type:          %d\n", dtype1);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    

		  printf("Local Attribute:\n");
		  status = HE5_SWlocattrinfo(SWid,"Longitude", "LocalAttribute_2",&dtype1,&nelem);
		  printf("\t\t Data type:          %d\n", dtype1);
		  printf("\t\t Number of elements: %lu \n", (unsigned long)nelem);    

                  nelem = 0;
                  status = HE5_SWlocattrinfo(SWid,"Time", "LOCAL_CHAR_ATTR",&dtype1,&nelem);
                  printf("\t\t Data type:          %d\n", dtype1);
                  printf("\t\t The size of string: %lu \n", (unsigned long)nelem);

		  /* Inquire Global Attributes */
		  /* ------------------------- */
		  printf("Global Attributes:\n");
		  nattr = HE5_SWinqattrs(SWid, NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  n = HE5_SWinqattrs(SWid, attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);
           
		  /* Inquire Group Attributes */
		  /* ------------------------ */
		  strbufsize = 0;
		  printf("\n");
		  printf("Group Attributes:\n");
		  nattr = HE5_SWinqgrpattrs(SWid, NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  strcpy(attrlist,"");
		  nattr = HE5_SWinqgrpattrs(SWid, attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);

		  /* Inquire Local Attributes */
		  /* ------------------------ */
		  strbufsize = 0;
		  printf("\n");
		  printf("Local Attributes:\n");
		  nattr = HE5_SWinqlocattrs(SWid, "Density", NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  strcpy(attrlist,"");
		  nattr = HE5_SWinqlocattrs(SWid, "Density", attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);

		  nattr = HE5_SWinqlocattrs(SWid, "Longitude", NULL, &strbufsize);
		  printf("\t\t Number of attributes:             %li \n", nattr);
		  printf("\t\t String length of attribute list:  %li \n", strbufsize);
		  strcpy(attrlist,"");
		  nattr = HE5_SWinqlocattrs(SWid, "Longitude", attrlist, &strbufsize);
		  printf("\t\t Attribute list:                   %s \n", attrlist);

                  status = HE5_SWcompinfo(SWid,"Spectra",&code,level);
                  printf("\n");
                  printf("\t\t Compression scheme       Compression level\n");
                  printf("\t\t    %d,                      %d\n", code, level[0]);
 
                  status = HE5_SWchunkinfo(SWid,"Spectra",&chunk_rank,chunk_dim);
                  printf("\n");
                  printf("\t\t Spectra chunk rank: %d\n", chunk_rank);
                  for (i = 0; i < chunk_rank; i++)
                        printf("\t\t Spectra chunk dimension %d: %lu\n",i+1, (unsigned long)chunk_dim[i]);


		}
    }

  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);

  return 0;
}

