/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include     <HE5_HdfEosDef.h>

/*
 * In this example we will retrieve information about (1) dimensions,
 * (2) dimension mappings (geolocation relations), (3) grid fields, 
 * and (4) (global/group/local) grid attributes.
 */


int main()
{

  herr_t          status = FAIL;

  int             i, rank[32], tilerank;
  int             projcode, zonecode, spherecode, tilecode;
  int             ndim = FAIL, nflds = FAIL;

  hid_t           gdfid = FAIL;
  hid_t           GDid1 = FAIL;
  hid_t           GDid2 = FAIL;

  hid_t           *ntype;
  hid_t           *ntype1;

  hsize_t         Dims[32], dimsize, count = 0, tiledims[8];

  long            xdimsize, ydimsize, n, strbufsize;

  double          upleftpt[2], lowrightpt[2], projparm[16];
    
  char            version[80];
  char            dimname[1024], fieldlist[1024];
  char            attrlist[80];

  int             level[ 5 ]      = { 0, 0, 0, 0, 0 };
  int             code            = 0;
    

  /*
   * Open the Grid File for read only access
   */	
  gdfid = HE5_GDopen("Grid.h5", H5F_ACC_RDONLY);
  if (gdfid != FAIL)
    {
	  HE5_EHgetversion(gdfid,version);
	  printf("Version:   \"%s\" \n", version);
	  
	  /* Attach the grid */

	  GDid1 = HE5_GDattach(gdfid, "UTMGrid");
	  GDid2 = HE5_GDattach(gdfid, "PolarGrid");
	  
	  ndim = HE5_GDinqdims(GDid1, dimname, Dims);
	  printf("Dimension list (UTMGrid): %s\n", dimname);
          for (i = 0; i < ndim; i++) printf("dim size: %li \n", (long)Dims[i]);
	  
	  ndim = HE5_GDinqdims(GDid2, dimname, Dims);
	  printf("Dimension list (PolarGrid): %s\n", dimname);
          for (i = 0; i < ndim; i++) printf("dim size: %li \n", (long)Dims[i]);
	  

	  dimsize = HE5_GDdiminfo(GDid1, "Time");
	  printf("Size of \"Time\" Array: %lu\n", (unsigned long)dimsize);

	  dimsize = HE5_GDdiminfo(GDid2, "Bands");
	  printf("Size of \"Bands\" Array: %lu\n", (unsigned long)dimsize);
	
	  status = HE5_GDgridinfo(GDid1, &xdimsize, &ydimsize,upleftpt, lowrightpt);
	  printf("X dim size, Y dim size (UTMGrid): %li %li\n",  xdimsize, ydimsize);
	  printf("Up left pt (UTMGrid): %lf %lf\n", upleftpt[0], upleftpt[1]);
	  printf("Low right pt (UTMGrid): %lf %lf\n",  lowrightpt[0], lowrightpt[1]);
	  
	  status = HE5_GDgridinfo(GDid2, &xdimsize, &ydimsize, upleftpt, lowrightpt);
	  printf("X dim size, Y dim size (PolarGrid): %li %li\n",  xdimsize, ydimsize);
	  printf("Up left pt (PolarGrid): %lf %lf\n",  upleftpt[0], upleftpt[1]);
	  printf("Low right pt (PolarGrid): %lf %lf\n",  lowrightpt[0], lowrightpt[1]);
	  
	       
	  status = HE5_GDprojinfo(GDid1, &projcode, &zonecode, &spherecode, NULL);
	  printf("projcode , zonecode (UTMGrid): %d %d\n", projcode, zonecode);
	  printf("spherecode (UTMGrid): %d\n", spherecode);	

	  status = HE5_GDprojinfo(GDid2, &projcode, NULL, &spherecode, projparm);
	  printf("projcode (PolarGrid): %d\n", projcode);	
	  printf("spherecode (PolarGrid): %d\n", spherecode);	
	  for (i = 0; i < 13; i++)
	    printf("Projection Parameter: %d %lf\n",i,projparm[i]);
	
	  ntype1 = (hid_t *)calloc(10, sizeof(hid_t)); 
	  nflds = HE5_GDinqfields(GDid1, fieldlist, rank, ntype1);
	  if (nflds != FAIL)
		{
		  printf("Data fields (UTMGrid): %s\n", fieldlist);
		  for (i = 0; i < nflds;i++)
			printf("Rank:   %i   Data type:   %i\n",rank[i], (int)ntype1[i]);
		}
	  
	  nflds = HE5_GDinqfields(GDid2, fieldlist, rank, ntype1);
	  if (nflds != FAIL)
		{
		  printf("Data fields (PolarGrid): %s\n", fieldlist);
		  for (i = 0; i < nflds;i++)
			printf("Rank:   %i   Data type:   %i\n",rank[i],(int)ntype1[i]);
		}
	
	  
	  status = HE5_GDfieldinfo(GDid2, "Spectra", rank, Dims, ntype1, dimname, NULL);
	  printf("Spectra rank: %d\n",rank[0]);
	  printf("Spectra dimensions:  \n");
	  for (i = 0; i < rank[0]; i++)
		printf("   %lu\n",(unsigned long)Dims[i]);
	  printf("Spectra dimension list: \n");
	  printf("   %s\n", dimname);
	  
          ntype = (hid_t *)calloc(10, sizeof(hid_t));
	  printf(" \n");
	  printf("Global Attributes \n");	
	  status = HE5_GDattrinfo(GDid1, "GlobalAttribute", ntype, &count);
	  printf("\tNumber of attribute elements: %lu \n", (unsigned long)count);
	  printf("\tData type of attribute:       %d \n", (int)*ntype);
	  
	  printf(" \n");
	  printf("Group Attributes \n");	
	  status = HE5_GDgrpattrinfo(GDid1, "GroupAttribute", ntype, &count);
	  printf("\tNumber of attribute elements: %lu \n", (unsigned long)count);
	  printf("\tData type of attribute:       %d \n", (int)*ntype);
	  
	  printf(" \n");
	  printf("Local Attributes \n");	
	  status = HE5_GDlocattrinfo(GDid1, "Vegetation", "LocalAttribute", ntype, &count);
	  printf("\tNumber of attribute elements: %lu \n", (unsigned long)count);
	  printf("\tData type of attribute:       %d \n", (int)*ntype);
	  
	  
	  printf(" \n");
	  printf("Global Attributes \n");	
	  n = HE5_GDinqattrs(GDid1, NULL, &strbufsize);
	  printf("\tNumber of attributes:                    %li \n", n);
	  printf("\tSize of attribute list:                  %li \n", strbufsize);
	  n = HE5_GDinqattrs(GDid1, attrlist, &strbufsize);
	  printf("\tAttribute list:                          %s \n", attrlist);
	  
	  
	  printf(" \n");
	  printf("Group Attributes \n");	
	  n = HE5_GDinqgrpattrs(GDid1, NULL, &strbufsize);
	  printf("\tNumber of attributes:                    %li \n", n);
	  printf("\tSize of attribute list:                  %li \n", strbufsize);
	  n = HE5_GDinqgrpattrs(GDid1, attrlist, &strbufsize);
	  printf("\tAttribute list:                          %s \n", attrlist);
	  
	  
	  printf(" \n");
	  printf("Local Attributes \n");	
	  n = HE5_GDinqlocattrs(GDid1, "Vegetation", NULL, &strbufsize);
	  printf("\tNumber of attributes:                    %li \n", n);
	  printf("\tSize of attribute list:                  %li \n", strbufsize);
	  n = HE5_GDinqlocattrs(GDid1, "Vegetation", attrlist, &strbufsize);
	  printf("\tAttribute list:                          %s \n", attrlist);
	  
	  
	  free(ntype);
          free(ntype1);
	  
	  n = HE5_GDnentries(GDid1, HE5_HDFE_NENTDIM, &strbufsize);
	  printf("Number of dimension entries (UTMGrid): %li \n", n);
	  printf("Length of Dimension List (UTMGrid):    %li \n", strbufsize);
	  
	  n = HE5_GDnentries(GDid1, HE5_HDFE_NENTDFLD, &strbufsize);
	  printf("Number of data fields (UTMGrid):       %li \n", n);
	  printf("Length of Field List (UTMGrid):        %li \n", strbufsize);	

          status = HE5_GDtileinfo(GDid1,"Vegetation",&tilecode,&tilerank,tiledims);
          printf("\n");
          printf("\t\t Vegetation tile code: %d\n", tilecode);
          printf("\t\t Vegetation tile rank: %d\n", tilerank);
          for (i = 0; i < tilerank; i++)
               printf("\t\t Vegetation tile dimension %d: %lu\n",i+1, (unsigned long)tiledims[i]);

          status = HE5_GDcompinfo(GDid1,"Vegetation",&code,level);
          printf("\t\t Compression scheme       Compression level\n");
          printf("\t\t    %d,                      %d\n", code, level[0]);
	  
	}

	
  status = HE5_GDdetach(GDid1);
  status = HE5_GDdetach(GDid2);
  status = HE5_GDclose(gdfid);
	
  return 0;
}





