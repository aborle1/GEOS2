/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999  Emergent IT Inc.  and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include <HE5_HdfEosDef.h>

int main()
{
  herr_t          status = FAIL;

  int             i, attr;
  int             attr1[5], attr2[5];

  hid_t           ptfid = FAIL, PTid = FAIL;    

  hid_t           *nt = (hid_t *)NULL;

  long            nattr, strbufsize;
    
  hsize_t         sz;
 
  char            attrlist[256] ;


  /* Open the HDF-EOS file, "Point.h5" */
  /* --------------------------------- */
  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDONLY);
  printf("File ID returned by HE5_PTopen() :       %d \n", ptfid);

  PTid = HE5_PTattach(ptfid, "FixedBuoy Point");
  printf("Point ID returned by HE5_PTattach() :    %d \n", PTid);

  nt = (hid_t *)calloc(1, sizeof(hid_t));

  /* Global attributes */
  /* ---------------- */
  printf("Global Attribute: \n");
  status = HE5_PTreadattr(PTid, "GlobalAttribute_int", &attr);
  printf("Status returned by HE5_PTreadattr() :    %d \n", status);
  printf("Attribute value:                         %d\n", attr);

  nattr = HE5_PTinqattrs(PTid, NULL, &strbufsize);
  printf("Number of attributes:                    %li\n", nattr);
  printf("Size (in bytes) of attribute list:       %li\n", strbufsize);
  nattr = HE5_PTinqattrs(PTid, attrlist, &strbufsize);
  printf("Attribute list:                          %s\n", attrlist);

  status = HE5_PTattrinfo(PTid, "GlobalAttr_Integer", nt, &sz);
  printf("Data type of attribute:                  %d\n", (int)*nt);
  printf("Number of attribute elements:            %d\n", (int)sz);


  /* Group attributes */
  /* ---------------- */
  printf("\n");
  printf("Group Attribute: \n");
  status = HE5_PTreadgrpattr(PTid, "GroupAttribute", attr1);
  printf("Status returned by HE5_PTreadgrpattr() : %d \n", status);
  printf("Attribute values: \n");
  for (i = 0; i < 5; i++)
    {
	  printf("\t\t %d \n", attr1[i]);
    }    
  nattr = HE5_PTinqgrpattrs(PTid, NULL, &strbufsize);
  printf("Number of attributes:                    %li\n", nattr);
  printf("Size (in bytes) of attribute list:       %li\n", strbufsize);
  nattr = HE5_PTinqgrpattrs(PTid, attrlist, &strbufsize);
  printf("Attribute list:                          %s\n", attrlist);

  status = HE5_PTgrpattrinfo(PTid, "GroupAttribute", nt, &sz);
  printf("Data type of attribute:                  %d\n",(int)*nt);
  printf("Number of attribute elements:            %d\n", (int)sz);

  /* Local attributes */
  /* ---------------- */
  printf("\n");
  printf("Local Attribute: \n");
  status = HE5_PTreadlocattr(PTid, "Observations", "LocalAttribute", attr2);
  printf("Status returned by HE5_PTreadlocattr() : %d \n", status);
  printf("Attribute values: \n");
  for (i = 0; i < 5; i++)
    {
	  printf("\t\t %d \n", attr2[i]);
    }    
  nattr = HE5_PTinqlocattrs(PTid, "Observations", NULL, &strbufsize);
  printf("Number of attributes:                    %li\n", nattr);
  printf("Size (in bytes) of attribute list:       %li\n", strbufsize);
  nattr = HE5_PTinqlocattrs(PTid, "Observations", attrlist, &strbufsize);
  printf("Attribute list:                          %s\n", attrlist);

  status = HE5_PTlocattrinfo(PTid, "Observations", "LocalAttribute", nt, &sz);
  printf("Data type of attribute:                  %d\n", (int)*nt);
  printf("Number of attribute elements:            %d\n", (int)sz);


  free(nt);
	
  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :      %d \n", status);

  status = HE5_PTclose(ptfid);
  printf("Status returned by HE5_PTclose() :       %d \n", status);

  return 0;
}







