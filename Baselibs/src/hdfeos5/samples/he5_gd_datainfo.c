/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |  
 ----------------------------------------------------------------------------  
 */

#include   <HE5_HdfEosDef.h>

#define FILENAME   "Grid.h5"
#define OBJECT     "UTMGrid"
#define FIELDNAME  "Vegetation"

int main(void)
{
  herr_t       status     = FAIL;

  int          fieldgroup = FAIL;

  hid_t        fid        = FAIL;
  hid_t        GDid       = FAIL;
  hid_t        datatype   = FAIL;

  H5T_class_t  classid    = H5T_NO_CLASS;     
  H5T_order_t  order      = H5T_ORDER_ERROR;

  size_t       size       = 0;

  fid = HE5_GDopen(FILENAME, H5F_ACC_RDONLY);
  printf("File ID returned by HE5_GDopen() :         %d \n", fid);
    
  GDid = HE5_GDattach(fid, OBJECT);
  printf("Grid ID returned by HE5_GDattach() :       %d \n", GDid);

  fieldgroup = HE5_HDFE_DATAGROUP;
  status = HE5_GDinqdatatype(GDid, FIELDNAME, NULL, fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_GDinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_ATTRGROUP;
  status = HE5_GDinqdatatype(GDid, NULL, "GlobalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_GDinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_GRPATTRGROUP;
  status = HE5_GDinqdatatype(GDid, NULL, "GroupAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_GDinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_LOCATTRGROUP;
  status = HE5_GDinqdatatype(GDid, FIELDNAME, "LocalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_GDinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  status = HE5_GDdetach(GDid);
  printf("Status returned by HE5_GDdetach() :        %d \n", status);

  status = HE5_GDclose(fid);
  printf("Status returned by HE5_GDclose() :         %d \n", status);
    
  return(0);
}






