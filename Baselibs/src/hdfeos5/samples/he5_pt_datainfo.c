/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |  
 ----------------------------------------------------------------------------  
 */


#include <HE5_HdfEosDef.h>

#define FILENAME   "Point.h5"
#define OBJECT     "FixedBuoy Point"


int main(void)
{
  herr_t       status     = FAIL;

  int          fieldgroup = FAIL;

  hid_t        fid = FAIL, PTid = FAIL, datatype = FAIL;

  H5T_class_t  classid  = H5T_NO_CLASS;  
   
  H5T_order_t  order    = H5T_ORDER_ERROR;

  size_t       size     = 0;

  fid = HE5_PTopen(FILENAME, H5F_ACC_RDONLY);
  printf("File ID returned by HE5_PTopen() :        %d \n", fid);
    
  PTid = HE5_PTattach(fid, OBJECT);
  printf("Point ID returned by HE5_PTattach() :     %d \n", PTid);

  fieldgroup = HE5_HDFE_DATAGROUP;
  status = HE5_PTinqdatatype(PTid, "Observations", NULL, fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_PTinqdatatype() :  %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_ATTRGROUP;
  status = HE5_PTinqdatatype(PTid, NULL, "GlobalAttribute_int", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_PTinqdatatype() :  %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_GRPATTRGROUP;
  status = HE5_PTinqdatatype(PTid, NULL, "GroupAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_PTinqdatatype() :  %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_LOCATTRGROUP;
  status = HE5_PTinqdatatype(PTid, "Observations", "LocalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_PTinqdatatype() :  %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", (int)datatype);
	  printf("\tclass ID:    %d \n", (int)classid);
	  printf("\torder:       %d \n", (int)order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :       %d \n", status);

  status = HE5_PTclose(fid);
  printf("Status returned by HE5_PTclose() :        %d \n", status);
    
  return(0);    
}


