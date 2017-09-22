/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |  
 ----------------------------------------------------------------------------  
 */


#include   <HE5_HdfEosDef.h>


#define FILENAME   "ZA.he5"
#define OBJECT     "ZA1"


int main(void)
{
  herr_t       status     = FAIL;

  int          fieldgroup = FAIL;

  hid_t        fid      = FAIL;
  hid_t        ZAid     = FAIL;
  hid_t        datatype = FAIL;

  H5T_class_t  classid = H5T_NO_CLASS;     
  H5T_order_t  order   = H5T_ORDER_ERROR;

  size_t       size    = 0;


  /* Open the HDF-EOS ZA file */
  /* ------------------------ */
  fid = HE5_ZAopen(FILENAME, H5F_ACC_RDONLY);
  printf("File ID returned by HE5_ZAopen() :         %d \n", fid);
    
  /* Attach to the "ZA1" za */
  /* ---------------------- */
  ZAid = HE5_ZAattach(fid, OBJECT);
  printf("ZA ID returned by HE5_ZAattach() :      %d \n", ZAid);

  /* Inquire data type information for the "Spectra" field */
  /* ----------------------------------------------------- */
  fieldgroup = HE5_HDFE_DATAGROUP;
  status = HE5_ZAinqdatatype(ZAid, "Spectra", NULL, fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_ZAinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  /* Inquire data type information for the attributes */
  /* ------------------------------------------------ */
  fieldgroup = HE5_HDFE_ATTRGROUP;
  status = HE5_ZAinqdatatype(ZAid, NULL, "GlobalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_ZAinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_GRPATTRGROUP;
  status = HE5_ZAinqdatatype(ZAid, NULL, "GroupAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_ZAinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_LOCATTRGROUP;
  status = HE5_ZAinqdatatype(ZAid, "Density", "LocalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_ZAinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}


  /* Detach from the za */
  /* ------------------ */
  status = HE5_ZAdetach(ZAid);
  printf("Status returned by HE5_ZAdetach() :        %d \n", status);

  /* Close the file */
  /* -------------- */
  status = HE5_ZAclose(fid);
  printf("Status returned by HE5_ZAclose() :         %d \n", status);
    
  return(0);
    
}




