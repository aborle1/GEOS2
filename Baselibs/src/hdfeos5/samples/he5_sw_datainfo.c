/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |  
 ----------------------------------------------------------------------------  
 */


#include   <HE5_HdfEosDef.h>


#define FILENAME   "Swath.h5"
#define OBJECT     "Swath1"


int main(void)
{
  herr_t       status     = FAIL;

  int          fieldgroup = FAIL;

  hid_t        fid      = FAIL;
  hid_t        SWid     = FAIL;
  hid_t        datatype = FAIL;

  H5T_class_t  classid = H5T_NO_CLASS;     
  H5T_order_t  order   = H5T_ORDER_ERROR;

  size_t       size    = 0;


  /* Open the HDF-EOS swath file */
  /* --------------------------- */
  fid = HE5_SWopen(FILENAME, H5F_ACC_RDONLY);
  printf("File ID returned by HE5_SWopen() :         %d \n", fid);
    
  /* Attach to the "Swath1" swath */
  /* ---------------------------- */
  SWid = HE5_SWattach(fid, OBJECT);
  printf("Swath ID returned by HE5_SWattach() :      %d \n", SWid);

  /* Inquire data type information for the "Spectra" field */
  /* ----------------------------------------------------- */
  fieldgroup = HE5_HDFE_DATAGROUP;
  status = HE5_SWinqdatatype(SWid, "Spectra", NULL, fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  /* Inquire data type information for the profile */
  /* --------------------------------------------- */
  fieldgroup = HE5_HDFE_PROFGROUP;
  status = HE5_SWinqdatatype(SWid, "Profile-2000", NULL, fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  /* Inquire data type information for the attributes */
  /* ------------------------------------------------ */
  status = HE5_EHinqglbdatatype(fid, "GlobalAttribute_1", &datatype, &classid, &order, &size);
  printf("Status returned by HE5_EHinqglbdatatype() :  %d \n", status);
  if (status != FAIL)
        {
          printf("\tdatatype:    %d \n", datatype);
          printf("\tclass ID:    %d \n", classid);
          printf("\torder:       %d \n", order);
          printf("\tsize:        %d \n", (int)size);
        }

  status = HE5_EHinqglbdatatype(fid, "GLOBAL_DOUBLE_ATTR_1", &datatype, &classid, &order, &size);
  printf("Status returned by HE5_EHinqglbdatatype()->GLOBAL_DOUBLE_ATTR_1  :  %d \n", status);
  if (status != FAIL)
        {
          printf("\tdatatype:    %d \n", datatype);
          printf("\tclass ID:    %d \n", classid);
          printf("\torder:       %d \n", order);
          printf("\tsize:        %d \n", (int)size);
        }

          
  fieldgroup = HE5_HDFE_ATTRGROUP;
  status = HE5_SWinqdatatype(SWid, NULL, "GlobalAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  status = HE5_SWinqdatatype(SWid, NULL, "GLOBAL_CHAR_ATTR_11", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype()->GLOBAL_CHAR_ATTR_11 :   %d \n", status);
  if (status != FAIL)
        {
          printf("\tdatatype:    %d \n", datatype);
          printf("\tclass ID:    %d \n", classid);
          printf("\torder:       %d \n", order);
          printf("\tsize:        %d \n", (int)size);
        }

  status = HE5_SWinqdatatype(SWid, NULL, "GLOBAL_CHAR_ATTR", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype()->GLOBAL_CHAR_ATTR :   %d \n", status);
  if (status != FAIL)
        {
          printf("\tdatatype:    %d \n", datatype);
          printf("\tclass ID:    %d \n", classid);
          printf("\torder:       %d \n", order);
          printf("\tsize:        %d \n", (int)size);
        }

  fieldgroup = HE5_HDFE_GRPATTRGROUP;
  status = HE5_SWinqdatatype(SWid, NULL, "GroupAttribute", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_LOCATTRGROUP;
  status = HE5_SWinqdatatype(SWid, "Density", "LocalAttribute_1", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  fieldgroup = HE5_HDFE_LOCATTRGROUP;
  status = HE5_SWinqdatatype(SWid, "Longitude", "LocalAttribute_2", fieldgroup, &datatype, &classid, &order, &size);
  printf("Status returned by HE5_SWinqdatatype() :   %d \n", status);
  if (status != FAIL)
	{
	  printf("\tdatatype:    %d \n", datatype);
	  printf("\tclass ID:    %d \n", classid);
	  printf("\torder:       %d \n", order);
	  printf("\tsize:        %d \n", (int)size);        
	}

  /* Detach from the swath */
  /* --------------------- */
  status = HE5_SWdetach(SWid);
  printf("Status returned by HE5_SWdetach() :        %d \n", status);

  /* Close the file */
  /* -------------- */
  status = HE5_SWclose(fid);
  printf("Status returned by HE5_SWclose() :         %d \n", status);
    
  return(0);
    
}




