/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include <HE5_HdfEosDef.h>


int main()
{
  herr_t      status = FAIL;

  int         attr       = 9999;                  /* Global attribute */     
  int         attr1[ 5 ] = { 1, 2, 3, 4, 5};      /* Group attribute  */   
  int         attr2[ 5 ] = { 10, 20, 30, 40, 50}; /* Local attribute  */

  hid_t       ptfid = FAIL, PTid = FAIL;

  hsize_t     count[8];
 
  float       flt = -7.5;                         /* Global attribute */
    


  /* Open the HDF-EOS file, "Point.h5" */
  /* --------------------------------- */
  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDWR);
  printf("File ID returned by HE5_PTopen() :         %d \n", ptfid);

  /* Write attribute to "Fixed Buoy Point" */
  /* ------------------------------------- */
  PTid = HE5_PTattach(ptfid, "FixedBuoy Point");
  printf("Point ID returned by HE5_PTattach() :      %d \n", PTid);

  count[0] = 1;
  status = HE5_PTwriteattr(PTid, "GlobalAttribute_int", H5T_NATIVE_INT, count, &attr);
  printf("Status returned by HE5_PTwriteattr() :     %d \n", status);

  count[0] = 5;
  status = HE5_PTwritegrpattr(PTid, "GroupAttribute", H5T_NATIVE_INT, count, attr1);
  printf("Status returned by HE5_PTwritegrpattr() :  %d \n", status);

  status = HE5_PTwritelocattr(PTid, "Observations", "LocalAttribute", H5T_NATIVE_INT, count, attr2);
  printf("Status returned by HE5_PTwritelocattr() :  %d \n", status);

  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :        %d \n", status);
    
  /* Write attributes to "Floating Buoy Point" */
  /* ----------------------------------------- */
  PTid = HE5_PTattach(ptfid, "FloatBuoy Point");
  printf("Point ID returned by HE5_PTattach() :      %d \n", PTid);

  count[0] = 1;    
  status = HE5_PTwriteattr(PTid, "GlobalAttribute_float", H5T_NATIVE_FLOAT, count, &flt);
  printf("Status returned by HE5_PTwriteattr() :     %d \n", status);    

  status = HE5_PTdetach(PTid);
  printf("Status returned by HE5_PTdetach() :        %d \n", status);

  status = HE5_PTclose(ptfid);
  printf("Status returned by HE5_PTclose() :         %d \n", status);

  return 0;
}






