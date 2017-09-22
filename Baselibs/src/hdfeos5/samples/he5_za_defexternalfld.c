/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 2002   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include    <HE5_HdfEosDef.h>


/* --------------------------------------------------------- */
/*   In this program we (1) open the "ZA.he5" HDF-EOS file,   */
/*   (2) attach to the "ZA1" za, and (3) define the external */
/*   data field "ExtData".                                   */
/* --------------------------------------------------------- */

int main()
{
  
  herr_t      status = FAIL;
  
  hid_t       zafid = FAIL;
  hid_t       ZAid  = FAIL;

  off_t       offset[3];

  hsize_t     size[3];
  

  /* Open the file, "ZA.he5", using the H5F_ACC_RDWR access code */
  /* ---------------------------------------------------------- */
  zafid = HE5_ZAopen("ZA.he5", H5F_ACC_RDWR);
  if (zafid != FAIL)
    {
	  /* Attach to the "ZA1" za */
	  /* ---------------------- */
	  ZAid = HE5_ZAattach(zafid, "ZA1"); 
	  if (ZAid != FAIL)
		{
		  /* Set the data sizes and offsets in external files */
		  /* ------------------------------------------------ */
		  size[0] = 10 * sizeof(int);       offset[0] = 0;
		  size[1] = 20 * sizeof(int);       offset[1] = 40;
		  size[2] = 30 * sizeof(int);       offset[2] = 80;

		  /* Set external data files first */
		  /* ----------------------------- */
		  status = HE5_ZAsetextdata(ZAid, "external_1.data,external_2.data,external_3.data", offset, size);
		  printf("Status returned by HE5_ZAsetextdata(...) :                        %d\n",status);

		  /* Define field containing external data */
		  /* ------------------------------------- */
		  status = HE5_ZAdefine(ZAid, "ExtData", "ExtDim", NULL, H5T_NATIVE_INT);
		  printf("Status returned by HE5_ZAdefine(...\"ExtData\",...) :         %d\n",status);		  
		}
    }

  /* Detach from the za */
  /* ------------------ */
  status = HE5_ZAdetach(ZAid);
  printf("Status returned by HE5_ZAdetach(...) :                            %d\n",status);

  /* Close the file */
  /* -------------- */
  status = HE5_ZAclose(zafid);
  printf("Status returned by HE5_ZAclose(...) :                             %d\n",status);
  
  return 0;
}





