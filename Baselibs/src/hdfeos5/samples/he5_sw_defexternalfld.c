/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */

#include    <HE5_HdfEosDef.h>


/* ----------------------------------------------------------- */
/*   In this program we (1) open the "Swath.h5" HDF-EOS file,  */
/*   (2) attach to the "Swath1" swath, and (3) define the      */
/*   external data field "ExtData".                            */
/* ----------------------------------------------------------- */

int main()
{
  
  herr_t      status = FAIL;
  
  hid_t       swfid = FAIL;
  hid_t       SWid  = FAIL;

  off_t       offset[3];

  hsize_t     size[3];
  

  /* Open the file, "Swath.h5", using the H5F_ACC_RDWR access code */
  /* ------------------------------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
    {
	  /* Attach to the "Swath1" swath */
	  /* ---------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1"); 
	  if (SWid != FAIL)
		{
		  /* Set the data sizes and offsets in external files */
		  /* ------------------------------------------------ */
		  size[0] = 10 * sizeof(int);       offset[0] = 0;
		  size[1] = 20 * sizeof(int);       offset[1] = 40;
		  size[2] = 30 * sizeof(int);       offset[2] = 80;

		  /* Set external data files first */
		  /* ----------------------------- */
		  status = HE5_SWsetextdata(SWid, "external_1.data,external_2.data,external_3.data", offset, size);
		  printf("Status returned by HE5_SWsetextdata(...) :                        %d\n",status);

		  /* Define field containing external data */
		  /* ------------------------------------- */
		  status = HE5_SWdefdatafield(SWid, "ExtData", "ExtDim", NULL, H5T_NATIVE_INT, 0);
		  printf("Status returned by HE5_SWdefdatafield(...\"ExtData\",...) :         %d\n",status);		  
		}
    }

  /* Detach from the swath */
  /* --------------------- */
  status = HE5_SWdetach(SWid);
  printf("Status returned by HE5_SWdetach(...) :                            %d\n",status);

  /* Close the file */
  /* -------------- */
  status = HE5_SWclose(swfid);
  printf("Status returned by HE5_SWclose(...) :                             %d\n",status);
  
  return 0;
}





