/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


#include <HE5_HdfEosDef.h>

int main()
{
  herr_t          status = FAIL;

  hid_t           ptfid = FAIL;
  hid_t           PTid1 = FAIL;

  hssize_t        recs[32];
    
  hsize_t         nrec;

  typedef struct 
	{
	  double    Time;
	  float     Conc[4];
	  char      Spc[8];
	} CmpData_1;

  CmpData_1  buf_1;

  buf_1.Time    = 13131313.0;

  buf_1.Conc[0] = 1.11;
  buf_1.Conc[1] = 2.22;
  buf_1.Conc[2] = 3.33;
  buf_1.Conc[3] = 4.44;

  strcpy(buf_1.Spc,"AM");

  /* -------------------------------------------------------------- */
  /* NOTE: To update all records, set "nrec" => 0 or "recs" => NULL */
  /*       the data buffer should be properly populated             */
  /* -------------------------------------------------------------- */

  /* Open the HDF-EOS file, "Point.h5" */
  /* --------------------------------- */
  ptfid = HE5_PTopen("Point.h5", H5F_ACC_RDWR);
  if (ptfid != FAIL)
    {
	  PTid1 = HE5_PTattach(ptfid, "Simple Point");
	  if (PTid1 != FAIL)
		{
		  nrec    = 1;
		  recs[0] = 0;
		  status = HE5_PTupdatelevel(PTid1, 0, "Concentration", nrec, recs, &buf_1);
		  printf("Status returned by HE5_PTupdatelevel() :  %d \n", status);
		  
		}

	  status = HE5_PTdetach(PTid1);
	  printf("Status returned by HE5_PTdetach() :       %d \n", status);
    }

  status = HE5_PTclose(ptfid);
    
  return 0;
}







