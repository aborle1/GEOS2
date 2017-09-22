/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */


/* File name: he5_pt_setup.c  */
/* -------------------------- */


#include <HE5_HdfEosDef.h>


int main()
{
  herr_t  status = FAIL;

  hid_t   ptfid  = FAIL;
  hid_t   PTid1  = FAIL;
  hid_t   PTid2  = FAIL;
  hid_t   PTid3  = FAIL;

  /*
   * We first open the HDF-EOS point file, "Point.h5". Because this file
   * does not already exist, we use the H5F_ACC_TRUNC access code in the
   * open statement.  The PTopen routine returns the point file id, ptfid,
   * which is used to identify the file in subsequent routines in the
   * library.
   */

  ptfid = HE5_PTopen("Point.h5", H5F_ACC_TRUNC);

  PTid1 = HE5_PTcreate(ptfid, "Simple Point");
  PTid2 = HE5_PTcreate(ptfid, "FixedBuoy Point");
  PTid3 = HE5_PTcreate(ptfid, "FloatBuoy Point");

  /*
   * We now close the point interface with the HE5_PTdetach routine.  This step
   * is necessary to properly store the point information within the file.
   */

  status = HE5_PTdetach(PTid1);
  status = HE5_PTdetach(PTid2);
  status = HE5_PTdetach(PTid3);

  /*
   * Finally, we close the swath file using the PTclose routine.  This will
   * release the point file handles established by PTopen.
   */

  status = HE5_PTclose(ptfid);
  
  return 0;
}


