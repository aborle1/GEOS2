/*
 ----------------------------------------------------------------------------
 |    Copyright (C) 1999   Emergent IT Inc. and Raytheon Systems Company    |
 ----------------------------------------------------------------------------
 */



#include    <HE5_HdfEosDef.h>


/* In this program we (1) open the "Swath.h5" HDF-EOS file, (2) attach to  */
/*  the "Swath1", and (3) read data from the "Spectra" and "Time" fields   */
/* ----------------------------------------------------------------------- */

int main()
{
  herr_t          status = FAIL;

  int             i, j,  rank = 0;    

  hid_t           swfid = FAIL, SWid = FAIL;
  hid_t           regionID = FAIL, periodID = FAIL;

  hid_t           *ntype;

  size_t          size = 0;

  hsize_t         dims[8];

  double          cornerlon[2], cornerlat[2];
  double          *datbuf, start_time, stop_time;

    
  /* Open the HDF-EOS swath file, "Swath.h5" */
  /* --------------------------------------- */
  swfid = HE5_SWopen("Swath.h5", H5F_ACC_RDWR);
  if (swfid != FAIL)
    {
	  /* Attach to the "Swath1" swath */
	  /* ---------------------------- */
	  SWid = HE5_SWattach(swfid, "Swath1");
	  if (SWid != FAIL)
		{
		  cornerlon[0] = 3. ;
		  cornerlat[0] = 5. ;
		  cornerlon[1] = 7. ;
		  cornerlat[1] = 12.;

		  regionID = HE5_SWdefboxregion(SWid, cornerlon, cornerlat, HE5_HDFE_MIDPOINT);
		  printf("\n");
		  printf("Region ID returned by HE5_SWdefboxregion()  :          %d \n", regionID);
	    
		  ntype = (hid_t *)calloc(1, sizeof(hid_t) );

		  status = HE5_SWregioninfo(SWid, regionID, "Longitude", ntype, &rank, dims, &size);
		  printf("Status returned by HE5_SWregioninfo(\"Longitude\")  :  %d \n", status);

		  status = HE5_SWregioninfo(SWid, regionID, "Spectra", ntype, &rank, dims, &size);
		  printf("Status returned by HE5_SWregioninfo(\"Spectra\")  :    %d \n", status);           
 	    
		  datbuf = (double *) malloc(size);

		  status = HE5_SWextractregion(SWid, regionID, "Spectra", HE5_HDFE_INTERNAL, datbuf);
		  printf("Status returned by HE5_SWextractregion()  :            %d \n", status);
		  printf("\n");
		  printf("====================  DATA  ===================== \n");
		  printf("\n");
		  for (i = 0; i < size / sizeof(double); i++)
			printf("\t %lf \n", datbuf[i]);
	    	    
		  free(datbuf);

		  /* Time Subsetting */
		  /* --------------- */
		  start_time = 35232487.2;
		  stop_time  = 36609898.1;

		  periodID = HE5_SWdeftimeperiod(SWid, start_time, stop_time, HE5_HDFE_MIDPOINT);
		  printf("\n");
		  printf("Period ID returned by HE5_SWdeftimeperiod() :          %d \n",  periodID);
	    
		  status = HE5_SWperiodinfo(SWid, periodID, "Time", ntype, &rank, dims, &size);
		  printf("Status returned by HE5_SWperiodinfo() :                %d \n",  status);

		  datbuf = (double *) malloc(size);

		  status = HE5_SWextractperiod(SWid, periodID, "Time", HE5_HDFE_INTERNAL, datbuf);
		  printf("Status returned by HE5_SWextractperiod()  :            %d \n", status);

		  printf("\n");
		  printf("====================  DATA  ===================== \n");
		  printf("\n");
		  for (i = 0; i < size / sizeof(double); i++)
			printf("\t\t %lf \n", datbuf[i]);

		  free(datbuf);
		  free(ntype);	    
		}
    }

  status = HE5_SWdetach(SWid);
  status = HE5_SWclose(swfid);

  return 0;
}



