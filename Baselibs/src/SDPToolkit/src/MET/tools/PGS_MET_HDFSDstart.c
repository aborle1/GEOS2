/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/***************************************************************************
BEGIN_FILE_PROLOG:
 
FILENAME:
         PGS_MET_HDFSDstart.c 
 
DESCRIPTION:
         The file contains functions PGS_MET_SDstart andPGS_MET_SDend
         to open and close HDF files(HDF4 and HDF5 based).

AUTHOR:
        Abe Taaheri /Emergent Information Technologies, Inc.
 
HISTORY:
        
        20-Mar-2001 AT   Initial version
        24-Apr-2002 AT   Implemented new flags for HDF5/HDF4 files opening

END_FILE_PROLOG:
***************************************************************************/
/* include files */
#include <sys/types.h>
#include <time.h>
#include <PGS_MET.h>
#include <PGS_PC_Prototypes.h>
#include <hdf.h>
#include <mfhdf.h>
#include <hdf5.h>

/* for HP version */
#ifdef __hpux
#include <sys/param.h>
#include <sys/unistd.h>
#endif    

/* for DEC ALPHA version */
#ifdef __alpha
#include <sys/param.h>
#include <unistd.h>
#endif    

struct HDF5files                /* structure to hold HDF5 filenames 
				   and their file IDs */
{
  char file_name[PGSd_SMF_PATH_MAX];              /* file name */
  PGSt_integer file_id;                           /* file ID */
  PGSt_integer open_flag;       /* flag that indicates file is open (=1) or
				   closed (=0) */
} cat_HDF5[PGSd_MET_MAX_NUM_FILES];



int N_HDF5_FILES_OPENED;

PGSt_SMF_status
PGS_MET_SDstart(char *filename, PGSt_uinteger flags, PGSt_integer *HDFfid)  
{
  hid_t           HDF5fid;
  int32           HDF4fid;
  static int      count = 0;
  char            msg[PGS_SMF_MAX_MSGBUF_SIZE];  /* message buffer */
  PGSt_SMF_status returnStatus = PGS_S_SUCCESS;
  char *          funcName = "PGS_MET_SDstart";
  
  PGSt_integer		FILE_IS_HDF4 = 0;
  PGSt_integer		FILE_IS_HDF5 = 0;
  PGSt_integer	        FILE_NOT_HDF = 0;
  PGSt_boolean            HDFflag = PGS_FALSE;
  FILE                    *fp;
  PGSt_uinteger           HDF5_flags = H5F_ACC_RDONLY;
  PGSt_uinteger           HDF4_flags;

  /* Define Access flags recognizable by HDF4 or HDF5 */
  HDF4_flags = flags;
  if((flags == HDF5_ACC_RDONLY) || (flags == HE5F_ACC_RDONLY)) HDF5_flags = H5F_ACC_RDONLY;
  if((flags == HDF5_ACC_RDWR) || (flags == HE5F_ACC_RDWR)) HDF5_flags = H5F_ACC_RDWR;
  if((flags == HDF5_ACC_CREATE) || (flags == HE5F_ACC_TRUNC)) HDF5_flags = H5F_ACC_CREAT;
  if((flags == HDF4_ACC_RDONLY) || (flags == DFACC_RDONLY)) HDF4_flags = DFACC_RDONLY;
  if((flags == HDF4_ACC_RDWR) || (flags == DFACC_RDWR)) HDF4_flags = DFACC_RDWR;
  if((flags == HDF4_ACC_CREATE) || (flags == DFACC_CREATE)) HDF4_flags = DFACC_CREATE;

  /* Find out the file is of type HDF4, HDF5, or NONE_HDF */
  if((fp = fopen(filename, "r")) != NULL) /* file exist */
    {
      HDFflag = PGS_TRUE; 
      fclose(fp);
    }
  if( HDFflag == PGS_TRUE)
    {
      returnStatus = PGS_MET_HDFFileType( filename, &FILE_IS_HDF4,
					  &FILE_IS_HDF5, &FILE_NOT_HDF);
      if(returnStatus != PGS_S_SUCCESS)
	{
	  sprintf(msg, " Cannot determine whether the file (%s) is HDF4, HDF5, or NONE-HDF type. ", filename);
	  PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	  *HDFfid = -1;
	  return(returnStatus);
	}
      
      /* If file is not a HDF file, We cannot open it Since this function is
	 supposed to open HDF file */
      if( FILE_NOT_HDF == 1)
	{
	  returnStatus = PGSMET_E_SD_START;
	  sprintf(msg, " File (%s) that exists is not HDF type and will not be opened. ",filename);
	  PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	  *HDFfid = -1;
	  return(returnStatus);
	}
      /* if file is HDF5 type open it with HDF5 routines, otherwise open it
	 with HDF4 routines */
      if( FILE_IS_HDF5 == 1)
	{
	  (void) H5Eset_auto(NULL, NULL);
	  if(HDF5_flags == H5F_ACC_RDWR)
	    {
	      HDF5fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
	    }
	  else if (HDF5_flags == H5F_ACC_RDONLY)
	    {
	      HDF5fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
	    }
	  else if (HDF5_flags == H5F_ACC_CREAT)
	    {
	      (void) H5Eset_auto(NULL, NULL);
	      HDF5fid = H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
	    }

	  if(HDF5fid < 0 )
	    {
	      returnStatus = PGSMET_E_SD_START;
	      sprintf(msg, " Cannot open HDF5 file (%s). ",filename);
	      PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	      *HDFfid = -1;
	      return(returnStatus);
	    }
	  else
	    {
	      cat_HDF5[count].file_id = (PGSt_integer) HDF5fid;
	      strcpy(cat_HDF5[count].file_name, filename);
	      cat_HDF5[count].open_flag = 1;
	      N_HDF5_FILES_OPENED = count+1;
	      
	      count++;
	      *HDFfid = (PGSt_integer) HDF5fid;
	      return(returnStatus);
	    }
	}
      else if( FILE_IS_HDF4 == 1)
	{
	  if( HDF4_flags == DFACC_RDWR)
	    {
	      HDF4fid = SDstart( filename, DFACC_RDWR);
	    }
	  else if(HDF4_flags == DFACC_RDONLY)
	    {
	      HDF4fid = SDstart( filename, DFACC_RDONLY);
	    }
	  else if (HDF4_flags == DFACC_CREATE)
	    {
	      HDF4fid = SDstart( filename, DFACC_CREATE);
	    }
	  
	  if (HDF4fid == FAIL)
	    {
	      returnStatus = PGSMET_E_SD_START;
	      sprintf(msg, " Cannot open HDF4 file (%s). ",filename);
	      PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	      *HDFfid = -1;
	      return(returnStatus);
	    }
	  else
	    {
	      *HDFfid = (PGSt_integer) HDF4fid;
	      return(returnStatus);
	    }
	}
    }
  else   /* file does not exist */
    {
      /* open it if flag is DFACC_CREATE or H5F_ACC_CREAT */
      if(HDF5_flags == H5F_ACC_CREAT) /* Craete HDF5 based file */
	{
	  (void) H5Eset_auto(NULL, NULL);
	  HDF5fid = H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
	  if(HDF5fid < 0 )
	    {
	      returnStatus = PGSMET_E_SD_START;
	      sprintf(msg, " Cannot open HDF5 file (%s). ",filename);
	      PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	      *HDFfid = -1;
	      return(returnStatus);
	    }
	  else
	    {
	      cat_HDF5[count].file_id = (PGSt_integer) HDF5fid;
	      strcpy(cat_HDF5[count].file_name, filename);
	      cat_HDF5[count].open_flag = 1;
	      N_HDF5_FILES_OPENED = count+1;
	      
	      count++;
	      *HDFfid = (PGSt_integer) HDF5fid;
	      return(returnStatus);
	    }
	}
      else if (HDF4_flags == DFACC_CREATE) /* Craete HDF4 based file */
	{
	  HDF4fid = SDstart( filename, DFACC_CREATE);
	  if (HDF4fid == FAIL)
	    {
	      returnStatus = PGSMET_E_SD_START;
	      sprintf(msg, " Cannot open HDF4 file (%s). ",filename);
	      PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	      *HDFfid = -1;
	      return(returnStatus);
	    }
	  else
	    {
	      *HDFfid = (PGSt_integer) HDF4fid;
	      return(returnStatus);
	    }
	}
      else
	{
	  returnStatus = PGSMET_E_SD_START;
	  sprintf(msg, " Cannot create non-existing HDF file (%s). Access flag should be HDF5_ACC_CREATE or HE5F_ACC_TRUNC for HDF5, or HDF4_ACC_CREATE or DFACC_CREATE for HDF4. ",filename);
	  PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	  *HDFfid = -1;
	  return(returnStatus);
	}
    }
  return(returnStatus);
}






PGSt_SMF_status
PGS_MET_SDend(PGSt_integer HDFfid)  
{
  herr_t          status = -1;
  PGSt_SMF_status returnStatus = PGS_S_SUCCESS;
  char            msg[PGS_SMF_MAX_MSGBUF_SIZE];  /* message buffer */
  char *          funcName = "PGS_MET_SDend";
  intn		  hdfRet;
  int             filecount;
  PGSt_integer		FILE_IS_HDF4 = 0;
  PGSt_integer		FILE_IS_HDF5 = 0;

  /* If the hdfFileId belongs to a hdf5 file, then user must have used
     PGS_MET_SDstart() to get hdfFileId. In that case the hdfFileId
     and the corresponding file name exist in the cat_HDF5 structure.
     Otherwise, the hdfFileId is an ID for hdf4 file. So first we try
     to find hdfFileId in cat_HDF5 and close it using HDF5 routines, 
     if we still have hdfRet = FAIL after the search, we try hdf4 calls 
     to close the file. */
	     
  hdfRet = FAIL;
  for (filecount = 0; filecount < PGSd_MET_MAX_NUM_FILES; filecount++)
    {
      if((cat_HDF5[filecount].file_id) == HDFfid)
	{
	  hdfRet = SUCCEED;
	  FILE_IS_HDF4 = 0;    /* file is not HDF4 type */
	  FILE_IS_HDF5 = 1;    /* file is HDF5 type */
	  break;
	  
	}
      else
	{
	  continue;
	}
    }
  if( hdfRet == FAIL)
    {
	  hdfRet = SUCCEED;
	  FILE_IS_HDF4 = 1;   /* file is HDF4 type */
	  FILE_IS_HDF5 = 0;   /* file is not HDF5 type */
    }

  if(FILE_IS_HDF5 == 1) /* file is HDF5 type */
    {
      status = H5Fclose((hid_t)HDFfid);
      if( status != -1) /* file is closed; Set open_flag to 0 */
	{
	  /*  strcpy (cat_HDF5[filecount].file_name,"");*/

	  cat_HDF5[filecount].open_flag = 0;
	}
      if( status == -1)
	{
	  if(cat_HDF5[filecount].open_flag == 0)
	    {
	      /* file already closed; return PGS_S_SUCCESS */
	      return(PGS_S_SUCCESS);
	    }
	  /* may be the file is actually a HDF4 file with an Id the same as 
	     another HDF5 file. So let's try closing it with hdf4 routine. If
	     problem occurs then we report error */
	  returnStatus = SDend((int32) HDFfid);
	  if (returnStatus == FAIL)
	    {
	      returnStatus = PGSMET_E_SD_END;
	      sprintf(msg, " Cannot close the HDF file with ID (%d)", HDFfid);
	      PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	      return(returnStatus);
	    }
	}
      else
	{
	  return(PGS_S_SUCCESS);  
	}
    }
  else if(FILE_IS_HDF4 == 1) /* file is HDF4 type */
    {
      returnStatus = SDend((int32) HDFfid);
      if (returnStatus == FAIL)
	{
	  returnStatus = PGSMET_E_SD_END;
	  sprintf(msg, " Cannot close the HDF4 file with ID (%d)", HDFfid);
	  PGS_SMF_SetDynamicMsg(returnStatus, msg, funcName);
	  return(returnStatus);
	}
      else
	{
	  return(PGS_S_SUCCESS);  
	}
    }
  return(PGS_S_SUCCESS);
}


PGSt_SMF_status
PGS_MET_SDstartF(char *filename, PGSt_integer flags, PGSt_integer *HDFfid)  
{
  PGSt_SMF_status returnStatus = PGS_S_SUCCESS;
  returnStatus = PGS_MET_SDstart(filename, (PGSt_uinteger) flags, HDFfid);
  return(returnStatus);
}

