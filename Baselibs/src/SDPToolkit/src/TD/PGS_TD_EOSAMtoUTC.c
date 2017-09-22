/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:   
   PGS_TD_EOSAM_to_UTC.c

DESCRIPTION:
   This function converts EOS AM spacecraft clock time in CCSDS
   day segmented Time Code (CDS) (with implicit P-field)
   format to UTC in CCSDS ASCII time code A format.
 
AUTHOR:
   Guru Tej S. Khalsa / Applied Research Corp.

HISTORY:
   31-May-1994  GTSK  Initial version

END_FILE_PROLOG:
*******************************************************************************/

/*******************************************************************************
BEGIN_PROLOG:

TITLE:
   Convert EOS AM Clock Time to UTC Time

NAME:
   PGS_TD_EOSAMtoUTC()

SYNOPSIS:
 C:
   #include PGS_TD.h

   PGSt_SMF_status
   PGS_TD_EOSAMtoUTC(
       PGSt_scTime   scTime[8],
       char          *asciiUTC)

 FORTRAN:
      include 'PGS_TD_3.f'
      include 'PGS_SMF.f'

      integer function pgs_td_eosamtoutc(sctime,asciiutc)
      character*8  sctime
      character*27 asciiutc

DESCRIPTION:
   This function converts EOS AM spacecraft clock time in
   platform-dependent format to UTC in CCSDS ASCII time code A format.
 
INPUTS:
   NAME      DESCRIPTION                  UNITS    MIN          MAX
   ----      -----------                  -----    ---          ---
   scTime    EOS AM clock time in CCSDS     ** see NOTES below **
             segmented time code
	     format (CDS).  

OUTPUTS:
   NAME      DESCRIPTION                  UNITS    MIN          MAX
   ----      -----------                  -----    ---          ---
   asciiUTC  UTC to equivalent CCSDS      ASCII    1958-01-01   2137-06-05
             ASCII Time Code (format A)                         T23:59:60.999999

RETURNS:
   PGS_S_SUCCESS               successful execution
   PGSTD_E_MILSEC_TOO_BIG      millisecond field too large (>=86401000)
   PGSTD_E_MICSEC_TOO_BIG      microsecond field too large (>=1000)

EXAMPLES:
   N/A

NOTES:
   TIME ACRONYMS:
     
     UTC is:  Coordinated Universal Time

   EOS AM TIME FORMAT:

     For EOS-AM, the input spacecraft (s/c) clock time scTime is a 63-bit value
     in CCSDS Day Segmented (CDS) format (Almost.  See Note below.), which is
     comprised of three binary counter values:
                 
     ("scTime" represents an array of 8 (unsigned) one byte elements)
          
     1)  A 15-bit value (the first two elements of array
         scTime each with 8 bits excluding the first bit of element 1
         which is not used for time, see Note below) containing the
         number of days since an epoch of January 1, 1958.  The range of 
         decimal values is 0-32767, computed as 256*element1
         +element2. The maximum decimal values of elements 1
         and 2 are 127 and 255 respectively.
     2)  A 32-bit value (elements 3,4,5 and 6 of array
         scTime,each with 8 bits) containing the milliseconds
         of the day. The range of values is 0-86400999 
         milliseconds, computed as 256*256*256*element3+
         256*256*element4+256*element5+element6. The maximum
         decimal values of elements 3,4,5 and 6 are 5,38,95
         and 231 respectively.
     3)  A 16-bit value (elements 7 and 8 of array
         scTime, each with 8 bits) containing the number of 
         microseconds of milliseconds.  The range of values
         is 0-999 microseconds, computed as 256*element6
         +element7. The maximum values of elements 6 and 7
          are 3 and 231.
       
     Note: this function expects an input array of 8 unsigned characters
     (PGSt_scTime).  This is 64 bits.  The first bit in the first byte of the
     return value is actually the secondary header ID flag which is always set
     to zero by EOS AM.  This makes the s/c time format used by EOS AM a
     variation of the CDS format (not supported by CCSDS).

   ASCII UTC:

     Toolkit functions that accept an ASCII time as input require the time to
     be UTC time in CCSDS ASCII Time Code A or CCSDS ASCII Time Code B format.
     Toolkit functions that return an ASCII time return the UTC time in CCSDS
     ASCII Time Code A format (see CCSDS 301.0-B-2 for details).
     (CCSDS => Consultative Committee for Space Data Systems)

      The general format is:
 
          YYYY-MM-DDThh:mm:ss.ddddddZ (Time Code A)
          YYYY-DDDThh:mm:ss.ddddddZ   (Time Code B)
 
          where:
              -,T,: are field delimiters
              Z is the (optional) time code terminator
              other fields are numbers as implied:
                Time Code A:
                   4 Year digits, 2 Month, 2 Day, 2 hour,
                   2 minute, 2 second, and up to 6 decimal
                   digits representing a fractional second
                Time Code B:
                   4 Year digits, 3 Day of year, 2 hour,
                   2 minute, 2 second, and up to 6 decimal
                   digits representing a fractional second

   REFERENCES FOR TIME:

     CCSDS 301.0-B-2 (CCSDS => Consultative Committee for Space Data Systems) 
     Astronomical Almanac, Explanatory Supplement to the Astronomical Almanac

REQUIREMENTS:  
   PGSTK-1160, PGSTK-1170


GLOBALS:
   No globals accessed

FILES:
   N/A


FUNCTIONS_CALLED:
   PGS_TD_calday()

END_PROLOG:
*******************************************************************************/

#include <stdio.h>
#include <PGS_TD.h>

/* constants */

#define EPOCH_DATE          2436205U   /* julian date Jan 1, 1958 */
#define MILLISECperDAY      86400000U  /* milliseconds in one day  */
#define MILLISECperHOUR     3600000U   /* milliseconds in one hour */
#define MILLISECperMINUTE   60000U     /* milliseconds in one min  */

/* name of this function */

#define FUNCTION_NAME "PGS_TD_EOSAMtoUTC()"

PGSt_SMF_status
PGS_TD_EOSAMtoUTC(              /* converts EOS AM s/c time to ASCII UTC */
    PGSt_scTime   scTime[8],    /* EOS AM s/c time in CCSDS unseg. time code */
    char          *asciiUTC)    /* equivalent UTC time in CCSDS ASCII Time Code
				   A format */
{
    PGSt_uinteger days;         /* days since epoch */
    PGSt_uinteger milsec;       /* millisecond of day */
    PGSt_uinteger micsec;       /* microsecond of millisecond */
			      
    PGSt_uinteger leapSec;      /* leap second holder */
	      		      
    PGSt_integer year;          /* calendar year */
    PGSt_integer month;         /* calendar month */
    PGSt_integer day;           /* calendar day */
    	      		      
    unsigned int  hours;        /* hour of day */
    unsigned int  minutes;      /* minute of hour */
    unsigned int  seconds;      /* second of minute */
    unsigned int  microSeconds; /* micro-second of second */
    
    
    days = (scTime[0]*256U) + scTime[1];
    milsec = ((scTime[2]*256U + scTime[3])*256U + scTime[4])*256U
             + scTime[5];
    micsec = (scTime[6]*256U) + scTime[7];

    if (milsec >= 86401000)
    {
	/* Size of Millisec should be < (a day + 1 second) since this value
	   represents milliseconds of the day in the day field its resolution
	   should not be as large as a day (plus a second in case of leap
	   seconds) */

	PGS_SMF_SetStaticMsg(PGSTD_E_MILSEC_TOO_BIG, FUNCTION_NAME);
	return PGSTD_E_MILSEC_TOO_BIG;
    }
    if (micsec >= 1000)
    {
	/* Size of micsec is equal to or larger than a millisec, microseconds
	   field should be less than 1000 (which is an entire millisec and
	   should be accounted for in the milliseconds field) */ 

	PGS_SMF_SetStaticMsg(PGSTD_E_MICSEC_TOO_BIG, FUNCTION_NAME);
	return PGSTD_E_MICSEC_TOO_BIG;
    }

    PGS_TD_calday((days+EPOCH_DATE),&year,&month,&day);
    
    if(milsec >= 86400000)
      leapSec = 1000;
    else
      leapSec = 0;
    
    hours = (milsec-leapSec)/MILLISECperHOUR;
    minutes   = ((milsec-(hours*MILLISECperHOUR)-leapSec))/MILLISECperMINUTE;
    seconds = (milsec - hours*MILLISECperHOUR - minutes*MILLISECperMINUTE)/1000;
    microSeconds = (milsec - hours*MILLISECperHOUR - minutes*MILLISECperMINUTE -
		    seconds*1000)*1000 + micsec;

    sprintf(asciiUTC,"%4d-%02d-%02dT%02u:%02u:%02u.%06uZ",
	    (int) year,
	    (int) month,
	    (int) day,
	    hours,
	    minutes,
            seconds,
	    microSeconds);

    return PGS_S_SUCCESS;
}

