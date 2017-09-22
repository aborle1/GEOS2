/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:
   PGS_TD_TAItoUT1pole.c

DESCRIPTION:
   This file contains the function PGS_TD_TAItoUT1pole()
   This tool converts continuous seconds since 12AM UTC 1-1-93 to UT1 time as
   a Julian date and returns x and y polar wander values and UT1-UTC as well.


AUTHOR:
   Guru Tej S. Khalsa / Applied Research Corporation
   Peter Noerdlinger  / Applied Research Corporation

HISTORY:
   15-Sep-1994  GTSK  Initial version
   13-Jun-1995  GTSK  Fixed up prolog
   21-May-1997  PDN   Fixed up comments
   22-May-1997  PDN   Deleted branch and references for _W_PRED_LEAPS

END_FILE_PROLOG:
*******************************************************************************/

/*******************************************************************************
BEGIN_PROLOG:

TITLE:    
   Get UT1 Julian date, X and Y Polar Wander and UT1-UTC Value

NAME:     
   PGS_TD_TAItoUT1pole()

SYNOPSIS:
 C:
   #include <PGS_TD.h>

   PGSt_SMF_status
   PGS_TD_TAItoUT1pole(
       PGSt_double  secTAI93,
       PGSt_double  jdUT1[2],
       PGSt_double  *xpole,
       PGSt_double  *ypole,
       PGSt_double  *diffUT1UTC,
       PGSt_double  *jdtable)

 FORTRAN:
      include 'PGS_SMF.f'
      include 'PGS_CSC_4.f'
      include 'PGS_TD_3.f'
   
      integer function pgs_td_taitout1pole(sectai93,jdut1,xpole,ypole,
     >                                     diffut1utc,jdtable)
      double precision  sectai93
      double precision  jdut1(2)
      double precision  xpole
      double precision  ypole
      double precision  diffut1utc
      double precision  jdtable


DESCRIPTION:
   This tool converts continuous seconds since 12AM UTC 1-1-93 to UT1 time as
   a Julian date and returns x and y polar wander values and UT1-UTC as well.

INPUTS:
   Name         Description                Units    Min           Max
   ----         -----------                -----    ---           ---
   secTAI93     continuous seconds since   seconds  -426297609.0  see NOTES
                12AM UTC Jan 1, 1993

OUTPUTS:
   Name         Description                Units    Min           Max
   ----         -----------                -----    ---           ---
   jdUT1        UT1 Julian date as two     days     N/A           N/A
                real numbers, the first
                a half integer number of
                days and the second the
                fraction of a day between
                this half integer number
                of days and the next half
                integer day number.
    xpole       x pole position            arc-sec
    ypole       y pole position            arc-sec
    diffUT1UTC  UT1 - UTC                  seconds
    jdtable     Julian Date of pole        days
                positions and times in
                table

RETURNS:
   PGS_S_SUCCESS               successful return 
   PGSCSC_W_PREDICTED_UT1      status of UT1-UTC correction is predicted
   PGSTD_E_NO_LEAP_SECS        no leap seconds correction available for
                               input time
   PGSCSC_W_DATA_FILE_MISSING  the file utcpole.dat could not be opened
   PGSTD_E_NO_UT1_VALUE        no UT1-UTC correction available
   PGS_E_TOOLKIT               something radically wrong occurred

EXAMPLES:
   None

NOTES:
   TIME ACRONYMS:
     
     TAI is:  International Atomic Time
     UT1 is:  Universal Time


   TIME BOUNDARIES for TRANSFORMATIONS BETWEEN UTC and TAI:

     The minimum and maximum times that can successfully be processed by 
     this function depend on the file "leapsec.dat" which relates leap 
     second (TAI-UTC) values to UTC Julian dates. The file "leapsec.dat" 
     contains dates of new leap seconds and the total leap seconds for 
     times on and after Jan 1, 1972.  For times between Jan 1, 1961 and 
     Jan 1, 1972 it contains coefficients for an approximation supplied 
     by the International Earth Rotation Service (IERS) and the United 
     States Naval Observatory (USNO).  The Toolkit then uses these 
     coefficients in an algorithm consistent with IERS/USNO usage. For 
     times after Jan 1, 1961, but before the last date in the file, the
     Toolkit sets TAI-UTC equal to the total number of leap seconds to 
     date, (or to the USNO/IERS approximation, for dates before Jan 1,
     1972). If an input date is before Jan 1, 1961 the Toolkit sets the
     leap seconds value to 0.0.  This is consistent with the fact that,
     for civil timekeeping since 1972, UTC replaces Greenwich Mean Solar 
     Time (GMT), which had no leap seconds. Thus for times before Jan 1, 
     1961, the user can, for most Toolkit-related purposes, encode 
     Greenwich Mean Solar Time as if it were UTC.  If an input date
     is after the last date in the file, or after Jan 1, 1961, but the 
     file cannot be read, the function will use a calculated value of 
     TAI-UTC based on a linear fit of the data known to be in the table
     as of early 1997.  This value is a crude estimate and may be off by 
     as much as 1.0 or more seconds.  If the data file, "leapsec.dat", 
     cannot be opened, or the time is outside the range from Jan 1, 1961 
     to the last date in the file, the return status level will be 'E'.  
     Even when the status level is 'E', processing will continue, using 
     the default value of TAI-UTC (0.0 for times before Jan 1, 1961, or 
     the linear fit for later times). Thus, the user should always 
     carefully check the return status.  

     The file "leapsec.dat" is updated when a new leap second is 
     announced by the IERS, which has been, historically, about once 
     every year or two. 


   TIME BOUNDARIES for TRANSFORMATIONS BETWEEN UT1 and OTHER TIMES:

     The minimum and maximum times that can successfully be processed by this
     function depend on the file utcpole.dat which relates UT1 - UTC values to
     UTC dates.  The file "utcpole.dat" starts at Jan 1, 1972; therefore,
     the correction UT1 - UTC, which can be as large as (+-) 0.9 s, is not
     available until that date, and an error status message will be returned if
     this function is called with a time before that date.  The file
     utcpole.dat, which is maintained periodically, contains final (definitive),
     and predicted values (partially reduced data and short term predictions of 
     good accuracy).  The predictions run about a year ahead of real time. By
     that time, the error in UT1 is generally equivalent to 10 to 12 meters of
     equivalent Earth surface motion. Thus, when the present function is used,
     users should carefully check the return status.  A success status 
     message will be returned if all input times correspond to final values.
     A warning status message will be returned if predicted values are
     encountered.  An error message will be returned if the time requested is
     beyond the end of the predictions, or the file cannot be read.

   TOOLKIT INTERNAL TIME (TAI):

     Toolkit internal time is the real number of continuous SI seconds since the
     epoch of UTC 12 AM 1-1-1993.  Toolkit internal time is also referred to in
     the toolkit as TAI (upon which it is based).

   JULIAN DATES:

     Format:

       Toolkit Julian dates are kept as an array of two real (high precision)
       numbers (C: PGSt_double, FORTRAN: DOUBLE PRECISION).  The first element
       of the array should be the half integer Julian day (e.g. N.5 where N is a
       Julian day number).  The second element of the array should be a real
       number greater than or equal to zero AND less than one (1.0) representing
       the time of the current day (as a fraction of that (86400 second) day.
       This format allows relatively simple translation to calendar days (since
       the Julian days begin at noon of the corresponding calendar day).  Users
       of the Toolkit are encouraged to adhere to this format to maintain high
       accuracy (one number to track significant digits to the left of the
       decimal and one number to track significant digits to the right of the
       decimal).  Toolkit functions that do NOT require a Julian type date as an
       input and return a Julian date will return the Julian date in the above
       mentioned format.  Toolkit functions that require a Julian date as an
       input and do NOT return a Julian date will first convert (internally) the
       input date to the above format if necessary.  Toolkit functions that have
       a Julian date as both an input and an output will assume the input is in
       the above described format but will not check and the format of the
       output may not be what is expected if any other format is used for the
       input.

     Meaning:

       Toolkit "Julian dates" are all derived from UTC Julian Dates.  A Julian
       date in any other time stream (e.g. TAI, TDT, UT1, etc.) is the UTC
       Julian date plus the known difference of the other stream from UTC
       (differences range in magnitude from 0 seconds to about a minute).

       Examples:

         In the following examples, all Julian Dates are expressed in Toolkit
         standard form as two double precision numbers. For display here, the
         two members of the array are enclosed in braces {} and separated by a
         comma.

         A) UTC to TAI Julian dates conversion

         The Toolkit UTC Julian date for 1994-02-01T12:00:00 is: 
         {2449384.50, 0.5}.
         TAI-UTC at 1994-02-01T12:00:00 is 28 seconds (.00032407407407 days). 
         The Toolkit TAI Julian date for 1994-02-01T12:00:00 is:
         {2449384.50, 0.5 + .00032407407407} = {2449384.50, 0.50032407407407}

         Note that the Julian day numbers in UTC and the target time stream may
         be different by + or - 1 for times near midnight:

         B) UTC to UT1 Julian dates conversion

         The Toolkit UTC Julian date for 1994-04-10T00:00:00 is: 
         {2449452.50, 0.0}.
         UT1-UTC at 1994-04-10T00:00:00 is -.04296 seconds 
         (-0.00000049722221 days).  The Toolkit UT1 Julian date for
         1994-04-10T00:00:00 is:
         {2449452.50, 0.0 - 0.0000004972222} = 
         {2449452.50, -0.0000004972222} =
         {2449451.50, 0.9999995027778}

   REFERENCES FOR TIME:

     CCSDS 301.0-B-2 (CCSDS => Consultative Committee for Space Data Systems) 
     Astronomical Almanac, Explanatory Supplement to the Astronomical Almanac

REQUIREMENTS:
   PGSTK - 1170, 1210

DETAILS:
   This tool converts TAI time to UT1 time.  Input is in seconds since 12AM UTC
   January 1st, 1993.  This function calls PGS_TD_TAItoUTCjd() to convert the
   input TAI time to UTC Julian date.  PGS_CSC_UTC_UT1Pole() is then called
   to get the UT1-UTC correction which is added to the value of the UTC Julian
   date (along with the leap second if one is occurring).

GLOBALS:  
   None

FILES:
   None

FUNCTIONS_CALLED:
   PGS_TD_TAItoTAIjd()
   PGS_TD_TAIjdtoUTCjd()
   PGS_CSC_UTC_UT1Pole()
   PGS_SMF_SetStaticMsg()
   PGS_SMF_SetDynamicMsg()
   PGS_SMF_SetUnknownMsg()

END_PROLOG:
*******************************************************************************/

#include <PGS_TD.h>
#include <PGS_CSC.h>

/* conversion constants */

#define SECONDSperDAY     86400.0 /* number of seconds in a day */

/* name of this function */

#define FUNCTION_NAME "PGS_TD_TAItoUT1pole()"

PGSt_SMF_status
PGS_TD_TAItoUT1pole(              /* converts TAI time to UT1 time */
    PGSt_double  secTAI93,        /* TAI time (seconds) */
    PGSt_double  jdUT1[2],        /* UTC Julian date equivalent of input time */
    PGSt_double  *xpole,          /* x pole position in seconds of arc */
    PGSt_double  *ypole,          /* y pole position in seconds of arc */
    PGSt_double  *diffUT1UTC,     /* difference in UT1 and UTC in seconds */
    PGSt_double  *jdtable)        /* Julian Date of pole positions and times in
				     table (units: days) */
{
    PGSt_double  jdUTC[2];        /* UTC Julian date equivalent of input time */
    PGSt_double  onLeap = 0.0;    /* in a leap second (= 1.) or not (= 0.) */

    char         mnemonic[PGS_SMF_MAX_MNEMONIC_SIZE]; /* mnemonic 
							 returned by
							 PGS_SMF_GetMsg()*/
    char         msg[PGS_SMF_MAX_MSG_SIZE];      /* message returned by
						    PGS_SMF_GetMsg() */

    PGSt_SMF_status   returnStatus;  /* value returned by
					this function */
    PGSt_SMF_status   returnStatus1; /* value returned by calls
					to PGS functions */
    PGSt_SMF_status   code;          /* status code returned
					by PGS_SMF_GetMsg() */

    returnStatus = PGS_S_SUCCESS;
    returnStatus1 = PGS_S_SUCCESS;

    /* Get jdUTC from input TAI time. */

    PGS_TD_TAItoTAIjd(secTAI93,jdUTC);

    returnStatus = PGS_TD_TAIjdtoUTCjd(jdUTC,jdUTC);

    switch(returnStatus)
    {
      case PGS_S_SUCCESS:
      case PGSTD_E_NO_LEAP_SECS:
	break;
      case PGSTD_M_LEAP_SEC_IGNORED:
	onLeap = 1.0;
	break;
      default:
	PGS_SMF_SetUnknownMsg(returnStatus,FUNCTION_NAME);
	return PGS_E_TOOLKIT;
    }
    
    /*  Get the difference in UT1 and UTC on Julian date jdUTC */
    
    returnStatus1 = PGS_CSC_UTC_UT1Pole(jdUTC,xpole,ypole,diffUT1UTC,jdtable);
    
    switch(returnStatus1)
    {
      case PGS_S_SUCCESS:
	if (returnStatus == PGSTD_M_LEAP_SEC_IGNORED)
	    returnStatus = returnStatus1;
	else if (returnStatus != PGS_S_SUCCESS)
	    PGS_SMF_SetStaticMsg(returnStatus,FUNCTION_NAME);
	break;
      case PGSCSC_W_PREDICTED_UT1:
	if (returnStatus != PGSTD_E_NO_LEAP_SECS)
	    returnStatus = returnStatus1;
	else
	    PGS_SMF_SetStaticMsg(returnStatus,FUNCTION_NAME);
	break;
      case PGSCSC_E_INACCURATE_UTCPOLE:
      case PGSCSC_W_JD_OUT_OF_RANGE:
	PGS_SMF_GetMsg(&code,mnemonic,msg);
	if(code != returnStatus1)
	    PGS_SMF_GetMsgByCode(returnStatus1,msg);
        returnStatus = PGSTD_E_NO_UT1_VALUE;
	PGS_SMF_SetDynamicMsg(returnStatus,msg,FUNCTION_NAME);
        break;
      case PGSCSC_W_DATA_FILE_MISSING:
      case PGS_E_TOOLKIT:
	return returnStatus1;
      default:
	PGS_SMF_SetUnknownMsg(returnStatus1,FUNCTION_NAME);
	return PGS_E_TOOLKIT;
    }

    /* Calculate jdUT1 by adding UT1-UTC to jdUTC (also add in leap second if
       input time is occurring during a leap second. */
    
    jdUT1[0] = jdUTC[0];
    jdUT1[1] = jdUTC[1] + (*diffUT1UTC + onLeap)/SECONDSperDAY;
    
    /* adjust jdUT1 parts if the UT1-UTC correction put it into the next Julian
       day or the previous Julian day */
    
    if (jdUT1[1] < 0.0)
    {
	jdUT1[1] += 1.0;
	jdUT1[0] -= 1.0;
    }

    /* IMPORTANT: this test must not be in an else clause dependent on the
       failure of the above test.  Computer roundoff error may cause the above
       test to succeed and then set jdUT1 to 1.0 which is undesireable. */
 
    if(jdUT1[1] >= 1.0)
    {
	jdUT1[1] -= 1.0;
	jdUT1[0] += 1.0;
    }
    
    return   returnStatus;                   /* value returned by
						this function */
}
