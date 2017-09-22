/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  COPYRIGHT[copyright mark] 2000, Raytheon System Company, its vendors,  */
/*  and suppliers.  ALL RIGHTS RESERVED.                                   */
/*                                                                         */
/*-------------------------------------------------------------------------*/
/*******************************************************************************
BEGIN_FILE_PROLOG:

FILENAME:
   PGS_CSC_TODtoJ2000.c

DESCRIPTION:
   This file contains the function PGS_CSC_TODtoJ2000()
   
AUTHOR:
   Peter Noerdlinger / Applied Research Corporation
   Guru Tej S Khalsa / Applied Research Corporation

HISTORY:
   22-Mar-1995 PDN  Initial Version
   23-Mar-1995 GTSK Code rewritten
   06-Jun-1995 GTSK Rewrote (again)
   12-Jul-1995 PDN  Fixed prolog
                                         
END_FILE_PROLOG:
*******************************************************************************/
/*******************************************************************************
BEGIN_PROLOG

TITLE:  
   Transform from ECI True of Date to ECI J2000 Coordinates

NAME:   
   PGS_CSC_TODtoJ2000()

SYNOPSIS:
C:
   #include <PGS_CSC.h>
   
   PGSt_SMF_status
   PGS_CSC_TODtoJ2000( 
       PGSt_integer threeOr6, 
       PGSt_double  secTAI93,  
       PGSt_double  posvelTOD[])
       PGSt_double  posvelECI[],

FORTRAN:
      include 'PGS_CSC_4.f'
      include 'PGS_SMF.f'

      integer function pgs_csc_todtoj2000(threeor6,sectai93,posveleci,
     >                                    posveltod)
      integer          threeor6
      double precision sectai93
      double precision posveltod(*)
      double precision posveleci(*)  
  
DESCRIPTION:
   This function transforms from TOD (true of date) coordinates to ECI (J2000)
   coordinates.
 
INPUTS:
   Name             Description            Units    Min        Max
   ----             -----------            -----    ---        --- 
   threeOr6         dimension of input
                    vector                 N/A       3          6
   secTAI93         TOD time               seconds
   posvelTOD[6]     Vector (position and 
                    possibly velocity) in 
		    ECI TOD
      posvelTOD[0]  x position             meters 
      posvelTOD[1]  y position             meters    
      posvelTOD[2]  z position             meters    
      posvelTOD[3]  x velocity             m/s   
      posvelTOD[4]  y velocity             m/s    
      posvelTOD[5]  z velocity             m/s   

OUTPUTS:
   Name             Description            Units    Min        Max
   ----             -----------            -----    ---        --- 
   posvelECI[]      Vector (position and 
                    possibly velocity) in 
                    ECI J2000
      posvelECI[0]  x position             meters
      posvelECI[1]  y position             meters
      posvelECI[2]  z position             meters
      posvelECI[3]  x velocity             m/s   
      posvelECI[4]  y velocity             m/s   
      posvelECI[5]  z velocity             m/s   

RETURNS:
   PGS_S_SUCCESS                  successful return
   PGSCSC_E_BAD_ARRAY_SIZE        incorrect array size

EXAMPLES:
C:

   PGSt_SMF_status    returnStatus;
   PGSt_double        secTAI93 = -44496000.0;
   PGSt_double        posvelTOD[6] = {0.5,0.75,0.90,0.3,0.2,0.8};
   PGSt_double        posvelECI[6];				     
  
   returnStatus = PGS_CSC_TODtoJ2000(6,secTAI93,posvelTOD,posvelECI)
                                                                      
   if(returnStatus != PGS_S_SUCCESS)
   {
     ** test errors, 
         take appropriate
          action **
   }

FORTRAN:

      implicit none
      integer		  pgs_csc_todtoj2000
      integer		  returnstatus
      integer		  threeor6
      double precision    sectai93
      double precision    posveltod(6)
      double precision    posveleci(6)
      integer             cnt1
      character*33 	  err
      character*241 	  msg
      
      do 10 cnt1  = 1,6
          posveltod(cnt1) = 100 * cnt1
   10 continue

      sectai93 = -44496000.0
      three0r6 = 6

      returnstatus = pgs_csc_todtoj2000(threeor6,sectai93,posveltod,
     >                                  posveleci)

      if (returnstatus .ne. pgs_s_success) then
	 pgs_smf_getmsg(returnstatus, err, msg)
	 write(*,*) err, msg
      endif
   
NOTES:
   If threeOr6 is 3, only position is transformed (in this case it is not
   necessary to pass in a 6-vector, a 3-vector may safely be input); if 6 then
   both position and velocity.

   TIME ACRONYMS:
     
     TAI is:  International Atomic Time
     TDB is:  Barycentric Dynamical Time
     TDT is:  Terrestrial Dynamical Time

   TOOLKIT INTERNAL TIME (TAI):

     Toolkit internal time is the real number of continuous SI seconds since the
     epoch of UTC 12 AM 1-1-1993.  Toolkit internal time is also referred to in
     the toolkit as TAI.

   REFERENCES FOR TIME:

     CCSDS 301.0-B-2 (CCSDS => Consultative Committee for Space Data Systems) 
     Astronomical Almanac, Explanatory Supplement to the Astronomical Almanac

REQUIREMENTS:  
   PGSTK - 0910, 1050

DETAILS:
   The sequence of operations for TOD -> J2000 is:

     1. get nutation angles and their rates and TDT from TAI
     2. nutate the vector
     3. precess the vector

GLOBALS:
   NONE

FILES:
   NONE
      
FUNCTIONS CALLED:
   PGS_CSC_wahr2()            obtain nutation angles and rates
   PGS_TD_TAItoTAIjd()        get TAI Julian date from Toolkit internal time
   PGS_TD_TAIjdtoTDTjed()     get TDT Julian ephemeris date from TAI Julian date
   PGS_CSC_precs2000()        precess vector from J2000 to TDT
   PGS_CSC_nutate2000()       nutate vector to pole of date
   PGS_SMF_SetStaticMsg()     set error/status message  
   PGS_SMF_SetUnknownMsg()    set unknown message 

END_PROLOG
*******************************************************************************/

#include <stdio.h>
#include <PGS_math.h>
#include <PGS_TD.h>
#include <PGS_CSC.h>

/* name of this function */

#define FUNCTION_NAME "PGS_CSC_TODtoJ2000"

PGSt_SMF_status 
PGS_CSC_TODtoJ2000(               /* transforms from TOD reference frame to 
                                     ECI J2000 ref. frame */
    PGSt_integer    threeOr6,     /* 3 to transform only x, 6 for x and v */
    PGSt_double     secTAI93,     /* time in toolkit internal format */
    PGSt_double     posvelTOD[],  /* position (m) and velocity (m/s) in TOD */
    PGSt_double     posvelECI[])  /* position (m) and velocity (m/s) in J2000 */
{    
    PGSt_double     jedTDT[2];    /* TDB expressed in Julian days */
    PGSt_double     dvnut[4];     /* the two nutation angles and their rates,
				     output from "PGS_CSC_wahr2" */
    int             index;        /* loop counter */

    switch ( threeOr6 )
    {
      case 3:
      case 6:
	break;
      default:
	PGS_SMF_SetStaticMsg(PGSCSC_E_BAD_ARRAY_SIZE, FUNCTION_NAME);
	return  PGSCSC_E_BAD_ARRAY_SIZE;
    }

    /* Initialize ECI values to TOD values, because the nutation and
       precession are "in place" functions */

    for (index=0; index < threeOr6; index++)
	posvelECI[index] = posvelTOD[index];

    /* get the nutation angles and their rates (the TDB Julian date is actually
       returned in the jedTDT variable here but this is just a throw away value
       since it is not needed here and the value in jedTDT is immediately
       overwritten--first with the TAI Julian date and then finally with the
       actual TDT Julian date) */

    PGS_CSC_quickWahr(secTAI93, jedTDT, dvnut);

    /* get TDT from TAI */

    PGS_TD_TAItoTAIjd(secTAI93, jedTDT);
    PGS_TD_TAIjdtoTDTjed(jedTDT, jedTDT);
    
    /* nutate the vector to pole of J2000 */

    PGS_CSC_nutate2000(threeOr6,jedTDT,dvnut,PGS_FALSE,posvelECI);

    /* Precess the vector from jedTDT to J2000  */

    PGS_CSC_precs2000(threeOr6,jedTDT,PGS_FALSE,posvelECI);

    return PGS_S_SUCCESS;    
}
