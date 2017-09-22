
!  $Id$

#include "MAPL_Generic.h"


!=============================================================================
module GEOS_VegdynGridCompMod

!BOP

! !MODULE: GEOS_Vegdyn -- child to the "Land" gridded component.  

!DESCRIPTION:
!   {\tt GEOS\_Vegdyn} is a gridded component that performs the
!   necessary interpolation to provide refreshed values of the 
!   dynamic vegetation values prescribed by external data/observations.\\
!
! There are no imports to this routine.
! Exports from this routine are the instaneous values of the
! vegetation parameters on tilespace to be used in other components
! of the land subroutine.  All exports and imports are stored on the
! tile grid inherited from the parent routine.\\
! 
! I. Parameter Class 1: Time AND spatially dependent parameters 
! from a binary data file\\
! 
! Current list: LAI, GRN\\
! 
! The gridded component stores the surrounding observations of 
! each parameter in the internal state.  If the run method 
! discovers that the current internal state does not contain the 
! observed values required to interpolate the values at the current 
! time, it performs the required i/o to refresh the values of 
! the internal state.  The first iteration of the run method 
! always has to fill the values.  No restart is required by this 
! gridded component for these parameters.  (A restart *is* now
! required for Vegetation Class 3 \\
!
! INTERNALS: ITY\\
!
! EXPORTS:  LAI, GRN\\

! !USES: 

  use ESMF
  use MAPL_Mod

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!EOP

  integer, parameter		     :: NTYPS = MAPL_NumVegTypes
  real,    dimension(   NTYPS)       :: VGRT
  real,    dimension(   NTYPS)	     :: VGZ2

  data VGRT  / 19700., 7000., 9400., 7000., 7000., 14000./
  data VGZ2   /35.0, 20.0, 17.0, 0.6, 0.5, 0.6/ ! Dorman and Sellers (1989)

contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp.

!EOP

!=============================================================================
!
! ErrLog Variables


    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (ESMF_Config          )            :: CF

    integer      :: RUN_DT
    integer      :: MY_STEP
    integer      :: ACCUMINT
    real         :: DT

!=============================================================================

! Begin...

!------------------------------------------------------------
! Get my name and set-up traceback handle
!------------------------------------------------------------

    call ESMF_GridCompGet(GC                                 ,&
                          NAME=COMP_NAME			   ,&
                          RC=STATUS )

    VERIFY_(STATUS)

    Iam = trim(COMP_NAME) // 'SetServices'

! -----------------------------------------------------------
! Set the Run entry point
! -----------------------------------------------------------

    call MAPL_GridCompSetEntryPoint (GC, ESMF_METHOD_RUN, Run, RC=STATUS)
    VERIFY_(STATUS)

! -----------------------------------------------------------
! Get the configuration
! -----------------------------------------------------------
 
    call ESMF_GridCompGet( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

! -----------------------------------------------------------
! Get the intervals
! -----------------------------------------------------------

    call ESMF_ConfigGetAttribute (CF, DT                         ,&
                                  Label="RUN_DT:"                ,&
                                  RC=STATUS)

    VERIFY_(STATUS)

    RUN_DT = nint(DT)

! -----------------------------------------------------------
! At the moment, this will refresh when the land parent 
! needs to refresh.
!
!    call ESMF_ConfigGetFloat ( CF, DT, Label=trim(COMP_NAME)//&
!    "_DT:", default=DT, RC=STATUS)
!     VERIFY_(STATUS)
!
!    MY_STEP = nint(DT)
!
! -----------------------------------------------------------


! -----------------------------------------------------------
! Set the state variable specs.
! -----------------------------------------------------------

!BOS

! -----------------------------------------------------------
!   Import States
! None at the moment
! -----------------------------------------------------------

! -----------------------------------------------------------
! Internal State 
! -----------------------------------------------------------

    call MAPL_AddInternalSpec(GC                          ,&
       SHORT_NAME = 'ITY'                                     ,&
       LONG_NAME  = 'vegetation_type'			      ,&
       UNITS      = '1'                                       ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       FRIENDLYTO = trim(COMP_NAME)                           ,&
       RC=STATUS  )
    VERIFY_(STATUS)  

! -----------------------------------------------------------
! These are variables that are considered time-independent
! and are stored and retrieved as-is
! -----------------------------------------------------------

! -----------------------------------------------------------
! Export Variables
! -----------------------------------------------------------

    call MAPL_AddExportSpec(GC                            ,&
       SHORT_NAME = 'LAI'                                     ,&
       LONG_NAME  = 'leaf_area_index'                         ,&
       UNITS      = '1'                                       ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       RC=STATUS  )

    VERIFY_(STATUS)  

    call MAPL_AddExportSpec(GC                            ,&
       SHORT_NAME = 'GRN'                                     ,&
       LONG_NAME  = 'greeness_fraction'			      ,&
       UNITS      = '1'                                       ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       RC=STATUS  )

    VERIFY_(STATUS)  	 

    call MAPL_AddExportSpec(GC                            ,&
       SHORT_NAME = 'Z2CH'                                    ,&
       LONG_NAME  = 'canopy_height'			      ,&
       UNITS      = 'm'                                       ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       RC=STATUS  )

    VERIFY_(STATUS)  

    call MAPL_AddExportSpec(GC                            ,&
       SHORT_NAME = 'ROOTL'                                   ,&
       LONG_NAME  = 'root_length_density'                     ,&
       UNITS      = 'm+2'                                     ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       RC=STATUS  )

    VERIFY_(STATUS)  	 

!EOS

!------------------------------------------------------------
! Set generic init and final methods
!------------------------------------------------------------

    call MAPL_GenericSetServices(GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

! -----------------------------------------------------------
! RUN -- Run method for the vegdyn component
! -----------------------------------------------------------

  subroutine RUN (GC,IMPORT, EXPORT, CLOCK, RC )

! -----------------------------------------------------------
! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC    
    type(ESMF_State),    intent(inout) :: IMPORT
    type(ESMF_State),    intent(inout) :: EXPORT
    type(ESMF_Clock),    intent(inout) :: CLOCK
    integer, optional,   intent(  out) :: RC

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

    type (MAPL_MetaComp),     pointer   :: MAPL
    type (ESMF_State       )            :: INTERNAL

    type (ESMF_Config )         :: CF

! INTERNAL pointers
 
    real, dimension(:), pointer :: ITY

! EXPORT pointers 

    real, dimension(:), pointer :: LAI
    real, dimension(:), pointer :: GRN
    real, dimension(:), pointer :: Z2CH
    real, dimension(:), pointer :: ROOTL
  
! Time attributes and placeholders

    type(ESMF_Time) :: CURRENT_TIME

! Others

    character(len=ESMF_MAXSTR)         :: LAIFile
    character(len=ESMF_MAXSTR)         :: GRNFile
    integer, dimension(:), allocatable :: VEG_TYPES

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    call ESMF_GridCompGet(GC, name=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
  
    Iam = trim(COMP_NAME) // "Run"

! Get my internal MAPL_Generic state
! -----------------------------------------------------------

    call MAPL_GetObjectFromGC(GC, MAPL, STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(MAPL,"TOTAL")

    call MAPL_Get(MAPL, INTERNAL_ESMF_STATE=INTERNAL, RC=STATUS)
    VERIFY_(STATUS) 

! -----------------------------------------------------------
! Get file names from configuration
! -----------------------------------------------------------

    call MAPL_GetResource(MAPL, LAIFILE, label = 'LAI_FILE:', &
         default = 'lai.dat', RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GetResource(MAPL, GRNFILE, label = 'GREEN_FILE:', &
         default = 'green.dat', RC=STATUS )
    VERIFY_(STATUS)

! get pointers to internal variables
! ----------------------------------
  
    call MAPL_GetPointer(INTERNAL,      ITY,      'ITY', RC=STATUS)
    VERIFY_(STATUS)

! get pointers to EXPORTS
! -----------------------

    call MAPL_GetPointer(EXPORT, LAI,   'LAI',    RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, GRN,   'GRN',    RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, ROOTL, 'ROOTL',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, Z2CH,  'Z2CH',   RC=STATUS)
    VERIFY_(STATUS)

! Do the lai and greeness interpolation
! -------------------------------------

    call ESMF_ClockGet  ( CLOCK, currTime=CURRENT_TIME, RC=STATUS )
    VERIFY_(STATUS)

    call MAPL_ReadForcing(MAPL,'LAI',LAIFILE,CURRENT_TIME,LAI,ON_TILES=.true.,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_ReadForcing(MAPL,'GRN',GRNFILE,CURRENT_TIME,GRN,ON_TILES=.true.,RC=STATUS)
    VERIFY_(STATUS)

! Vegetation types used to index into tables
!-------------------------------------------

    allocate(VEG_TYPES(size(ITY)))
    VEG_TYPES=nint(ITY)

! Vegetation displacement height depends on type only
! Root length density no longer depends on time of year
! -----------------------------------------------------

    Z2CH  = VGZ2(VEG_TYPES) 
    ROOTL = VGRT(VEG_TYPES)

    deallocate(VEG_TYPES)

!  All done
! ---------

    call MAPL_TimerOff(MAPL,"TOTAL")

    RETURN_(ESMF_SUCCESS)
  end subroutine RUN

end module GEOS_VegdynGridCompMod


