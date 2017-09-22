!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_DataSea -- A `fake' ocean surface

! !INTERFACE:

module GEOS_DataSeaGridCompMod

! !USES: 

  use ESMF
  use MAPL_Mod

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION:
! 
!   {\tt GEOS\_DataSea} is a gridded component that reads the 
!   ocean\_bcs file 
!   This module interpolates the SST and sea ice data from 
!   either daily or monthly values to the correct time of the simulation.
!   Data are read only if the simulation time is not in the save interval.
!   Surface Albedo and Surface roughness calculations are also takencare of in
!   this module.
!

!EOP

   contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

!  !DESCRIPTION: This version uses the MAPL\_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp.
!
!EOP

!=============================================================================
!
! ErrLog Variables


    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = "SetServices"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam


! Set the Run entry point
! -----------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run, RC=STATUS)
    VERIFY_(STATUS)


! Set the state variable specs.
! -----------------------------

!BOS

! !Import state:

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'HW',                                &
    LONG_NAME          = 'water_skin_layer_depth',            &
    UNITS              = 'm',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'TW',                                &
    LONG_NAME          = 'water_skin_temperature',            &
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'SW',                                &
    LONG_NAME          = 'water_skin_salinity',               &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
       SHORT_NAME         = 'SWHEAT',                         &
       LONG_NAME          = 'solar_heating_rate',             &
       UNITS              = 'W m-2',                          &
       DIMS               = MAPL_DimsHorzVert,                &
       VLOCATION          = MAPL_VLocationCenter,             &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                                 &
         SHORT_NAME       = 'TAUX',                           &
         LONG_NAME        = 'Agrid_eastward_stress_on_skin',  &
         UNITS            = 'N m-2',                          &
         DIMS             = MAPL_DimsHorzOnly,                &
         VLOCATION        = MAPL_VLocationNone,               &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                                 &
         SHORT_NAME       = 'TAUY',                           &
         LONG_NAME        = 'Agrid_northward_stress_on_skin',    &
         UNITS            = 'N m-2',                          &
         DIMS             = MAPL_DimsHorzOnly,                &
         VLOCATION        = MAPL_VLocationNone,               &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

!  !Export state:

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'UW',                                &
    LONG_NAME          = 'zonal_velocity_of_surface_water',   &
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'VW',                                &
    LONG_NAME          = 'meridional_velocity_of_surface_water',&
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                 &
    SHORT_NAME         = 'TS_FOUND',                          &
    LONG_NAME          = 'foundation_temperature_for_interface_layer',&
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                 &
    SHORT_NAME         = 'TSCALE',                            &
    LONG_NAME          = 'relaxation_time_scale',             &
    UNITS              = 'sec',                               &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                 &
    SHORT_NAME         = 'DEL_RHO',                           &
    LONG_NAME          = 'density_difference',                &
    UNITS              = 'kg m-3',                            &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                 &
    SHORT_NAME         = 'USTAR_w',                           &
    LONG_NAME          = 'ustar_over_water',                  &
    UNITS              = 'm s-1',                             &
    DIMS               = MAPL_DimsHorzOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

!EOS

    call MAPL_TimerAdd(GC,    name="RUN"     ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-UPDATE"     ,RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: RUN -- Run stage for the DataSea component

! !INTERFACE:

subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )


! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the SST and Ice information.

!EOP


! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp),     pointer   :: MAPL
  logical                             :: FRIENDLY
  type(ESMF_FIELD)                    :: FIELD
  type (ESMF_Time)                    :: CurrentTime
  character(len=ESMF_MAXSTR)          :: DATASEAFILE
  integer                             :: IFCST
  logical                             :: FCST
  real, pointer, dimension(:,:)       :: SST
  real                                :: TAU_SST_OC
  real                                :: TAU_SST_ML
  real                                :: ML_DEPTH
  real                                :: DT
  real                                :: RUN_DT

  real                                :: TAU_MIN, TAU_MAX, TAU_FACTOR, HM
  integer                             :: DoTau
  integer                             :: DO_SKIN_LAYER                         ! controls default value of TAU_SST. See Saltwater for details.
  real, allocatable                   :: TAU(:,:), DRHO(:,:), Sbulk(:,:), &    ! Sbulk: bulk salinity corresponds to bulk SST
                                         F1(:,:),  F2(:,:),   USTAR(:,:)
  real, parameter                     :: RHO0=1025.0                           ! reference density (kg/m^3)
  real, parameter                     :: ALPHA=-0.23, BETA=0.76                ! coefficients of linear equation of state 

! pointers to export

   real, pointer, dimension(:,:)  :: UW
   real, pointer, dimension(:,:)  :: VW
   real, pointer, dimension(:,:)  :: TS_FOUND
   real, pointer, dimension(:,:)  :: TSCL
   real, pointer, dimension(:,:)  :: DRHO_o
   real, pointer, dimension(:,:)  :: USTAR_w

! pointers to import

   real, pointer, dimension(:,:)  :: TW
   real, pointer, dimension(:,:)  :: HW
   real, pointer, dimension(:,:)  :: SW

   real, pointer, dimension(:,:)  :: TAUX
   real, pointer, dimension(:,:)  :: TAUY

!  Begin...
!----------

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!----------------------------------

    call MAPL_GetObjectFromGC(GC, MAPL, STATUS)
    VERIFY_(STATUS)

! Start Total timer
!------------------

   call MAPL_TimerOn(MAPL,"TOTAL")
   call MAPL_TimerOn(MAPL,"RUN" )


! Pointers to Imports
!--------------------

    call MAPL_GetPointer(IMPORT, TW      ,  'TW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, HW      ,  'HW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, SW      ,  'SW'     , RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetPointer(IMPORT, TAUX    ,  'TAUX'   , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TAUY    ,  'TAUY'   , RC=STATUS)
    VERIFY_(STATUS)

! Check that they are friendly
!-----------------------------

    call ESMF_StateGet (IMPORT, 'TW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'HW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'SW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

!  Pointers to Exports
!---------------------

    call MAPL_GetPointer(EXPORT,      UW  , 'UW'       , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,      VW  , 'VW'       , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, TS_FOUND , 'TS_FOUND' , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, TSCL     , 'TSCALE'   , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, DRHO_o   , 'DEL_RHO'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, USTAR_w  , 'USTAR_w'  , RC=STATUS)
    VERIFY_(STATUS)

! Set current time and calendar
!------------------------------

    call ESMF_ClockGet(CLOCK, currTime=CurrentTime, rc=STATUS)
    VERIFY_(STATUS)

! Get the file name from the resource file
!-----------------------------------------

    call MAPL_GetResource(MAPL,DATASeaFILE,LABEL="DATA_SST_FILE:", RC=STATUS)
    VERIFY_(STATUS)

! In atmospheric forecast mode we don't have future SSTs
!-------------------------------------------------------

    call MAPL_GetResource(MAPL,IFCST,LABEL="IS_FCST:",default=0,   RC=STATUS)
    VERIFY_(STATUS)

    FCST = IFCST==1

! Get relaxation time and other parameters
!-----------------------------------------

    call MAPL_GetResource(MAPL,DO_SKIN_LAYER, Label="USE_SKIN_LAYER:", default=0,     RC=STATUS)
    VERIFY_(STATUS)

    if (DO_SKIN_LAYER==0) then
       call MAPL_GetResource(MAPL,TAU_SST_OC, LABEL="TAU_SST:",      default=0.001,      RC=STATUS)
    else
       call MAPL_GetResource(MAPL,TAU_SST_OC, LABEL="TAU_SST:",      default=1.0e+15,    RC=STATUS)
    end if
    VERIFY_(STATUS)

    call MAPL_GetResource(MAPL,TAU_SST_ML, LABEL="TAU_SST_ML:",   default=86400.*60., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource(MAPL,RUN_DT,     LABEL="RUN_DT:" ,                          RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource(MAPL,DT,         LABEL="DT:"     ,      default=RUN_DT,     RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource(MAPL,ML_DEPTH,   LABEL="ML_DEPTH"   ,   default=20.,        RC=STATUS)
    VERIFY_(STATUS)

! Adjust temp profile based on stability?
!---------------------------------------

    call MAPL_GetResource (MAPL,DoTau,     LABEL="Calc_TAU_SST:", default=0,          RC=STATUS)
    VERIFY_(STATUS)

    if(DoTau /= 0) then
       call MAPL_GetResource(MAPL,TAU_MIN,    LABEL="TAU_MIN:",    default=7200.,     RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource(MAPL,TAU_MAX,    LABEL="TAU_MAX:",    default=7200.,     RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource(MAPL,TAU_FACTOR, LABEL="TAU_FACTOR:", default=1.0,       RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource(MAPL,HM,         Label="SKINH:",      default=2.,        RC=STATUS)
       VERIFY_(STATUS)

       allocate(TAU  (size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
       allocate(DRHO (size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
       allocate(Sbulk(size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
       allocate(F1   (size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
       allocate(F2   (size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
       allocate(USTAR(size(TW,1),size(TW,2)), stat=STATUS);   VERIFY_(STATUS)
    end if

! SST is the ML base temperature that we relax to (usually Reynolds SST or `bulk' SST)
!-------------------------------------------------------------------------------------

    allocate(SST(size(TW,1),size(TW,2)), stat=STATUS)
    VERIFY_(STATUS)

!  Update the friendly skin values
!---------------------------------

   call MAPL_TimerOn(MAPL,"-UPDATE" )

!  Read bulk SST from retrieval
!---------------------------------

   call MAPL_ReadForcing(MAPL,'SST',DATASeaFILE, CURRENTTIME, SST, INIT_ONLY=FCST, RC=STATUS)
    VERIFY_(STATUS)

   call MAPL_TimerOff(MAPL,"-UPDATE" )

!  negative base temperatures tag regions where we want a shallow
!  mixed layer whose temperatures can depart from SST; 
!  normal positive (in Kelvins) temperatures result
!  in fully prescribed skin temperatures, for which we relax a
!  deep layer very quickly to SST. 

   if(DoTau == 0) then
   !  Update TW based on relaxation time & bulk SST

     where (TW /= MAPL_Undef .and. SST /= MAPL_Undef .and. SST>0.)
        TW   = (TW + (DT/TAU_SST_OC)*SST     )/(1.+ (DT/TAU_SST_OC))
        HW   = HW
     end where

     where (TW /= MAPL_Undef .and. SST /= MAPL_Undef .and. SST<0.)       ! this never happens because SST is in deg K and always > 0.0K
        TW   = (TW + (DT/TAU_SST_ML)*abs(SST))/(1.+ (DT/TAU_SST_ML))
        HW   = ML_DEPTH*MAPL_RHOWTR
     end where

     SW   = 30.0
   else

!    Calculate relaxation time scale (TAU) based on momentum stress at top
!    ocean surface wind stress is a combination of wind stress from atmosphere and stress over sea-ice.
!    in this component, i.e., DataSea GC, we account for OCEAN ONLY, hence contribution to stess from sea-ice is ignored.

     SW   = 30.0
     Sbulk= 30.0                                           ! set to 30 psu for now

!    !Density difference between mean & `bulk' ocean 
     DRHO = ALPHA * (SST - TW) + BETA * (Sbulk - SW)       ! based on linear eqution of state; if- Rho(SST) > Rho(SW) => Stable
!    If DRHO is calculated using TW-SST, diurnal warming is wrong.
!    For e.g., where USTAR is small and, insolation is high, we should have large TW-SST, 
!    this happens when we compute DRHO using SST-TW and does NOT happen when if use TW-SST. 
!    Also see experiment results. SA. Dec 2012.

!    DRHO(i,j) = density_point(TS(i,j),SS(i,j),0.0) - density_point(TW(i,j),SW(i,j),0.0) ! not sure if the function is correct!             

     USTAR = sqrt(sqrt(TAUX**2+TAUY**2)/RHO0)
     F1    = DRHO*(HM**2)/RHO0
     F2    = (USTAR**3)+tiny(1.0)
     TAU   = TAU_FACTOR*F1/F2                              ! for this to make sense, TAU_FACTOR has DIMENSIONS of [m/s^2]- missing `g'?
                                                           ! Dividing by g, i.e., Non-dimensionalizing makes TAU very small everywhere..NOT Good.

!    !For unstable and weakly stable stratification (for e.g., strong wind) set TAU_MIN
!    !For strongly stable stratification            (for e.g.,   weak wind) set TAU_MAX 
     TAU=min(max(TAU,TAU_MIN),TAU_MAX)

     where (TW /= MAPL_UNDEF)
        TW   =(TW + (DT/TAU)*SST)/(1.+ (DT/TAU))
     end where

     if(associated(TSCL  ))  then
        where(TW /= MAPL_UNDEF)
          TSCL    = TAU
        elsewhere
          TSCL    = MAPL_UNDEF
        end where
     end if

     HW = HW

     if(associated(DRHO_o))  then
        where(TW /= MAPL_UNDEF)
          DRHO_o  = DRHO
        elsewhere
          DRHO_o  = MAPL_UNDEF
        end where
     end if

     if(associated(USTAR_w)) then
        where(TW /= MAPL_UNDEF)
          USTAR_w = USTAR
        elsewhere
          USTAR_w = MAPL_UNDEF
        end where
     end if

   end if

!  Update the exports
!--------------------

   if(associated(UW)) UW = 0.0
   if(associated(VW)) VW = 0.0

   if(associated(TS_FOUND)) then
      where(TW /= MAPL_UNDEF)
         TS_FOUND = abs(SST)
      elsewhere
         TS_FOUND = MAPL_UNDEF
      end where
   end if

! Clean-up
!---------

   deallocate(SST,     STAT=STATUS); VERIFY_(STATUS)

   if(DoTau /= 0) then
     deallocate(TAU,   STAT=STATUS); VERIFY_(STATUS)
     deallocate(DRHO,  STAT=STATUS); VERIFY_(STATUS)
     deallocate(Sbulk, STAT=STATUS); VERIFY_(STATUS)
     deallocate(F1,    STAT=STATUS); VERIFY_(STATUS)
     deallocate(F2,    STAT=STATUS); VERIFY_(STATUS)
     deallocate(USTAR, STAT=STATUS); VERIFY_(STATUS)
   end if

!  All done
!-----------

   call MAPL_TimerOff(MAPL,"RUN"  )
   call MAPL_TimerOff(MAPL,"TOTAL")

   RETURN_(ESMF_SUCCESS)
end subroutine RUN

!#######################################################################
! From MOM4p1. It is SAME in MOM5 as well. 
! See mom/src/mom5/ocean_core/ocean_density.F90
! <FUNCTION NAME="density_point">
!
! <DESCRIPTION>
! Compute density at a single model grid point. 
!
! Note that pressure here is 
!
! sea pressure = absolute pressure - press_standard  (dbars)
!
! </DESCRIPTION>
!
  function density_point (s1, t1, p1_dbars)

    real, intent(in) :: s1, t1, p1_dbars
    real             :: t2, sp5, p1, p1t1
    real             :: num, den
    real             :: density_point
    real :: a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
    real :: b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12
    real ::  press_standard

    press_standard=0.0 ! Should be atmospheric standard pressure, zero for our purpose

    a0  =  9.9983912878771446d+02
    a1  =  7.0687133522652896d+00
    a2  = -2.2746841916232965d-02
    a3  =  5.6569114861400121d-04
    a4  =  2.3849975952593345d+00
    a5  =  3.1761924314867009d-04
    a6  =  1.7459053010547962d-03
    a7  =  1.2192536310173776d-02
    a8  =  2.4643435731663949d-07
    a9  =  4.0525405332794888d-06
    a10 = -2.3890831309113187d-08
    a11 = -5.9016182471196891d-12

    b0  =  1.0000000000000000d+00
    b1  =  7.0051665739672298d-03
    b2  = -1.5040804107377016d-05
    b3  =  5.3943915288426715d-07
    b4  =  3.3811600427083414d-10
    b5  =  1.5599507046153769d-03
    b6  = -1.8137352466500517d-06
    b7  = -3.3580158763335367d-10
    b8  =  5.7149997597561099d-06
    b9  =  7.8025873978107375d-10
    b10 =  7.1038052872522844d-06
    b11 = -2.1692301739460094d-17
    b12 = -8.2564080016458560d-18

    t2  = t1*t1
    sp5 = sqrt(s1)

    p1   = p1_dbars - press_standard
    p1t1 = p1*t1

    num = a0 + t1*(a1 + t1*(a2+a3*t1))                   &
         + s1*(a4 + a5*t1  + a6*s1)                      &
         + p1*(a7 + a8*t2 + a9*s1 + p1*(a10+a11*t2))

    den = b0 + t1*(b1 + t1*(b2 + t1*(b3 + t1*b4 )))      &
         + s1*(b5 + t1*(b6 + b7*t2) + sp5*(b8 + b9*t2))  &
         + p1*(b10 + p1t1*(b11*t2 + b12*p1))

    density_point = num/(tiny(1.0)+den)

  end function density_point

end module GEOS_DataSeaGridCompMod
