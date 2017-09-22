!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_OradGridCompMod -- Implements absorption of solar radiation in the ocean.

! !INTERFACE:

module GEOS_OradGridCompMod

! !USES:

  use ESMF
  use MAPL_Mod
#ifdef USE_ODAS
  use gmaoodas_gridcomp, only: news => ocean_news_grid   
  use odas_gems_module
#endif  

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION:
! 
!   {\tt GEOS\_Orad} is a light-weight gridded component that updates
!      the the solar radiation penetrating the ocean
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

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices, which sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp. The import
!   and internal variables are allocated and initialized by generic.  Here
!   generic is used for the ocean grid.

!EOP

!=============================================================================

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // 'SetServices'

! Set the Run entry point
! -----------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run, RC=STATUS )
    VERIFY_(STATUS)

! Set the state variable specs.
! -----------------------------

!BOS

! !INTERNAL STATE:


   call MAPL_AddInternalSpec(GC                                        ,&
        LONG_NAME          = 'KPAR_previous'                                ,&
        UNITS              = 'm-1'                                          ,&
        SHORT_NAME         = 'KPAR_PREV'                                    ,&
        DIMS               = MAPL_DimsHorzOnly                              ,&
        VLOCATION          = MAPL_VLocationNone                             ,&
        REFRESH_INTERVAL   = -1, &! kludgy flag to indicate time not set
        RC=STATUS                                                            )

     VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC                                        ,&
        LONG_NAME          = 'KPAR_next'                                    ,&
        UNITS              = 'm-1'                                          ,&
        SHORT_NAME         = 'KPAR_NEXT'                                    ,&
        DIMS               = MAPL_DimsHorzOnly                              ,&
        VLOCATION          = MAPL_VLocationNone                             ,&
        REFRESH_INTERVAL   = -1, & ! kludgy flag to indicate time not set
        RC=STATUS                                                            )

     VERIFY_(STATUS)

!  !EXPORT STATE:

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'SWHEAT',                            &
        LONG_NAME          = 'solar_heating_rate',                &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'KPAR',                              &
        LONG_NAME          = 'PAR_extinction_coefficient',        &
        units              = 'm-1',                               &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


!  !IMPORT STATE:

     call MAPL_AddImportSpec(GC,                                 &
        LONG_NAME  = 'cosine_of_the_solar_zenith_angle',              &
        UNITS      = '1',                                             &
        SHORT_NAME = 'COSZ',                                          &
        DIMS       = MAPL_DimsHorzOnly,                               &
        VLOCATION  = MAPL_VLocationNone,                              &
                                                           RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'PENUVR',                            &
        LONG_NAME          = 'net_downward_penetrating_direct_UV_flux',  &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'PENPAR',                            &
        LONG_NAME          = 'net_downward_penetrating_direct_PAR_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'PENUVF',                            &
        LONG_NAME          = 'net_downward_penetrating_diffuse_UV_flux',  &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'PENPAF',                            &
        LONG_NAME          = 'net_downward_penetrating_diffuse_PAR_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                 &
        SHORT_NAME = 'FROCEAN',                                       &
        LONG_NAME  = 'ocean_fraction_of_grid_cell',                   &
        UNITS      = '1',                                             &
        DIMS       = MAPL_DimsHorzOnly,                               &
        VLOCATION  = MAPL_VLocationNone,                              &
                                                           RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                 &
        SHORT_NAME = 'DH',                                            &
        LONG_NAME  = 'Layer mass',                                    &
        UNITS      = 'dyn-m',                                         &
        DIMS       = MAPL_DimsHorzVert,                               &
        VLOCATION  = MAPL_VLocationCenter,                            &
                                                           RC=STATUS  )
     VERIFY_(STATUS)


     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'PENUVR',                            &
        LONG_NAME          = 'net_downward_penetrating_direct_UV_flux',  &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'PENPAR',                            &
        LONG_NAME          = 'net_downward_penetrating_direct_PAR_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'PENUVF',                            &
        LONG_NAME          = 'net_downward_penetrating_diffuse_UV_flux',  &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'PENPAF',                            &
        LONG_NAME          = 'net_downward_penetrating_diffuse_PAR_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                 &
        SHORT_NAME = 'FROCEAN',                                       &
        LONG_NAME  = 'ocean_fraction_of_grid_cell',                   &
        UNITS      = '1',                                             &
        DIMS       = MAPL_DimsHorzOnly,                               &
        VLOCATION  = MAPL_VLocationNone,                              &
                                                           RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                 &
        SHORT_NAME = 'DH',                                            &
        LONG_NAME  = 'Layer mass',                                    &
        UNITS      = 'dyn-m',                                         &
        DIMS       = MAPL_DimsHorzVert,                               &
        VLOCATION  = MAPL_VLocationCenter,                            &
                                                           RC=STATUS  )
     VERIFY_(STATUS)

#ifdef USE_ODAS
     call MAPL_AddImportSpec(GC,                                      &
       SHORT_NAME = 'CHLOROPHYLL',                                    &
       LONG_NAME  = 'chlorophyll_concentration',                      &
       UNITS      = 'mg m-3',                                         &
       DIMS       = MAPL_DimsHorzVert,                                &
       VLOCATION  = MAPL_VLocationCenter,                             &
       RESTART    = .false.,                                          &
       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                      &
       SHORT_NAME = 'AICE',                                           &
       LONG_NAME  = 'ice_concentration_of_grid_cell',                 &
       UNITS      = '1',                                              &
       DIMS       = MAPL_DimsHorzOnly,                                &
       VLOCATION  = MAPL_VLocationNone,                               &
       RESTART    = .false.,                                          &
       RC=STATUS  )
     VERIFY_(STATUS)
#endif
!EOS

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC,    name="RUN"   ,RC=STATUS)
    VERIFY_(STATUS)
  
! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( GC, RC=STATUS)
    VERIFY_(STATUS)

!  All done
!-----------

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IROUTINE: RUN -- First Run stage for the Orad component

! !INTERFACE:

subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the penetrating radiation. Eventually
!   this will do a full radiative transfer calculation.

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer   :: MAPL
  type (ESMF_Time)                    :: CurrentTime
  character(len=ESMF_MAXSTR)          :: DATAFILE
  real, pointer, dimension(:,:)       :: KPAR
  real, pointer, dimension(:,:)       :: Z
  real, pointer, dimension(:,:)       :: UVR
  real, pointer, dimension(:,:)       :: PAR
  integer                             :: L
  integer                             :: IM,JM,LM

! pointers to export

   real, pointer, dimension(:,:,:)  :: QSW
   real, pointer, dimension(:,:  )  :: KPARX

! pointers to import

   real, pointer, dimension(:,:)   :: COSZ
   real, pointer, dimension(:,:)   :: FR
   real, pointer, dimension(:,:)   :: PRUVR
   real, pointer, dimension(:,:)   :: PRPAR
   real, pointer, dimension(:,:)   :: PRUVF
   real, pointer, dimension(:,:)   :: PRPAF
   real, pointer, dimension(:,:,:) :: H
#ifdef USE_ODAS
   real, pointer :: chlorophyll(:, :, :) => null(), icefraction(:, :) => null(), climatology(:, :) => null(), lats(:, :) => null()
   real :: kpar_clim_threshhold_theta = 70.0, kpar_clim_blend_scale = 10.0 
   logical :: flag = .true.
#endif
! ponters to export
   real, pointer, dimension(:,:)   :: FRx
   real, pointer, dimension(:,:)   :: PRUVRx
   real, pointer, dimension(:,:)   :: PRPARx
   real, pointer, dimension(:,:)   :: PRUVFx
   real, pointer, dimension(:,:)   :: PRPAFx

   real, parameter                 :: KUVR = 0.09

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "Run"

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Start Total timer
!------------------

   call MAPL_TimerOn(MAPL,"TOTAL")
   call MAPL_TimerOn(MAPL,"RUN" )

! Pointers to outputs
!--------------------

   call MAPL_GetPointer(EXPORT,QSW   , 'SWHEAT'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,KPARX , 'KPAR'    ,    RC=STATUS); VERIFY_(STATUS)

! Our only possible exports are the heating rates
!------------------------------------------------

! Pointers to inputs
!-------------------
   
   call MAPL_GetPointer(IMPORT,COSZ  , 'COSZ'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,FR    , 'FROCEAN' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PRUVF , 'PENUVF'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PRPAF , 'PENPAF'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PRUVR , 'PENUVR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PRPAR , 'PENPAR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,H     , 'DH'      ,    RC=STATUS); VERIFY_(STATUS)

! Pointers to exports
!-------------------
   
   call GET_POINTER(EXPORT,FRx    , 'FROCEAN' ,    RC=STATUS); VERIFY_(STATUS)
   call GET_POINTER(EXPORT,PRUVFx , 'PENUVF'  ,    RC=STATUS); VERIFY_(STATUS)
   call GET_POINTER(EXPORT,PRPAFx , 'PENPAF'  ,    RC=STATUS); VERIFY_(STATUS)
   call GET_POINTER(EXPORT,PRUVRx , 'PENUVR'  ,    RC=STATUS); VERIFY_(STATUS)
   call GET_POINTER(EXPORT,PRPARx , 'PENPAR'  ,    RC=STATUS); VERIFY_(STATUS)

! Grid sizes
!-----------
   
   IM = size(H,1)
   JM = size(H,2)
   LM = size(H,3)

   if(associated(KPARX)) then
      KPAR => KPARX
   else
      allocate(KPAR(IM,JM), stat=STATUS)
      VERIFY_(STATUS)
   end if

   allocate(   Z(IM,JM), stat=STATUS)
   VERIFY_(STATUS)
   allocate(UVR (IM,JM), stat=STATUS)
   VERIFY_(STATUS)
   allocate(PAR (IM,JM), stat=STATUS)
   VERIFY_(STATUS)

! Get current time from clock
!----------------------------

   call ESMF_ClockGet(CLOCK, currTime=CurrentTime, rc=STATUS)
   VERIFY_(STATUS)

! Get KPAR from data file
!------------------------

   call MAPL_GetResource(MAPL,DATAFILE,LABEL="KPAR_FILE:"     , RC=STATUS)
   VERIFY_(STATUS)
#ifdef USE_ODAS
   kpar=0.1
   if(trim(datafile) == "chlorophyll") then
      ! kpar calculation from chlorophyll concentration in euphotic zone 
      ! After Morel et al., Remote Sensing of Environment (111), 69-88 2007
      call MAPL_GetPointer(import, chlorophyll, 'CHLOROPHYLL', notfoundOK = .true., __RC__)
      if(associated(chlorophyll)) then; 
         kpar = min(0.5, 0.0166 + 0.0773*min(20.0, max(0.0, maxval(array = chlorophyll, dim = 3)))**0.6715);
         kpar = min(0.5, max(0.02, 0.0864 + 0.884*kpar - 0.00137/kpar));
      endif; 
      allocate(climatology(1:im, 1:jm), stat = status); 
      inquire(file = "kpar.data", exist = flag);
      if(flag) then; call MAPL_ReadForcing(mapl, 'KPAR', 'kpar.data', currenttime, climatology, __RC__);
      else; climatology = 0.07; 
      endif; 
      
      call MAPL_GetResource(mapl, kpar_clim_threshhold_theta, LABEL = "KPAR_CLIM_THRESHHOLD_THETA:", default = 70.0, __RC__)
      call MAPL_GetResource(mapl, kpar_clim_blend_scale, LABEL = "KPAR_CLIM_BLEND_SCALE:", default = 10.0, __RC__)
      if(kpar_clim_threshhold_theta < 90.0) then; 
         call MAPL_Get(mapl, lats = lats, __RC__);
         kpar = (1.0 - fr) &
              + fr*(min(1.0, max(0.0, (90.0*abs(lats)/asin(1.0) - kpar_clim_threshhold_theta)/kpar_clim_blend_scale))*climatology &
              + (1.0 - min(1.0, max(0.0, (90.0*abs(lats)/asin(1.0) - kpar_clim_threshhold_theta)/kpar_clim_blend_scale)))*kpar); 
      else; 
         call MAPL_GetPointer(import, icefraction, 'AICE', notfoundOK = .true., __RC__);
         kpar = (1.0 - fr) + fr*(icefraction*climatology + (1.0 - icefraction)*kpar);
      endif; 
      deallocate(climatology, stat = status);  
   else if(trim(datafile) /= "none") then; call MAPL_ReadForcing(MAPL,'KPAR',DATAFILE,CURRENTTIME,KPAR, __RC__)
   endif;
#else
   call MAPL_ReadForcing(MAPL,'KPAR',DATAFILE,CURRENTTIME,KPAR, RC=STATUS)
   VERIFY_(STATUS)
#endif

#ifdef USE_ODAS
       call hemisphere_write_message(news, "global mean kpar is " // trim(float2str(hemisphere_mean(news, pack(array = kpar, mask = (fr == 1.0))))))
#endif

! Use Beer'S Law to compute flux divergence
!------------------------------------------
   
   UVR = (PRUVF+PRUVR)*FR
   PAR = (PRPAF+PRPAR)*FR
   Z   = 0.0

   if(associated(QSW)) then
      QSW(:,:,1) = UVR + PAR

      do L=2,LM   
         Z = Z + H(:,:,L-1)
         QSW(:,:,L  ) = UVR*exp(-KUVR*Z) + PAR*exp(-KPAR*Z)
         QSW(:,:,L-1) = QSW(:,:,L-1) - QSW(:,:,L)
      enddo
      Z = Z + H(:,:,LM)
      QSW(:,:,LM) = QSW(:,:,LM) - (UVR*exp(-KUVR*Z) + PAR*exp(-KPAR*Z))
   end if

   if(.not.associated(KPARX)) deallocate(KPAR)

   deallocate(Z   )
   deallocate(PAR )
   deallocate(UVR )

   if (associated(FRx))    FRX = FR
   if (associated(PRUVFx)) PRUVFX = PRUVF
   if (associated(PRPAFx)) PRPAFX = PRPAF
   if (associated(PRUVRx)) PRUVRX = PRUVR
   if (associated(PRPARx)) PRPARX = PRPAR

!  All done
!-----------

   call MAPL_TimerOff(MAPL,"RUN" )
   call MAPL_TimerOff(MAPL,"TOTAL")

   RETURN_(ESMF_SUCCESS)

 end subroutine RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GEOS_OradGridCompMod

