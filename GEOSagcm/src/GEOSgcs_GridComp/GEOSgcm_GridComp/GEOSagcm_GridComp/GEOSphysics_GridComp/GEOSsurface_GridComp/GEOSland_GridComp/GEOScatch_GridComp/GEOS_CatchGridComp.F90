!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
module GEOS_CatchGridCompMod

!BOP
! !MODULE: GEOS_Catch --- ESMF gridded component implementing Catchment LSM

! !DESCRIPTION:
!
!   {\tt Catch} is a gridded component to compute the energy and water
!   fluxes due to land-surface processes, using the Catchment LSM
!   of Koster et al. (2000). All of its calculations are done
!   in a tile space defined by the inherited location stream.
!   It has a two-stage run method. The first stage obtains
!   drag coefficients at all the subtiles and defines
!   effective tile-mean surface quantities. The second
!   stage calls the Catchment LSM. {\tt Catch} has no children.

!
! !USES:

  use sfclayer  ! using module that contains sfc layer code
  use ESMF
  use GEOS_Mod
  use DragCoefficientsMod
  use CATCHMENT_MODEL, ONLY :                 &
       catchment
  USE STIEGLITZSNOW,   ONLY :                 &
       snow_albedo
  USE CATCH_CONSTANTS, ONLY :                 &
       N_SNOW         => CATCH_N_SNOW,        &
       RHOFS          => CATCH_SNWALB_RHOFS,  &
       SNWALB_VISMAX  => CATCH_SNWALB_VISMAX, &
       SNWALB_NIRMAX  => CATCH_SNWALB_NIRMAX, &
       SLOPE          => CATCH_SNWALB_SLOPE,  &
       N_CONSTIT      => CATCH_N_CONSTIT
  USE MAPL_BaseMod
  USE lsm_routines, ONLY : sibalb
 
implicit none
private

! !PUBLIC MEMBER FUNCTIONS:

public SetServices

!
!EOP

integer,parameter :: FSAT=1  !  Saturated subtile
integer,parameter :: FTRN=2  !  Transition subtile
integer,parameter :: FWLT=3  !  Wilting subtile
integer,parameter :: FSNW=4  !  Snowcover subtile

integer,parameter :: NUM_SUBTILES=4

! Vegetation type as follows:
!                  1:  BROADLEAF EVERGREEN TREES
!                  2:  BROADLEAF DECIDUOUS TREES
!                  3:  NEEDLELEAF TREES
!                  4:  GROUND COVER
!                  5:  BROADLEAF SHRUBS
!                  6:  DWARF TREES (TUNDRA)
!===================================================
!ALT: we currently use only 6 types (see above)
!     in the legacy code we used to have 8 
!     (or 10 with the sea and land ice) with
!     these additional entries
!                  7:  BARE SOIL
!                  8:  DESERT


integer,parameter :: NTYPS = MAPL_NUMVEGTYPES

real,   parameter :: HPBL           = 1000.
real,   parameter :: MIN_VEG_HEIGHT = 0.01
real,   parameter :: Z0_BY_ZVEG     = 0.13
real,   parameter :: D0_BY_ZVEG     = 0.66

! Emissivity values from Wilber et al (1999, NATA-TP-1999-209362)
! Fu-Liou bands have been combined to Chou bands (though these are broadband only)
! IGBP veg types have been mapped to Sib-Mosaic types 
! Details in ~suarez/Emiss on cerebus

real,   parameter :: EMSVEG(NTYPS) = (/ 0.99560, 0.99000, 0.99560, 0.99320, &
                                        0.99280, 0.99180 /)
real,   parameter :: EMSBARESOIL   =    0.94120
real,   parameter :: EMSSNO        =    0.99999

! moved SURFLAY from catchment.F90 to enable run-time changes for off-line system
! - reichle, 29 Oct 2010

! real,   parameter :: SURFLAY = 20.  ! moved to GetResource in RUN2  LLT:12Jul3013

contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for component
! !INTERFACE:

subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),intent(INOUT) :: GC
    integer, optional,  intent(  OUT) :: RC

! !DESCRIPTION:
! This version uses GEOS\_GenericSetServices, overriding
! only the run method. It also relies on MAPL\_Generic to
! handle data services. 

!EOP
!
! ErrLog Variables

    character(len=ESMF_MAXSTR) :: Iam
    character(len=ESMF_MAXSTR) :: COMP_NAME
    integer                    :: STATUS

! Begin...
! --------

! Get my name and set-up traceback handle
! ------------------------------------------------------------------------------

    Iam='SetServices'
    call ESMF_GridCompGet ( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam=trim(COMP_NAME)//trim(Iam)

! Set the Run entry points
! ------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, RUN1, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, RUN2, RC=STATUS )
    VERIFY_(STATUS)


! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_pressure'            ,&
         UNITS              = 'Pa'                          ,&
         SHORT_NAME         = 'PS'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 

    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_air_temperature'     ,&
         UNITS              = 'K'                           ,&
         SHORT_NAME         = 'TA'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 

    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_air_specific_humidity',&
         UNITS              = 'kg kg-1'                     ,&
         SHORT_NAME         = 'QA'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_wind_speed'          ,&
         UNITS              = 'm s-1'                       ,&
         SHORT_NAME         = 'UU'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'levellm_uwind',                     &
        UNITS              = 'm s-1',                             &
        SHORT_NAME         = 'UWINDLMTILE',                       &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'levellm_vwind',                     &
        UNITS              = 'm s-1',                             &
        SHORT_NAME         = 'VWINDLMTILE',                       &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'liquid_water_convective_precipitation',&
         UNITS              = 'kg m-2 s-1'                  ,&
         SHORT_NAME         = 'PCU'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'liquid_water_large_scale_precipitation',&
         UNITS              = 'kg m-2 s-1'                  ,&
         SHORT_NAME         = 'PLS'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'snowfall'                    ,&
         UNITS              = 'kg m-2 s-1'                  ,&
         SHORT_NAME         = 'SNO'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_par_beam_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DRPAR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_par_diffuse_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DFPAR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_nir_beam_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DRNIR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_nir_diffuse_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DFNIR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_uvr_beam_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DRUVR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_uvr_diffuse_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'DFUVR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_downwelling_longwave_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'LWDNSRF'                     ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'linearization_of_surface_upwelling_longwave_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'ALW'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'linearization_of_surface_upwelling_longwave_flux',&
         UNITS              = 'W_m-2 K-1'                   ,&
         SHORT_NAME         = 'BLW'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'leaf_area_index'             ,&
         UNITS              = '1'                           ,&
         SHORT_NAME         = 'LAI'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'greeness_fraction'           ,&
         UNITS              = '1'                           ,&
         SHORT_NAME         = 'GRN'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'evaporation'                 ,&
         UNITS              = 'kg m-2 s-1'                  ,&
         SHORT_NAME         = 'EVAP'                        ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'derivative_of_evaporation_wrt_QS',&
         UNITS              = 'kg m-2 s-1'                  ,&
         SHORT_NAME         = 'DEVAP'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'upward_sensible_heat_flux'   ,&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'SH'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'derivative_of_sensible_heat_wrt_Ts',&
         UNITS              = 'W m-2 K-1'                   ,&
         SHORT_NAME         = 'DSH'                         ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'surface_layer_height'        ,&
         UNITS              = 'm'                           ,&
         SHORT_NAME         = 'DZ'                          ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'vegetation_root_length'      ,&
         UNITS              = 'm'                           ,&
         SHORT_NAME         = 'ROOTL'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                         ,&
         LONG_NAME          = 'canopy_height'               ,&
         UNITS              = 'm'                           ,&
         SHORT_NAME         = 'Z2CH'                        ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  ) 
    VERIFY_(STATUS)
    
    call MAPL_AddImportSpec(GC,                         &
         SHORT_NAME         = 'THATM',                       &
         LONG_NAME          = 'effective_surface_skin_temperature',&
         UNITS              = 'K',                           &
         DIMS               = MAPL_DimsTileOnly,             &
         VLOCATION          = MAPL_VLocationNone,            &
                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                         &
         SHORT_NAME         = 'QHATM',                       &
         LONG_NAME          = 'effective_surface_specific_humidity',&
         UNITS              = 'kg kg-1',                     &
         DIMS               = MAPL_DimsTileOnly,             &
         VLOCATION          = MAPL_VLocationNone,            &
                                                  RC=STATUS  )
    VERIFY_(STATUS)
 
    call MAPL_AddImportSpec(GC,                         &
         SHORT_NAME         = 'CTATM',                       &
         LONG_NAME          = 'surface_exchange_coefficient_for_heat', &
         UNITS              = 'kg m-2 s-1',                  &
         DIMS               = MAPL_DimsTileOnly,             &
         VLOCATION          = MAPL_VLocationNone,            &
                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                         &
         SHORT_NAME         = 'CQATM',                       &
         LONG_NAME          = 'surface_exchange_coefficient_for_moisture', &
         UNITS              = 'kg m-2 s-1',                  &
         DIMS               = MAPL_DimsTileOnly,             &
         VLOCATION          = MAPL_VLocationNone,            &
                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                          ,&
       SHORT_NAME = 'ITY'                                     ,&
       LONG_NAME  = 'vegetation_type'			      ,&
       UNITS      = '1'                                       ,&
       DIMS       = MAPL_DimsTileOnly                         ,&
       VLOCATION  = MAPL_VLocationNone                        ,&
       RC=STATUS  )
    VERIFY_(STATUS)  

!  !INTERNAL STATE:

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'topo_baseflow_param_1'     ,&
    UNITS              = 'kg m-4'                         ,&
    SHORT_NAME         = 'BF1'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&

                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'topo_baseflow_param_2'     ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'BF2'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'topo_baseflow_param_3'     ,&
    UNITS              = 'log(m)'                    ,&
    SHORT_NAME         = 'BF3'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'max_rootzone_water_content',&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'VGWMAX'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'moisture_threshold'        ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'CDCR1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'max_water_content'         ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'CDCR2'                     ,&
    FRIENDLYTO         = trim(COMP_NAME)             ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'saturated_matric_potential',&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'PSIS'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'clapp_hornberger_b'        ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'BEE'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_porosity'             ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'POROS'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'wetness_at_wilting_point'  ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'WPWET'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'sfc_sat_hydraulic_conduct' ,&
    UNITS              = 'm s-1'                     ,&
    SHORT_NAME         = 'COND'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'vertical_transmissivity'   ,&
    UNITS              = 'm-1'                       ,&
    SHORT_NAME         = 'GNU'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'wetness_param_1'           ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARS1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'wetness_param_2'           ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARS2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'wetness_param_3'           ,&
    UNITS              = 'm+4 kg-2'                  ,&
    SHORT_NAME         = 'ARS3'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'shape_param_1'             ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARA1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'shape_param_2'             ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ARA2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'shape_param_3'             ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARA3'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'shape_param_4'             ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ARA4'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'min_theta_param_1'         ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARW1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'min_theta_param_2'         ,&
    UNITS              = 'm+2 kg-1'                  ,&
    SHORT_NAME         = 'ARW2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'min_theta_param_3'          ,&
    UNITS              = 'm+4 kg-2'                  ,&
    SHORT_NAME         = 'ARW3'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'min_theta_param_4'         ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ARW4'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_1'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'TSA1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_2'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'TSA2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_3'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'TSB1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_4'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'TSB2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_5'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ATAU'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'water_transfer_param_6'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'BTAU'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'Placeholder. Used to be vegetation_type.',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'OLD_ITY'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'canopy_temperature'        ,&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TC'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'canopy_specific_humidity'  ,&
    UNITS              = 'kg kg-1'                   ,&
    SHORT_NAME         = 'QC'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'interception_reservoir_capac',&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'CAPAC'                     ,&
    FRIENDLYTO         = trim(COMP_NAME)             ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'catchment_deficit'         ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'CATDEF'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'root_zone_excess'          ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'RZEXC'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'surface_excess'            ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'SRFEXC'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_1' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'GHTCNT1'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_2' ,&
    UNITS              = 'J_m-2'                     ,&
    SHORT_NAME         = 'GHTCNT2'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_3' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'GHTCNT3'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_4' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'GHTCNT4'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_5' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'GHTCNT5'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'soil_heat_content_layer_6' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'GHTCNT6'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'mean_catchment_temp_incl_snw',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TSURF'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_mass_layer_1'         ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'WESNN1'                    ,&
    FRIENDLYTO         = trim(COMP_NAME)             ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_mass_layer_2'         ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'WESNN2'                    ,&
    FRIENDLYTO         = trim(COMP_NAME)             ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_mass_layer_3'         ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'WESNN3'                    ,&
    FRIENDLYTO         = trim(COMP_NAME)             ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'heat_content_snow_layer_1' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'HTSNNN1'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'heat_content_snow_layer_2' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'HTSNNN2'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'heat_content_snow_layer_3' ,&
    UNITS              = 'J m-2'                     ,&
    SHORT_NAME         = 'HTSNNN3'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_depth_layer_1'        ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'SNDZN1'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_depth_layer_2'        ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'SNDZN2'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'snow_depth_layer_3'        ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'SNDZN3'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'surface_heat_exchange_coefficient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CH'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'surface_momentum_exchange_coefficient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CM'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'surface_moisture_exchange_coffiecient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CQ'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC                  ,&
    LONG_NAME          = 'subtile_fractions'         ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'FR'                        ,&
    DIMS               = MAPL_DimsTileTile           ,&
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone          ,&
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddInternalSpec(GC,                  &
    SHORT_NAME         = 'WW',                        &
    LONG_NAME          = 'vertical_velocity_scale_squared', &
    UNITS              = 'm+2 s-2',                   &
    DIMS               = MAPL_DimsTileTile,           &
    NUM_SUBTILES       = NUM_SUBTILES                ,&
    VLOCATION          = MAPL_VLocationNone,          &
    RESTART            = MAPL_RestartRequired        ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)


!  !EXPORT STATE:

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'evaporation'               ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'EVAPOUT'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'sublimation'               ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'SUBLIM'                    ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'upward_sensible_heat_flux' ,&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'SHOUT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'runoff_flux'               ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'RUNOFF'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'interception_loss_energy_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'EVPINT'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'baresoil_evap_energy_flux' ,&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'EVPSOI'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'transpiration_energy_flux' ,&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'EVPVEG'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'snow_ice_evaporation_energy_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'EVPICE'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil moisture in Upper 10cm'     ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'WAT10CM'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'totoal soil moisture'      ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'WATSOI'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil frozen water content' ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'ICESOI'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'snowpack_evaporation_energy_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'EVPSNO'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)
  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'baseflow_flux'             ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'BASEFLOW'                  ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'overland_runoff_including_throughflow'  ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'RUNSURF'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'snowmelt_flux'             ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'SMELT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_outgoing_longwave_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'HLWUP'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    LONG_NAME          = 'surface_net_downward_longwave_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'LWNDSRF'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
    VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    LONG_NAME          = 'surface_net_downward_shortwave_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'SWNDSRF'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
    VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'total_latent_energy_flux'  ,&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'HLATN'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'rainwater_infiltration_flux',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'QINFIL'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'areal_fraction_saturated_zone',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'AR1'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'areal_fraction_transpiration_zone',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'AR2'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'root_zone_equilibrium_moisture',&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'RZEQ'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'ground_energy_flux'        ,&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'GHFLX'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'ave_catchment_temp_incl_snw',& 
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TPSURF'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'temperature_top_snow_layer',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TPSNOW'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'temperature_unsaturated_zone',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TPUNST'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'temperature_saturated_zone',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TPSAT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'temperature_wilted_zone'   ,&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TPWLT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'fractional_area_of_land_snowcover',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ASNOW'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'downward_heat_flux_into_snow',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'SHSNOW'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'averaged_snow_temperature' ,&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'AVETSNOW'                  ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'fractional_area_of_saturated_zone',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'FRSAT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'fractional_area_of_unsaturated_zone',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'FRUST'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'fractional_area_of_wilting_zone',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'FRWLT'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'snow_mass'                 ,&
    UNITS              = 'kg m-2'                    ,&
    SHORT_NAME         = 'SNOWMASS'                  ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'snow_depth'                ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'SNOWDP'                    ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_soil_wetness'      ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'WET1'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'root_zone_soil_wetness'    ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'WET2'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'ave_prof_soil__moisture'   ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'WET3'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'water_surface_layer'       ,&
    UNITS              = 'm3 m-3'                    ,&
    SHORT_NAME         = 'WCSF'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'water_root_zone'           ,&
    UNITS              = 'm3 m-3'                    ,&
    SHORT_NAME         = 'WCRZ'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'water_ave_prof'            ,&
    UNITS              = 'm3 m-3'                   ,&
    SHORT_NAME         = 'WCPR'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_1' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP1'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_2' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP2'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_3' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP3'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_4' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP4'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_5' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP5'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'soil_temperatures_layer_6' ,&
    UNITS              = 'C'                         ,&
    SHORT_NAME         = 'TP6'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_emissivity'        ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'EMIS'                      ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_albedo_visible_beam',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ALBVR'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_albedo_visible_diffuse',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ALBVF'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_albedo_near_infrared_beam',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ALBNR'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_albedo_near_infrared_diffuse',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ALBNF'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'change_surface_skin_temperature',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'DELTS'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'change_surface_specific_humidity',&
    UNITS              = 'kg kg-1'                   ,&
    SHORT_NAME         = 'DELQS'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'change_evaporation'        ,&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'DELEVAP'                   ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'change_upward_sensible_energy_flux',&
    UNITS              = 'W m-2'                     ,&
    SHORT_NAME         = 'DELSH'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_skin_temperature'  ,&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TST'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'land_surface_skin_temperature'  ,&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'LST'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_specific_humidity' ,&
    UNITS              = 'kg kg-1'                   ,&
    SHORT_NAME         = 'QST'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'turbulence_surface_skin_temperature',&
    UNITS              = 'K'                         ,&
    SHORT_NAME         = 'TH'                        ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'turbulence_surface_skin_specific_hum',&
    UNITS              = 'kg kg-1'                   ,&
    SHORT_NAME         = 'QH'                        ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_heat_exchange_coefficient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CHT'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_momentum_exchange_coefficient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CMT'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_moisture_exchange_coefficient',&
    UNITS              = 'kg m-2 s-1'                ,&
    SHORT_NAME         = 'CQT'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'neutral_drag_coefficient'  ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'CNT'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_bulk_richardson_number',&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'RIT'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_roughness'         ,&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'Z0'                        ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOT2M',                     &
        LONG_NAME          = 'temperature 2m wind from MO sfc', &
        UNITS              = 'K',                         &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOQ2M',                     &
        LONG_NAME          = 'humidity 2m wind from MO sfc',    &
        UNITS              = 'kg kg-1',                   &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOU2M',                    &
        LONG_NAME          = 'zonal 2m wind from MO sfc',&
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOV2M',                    &
        LONG_NAME          = 'meridional 2m wind from MO sfc', &
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOT10M',                     &
        LONG_NAME          = 'temperature 10m wind from MO sfc', &
        UNITS              = 'K',                         &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOQ10M',                     &
        LONG_NAME          = 'humidity 10m wind from MO sfc',    &
        UNITS              = 'kg kg-1',                   &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOU10M',                    &
        LONG_NAME          = 'zonal 10m wind from MO sfc',&
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOV10M',                    &
        LONG_NAME          = 'meridional 10m wind from MO sfc', &
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOU50M',                    &
        LONG_NAME          = 'zonal 50m wind from MO sfc',&
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        SHORT_NAME         = 'MOV50M',                    &
        LONG_NAME          = 'meridional 50m wind from MO sfc', &
        UNITS              = 'm s-1',                     &
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone,          &
                                               RC=STATUS  )
     VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'surface_roughness_for_heat',&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'Z0H'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    LONG_NAME          = 'zero_plane_displacement_height',&
    UNITS              = 'm'                         ,&
    SHORT_NAME         = 'D0'                        ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'GUST',                      &
    LONG_NAME          = 'gustiness',                 &
    UNITS              = 'm s-1',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'VENT',                      &
    LONG_NAME          = 'surface_ventilation_velocity',&
    UNITS              = 'm s-1',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'ACCUM',                             &
        LONG_NAME          = 'net_ice_accumulation_rate',         &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'EVLAND',                    &
    LONG_NAME          = 'Evaporation_land',          &
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'PRLAND',                    &
    LONG_NAME          = 'Total_precipitation_land',  &
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SNOLAND',                   &
    LONG_NAME          = 'snowfall_land',             &
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'DRPARLAND',                 &
    LONG_NAME          = 'surface_downwelling_par_beam_flux', &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'DFPARLAND',                 &
    LONG_NAME          = 'surface_downwelling_par_diffuse_flux', &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'LHSNOW',                    &
    LONG_NAME          = 'Latent_heat_flux_snow',     &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SWNETSNOW',                    &
    LONG_NAME          = 'Net_shortwave_snow',        &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'LWUPSNOW',                    &
    LONG_NAME          = 'Net_longwave_snow',         &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'LWDNSNOW',                    &
    LONG_NAME          = 'Net_longwave_snow',         &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TCSORIG',                   &
    LONG_NAME          = 'Input_tc_for_snow',         &
    UNITS              = 'K',                         &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TPSN1IN',                   &
    LONG_NAME          = 'Input_temp_of_top_snow_lev',&
    UNITS              = 'K',                         &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TPSN1OUT',                  &
    LONG_NAME          = 'Output_temp_of_top_snow_lev',&
    UNITS              = 'K',                         &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'GHSNOW',                    &
    LONG_NAME          = 'Ground_heating_snow',       &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'LHLAND',                    &
    LONG_NAME          = 'Latent_heat_flux_land',     &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SHLAND',                    &
    LONG_NAME          = 'Sensible_heat_flux_land',   &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SWLAND',                    &
    LONG_NAME          = 'Net_shortwave_land',        &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SWDOWNLAND',                &
    LONG_NAME          = 'Incident_shortwave_land',   &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'LWLAND',                    &
    LONG_NAME          = 'Net_longwave_land',         &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'GHLAND',                    &
    LONG_NAME          = 'Ground_heating_land',       &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'GHTSKIN',                   &
    LONG_NAME          = 'Ground_heating_skin_temp',  &
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SMLAND',                    &
    LONG_NAME          = 'Snowmelt_flux_land',        &
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TWLAND',                    &
    LONG_NAME          = 'Avail_water_storage_land',  &
    UNITS              = 'kg m-2',                    &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TELAND',                    &
    LONG_NAME          = 'Total_energy_storage_land', &
    UNITS              = 'J m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'TSLAND',                    &
    LONG_NAME          = 'Total_snow_storage_land',   &
    UNITS              = 'kg m-2',                    &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'DWLAND',                    &
    LONG_NAME          = 'rate_of_change_of_total_land_water',&
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'DHLAND',                    &
    LONG_NAME          = 'rate_of_change_of_total_land_energy',&
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SPLAND',                    &
    LONG_NAME          = 'rate_of_spurious_land_energy_source',&
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SPWATR',                    &
    LONG_NAME          = 'rate_of_spurious_land_water_source',&
    UNITS              = 'kg m-2 s-1',                &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
    SHORT_NAME         = 'SPSNOW',                    &
    LONG_NAME          = 'rate_of_spurious_snow_energy',&
    UNITS              = 'W m-2',                     &
    DIMS               = MAPL_DimsTileOnly,           &
    VLOCATION          = MAPL_VLocationNone,          &
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                         ,&
    LONG_NAME          = 'vegetation_type'           ,&
    UNITS              = '1'                         ,&
    SHORT_NAME         = 'ITY'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)



!EOS

    call MAPL_TimerAdd(GC,    name="RUN1"  ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-SURF" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="RUN2"  ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-CATCH" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-ALBEDO" ,RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final method
! ---------------------------------

    call MAPL_GenericSetServices ( GC, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)

end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
! !IROUTINE: RUN1 -- First Run stage for the catchment component
! !INTERFACE:

subroutine RUN1 ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),intent(inout) :: GC     !Gridded component
    type(ESMF_State),   intent(inout) :: IMPORT !Import state
    type(ESMF_State),   intent(inout) :: EXPORT !Export state
    type(ESMF_Clock),   intent(inout) :: CLOCK  !The clock
    integer,optional,   intent(out  ) :: RC     !Error code:

! !DESCRIPTION: Does the cds computation and roughness length
!EOP
! ErrLog Variables

    character(len=ESMF_MAXSTR) :: IAm
    integer :: STATUS

    character(len=ESMF_MAXSTR) :: COMP_NAME

! Locals

    type(MAPL_MetaComp),pointer :: MAPL
    type(ESMF_State)                :: INTERNAL
    type(ESMF_Alarm)                :: ALARM
    type(ESMF_Config)               :: CF

! -----------------------------------------------------
! IMPORT Pointers
! ----------------------------------------------------  -

    real, dimension(:),     pointer :: ITY
    real, dimension(:),     pointer :: PS
    real, dimension(:),     pointer :: TA
    real, dimension(:),     pointer :: QA
    real, dimension(:),     pointer :: UU
    real, pointer, dimension(:)    :: UWINDLMTILE
    real, pointer, dimension(:)    :: VWINDLMTILE
    real, dimension(:),     pointer :: DZ
    real, dimension(:),     pointer :: LAI
    real, dimension(:),     pointer :: Z2CH
    real, dimension(:),     pointer :: PCU

! -----------------------------------------------------
! INTERNAL Pointers
! -----------------------------------------------------

    real, dimension(:,:), pointer :: TC
    real, dimension(:,:), pointer :: QC
    real, dimension(:,:), pointer :: CH
    real, dimension(:,:), pointer :: CM
    real, dimension(:,:), pointer :: CQ
    real, dimension(:,:), pointer :: FR
    real, dimension(:,:), pointer :: WW

! -----------------------------------------------------
! EXPORT Pointers
! -----------------------------------------------------

    real, dimension(:),   pointer :: TH
    real, dimension(:),   pointer :: QH
    real, dimension(:),   pointer :: CHT
    real, dimension(:),   pointer :: CMT
    real, dimension(:),   pointer :: CQT
    real, dimension(:),   pointer :: CNT
    real, dimension(:),   pointer :: RIT
    real, dimension(:),   pointer :: Z0
    real, dimension(:),   pointer :: Z0H
    real, dimension(:),   pointer :: D0
    real, dimension(:),   pointer :: GST
    real, dimension(:),   pointer :: VNT
   real, pointer, dimension(:  )  :: MOT2M
   real, pointer, dimension(:  )  :: MOQ2M
   real, pointer, dimension(:  )  :: MOU2M
   real, pointer, dimension(:  )  :: MOV2M
   real, pointer, dimension(:  )  :: MOT10M
   real, pointer, dimension(:  )  :: MOQ10M
   real, pointer, dimension(:  )  :: MOU10M
   real, pointer, dimension(:  )  :: MOV10M
   real, pointer, dimension(:  )  :: MOU50M
   real, pointer, dimension(:  )  :: MOV50M
    real, dimension(:),   pointer :: ITYO


! From old bucket version of CDS calculation
! ------------------------------------------

    integer             :: N
    integer             :: NT
    real,   allocatable :: UCN(:)
    real,   allocatable :: TVA(:)
    real,   allocatable :: TVS(:)
    real,   allocatable :: URA(:)
    real,   allocatable :: UUU(:)
    real,   allocatable :: ZVG(:)
    real,   allocatable :: DZE(:)
    real,   allocatable :: D0T(:)
    real,   allocatable :: CHX(:)
    real,   allocatable :: CQX(:)
    real,   allocatable :: CN(:)
    real,   allocatable :: RE(:)
    real,   allocatable :: ZT(:)
    real,   allocatable :: ZQ(:)
    integer,allocatable :: VEG(:)
    real,   allocatable :: Z0T(:,:)
   real, allocatable              :: U50M (:)
   real, allocatable              :: V50M (:)
   real, allocatable              :: T10M (:)
   real, allocatable              :: Q10M (:)
   real, allocatable              :: U10M (:)
   real, allocatable              :: V10M (:)
   real, allocatable              :: T2M (:)
   real, allocatable              :: Q2M (:)
   real, allocatable              :: U2M (:)
   real, allocatable              :: V2M (:)
    real, allocatable              :: RHOH(:)
    real, allocatable              :: VKH(:)
    real, allocatable              :: VKM(:)
    real, allocatable              :: USTAR(:)
    real, allocatable              :: XX(:)
    real, allocatable              :: YY(:)
    real, allocatable              :: CU(:)
    real, allocatable              :: CT(:)
    real, allocatable              :: RIB(:)
    real, allocatable              :: ZETA(:)
    real, allocatable              :: WS(:)
    integer, allocatable           :: IWATER(:)
    real, allocatable              :: PSMB(:)
    real, allocatable              :: PSL(:)
    integer                        :: niter

    integer                        :: CHOOSEMOSFC
    integer                        :: CHOOSEZ0
    real                           :: SCALE4Z0
!=============================================================================
! Begin...
! ------------------------------------------------------------------------------


! ------------------------------------------------------------------------------
! Get the target component's name and set-up traceback handle.
! ------------------------------------------------------------------------------

    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam=trim(COMP_NAME)//"RUN1"

! Get my internal MAPL_Generic state
! ----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS )
    VERIFY_(STATUS)

! Start timers
! ------------

    call MAPL_TimerOn(MAPL,"TOTAL")
    call MAPL_TimerOn(MAPL,"RUN1")

! Get parameters from generic state
! ---------------------------------

    call MAPL_Get ( MAPL                          ,&
                                INTERNAL_ESMF_STATE=INTERNAL   ,&
                                                      RC=STATUS )
    VERIFY_(STATUS)

! Get parameters (0:Louis, 1:Monin-Obukhov)
! -----------------------------------------
    call MAPL_GetResource ( MAPL, CHOOSEMOSFC, Label="CHOOSEMOSFC:", DEFAULT=1, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, CHOOSEZ0, Label="CHOOSEZ0:", DEFAULT=3, RC=STATUS)
    VERIFY_(STATUS)

!   call MAPL_GetResource ( MAPL, SCALE4Z0, Label="SCALE4Z0:", DEFAULT=1.0, RC=STATUS)  ! Pre Ganymed-4_1 value
    call MAPL_GetResource ( MAPL, SCALE4Z0, Label="SCALE4Z0:", DEFAULT=2.0, RC=STATUS)  ! H5_0 Zero-diff
!    call MAPL_GetResource ( MAPL, SCALE4Z0, Label="SCALE4Z0:", DEFAULT=0.5, RC=STATUS) ! Post H5_0
    VERIFY_(STATUS)

! Pointers to inputs
!-------------------

   call MAPL_GetPointer(IMPORT,UU     , 'UU'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UWINDLMTILE     , 'UWINDLMTILE'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VWINDLMTILE     , 'VWINDLMTILE'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DZ     , 'DZ'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,TA     , 'TA'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,QA     , 'QA'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PS     , 'PS'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,LAI    , 'LAI'    ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,Z2CH   , 'Z2CH'   ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PCU    , 'PCU'    ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,ITY    , 'ITY'    ,    RC=STATUS)
   VERIFY_(STATUS)

! Pointers to internals
!----------------------
 
   call MAPL_GetPointer(INTERNAL,TC   , 'TC'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,QC   , 'QC'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,FR   , 'FR'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CH   , 'CH'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CM   , 'CM'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CQ   , 'CQ'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,WW   , 'WW'     ,    RC=STATUS)
   VERIFY_(STATUS)

! Pointers to outputs
!--------------------

   call MAPL_GetPointer(EXPORT,QH    , 'QH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TH    , 'TH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,CHT   , 'CHT'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,CMT   , 'CMT'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,CQT   , 'CQT'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,CNT   , 'CNT'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,RIT   , 'RIT'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Z0    , 'Z0'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Z0H   , 'Z0H'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,D0    , 'D0'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,GST   , 'GUST'    ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,VNT   , 'VENT'    ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOT2M, 'MOT2M'   ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOQ2M, 'MOQ2M'   ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOU2M, 'MOU2M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOV2M, 'MOV2M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOT10M, 'MOT10M'   ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOQ10M, 'MOQ10M'   ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOU10M, 'MOU10M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOV10M, 'MOV10M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOU50M, 'MOU50M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,MOV50M, 'MOV50M'  ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ITYO  , 'ITY'     ,    RC=STATUS)
   VERIFY_(STATUS)

   NT = size(TA)

   allocate(TVA(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(TVS(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(URA(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(UUU(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VEG(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(DZE(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZVG(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(Z0T(NT,NUM_SUBTILES),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(D0T(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CHX(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CQX(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(RE (NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CN (NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZT (NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZQ (NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(UCN(NT),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(T2M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(Q2M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U2M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(v2M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(T10M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(Q10M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U10M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(v10M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U50M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(v50M (NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(RHOH(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(PSMB(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(PSL(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VKH(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VKM(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(USTAR(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(XX(NT)   ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(YY(NT)   ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CU(NT)   ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CT(NT)   ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(RIB(NT)  ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZETA(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(WS(NT)   ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(IWATER(NT),STAT=STATUS)
   VERIFY_(STATUS)

!  Vegetation types used to index into tables
!--------------------------------------------

   VEG = nint(ITY(:))
   ASSERT_((count(VEG>NTYPS.or.VEG<1)==0))

!  Clear the output tile accumulators
!------------------------------------
   
   CHX = 0.0
   CQX = 0.0

   if(associated(TH )) TH  = 0.0
   if(associated(QH )) QH  = 0.0
   if(associated(CMT)) CMT = 0.0
   if(associated(CNT)) CNT = 0.0
   if(associated(RIT)) RIT = 0.0
   if(associated(Z0H)) Z0H = 0.0
   if(associated(GST)) GST = 0.0
   if(associated(VNT)) VNT = 0.0
   if(associated(MOU50M)) MOU50M = 0.0
   if(associated(MOV50M)) MOV50M = 0.0
   if(associated(MOT10M)) MOT10M = 0.0
   if(associated(MOQ10M)) MOQ10M = 0.0
   if(associated(MOU10M)) MOU10M = 0.0
   if(associated(MOV10M)) MOV10M = 0.0
   if(associated( MOT2M))  MOT2M = 0.0
   if(associated( MOQ2M))  MOQ2M = 0.0
   if(associated( MOU2M))  MOU2M = 0.0
   if(associated( MOV2M))  MOV2M = 0.0

   SUBTILES: do N=1,NUM_SUBTILES

!  Effective vegetation height. In catchment, LAI dependence 
!   includes the effect of partially vegetated areas,
!   as well as the phenology of the deciduous types. These
!   effects will be separated in future formulations.
    ZVG  = Z2CH - (Z2CH - MIN_VEG_HEIGHT)*exp(-LAI)          ! H5_0 Zero-diff
!   ZVG  = Z2CH - SCALE4Z0*(Z2CH - MIN_VEG_HEIGHT)*exp(-LAI) ! Post H5_0

!  For now roughnesses and displacement heights
!   are the same for all subtiles.
   Z0T(:,N)  = Z0_BY_ZVEG*ZVG*SCALE4Z0   ! H5_0 Zero-diff
!   Z0T(:,N)  = Z0_BY_ZVEG*ZVG           ! Post H5_0
   D0T  = D0_BY_ZVEG*ZVG

   DZE  = max(DZ - D0T, 10.)

   if(associated(Z0 )) Z0  = Z0T(:,N)
   if(associated(D0 )) D0  = D0T

!  Compute the three surface exchange coefficients
!-------------------------------------------------

! Choose sfc layer: if CHOOSEMOSFC is 1, choose helfand MO,
!                   if CHOOSEMOSFC is 0 (default), choose louis

   call MAPL_TimerOn(MAPL,"-SURF")
   if(CHOOSEMOSFC.eq.0) then

    call louissurface(3,N,UU,WW,PS,TA,TC,QA,QC,PCU,LAI,Z0T,DZE,CM,CN,RIB,ZT,ZQ,CH,CQ,UUU,UCN,RE)

   elseif (CHOOSEMOSFC.eq.1)then
  
    niter = 6   ! number of internal iterations in the helfand MO surface layer routine
    IWATER = 3
  
    PSMB = PS * 0.01            ! convert to MB
! Approximate pressure at top of surface layer: hydrostatic, eqn of state using avg temp and press
    PSL = PSMB * (1. - (DZE*MAPL_GRAV)/(MAPL_RGAS*(TA+TC(:,N)) ) ) /   &
               (1. + (DZE*MAPL_GRAV)/(MAPL_RGAS*(TA+TC(:,N)) ) )
  
    CALL helfsurface( UWINDLMTILE,VWINDLMTILE,TA,TC(:,N),QA,QC(:,N),PSL,PSMB,Z0T(:,N),lai,  &
                      IWATER,DZE,niter,nt,RHOH,VKH,VKM,USTAR,XX,YY,CU,CT,RIB,ZETA,WS,  &
                      t2m,q2m,u2m,v2m,t10m,q10m,u10m,v10m,u50m,v50m,CHOOSEZ0)
  
    CM(:,N)  = VKM
    CH(:,N)  = VKH
    CQ(:,N)  = VKH
  
    CN = (MAPL_KARMAN/ALOG(DZE/Z0T(:,N) + 1.0)) * (MAPL_KARMAN/ALOG(DZE/Z0T(:,N) + 1.0))
    ZT = Z0T(:,N)
    ZQ = Z0T(:,N)
    RE = 0.
    UUU = UU  
    UCN = 0.
  
!  Aggregate to tiles for MO only diagnostics
!--------------------------------------------
      if(associated(MOU50M))MOU50M = MOU50M + U50M(:)*FR(:,N)
      if(associated(MOV50M))MOV50M = MOV50M + V50M(:)*FR(:,N)
      if(associated(MOT10M))MOT10M = MOT10M + T10M(:)*FR(:,N)
      if(associated(MOQ10M))MOQ10M = MOQ10M + Q10M(:)*FR(:,N)
      if(associated(MOU10M))MOU10M = MOU10M + U10M(:)*FR(:,N)
      if(associated(MOV10M))MOV10M = MOV10M + V10M(:)*FR(:,N)
      if(associated(MOT2M))MOT2M = MOT2M + T2M(:)*FR(:,N)
      if(associated(MOQ2M))MOQ2M = MOQ2M + Q2M(:)*FR(:,N)
      if(associated(MOU2M))MOU2M = MOU2M + U2M(:)*FR(:,N)
      if(associated(MOV2M))MOV2M = MOV2M + V2M(:)*FR(:,N)

    endif
    call MAPL_TimerOff(MAPL,"-SURF")

!  Aggregate to tile
!-------------------

      CHX     = CHX + CH(:,N)*FR(:,N)
      CQX     = CQX + CQ(:,N)*FR(:,N)

      if(associated(CMT)) CMT     = CMT + CM(:,N)        *FR(:,N)
      if(associated(CNT)) CNT     = CNT + CN(:  )        *FR(:,N)
      if(associated(RIT)) RIT     = RIT + RIB(:  )       *FR(:,N)
      if(associated( TH)) TH      = TH  + CH(:,N)*TC(:,N)*FR(:,N)
      if(associated( QH)) QH      = QH  + CQ(:,N)*QC(:,N)*FR(:,N)
      if(associated(Z0H)) Z0H     = Z0H + ZT             *FR(:,N)
      if(associated(GST)) GST     = GST + WW(:,N)        *FR(:,N)
      if(associated(VNT)) VNT     = VNT + UUU            *FR(:,N)

      WW(:,N) = max(CH(:,N)*(TC(:,N)-TA-(MAPL_GRAV/MAPL_CP)*DZE)/TA + MAPL_VIREPS*CQ(:,N)*(QC(:,N)-QA),0.0)
      WW(:,N) = (HPBL*MAPL_GRAV*WW(:,N))**(2./3.)

   end do SUBTILES

   if(associated( TH)) TH  = TH /CHX
   if(associated( QH)) QH  = QH /CQX
   if(associated(CHT)) CHT = CHX
   if(associated(CQT)) CQT = CQX
   if(associated(GST)) GST = sqrt(max(GST+UCN,0.0))
   if(associated(ITYO)) ITYO = ITY

   deallocate(TVA)
   deallocate(TVS)
   deallocate(URA)
   deallocate(UUU)
   deallocate(ZVG)
   deallocate(DZE)
   deallocate(Z0T)
   deallocate(D0T)
   deallocate(CHX)
   deallocate(CQX)
   deallocate(VEG)
   deallocate(RE )
   deallocate(CN )
   deallocate(ZT )
   deallocate(ZQ )
   deallocate(UCN)
   deallocate(U50M )
   deallocate(V50M )
   deallocate(T10M )
   deallocate(Q10M )
   deallocate(U10M )
   deallocate(V10M )
   deallocate(T2M )
   deallocate(Q2M )
   deallocate(U2M )
   deallocate(V2M )
   deallocate(RHOH)
   deallocate(VKH)
   deallocate(VKM)
   deallocate(USTAR)
   deallocate(XX)
   deallocate(YY)
   deallocate(CU)
   deallocate(CT)
   deallocate(RIB)
   deallocate(ZETA)
   deallocate(WS)
   deallocate(IWATER)
   deallocate(PSMB)
   deallocate(PSL)

!  All done
! ------------------------------------------------------------------------------

    call MAPL_TimerOff ( MAPL, "RUN1"  )
    call MAPL_TimerOff ( MAPL, "TOTAL" )

    RETURN_(ESMF_SUCCESS)

end subroutine RUN1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

subroutine RUN2 ( GC, IMPORT, EXPORT, CLOCK, RC )

! ------------------------------------------------------------------------------
! !ARGUMENTS:
! ------------------------------------------------------------------------------

    type(ESMF_GridComp),intent(inout) :: GC
    type(ESMF_State),   intent(inout) :: IMPORT
    type(ESMF_State),   intent(inout) :: EXPORT
    type(ESMF_Clock),   intent(inout) :: CLOCK
    integer,optional,   intent(out  ) :: RC

! ------------------------------------------------------------------------------
! ErrLog Variables
! ------------------------------------------------------------------------------

    character(len=ESMF_MAXSTR) :: Iam="RUN2"
    integer :: STATUS
    character(len=ESMF_MAXSTR) :: COMP_NAME

! ------------------------------------------------------------------------------
! Local derived type aliases
! ------------------------------------------------------------------------------

    type(MAPL_MetaComp),pointer :: MAPL
    type(ESMF_Alarm)                :: ALARM

    integer :: IM,JM
    real    :: SURFLAY              ! Default (Ganymed-3 and earlier) SURFLAY=20.0 for Old Soil Params
                                    !         (Ganymed-4 and later  ) SURFLAY=50.0 for New Soil Params
    real    :: SCALE4Z0

! ------------------------------------------------------------------------------
! Begin: Get the target components name and
! set-up traceback handle.
! ------------------------------------------------------------------------------

    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam=trim(COMP_NAME)//trim(Iam)

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Get parameters from generic state.
!-----------------------------------

    call MAPL_Get(MAPL, RUNALARM=ALARM, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, SURFLAY, Label="SURFLAY:", DEFAULT=50.0, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, SCALE4Z0, Label="SCALE4Z0:", DEFAULT=2.0, RC=STATUS)  ! H5_0 Zero-diff
!    call MAPL_GetResource ( MAPL, SCALE4Z0, Label="SCALE4Z0:", DEFAULT=0.5, RC=STATUS) ! Post H5_0
    VERIFY_(STATUS)

! ------------------------------------------------------------------------------
! If its time, recalculate the LSM tile routine
! ------------------------------------------------------------------------------

    call MAPL_TimerOn ( MAPL,"TOTAL" )
    call MAPL_TimerOn ( MAPL,"RUN2"  )

    if(ESMF_AlarmIsRinging(ALARM, RC=STATUS))then
       call ESMF_AlarmRingerOff(ALARM, RC=STATUS)
       VERIFY_(STATUS)
       call Driver ( RC=STATUS )
       VERIFY_(STATUS)
    endif

    call MAPL_TimerOff ( MAPL, "RUN2"  )
    call MAPL_TimerOff ( MAPL, "TOTAL" )

    RETURN_(ESMF_SUCCESS)

    contains

! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------

      subroutine Driver ( RC )
        integer,optional,intent(OUT) :: RC

        character(len=ESMF_MAXSTR) :: IAm
        integer :: STATUS

        ! --------------------------------------------------------------------------
        ! Local derived type aliases
        ! --------------------------------------------------------------------------

        type(ESMF_STATE) :: INTERNAL

        ! -----------------------------------------------------
        ! IMPORT Pointers
        ! -----------------------------------------------------

        real, dimension(:),   pointer :: PS
        real, dimension(:),   pointer :: TA
        real, dimension(:),   pointer :: QA
        real, dimension(:),   pointer :: UU
        real, dimension(:),   pointer :: DZ
        real, dimension(:),   pointer :: PCU
        real, dimension(:),   pointer :: PLS
        real, dimension(:),   pointer :: SNO
        real, dimension(:),   pointer :: THATM
        real, dimension(:),   pointer :: QHATM
        real, dimension(:),   pointer :: CTATM
        real, dimension(:),   pointer :: CQATM

        real, dimension(:),   pointer :: drpar
        real, dimension(:),   pointer :: dfpar
        real, dimension(:),   pointer :: drnir
        real, dimension(:),   pointer :: dfnir
        real, dimension(:),   pointer :: druvr
        real, dimension(:),   pointer :: dfuvr
        real, dimension(:),   pointer :: lwdnsrf
        real, dimension(:),   pointer :: alw
        real, dimension(:),   pointer :: blw

        real, dimension(:),   pointer :: evap
        real, dimension(:),   pointer :: devap
        real, dimension(:),   pointer :: sh
        real, dimension(:),   pointer :: dsh

        real, dimension(:),   pointer :: ROOTL
        real, dimension(:),   pointer :: Z2CH
        real, dimension(:),   pointer :: LAI
        real, dimension(:),   pointer :: GRN
        real, dimension(:),   pointer :: ity

        ! -----------------------------------------------------
        ! INTERNAL Pointers
        ! -----------------------------------------------------

        real, dimension(:),   pointer :: bf1
        real, dimension(:),   pointer :: bf2
        real, dimension(:),   pointer :: bf3
        real, dimension(:),   pointer :: vgwmax
        real, dimension(:),   pointer :: cdcr1
        real, dimension(:),   pointer :: cdcr2
        real, dimension(:),   pointer :: psis
        real, dimension(:),   pointer :: bee
        real, dimension(:),   pointer :: poros
        real, dimension(:),   pointer :: wpwet
        real, dimension(:),   pointer :: cond
        real, dimension(:),   pointer :: gnu
        real, dimension(:),   pointer :: ars1
        real, dimension(:),   pointer :: ars2
        real, dimension(:),   pointer :: ars3
        real, dimension(:),   pointer :: ara1
        real, dimension(:),   pointer :: ara2
        real, dimension(:),   pointer :: ara3
        real, dimension(:),   pointer :: ara4
        real, dimension(:),   pointer :: arw1
        real, dimension(:),   pointer :: arw2
        real, dimension(:),   pointer :: arw3
        real, dimension(:),   pointer :: arw4
        real, dimension(:),   pointer :: tsa1
        real, dimension(:),   pointer :: tsa2
        real, dimension(:),   pointer :: tsb1
        real, dimension(:),   pointer :: tsb2
        real, dimension(:),   pointer :: atau
        real, dimension(:),   pointer :: btau
        real, dimension(:),   pointer :: old_ity
        real, dimension(:),   pointer :: capac
        real, dimension(:),   pointer :: catdef
        real, dimension(:),   pointer :: rzexc
        real, dimension(:),   pointer :: srfexc
        real, dimension(:),   pointer :: ghtcnt1
        real, dimension(:),   pointer :: ghtcnt2
        real, dimension(:),   pointer :: ghtcnt3
        real, dimension(:),   pointer :: ghtcnt4
        real, dimension(:),   pointer :: ghtcnt5
        real, dimension(:),   pointer :: ghtcnt6
        real, dimension(:),   pointer :: tsurf
        real, dimension(:),   pointer :: wesnn1
        real, dimension(:),   pointer :: wesnn2
        real, dimension(:),   pointer :: wesnn3
        real, dimension(:),   pointer :: htsnnn1
        real, dimension(:),   pointer :: htsnnn2
        real, dimension(:),   pointer :: htsnnn3
        real, dimension(:),   pointer :: sndzn1
        real, dimension(:),   pointer :: sndzn2
        real, dimension(:),   pointer :: sndzn3
        real, dimension(:,:), pointer :: tc
        real, dimension(:,:), pointer :: qc
        real, dimension(:,:), pointer :: ch
        real, dimension(:,:), pointer :: cm
        real, dimension(:,:), pointer :: cq
        real, dimension(:,:), pointer :: fr

        ! -----------------------------------------------------
        ! EXPORT Pointers
        ! -----------------------------------------------------

        real, dimension(:),   pointer :: evapout
        real, dimension(:),   pointer :: sublim
        real, dimension(:),   pointer :: shout
        real, dimension(:),   pointer :: runoff
        real, dimension(:),   pointer :: evpint
        real, dimension(:),   pointer :: evpsoi
        real, dimension(:),   pointer :: evpveg
        real, dimension(:),   pointer :: evpice
        real, dimension(:),   pointer :: evpsno
        real, dimension(:),   pointer :: bflow
        real, dimension(:),   pointer :: runsurf
        real, dimension(:),   pointer :: smelt
        real, dimension(:),   pointer :: accum
        real, dimension(:),   pointer :: hlwup
        real, dimension(:),   pointer :: swndsrf
        real, dimension(:),   pointer :: lwndsrf
        real, dimension(:),   pointer :: hlatn
        real, dimension(:),   pointer :: qinfil
        real, dimension(:),   pointer :: ar1
        real, dimension(:),   pointer :: ar2
        real, dimension(:),   pointer :: rzeq
        real, dimension(:),   pointer :: ghflx
        real, dimension(:),   pointer :: tpsurf
        real, dimension(:),   pointer :: tpsn1
        real, dimension(:),   pointer :: tpust
        real, dimension(:),   pointer :: tpsat
        real, dimension(:),   pointer :: tpwlt
        real, dimension(:),   pointer :: asnow
        real, dimension(:),   pointer :: frsat
        real, dimension(:),   pointer :: frust
        real, dimension(:),   pointer :: frwlt
        real, dimension(:),   pointer :: tp1
        real, dimension(:),   pointer :: tp2
        real, dimension(:),   pointer :: tp3
        real, dimension(:),   pointer :: tp4
        real, dimension(:),   pointer :: tp5
        real, dimension(:),   pointer :: tp6
        real, dimension(:),   pointer :: emis
        real, dimension(:),   pointer :: albvr
        real, dimension(:),   pointer :: albvf
        real, dimension(:),   pointer :: albnr
        real, dimension(:),   pointer :: albnf
        real, dimension(:),   pointer :: delts
        real, dimension(:),   pointer :: delqs
        real, dimension(:),   pointer :: delevap
        real, dimension(:),   pointer :: delsh
        real, dimension(:),   pointer :: tst
        real, dimension(:),   pointer :: lst
        real, dimension(:),   pointer :: qst

        real, dimension(:),   pointer :: WET1
        real, dimension(:),   pointer :: WET2
        real, dimension(:),   pointer :: WET3
        real, dimension(:),   pointer :: WCSF
        real, dimension(:),   pointer :: WCRZ
        real, dimension(:),   pointer :: WCPR
        real, dimension(:),   pointer :: SNOMAS
        real, dimension(:),   pointer :: SNOWDP

        real, dimension(:),   pointer :: EVLAND
        real, dimension(:),   pointer :: PRLAND
        real, dimension(:),   pointer :: SNOLAND
        real, dimension(:),   pointer :: DRPARLAND
        real, dimension(:),   pointer :: DFPARLAND
        real, dimension(:),   pointer :: LHSNOW
        real, dimension(:),   pointer :: SWNETSNOW1
        real, dimension(:),   pointer :: LWUPSNOW
        real, dimension(:),   pointer :: LWDNSNOW
        real, dimension(:),   pointer :: TCSORIG
        real, dimension(:),   pointer :: TPSN1IN
        real, dimension(:),   pointer :: TPSN1OUT
        real, dimension(:),   pointer :: GHSNOW
        real, dimension(:),   pointer :: LHLAND
        real, dimension(:),   pointer :: SHLAND
        real, dimension(:),   pointer :: SWLAND
        real, dimension(:),   pointer :: SWDOWNLAND
        real, dimension(:),   pointer :: LWLAND
        real, dimension(:),   pointer :: GHLAND
        real, dimension(:),   pointer :: GHTSKIN
        real, dimension(:),   pointer :: SMLAND
        real, dimension(:),   pointer :: TWLAND
        real, dimension(:),   pointer :: TELAND
        real, dimension(:),   pointer :: TSLAND
        real, dimension(:),   pointer :: DWLAND
        real, dimension(:),   pointer :: DHLAND
        real, dimension(:),   pointer :: SPLAND
        real, dimension(:),   pointer :: SPWATR
        real, dimension(:),   pointer :: SPSNOW

        real, dimension(:),   pointer :: WAT10CM
        real, dimension(:),   pointer :: WATSOI
        real, dimension(:),   pointer :: ICESOI
        real, dimension(:),   pointer :: SHSNOW
        real, dimension(:),   pointer :: AVETSNOW

        ! --------------------------------------------------------------------------
        ! Local pointers for tile variables
        ! --------------------------------------------------------------------------

        INTEGER,pointer,dimension(:) :: CAT_ID
        real,pointer,dimension(:) :: dzsf
        real,pointer,dimension(:) :: swnetfree
        real,pointer,dimension(:) :: swnetsnow
        real,pointer,dimension(:) :: qa1
        real,pointer,dimension(:) :: qa2
        real,pointer,dimension(:) :: qa4
        real,pointer,dimension(:) :: tilezero
        real,pointer,dimension(:) :: zth
        real,pointer,dimension(:) :: lats
        real,pointer,dimension(:) :: lons
        real,pointer,dimension(:) :: slr
        real,pointer,dimension(:) :: rdc
	real,pointer,dimension(:) :: PRECU
	real,pointer,dimension(:) :: PRELS
	real,pointer,dimension(:) :: SNOW
	real,pointer,dimension(:) :: UUU, RHO
	real,pointer,dimension(:) :: LAI0,GRN0,ZVG
	real,pointer,dimension(:) :: Z0, D0
	real,pointer,dimension(:) :: sfmc, rzmc, prmc, entot, wtot
	real,pointer,dimension(:) :: ghflxsno, ghflxtskin
        real,pointer,dimension(:) :: SHSNOW1, AVETSNOW1, WAT10CM1, WATSOI1, ICESOI1
        real,pointer,dimension(:) :: LHSNOW1, LWUPSNOW1, LWDNSNOW1, NETSWSNOW
        real,pointer,dimension(:) :: TCSORIG1, TPSN1IN1, TPSN1OUT1
	real,pointer,dimension(:) :: WCHANGE, ECHANGE, HSNACC, EVACC, SHACC
	real,pointer,dimension(:) :: SNOVR, SNOVF, SNONR, SNONF
	real,pointer,dimension(:) :: VSUVR, VSUVF
	real,pointer,dimension(:) :: ALWX, BLWX
        real,pointer,dimension(:) :: LHACC, SUMEV

!       real*8,pointer,dimension(:) :: fsum

        real,pointer,dimension(:,:) :: ghtcnt
        real,pointer,dimension(:,:) :: wesnn
        real,pointer,dimension(:,:) :: htsnnn
        real,pointer,dimension(:,:) :: sndzn
        real,pointer,dimension(:,:) :: shsbt
        real,pointer,dimension(:,:) :: dshsbt
        real,pointer,dimension(:,:) :: evsbt
        real,pointer,dimension(:,:) :: devsbt
        real,pointer,dimension(:,:) :: CFT
        real,pointer,dimension(:,:) :: RA
        real,pointer,dimension(:,:) :: CFQ
        real,pointer,dimension(:,:) :: TCO
        real,pointer,dimension(:,:) :: QCO
        real,pointer,dimension(:,:) :: DQS
        real,pointer,dimension(:,:) :: QSAT

        integer,dimension(:),pointer :: veg

        ! --------------------------------------------------------------------------
        ! Locals for parameter lookup
        ! --------------------------------------------------------------------------

        ! vegetation calculations

        real,dimension(NTYPS) :: VGRF11
        real,dimension(NTYPS) :: VGRF12
        real,dimension(NTYPS) :: VGTR11
        real,dimension(NTYPS) :: VGTR12
        real,dimension(NTYPS) :: VGROCA
        real,dimension(NTYPS) :: VGROTD
        real,dimension(NTYPS) :: VGRDRS
        real,dimension(NTYPS) :: VGDDA, VGDDB, VGDDC
        real,dimension(NTYPS) :: VGRDA, VGRDB

        real,dimension(:),allocatable :: RSL1, RSL2
        real,dimension(:),allocatable :: SQSCAT

        ! albedo calculation stuff

        type(ESMF_Config)           :: CF
        type(MAPL_SunOrbit)         :: ORBIT
        type(ESMF_Time)             :: CURRENT_TIME
        type(ESMF_Time)             :: BEFORE
        type(ESMF_Time)             :: NOW
        type(ESMF_Time)             :: MODELSTART
        type(ESMF_Time)             :: AFTER
        type(ESMF_TimeInterval)     :: DELT
        type(ESMF_TimeInterval)     :: TINT
        real                        :: DT_SOLAR
        type(ESMF_Alarm)            :: SOLALARM
        logical                     :: solalarmison
        logical                     :: debugzth

        real,pointer,dimension(:)   :: VISDF
        real,pointer,dimension(:)   :: NIRDF
        character (len=ESMF_MAXSTR) :: VISDFFILE
        character (len=ESMF_MAXSTR) :: NIRDFFILE
        real                        :: FAC

        real,parameter              :: PRECIPFRAC=1.0
        real                        :: DT
        integer                     :: NTILES
        integer                     :: I, N

        integer                     :: OFFLINE_MODE
        logical                     :: OFFLINE

	! dummy variables for call to get snow temp

        real    :: FICE
        logical :: DUMFLAG1,DUMFLAG2
        logical, save                   :: check_ity = .true.
        integer                         :: nmax
        type(ESMF_VM)                   :: VM

! variables for call catch with choice of tile to print
        real                            :: lonbeg,lonend,latbeg,latend
#ifdef DBG_CATCH_INPUTS
        ! vars for debugging purposes
        type(ESMF_Grid)                 :: TILEGRID
        type (MAPL_LocStream)           :: LOCSTREAM
        integer, pointer                :: mask(:)
        integer                         :: nt
        integer, save                   :: unit_i=0
        logical, save                   :: firsttime=.true.
        integer                         :: unit
	integer 			:: NT_GLOBAL

#endif
        ! --------------------------------------------------------------------------
        ! Lookup tables
        ! --------------------------------------------------------------------------

        data VGRF11 / 0.100, 0.100, 0.070, 0.105, 0.100, 0.100 /
        data VGRF12 / 0.160, 0.160, 0.160, 0.360, 0.160, 0.160 /
        data VGTR11 / 0.050, 0.050, 0.050, 0.070, 0.050, 0.050 /
        data VGTR12 / 0.001, 0.001, 0.001, 0.220, 0.001, 0.001 /
        data VGROTD / 1.000, 1.000, 0.500, 0.500, 0.500, 0.200 / 

        data VGROCA / 0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6, 0.384E-6/
        data VGRDRS / 0.750E13, 0.750E13, 0.750E13, 0.400E13, 0.750E13, 0.750E13/

! Correction to RDC formulation -Randy Koster, 4/1/2011
!        data VGRDA / 285.9, 294.9, 652.9,  25.8,  100.7,  22.9,  23.8, 23.8/
!        data VGRDB / 5.1 ,  7.2, 10.8,  4.8,  1.8,  5.1,  .000, .000/

        data VGRDA / 285.9, 355.18, 660.24,  30.06,  100.7,  24.36/
        data VGRDB / 5.1 ,  7.2, 10.5,  4.8,  1.8,  5.1/

        ! Begin

        IAm=trim(COMP_NAME)//"Driver"

        ! --------------------------------------------------------------------------
        ! Get time step from configuration
        ! --------------------------------------------------------------------------

        call ESMF_GridCompGet  ( GC, CONFIG=CF, RC=STATUS )
        VERIFY_(STATUS)

        ! --------------------------------------------------------------------------
        ! Get my internal MAPL_Generic state
        ! --------------------------------------------------------------------------

        call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS )
        VERIFY_(STATUS)

        call MAPL_Get(MAPL, HEARTBEAT = DT, RC=STATUS)
        VERIFY_(STATUS)

        call ESMF_ConfigGetAttribute ( CF, DT                  ,&
             Label   = trim(COMP_NAME)//"_DT:"     ,&
             Default = DT                          ,&
             RC=STATUS )
        VERIFY_(STATUS)


        call ESMF_ConfigGetAttribute ( CF, OFFLINE_MODE, LABEL='CATCH_IN_OFFLINE_MODE:', &
             default=0, RC=STATUS )
        VERIFY_(STATUS)

        OFFLINE = OFFLINE_MODE /= 0

        ! --------------------------------------------------------------------------
        ! Get parameters from generic state.
        ! --------------------------------------------------------------------------

        call MAPL_Get ( MAPL                 ,&
             RUNALARM  = ALARM                            ,&
             ORBIT     = ORBIT                            ,&
             TILELATS  = LATS                             ,&
             TILELONS  = LONS                             ,&
             INTERNAL_ESMF_STATE = INTERNAL               ,&
             RC=STATUS )
        VERIFY_(STATUS)


        ! --------------------------------------------------------------------------
        ! Get name of albedo files from configuration
        ! --------------------------------------------------------------------------
        
        call MAPL_GetResource(MAPL      ,&
             VISDFFILE                   ,&
             label   = 'VISDF_FILE:'     ,&
             default = 'visdf.dat'       ,&
             RC=STATUS )
        VERIFY_(STATUS)

        call MAPL_GetResource(MAPL      ,&
             NIRDFFILE                   ,&
             label   = 'NIRDF_FILE:'     ,&
             default = 'nirdf.dat'       ,&
             RC=STATUS )
        VERIFY_(STATUS)

        ! -----------------------------------------------------
        ! IMPORT Pointers
        ! -----------------------------------------------------

        call MAPL_GetPointer(IMPORT,PS     ,'PS'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,TA     ,'TA'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,QA     ,'QA'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,UU     ,'UU'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DZ     ,'DZ'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,PCU    ,'PCU'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,PLS    ,'PLS'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,SNO    ,'SNO'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DRPAR  ,'DRPAR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DFPAR  ,'DFPAR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DRNIR  ,'DRNIR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DFNIR  ,'DFNIR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DRUVR  ,'DRUVR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DFUVR  ,'DFUVR'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,LWDNSRF,'LWDNSRF',RC=STATUS); VERIFY_(STATUS)

        call MAPL_GetPointer(IMPORT,ALW    ,'ALW'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,BLW    ,'BLW'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,EVAP   ,'EVAP'   ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DEVAP  ,'DEVAP'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,SH     ,'SH'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,DSH    ,'DSH'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,THATM  ,'THATM'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,QHATM  ,'QHATM'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,CTATM  ,'CTATM'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,CQATM  ,'CQATM'  ,RC=STATUS); VERIFY_(STATUS)

        call MAPL_GetPointer(IMPORT,ITY    ,'ITY'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,LAI    ,'LAI'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,GRN    ,'GRN'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,ROOTL  ,'ROOTL'  ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(IMPORT,Z2CH   ,'Z2CH'   ,RC=STATUS); VERIFY_(STATUS)

        ! -----------------------------------------------------
        ! INTERNAL Pointers
        ! -----------------------------------------------------

        call MAPL_GetPointer(INTERNAL,BF1        ,'BF1'        ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,BF2        ,'BF2'        ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,BF3        ,'BF3'        ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,VGWMAX     ,'VGWMAX'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CDCR1      ,'CDCR1'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CDCR2      ,'CDCR2'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,PSIS       ,'PSIS'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,BEE        ,'BEE'        ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,POROS      ,'POROS'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,WPWET      ,'WPWET'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,COND       ,'COND'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GNU        ,'GNU'        ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARS1       ,'ARS1'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARS2       ,'ARS2'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARS3       ,'ARS3'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARA1       ,'ARA1'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARA2       ,'ARA2'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARA3       ,'ARA3'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARA4       ,'ARA4'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARW1       ,'ARW1'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARW2       ,'ARW2'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARW3       ,'ARW3'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ARW4       ,'ARW4'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TSA1       ,'TSA1'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TSA2       ,'TSA2'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TSB1       ,'TSB1'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TSB2       ,'TSB2'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,ATAU       ,'ATAU'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,BTAU       ,'BTAU'       ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TC         ,'TC'         ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,QC         ,'QC'         ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CAPAC      ,'CAPAC'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CATDEF     ,'CATDEF'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,RZEXC      ,'RZEXC'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,SRFEXC     ,'SRFEXC'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT1    ,'GHTCNT1'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT2    ,'GHTCNT2'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT3    ,'GHTCNT3'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT4    ,'GHTCNT4'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT5    ,'GHTCNT5'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,GHTCNT6    ,'GHTCNT6'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,TSURF      ,'TSURF'      ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,WESNN1     ,'WESNN1'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,WESNN2     ,'WESNN2'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,WESNN3     ,'WESNN3'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,HTSNNN1    ,'HTSNNN1'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,HTSNNN2    ,'HTSNNN2'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,HTSNNN3    ,'HTSNNN3'    ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,SNDZN1     ,'SNDZN1'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,SNDZN2     ,'SNDZN2'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,SNDZN3     ,'SNDZN3'     ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CH         ,'CH'         ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CM         ,'CM'         ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,CQ         ,'CQ'         ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(INTERNAL,FR         ,'FR'         ,RC=STATUS); VERIFY_(STATUS)


        ! -----------------------------------------------------
        ! EXPORT POINTERS
        ! -----------------------------------------------------

        call MAPL_GetPointer(EXPORT,EVAPOUT,'EVAPOUT',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SUBLIM,'SUBLIM',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SHOUT,  'SHOUT'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,RUNOFF, 'RUNOFF' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVPINT, 'EVPINT' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVPSOI, 'EVPSOI' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVPVEG, 'EVPVEG' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVPICE, 'EVPICE' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WAT10CM,'WAT10CM',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WATSOI, 'WATSOI' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ICESOI, 'ICESOI' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVPSNO, 'EVPSNO'              ,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,BFLOW,  'BASEFLOW',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,RUNSURF,'RUNSURF',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SMELT,  'SMELT'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,HLWUP,  'HLWUP'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SWNDSRF,'SWNDSRF',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LWNDSRF,'LWNDSRF',ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,HLATN,  'HLATN'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,QINFIL, 'QINFIL' ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,AR1,    'AR1'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,AR2,    'AR2'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,RZEQ,   'RZEQ'   ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,GHFLX,  'GHFLX'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPSURF, 'TPSURF' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPSN1,  'TPSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPUST,  'TPUNST' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPSAT,  'TPSAT'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPWLT,  'TPWLT'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ASNOW,  'ASNOW'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SHSNOW, 'SHSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,AVETSNOW,'AVETSNOW',           RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,FRSAT,  'FRSAT'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,FRUST,  'FRUST'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,FRWLT,  'FRWLT'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP1,    'TP1'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP2,    'TP2'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP3,    'TP3'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP4,    'TP4'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP5,    'TP5'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TP6,    'TP6'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EMIS,   'EMIS'   ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ALBVR,  'ALBVR'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ALBVF,  'ALBVF'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ALBNR,  'ALBNR'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ALBNF,  'ALBNF'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DELTS,  'DELTS'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DELQS,  'DELQS'  ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TST  ,  'TST'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,QST  ,  'QST'    ,ALLOC=.true.,RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LST  ,  'LST'    ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WET1 ,  'WET1'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WET2 ,  'WET2'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WET3 ,  'WET3'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WCSF ,  'WCSF'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WCRZ ,  'WCRZ'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,WCPR ,  'WCPR'   ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,ACCUM,  'ACCUM'  ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SNOMAS,'SNOWMASS',             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SNOWDP, 'SNOWDP' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,EVLAND, 'EVLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,PRLAND, 'PRLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SNOLAND, 'SNOLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DRPARLAND, 'DRPARLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DFPARLAND, 'DFPARLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LHSNOW, 'LHSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SWNETSNOW1, 'SWNETSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LWUPSNOW, 'LWUPSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LWDNSNOW, 'LWDNSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TCSORIG, 'TCSORIG' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPSN1IN, 'TPSN1IN' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TPSN1OUT, 'TPSN1OUT' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LHLAND, 'LHLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SHLAND, 'SHLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SWLAND, 'SWLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SWDOWNLAND, 'SWDOWNLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,LWLAND, 'LWLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,GHLAND, 'GHLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,GHSNOW, 'GHSNOW' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,GHTSKIN,'GHTSKIN',             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SMLAND, 'SMLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TWLAND, 'TWLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TELAND, 'TELAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,TSLAND, 'TSLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DWLAND, 'DWLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,DHLAND, 'DHLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SPLAND, 'SPLAND' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SPWATR, 'SPWATR' ,             RC=STATUS); VERIFY_(STATUS)
        call MAPL_GetPointer(EXPORT,SPSNOW, 'SPSNOW' ,             RC=STATUS); VERIFY_(STATUS)

        NTILES = size(PS)

        ! --------------------------------------------------------------------------
        ! ALLOCATE LOCAL POINTERS
        ! --------------------------------------------------------------------------

        allocate(GHTCNT (6,NTILES))
        allocate(WESNN  (3,NTILES))
        allocate(HTSNNN (3,NTILES))
        allocate(SNDZN  (3,NTILES))
        allocate(TILEZERO (NTILES))
        allocate(DZSF     (NTILES))
        allocate(SWNETFREE(NTILES))
        allocate(SWNETSNOW(NTILES))
        allocate(VEG      (NTILES))  
        allocate(ZTH      (NTILES))  
        allocate(SLR      (NTILES))  
        allocate(RSL1     (NTILES)) 
        allocate(RSL2     (NTILES)) 
        allocate(SQSCAT   (NTILES))
        allocate(RDC      (NTILES))  
        allocate(VISDF    (NTILES))
        allocate(NIRDF    (NTILES))
	allocate(UUU      (NTILES))
	allocate(RHO      (NTILES))
	allocate(ZVG      (NTILES))
	allocate(LAI0     (NTILES))
	allocate(GRN0     (NTILES))
	allocate(Z0       (NTILES))
	allocate(D0       (NTILES))
	allocate(SFMC     (NTILES))
	allocate(RZMC     (NTILES))
	allocate(PRMC     (NTILES))
	allocate(ENTOT    (NTILES))
	allocate(ghflxsno (NTILES))
	allocate(ghflxtskin(NTILES))
	allocate(WTOT     (NTILES))
	allocate(WCHANGE  (NTILES))
	allocate(ECHANGE  (NTILES))
	allocate(HSNACC   (NTILES))
	allocate(EVACC    (NTILES))
	allocate(SHACC    (NTILES))
	allocate(VSUVR    (NTILES))
	allocate(VSUVF    (NTILES))
	allocate(SNOVR    (NTILES))
	allocate(SNOVF    (NTILES))
	allocate(SNONR    (NTILES))
	allocate(SNONF    (NTILES))
	allocate(CAT_ID   (NTILES))
	allocate(ALWX     (NTILES))
	allocate(BLWX     (NTILES))
        allocate(SHSNOW1   (NTILES))
        allocate(AVETSNOW1 (NTILES))
        allocate(WAT10CM1  (NTILES))
        allocate(WATSOI1   (NTILES))
        allocate(ICESOI1   (NTILES))
        allocate(LHSNOW1   (NTILES))
        allocate(LWUPSNOW1 (NTILES))
        allocate(LWDNSNOW1 (NTILES))
        allocate(NETSWSNOW (NTILES))
        allocate(TCSORIG1  (NTILES))
        allocate(TPSN1IN1  (NTILES))
        allocate(TPSN1OUT1 (NTILES))
        allocate(LHACC     (NTILES))
        allocate(SUMEV     (NTILES))

        allocate(SHSBT    (NTILES,NUM_SUBTILES))
        allocate(DSHSBT   (NTILES,NUM_SUBTILES))
        allocate(EVSBT    (NTILES,NUM_SUBTILES))
        allocate(DEVSBT   (NTILES,NUM_SUBTILES))
        allocate(CFT      (NTILES,NUM_SUBTILES))
        allocate(CFQ      (NTILES,NUM_SUBTILES))
        allocate(TCO      (NTILES,NUM_SUBTILES))
        allocate(QCO      (NTILES,NUM_SUBTILES))
        allocate(DQS      (NTILES,NUM_SUBTILES))
        allocate(QSAT     (NTILES,NUM_SUBTILES))
        allocate(RA       (NTILES,NUM_SUBTILES))

        call ESMF_VMGetCurrent ( VM, RC=STATUS )
        ! --------------------------------------------------------------------------
        ! Catchment Id and vegetation types used to index into tables
        ! --------------------------------------------------------------------------

        CAT_ID = 1
        VEG    = nint(ITY)

        ! --------------------------------------------------------------------------
        ! surface layer depth for soil moisture
        ! --------------------------------------------------------------------------
        
        DZSF(    :) = SURFLAY

        ! --------------------------------------------------------------------------
        ! build arrays from internal state
        ! --------------------------------------------------------------------------

        GHTCNT(1,:) = GHTCNT1
        GHTCNT(2,:) = GHTCNT2
        GHTCNT(3,:) = GHTCNT3
        GHTCNT(4,:) = GHTCNT4
        GHTCNT(5,:) = GHTCNT5
        GHTCNT(6,:) = GHTCNT6

        WESNN (1,:) = WESNN1
        WESNN (2,:) = WESNN2
        WESNN (3,:) = WESNN3

        HTSNNN(1,:) = HTSNNN1
        HTSNNN(2,:) = HTSNNN2
        HTSNNN(3,:) = HTSNNN3

        SNDZN (1,:) = SNDZN1
        SNDZN (2,:) = SNDZN2
        SNDZN (3,:) = SNDZN3

        debugzth = .false.

        ! --------------------------------------------------------------------------
        ! Get the current time. 
        ! --------------------------------------------------------------------------

        call ESMF_ClockGet( CLOCK, currTime=CURRENT_TIME, startTime=MODELSTART, TIMESTEP=DELT,  RC=STATUS )
        VERIFY_(STATUS)
        if (MAPL_AM_I_Root(VM).and.debugzth) then
         print *,' start time of clock '
         CALL ESMF_TimePrint ( MODELSTART, OPTIONS="string", RC=STATUS )
        endif

        ! ----------------------------------------------------------------------------------
        ! Update the interpolation limits for MODIS albedo corrections
        ! in the internal state and get their midmonth times
        ! ----------------------------------------------------------------------------------

        call MAPL_ReadForcing(MAPL,'VISDF',VISDFFILE,CURRENT_TIME,VISDF,ON_TILES=.true.,RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_ReadForcing(MAPL,'NIRDF',NIRDFFILE,CURRENT_TIME,NIRDF,ON_TILES=.true.,RC=STATUS)
        VERIFY_(STATUS)

        ! --------------------------------------------------------------------------
        ! retrieve the zenith angle
        ! --------------------------------------------------------------------------

!! The next sequence is to make sure that the albedo here and in solar are in sync
!!
! Need to know when Solar was called last, so first get the solar alarm
        call ESMF_ClockGetAlarm ( CLOCK, alarmname="SOLAR_Alarm", ALARM=SOLALARM, RC=STATUS )
        VERIFY_(STATUS)
! Get the interval of the solar alarm - first get it in seconds
        call ESMF_ConfigGetAttribute ( CF, DT_SOLAR, Label="SOLAR_DT:", DEFAULT=DT, RC=STATUS )
        VERIFY_(STATUS)
! Now make an ESMF interval from the increment in seconds
        CALL ESMF_TimeIntervalSet ( TINT, S=NINT(DT_SOLAR), RC=STATUS )
        VERIFY_(STATUS)
! Now print out the solar alarm interval
        if (MAPL_AM_I_Root(VM).and.debugzth) CALL ESMF_TimeIntervalPrint ( TINT, OPTIONS="string", RC=STATUS )
! Now find out if it is ringing now: if so, set "BEFORE" to last time it rang before now
         solalarmison = ESMF_AlarmIsRinging(SOLALARM,RC=STATUS)
         VERIFY_(STATUS)
         if (MAPL_AM_I_Root(VM).and.debugzth)print *,' logical for solar alarm ',solalarmison
!     if so, set "BEFORE" to last time it rang before now
        if(solalarmison) then
         if (MAPL_AM_I_Root(VM).and.debugzth)print *,' In catch, solar alarm is ringing '
         NOW = CURRENT_TIME
         BEFORE = NOW - TINT
! Now print out the last time solar alarm rang
         if (MAPL_AM_I_Root(VM).and.debugzth)CALL ESMF_TimePrint ( BEFORE, OPTIONS="string", RC=STATUS )
!     If alarm is not ringing now, find out when it rang last
        else
         if (MAPL_AM_I_Root(VM).and.debugzth)print *,' In catch, solar alarm is not ringing '
         call ESMF_AlarmGet ( SOLALARM, prevRingTime=BEFORE, RC=STATUS )
         VERIFY_(STATUS)
! PrevRingTime can lie: if alarm never went off yet it gives next alarm time, not prev.
         if(BEFORE > CURRENT_TIME) then
          BEFORE = BEFORE-TINT
          if (MAPL_AM_I_Root(VM).and.debugzth)print *,' In catch, solar alarm not ringing, prev time lied '
          if (MAPL_AM_I_Root(VM).and.debugzth)CALL ESMF_TimePrint ( BEFORE, OPTIONS="string", RC=STATUS )
         else
          if (MAPL_AM_I_Root(VM).and.debugzth)print *,' In catch, solar alarm not ringing, prev time okay '
          if (MAPL_AM_I_Root(VM).and.debugzth)CALL ESMF_TimePrint ( BEFORE, OPTIONS="string", RC=STATUS )
         endif
! Now print out the last time solar alarm rang
        endif

! Get the zenith angle at the center of the time between the last solar call and the next one
        call MAPL_SunGetInsolation(LONS, LATS,      &
            ORBIT, ZTH, SLR, &
            INTV = TINT,     &
            currTime=BEFORE+DELT,  &
            RC=STATUS )
        VERIFY_(STATUS)

        ZTH = max(0.0,ZTH)

        ZVG  = Z2CH - (Z2CH - MIN_VEG_HEIGHT)*exp(-LAI) ! H5_0 Zero-diff 
!        ZVG  = Z2CH - SCALE4Z0*(Z2CH - MIN_VEG_HEIGHT)*exp(-LAI) ! Post H5_0

        !  For now roughnesses and displacement heights
        !   are the same for all subtiles.
        !---------------------------------------------------

        Z0   = Z0_BY_ZVEG*ZVG*SCALE4Z0 ! H5_0 Zero-diff
!        Z0   = Z0_BY_ZVEG*ZVG ! Post H5_0 
        D0   = D0_BY_ZVEG*ZVG

        UUU = max(UU,MAPL_USMIN) * (log((ZVG-D0+Z0)/Z0) &
             / log((max(DZ-D0,10.)+Z0)/Z0))

        ! --------------------------------------------------------------------------
        ! Update raditation exports
        ! --------------------------------------------------------------------------


        call    SIBALB(NTILES, VEG, LAI, GRN, ZTH, & 
                       VISDF, VISDF, NIRDF, NIRDF, & ! Sarith parameters on tiles USE ONLY DIFFUSE
                       ALBVR, ALBNR, ALBVF, ALBNF  ) ! instantaneous snow-free albedos on tiles

        call   SNOW_ALBEDO(NTILES,N_snow, 0, VEG, LAI, ZTH,  &
                 RHOFS,                                              &   
                 SNWALB_VISMAX, SNWALB_NIRMAX, SLOPE,                & 
                 WESNN, HTSNNN, SNDZN,                               &
                 ALBVR, ALBNR, ALBVF, ALBNF, & ! instantaneous snow-free albedos on tiles
                 SNOVR, SNONR, SNOVF, SNONF )  ! instantaneous snow albedos on tiles
!                 RCONSTIT, UM, RTS, PARDIR, PARDIF,      ! N_constit = 0, thus, all the optional           
!                 ABVIS, ABNIR                              arguments RCONSTIT and after are not in use.  
!                 )



!        call   SNOW_ALBEDO(NTILES, VEG, LAI, ZTH,  & 
!                       WESNN,HTSNNN,SNDZN,         & 
!                       UUU, TA, DRPAR, DFPAR,      & ! (Replace TA with snow temp. eventually)
!                       ALBVR, ALBNR, ALBVF, ALBNF, & ! instantaneous snow-free albedos on tiles
!                       SNOVR, SNONR, SNOVF, SNONF )  ! instantaneous snow albedos on tiles

        ! --------------------------------------------------------------------------
        ! albedo/swnet partitioning
        ! --------------------------------------------------------------------------

        VSUVR = DRPAR + DRUVR
        VSUVF = DFPAR + DFUVR

        if(associated(SWDOWNLAND)) SWDOWNLAND = DRPAR + DFPAR + DRUVR + DFUVR + DRNIR + DFNIR

        SWNETFREE = (1.-ALBVR)*VSUVR + (1.-ALBVF)*VSUVF + (1.-ALBNR)*DRNIR + (1.-ALBNF)*DFNIR 
        SWNETSNOW = (1.-SNOVR)*VSUVR + (1.-SNOVF)*VSUVF + (1.-SNONR)*DRNIR + (1.-SNONF)*DFNIR 

        ! --------------------------------------------------------------------------
        ! Parameters that depend on vegetation type only
        ! --------------------------------------------------------------------------

        RSL1   = VGRDRS(VEG)/(ROOTL*VGROTD(VEG))

        RSL2   = ROOTL*VGROCA(VEG)
        RSL2   = (RSL2 - 3.0 - 2.*alog(RSL2/(1.-RSL2)))/(8.*MAPL_PI*ROOTL*VGROTD(VEG))

        ! --------------------------------------------------------------------------
        ! Greenness and type dependent parameters
        ! --------------------------------------------------------------------------

        SQSCAT = (VGTR11(VEG)+VGRF11(VEG)) *     GRN  + &
                 (VGTR12(VEG)+VGRF12(VEG)) * (1.-GRN) 
        SQSCAT = sqrt(1.0 - SQSCAT)

        ! --------------------------------------------------------------------------
        ! LAI and type dependent parameters; RDC formulation can use veg frac in next version.
        ! --------------------------------------------------------------------------

! Correction to RDC formulation -Randy Koster, 4/1/2011
!        RDC = max(VGRDA(VEG)*min(VGRDB(VEG),LAI),0.001)
        RDC = max(VGRDA(VEG)*min(1., LAI/VGRDB(VEG)),0.001)
        RHO = PS/(MAPL_RGAS*(TA*(1+MAPL_VIREPS*QA)))

        if(OFFLINE) then
           do N=1,NUM_SUBTILES
              CFT   (:,N) = 1.0
              CFQ   (:,N) = 1.0
              SHSBT (:,N) = CH(:,N)*(TC(:,N)-TA)
              EVSBT (:,N) = CQ(:,N)*(QC(:,N)-QA)
              DSHSBT(:,N) = CH(:,N)
              DEVSBT(:,N) = CQ(:,N)
           end do
           BLWX = MAPL_STFBOL*TA**3
           ALWX = -3.0*BLWX*TA
           BLWX =  4.0*BLWX
        else
           do N=1,NUM_SUBTILES
              CFT   (:,N) = (CH(:,N)/CTATM)
              CFQ   (:,N) = (CQ(:,N)/CQATM)
              SHSBT (:,N) = (SH  + DSH  *(TC(:,N)-THATM))*CFT(:,N)
              EVSBT (:,N) = (EVAP+ DEVAP*(QC(:,N)-QHATM))*CFQ(:,N)
              DSHSBT(:,N) =  DSH  *CFT(:,N)
              DEVSBT(:,N) =  DEVAP*CFQ(:,N)
           end do
           ALWX = ALW
           BLWX = BLW
        end if

        do N=1,NUM_SUBTILES
           DQS(:,N) = GEOS_DQSAT ( TC(:,N), PS, QSAT=QSAT(:,N), PASCALS=.true., RAMP=0.0 )
           QC (:,N) = min(max(QA(:),QSAT(:,N)),QC(:,N))
           QC (:,N) = max(min(QA(:),QSAT(:,N)),QC(:,N))
           RA (:,N) = RHO/CH(:,N)
        end do


        QC(:,FSNW) = QSAT(:,FSNW)

	! --------------------------------------------------------------------------
	! protect the forcing from unsavory values, as per practice in offline
	! driver
	! --------------------------------------------------------------------------

        ASSERT_(count(PLS<0.)==0)
        ASSERT_(count(PCU<0.)==0)
        ASSERT_(count(SNO<0.)==0)

        LAI0  = max(0.0001     , LAI)
        GRN0  = max(0.0001     , GRN)		
        ZTH   = max(0.0001     , ZTH)

        TCO   = TC
        QCO   = QC

        ! --------------------------------------------------------------------------
        ! actual CATCHMENT call
        ! --------------------------------------------------------------------------

        TILEZERO = 0.0

        call MAPL_TimerOn  ( MAPL, "-CATCH" )

#ifdef DBG_CATCH_INPUTS
        call MAPL_Get(MAPL, LocStream=LOCSTREAM, RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_LocStreamGet(LOCSTREAM, TILEGRID=TILEGRID, RC=STATUS)
        VERIFY_(STATUS)

        call MAPL_TileMaskGet(tilegrid,  mask, rc=status)
        VERIFY_(STATUS)

         if (UNIT_i == 0) then
           unit_i = GETFILE( "catch_inputs.data", form="unformatted", RC=STATUS )
           VERIFY_(STATUS)
        endif
        unit = unit_i

! Inputs
        call MAPL_VarWrite(unit, tilegrid, PCU, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, PLS, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SNO, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, UUU, mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, EVSBT(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DEVSBT(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SHSBT(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DSHSBT(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, EVSBT(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DEVSBT(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SHSBT(:,FTRN),  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DSHSBT(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, EVSBT(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DEVSBT(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SHSBT(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DSHSBT(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, EVSBT(:,FSNW), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DEVSBT(:,FSNW), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SHSBT(:,FSNW), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, TILEZERO      ,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DSHSBT(:,FSNW), mask=mask, rc=status); VERIFY_(STATUS)
        
        call MAPL_VarWrite(unit, tilegrid, TA, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, QA, mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, RA(:,FSAT),  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RA(:,FTRN),  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RA(:,FWLT),  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RA(:,FSNW), mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, ZTH,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DRPAR,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DFPAR,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SWNETFREE,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SWNETSNOW,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, LWDNSRF, mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, PS*.01, mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, LAI0,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, GRN0,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, Z2CH,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, SQSCAT,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RSL1,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RSL2,  mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, RDC, mask=mask, rc=status); VERIFY_(STATUS)

        call MAPL_VarWrite(unit, tilegrid, QSAT(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DQS(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, ALWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, BLWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, QSAT(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DQS(:,FTRN) , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, ALWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, BLWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, QSAT(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DQS(:,FWLT) , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, ALWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, BLWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, QSAT(:,FSNW) , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, DQS(:,FSNW) , mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, ALWX, mask=mask, rc=status); VERIFY_(STATUS)
        call MAPL_VarWrite(unit, tilegrid, BLWX, mask=mask, rc=status); VERIFY_(STATUS)

! params
        if (firsttime) then
            firsttime = .false.
           unit = GETFILE( "catch_params.data", form="unformatted", RC=STATUS )
           VERIFY_(STATUS)

           NT_GLOBAL = size(mask)

           call WRITE_PARALLEL(NT_GLOBAL, UNIT)
           call WRITE_PARALLEL(DT, UNIT)
           call WRITE_PARALLEL(PRECIPFRAC, UNIT)
           call MAPL_VarWrite(unit, tilegrid, VEG, mask=mask, rc=status); VERIFY_(STATUS)

           call MAPL_VarWrite(unit, tilegrid, BF1,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, BF2,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, BF3,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, VGWMAX,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, CDCR1,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, CDCR2, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, PSIS, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, BEE,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, POROS,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, WPWET,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, COND,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GNU, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARS1, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARS2, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARS3, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARA1, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARA2,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARA3, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARA4, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARW1,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARW2, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARW3,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ARW4,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TSA1,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TSA2,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TSB1,  mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TSB2, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, ATAU, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, BTAU, mask=mask, rc=status); VERIFY_(STATUS)

           call FREE_FILE(unit, RC=STATUS)
           VERIFY_(STATUS)

! Updates
           unit = GETFILE( "catch_updates.data", form="unformatted", RC=STATUS )
           VERIFY_(STATUS)


           call MAPL_VarWrite(unit, tilegrid, TC(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TC(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TC(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, QC(:,FSAT), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, QC(:,FTRN), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, QC(:,FWLT), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, CAPAC, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, CATDEF, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, RZEXC, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, SRFEXC, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(1,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(2,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(3,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(4,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(5,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, GHTCNT(6,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, TSURF, mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, WESNN(1,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, WESNN(2,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, WESNN(3,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, HTSNNN(1,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, HTSNNN(2,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, HTSNNN(3,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, SNDZN(1,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, SNDZN(2,:), mask=mask, rc=status); VERIFY_(STATUS)
           call MAPL_VarWrite(unit, tilegrid, SNDZN(3,:), mask=mask, rc=status); VERIFY_(STATUS)
           
           call FREE_FILE(unit, RC=STATUS)
           VERIFY_(STATUS)

        end if
        deallocate(mask)
#endif

! Sanity Check to ensure IMPORT ITY from VEGDYN is consistent with INTERNAL ITY from CATCH
! ----------------------------------------------------------------------------------------
        if (check_ity) then
            call MAPL_GetPointer(INTERNAL,OLD_ITY,'OLD_ITY',RC=STATUS)
            VERIFY_(STATUS)
            N = count(OLD_ITY.ne.ITY)
            call ESMF_VMGetCurrent ( VM, RC=STATUS )
            VERIFY_(STATUS)
            call MAPL_CommsAllReduceMax ( VM, N, NMAX, 1, RC=STATUS )
            VERIFY_(STATUS)
            if( NMAX.ne.0 ) then
                print *, 'CATCH_INTERNAL_RST is NOT consistent with VEGDYN Data'
            endif
            ASSERT_(NMAX==0)
            check_ity = .false.
        endif
! ----------------------------------------------------------------------------------------
        call ESMF_ConfigGetAttribute ( CF, lonbeg, Label="PRINTLONBEG:", DEFAULT=MAPL_UNDEF, RC=STATUS )
        call ESMF_ConfigGetAttribute ( CF, lonend, Label="PRINTLONEND:", DEFAULT=MAPL_UNDEF, RC=STATUS )
        call ESMF_ConfigGetAttribute ( CF, latbeg, Label="PRINTLATBEG:", DEFAULT=MAPL_UNDEF, RC=STATUS )
        call ESMF_ConfigGetAttribute ( CF, latend, Label="PRINTLATEND:", DEFAULT=MAPL_UNDEF, RC=STATUS )
! ----------------------------------------------------------------------------------------

        call CATCHMENT ( NTILES, LONS, LATS                       ,&
             DT	      ,     PRECIPFRAC, cat_id, VEG,     DZSF     ,&
             PCU      ,     PLS           ,     SNO               ,&
             UUU                                                  ,&

             EVSBT(:,FSAT),     DEVSBT(:,FSAT),     TILEZERO      ,&
             SHSBT(:,FSAT),     TILEZERO      ,     DSHSBT(:,FSAT),&
             EVSBT(:,FTRN),     DEVSBT(:,FTRN),     TILEZERO      ,&
             SHSBT(:,FTRN),     TILEZERO      ,     DSHSBT(:,FTRN),&
             EVSBT(:,FWLT),     DEVSBT(:,FWLT),     TILEZERO      ,&
             SHSBT(:,FWLT),     TILEZERO      ,     DSHSBT(:,FWLT),&
             EVSBT(:,FSNW),     DEVSBT(:,FSNW),     TILEZERO      ,&
             SHSBT(:,FSNW),     TILEZERO      ,     DSHSBT(:,FSNW),&

             TA           ,     QA                                ,&

             RA(:,FSAT), RA(:,FTRN), RA(:,FWLT), RA(:,FSNW)       ,&

             ZTH, DRPAR, DFPAR, SWNETFREE, SWNETSNOW, LWDNSRF     ,&

             PS*.01                                               ,&

             LAI0, GRN0, Z2CH, SQSCAT, RSL1, RSL2, RDC            ,&

             QSAT(:,FSAT) ,    DQS(:,FSAT) ,   ALWX  ,    BLWX      ,&
             QSAT(:,FTRN) ,    DQS(:,FTRN) ,   ALWX  ,    BLWX      ,&
             QSAT(:,FWLT) ,    DQS(:,FWLT) ,   ALWX  ,    BLWX      ,&
             QSAT(:,FSNW) ,    DQS(:,FSNW) ,   ALWX  ,    BLWX      ,&

             BF1, BF2, BF3, VGWMAX, CDCR1, CDCR2, PSIS	          ,&
             BEE, POROS, WPWET, COND, GNU	                  ,&
             ARS1, ARS2, ARS3, ARA1, ARA2, ARA3, ARA4	          ,&
             ARW1, ARW2, ARW3, ARW4, TSA1, TSA2, TSB1, TSB2	  ,&
             ATAU, BTAU, .false.			          ,&

             TC(:,FSAT), TC(:,FTRN), TC(:,FWLT)		          ,& 
             QC(:,FSAT), QC(:,FTRN), QC(:,FWLT)		          ,&

             CAPAC, CATDEF, RZEXC, SRFEXC, GHTCNT, TSURF          ,&
             WESNN, HTSNNN, SNDZN                                 ,&

             EVAPOUT, SHOUT, RUNOFF, EVPINT, EVPSOI, EVPVEG       ,&
             EVPICE                                               ,&
             BFLOW                                                ,&
             RUNSURF                                              ,&
             SMELT                                                ,&
             HLWUP                                                ,&
             SWNDSRF                                              ,&
             HLATN                                                ,&
             QINFIL                                               ,&
             AR1                                                  ,&
             AR2                                                  ,&
             RZEQ                                                 ,&
             GHFLX                                                ,&
             GHFLXSNO                                             ,&
             GHFLXTSKIN                                           ,&
             TC(:,FSNW)                                           ,&
             ASNOW                                                ,&
             TP1, TP2, TP3, TP4, TP5, TP6,  SFMC, RZMC, PRMC      ,&
             ENTOT,WTOT, WCHANGE, ECHANGE, HSNACC, EVACC, SHACC   ,&
             SHSNOW1, AVETSNOW1, WAT10CM1, WATSOI1, ICESOI1       ,&
             LHSNOW1, LWUPSNOW1, LWDNSNOW1, NETSWSNOW             ,&
             TCSORIG1, TPSN1IN1, TPSN1OUT1                        ,&
             lonbeg,lonend,latbeg,latend,LHACC=LHACC               )

        call MAPL_TimerOff ( MAPL, "-CATCH" )

        QC(:,FSNW) =  GEOS_QSAT ( TC(:,FSNW), PS, PASCALS=.true., RAMP=0.0 )

        ! --------------------------------------------------------------------------
        ! update subtile fractions
        ! --------------------------------------------------------------------------

        EMIS    = EMSVEG(VEG) + (EMSBARESOIL - EMSVEG(VEG))*exp(-LAI)

        EMIS    = EMIS     *(1.-ASNOW) + EMSSNO   *ASNOW

        call MAPL_SunGetInsolation(LONS, LATS,      &
            ORBIT, ZTH, SLR, &
            INTV = TINT,     &
            currTime=CURRENT_TIME+DELT,  &
            RC=STATUS )
        VERIFY_(STATUS)

        ZTH = max(0.0,ZTH)

        ! --------------------------------------------------------------------------
        ! Update raditation exports
        ! --------------------------------------------------------------------------


        call MAPL_TimerOn(MAPL,"-ALBEDO")
        call    SIBALB(NTILES, VEG, LAI, GRN, ZTH, & 
                       VISDF, VISDF, NIRDF, NIRDF, & ! Sarith parameters on tiles USE ONLY DIFFUSE
                       ALBVR, ALBNR, ALBVF, ALBNF  ) ! instantaneous snow-free albedos on tiles

        call   SNOW_ALBEDO(NTILES,N_snow, 0, VEG, LAI, ZTH,  &
                 RHOFS,                                              &   
                 SNWALB_VISMAX, SNWALB_NIRMAX, SLOPE,                & 
                 WESNN, HTSNNN, SNDZN,                               &
                 ALBVR, ALBNR, ALBVF, ALBNF, & ! instantaneous snow-free albedos on tiles
                 SNOVR, SNONR, SNOVF, SNONF )  ! instantaneous snow albedos on tiles
!                 RCONSTIT, UM, RTS, PARDIR, PARDIF,      ! N_constit = 0, thus, all the optional           
!                 ABVIS, ABNIR                              arguments RCONSTIT and after are not in use.  
!                 )

!        call   SNOW_ALBEDO(NTILES, VEG, LAI, ZTH,  & 
!                       WESNN,HTSNNN,SNDZN,         & 
!                       UUU, TA, DRPAR, DFPAR,      & ! (Replace TA with snow temp. eventually)
!                       ALBVR, ALBNR, ALBVF, ALBNF, & ! instantaneous snow-free albedos on tiles
!                       SNOVR, SNONR, SNOVF, SNONF )  ! instantaneous snow albedos on tiles

        ALBVR   = ALBVR    *(1.-ASNOW) + SNOVR    *ASNOW
        ALBVF   = ALBVF    *(1.-ASNOW) + SNOVF    *ASNOW
        ALBNR   = ALBNR    *(1.-ASNOW) + SNONR    *ASNOW
        ALBNF   = ALBNF    *(1.-ASNOW) + SNONF    *ASNOW
        call MAPL_TimerOff(MAPL,"-ALBEDO")

        LWNDSRF = LWDNSRF - HLWUP

        ! --------------------------------------------------------------------------
        ! update outputs
        ! --------------------------------------------------------------------------

        DELTS = 0.0
        DELQS = 0.0

        do N=1,NUM_SUBTILES
           DELTS   = DELTS + CFT(:,N)*(TC(:,N)-TCO(:,N))*FR(:,N)
           DELQS   = DELQS + CFQ(:,N)*(QC(:,N)-QCO(:,N))*FR(:,N)
        end do

        FR(:,FSAT) =           AR1  * (1-ASNOW)
        FR(:,FTRN) =           AR2  * (1-ASNOW)
        FR(:,FWLT) = (1.0-(AR1+AR2))* (1-ASNOW)
        FR(:,FSNW) =                     ASNOW

        FR = min( max( fr,0.0 ), 1.0 )

        TST   = 0.0
        QST   = 0.0
        do N=1,NUM_SUBTILES
           TST     = TST   +           TC(:,N)          *FR(:,N)
           QST     = QST   +           QC(:,N)          *FR(:,N)
        end do

!amm add correction term to latent heat diagnostics (HLATN is always allocated)
!    this will impact the export LHLAND
        HLATN = HLATN - LHACC
! also add some portion of the correction term to evap from soil, int, veg and snow
        SUMEV = EVPICE+EVPSOI+EVPVEG+EVPINT
        where(SUMEV>0.)
        EVPICE = EVPICE - EVACC*EVPICE/SUMEV
        EVPSOI = EVPSOI - EVACC*EVPSOI/SUMEV
        EVPINT = EVPINT - EVACC*EVPINT/SUMEV
        EVPVEG = EVPVEG - EVACC*EVPVEG/SUMEV
        endwhere

        if(associated( LST  )) LST    = TST
        if(associated( TPSURF))TPSURF = TSURF
        if(associated( WET1 )) WET1   = max(min(SFMC / POROS,1.0),0.0)
        if(associated( WET2 )) WET2   = max(min(RZMC / POROS,1.0),0.0)
        if(associated( WET3 )) WET3   = max(min(PRMC / POROS,1.0),0.0)
        if(associated( WCSF )) WCSF   = SFMC
        if(associated( WCRZ )) WCRZ   = RZMC
        if(associated( WCPR )) WCPR   = PRMC

        if(associated( ACCUM)) ACCUM  = SNO - EVPICE*(1./MAPL_ALHS) - SMELT 

        if(associated(EVPSNO)) EVPSNO = EVPICE
        if(associated(SUBLIM)) SUBLIM = EVPICE*(1./MAPL_ALHS)*FR(:,FSNW)
        if(associated(EVLAND)) EVLAND = EVAPOUT-EVACC
        if(associated(PRLAND)) PRLAND = PCU+PLS+SNO
        if(associated(SNOLAND)) SNOLAND = SNO
        if(associated(DRPARLAND)) DRPARLAND = DRPAR
        if(associated(DFPARLAND)) DFPARLAND = DFPAR
        if(associated(LHLAND)) LHLAND = HLATN
        if(associated(SHLAND)) SHLAND = SHOUT-SHACC
        if(associated(SWLAND)) SWLAND = SWNDSRF
        if(associated(LWLAND)) LWLAND = LWNDSRF
        if(associated(GHLAND)) GHLAND = GHFLX
        if(associated(GHSNOW)) GHSNOW = GHFLXSNO
        if(associated(SHSNOW)) SHSNOW = SHSNOW1                   
        if(associated(AVETSNOW)) AVETSNOW = AVETSNOW1             
        if(associated(WAT10CM)) WAT10CM = WAT10CM1                
        if(associated(WATSOI)) WATSOI = WATSOI1                   
        if(associated(ICESOI)) ICESOI = ICESOI1                   
        if(associated(LHSNOW)) LHSNOW = LHSNOW1                   
        if(associated(LWUPSNOW)) LWUPSNOW = LWUPSNOW1             
        if(associated(LWDNSNOW)) LWDNSNOW = LWDNSNOW1             
        if(associated(SWNETSNOW1)) SWNETSNOW1 = NETSWSNOW         
        if(associated(TCSORIG)) TCSORIG = TCSORIG1                
        if(associated(TPSN1IN)) TPSN1IN = TPSN1IN1                
        if(associated(TPSN1OUT)) TPSN1OUT = TPSN1OUT1
        if(associated(GHTSKIN))GHTSKIN = GHFLXTSKIN
        if(associated(SMLAND)) SMLAND = SMELT
        if(associated(TWLAND)) TWLAND = WTOT
        if(associated(TELAND)) TELAND = ENTOT
        if(associated(TSLAND)) TSLAND = WESNN (1,:) + WESNN (2,:) + WESNN (3,:)
        if(associated(DWLAND)) DWLAND = WCHANGE
        if(associated(DHLAND)) DHLAND = ECHANGE
        if(associated(SPLAND)) SPLAND = SHACC
        if(associated(SPWATR)) SPWATR = EVACC
        if(associated(SPSNOW)) SPSNOW = HSNACC

        if(associated(FRSAT )) FRSAT  = max( min( FR(:,FSAT),1.0 ), 0.0 )
        if(associated(FRUST )) FRUST  = max( min( FR(:,FTRN),1.0 ), 0.0 )
        if(associated(FRWLT )) FRWLT  = max( min( FR(:,FWLT),1.0 ), 0.0 )

        if(associated(SNOMAS)) SNOMAS = WESNN (1,:) + WESNN (2,:) + WESNN (3,:)
        if(associated(SNOWDP)) SNOWDP = SNDZN (1,:) + SNDZN (2,:) + SNDZN (3,:)

        if(associated(TPSN1)) then
           where(WESNN(1,:)>0.)
              TPSN1  = TC(:,FSNW)
           elsewhere
              TPSN1  = MAPL_UNDEF
           end where
        end if

        if(associated(TPSAT)) then
           where(FR(:,FSAT)>0.)
              TPSAT  = TC(:,FSAT)
           elsewhere
              TPSAT  = MAPL_UNDEF
           end where
        end if

        if(associated(TPWLT)) then
           where(FR(:,FWLT)>0.)
              TPWLT  = TC(:,FWLT)
           elsewhere
              TPWLT  = MAPL_UNDEF
           end where
        end if

        if(associated(TPUST)) then
           where(FR(:,FTRN)>0.)
              TPUST  = TC(:,FTRN)
           elsewhere
              TPUST  = MAPL_UNDEF
           end where
        end if


        ! --------------------------------------------------------------------------
        ! update internal state arrays
        ! --------------------------------------------------------------------------

        GHTCNT1 = GHTCNT(1,:)
        GHTCNT2 = GHTCNT(2,:)
        GHTCNT3 = GHTCNT(3,:)
        GHTCNT4 = GHTCNT(4,:)
        GHTCNT5 = GHTCNT(5,:)
        GHTCNT6 = GHTCNT(6,:)

        WESNN1  = WESNN (1,:)
        WESNN2  = WESNN (2,:)
        WESNN3  = WESNN (3,:)

        HTSNNN1 = HTSNNN(1,:)
        HTSNNN2 = HTSNNN(2,:)
        HTSNNN3 = HTSNNN(3,:)

        SNDZN1  = SNDZN (1,:)
        SNDZN2  = SNDZN (2,:)
        SNDZN3  = SNDZN (3,:)

        ! --------------------------------------------------------------------------

        deallocate(GHTCNT   )
        deallocate(WESNN    )
        deallocate(HTSNNN   )
        deallocate(SNDZN    )
	deallocate(TILEZERO )
        deallocate(DZSF     )
	deallocate(SWNETFREE)
	deallocate(SWNETSNOW)
	deallocate(VEG      )
	deallocate(ZTH      )
	deallocate(SLR      )
	deallocate(RSL1     )
	deallocate(RSL2     )
	deallocate(SQSCAT   )
	deallocate(RDC      )
        deallocate(UUU      )
        deallocate(RHO      )
        deallocate(ZVG      )
        deallocate(LAI0     )
        deallocate(GRN0     )
        deallocate(Z0       )
	deallocate(D0       )
	deallocate(SFMC     )
	deallocate(RZMC     )
        deallocate(PRMC     )
        deallocate(ENTOT    )
        deallocate(WTOT     )
        deallocate(GHFLXSNO )
        deallocate(SHSNOW1  )
        deallocate(AVETSNOW1)
        deallocate(WAT10CM1 )
        deallocate(WATSOI1  )
        deallocate(ICESOI1  )
        deallocate(LHSNOW1  )
        deallocate(LWUPSNOW1)
        deallocate(LWDNSNOW1)
        deallocate(NETSWSNOW)
        deallocate(TCSORIG1 )
        deallocate(LHACC )
        deallocate(SUMEV )
        deallocate(TPSN1IN1 )
        deallocate(TPSN1OUT1)
        deallocate(GHFLXTSKIN)
        deallocate(WCHANGE  )
        deallocate(ECHANGE  )
        deallocate(HSNACC   )
        deallocate(EVACC    )
        deallocate(SHACC    )
        deallocate(VISDF    )
        deallocate(NIRDF    )
        deallocate(VSUVR    )
        deallocate(VSUVF    )
        deallocate(SNOVR    )
        deallocate(SNOVF    )
        deallocate(SNONR    )
        deallocate(SNONF    )
        deallocate(SHSBT    )
        deallocate(DSHSBT   )
        deallocate(EVSBT    )
        deallocate(DEVSBT   )
        deallocate(CFT      )
        deallocate(CFQ      )
        deallocate(TCO      )
        deallocate(QCO      )
        deallocate(DQS      )
        deallocate(QSAT     )
        deallocate(RA       )
        deallocate(CAT_ID   )
        deallocate(ALWX     )
        deallocate(BLWX     )

        RETURN_(ESMF_SUCCESS)

      end subroutine Driver

end subroutine RUN2


end module GEOS_CatchGridCompMod

