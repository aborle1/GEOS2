
!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
module GEOS_SaltwaterGridCompMod

!BOP
! !MODULE: GEOS_SaltwaterGridCompMod -- Implements slab saltwater tiles.

! !DESCRIPTION:
! 
!   {\tt GEOS\_Saltwater} is a light-weight gridded component that updates
!      the skin sub-tiles at saltwater points, be they ocean, estuary, or salt
!      lake. Currently each tile can have only two subtiles, open-water and ice.
!      But the code is easily extensible to multiple ice types,
!      and includes implementation of LANL CICE thermodynamics on salt water tiles.
!
!      The component is written with a two stage run method for use with
!      semi-implicit turbulence components. The first run stage computes
!      exchange coefficients for heat, moisture and momentum at each sub-tile
!      and combines these to tile space, accounting for sub tile variability
!      by passing back an effective surface value of the exchanged quantity.
!

! !USES:

  use sfclayer  ! using module that contains sfc layer code
  use ESMF
  use MAPL_Mod
  use GEOS_UtilsMod
  use DragCoefficientsMod
  
! LANL CICE Thermodynamics modules
  use ice_kinds_mod
  use ice_constants,      only: TFfresh, puny
  use ice_constants,      only: awtvdr, awtvdf, awtidr, awtidf
  use ice_constants,      only: m2_to_km2
  use ice_domain_size,    only: init_column_physics
  use ice_itd,            only: init_itd, cleanup_itd
  use ice_therm_vertical, only: init_thermo_vertical,  &
                                init_vertical_profile, &
                                thermo_vertical,       &
                                diagnose_internal_ice_temp, &
                                frzmlt_bottom_lateral   
  use ice_state,          only: nt_tsfc, nt_iage, nt_volpn, init_trcr_depend
  use ice_therm_itd,      only: linear_itd, add_new_ice, lateral_melt,    &
                                freeboard_ccsm
  use ice_init,           only: input_data, set_state_var, &
                                alloc_column_physics, dealloc_column_physics
  use ice_age,            only: iage_converter
  use ice_meltpond,       only: compute_ponds

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!EOP

  integer            :: ICE                                    ! set later via resource parameter, based on usage of CICE
  integer            :: WATER                                  ! set later via resource parameter, based on usage of CICE
  integer            :: NUM_SUBTILES                           ! set later via resource parameter, based on usage of CICE

  real, parameter    :: KUVR = 0.09

  integer            :: DO_CICE_THERMO                         ! default (=0) is to run saltwater, with no LANL CICE Thermodynamics

  integer, parameter :: NUM_3D_ICE_TRACERS = 3
  integer, parameter :: NUM_SNOW_LAYERS    = 1
  integer            :: NUM_ICE_LAYERS                         ! set via resource parameter
  integer            :: NUM_ICE_CATEGORIES                     ! set via resource parameter

! Following could also be controlled via resource parameter
  integer, parameter :: NUM_DUDP = 5                           ! number of DUst Depositions
  integer, parameter :: NUM_DUWT = 5
  integer, parameter :: NUM_DUSD = 5
  integer, parameter :: NUM_BCDP = 2                           ! number of Black Carbon 
  integer, parameter :: NUM_BCWT = 2
  integer, parameter :: NUM_OCDP = 2                           ! number of Organic Carbon 
  integer, parameter :: NUM_OCWT = 2

  integer, parameter :: NB_CHOU_UV  = 5                        ! number of UV bands
  integer, parameter :: NB_CHOU_NIR = 3                        ! number of near-IR bands
  integer, parameter :: NB_CHOU     = NB_CHOU_UV + NB_CHOU_NIR ! total number of bands


   contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

    !ARGUMENTS:

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
!   generic is used for tiles.

!EOP

!=============================================================================

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (MAPL_MetaComp),  pointer          :: MAPL
    type (ESMF_Config)                      :: CF

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // 'SetServices'

! Get my MAPL_Generic state
!--------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)


! Sea-Ice Thermodynamics computation: using CICE or not?
!-------------------------------------------------------

    call MAPL_GetResource ( MAPL,       DO_CICE_THERMO,     Label="USE_CICE_Thermo:" ,       DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS )
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run1, RC=STATUS )
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run2, RC=STATUS )
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,  Finalize, RC=STATUS )
    VERIFY_(STATUS)

! Get constants from CF
! ---------------------

    if (DO_CICE_THERMO == 0) then     
       NUM_ICE_CATEGORIES = 1
       NUM_ICE_LAYERS     = 1

       ! Before merging LANL CICEthermo with SaltWater, ICE, WATER and NUM_SUBTILES were set as parameters
       ICE                = 1
       WATER              = 2
    else
       call ESMF_ConfigGetAttribute(CF, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" , RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute(CF, NUM_ICE_LAYERS,     Label="CICE_N_ICE_LAYERS:" ,     RC=STATUS)
       VERIFY_(STATUS)

       ICE                = 2
       WATER              = 1
    end if
 
    NUM_SUBTILES       = NUM_ICE_CATEGORIES + 1

! Set the state variable specs.
! -----------------------------

!BOS

!  !EXPORT STATE:

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'EMIS',                              &
        LONG_NAME          = 'surface_emissivity',                &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        LONG_NAME          = 'surface_albedo_for_visible_beam',   &
        UNITS              = '1',                                 &
        SHORT_NAME         = 'ALBVR',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        LONG_NAME          = 'surface_albedo_for_visible_diffuse',&
        UNITS              = '1',                                 &
        SHORT_NAME         = 'ALBVF',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        LONG_NAME          = 'surface_albedo_for_near_infrared_beam', &
        UNITS              = '1',                                 &
        SHORT_NAME         = 'ALBNR',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        LONG_NAME          = 'surface_albedo_for_near_infrared_diffuse', &
        UNITS              = '1',                                 &
        SHORT_NAME         = 'ALBNF',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


     call MAPL_AddExportSpec(GC,                     &
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

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'ocean_snowfall'            ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'SNOWOCN'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'ocean_rainfall'            ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'RAINOCN'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'upward_sensible_heat_flux' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SHOUT'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'open_water_upward_sensible_heat_flux' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SHWTR'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'sea_ice_upward_sensible_heat_flux' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SHICE'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'surface_outgoing_longwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'HLWUP'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'open_water_net_downward_longwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'LWNDWTR'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'sea_ice_net_downward_longwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'LWNDICE'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'surface_net_downward_longwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'LWNDSRF'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'surface_net_downward_shortwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SWNDSRF'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'open_water_net_downward_shortwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SWNDWTR'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                     ,&
        LONG_NAME          = 'sea_ice_net_downward_shortwave_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SWNDICE'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'total_latent_energy_flux'  ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'HLATN'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'open_water_latent_energy_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'HLATWTR'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'sea_ice_latent_energy_flux',&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'HLATICE'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'TST',                               &
        LONG_NAME          = 'surface_skin_temperature',          &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'QST',                               &
        LONG_NAME          = 'surface_specific_humidity',         &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'TH',                                &
        LONG_NAME          = 'turbulence_surface_temperature',    &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'QH',                                &
        LONG_NAME          = 'turbulence_surface_specific_humidity', &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'UH',                                &
        LONG_NAME          = 'turbulence_surface_zonal_velocity', &
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'VH',                                &
        LONG_NAME          = 'turbulence_surface_meridional_velocity', &
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'DELTS',                             &
        LONG_NAME          = 'change_of_surface_skin_temperature',&
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'DELQS',                             &
        LONG_NAME          = 'change_of_surface_specific_humidity',&
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'CHT',                               &
        LONG_NAME          = 'surface_heat_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'CMT',                               &
        LONG_NAME          = 'surface_momentum_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'CQT',                               &
        LONG_NAME          = 'surface_moisture_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'CNT',                               &
        LONG_NAME          = 'neutral_drag_coefficient',          &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'RIT',                               &
        LONG_NAME          = 'surface_bulk_richardson_number',    &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'RET',                               &
        LONG_NAME          = 'surface_reynolds_number',           &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'FRACI',                             &
        LONG_NAME          = 'ice_covered_fraction_of_tile',      &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        SHORT_NAME         = 'FRACW',                             &
        LONG_NAME          = 'water_covered_fraction_of_tile',      &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                             &
        LONG_NAME          = 'surface_pressure',                  &
        UNITS              = 'Pa',                                &
        SHORT_NAME         = 'PS',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
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

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'surface_roughness'         ,&
        UNITS              = 'm'                         ,&
        SHORT_NAME         = 'Z0'                        ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
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
        LONG_NAME          = 'eastward_stress_over_water',&
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUXW'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'northward_stress_over_water',&
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUYW'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'eastward_stress_over_ice',  &
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUXI'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'northward_stress_over_ice',  &
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUYI'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'eastward_stress_on_ocean'  ,&
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUXO'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'northward_stress_on_ocean', &
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUYO'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'ocean_ustar_cubed',         &
        UNITS              = 'm+3 s-3'                   ,&
        SHORT_NAME         = 'OUSTAR3'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'surface_wind_speed',        &
        UNITS              = 'm s-1'                     ,&
        SHORT_NAME         = 'UU'                        ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                    ,&
          SHORT_NAME         = 'PENUVF',                    &
          LONG_NAME          = 'downwelling_uvr_diffuse_flux_at_skin_base',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                    ,&
          SHORT_NAME         = 'PENUVR',                    &
          LONG_NAME          = 'downwelling_uvr_direct_flux_at_skin_base',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                    ,&
          SHORT_NAME         = 'PENPAF',                    &
          LONG_NAME          = 'downwelling_par_diffuse_flux_at_skin_base',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                    ,&
          SHORT_NAME         = 'PENPAR',                    &
          LONG_NAME          = 'downwelling_par_direct_flux_at_skin_base',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'DCOOL',                     &
          LONG_NAME          = 'depth_of_cool_layer'       ,&
          UNITS              = 'm'                         ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&
          SHORT_NAME         = 'DWARM',                      &
          LONG_NAME          = 'depth_at_base_of_warm_layer',&
          UNITS              = 'm'                          ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                                 ,&
          SHORT_NAME         = 'TDROP',                             &
          LONG_NAME          = 'temperature_drop_across_cool_layer',&
          UNITS              = 'K'                                 ,&
          DIMS               = MAPL_DimsTileOnly                   ,&
          VLOCATION          = MAPL_VLocationNone                  ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'QCOOL',                     &
          LONG_NAME          = 'net_heating_in_cool_layer' ,&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'QWARM',                     &
          LONG_NAME          = 'net_heating_in_warm_layer' ,&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&
          SHORT_NAME         = 'SWCOOL',                     &
          LONG_NAME          = 'solar_heating_in_cool_layer',&
          UNITS              = 'W m-2'                      ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&
          SHORT_NAME         = 'SWWARM',                     &
          LONG_NAME          = 'solar_heating_in_warm_layer',&
          UNITS              = 'W m-2'                      ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                           ,&
          SHORT_NAME         = 'PHIW',                        &
          LONG_NAME          = 'Similarity_function_in_warm_layer',&
          UNITS              = '1'                           ,&
          DIMS               = MAPL_DimsTileOnly             ,&
          VLOCATION          = MAPL_VLocationNone            ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'LANGM'                     ,&
          LONG_NAME          = 'Langmuir_number'           ,&
          UNITS              = '1'                         ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                        ,&
          SHORT_NAME         = 'USTARW',                    &
          LONG_NAME          = 'ustar_over_water'          ,&
          UNITS              = 'm s-1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                                  ,&
          SHORT_NAME         = 'TBAR',                               &
          LONG_NAME          = 'mean_temperature_of_interface_layer',&
          UNITS              = 'K'                                  ,&
          DIMS               = MAPL_DimsTileOnly                    ,&
          VLOCATION          = MAPL_VLocationNone                   ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'LCOOL',                     &
          LONG_NAME          = 'Saunders_parameter'        ,&
          UNITS              = '1'                         ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'BCOOL',                     &
          LONG_NAME          = 'bouyancy_generation_in_cool_layer',&
          UNITS              = 'm+2 s-3'                   ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                        &
          SHORT_NAME         = 'TDEL',                     &
          LONG_NAME          = 'temperature_at_base_of_cool_layer', &
          UNITS              = 'K',                        &
          DIMS               = MAPL_DimsTileOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                        &
          SHORT_NAME         = 'TS_FOUND',                 &
          LONG_NAME          = 'foundation_temperature_for_interface_layer',        &
          UNITS              = 'K',                        &
          DIMS               = MAPL_DimsTileOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                        &
          SHORT_NAME         = 'TAUTW',                    &
          LONG_NAME          = 'relaxation_time_of_TW_to_TS_FOUND', &
          UNITS              = 's',                        &
          DIMS               = MAPL_DimsTileOnly,          &
          VLOCATION          = MAPL_VLocationNone,         &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                         ,&
          SHORT_NAME         = 'ZETA_W'                    ,&
          LONG_NAME          = 'Stability_parameter_in_Warm_Layer',                 &
          UNITS              = '1'                         ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
          LONG_NAME          = 'river_discharge_at_ocean_points',&
          UNITS              = 'kg m-2 s-1'                ,&
          SHORT_NAME         = 'DISCHARGE'                 ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

!  !INTERNAL STATE:


     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'HSKINW',                            &
        LONG_NAME          = 'water_skin_layer_mass',             &
        UNITS              = 'kg m-2',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 5.0*MAPL_RHO_SEAWATER,               &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'TSKINW',                            &
        LONG_NAME          = 'water_skin_temperature',            &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 280.0,                               &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'SSKINW',                            &
        LONG_NAME          = 'water_skin_salinity',               &
        UNITS              = 'psu',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 30.0,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'HSKINI',                            &
        LONG_NAME          = 'ice_skin_layer_mass',               &
        UNITS              = 'kg m-2',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.5*MAPL_RHOWTR,                     &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

  if (DO_CICE_THERMO == 0) then
     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'TSKINI',                            &
        LONG_NAME          = 'ice_skin_temperature',              &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = MAPL_TICE-1.8,                               &
                                                       RC=STATUS  )
     VERIFY_(STATUS)
  else
     call MAPL_AddInternalSpec(GC,                                &
         SHORT_NAME         = 'TSKINI',                            &
         LONG_NAME          = 'ice_skin_temperature',              &
         UNITS              = 'K',                                 &
         UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &    ! accroding to Atanas, because of this line, TSKINI is now rank 2 array.
         DIMS               = MAPL_DimsTileOnly,                   &    ! and therefore must be protected via DO_CICE_THERMO flag. SA. Aug.2015
         VLOCATION          = MAPL_VLocationNone,                  &
         FRIENDLYTO         = 'SEAICE',                            &
         DEFAULT            = MAPL_TICE-1.8,                       &
                                           RC=STATUS  )
    VERIFY_(STATUS)
  end if

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'SSKINI',                            &
        LONG_NAME          = 'ice_skin_salinity',                 &
        UNITS              = 'psu',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 30.0,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'QS',                                &
        LONG_NAME          = 'surface_specific_humidity',         &
        UNITS              = 'kg kg-1',                           &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.01,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CH',                                &
        LONG_NAME          = 'surface_heat_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CM',                                &
        LONG_NAME          = 'surface_momentum_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CQ',                                &
        LONG_NAME          = 'surface_moisture_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'Z0',                                &
        LONG_NAME          = 'aerodynamic_roughness',             &
        UNITS              = 'm',                                 &
        DEFAULT            = 0.00005,                             &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'WW',                                &
        LONG_NAME          = 'vertical_velocity_scale_squared',   &
        UNITS              = 'm+2 s-2',                           &
        DEFAULT            = 0.0,                                 &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'TWMTS',                             &
        LONG_NAME          = 'departure_of_skin_temperature_from_mean_interface_temperature',   &
        UNITS              = 'K',                                 &
        DEFAULT            = 0.0,                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!  !IMPORT STATE:

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'ALW',                               &
        LONG_NAME          = 'linearization_of_surface_upwelling_longwave_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'BLW',                               &
        LONG_NAME          = 'linearization_of_surface_upwelling_longwave_flux', &
        UNITS              = 'W m-2 K-1',                         &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'LWDNSRF',                           &
        LONG_NAME          = 'surface_downwelling_longwave_flux', &
        UNITS              = 'W m-2',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC                             ,&
        LONG_NAME          = 'surface_downwelling_par_beam_flux' ,&
        UNITS              = 'W m-2'                             ,&
        SHORT_NAME         = 'DRPAR'                             ,&
        DIMS               = MAPL_DimsTileOnly                   ,&
        VLOCATION          = MAPL_VLocationNone                  ,&
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

    call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'evaporation',                       &
        UNITS              = 'kg m-2 s-1',                        &
        SHORT_NAME         = 'EVAP ',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'upward_sensible_heat_flux',         &
        UNITS              = 'W m-2',                             &
        SHORT_NAME         = 'SH',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'eastward_surface_stress',           &
        UNITS              = 'N m-2',                             &
        SHORT_NAME         = 'TAUX',                              &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'northward_surface_stress',          &
        UNITS              = 'N m-2',                             &
        SHORT_NAME         = 'TAUY',                              &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'derivative_of_evaporation',         &
        UNITS              = 'kg m-2 s-1',                        &
        SHORT_NAME         = 'DEVAP',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'derivative_of_upward_sensible_heat_flux', &
        UNITS              = 'W m-2',                             &
        SHORT_NAME         = 'DSH',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'snowfall',                          &
        UNITS              = 'kg m-2 s-1',                        &
        SHORT_NAME         = 'SNO',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

! Surface air quantities

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'surface_air_temperature',           &
        UNITS              = 'K',                                 &
        SHORT_NAME         = 'TA',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'surface_air_specific_humidity',     &
        UNITS              = 'kg kg-1',                           &
        SHORT_NAME         = 'QA',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'surface_wind_speed',                &
        UNITS              = 'm s-1',                             &
        SHORT_NAME         = 'UU',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
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

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'surface_layer_height',              &
        UNITS              = 'm',                                 &
        SHORT_NAME         = 'DZ',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'surface_pressure',                  &
        UNITS              = 'Pa',                                &
        SHORT_NAME         = 'PS',                                &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        LONG_NAME          = 'liquid_water_convective_precipitation',&
        UNITS              = 'kg m-2 s-1'                        ,&
        SHORT_NAME         = 'PCU'                               ,&
        DIMS               = MAPL_DimsTileOnly                   ,&
        VLOCATION          = MAPL_VLocationNone                  ,&
                                                       RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC                            ,&
        LONG_NAME          = 'liquid_water_large_scale_precipitation',&
        UNITS              = 'kg m-2 s-1'                       ,&
        SHORT_NAME         = 'PLS'                              ,&
        DIMS               = MAPL_DimsTileOnly                  ,&
        VLOCATION          = MAPL_VLocationNone                 ,&
                                                      RC=STATUS  ) 
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'THATM',                             &
        LONG_NAME          = 'effective_surface_skin_temperature',&
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'QHATM',                             &
        LONG_NAME          = 'effective_surface_specific_humidity',&
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'UHATM',                             &
        LONG_NAME          = 'effective_surface_zonal_velocity',&
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'VHATM',                             &
        LONG_NAME          = 'effective_surface_meridional_velocity',&
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'CTATM',                             &
        LONG_NAME          = 'surface_exchange_coefficient_for_heat', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'CQATM',                             &
        LONG_NAME          = 'surface_exchange_coefficient_for_moisture', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'CMATM',                             &
        LONG_NAME          = 'surface_exchange_coefficient_for_momentum', &
        UNITS              = 'kg m-2 s-1',                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'FRACICE',                           &
        LONG_NAME          = 'ice_covered_fraction_of_tile',      &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'UW',                                &
        LONG_NAME          = 'zonal_velocity_of_surface_water',   &
        UNITS              = 'm s-1 ',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'UI',                                &
        LONG_NAME          = 'zonal_velocity_of_surface_ice',     &
        UNITS              = 'm s-1 ',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'VW',                                &
        LONG_NAME          = 'meridional_velocity_of_surface_water',   &
        UNITS              = 'm s-1 ',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'VI',                                &
        LONG_NAME          = 'meridional_velocity_of_surface_ice',     &
        UNITS              = 'm s-1 ',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'KPAR',                              &
        LONG_NAME          = 'PAR_extinction_coefficient',        &
        UNITS              = 'm-1',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
          SHORT_NAME         = 'TS_FOUND',                        &
          LONG_NAME          = 'foundation_temperature_for_interface_layer',&
          UNITS              = 'K',                               &
          DIMS               = MAPL_DimsTileOnly,                 &
          VLOCATION          = MAPL_VLocationNone,                &
        DEFAULT            = 280.0,                               &
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddImportSpec (GC,                                   &
         SHORT_NAME = 'DTSDT',                                     &
         LONG_NAME  = 'skin_temperature_analysis_tendency',        &
         UNITS      = 'K s-1',                                     &
         RESTART    = MAPL_RestartSkip,                            &
         DIMS       = MAPL_DimsTileOnly,                           &
         VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)
    
    call MAPL_AddImportSpec(GC,                    &
          LONG_NAME          = 'river_discharge_at_ocean_points',&
          UNITS              = 'kg m-2 s-1'                ,&
          SHORT_NAME         = 'DISCHARGE'                 ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RESTART            = MAPL_RestartSkip            ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)

! Additions for LANL CICE Thermodynamics
!----------------------------------

cice_extra_vars: if (DO_CICE_THERMO /= 0) then
!-------------------Exports---------------------------------------------------------------

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FRAZIL',                    &
    LONG_NAME          = 'frazil_ice_growth'         ,&
    UNITS              = 'm s-1'           ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FRESH',                     &
    LONG_NAME          = 'fresh_water_flux_to_ocean' ,&
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSALT',                     &
    LONG_NAME          = 'salt_flux_to_ocean'        ,&
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FHOCN',                     &
    LONG_NAME          = 'actual_ocean_ice_flux'     ,&
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSWTHRU',                   &
    LONG_NAME          = 'SW_flux_thru_ice_to_ocean' ,&
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSWABS',                         &
    LONG_NAME          = 'SW_flux_absorbed_by_skin_layer' ,&
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'CONGEL',                    &
    LONG_NAME          = 'congelation_ice_growth'    ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'SNOICE',                    &
    LONG_NAME          = 'snow_ice_formation'        ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTT',                     &
    LONG_NAME          = 'top_ice_melt'              ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTB',                     &
    LONG_NAME          = 'basal_ice_melt'            ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTL',                     &
    LONG_NAME          = 'lateral_ice_melt'          ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTS',                     &
    LONG_NAME          = 'snow_melt'                 ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'HICE',                        &
    LONG_NAME          = 'grid_cell_mean_ice_thickness',&
    UNITS              = 'm'                         ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'HSNO',                         &
    LONG_NAME          = 'grid_cell_mean_snow_thickness',&
    UNITS              = 'm'                         ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'TSKINWCICE',                 &
    LONG_NAME          = 'CICE_water_skin_temperature',&
    UNITS              = 'K'                         ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'ISTSFC',                         &
    LONG_NAME          = 'snow_or_ice_surface_temperature',&
    UNITS              = 'C'                         ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'IAGE',                      &
    LONG_NAME          = 'sea_ice_age'               ,&
    UNITS              = 'years'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'SSKINW2',                   &
    LONG_NAME          = 'sea_skin_layer_salinity',   &
    UNITS              = 'psu'                       ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                           &
    SHORT_NAME         = 'DAIDTT',                                 &
    LONG_NAME          = 'ice_area_tendency_dueto_thermodynamics', &
    UNITS              = '% day-1',                                &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                           &
    SHORT_NAME         = 'DVIDTT',                                   &
    LONG_NAME          = 'ice_volume_tendency_dueto_thermodynamics', &
    UNITS              = 'cm day-1',                                 &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                        &
    SHORT_NAME         = 'FBOT',                                     &
    LONG_NAME          = 'net_downward_heat_flux_from_ice_to_ocean', &
    UNITS              = 'W m-2',                                    &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                   &
         SHORT_NAME         = 'USTARI'                   ,           &
    LONG_NAME          = 'ice_ocean_friction_velocity',         &
    UNITS              = 'm s-1'                   ,            &
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,                         &
    SHORT_NAME         = 'HICEUNT',                                       &
    LONG_NAME          = 'grid_cell_mean_ice_thickness_untouched_by_run2',&
    UNITS              = 'm'                         ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,     &
    SHORT_NAME         = 'SNOONICE',                  &
    LONG_NAME          = 'snow_fall_on_top_of_ice',   &
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                             &
         SHORT_NAME         = 'SIALB'                       ,&
    LONG_NAME          = 'broad_band_sea_ice_albedo'   ,&
    UNITS              = '1'                           ,&
    DIMS               = MAPL_DimsTileOnly             ,&
    VLOCATION          = MAPL_VLocationNone            ,&
                                               RC=STATUS  )
  VERIFY_(STATUS)

  ! CMIP5 exports; this is only one part of the list, the rest are in CICEDyna     

  call MAPL_AddExportSpec(GC,                          &
         SHORT_NAME         = 'evap_CMIP5'                ,&
    LONG_NAME          = 'water_evaporation_flux',    &
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'pr_CMIP5'                  ,                                        &
    LONG_NAME          = 'surface_rainfall_rate_into_the_sea_ice_portion_of_the_grid_cell',   &
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'prsn_CMIP5'                ,                                        &
    LONG_NAME          = 'surface_snowfall_rate_into_the_sea_ice_portion_of_the_grid_cell',   &
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'grFrazil_CMIP5'            ,&
    LONG_NAME          = 'frazil_sea_ice_growth_rate',&
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'grCongel_CMIP5'                    ,&
    LONG_NAME          = 'congelation_sea_ice_growth_rate',   &
    UNITS              = 'kg m-2 s-1'                        ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'grLateral_CMIP5'              ,&
        LONG_NAME          = 'lateral_sea_ice_growth_rate'  ,&
        UNITS              = 'kg m-2 s-1'                   ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'snoToIce_CMIP5'            ,&
        LONG_NAME          = 'snow_ice_formation_rate',   &
        UNITS              = 'kg m-2 s-1'                ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'snomelt_CMIP5'             ,&
        LONG_NAME          = 'snow_melt_rate',            &
        UNITS              = 'kg m-2 s-1'                ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'tmelt_CMIP5'               ,                 &
        LONG_NAME          = 'rate_of_melt_at_upper_surface_of_sea_ice',   &
        UNITS              = 'kg m-2 s-1'                                 ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'bmelt_CMIP5'               ,     &
        LONG_NAME          = 'rate_of_melt_at_sea_ice_base',   &
        UNITS              = 'kg m-2 s-1'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'sfdsi_CMIP5'               ,         &
        LONG_NAME          = 'downward_sea_ice_basal_salt_flux',   &
        UNITS              = 'kg m-2 s-1'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'hfsifrazil_CMIP5'             ,                          &
        LONG_NAME          = 'heat_flux_into_sea_water_due_to_frazil_ice_formation',   &
        UNITS              = 'W m-2'                        ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                             &
         SHORT_NAME         = 'ialb_CMIP5'                   ,&
        LONG_NAME          = 'bare_sea_ice_albedo'          ,&
        UNITS              = '1'                            ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                             &
         SHORT_NAME         = 'rsdssi_CMIP5'                 ,             &
        LONG_NAME          = 'surface_downwelling_shortwave_flux_in_air' ,&
        UNITS              = 'W m-2'                        ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                             &
         SHORT_NAME         = 'rsussi_CMIP5'                 ,           &
        LONG_NAME          = 'surface_upwelling_shortwave_flux_in_air' ,&
        UNITS              = 'W m-2'                        ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS) 

  call MAPL_AddExportSpec(GC,                             &
         SHORT_NAME         = 'fsitherm_CMIP5'               ,                           &
        LONG_NAME          = 'water_flux_into_sea_water_due_to_sea_ice_thermodynamics' ,&
        UNITS              = 'kg m-2 s-1'                   ,&
        DIMS               = MAPL_DimsTileOnly              ,&
        VLOCATION          = MAPL_VLocationNone             ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
        SHORT_NAME         = 'HSKINW0'                ,&
        LONG_NAME          = 'skin_water_mass_at_the_beginning_of_the_time_step'            ,&
        UNITS              = 'kg m-2'                    ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
        SHORT_NAME         = 'HSKINW1'                ,&
        LONG_NAME          = 'skin_water_mass_at_the_end_of_the_time_step'            ,&
        UNITS              = 'kg m-2'                    ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
        SHORT_NAME         = 'TSKINW0'                ,&
        LONG_NAME          = 'water_skin_temp_at_the_beginning_of_the_time_step'            ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
        SHORT_NAME         = 'TSKINW1'                ,&
        LONG_NAME          = 'water_skin_temp_at_the_end_of_the_time_step'            ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
         SHORT_NAME         = 'TSKINWinc1'                ,&
        LONG_NAME          = 'DTS_for_step_1'            ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
         SHORT_NAME         = 'TSKINWinc2'                ,&
        LONG_NAME          = 'DTS_for_step_2'            ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                          &
         SHORT_NAME         = 'TSKINWinc3'                ,&
        LONG_NAME          = 'DTS_for_step_3'            ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                     &
         SHORT_NAME         = 'TSKINWinctotal'            ,&
        LONG_NAME          = 'DTS_for_complete_step'     ,&
        UNITS              = 'K'                         ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  ) 
  VERIFY_(STATUS)
 
  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'FWFLUX'                    ,     &
          LONG_NAME          = 'fresh_water_flux_weighted_by_fr',&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTPR'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_due_to_precip',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTFLX'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTSW'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_shortwave_over_openwater',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTLW'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_longwave',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTLWD'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_longwave_down',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTLWU'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_longwave_up',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTSH'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_sensible',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTLH'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_latent',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTSWI'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_shortwave_over_ice',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'AHTSNO'                    ,     &
          LONG_NAME          = 'atmospheric_heat_flux_into_skin_due_to_melting_snow',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'CFQW'                    ,     &
          LONG_NAME          = 'CFQ_over_open_water',&
          UNITS              = '1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'EVPW0'                    ,     &
          LONG_NAME          = 'evaporation_over_open_water_before_correction',&
          UNITS              = 'kg m-2 s-1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'EVPW0A'                    ,     &
          LONG_NAME          = 'evaporation_over_open_water_before_correction_partA',&
          UNITS              = 'kg m-2 s-1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'EVPW0B'                    ,     &
          LONG_NAME          = 'evaporation_over_open_water_before_correction_partB',&
          UNITS              = 'kg m-2 s-1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'EVPW1'                    ,     &
          LONG_NAME          = 'evaporation_over_open_water_correction_by_dts',&
          UNITS              = 'kg m-2 s-1'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'FCONDTOP'                  ,             &
          LONG_NAME          = 'conductive_heat_flux_at_ice_top_surface',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )

  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'FCONDBOT'                  ,                &
          LONG_NAME          = 'conductive_heat_flux_at_ice_bottom_surface',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME         = 'NEWICEERG'                  ,                &
          LONG_NAME          = 'heat_flux_associated_with_new_ice_generation',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
         SHORT_NAME          = 'SUBLIMFLX'                  ,                &
          LONG_NAME          = 'heat_flux_associated_with_sublimation_of_snow_ice',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                            ,&
          SHORT_NAME         = 'CO2SC',                     &
          LONG_NAME          = 'CO2 Surface Concentration Bin 001',&
          UNITS              = '1e-6'                      ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  ) 
  VERIFY_(STATUS)

!  Category dimensional exports

   call MAPL_AddExportSpec(GC,                    &                  
         SHORT_NAME         = 'FCONDBOTN'                 ,                            &
          LONG_NAME          = 'conductive_heat_flux_at_ice_bottom_over_ice_categories',&
          UNITS              = 'W m-2'                     ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/)      ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   ! this export actually has dimensions(nlayer,nice) but is collapsed
   ! into one for ease of history
   call MAPL_AddExportSpec(GC,                    &
          LONG_NAME          = 'internal_ice_temperature_over_ice_categories',&
          UNITS              = 'degC'                ,&
          SHORT_NAME         = 'TINZ'                 ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES*NUM_ICE_LAYERS/) ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'DUDP'                      ,&
          LONG_NAME          = 'Dust Dry Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUDP/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'DUWT'                      ,&
          LONG_NAME          = 'Dust Wet Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUWT/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
 
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'DUSD'                      ,&
          LONG_NAME          = 'Dust Sedimentation'        ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUSD/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'BCDP'                            ,&
          LONG_NAME          = 'Black Carbon Dry Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'BCWT'                            ,&
          LONG_NAME          = 'Black Carbon Wet Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'OCDP'                            ,&
          LONG_NAME          = 'Organic Carbon Dry Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'OCWT'                            ,&
          LONG_NAME          = 'Organic Carbon Wet Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'FSWBAND'                         ,                   &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                    &
         SHORT_NAME         = 'FSWBANDNA'                       ,                                       &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
     
! following 3 exports (HFLUX, WATERFLUX, SALTFLUX) are for Yury- need to fill them up.

   call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'HFLUX',                            &
        LONG_NAME          = 'heat_flux_bw_saltwater_ocean',     &
        UNITS              = 'W m-2',                            &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'WATERFLUX',                        &
        LONG_NAME          = 'water_flux_bw_saltwater_ocean',    &
        UNITS              = 'kg m-2 s-1',                       &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME         = 'SALTFLUX',                         &
        LONG_NAME          = 'salt_flux_bw_saltwater_ocean',     &
        UNITS              = 'kg m-2 s-1',                       &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

!-------------------Internal--------------------------------------------------------------

    call MAPL_AddInternalSpec(GC,                                  &
        SHORT_NAME         = 'FR',                                &
        LONG_NAME          = 'subtile_fractions_of_grid_cell',    &
        UNITS              = '1',                                 &
         PRECISION          = MAPL_R8,                             &    ! Bin, Yury: Please listen to Matt and Atanas! Kindly work on interfacing  
         DIMS               = MAPL_DimsTileOnly,                   &    ! all the R8 variables- internally, within CICE and doing GEOS computations in 
         UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &    ! R4. SA. Aug.2015
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLICE',                            &
        LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLSNO',                            &
        LONG_NAME          = 'snow_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLPOND',                           &
        LONG_NAME          = 'pond_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'APONDN',                            &
        LONG_NAME          = 'pond_concentration',                &
        UNITS              = '1',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'HPONDN',                            &
        LONG_NAME          = 'pond_depth',                        &
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'ERGICE',                            &
        LONG_NAME          = 'ice_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS,NUM_ICE_CATEGORIES/),&
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'ERGSNO',                            &
        LONG_NAME          = 'snow_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS,NUM_ICE_CATEGORIES/),&
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'TAUAGE',                            &
        LONG_NAME          = 'volume_weighted_mean_ice_age',      &
        UNITS              = 's',                                 &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                               &
        SHORT_NAME         = 'SLMASK',                           &
        LONG_NAME          = 'salt_water_lake_mask',             &
        UNITS              = '1',                                &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
        DEFAULT            = 0.0,                                &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

!-------------------Imports---------------------------------------------------------------

   call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'TAUXBOT',                           &
        LONG_NAME          = 'eastward_stress_at_base_of_ice',    &
        UNITS              = 'N m-2',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'TAUYBOT',                           &
        LONG_NAME          = 'northward_stress_at_base_of_ice',   &
        UNITS              = 'N m-2',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'CO2SC',                             &
        LONG_NAME          = 'CO2 Surface Concentration Bin 001', &
        UNITS              = '1e-6',                              &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        RESTART            = MAPL_RestartSkip,                    &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                            &
         SHORT_NAME         = 'DUDP'                      ,&
          LONG_NAME          = 'Dust Dry Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUDP/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RESTART            = MAPL_RestartSkip            ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                            &
         SHORT_NAME         = 'DUWT'                      ,&
          LONG_NAME          = 'Dust Wet Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUWT/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RESTART            = MAPL_RestartSkip            ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                            &
         SHORT_NAME         = 'DUSD'                      ,&
          LONG_NAME          = 'Dust Sedimentation'        ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUSD/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RESTART            = MAPL_RestartSkip            ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                  &
         SHORT_NAME         = 'BCDP'                            ,&
          LONG_NAME          = 'Black Carbon Dry Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                  &
         SHORT_NAME         = 'BCWT'                            ,&
          LONG_NAME          = 'Black Carbon Wet Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                  &
         SHORT_NAME         = 'OCDP'                            ,&
          LONG_NAME          = 'Organic Carbon Dry Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                                  &
         SHORT_NAME         = 'OCWT'                            ,&
          LONG_NAME          = 'Organic Carbon Wet Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                    &
         SHORT_NAME         = 'FSWBAND'                         ,                   &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,                    &
         SHORT_NAME         = 'FSWBANDNA'                       ,                                       &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RESTART            = MAPL_RestartSkip                  ,&
          RC=STATUS  ) 
   VERIFY_(STATUS)
endif cice_extra_vars
      call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'UUA',                             &
        LONG_NAME          = 'interpolated_effective_surface_zonal_velocity',&
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        RESTART            = MAPL_RestartSkip,                    &
        RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'VVA',                             &
        LONG_NAME          = 'interpolated_effective_surface_meridional_velocity',&
        UNITS              = 'm s-1',                             &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        RESTART            = MAPL_RestartSkip,                    &
        RC=STATUS  )
     VERIFY_(STATUS)


!EOS


! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC,    name="INITIALIZE"   ,         RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerAdd(GC,    name="RUN1"   ,               RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="RUN2"  ,                RC=STATUS)
    VERIFY_(STATUS)
  
    call MAPL_TimerAdd(GC,    name="-BeforeSaltWaterCore"  ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-OpenWater"  ,          RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Thermo1"    ,          RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Thermo2"    ,          RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Albedo"     ,          RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerAdd(GC,    name="FINALIZE"   ,         RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( GC,  RC=STATUS )
    VERIFY_(STATUS)
 
! Set the Run entry point
! -----------------------

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: Initialize -- Initialize method for the GEOS CICE Thermodynamic

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the CICE thermodynamics Gridded Component.
!   It then does a Generic\_Initialize and also CICE data structures 

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm 
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME
    
! Local derived type aliases

    type (MAPL_MetaComp    ), pointer   :: MAPL => null()

    real                        :: DTI
    real                        :: ALBICEV, ALBSNOWV, ALBICEI, ALBSNOWI
    real                        :: USTAR_MIN, AHMAX
    real                        :: KSNO

    character(len=ESMF_MAXSTR)  :: CONDTYPE
    character(len=ESMF_MAXSTR)  :: SHORTWAVE 

    integer                     :: DO_POND
    integer                     :: PRES_ICE

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOn(MAPL,"TOTAL")
    call MAPL_TimerOn(MAPL,"INITIALIZE")

    call MAPL_Get(MAPL, HEARTBEAT = DTI, RC=STATUS)
    VERIFY_(STATUS)

    cice_init_: if (DO_CICE_THERMO /= 0) then
       call MAPL_GetResource ( MAPL, DTI,       Label="CICE_DT:",           DEFAULT=DTI,               RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, ALBICEV,   Label="ALBICEV:",           DEFAULT=0.73,              RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, ALBICEI,   Label="ALBICEI:",           DEFAULT=0.33,              RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, ALBSNOWV,  Label="ALBSNOWV:",          DEFAULT=0.96,              RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, ALBSNOWI,  Label="ALBSNOWI:",          DEFAULT=0.68,              RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, CONDTYPE,  Label="CICE_CONDUCTIVITY:", DEFAULT="bubbly",          RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, SHORTWAVE, Label="CICE_SHORTWAVE:" ,   DEFAULT="shortwave_ccsm" , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, DO_POND,   Label="CICE_DO_POND:" ,     DEFAULT=0,                 RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, USTAR_MIN, Label="CICE_USTAR_MIN:",    DEFAULT=0.001,             RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, AHMAX,     Label="CICE_AH_MAX:",       DEFAULT=0.5,               RC=STATUS)
       VERIFY_(STATUS)

       ! It is desired to sometimes run the coupled model with prescribed ice. 
       ! 1: prescribe ice, as in AMIP mode.
       call MAPL_GetResource ( MAPL, PRES_ICE,  Label="PRESCRIBED_ICE:" , DEFAULT=1, RC=STATUS) 
       VERIFY_(STATUS)

       if (PRES_ICE == 1) then
          KSNO = 2.0  ! sea ice conductivity used in zero-layer ice param. 
       else
          KSNO = 0.3  ! true snow conductivity
       endif

       if(MAPL_AM_I_ROOT()) then
          print*, 'Model time step = ', DTI
          print*, 'Sea ice albedo parameters:'
          print*, 'ALBICEV  = ', ALBICEV
          print*, 'ALBICEI  = ', ALBICEI
          print*, 'ALBSNOWV = ', ALBSNOWV
          print*, 'ALBSNOWI = ', ALBSNOWI
          print*, 'Sea ice conductivity parameterization:'
          print*, 'CONDTYPE = ', CONDTYPE

          if (DO_POND == 1) then
             print*, 'DO explicit melt ponding'
          else
             print*, 'DO NOT do any explicit melt ponding'
          endif

          print*, 'Sea ice shortwave parameterization:'
          print*, 'shortwave = ', SHORTWAVE
          print*, 'ustar_min = ', USTAR_MIN
          print*, 'ahmax     = ', AHMAX
       endif

       call init_column_physics(NUM_ICE_CATEGORIES,NUM_ICE_LAYERS)
       call alloc_column_physics( MAPL_AM_I_Root(), Iam )
       call input_data (DTI, ALBICEV, ALBSNOWV, ALBICEI, ALBSNOWI, CONDTYPE, USTAR_MIN, AHMAX, KSNO)
       call init_thermo_vertical
       call init_itd
       call init_trcr_depend(.true., (DO_POND==1))  ! 2nd argument must evaluate to a logical, e.g., TR_POND = DO_POND == 1 
    endif cice_init_

! Call Initialize for every Child
!--------------------------------

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)
 
! All Done
!---------

    call MAPL_TimerOff(MAPL,"INITIALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL")

    RETURN_(ESMF_SUCCESS)

  end subroutine Initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: RUN1 -- First Run stage for the Saltwater component
! !INTERFACE:

subroutine RUN1 ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:
  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the sea-surface conditions

!EOP


! ErrLog Variables

  character(len=ESMF_MAXSTR)      :: IAm
  integer                         :: STATUS
  character(len=ESMF_MAXSTR)      :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer   :: MAPL => null()
  type (ESMF_Config)              :: CF
  type (ESMF_State   )            :: INTERNAL

! pointers to export

   real, pointer, dimension(:  )  :: TH     => null()
   real, pointer, dimension(:  )  :: QH     => null()
   real, pointer, dimension(:  )  :: UH     => null()
   real, pointer, dimension(:  )  :: VH     => null()
   real, pointer, dimension(:  )  :: TST    => null()
   real, pointer, dimension(:  )  :: QST    => null()
   real, pointer, dimension(:  )  :: CHT    => null()
   real, pointer, dimension(:  )  :: CMT    => null()
   real, pointer, dimension(:  )  :: CQT    => null()
   real, pointer, dimension(:  )  :: CNT    => null()
   real, pointer, dimension(:  )  :: RIT    => null()
   real, pointer, dimension(:  )  :: RET    => null()
   real, pointer, dimension(:  )  :: Z0O    => null()
   real, pointer, dimension(:  )  :: Z0H    => null()
   real, pointer, dimension(:  )  :: MOT2M  => null()
   real, pointer, dimension(:  )  :: MOQ2M  => null()
   real, pointer, dimension(:  )  :: MOU2M  => null()
   real, pointer, dimension(:  )  :: MOV2M  => null()
   real, pointer, dimension(:  )  :: MOT10M => null()
   real, pointer, dimension(:  )  :: MOQ10M => null()
   real, pointer, dimension(:  )  :: MOU10M => null()
   real, pointer, dimension(:  )  :: MOV10M => null()
   real, pointer, dimension(:  )  :: MOU50M => null()
   real, pointer, dimension(:  )  :: MOV50M => null()
   real, pointer, dimension(:  )  :: GST    => null()
   real, pointer, dimension(:  )  :: VNT    => null()

! pointers to internal

   real, pointer, dimension(:  )  :: TW  => null()
   real, pointer, dimension(:  )  :: TI  => null()
   real, pointer, dimension(:,:)  :: QS  => null()
   real, pointer, dimension(:,:)  :: CH  => null()
   real, pointer, dimension(:,:)  :: CM  => null()
   real, pointer, dimension(:,:)  :: CQ  => null()
   real, pointer, dimension(:,:)  :: WW  => null()
   real, pointer, dimension(:,:)  :: Z0  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)  :: FR8 => null()      ! ice fr is internal with LANL CICE.
   real,               pointer, dimension(:,:)  :: TI8 => null()      ! ice temperature    with LANL CICE uses TI8 not TI.

! pointers to import

   real, pointer, dimension(:)    :: UU  => null()
   real, pointer, dimension(:)    :: UWINDLMTILE => null()
   real, pointer, dimension(:)    :: VWINDLMTILE => null()
   real, pointer, dimension(:)    :: UW  => null()
   real, pointer, dimension(:)    :: UI  => null()
   real, pointer, dimension(:)    :: VW  => null()
   real, pointer, dimension(:)    :: VI  => null()
   real, pointer, dimension(:)    :: DZ  => null()     
   real, pointer, dimension(:)    :: TA  => null()
   real, pointer, dimension(:)    :: QA  => null()     
   real, pointer, dimension(:)    :: PS  => null()
   real, pointer, dimension(:)    :: PCU => null()
   real, pointer, dimension(:)    :: FI  => null()

   integer                        :: N
   integer                        :: NT
   integer                        :: NC
   integer                        :: niter


   real, allocatable              :: TS (:,:)
   real, allocatable              :: US (:,:)
   real, allocatable              :: VS (:,:)
   real, allocatable              :: FR (:,:)
   real, allocatable              :: CN (:)
   real, allocatable              :: RE (:)
   real, allocatable              :: ZT (:)
   real, allocatable              :: ZQ (:)
   real, allocatable              :: UUU(:)
   real, allocatable              :: CHB(:)
   real, allocatable              :: CQB(:)
   real, allocatable              :: CMB(:)
   real, allocatable              :: UCN(:)
   real, allocatable              :: LAI(:)
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
   real, allocatable              :: RHO(:)
   real, allocatable              :: VKH(:)
   real, allocatable              :: fakelai(:)
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

   real            :: OCEANICEZ0
   real            :: OCEANZ0
   real            :: ICEZ0
   real, parameter :: HPBL = 1000.

   integer         :: CHOOSEMOSFC
   integer         :: CHOOSEZ0
!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run1"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Start Total timer
!------------------

   call MAPL_TimerOn(MAPL,"TOTAL")
   call MAPL_TimerOn(MAPL,"RUN1" )

! Get parameters from generic state.
!-----------------------------------

    call MAPL_Get(MAPL,                          &
         INTERNAL_ESMF_STATE = INTERNAL,         &
                                       RC=STATUS )
    VERIFY_(STATUS)

! Get parameters (0:Louis, 1:Monin-Obukhov)
! -----------------------------------------
    call MAPL_GetResource ( MAPL, CHOOSEMOSFC, Label="CHOOSEMOSFC:", DEFAULT=1, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, CHOOSEZ0,    Label="CHOOSEZ0:",    DEFAULT=3, RC=STATUS)
    VERIFY_(STATUS)

! Get roughness parameters with and without CICE Thermodynamics
! -------------------------------------------------------------
    call MAPL_GetResource ( MAPL, OCEANICEZ0,  Label="OCEANICEZ0:" , DEFAULT=1.0e-3, RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, OCEANZ0,     Label="OCEANZ0:" ,    DEFAULT=1.0e-3, RC=STATUS) 
    VERIFY_(STATUS)
! icez0 value is based on literature (Ask Bin). It could be revisited, later.
    call MAPL_GetResource ( MAPL, ICEZ0,       Label="ICEZ0:" ,      DEFAULT=5.0e-4, RC=STATUS)
    VERIFY_(STATUS)

! Pointers to inputs
!-------------------

   call MAPL_GetPointer(IMPORT,UU     , 'UU'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UWINDLMTILE     , 'UWINDLMTILE'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VWINDLMTILE     , 'VWINDLMTILE'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UW     , 'UW'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UI     , 'UI'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VW     , 'VW'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VI     , 'VI'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DZ     , 'DZ'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,TA     , 'TA'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,QA     , 'QA'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PS     , 'PS'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PCU    , 'PCU'    ,    RC=STATUS)
   VERIFY_(STATUS)
   if (DO_CICE_THERMO == 0) then
      call MAPL_GetPointer(IMPORT,FI     , 'FRACICE',    RC=STATUS)
      VERIFY_(STATUS)
   endif


! Pointers to internals
!----------------------

   call MAPL_GetPointer(INTERNAL,TW   , 'TSKINW' ,    RC=STATUS)
   VERIFY_(STATUS)
   if (DO_CICE_THERMO == 0) then
      call MAPL_GetPointer(INTERNAL,TI   , 'TSKINI' , RC=STATUS)
      VERIFY_(STATUS)
   else
      call MAPL_GetPointer(INTERNAL,FR8  , 'FR'  ,    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,TI8  , 'TSKINI'  ,RC=STATUS)
      VERIFY_(STATUS)
   endif
   call MAPL_GetPointer(INTERNAL,QS   , 'QS'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CH   , 'CH'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CM   , 'CM'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CQ   , 'CQ'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,Z0   , 'Z0'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,WW   , 'WW'     ,    RC=STATUS)
   VERIFY_(STATUS)

! Pointers to outputs
!--------------------

   call MAPL_GetPointer(EXPORT,QH    , 'QH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TH    , 'TH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,UH    , 'UH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,VH    , 'VH'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,QST   , 'QST'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TST   , 'TST'     ,    RC=STATUS)
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
   call MAPL_GetPointer(EXPORT,RET   , 'RET'     ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Z0O   , 'Z0'      ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Z0H   , 'Z0H'     ,    RC=STATUS)
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
   call MAPL_GetPointer(EXPORT,GST   , 'GUST'    ,    RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,VNT   , 'VENT'    ,    RC=STATUS)
   VERIFY_(STATUS)

   NT = size(TA)
   if(NT == 0) then
      call MAPL_TimerOff(MAPL,"RUN1" )
      call MAPL_TimerOff(MAPL,"TOTAL")
      RETURN_(ESMF_SUCCESS)
   end if

   allocate(RE (NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CN (NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZT (NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(T2M (NT)  ,  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(Q2M (NT)  ,  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U2M (NT)  ,  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(V2M (NT)  ,  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(T10M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(Q10M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U10M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(V10M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(U50M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(V50M (NT)  , STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZQ (NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(UUU(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(RHO(NT) ,    STAT=STATUS)
   VERIFY_(STATUS)
   allocate(PSMB(NT) ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(PSL(NT) ,    STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VKH(NT) ,    STAT=STATUS)
   VERIFY_(STATUS)
   allocate(fakelai(NT) ,STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VKM(NT) ,    STAT=STATUS)
   VERIFY_(STATUS)
   allocate(USTAR(NT) ,  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(XX(NT)   ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(YY(NT)   ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CU(NT)   ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CT(NT)   ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(RIB(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(ZETA(NT) ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(WS(NT)   ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(IWATER(NT),  STAT=STATUS)
   VERIFY_(STATUS)
   allocate(LAI(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CHB(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CQB(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(CMB(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(UCN(NT)  ,   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(US (NT,2),   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(VS (NT,2),   STAT=STATUS)
   VERIFY_(STATUS)
   allocate(TS (NT,NUM_SUBTILES),   STAT=STATUS)
   VERIFY_(STATUS)
   if (DO_CICE_THERMO == 0) then
      allocate(FR (NT,NUM_SUBTILES),STAT=STATUS)
      VERIFY_(STATUS)
   endif

   TS(:,WATER) = TW               ! TW (in K): returned from MOM/dataocean. Max, should it be TS(:,WATER) = TW - TWMTS? 

   if ( DO_CICE_THERMO == 0) then
      TS(:,ICE  ) = TI
      FR(:,WATER) = 1.0-FI
      FR(:,ICE  ) = FI
   else
      TS(:,ICE:)  = TI8           ! TI8(in K): returned from CICEDyna
   endif

   US(:,WATER) = UW
   US(:,ICE  ) = UI
   VS(:,WATER) = VW
   VS(:,ICE  ) = VI

!  Clear the output tile accumulators
!------------------------------------

                       CHB = 0.0
                       CQB = 0.0
                       CMB = 0.0
   if(associated(CMT)) CMT = 0.0
   if(associated(TST)) TST = 0.0
   if(associated(QST)) QST = 0.0
   if(associated(CNT)) CNT = 0.0
   if(associated(RIT)) RIT = 0.0
   if(associated(RET)) RET = 0.0
   if(associated(TH )) TH  = 0.0
   if(associated(QH )) QH  = 0.0
   if(associated(UH )) UH  = 0.0
   if(associated(VH )) VH  = 0.0
   if(associated(Z0O)) Z0O = 0.0
   if(associated(Z0H)) Z0H = 0.0
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
   if(associated(VNT)) VNT = 0.0

   SUB_TILES: do N=1,NUM_SUBTILES

! Choose sfc layer: if CHOOSEMOSFC is 1 (default), choose helfand MO, 
!                   if CHOOSEMOSFC is 0          , choose louis

      sfc_layer: if(CHOOSEMOSFC.eq.0) then
         call louissurface(1,N,UU,WW,PS,TA,TS,QA,QS,PCU,LAI,Z0,DZ,CM,CN,RIB,ZT,ZQ,CH,CQ,UUU,UCN,RE)

      elseif (CHOOSEMOSFC.eq.1) then

         niter = 6   ! number of internal iterations in the helfand MO surface layer routine
         if(N==WATER)then
            IWATER=1
            Z0(:,N)=OCEANZ0
         else
            IWATER= 5
            if ( DO_CICE_THERMO == 0) then
               Z0(:,N)=OCEANICEZ0
            else
               Z0(:,N)= ICEZ0
            endif
         endif

         PSMB = PS * 0.01            ! convert to MB
         fakelai  = 1.e-4
         ! Approximate pressure at top of surface layer: hydrostatic, eqn of state using avg temp and press
         PSL = PSMB * (1. - (DZ*MAPL_GRAV)/(MAPL_RGAS*(TA+TS(:,N)) ) ) /   &
                      (1. + (DZ*MAPL_GRAV)/(MAPL_RGAS*(TA+TS(:,N)) ) ) 

         call helfsurface( UWINDLMTILE,VWINDLMTILE,TA,TS(:,N),QA,QS(:,N),PSL,PSMB,Z0(:,N),        &
                           fakelai,IWATER,DZ,niter,nt,RHO,VKH,VKM,USTAR,XX,YY,CU,CT,RIB,ZETA,WS,  &
                           t2m,q2m,u2m,v2m,t10m,q10m,u10m,v10m,u50m,v50m,CHOOSEZ0)

         CM(:,N)  = VKM
         CH(:,N)  = VKH
         CQ(:,N)  = VKH

         CN = (MAPL_KARMAN/ALOG(DZ/Z0(:,N) + 1.0)) * (MAPL_KARMAN/ALOG(DZ/Z0(:,N) + 1.0))
         ZT = Z0(:,N)
         ZQ = Z0(:,N)
         RE = 0.
         UUU = UU
         UCN = 0.

         !  Aggregate to tiles for MO only diagnostics
         !--------------------------------------------
         if ( DO_CICE_THERMO == 0) then
            if(associated(MOU50M))MOU50M = MOU50M + U50M(:)*FR(:,N)
            if(associated(MOV50M))MOV50M = MOV50M + V50M(:)*FR(:,N)
            if(associated(MOT10M))MOT10M = MOT10M + T10M(:)*FR(:,N)
            if(associated(MOQ10M))MOQ10M = MOQ10M + Q10M(:)*FR(:,N)
            if(associated(MOU10M))MOU10M = MOU10M + U10M(:)*FR(:,N)
            if(associated(MOV10M))MOV10M = MOV10M + V10M(:)*FR(:,N)
            if(associated(MOT2M ))MOT2M  = MOT2M  + T2M (:)*FR(:,N)
            if(associated(MOQ2M ))MOQ2M  = MOQ2M  + Q2M (:)*FR(:,N)
            if(associated(MOU2M ))MOU2M  = MOU2M  + U2M (:)*FR(:,N)
            if(associated(MOV2M ))MOV2M  = MOV2M  + V2M (:)*FR(:,N)
         else
            if(associated(MOU50M))MOU50M = MOU50M + U50M(:)*FR8(:,N)
            if(associated(MOV50M))MOV50M = MOV50M + V50M(:)*FR8(:,N)
            if(associated(MOT10M))MOT10M = MOT10M + T10M(:)*FR8(:,N)
            if(associated(MOQ10M))MOQ10M = MOQ10M + Q10M(:)*FR8(:,N)
            if(associated(MOU10M))MOU10M = MOU10M + U10M(:)*FR8(:,N)
            if(associated(MOV10M))MOV10M = MOV10M + V10M(:)*FR8(:,N)
            if(associated(MOT2M ))MOT2M  = MOT2M  + T2M (:)*FR8(:,N)
            if(associated(MOQ2M ))MOQ2M  = MOQ2M  + Q2M (:)*FR8(:,N)
            if(associated(MOU2M ))MOU2M  = MOU2M  + U2M (:)*FR8(:,N)
            if(associated(MOV2M ))MOV2M  = MOV2M  + V2M (:)*FR8(:,N)
         endif
      endif sfc_layer

      !  Aggregate to tiles
      !--------------------
      if ( DO_CICE_THERMO == 0) then

                             CHB     = CHB + CH(:,N)*FR(:,N)
                             CQB     = CQB + CQ(:,N)*FR(:,N)
                             CMB     = CMB + CM(:,N)*FR(:,N)
         if(associated(TST)) TST     = TST + TS(:,N)*FR(:,N)
         if(associated(QST)) QST     = QST + QS(:,N)*FR(:,N)
         if(associated(CNT)) CNT     = CNT + CN(:  )*FR(:,N)
         if(associated(RIT)) RIT     = RIT + RIB(: )*FR(:,N)
         if(associated(RET)) RET     = RET + RE(:  )*FR(:,N)
         if(associated(Z0O)) Z0O     = Z0O + Z0(:,N)*FR(:,N)
         if(associated(Z0H)) Z0H     = Z0H + ZT(:  )*FR(:,N)
         if(associated(GST)) GST     = GST + WW(:,N)*FR(:,N)
         if(associated(VNT)) VNT     = VNT + UUU    *FR(:,N)
      else
                             CHB     = CHB + CH(:,N)*FR8(:,N)
                             CQB     = CQB + CQ(:,N)*FR8(:,N)
                             CMB     = CMB + CM(:,N)*FR8(:,N)
         if(associated(TST)) TST     = TST + TS(:,N)*FR8(:,N)
         if(associated(QST)) QST     = QST + QS(:,N)*FR8(:,N)
         if(associated(CNT)) CNT     = CNT + CN(:  )*FR8(:,N)
         if(associated(RIT)) RIT     = RIT + RIB(: )*FR8(:,N)
         if(associated(RET)) RET     = RET + RE(:  )*FR8(:,N)
         if(associated(Z0O)) Z0O     = Z0O + Z0(:,N)*FR8(:,N)
         if(associated(Z0H)) Z0H     = Z0H + ZT(:  )*FR8(:,N)
         if(associated(GST)) GST     = GST + WW(:,N)*FR8(:,N)
         if(associated(VNT)) VNT     = VNT + UUU    *FR8(:,N)
      endif

      !  Aggregate effective, CD-weighted, surface values of T and Q
      !-------------------------------------------------------------

      if ( DO_CICE_THERMO == 0) then
         if(associated(TH)) TH      = TH  + CH(:,N)*TS(:,N)*FR(:,N)
         if(associated(QH)) QH      = QH  + CQ(:,N)*QS(:,N)*FR(:,N)
         if(associated(UH)) UH      = UH  + CM(:,N)*US(:,N)*FR(:,N)
         if(associated(VH)) VH      = VH  + CM(:,N)*VS(:,N)*FR(:,N)
      else
         if(associated(TH)) TH      = TH  + CH(:,N)*TS(:,N)*FR8(:,N)
         if(associated(QH)) QH      = QH  + CQ(:,N)*QS(:,N)*FR8(:,N)
         if(N == WATER) then
            NC = N
         else 
            NC = ICE
         endif
         ! Because US & VS have dim (ntiles, 2)
         if(associated(UH)) UH      = UH  + CM(:,N)*US(:,NC)*FR8(:,N)
         if(associated(VH)) VH      = VH  + CM(:,N)*VS(:,NC)*FR8(:,N)
      endif


      WW(:,N) = max(CH(:,N)*(TS(:,N)-TA-(MAPL_GRAV/MAPL_CP)*DZ)/TA + MAPL_VIREPS*CQ(:,N)*(QS(:,N)-QA),0.0)
      WW(:,N) = (HPBL*MAPL_GRAV*WW(:,N))**(2./3.)

   end do SUB_TILES

   if(associated(TH )) TH  = TH /CHB
   if(associated(QH )) QH  = QH /CQB
   if(associated(UH )) UH  = UH /CMB
   if(associated(VH )) VH  = VH /CMB
   if(associated(CHT)) CHT = CHB
   if(associated(CQT)) CQT = CQB
   if(associated(CMT)) CMT = CMB
   if(associated(GST)) GST = sqrt(max(GST+UCN,0.0))

   deallocate(UUU)
   deallocate(LAI)
   deallocate(CHB)
   deallocate(CQB)
   deallocate(CMB)
   deallocate(RE )
   deallocate(CN )
   deallocate(ZT )
   deallocate(ZQ )
   deallocate(UCN)
   deallocate(TS )
   if ( DO_CICE_THERMO == 0) deallocate(FR )
   deallocate(VS )
   deallocate(US )
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
   deallocate(RHO)
   deallocate(VKH)
   deallocate(fakelai)
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
!-----------

   call MAPL_TimerOff(MAPL,"RUN1" )
   call MAPL_TimerOff(MAPL,"TOTAL")

   RETURN_(ESMF_SUCCESS)


 end subroutine RUN1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: RUN2 -- Second Run stage for the Saltwater component

! !INTERFACE:

subroutine RUN2 ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the sea-surface conditions

!EOP


! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer       :: MAPL => null()
  type (ESMF_State       )            :: INTERNAL
  type (MAPL_SunOrbit)                :: ORBIT
  type (ESMF_Config      )            :: CF

  real, pointer, dimension(:)         :: LATS => null()
  real, pointer, dimension(:)         :: LONS => null()

  real, pointer, dimension(:)         :: AREA => null()     ! needed to calculate TILEAREA in SaltWaterCore

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run2"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Start Total timer
!------------------

   call MAPL_TimerOn(MAPL,"TOTAL")
   call MAPL_TimerOn(MAPL,"RUN2" )

! Get parameters from generic state.
!-----------------------------------

    call MAPL_Get(MAPL,             &
         TILELATS  = LATS ,                      &
         TILELONS  = LONS ,                      &
         TILEAREA  = AREA ,                      &
         ORBIT     = ORBIT,                      &
         INTERNAL_ESMF_STATE = INTERNAL,         &
         CF = CF,                                &
                                       RC=STATUS )
    VERIFY_(STATUS)

! Update the skin variables each step
!------------------------------------

    call SALTWATERCORE(NT=size(LONS), RC=STATUS )
    VERIFY_(STATUS)

!  All done
!-----------

   call MAPL_TimerOff(MAPL,"RUN2" )
   call MAPL_TimerOff(MAPL,"TOTAL")

   RETURN_(ESMF_SUCCESS)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine SALTWATERCORE(NT,RC)
   integer,           intent(IN ) :: NT
   integer, optional, intent(OUT) :: RC
     
!  Locals

   character(len=ESMF_MAXSTR)     :: IAm
   integer                        :: STATUS

! pointers to export

   real, pointer, dimension(:  )  :: EMISS   => null()
   real, pointer, dimension(:  )  :: ALBVF   => null() 
   real, pointer, dimension(:  )  :: ALBVR   => null() 
   real, pointer, dimension(:  )  :: ALBNF   => null() 
   real, pointer, dimension(:  )  :: ALBNR   => null() 
   real, pointer, dimension(:  )  :: EVAPOUT => null()
   real, pointer, dimension(:  )  :: SUBLIM  => null()
   real, pointer, dimension(:  )  :: SNOWOCN => null()
   real, pointer, dimension(:  )  :: RAINOCN => null()
   real, pointer, dimension(:  )  :: SHWTR   => null()
   real, pointer, dimension(:  )  :: SHICE   => null()
   real, pointer, dimension(:  )  :: SHOUT   => null()
   real, pointer, dimension(:  )  :: HLATN   => null()
   real, pointer, dimension(:  )  :: HLATWTR => null()
   real, pointer, dimension(:  )  :: HLATICE => null()
   real, pointer, dimension(:  )  :: HLWUP   => null()
   real, pointer, dimension(:  )  :: SWNDSRF => null()
   real, pointer, dimension(:  )  :: LWNDSRF => null()
   real, pointer, dimension(:  )  :: SWNDWTR => null()
   real, pointer, dimension(:  )  :: LWNDWTR => null()
   real, pointer, dimension(:  )  :: SWNDICE => null()
   real, pointer, dimension(:  )  :: LWNDICE => null()

   real, pointer, dimension(:  )  :: DELTS  => null()
   real, pointer, dimension(:  )  :: DELQS  => null()
   real, pointer, dimension(:  )  :: TST    => null()
   real, pointer, dimension(:  )  :: QST    => null()
   real, pointer, dimension(:  )  :: TAUXW  => null()
   real, pointer, dimension(:  )  :: TAUYW  => null()
   real, pointer, dimension(:  )  :: TAUXI  => null()
   real, pointer, dimension(:  )  :: TAUYI  => null()
   real, pointer, dimension(:  )  :: TAUXO  => null()
   real, pointer, dimension(:  )  :: TAUYO  => null()
   real, pointer, dimension(:  )  :: USTR3  => null()
   real, pointer, dimension(:  )  :: UUEX   => null()
   real, pointer, dimension(:  )  :: PSEX   => null()
   real, pointer, dimension(:  )  :: PENUVR => null()
   real, pointer, dimension(:  )  :: PENUVF => null()
   real, pointer, dimension(:  )  :: PENPAR => null()
   real, pointer, dimension(:  )  :: PENPAF => null()
   real, pointer, dimension(:  )  :: FRI    => null()
   real, pointer, dimension(:  )  :: FRW    => null()

   real, pointer, dimension(:  )  :: Dcool     => null()           ! cool-skin, diurnal warming related exports
   real, pointer, dimension(:  )  :: Dwarm     => null()
   real, pointer, dimension(:  )  :: Tdrop     => null()
   real, pointer, dimension(:  )  :: Tbar      => null()
   real, pointer, dimension(:  )  :: Qcool     => null()
   real, pointer, dimension(:  )  :: SWcool    => null()
   real, pointer, dimension(:  )  :: Qwarm     => null()
   real, pointer, dimension(:  )  :: SWwarm    => null()
   real, pointer, dimension(:  )  :: Phiw      => null()
   real, pointer, dimension(:  )  :: Langm     => null()
   real, pointer, dimension(:  )  :: Ustarw    => null()
   real, pointer, dimension(:  )  :: Lcool     => null()
   real, pointer, dimension(:  )  :: Bcool     => null()
   real, pointer, dimension(:  )  :: Tdel      => null()
   real, pointer, dimension(:  )  :: TAUTW     => null()
   real, pointer, dimension(:  )  :: TS_FOUNDe => null()
   real, pointer, dimension(:  )  :: ZETA_W    => null()
   real, pointer, dimension(:)    :: DISCHARGE => null()

   real, pointer, dimension(:  )  :: FRAZIL     => null()          ! CICE related exports.
   real, pointer, dimension(:  )  :: CONGELO    => null()
   real, pointer, dimension(:  )  :: SNOICEO    => null()
   real, pointer, dimension(:  )  :: FRESH      => null()
   real, pointer, dimension(:  )  :: FSALT      => null()
   real, pointer, dimension(:  )  :: FHOCN      => null()
   real, pointer, dimension(:  )  :: FSWTRUO    => null()
   real, pointer, dimension(:  )  :: FSWABSO    => null()
   real, pointer, dimension(:  )  :: MELTL      => null()
   real, pointer, dimension(:  )  :: MELTTL     => null()
   real, pointer, dimension(:  )  :: MELTBL     => null()
   real, pointer, dimension(:  )  :: MELTSL     => null()
   real, pointer, dimension(:  )  :: HICE       => null()
   real, pointer, dimension(:  )  :: HSNO       => null()
   real, pointer, dimension(:  )  :: HICEUNT    => null()
   real, pointer, dimension(:  )  :: SNOONICE   => null()
   real, pointer, dimension(:  )  :: ISTSFC     => null()
   real, pointer, dimension(:  )  :: TSKINWCICE => null()
   real, pointer, dimension(:  )  :: IAGE       => null()
   real, pointer, dimension(:  )  :: SSKINW2    => null()
   real, pointer, dimension(:  )  :: DAIDTT     => null()
   real, pointer, dimension(:  )  :: DVIDTT     => null()
   real, pointer, dimension(:  )  :: FBOTL      => null()
   real, pointer, dimension(:  )  :: USTARI     => null()
   real, pointer, dimension(:  )  :: HW0        => null()
   real, pointer, dimension(:  )  :: HW1        => null()
   real, pointer, dimension(:  )  :: TW0        => null()
   real, pointer, dimension(:  )  :: TW1        => null()
   real, pointer, dimension(:  )  :: TWINC1     => null()
   real, pointer, dimension(:  )  :: TWINC2     => null()
   real, pointer, dimension(:  )  :: TWINCT     => null()
   real, pointer, dimension(:  )  :: FWFLUX     => null()
   real, pointer, dimension(:  )  :: AHTFLX     => null()
   real, pointer, dimension(:  )  :: AHTPR      => null()
   real, pointer, dimension(:  )  :: AHTSW      => null()
   real, pointer, dimension(:  )  :: AHTLW      => null()
   real, pointer, dimension(:  )  :: AHTLWD     => null()
   real, pointer, dimension(:  )  :: AHTLWU     => null()
   real, pointer, dimension(:  )  :: AHTSH      => null()
   real, pointer, dimension(:  )  :: AHTLH      => null()
   real, pointer, dimension(:  )  :: AHTSWI     => null()
   real, pointer, dimension(:  )  :: AHTSNO     => null()
   real, pointer, dimension(:  )  :: CFQW       => null()
   real, pointer, dimension(:  )  :: EVPW0      => null()
   real, pointer, dimension(:  )  :: EVPW0A     => null()
   real, pointer, dimension(:  )  :: EVPW0B     => null()
   real, pointer, dimension(:  )  :: EVPW1      => null()
   real, pointer, dimension(:  )  :: FCONDTOP   => null()
   real, pointer, dimension(:  )  :: FCONDB     => null()
   real, pointer, dimension(:  )  :: NIERG      => null()
   real, pointer, dimension(:  )  :: SBLXOUT    => null()
   real, pointer, dimension(:  )  :: SIALB      => null()
   real, pointer, dimension(:  )  :: CO2SCEX    => null()

   ! pointers to category dimensional exports (CICE)
   real, pointer, dimension(:,:)  :: FCONDBOTN   => null()
   real, pointer, dimension(:,:)  :: TINZ        => null()

   real, pointer, dimension(:,:)  :: DUDPEX      => null()
   real, pointer, dimension(:,:)  :: DUWTEX      => null()
   real, pointer, dimension(:,:)  :: DUSDEX      => null()
   real, pointer, dimension(:,:)  :: BCDPEX      => null()
   real, pointer, dimension(:,:)  :: BCWTEX      => null()
   real, pointer, dimension(:,:)  :: OCDPEX      => null()
   real, pointer, dimension(:,:)  :: OCWTEX      => null()
   real, pointer, dimension(:,:)  :: FSWBANDEX   => null()
   real, pointer, dimension(:,:)  :: FSWBANDNAEX => null()

   ! pointers to CMIP5 exports (CICE)
   real, pointer, dimension(:  )  :: EVAP_C5        => null()
   real, pointer, dimension(:  )  :: PR_C5          => null()
   real, pointer, dimension(:  )  :: PRSN_C5        => null()
   real, pointer, dimension(:  )  :: GRFRAZIL_C5    => null()
   real, pointer, dimension(:  )  :: GRCONGEL_C5    => null()
   real, pointer, dimension(:  )  :: GRLATERAL_C5   => null()
   real, pointer, dimension(:  )  :: SNOTOICE_C5    => null()
   real, pointer, dimension(:  )  :: SNOMELT_C5     => null()
   real, pointer, dimension(:  )  :: TMELT_C5       => null()
   real, pointer, dimension(:  )  :: BMELT_C5       => null()
   real, pointer, dimension(:  )  :: SFDSI_C5       => null()
   real, pointer, dimension(:  )  :: HFSIFRAZIL_C5  => null()
   real, pointer, dimension(:  )  :: IALB_C5        => null()
   real, pointer, dimension(:  )  :: RSDSSI_C5      => null()
   real, pointer, dimension(:  )  :: RSUSSI_C5      => null()
   real, pointer, dimension(:  )  :: FSITHERM_CMIP5 => null()

! pointers to internal

   real, pointer, dimension(:  )  :: TW    => null()
   real, pointer, dimension(:  )  :: TI    => null()
   real, pointer, dimension(:  )  :: HW    => null()
   real, pointer, dimension(:  )  :: HI    => null()
   real, pointer, dimension(:  )  :: SW    => null()
   real, pointer, dimension(:  )  :: SI    => null()
   real, pointer, dimension(:,:)  :: QS    => null()
   real, pointer, dimension(:,:)  :: CH    => null()
   real, pointer, dimension(:,:)  :: CQ    => null()
   real, pointer, dimension(:,:)  :: CM    => null()
   real, pointer, dimension(:  )  :: TWMTS => null()

   real(kind=MAPL_R8), pointer, dimension(:,:)   :: FR8     => null()     ! CICE related
   real,               pointer, dimension(:,:)   :: TI8     => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)   :: VOLICE  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)   :: VOLSNO  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)   :: VOLPOND => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)   :: APONDN  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:)   :: HPONDN  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:,:) :: ERGICE  => null()
   real(kind=MAPL_R8), pointer, dimension(:,:,:) :: ERGSNO  => null()

   real, pointer, dimension(:,:)   :: TAUAGE => null()
   real, pointer, dimension(:)     :: SLMASK => null()


! pointers to import

   real, pointer, dimension(:)    :: ALW => null()
   real, pointer, dimension(:)    :: BLW => null()
   real, pointer, dimension(:)    :: LWDNSRF => null()
   real, pointer, dimension(:)    :: DRPAR => null()
   real, pointer, dimension(:)    :: DFPAR => null()
   real, pointer, dimension(:)    :: DRNIR => null()
   real, pointer, dimension(:)    :: DFNIR => null()
   real, pointer, dimension(:)    :: DRUVR => null()
   real, pointer, dimension(:)    :: DFUVR => null()
   real, pointer, dimension(:)    :: EVAP  => null()
   real, pointer, dimension(:)    :: SH => null()
   real, pointer, dimension(:)    :: TAUX => null()
   real, pointer, dimension(:)    :: TAUY => null()
   real, pointer, dimension(:)    :: DEV => null()
   real, pointer, dimension(:)    :: DSH => null()
   real, pointer, dimension(:)    :: SNO => null()
   real, pointer, dimension(:)    :: PLS => null()
   real, pointer, dimension(:)    :: PCU => null()
   real, pointer, dimension(:)    :: PS => null()
   real, pointer, dimension(:)    :: UU => null()
   real, pointer, dimension(:)    :: FI => null()
   real, pointer, dimension(:)    :: THATM => null()
   real, pointer, dimension(:)    :: QHATM => null()
   real, pointer, dimension(:)    :: UHATM => null()
   real, pointer, dimension(:)    :: VHATM => null()
   real, pointer, dimension(:)    :: UUA   => null()
   real, pointer, dimension(:)    :: VVA   => null()
   real, pointer, dimension(:)    :: CTATM => null()
   real, pointer, dimension(:)    :: CQATM => null()
   real, pointer, dimension(:)    :: CMATM => null()
   real, pointer, dimension(:)    :: UW => null()
   real, pointer, dimension(:)    :: UI => null()
   real, pointer, dimension(:)    :: VW => null()
   real, pointer, dimension(:)    :: VI => null()
   real, pointer, dimension(:)    :: KPAR => null()
   real, pointer, dimension(:)    :: TS_FOUNDi => null()
   real, pointer, dimension(:)    :: DTSDT => null()
   real, pointer, dimension(:)    :: DISCHARGE_IM => null()

   real, pointer, dimension(:)    :: TAUXBOT   => null()                  ! CICE related
   real, pointer, dimension(:)    :: TAUYBOT   => null()
   real, pointer, dimension(:)    :: TSKINWS   => null()
   real, pointer, dimension(:)    :: CO2SC     => null()
   real, pointer, dimension(:,:)  :: DUDP      => null()
   real, pointer, dimension(:,:)  :: DUWT      => null()
   real, pointer, dimension(:,:)  :: DUSD      => null()
   real, pointer, dimension(:,:)  :: BCDP      => null()
   real, pointer, dimension(:,:)  :: BCWT      => null()
   real, pointer, dimension(:,:)  :: OCDP      => null()
   real, pointer, dimension(:,:)  :: OCWT      => null()
   real, pointer, dimension(:,:)  :: FSWBAND   => null()
   real, pointer, dimension(:,:)  :: FSWBANDNA => null()

   real, allocatable                   :: TS (:,:)                  ! Following 4 Variables: TS to FR need to be 
   real, allocatable                   :: HH (:,:)                  ! allocatable because NUM_SUBTILES is NOT a parameter
   real, allocatable                   :: SS (:,:)
   real, allocatable                   :: FR (:,:)
   real,    dimension(NT)              :: SHF
   real,    dimension(NT)              :: EVP
   real,    dimension(NT)              :: SHD
   real,    dimension(NT)              :: EVD
   real,    dimension(NT)              :: CFQ
   real,    dimension(NT)              :: CFT
   !real,    dimension(NT)              :: UUA
   !real,    dimension(NT)              :: VVA
   real,    dimension(NT)              :: TXW
   real,    dimension(NT)              :: TYW
   real,    dimension(NT)              :: TXI
   real,    dimension(NT)              :: TYI
   real,    dimension(NT)              :: TXO
   real,    dimension(NT)              :: TYO
   real,    dimension(NT)              :: DQS
   real,    dimension(NT)              :: DTS
   real,    dimension(NT)              :: DTX
   real,    dimension(NT)              :: DTY
   real,    dimension(NT)              :: SWN
   real,    dimension(NT)              :: PEN
   real,    dimension(NT)              :: PUR
   real,    dimension(NT)              :: PUF
   real,    dimension(NT)              :: PPR
   real,    dimension(NT)              :: PPF
   real,    dimension(NT)              :: LHF
   real,    dimension(NT)              :: ZTH
   real,    dimension(NT)              :: SLR
   real,    dimension(NT)              :: ALBVRI
   real,    dimension(NT)              :: ALBVFI
   real,    dimension(NT)              :: ALBNRI
   real,    dimension(NT)              :: ALBNFI
   real,    dimension(NT)              :: ALBVRO
   real,    dimension(NT)              :: ALBVFO
   real,    dimension(NT)              :: ALBNRO
   real,    dimension(NT)              :: ALBNFO
   real,    dimension(NT)              :: VSUVR
   real,    dimension(NT)              :: VSUVF


   real,    dimension(NT)              :: LCOOL_
   real,    dimension(NT)              :: BCOOL_
   real,    dimension(NT)              :: USTARW_
   real,    dimension(NT)              :: QCOOL_
   real,    dimension(NT)              :: QWARM_
   real,    dimension(NT)              :: SWCOOL_
   real,    dimension(NT)              :: SWWARM_
   real,    dimension(NT)              :: PHIW_
   real,    dimension(NT)              :: DWARM_
   real,    dimension(NT)              :: TBAR_
   real,    dimension(NT)              :: DCOOL_
   real,    dimension(NT)              :: TDROP_
   real,    dimension(NT)              :: TDEL_
   real,    dimension(NT)              :: LANGM_
   real,    dimension(NT)              :: TAUTW_
   real,    dimension(NT)              :: ZETA_W_
   real,    dimension(NT)              :: uStokes_                  ! Stokes velocity should be an import from Wave Watch
   real,    dimension(NT)              :: FRESHATM                  ! fresh water flux from atmosphere

   integer                             :: N
   real                                :: DT
   real                                :: MAXWATERDEPTH
   real                                :: MINWATERDEPTH
   real                                :: MAXICEDEPTH
   real                                :: MINICEDEPTH
   integer                             :: n_iter_cool               ! number of iterations to compute cool-skin layer
   real                                :: fr_ice_thresh             ! threshold on ice fraction, used in diurnal warming and cool-skin layer
   real                                :: MUSKIN                    ! exponent in T(z) profile in warm layer, based on Zeng & Beljaars, 2005
   real                                :: STOKES_SPEED              ! Stokes velocity in m/s. A global constant number until Wave Watch is ready
   integer                             :: DO_SKIN_LAYER
   integer                             :: USE_KPAR

   real                                :: MAXSALINITY
   real                                :: MINSALINITY

! following are related  to CICE

   integer                             :: DIAG_ICE_BUDGET           ! default (=0) is to not compute certain flux budgets over Sea Ice 
   integer                             :: NSUB, I, K, L
   integer                             :: DO_POND

   real                                :: LATSO, LONSO
   real,    dimension(1)               :: LATSD, LONSD
   real                                :: TOTALAREA, ALLTOTALAREA
   logical, dimension(1)               :: OBSERVE

   real,    dimension(1)               :: FRZ_ONSET, MLT_ONSET
   real,    dimension(1)               :: RDUM 
   real                                :: FRZMLT_MAX
   real(kind=MAPL_R8)                  :: DTDB
   real(kind=MAPL_R8), dimension(1)    :: FRZMLTDB, TSCDB, TFDB, TAUXBOTDB, TAUYBOTDB, &
                                          TBOTDB, FBOTDB, RSIDEDB

   real,    dimension(1)               :: FSWABS
   real                                :: YDAY 

   integer,            allocatable    :: TRCRTYPE      (:)
   real,               allocatable    :: TRACERS       (:,:)
   real,               allocatable    :: TF            (:)
   real,               allocatable    :: FRZMLT        (:)
   real,               allocatable    :: MELTLN        (:)
   real,               allocatable    :: FRAZLN        (:)
   real,               allocatable    :: FRESHN        (:)
   real,               allocatable    :: FRESHL        (:)
   real,               allocatable    :: FSALTN        (:)
   real,               allocatable    :: FSALTL        (:)
   real,               allocatable    :: FHOCNN        (:)
   real,               allocatable    :: FHOCNL        (:)
   real,               allocatable    :: RSIDE         (:)
   real,               allocatable    :: FSWTHRU       (:,:)        ! FSWTHRU is also an EXPORT 
   real,               allocatable    :: FCOND         (:,:)
   real,               allocatable    :: FCONDBOT      (:,:)
   real,               allocatable    :: FSWTHRUWTR    (:,:)
   real,               allocatable    :: TBOT          (:)
   real,               allocatable    :: FBOT          (:)
   real,               allocatable    :: ALBVRN        (:,:)
   real,               allocatable    :: ALBNRN        (:,:)
   real,               allocatable    :: ALBVFN        (:,:)
   real,               allocatable    :: ALBNFN        (:,:)
   real,               allocatable    :: FSWSFC        (:,:)
   real,               allocatable    :: FSWINT        (:,:)
   real,               allocatable    :: ISWABS        (:,:,:)
   real,               allocatable    :: SSWABS        (:,:,:)
   real,               allocatable    :: MELTT         (:)
   real,               allocatable    :: MELTS         (:)
   real,               allocatable    :: MELTB         (:)
   real,               allocatable    :: CONGEL        (:)
   real,               allocatable    :: SNOICE        (:)
   real,               allocatable    :: DTSACCUM      (:)
   real,               allocatable    :: HW_TMP        (:)
   real,               allocatable    :: TS_TMP        (:)

   real,               allocatable    :: TS_OLD        (:,:)
   real,               allocatable    :: HW_OLD        (:)          ! following 3 arrays are used for doing DIAG_ICE_BUDGET
   real,               allocatable    :: TW_OLD        (:)
   real,               allocatable    :: DENTH         (:)

   real,               allocatable    :: ALBIN         (:,:)
   real,               allocatable    :: ALBSN         (:,:)
   real,               allocatable    :: ALBPND        (:,:)

   real,               allocatable    :: DRUVRTHRU     (:,:)
   real,               allocatable    :: DFUVRTHRU     (:,:)
   real,               allocatable    :: DRPARTHRU     (:,:)
   real,               allocatable    :: DFPARTHRU     (:,:)

   real,               allocatable    :: TILEAREA      (:)
   real,               allocatable    :: FRESHL_OLD    (:)
   real,               allocatable    :: TOTALFLUX     (:)
   real,               allocatable    :: NEWICEERG     (:) ! newly generated ice energy <=0 (W m-2)  
   real,               allocatable    :: SBLX          (:) ! 

   real,               allocatable    :: tmp2  (:,:)

!  Following arrays have to be R8 for CICE 
   real(kind=MAPL_R8), allocatable     :: AICENINIT    (:,:)
   real(kind=MAPL_R8), allocatable     :: VICENINIT    (:,:)
   real(kind=MAPL_R8), allocatable     :: FRCICE       (:)
   real(kind=MAPL_R8), allocatable     :: FR_OLD       (:)
   real(kind=MAPL_R8), allocatable     :: VOLSNO_OLD   (:)
   real(kind=MAPL_R8), allocatable     :: VOLICE_OLD   (:)
   real(kind=MAPL_R8), allocatable     :: VOLICE_DELTA (:,:)
   real(kind=MAPL_R8), allocatable     :: FR8TMP       (:,:)  

!  -------------------------------------------------------------------

   type (ESMF_TimeInterval)            :: DELT
   real                                :: DT_SOLAR
   type (ESMF_TimeInterval)            :: TINT
   type (ESMF_Time)                    :: CURRENT_TIME
   type (ESMF_Time)                    :: NOW
   type (ESMF_Time)                    :: BEFORE
   type (ESMF_Time)                    :: MODELSTART
   type (ESMF_Alarm)                   :: SOLALARM
   logical                             :: solalarmison
   type(ESMF_VM)                       :: VM
   type(ESMF_VM)                       :: VMG               ! for CICE
   integer                             :: MYPE              ! for CICE
   logical                             :: debugzth

   real, parameter :: EMSH2O          = 0.99070
   real            :: EMSICE

   real, parameter :: SALTWATERCAP    = MAPL_CAPWTR
   real, parameter :: SALTWATERICECAP = MAPL_CAPICE

!  Begin...
!----------

   IAm =  trim(COMP_NAME) // "SALTWATERCORE"

   call MAPL_TimerOn(MAPL,   "-BeforeSaltWaterCore" )

! Pointers to inputs
!-------------------

   call MAPL_GetPointer(IMPORT,ALW    , 'ALW'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,BLW    , 'BLW'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,LWDNSRF, 'LWDNSRF',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DRPAR  , 'DRPAR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DFPAR  , 'DFPAR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DRNIR  , 'DRNIR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DFNIR  , 'DFNIR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DRUVR  , 'DRUVR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DFUVR  , 'DFUVR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,EVAP   , 'EVAP'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,SH     , 'SH'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,TAUX   , 'TAUX'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,TAUY   , 'TAUY'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DEV    , 'DEVAP'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DSH    , 'DSH'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,SNO    , 'SNO'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PLS    , 'PLS'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PCU    , 'PCU'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,PS     , 'PS'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UU     , 'UU'     ,    RC=STATUS); VERIFY_(STATUS)

   if ( DO_CICE_THERMO == 0) then
      call MAPL_GetPointer(IMPORT,FI     , 'FRACICE',    RC=STATUS); VERIFY_(STATUS)
   endif

   call MAPL_GetPointer(IMPORT,UW     , 'UW'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VW     , 'VW'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UI     , 'UI'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VI     , 'VI'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,THATM  , 'THATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,QHATM  , 'QHATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UHATM  , 'UHATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VHATM  , 'VHATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,UUA    , 'UUA'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,VVA    , 'VVA'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,CTATM  , 'CTATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,CQATM  , 'CQATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,CMATM  , 'CMATM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,KPAR   , 'KPAR'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,TS_FOUNDi,'TS_FOUND',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT,DTSDT  , 'DTSDT'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, DISCHARGE_IM, 'DISCHARGE', RC=STATUS); VERIFY_(STATUS)

   if (DO_CICE_THERMO /= 0) then    ! following additional internals for CICE
      call MAPL_GetPointer(IMPORT,TAUXBOT, 'TAUXBOT',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,TAUYBOT, 'TAUYBOT',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,CO2SC  , 'CO2SC'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,DUDP   , 'DUDP'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,DUWT   , 'DUWT'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,DUSD   , 'DUSD'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,BCDP   , 'BCDP'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,BCWT   , 'BCWT'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,OCDP   , 'OCDP'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,OCWT   , 'OCWT'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,FSWBAND ,'FSWBAND' ,   RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT,FSWBANDNA,'FSWBANDNA', RC=STATUS); VERIFY_(STATUS)
   endif

! Pointers to internals
!----------------------

   call MAPL_GetPointer(INTERNAL,TW     ,'TSKINW',    RC=STATUS); VERIFY_(STATUS)
   if ( DO_CICE_THERMO == 0) then
      call MAPL_GetPointer(INTERNAL,TI  ,'TSKINI',    RC=STATUS); VERIFY_(STATUS)
   else
      call MAPL_GetPointer(INTERNAL,TI8 ,'TSKINI' ,   RC=STATUS); VERIFY_(STATUS)
   endif
   call MAPL_GetPointer(INTERNAL,HW     ,'HSKINW',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,HI     ,'HSKINI',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,SW     ,'SSKINW',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,SI     ,'SSKINI',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,QS     , 'QS'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CH     , 'CH'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CQ     , 'CQ'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,CM     , 'CM'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,TWMTS  , 'TWMTS',    RC=STATUS); VERIFY_(STATUS)

   if (DO_CICE_THERMO /= 0) then    ! following additional internal with LANL CICE
      call MAPL_GetPointer(INTERNAL,FR8 ,   'FR'   ,     RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,VOLICE ,'VOLICE',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,VOLSNO ,'VOLSNO',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,VOLPOND,'VOLPOND',   RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,APONDN, 'APONDN',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,HPONDN, 'HPONDN',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,ERGICE ,'ERGICE',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,ERGSNO ,'ERGSNO',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,TAUAGE ,'TAUAGE',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL,SLMASK ,'SLMASK',    RC=STATUS); VERIFY_(STATUS)
   endif

! Pointers to outputs
!--------------------

   call MAPL_GetPointer(EXPORT,EMISS  , 'EMIS' , alloc=.true., RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ALBVF  , 'ALBVF', alloc=.true., RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ALBVR  , 'ALBVR', alloc=.true., RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ALBNF  , 'ALBNF', alloc=.true., RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ALBNR  , 'ALBNR', alloc=.true., RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,QST    , 'QST'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TST    , 'TST'     ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,DELTS  , 'DELTS'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,DELQS  , 'DELQS'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUXW  , 'TAUXW'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUYW  , 'TAUYW'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUXI  , 'TAUXI'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUYI  , 'TAUYI'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUXO  , 'TAUXO'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TAUYO  , 'TAUYO'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,USTR3  , 'OUSTAR3' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,UUEX   , 'UU'      ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,PSEX   , 'PS'      ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,PENUVR , 'PENUVR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,PENUVF , 'PENUVF'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,PENPAR , 'PENPAR'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,PENPAF , 'PENPAF'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,EVAPOUT, 'EVAPOUT' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SUBLIM,  'SUBLIM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SNOWOCN, 'SNOWOCN' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,RAINOCN, 'RAINOCN' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SHOUT  , 'SHOUT'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SHWTR  , 'SHWTR'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SHICE  , 'SHICE'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,HLATN  , 'HLATN'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,HLATWTR, 'HLATWTR' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,HLATICE, 'HLATICE' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,HLWUP  , 'HLWUP'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,LWNDSRF, 'LWNDSRF' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SWNDSRF, 'SWNDSRF' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,LWNDWTR, 'LWNDWTR' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SWNDWTR, 'SWNDWTR' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,LWNDICE, 'LWNDICE' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SWNDICE, 'SWNDICE' ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,FRI    , 'FRACI'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,FRW    , 'FRACW'   ,    RC=STATUS); VERIFY_(STATUS)


   call MAPL_GetPointer(EXPORT,Dwarm  , 'DWARM'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Dcool  , 'DCOOL'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Tbar   , 'TBAR'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Tdrop  , 'TDROP'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Qcool  , 'QCOOL'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,USTARW , 'USTARW'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Lcool  , 'LCOOL'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SWcool , 'SWCOOL'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,SWwarm , 'SWWARM'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Qwarm  , 'QWARM'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Phiw   , 'PHIW'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Langm  , 'LANGM'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Bcool  , 'BCOOL'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,Tdel   , 'TDEL'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TS_FOUNDe, 'TS_FOUND' , RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,TauTW  , 'TAUTW'   ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,ZETA_W , 'ZETA_W'  ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT, DISCHARGE, 'DISCHARGE', RC=STATUS); VERIFY_(STATUS)
   if(associated(DISCHARGE)) DISCHARGE = DISCHARGE_IM

   if (DO_CICE_THERMO /= 0) then    ! with CICE, saltwater needs following additional internals
      call MAPL_GetPointer(EXPORT,FRAZIL , 'FRAZIL'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,CONGELO, 'CONGEL'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SNOICEO, 'SNOICE'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FRESH  , 'FRESH'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSALT  , 'FSALT'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FHOCN  , 'FHOCN'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSWTRUO, 'FSWTHRU' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSWABSO, 'FSWABS'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,MELTL  , 'MELTL'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,MELTTL , 'MELTT'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,MELTBL , 'MELTB'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,MELTSL , 'MELTS'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HICE   , 'HICE'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HSNO   , 'HSNO'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HICEUNT, 'HICEUNT' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SNOONICE,'SNOONICE',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,ISTSFC , 'ISTSFC'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TSKINWCICE,'TSKINWCICE',RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,IAGE   , 'IAGE'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SSKINW2, 'SSKINW2' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,DAIDTT , 'DAIDTT'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,DVIDTT , 'DVIDTT'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FBOTL  , 'FBOT'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,USTARI , 'USTARI'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HW0    , 'HSKINW0' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HW1    , 'HSKINW1' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TW0    , 'TSKINW0' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TW1    , 'TSKINW1' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TWINC1 , 'TSKINWinc1' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TWINC2 , 'TSKINWinc2' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TWINCT , 'TSKINWinctotal' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FWFLUX , 'FWFLUX'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTFLX , 'AHTFLX'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTPR  , 'AHTPR'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTSW  , 'AHTSW'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTLW  , 'AHTLW'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTLWD , 'AHTLWD'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTLWU , 'AHTLWU'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTSH  , 'AHTSH'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTLH  , 'AHTLH'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTSWI , 'AHTSWI'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,AHTSNO , 'AHTSNO'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,CFQW   , 'CFQW'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,EVPW0  , 'EVPW0'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,EVPW0A , 'EVPW0A'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,EVPW0B , 'EVPW0B'  ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,EVPW1  , 'EVPW1'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FCONDTOP,'FCONDTOP',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FCONDB,  'FCONDBOT',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,NIERG,  'NEWICEERG',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SBLXOUT,'SUBLIMFLX',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SIALB,   'SIALB'   ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,CO2SCEX, 'CO2SC'   ,    RC=STATUS); VERIFY_(STATUS)

      ! category dimensional exports
      call MAPL_GetPointer(EXPORT,FCONDBOTN,  'FCONDBOTN' ,  RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TINZ   ,    'TINZ'      ,  RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,DUDPEX ,    'DUDP'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,DUWTEX ,    'DUWT'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,DUSDEX ,    'DUSD'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,BCDPEX ,    'BCDP'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,BCWTEX ,    'BCWT'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,OCDPEX ,    'OCDP'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,OCWTEX ,    'OCWT'    ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSWBANDEX,  'FSWBAND',     RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSWBANDNAEX,'FSWBANDNA',   RC=STATUS); VERIFY_(STATUS)

      ! CMIP5 exports
      call MAPL_GetPointer(EXPORT,EVAP_C5,        'evap_CMIP5' ,     RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,PR_C5,          'pr_CMIP5'   ,     RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,PRSN_C5,        'prsn_CMIP5' ,     RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,GRFRAZIL_C5,    'grFrazil_CMIP5' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,GRCONGEL_C5,    'grCongel_CMIP5' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,GRLATERAL_C5,   'grLateral_CMIP5', RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SNOTOICE_C5,    'snoToIce_CMIP5' , RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SNOMELT_C5,     'snomelt_CMIP5' ,  RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,TMELT_C5,       'tmelt_CMIP5' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,BMELT_C5,       'bmelt_CMIP5' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,SFDSI_C5,       'sfdsi_CMIP5' ,    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,HFSIFRAZIL_C5,  'hfsifrazil_CMIP5',RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,IALB_C5,        'ialb_CMIP5',      RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,RSDSSI_C5,      'rsdssi_CMIP5',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,RSUSSI_C5,      'rsussi_CMIP5',    RC=STATUS); VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT,FSITHERM_CMIP5,'fsitherm_CMIP5',   RC=STATUS); VERIFY_(STATUS)

   endif

   allocate(TS (NT,NUM_SUBTILES),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(HH (NT,NUM_SUBTILES),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(SS (NT,NUM_SUBTILES),STAT=STATUS)
   VERIFY_(STATUS)
   allocate(FR (NT,NUM_SUBTILES),STAT=STATUS)
   VERIFY_(STATUS)

! Get the time step
! -----------------

    call MAPL_Get(MAPL, HEARTBEAT = DT, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DT, Label="DT:", DEFAULT=DT, RC=STATUS)
    VERIFY_(STATUS)

! Get parameters
! --------------

    call MAPL_GetResource ( MAPL, MAXICEDEPTH  , Label="MAX_SEAICE_DEPTH:", DEFAULT=2.0  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, MINICEDEPTH  , Label="MIN_SEAICE_DEPTH:", DEFAULT=1.E-6, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DO_SKIN_LAYER, Label="USE_SKIN_LAYER:"  , DEFAULT=0    , RC=STATUS)
    VERIFY_(STATUS)

    if (DO_SKIN_LAYER==0) then
       call MAPL_GetResource ( MAPL, MAXWATERDEPTH, Label="MAX_WATER_DEPTH:" , DEFAULT=1000., RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, MINWATERDEPTH, Label="MIN_WATER_DEPTH:" , DEFAULT=1000., RC=STATUS)
       VERIFY_(STATUS)
    else 
       call MAPL_GetResource ( MAPL, MAXWATERDEPTH, Label="MAX_WATER_DEPTH:" , DEFAULT=2.,   RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL, MINWATERDEPTH, Label="MIN_WATER_DEPTH:" , DEFAULT=2.,   RC=STATUS)
       VERIFY_(STATUS)
    end if

    call MAPL_GetResource ( MAPL, MUSKIN,        Label="MU_SKIN:"         , DEFAULT=0.2  ,   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, STOKES_SPEED,  Label="STOKES_VELOCITY:" , DEFAULT=1.E-2,   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, USE_KPAR,      Label="USE_KPAR:",         DEFAULT=1,       RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, EMSICE,        Label="CICE_EMSICE:",      DEFAULT=0.99999, RC=STATUS)
    VERIFY_(STATUS)

    MAXWATERDEPTH   = MAXWATERDEPTH*MAPL_RHOWTR      ! should instead use MAPL_RHO_SEAWATER. But that won't give zero-diff. so change "later" [SA]
    MINWATERDEPTH   = MINWATERDEPTH*MAPL_RHOWTR
    MAXICEDEPTH     = MAXICEDEPTH  *MAPL_RHOWTR
    MINICEDEPTH     = MINICEDEPTH  *MAPL_RHOWTR

    if(DO_SKIN_LAYER==0) TWMTS = 0.

    call MAPL_GetResource ( MAPL, MAXSALINITY, Label="MAX_SALINITY:" , DEFAULT=40.0 , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, MINSALINITY, Label="MIN_SALINITY:" , DEFAULT=5.0 , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DO_POND,     Label="CICE_DO_POND:" , DEFAULT=0,      RC=STATUS)
    VERIFY_(STATUS)

! Copy friendly internals into tile-tile local variables
!-------------------------------------------------------

    HH(:,WATER) = HW
    SS(:,WATER) = SW*HW
    TS(:,WATER) = TW - TWMTS
    if ( DO_CICE_THERMO == 0) then
       HH(:,ICE  ) = HI
       SS(:,ICE  ) = SI*HI
       TS(:,ICE  ) = TI
       FR(:,WATER) = 1.0-FI
       FR(:,ICE  ) = FI
    endif

! Initialize PAR and UVR beam fluxes
!-----------------------------------

    VSUVR = DRPAR + DRUVR
    VSUVF = DFPAR + DFUVR
    PUR   = 0.0
    PUF   = 0.0
    PPR   = 0.0
    PPF   = 0.0

    PREP_CICE_LOCAL_VARS:  if ( DO_CICE_THERMO /= 0) then
      if(associated(TW0))   TW0  = TS(:,WATER) 
      if(associated(HW0))   HW0  = HH(:,WATER) 
      TS(:,ICE: )  = TI8
      HH(:,ICE: )  = 0.0
      SS(:,ICE: )  = 0.0
!     SA: Max, Some thing needs to be done about SS(:,ICE) and SI, i.e., ice salinity, w/CICE Thermodynamics

!     allocate arrays for CICE Thermodynamics
      allocate(TRCRTYPE  (NUM_3D_ICE_TRACERS),                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TRACERS   (NUM_3D_ICE_TRACERS,NUM_ICE_CATEGORIES),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TF        (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRZMLT    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(MELTLN    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRAZLN    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRESHN    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRESHL    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSALTN    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSALTL    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FHOCNN    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FHOCNL    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(RSIDE     (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSWTHRU   (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FCOND     (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FCONDBOT  (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSWTHRUWTR(NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TBOT      (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FBOT      (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBVRN    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBNRN    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBVFN    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBNFN    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSWSFC    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSWINT    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ISWABS    (NT,NUM_ICE_LAYERS,NUM_ICE_CATEGORIES), STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SSWABS    (NT,NUM_SNOW_LAYERS,NUM_ICE_CATEGORIES),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(MELTT     (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(MELTS     (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(MELTB     (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(CONGEL    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SNOICE    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DTSACCUM  (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(HW_TMP    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TS_TMP    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TS_OLD    (NT,NUM_SUBTILES),                      STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBIN     (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBSN     (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ALBPND    (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DRUVRTHRU (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DFUVRTHRU (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DRPARTHRU (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DFPARTHRU (NT,NUM_ICE_CATEGORIES),                STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRESHL_OLD(NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TOTALFLUX (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(NEWICEERG (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SBLX      (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(AICENINIT (NT, NUM_ICE_CATEGORIES),               STAT=STATUS)
      VERIFY_(STATUS)
      allocate(VICENINIT (NT, NUM_ICE_CATEGORIES),               STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FR8TMP    (NT, NUM_SUBTILES),                     STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FRCICE    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FR_OLD    (NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(VOLSNO_OLD(NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(VOLICE_OLD(NT),                                   STAT=STATUS)
      VERIFY_(STATUS)
      allocate(VOLICE_DELTA(NT,NUM_ICE_CATEGORIES),                 STAT=STATUS)
      VERIFY_(STATUS)

      call MAPL_GetResource ( MAPL, LATSO, Label="LATSO:", DEFAULT=70.0, RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetResource ( MAPL, LONSO, Label="LONSO:", DEFAULT=70.0, RC=STATUS)
      VERIFY_(STATUS)

!     initialize arrays for CICE Thermodynamics
      call CICE_PREP_THERMO(TF,TRCRTYPE,TRACERS,FRZMLT,MELTLN,FRAZLN,FRESHN,FRESHL,FSALTN,FSALTL,FHOCNN,FHOCNL,RSIDE,  &
                            FSWTHRU,FCOND,FCONDBOT,FSWTHRUWTR,TBOT,FBOT,ALBIN,ALBSN,ALBPND,ALBVRN,ALBNRN,ALBVFN,ALBNFN,FSWSFC,FSWINT,     &
                            ISWABS,SSWABS,FSWABS,MELTT,MELTS,MELTB,CONGEL,SNOICE,UW,VW,SLMASK,LATS,LONS,LATSO,LONSO,   &
                            FR8,FRCICE,SW,TAUAGE,ICE,WATER,NT,VOLPOND,DT,VOLICE,VOLSNO,ERGICE,ERGSNO,TS,VOLICE_DELTA, NEWICEERG, SBLX)

      FR_OLD     = FRCICE   ! FRCICE is initialized by above subroutine CICE_PREP_THERMO
      TS_OLD     = TS
      VOLICE_OLD = sum(VOLICE,dim=2)
      VOLSNO_OLD = sum(VOLSNO,dim=2)

      AICENINIT  = FR8(:,ICE:)
      VICENINIT  = VOLICE

      call MAPL_GetResource ( MAPL, FRZMLT_MAX, Label="CICE_FRZMLT_MAX:" , DEFAULT=1000., RC=STATUS)
      VERIFY_(STATUS)
      DTDB = REAL(DT, kind=MAPL_R8)     ! Convert DT precision: Real4 to Real8 for usage in CICE
      do k=1, NT 
          call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)
          FRZMLT(K) = (TF(K)-(TS(K,WATER)-TFfresh))*SALTWATERCAP*HH(K,WATER)/DT
          FRZMLT(K) = min(max(FRZMLT(K),-FRZMLT_MAX),FRZMLT_MAX)
          !*** reset TW to freezing point  
          if(TS(K,WATER) < (TF(K)+TFfresh)) then
             TS(K,WATER) = TF(K)+Tffresh
          endif
          if(FRZMLT(K)<0.0) then ! heat the already existing ice from below
             FRZMLTDB  = REAL(FRZMLT(K),            kind=MAPL_R8)
             TSCDB     = REAL(TS(K,WATER)-TFfresh,  kind=MAPL_R8)
             TFDB      = REAL(TF(K),                kind=MAPL_R8)
             TAUXBOTDB = REAL(TAUXBOT(K),           kind=MAPL_R8)
             TAUYBOTDB = REAL(TAUYBOT(K),           kind=MAPL_R8)
             call frzmlt_bottom_lateral (1,1,1,1,1,1,DTDB, &
                  OBSERVE,                                 &
                  FRCICE(K),        FRZMLTDB,       & ! in
                  ERGICE(K,:,:),    ERGSNO(K,:,:),  & ! in
                  TSCDB,            TFDB,           & ! in
                  TAUXBOTDB,        TAUYBOTDB,      & ! in
                  TBOTDB, FBOTDB,   RSIDEDB      )    ! out

                  TBOT(K)  =  TBOTDB(1)
                  FBOT(K)  =  FBOTDB(1)
                  RSIDE(K) =  RSIDEDB(1)
          else
                  TBOT(K)  =  TF(K)
                  FBOT(K)  =  0.0
                  RSIDE(K) =  0.0
          endif
     enddo !k
 
!     Output additional CICE diagnostics?
      call MAPL_GetResource ( MAPL, DIAG_ICE_BUDGET, Label="DIAG_ICE_BUDGET:" , DEFAULT=0    , RC=STATUS)
      VERIFY_(STATUS)

      if (DIAG_ICE_BUDGET /= 0) then
        call ESMF_GridCompGet( GC, VM=VMG, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_VMGet      (VMG, localpet=MYPE,  RC=STATUS)
        VERIFY_(STATUS)

        allocate(HW_OLD   (NT), STAT=STATUS)
        VERIFY_(STATUS)
        allocate(TW_OLD   (NT), STAT=STATUS)
        VERIFY_(STATUS)
        allocate(DENTH    (NT), STAT=STATUS)
        VERIFY_(STATUS)
        allocate(TILEAREA (NT), STAT=STATUS)
        VERIFY_(STATUS)

        HW_OLD = HH(:,WATER)
        TW_OLD = TS(:,WATER)
      endif

      if(associated(RSDSSI_C5))   RSDSSI_C5  = FRCICE * (VSUVR + VSUVF + DRNIR + DFNIR)
      if(associated(TSKINWCICE )) TSKINWCICE = TS(:,WATER)       ! export TSKINWCICE contains skin temp. from MOM
    endif PREP_CICE_LOCAL_VARS

! Add analysis increment. This is zero if ANA_TS is false.
!---------------------------------------------------------
    TS(:,WATER) = TS(:,WATER) + DT*DTSDT

    debugzth = .false.

    call ESMF_VMGetCurrent ( VM, RC=STATUS )

        ! --------------------------------------------------------------------------
        ! Get the current time. 
        ! --------------------------------------------------------------------------

    call ESMF_ClockGet( CLOCK, currTime=CURRENT_TIME, startTime=MODELSTART, TIMESTEP=DELT,  RC=STATUS )
      VERIFY_(STATUS)
    if (MAPL_AM_I_Root(VM).and.debugzth) then
      print *,' start time of clock '
      CALL ESMF_TimePrint ( MODELSTART, OPTIONS="string", RC=STATUS )
    endif

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

    call ALBSEA    (ALBVRO,ALBVFO,ALBNRO,ALBNFO,ZTH)

! Albedo over Sea-Ice. With LANL CICE, it is based on current ice states, 
! also compute shortwave radiation passing thru bottom of ice and skin layer bottom (DxxxTHRU; xx=RUVR, FUVR, ...)
!------------------------------------------------------------------------------------------------------------------

    if ( DO_CICE_THERMO == 0) then
       call ALBSEAICE (ALBVRI,ALBVFI,ALBNRI,ALBNFI,ZTH,LATS,CURRENT_TIME)  ! GEOS albedo over sea ice
    else
      call CICE_ALBSEAICE (ICE,NUM_ICE_CATEGORIES,NUM_ICE_LAYERS,NUM_SNOW_LAYERS,NT,DO_POND,LATSO,LONSO,LATS,LONS,ZTH,FR8,TS,&
                           DRPAR,DFPAR,DRNIR,DFNIR,DRUVR,DFUVR,VSUVR,VSUVF,VOLICE,VOLSNO,APONDN,HPONDN,                      &
                           ISWABS,FSWSFC, FSWINT,FSWTHRU,SSWABS,ALBIN,ALBSN,ALBPND,ALBVRN,ALBVFN,ALBNRN,ALBNFN,              &
                           DRUVRTHRU,DFUVRTHRU,DRPARTHRU,DFPARTHRU)

      ! These computations are done only for the sake of FSWTHRUWTR, through open water
      if(associated(PENUVR)) PENUVR = 0.0
      if(associated(PENUVF)) PENUVF = 0.0
      if(associated(PENPAR)) PENPAR = 0.0
      if(associated(PENPAF)) PENPAF = 0.0
      do N=1, NUM_ICE_CATEGORIES
         do k=1, NT
             PEN(K) = exp(-(KUVR/MAPL_RHO_SEAWATER)*HH(K,WATER))
             PUR(K) = DRUVRTHRU(K,N)*PEN(K)
             PUF(K) = DFUVRTHRU(K,N)*PEN(K)

             PEN(K) = exp(-(KPAR(K)/MAPL_RHO_SEAWATER)*HH(K,WATER))
             PPR(K) = DRPARTHRU(K,N)*PEN(K)
             PPF(K) = DFPARTHRU(K,N)*PEN(K)
             PEN(K) = PUR(K) + PUF(K) + PPR(K) + PPF(K)

             FSWTHRUWTR(K,N) = PEN(K)

             if(associated(PENUVR)) PENUVR(K)  = PENUVR(K) + FR8(K,N+1) * PUR(K)
             if(associated(PENUVF)) PENUVF(K)  = PENUVF(K) + FR8(K,N+1) * PUF(K)
             if(associated(PENPAR)) PENPAR(K)  = PENPAR(K) + FR8(K,N+1) * PPR(K)
             if(associated(PENPAF)) PENPAF(K)  = PENPAF(K) + FR8(K,N+1) * PPF(K)
         enddo
      enddo
    endif
 
    call MAPL_TimerOff(MAPL,    "-BeforeSaltWaterCore" )

! Cycle through sub-tiles doing water and energy budget
!------------------------------------------------------

    if ( DO_CICE_THERMO == 0) then
       if(associated(FRI)) FRI = FI
    endif
    if(associated(EVAPOUT)) EVAPOUT = 0.0
    if(associated(SHOUT  )) SHOUT   = 0.0
    if(associated(HLATN  )) HLATN   = 0.0
    if(associated(DELTS  )) DELTS   = 0.0
    if(associated(DELQS  )) DELQS   = 0.0

    if ( DO_CICE_THERMO /= 0) then    ! These exports are aggregate over NUM_SUBTILES, That's why we need to Init. to 0.
      if(associated(SUBLIM )) SUBLIM  = 0.0
      if(associated(HLATICE)) HLATICE = 0.0
      if(associated(SHICE  )) SHICE   = 0.0
      if(associated(MELTTL )) MELTTL  = 0.0
      if(associated(MELTBL )) MELTBL  = 0.0
      if(associated(MELTSL )) MELTSL  = 0.0
      if(associated(CONGELO)) CONGELO = 0.0
      if(associated(SNOICEO)) SNOICEO = 0.0
      if(associated(FBOTL  )) FBOTL   = 0.0
      if(associated(SBLXOUT)) SBLXOUT = 0.0

      if(associated(EVAP_C5))        EVAP_C5         =  0.0
      if(associated(GRCONGEL_C5))    GRCONGEL_C5     =  0.0
      if(associated(GRLATERAL_C5))   GRLATERAL_C5    =  0.0
      if(associated(SNOMELT_C5))     SNOMELT_C5      =  0.0
      if(associated(TMELT_C5))       TMELT_C5        =  0.0
      if(associated(BMELT_C5))       BMELT_C5        =  0.0
      if(associated(IALB_C5))        IALB_C5         =  0.0
      if(associated(RSUSSI_C5))      RSUSSI_C5       =  0.0
      if(associated(FSITHERM_CMIP5)) FSITHERM_CMIP5  =  0.0

      if(associated(HICEUNT )) HICEUNT    = sum(VOLICE, dim=2)
      if(associated(USTARI))   USTARI     = sqrt(sqrt(TAUXBOT**2+TAUYBOT**2)/MAPL_RHO_SEAWATER)
    end if

! Without LANL CICE, i.e., with GEOS CICE, open water and sea-ice tiles were handled together. 
! But to keep the functionality of both (GEOS and LANL) sea-ice, accessible, they are now split.
! Generally speaking, following accounts for change in fluxes and state variables due to 
! fluxes at the top of interface (with LANL CICE, they are in "Thermo1", below). Changes due to 
! bottom of interface layer fluxes is handled later, in an implicit fashion 
! (open-water: SKIN_SST; sea-ice: with LANL CICE: "Thermo2"). 
! Note: 1. with GEOS CICE, there is no account of bottom flux (Max, please verify)
!       2. The sequence of computations: first over ice tiles and then over water is important to be 
!          able to reproduce Heracles-4_0 results (zero diff). 
! ---------------------------------------------------------------------------------------------------

! If using GEOS CICE Thermodynamics. If using LANL CICE, this is accomplished in Thermo1
!---------------------------------------------------------------------------------------

    geos_cice: if ( DO_CICE_THERMO == 0) then
       N   = ICE
       CFT = (CH(:,N)/CTATM)
       CFQ = (CQ(:,N)/CQATM)
       EVP = CFQ*(EVAP + DEV*(QS(:,N)-QHATM))
       SHF = CFT*(SH   + DSH*(TS(:,N)-THATM))
       SHD = CFT*DSH
       EVD = CFQ*DEV*GEOS_DQSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.)
       DTS = LWDNSRF - (ALW + BLW*TS(:,N)) - SHF

       DTX = DT  / ( SALTWATERICECAP*HH(:,N))
       SWN = (1.-ALBVRI)*VSUVR + (1.-ALBVFI)*VSUVF + &
             (1.-ALBNRI)*DRNIR + (1.-ALBNFI)*DFNIR
       DTS = DTX * ( DTS + SWN - EVP*MAPL_ALHS )
       DTS = DTS   / ( 1.0 + DTX*(BLW + SHD + EVD*MAPL_ALHS) )
       DTS = DTS - max((TS(:,N) + DTS)-MAPL_TICE, 0.)
       EVP = EVP + EVD * DTS
       SHF = SHF + SHD * DTS
       LHF = EVP * MAPL_ALHS

       if(associated(HLATICE)) then
          WHERE( FR(:,N)>0.0 )
             HLATICE = LHF
          ELSEWHERE
             HLATICE = MAPL_UNDEF
          ENDWHERE
       endif
       if(associated(  SHICE)) then
          WHERE( FR(:,N)>0.0 )
             SHICE   = SHF
          ELSEWHERE
             SHICE   = MAPL_UNDEF
          ENDWHERE
       endif

! Update SEA-ICE surface temperature and moisture
!----------------------------------------

       TS(:,N) = TS(:,N) + DTS
       DQS     = GEOS_QSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.) - QS(:,N)
       QS(:,N) = QS(:,N) + DQS

       HH(:,N) = HH(:,N) + DT*(SNO - EVP)
       HH(:,N) = max(min(HH(:,N),  MAXICEDEPTH),  MINICEDEPTH)

       if(associated(SUBLIM  ))SUBLIM  =           EVP    *FR(:,N)
       if(associated(EVAPOUT)) EVAPOUT = EVAPOUT + EVP    *FR(:,N)
       if(associated(SHOUT  )) SHOUT   = SHOUT   + SHF    *FR(:,N)
       if(associated(HLATN  )) HLATN   = HLATN   + LHF    *FR(:,N)
       if(associated(DELTS  )) DELTS   = DELTS   + DTS*CFT*FR(:,N)
       if(associated(DELQS  )) DELQS   = DELQS   + DQS*CFQ*FR(:,N)

    endif geos_cice

    call MAPL_TimerOn(MAPL,    "-OpenWater")

    N   = WATER
    CFT = (CH(:,N)/CTATM)
    CFQ = (CQ(:,N)/CQATM)
    EVP = CFQ*(EVAP + DEV*(QS(:,N)-QHATM))
    SHF = CFT*(SH   + DSH*(TS(:,N)-THATM))
    SHD = CFT*DSH
    EVD = CFQ*DEV*GEOS_DQSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.)
    DTS = LWDNSRF - (ALW + BLW*TS(:,N)) - SHF
    if(associated(CFQW  )) CFQW   =  CFQ 
    if(associated(EVPW0A)) EVPW0A =  FR8(:,WATER)*CFQ*EVAP
    if(associated(EVPW0B)) EVPW0B =  FR8(:,WATER)*CFQ*DEV*(QS(:,N)-QHATM)
    if(associated(EVPW0 )) EVPW0  =  FR8(:,WATER)*EVP

    ! FR accounts for skin under ice
    if ( DO_CICE_THERMO == 0) then
       DTX = DT*FR(:,N)  / (SALTWATERCAP*HH(:,N))
    else
       DTX = DT*FR8(:,N) / (SALTWATERCAP*HH(:,N))
    end if

    if(associated(FRW)) then
      if (DO_CICE_THERMO == 0) then
        FRW = FR(:,N)
      else
        FRW = FR8(:,N)
      endif
    endif

    if (DO_SKIN_LAYER /= 0) DTX = DTX*((MUSKIN+1.)/MUSKIN)

    PEN = exp(-(KUVR/MAPL_RHOWTR)*HW)           ! replace MAPL_RHOWTR with MAPL_RHO_SEAWATER
    PUR = (1.-ALBVRO)*DRUVR*PEN
    PUF = (1.-ALBVFO)*DFUVR*PEN
    PEN = exp(-(KPAR/MAPL_RHOWTR)*HW)           ! replace MAPL_RHOWTR with MAPL_RHO_SEAWATER
    PPR = (1.-ALBVRO)*DRPAR*PEN
    PPF = (1.-ALBVFO)*DFPAR*PEN
    PEN = PUR + PUF + PPR + PPF
 
    SWN = (1.-ALBVRO)*VSUVR + (1.-ALBVFO)*VSUVF + &
          (1.-ALBNRO)*DRNIR + (1.-ALBNFO)*DFNIR

    ikpar: if ( USE_KPAR /= 1) then    ! (if NOT default) compute penetrated shortwave using below...
       call SIMPLE_SW_ABS(NT, USE_KPAR, (HW/MAPL_RHOWTR), ZTH, SWN, PEN)  ! replace MAPL_RHOWTR with MAPL_RHO_SEAWATER
    end if ikpar
    SWN   = SWN - PEN

! DTY accounts for ice on top of water. Part of Shortwave is absorbed by ice and rest goes to warm water.
! Skin layer only absorbs the portion of SW radiation passing thru the bottom of ice MINUS
! the portion passing thru the skin layer    

    if ( DO_CICE_THERMO == 0) then
       DTY = 0.0d0
    else
       DTY = DT / (SALTWATERCAP*HW) * sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2)
    endif

    DTS = DTX * ( DTS + SWN - EVP*MAPL_ALHL - MAPL_ALHF*SNO ) + DTY
    DTS = DTS   / ( 1.0 + DTX*(BLW + SHD + EVD*MAPL_ALHL) )
    EVP = EVP + EVD * DTS
    SHF = SHF + SHD * DTS
    LHF = EVP * MAPL_ALHL
    if(associated(EVPW1)) EVPW1 =  FR8(:,WATER)*EVD*DTS

    if ( DO_CICE_THERMO == 0) then
       if(associated(HLATWTR)) then
          WHERE( FR(:,N)>0.0 )
             HLATWTR = LHF
          ELSEWHERE
             HLATWTR = MAPL_UNDEF
          ENDWHERE
       endif
       if(associated(  SHWTR)) then
          WHERE( FR(:,N)>0.0 )
             SHWTR   = SHF
          ELSEWHERE
             SHWTR   = MAPL_UNDEF
          ENDWHERE
       endif
    else
       if(associated(HLATWTR)) then
          WHERE( FR8(:,N)>0.0 )
             HLATWTR = LHF
          ELSEWHERE
             HLATWTR = MAPL_UNDEF
          ENDWHERE
       endif
       if(associated(  SHWTR)) then
          WHERE( FR8(:,N)>0.0 )
             SHWTR   = SHF
          ELSEWHERE
             SHWTR   = MAPL_UNDEF
          ENDWHERE
       endif
    endif

! Update WATER surface temperature and moisture
!----------------------------------------

    TS(:,N) = TS(:,N) + DTS
    DQS     = GEOS_QSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.) - QS(:,N)
    QS(:,N) = QS(:,N) + DQS

    if(DO_SKIN_LAYER/=0) then
       TWMTS = TWMTS - (1.0/(MUSKIN+1.0))*DTS
    end if

! Layer thickness; liquid precip goes right thru ice.
! FRESHATM is useful for mass flux balance.
! freshwater flux from atmosphere needs to be added to HW
! here since it carries zero enthalpy 
!---------------------------------------------------

    if ( DO_CICE_THERMO == 0) then
       FRESHATM    = FR(:,N)*(SNO - EVP) + PCU + PLS
    else
       FRESHATM    =FR8(:,N)*(SNO - EVP) + PCU + PLS
    endif
    HH(:,N) = HH(:,N) + DT*FRESHATM

    if ( DO_CICE_THERMO == 0) then
       HH(:,N) = max(min(HH(:,N),MAXWATERDEPTH),MINWATERDEPTH)
    end if

    if ( DO_CICE_THERMO == 0) then
       if(associated(EVAPOUT)) EVAPOUT = EVAPOUT + EVP    *FR(:,N)
       if(associated(SHOUT  )) SHOUT   = SHOUT   + SHF    *FR(:,N)
       if(associated(HLATN  )) HLATN   = HLATN   + LHF    *FR(:,N)
       if(associated(DELTS  )) DELTS   = DELTS   + DTS*CFT*FR(:,N)
       if(associated(DELQS  )) DELQS   = DELQS   + DQS*CFQ*FR(:,N)
    else
       if(associated(EVAPOUT)) EVAPOUT = EVAPOUT + EVP    *FR8(:,N)
       if(associated(SHOUT  )) SHOUT   = SHOUT   + SHF    *FR8(:,N)
       if(associated(HLATN  )) HLATN   = HLATN   + LHF    *FR8(:,N)
       if(associated(DELTS  )) DELTS   = DELTS   + DTS*CFT*FR8(:,N)
       if(associated(DELQS  )) DELQS   = DELQS   + DQS*CFQ*FR8(:,N)
    endif

    call MAPL_TimerOff(MAPL,   "-OpenWater")

! Copy back to friendly internal variables
!-----------------------------------------

    if ( DO_CICE_THERMO == 0) then
       HI = HH(:,ICE  )
       HW = HH(:,WATER)
       SI = SS(:,ICE  )/HI
       SW = SS(:,WATER)/HW
       TI = TS(:,ICE  )
       TW = TS(:,WATER) + TWMTS
    else
       TW = TS(:,WATER) + TWMTS              ! SA: I don't fully understand how LANL CICE should interact w/Skin Layer. Jul 2015.
       HW = HH(:,WATER)                                     !     So for now, have the same skin layer interaction as in GEOS CICE
       SW = SS(:,WATER)/HW
       DTSACCUM = 0.0
       DTSACCUM = DTSACCUM+DTS
       if(associated(TWINC1 )) TWINC1  = DTS
       if(associated(FWFLUX )) FWFLUX  = FRESHATM
       !if(associated(AHTFLX )) AHTFLX  =                                                                   &
       !                               FR8(:,WATER)*(SWN-LHF-SHF-MAPL_ALHF*SNO+LWDNSRF-ALW-BLW*TS(:,WATER)) &
       !                              + sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2)         &
       !                              + SALTWATERCAP*FRESHATM*TS(:,WATER)
       if(associated(AHTFLX )) AHTFLX  =                                                                   &
                                      FR8(:,WATER)*(SWN-LHF-SHF-MAPL_ALHF*SNO+LWDNSRF-ALW-BLW*TS(:,WATER)) &
                                     + sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2)       
       if(associated(AHTPR )) AHTPR  =  SALTWATERCAP*FRESHATM*TS(:,WATER) 
       if(associated(AHTSW )) AHTSW  =  FR8(:,WATER)*SWN 
       if(associated(AHTLW )) AHTLW  =  FR8(:,WATER)*(LWDNSRF-ALW-BLW*TS(:,WATER)) 
       if(associated(AHTLWD)) AHTLWD =  FR8(:,WATER)*LWDNSRF
       if(associated(AHTLWU)) AHTLWU =  FR8(:,WATER)*(ALW+BLW*TS(:,WATER))
       if(associated(AHTSH )) AHTSH  =  FR8(:,WATER)*SHF
       if(associated(AHTLH )) AHTLH  =  FR8(:,WATER)*LHF
       if(associated(AHTSWI)) AHTSWI =  sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2) 
       if(associated(AHTSNO)) AHTSNO =  FR8(:,WATER)*MAPL_ALHF*SNO
                                    

       if(associated(PENUVR)) PENUVR  = PENUVR + FR8(:,WATER) * PUR
       if(associated(PENUVF)) PENUVF  = PENUVF + FR8(:,WATER) * PUF
       if(associated(PENPAR)) PENPAR  = PENPAR + FR8(:,WATER) * PPR
       if(associated(PENPAF)) PENPAF  = PENPAF + FR8(:,WATER) * PPF

       if ( DIAG_ICE_BUDGET /= 0) then
          TILEAREA  = AREA*MAPL_RADIUS*MAPL_RADIUS
          DENTH = SALTWATERCAP*(HH(:,WATER)*(TS(:,WATER)-TFfresh)-HW_OLD*(TW_OLD-TFfresh))
          TOTALAREA = sum(DENTH*TILEAREA)

          call ESMF_VMBarrier(VMG, rc=status)
          VERIFY_(STATUS)
          call MAPL_CommsAllReduceSum(VMG, TOTALAREA, ALLTOTALAREA, 1, RC=STATUS)
          VERIFY_(STATUS)

          if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' After Open water******************* '
          if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' total skin enthalpy change = ', &
                                                    ALLTOTALAREA

          TOTALFLUX =                                                                   &  ! related to AHTFLX
                   FR8(:,WATER)*(SWN-LHF-SHF-MAPL_ALHF*SNO+LWDNSRF-ALW-BLW*TS(:,WATER)) &
                       + sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2)    &
                       + SALTWATERCAP*FRESHATM*(TS(:,WATER) -TFfresh)
          TOTALAREA = sum(TOTALFLUX * DT*TILEAREA)

          call ESMF_VMBarrier(VMG, rc=status)
          VERIFY_(STATUS)
          call MAPL_CommsAllReduceSum(VMG, TOTALAREA, ALLTOTALAREA, 1, RC=STATUS)
          VERIFY_(STATUS)

          if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' total heatflux * dt        = ', &
                                       ALLTOTALAREA
       endif

       if(associated(PR_C5)) then
          PR_C5 = FRCICE * (PLS + PCU)
          where(FRCICE == 0.0)
             PR_C5 = 0.0
          endwhere
       endif
       if(associated(PRSN_C5)) then
          PRSN_C5 = FRCICE * SNO
          where(FRCICE == 0.0)
             PRSN_C5 = 0.0
          endwhere
       endif
    end if

! Atmospheric surface stresses
!-----------------------------
    !*** already computed in surface
    !UUA = (TAUX/CMATM + UHATM)
    !VVA = (TAUY/CMATM + VHATM)

! Net Solar insolation (including UV & IR, direct & diffuse) in interface layer 
!------------------------------------------------------------------------------

    SWN = (1.-ALBVRO)*VSUVR + (1.-ALBVFO)*VSUVF + &
          (1.-ALBNRO)*DRNIR + (1.-ALBNFO)*DFNIR


! how many cool-skin iterations to do?
!-------------------------------------

    call MAPL_GetResource ( MAPL, n_iter_cool, Label="COOL_SKIN_LAYER_ITERATIONS" , DEFAULT=3,    RC=STATUS)
    VERIFY_(STATUS)

! Marginal Ice Zone- threshold on fraction: if no LANL CICE, SST IS ALLOWED TO VARY WITHIN ICE EXTENT.
!-------------------------------------------------------------------------------------------------

    if ( DO_CICE_THERMO == 0) then
       call MAPL_GetResource ( MAPL, fr_ice_thresh, Label="THRESHOLD_ICE_FR_SST:" , DEFAULT=0.0,    RC=STATUS)
    else
       call MAPL_GetResource ( MAPL, fr_ice_thresh, Label="THRESHOLD_ICE_FR_SST:" , DEFAULT=0.15,   RC=STATUS)
    end if
    VERIFY_(STATUS)

! Cool-skin and diurnal warm layer. It changes TS, TWMTS, TW if DO_SKIN_LAYER = 1
!--------------------------------------------------------------------------------

    if ( DO_CICE_THERMO == 0) then
       call SKIN_SST (DO_SKIN_LAYER,NT,CM,UUA,VVA,UW,VW,HW,SWN,LHF,SHF,LWDNSRF,                   &
                      ALW,BLW,PEN,STOKES_SPEED,DT,MUSKIN,TS_FOUNDi,DWARM_,TBAR_,TXW,TYW,USTARW_,  &
                      DCOOL_,TDROP_,SWCOOL_,QCOOL_,BCOOL_,LCOOL_,TDEL_,SWWARM_,QWARM_,ZETA_W_,    &
                      PHIW_,LANGM_,TAUTW_,uStokes_,TS,TWMTS,TW,WATER,FR,n_iter_cool,fr_ice_thresh)
    else
       allocate(tmp2(NT, NUM_SUBTILES), STAT=STATUS)          
       VERIFY_(STATUS)                                        
       tmp2 = REAL(FR8, KIND=MAPL_R4)                  
 
       call SKIN_SST (DO_SKIN_LAYER,NT,CM,UUA,VVA,UW,VW,HW,SWN,LHF,SHF,LWDNSRF,                   &
                      ALW,BLW,PEN,STOKES_SPEED,DT,MUSKIN,TS_FOUNDi,DWARM_,TBAR_,TXW,TYW,USTARW_,  &
                      DCOOL_,TDROP_,SWCOOL_,QCOOL_,BCOOL_,LCOOL_,TDEL_,SWWARM_,QWARM_,ZETA_W_,    &
                      PHIW_,LANGM_,TAUTW_,uStokes_,TS,TWMTS,TW,WATER,tmp2,n_iter_cool,&
                      fr_ice_thresh)
 
       deallocate(tmp2)
    end if

    if(associated(SWcool)) SWcool = SWCOOL_
    if(associated(SWWARM)) SWWARM = SWWARM_
    if(associated(Qcool )) Qcool  = QCOOL_
    if(associated(QWARM )) QWARM  = QWARM_
    if(associated(PHIW  )) PHIW   = PHIW_
    if(associated(LANGM )) LANGM  = LANGM_

    if(associated(Dcool )) Dcool  = DCOOL_
    if(associated(Dwarm )) Dwarm  = DWARM_
    if(associated(Tdrop )) Tdrop  = TDROP_
    if(associated(Tbar  )) Tbar   = TBAR_
    if(associated(Ustarw)) Ustarw = USTARW_
    if(associated(Lcool )) Lcool  = LCOOL_
    if(associated(Bcool )) Bcool  = BCOOL_
    if(associated(Tdel  )) Tdel   = TDEL_
    if(associated(TauTW )) TauTW  = TAUTW_
    if(associated(ZETA_W)) ZETA_W  = ZETA_W_
    if(associated(TS_FOUNDe)) TS_FOUNDe = TS_FOUNDi

! Stress over ice
!----------------

    if ( DO_CICE_THERMO == 0) then
       where( FR(:,ICE)>0.0 )
          TXI = CM(:,ICE)*(UUA - UI)
          TYI = CM(:,ICE)*(VVA - VI)
       elsewhere
          TXI = MAPL_UNDEF
          TYI = MAPL_UNDEF
       end where
    else
       where(FRCICE > puny)
          TXI = sum(CM(:,ICE:)*FR8(:,ICE:), dim=2)/FRCICE*(UUA - UI)
          TYI = sum(CM(:,ICE:)*FR8(:,ICE:), dim=2)/FRCICE*(VVA - VI)
       elsewhere
          TXI = 0.0
          TYI = 0.0
       endwhere
    end if

! Average ocean stress
!---------------------

    if ( DO_CICE_THERMO == 0) then
       where( FR(:,ICE)>0.0 .and. FR(:,WATER)>0.0)
          TXO = TXI*FR(:,ICE) + TXW*FR(:,WATER)
          TYO = TYI*FR(:,ICE) + TYW*FR(:,WATER)
       end where

       where(FR(:,WATER)==0.0)
          TXO = TXI
          TYO = TYI
       end where

       where(FR(:,ICE)==0.0)
          TXO = TXW
          TYO = TYW
       end where
    else
       TXO = sum(CM(:,ICE:)*FR8(:,ICE:), dim=2)*(UUA - UI) + TXW*FR8(:,WATER)
       TYO = sum(CM(:,ICE:)*FR8(:,ICE:), dim=2)*(VVA - VI) + TYW*FR8(:,WATER)
    end if

    if(associated(PSEX )) PSEX  = PS 
    if(associated(TAUXW)) TAUXW = TXW
    if(associated(TAUYW)) TAUYW = TYW
    if(associated(TAUXI)) TAUXI = TXI
    if(associated(TAUYI)) TAUYI = TYI
    if(associated(TAUXO)) TAUXO = TXO
    if(associated(TAUYO)) TAUYO = TYO
    if(associated(USTR3)) USTR3 = sqrt(sqrt(TXO*TXO+TYO*TYO)/MAPL_RHOWTR)**3    ! SA: replace MAPL_RHOWTR with MAPL_RHO_SEAWATER
    if(associated(UUEX))  UUEX  = UU

! Copies for export
!------------------

    if(associated(SNOWOCN)) SNOWOCN = SNO
    if(associated(RAINOCN)) RAINOCN = PCU + PLS
    if(associated(TST    )) TST     = 0.0
    if(associated(QST    )) QST     = 0.0
    if(associated(HLWUP  )) HLWUP   = ALW 
    if(associated(LWNDSRF)) LWNDSRF = LWDNSRF - ALW

    if ( DO_CICE_THERMO == 0) then
       if(associated(LWNDWTR)) then
          where( FR(:,WATER)>0.0 )
             LWNDWTR = LWDNSRF - ALW - BLW*TS(:,WATER)
          elsewhere
             LWNDWTR = MAPL_UNDEF
          end where
       endif
    else
       if(associated(LWNDWTR)) then
          where( FR8(:,WATER)>0.0 )
             LWNDWTR = LWDNSRF - ALW - BLW*TS(:,WATER)
          elsewhere
             LWNDWTR = MAPL_UNDEF
          end where
       endif
    end if

    if ( DO_CICE_THERMO == 0) then    ! See below Thermo1 and Thermo2 on how following are computed when using LANL CICE.
       if(associated(LWNDICE)) then 
          where( FR(:,ICE)>0.0 ) 
             LWNDICE = LWDNSRF - ALW - BLW*TS(:,  ICE) 
          elsewhere
             LWNDICE = MAPL_UNDEF
          end where
       endif

       do N=1,NUM_SUBTILES
          if(associated(TST    )) TST     = TST     +     TS(:,N)*FR(:,N)
          if(associated(QST    )) QST     = QST     +     QS(:,N)*FR(:,N)
          if(associated(LWNDSRF)) LWNDSRF = LWNDSRF - BLW*TS(:,N)*FR(:,N)
          if(associated(HLWUP  )) HLWUP   = HLWUP   + BLW*TS(:,N)*FR(:,N)
       end do

       EMISS = EMSH2O*FR(:,WATER) + EMSICE*FR(:,ICE)
       ALBVR = ALBVRO*FR(:,WATER) + ALBVRI*FR(:,ICE)
       ALBVF = ALBVFO*FR(:,WATER) + ALBVFI*FR(:,ICE)
       ALBNR = ALBNRO*FR(:,WATER) + ALBNRI*FR(:,ICE)
       ALBNF = ALBNFO*FR(:,WATER) + ALBNFI*FR(:,ICE)

       if(associated(SWNDSRF)) then 
       SWNDSRF = &
           (1.-ALBVR)*VSUVR + (1.-ALBVF)*VSUVF + &
                    (1.-ALBNR)*DRNIR + (1.-ALBNF)*DFNIR
       end if

       if(associated(SWNDWTR)) then 
          where( FR(:,WATER)>0.0 ) 
             SWNDWTR = (1.-ALBVRO)*VSUVR + (1.-ALBVFO)*VSUVF + &
                       (1.-ALBNRO)*DRNIR + (1.-ALBNFO)*DFNIR 
          elsewhere
             SWNDWTR = MAPL_UNDEF
          end where
       end if

       if(associated(SWNDICE)) then 
          where( FR(:,ICE)>0.0 ) 
             SWNDICE = (1.-ALBVRI)*VSUVR + (1.-ALBVFI)*VSUVF + &
                       (1.-ALBNRI)*DRNIR + (1.-ALBNFI)*DFNIR
          elsewhere
             SWNDICE = MAPL_UNDEF
          end where
       end if
    end if

!xxxxxxxxxxxxxxxxxxxxxxxxxxLANL CICE: 2 step update procedure-- STARTS xxxxxxxxxxxxxxxxxxxxxxxxx
    call MAPL_TimerOn(MAPL,   "-Thermo1")

! 1st Step of LANL CICE Thermodynamics
! ------------------------------------

    cice_th1_: if ( DO_CICE_THERMO /= 0) then

          FR8TMP = FR8 
       categories_th1_: do N=ICE, NUM_SUBTILES   ! Loop over ice catgories. Also, recall ICE = 2 with LANL CICE

          NSUB = N - ICE + 1

          CFT = (CH(:,N)/CTATM)
          CFQ = (CQ(:,N)/CQATM)
          EVP = CFQ*(EVAP + DEV*(QS(:,N)-QHATM))
          SHF = CFT*(SH   + DSH*(TS(:,N)-THATM))
          SHD = CFT*DSH
          EVD = CFQ*DEV*GEOS_DQSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.)
          LHF = EVP * MAPL_ALHS

          !call CICE_THERMO1 (N,NSUB,NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,TF,FR8,TS,HH,&
          call CICE_THERMO1 (N,NSUB,NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,TF,FR8TMP,TS,HH,&
                           ERGICE,ERGSNO,TAUXBOT,TAUYBOT,TBOT,ISWABS,SSWABS,             &
                           DO_POND,FRZMLT,FBOT,RSIDE,PCU,PLS,                            &
                           FSWTHRU,FCOND,FCONDBOT,EVP,FRESHN,FSALTN,FHOCNN,              &
                           MELTT,MELTS,MELTB,CONGEL,SNOICE,VOLICE,VOLSNO,SHF,LHF,        &
                           VOLPOND,APONDN,HPONDN,SALTWATERCAP,TAUAGE,TRACERS,ALW,BLW,    &
                           FSWSFC,FSWINT,FSWABS,LWDNSRF,EVD,SHD,SNO,SBLX)

!         Some aggregation of fluxes to the Ocean has to be done now, before using in step2

          FRESHL   = FRESHL   + FRESHN *FR8(:,N)
          FSALTL   = FSALTL   + FSALTN *FR8(:,N)
          FHOCNL   = FHOCNL   + FHOCNN *FR8(:,N)

          NEWICEERG = NEWICEERG + (FCONDBOT(:,NSUB) - FBOT) * FR8(:,N)

!         Update surface temperature and moisture
!         ----------------------------------------

          if(associated(SHICE)  ) SHICE   = SHICE   + SHF        *FR8(:,N)     ! aggregate ice surface fluxes into atm
          if(associated(HLATICE)) HLATICE = HLATICE + LHF        *FR8(:,N)
          if(associated(EVAPOUT)) EVAPOUT = EVAPOUT + EVP        *FR8(:,N)
          if(associated(SUBLIM )) SUBLIM  = SUBLIM  + EVP        *FR8(:,N)
          if(associated(SHOUT  )) SHOUT   = SHOUT   + SHF        *FR8(:,N)
          if(associated(HLATN  )) HLATN   = HLATN   + LHF        *FR8(:,N)
          if(associated(LWNDSRF)) LWNDSRF = LWNDSRF - BLW*TS(:,N)*FR8(:,N)
          if(associated(HLWUP  )) HLWUP   = HLWUP   + BLW*TS(:,N)*FR8(:,N)

          if(associated(MELTTL )) MELTTL   = MELTTL   + MELTT   *FR8(:,N) / DT ! m per step -> m s-1
          if(associated(MELTBL )) MELTBL   = MELTBL   + MELTB   *FR8(:,N) / DT ! m per step -> m s-1
          if(associated(MELTSL )) MELTSL   = MELTSL   + MELTS   *FR8(:,N) / DT ! m per step -> m s-1
          if(associated(CONGELO)) CONGELO  = CONGELO  + CONGEL  *FR8(:,N) / DT ! m per step -> m s-1

          if(associated(SBLXOUT)) SBLXOUT  = SBLXOUT  + SBLX    *FR8(:,N) / DT

          if(associated(EVAP_C5))     EVAP_C5      = EVAP_C5     +                      EVP * FR8(:,N)
          if(associated(GRCONGEL_C5)) GRCONGEL_C5  = GRCONGEL_C5 + MAPL_RHO_SEAICE * CONGEL * FR8(:,N) / DT ! kg m-2 s-1
          if(associated(SNOMELT_C5))  SNOMELT_C5   = SNOMELT_C5  + MAPL_RHO_SNOW   * MELTS  * FR8(:,N) / DT ! kg m-2 s-1
          if(associated(TMELT_C5))    TMELT_C5     = TMELT_C5    + MAPL_RHO_SEAICE * MELTT  * FR8(:,N) / DT ! kg m-2 s-1
          if(associated(BMELT_C5))    BMELT_C5     = BMELT_C5    + MAPL_RHO_SEAICE * MELTB  * FR8(:,N) / DT ! kg m-2 s-1

!         Aggregate ts and qs change over ice categories

          DTS     = TS(:,N) - TS_OLD(:,N)
          DQS     = GEOS_QSAT(TS(:,N), PS, RAMP=0.0, PASCALS=.TRUE.) - QS(:,N)
          QS(:,N) = QS(:,N) + DQS

          if(associated(DELTS  )) DELTS   = DELTS   + DTS*CFT*FR8(:,N)
          if(associated(DELQS  )) DELQS   = DELQS   + DQS*CFQ*FR8(:,N)
       end do categories_th1_

       FR8 = FR8TMP
       FRCICE = sum(FR8(:,ICE:), dim=2) 
       FR8(:,WATER) = min(max(1.0-FRCICE, 0.0), 1.0)

       if(associated(FSWTRUO)) FSWTRUO     = sum(FR8(:,ICE:)*FSWTHRU,dim=2)
       if(associated(FBOTL  )) FBOTL       = FBOT
       if(associated(FSWABSO)) FSWABSO     = sum(FR8(:,ICE:)*(FSWTHRU-FSWTHRUWTR),dim=2)
       if(associated(TST    )) TST         = sum(TS*FR8,dim=2)
       if(associated(QST    )) QST         = sum(QS*FR8,dim=2)

       if(associated(SNOONICE )) SNOONICE  = FRCICE*SNO

       if(associated(LWNDICE)) then
          where( FRCICE>puny )
             LWNDICE = LWDNSRF - ALW - BLW*(sum(TS(:,ICE:)*FR8(:,ICE:), dim=2) / FRCICE)
          elsewhere
             LWNDICE = MAPL_UNDEF
          end where
       endif

       if(associated(SHICE)) then
          where( FRCICE>puny )
             SHICE = SHICE / FRCICE
          elsewhere
             SHICE = MAPL_UNDEF
          endwhere
       endif

       if(associated(HLATICE)) then
          where( FRCICE>puny )
             HLATICE = HLATICE / FRCICE   ! SA: Bin, protect with a "tiny"? so that we DO NOT have hlatice/EPSILON
          elsewhere
             HLATICE = MAPL_UNDEF
          endwhere
       endif

       if(associated(FCONDTOP)) then
          where(FRCICE > puny)
             FCONDTOP = sum(FCOND*FR8(:,ICE:),dim=2)/FRCICE
          elsewhere
             FCONDTOP = MAPL_UNDEF
          endwhere
       endif

       if(associated(FCONDB)) then
          where(FRCICE > puny)
             FCONDB = sum(FCONDBOT*FR8(:,ICE:),dim=2)/FRCICE
          elsewhere
             FCONDB = MAPL_UNDEF
          endwhere
       end if

       if(associated(SNOMELT_C5)) then
          where(FRCICE == 0.0)
             SNOMELT_C5 = 0.0
          endwhere
       endif

       if(associated(TMELT_C5)) then
          where(FRCICE == 0.0)
             TMELT_C5 = 0.0
          endwhere
       endif

       if(associated(BMELT_C5)) then
          where(FRCICE == 0.0)
             BMELT_C5 = 0.0
          endwhere
       endif

       if(associated(HFSIFRAZIL_C5)) then
          HFSIFRAZIL_C5 = FRZMLT
          where(FRZMLT < 0.0)
             HFSIFRAZIL_C5 = 0.0
          endwhere
       endif

       if ( DIAG_ICE_BUDGET /= 0) then
          TOTALFLUX = -FRESHL+FRCICE*SNO-SUBLIM
          where(SLMASK > 0.5)
             TOTALFLUX = 0.0
          endwhere

          call CICE_ICE_BUDGET (DT, NT, 1, VMG, TILEAREA, TOTALFLUX, SLMASK, VOLICE, VOLICE_OLD, VOLSNO, VOLSNO_OLD)

          VOLICE_OLD = sum(VOLICE,dim=2)
          VOLSNO_OLD = sum(VOLSNO,dim=2)
       endif

       ALBVRI = sum(ALBVRN(:,:)*FR8(:,ICE:),dim=2)
       ALBVFI = sum(ALBVFN(:,:)*FR8(:,ICE:),dim=2)
       ALBNRI = sum(ALBNRN(:,:)*FR8(:,ICE:),dim=2)
       ALBNFI = sum(ALBNFN(:,:)*FR8(:,ICE:),dim=2)

       EMISS = EMSH2O*FR8(:,WATER) + EMSICE*sum(FR8(:,ICE:),dim=2)
       ALBVR = ALBVRO*FR8(:,WATER) + ALBVRI
       ALBVF = ALBVFO*FR8(:,WATER) + ALBVFI
       ALBNR = ALBNRO*FR8(:,WATER) + ALBNRI
       ALBNF = ALBNFO*FR8(:,WATER) + ALBNFI

       where( FRCICE>puny )
          ALBVRI = ALBVRI / FRCICE
          ALBVFI = ALBVFI / FRCICE
          ALBNRI = ALBNRI / FRCICE
          ALBNFI = ALBNFI / FRCICE
       endwhere

       if(associated(SWNDICE)) then
          where( FRCICE>0.0 )
             SWNDICE = (1.-ALBVRI)*VSUVR + (1.-ALBVFI)*VSUVF + &
                       (1.-ALBNRI)*DRNIR + (1.-ALBNFI)*DFNIR
          elsewhere
             SWNDICE = MAPL_UNDEF
          end where
       end if
    end if cice_th1_


    call MAPL_TimerOff(MAPL,  "-Thermo1")

! 2nd Step of LANL CICE Thermodynamics: 
! loops over ice categories within the  subroutines, 
! redistributing ice and water mass due to freezing and melting
! -------------------------------------------------------------

    call MAPL_TimerOn(MAPL,    "-Thermo2")

    cice_th2_: if ( DO_CICE_THERMO /= 0) then

       FRESHL_OLD = FRESHL

       call CICE_THERMO2_STEP1 (NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,TF,FR8,TS,    &
                                VOLICE,VOLSNO,VOLPOND,ERGICE,ERGSNO,                    &
                                AICENINIT,VICENINIT,TRCRTYPE,FRCICE,FRZMLT,FRAZLN,      &
                                FRESHL,FSALTL,FHOCNL,RSIDE,MELTLN,VOLICE_DELTA,         &
                                TRACERS,TAUAGE,SNOICE,HH,SS,HW,SW)

       FRCICE       = sum(FR8(:,ICE:), dim=2)
       FR8(:,WATER) = min(max(1.0-FRCICE, 0.0), 1.0)

       if(associated(FSITHERM_CMIP5)) FSITHERM_CMIP5 = FRESHL
       if(associated(FCONDBOTN))      FCONDBOTN      = FCONDBOT

       if(DIAG_ICE_BUDGET /= 0) then
          TOTALFLUX = -(FRESHL - FRESHL_OLD)
          where(SLMASK > 0.5)
             TOTALFLUX = 0.0
          end where
          call CICE_ICE_BUDGET (DT, NT, 2, VMG, TILEAREA, TOTALFLUX, SLMASK, VOLICE, VOLICE_OLD, VOLSNO, VOLSNO_OLD)
       endif

       !*** artificially do a lateral melt step over those frozen lake tiles if the ice gets too thick
       call CICE_THERMO2_STEP2 (NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,FR8,TS,          &
                                VOLICE,VOLSNO,VOLPOND,ERGICE,ERGSNO,                     &
                                TRCRTYPE,FRCICE,SLMASK,TRACERS,TAUAGE)
       TI8 = TS(:,ICE:)

       ! aggregate ice concentration after step2
       ! These are the final area fractions that are in the internal state

       FRCICE = sum(FR8(:,ICE:), dim=2)
       FR8(:,WATER) = min(max(1.0-FRCICE, 0.0), 1.0)

       if(associated(GRLATERAL_C5)) then
          GRLATERAL_C5 = MAPL_RHO_SEAICE*sum(VOLICE_DELTA, dim=2)/DT
       endif

       if(associated(SNOTOICE_C5)) then
          SNOTOICE_C5  =  MAPL_RHO_SEAICE * SNOICE / DT ! kg m-2 s-1
          where( sum(VOLSNO, dim=2) == 0.0)
             SNOTOICE_C5 = 0.0
          end where
       endif

       if(associated(SFDSI_C5)) then
          SFDSI_C5  =  FSALTL ! kg m-2 s-1
          where(FRCICE == 0.0)
             SFDSI_C5 = 0.0
          end where
       endif

       if(associated(DAIDTT)) then
          DAIDTT = (FRCICE - FR_OLD) / DT * 8640000
       endif

       if(associated(TINZ   )) then
          TINZ = MAPL_UNDEF
          do K=1, NT
             call diagnose_internal_ice_temp(VOLICE(K,:), ERGICE(K,:,:), TINZ(K,:))
          enddo
       endif

       !*** in a time splitting fasion, we update TW here to account for FHOCNL accumulated in thermodynamics
       !*** the order of updating TW,SW,and HW is important here: ALWAYS, 
       !*** update  HW/HH(:,WATER) first, then TW/TS(:,WATER), finally SW
       !*** failing to follow the above order will result in non-conservative mass/energy  
       !*** account for ice meltwater at the top and bottom surface or water frozen at the bottom surface  


       HW_TMP      = HH(:,WATER)
       HH(:,WATER) = HH(:,WATER) + DT*FRESHL
       HH(:,WATER) = max(min(HH(:,WATER),MAXWATERDEPTH),MINWATERDEPTH)
       HW = HH(:,WATER)

       DTS          = DT*FHOCNL/(SALTWATERCAP*HH(:,WATER))
       TS_TMP       = TS(:, WATER)
       where(NEWICEERG < 0.0) ! ice growth
          TS(:, WATER) = HW_TMP/HH(:,WATER)*TS_TMP + (1.-HW_TMP/HH(:,WATER))*TFfresh + &
                        DT * (FHOCNL-NEWICEERG) / (SALTWATERCAP * HH(:,WATER))
       elsewhere ! code below deals with oceanic melting of ice in an energy conservative
                 ! way, i.e., it takes account of enthalpy of melt; it raises temperature 
                 ! of melt from 0C to the final value.  
                 ! FHOCNL is the change of ice/snow enthalpy due to melt
          TS(:, WATER) = HW_TMP/HH(:,WATER)*TS_TMP + (1.-HW_TMP/HH(:,WATER))*TFfresh + &
                        DT * FHOCNL / (SALTWATERCAP * HH(:,WATER))
       endwhere
       if(associated(NIERG)) NIERG = NEWICEERG 

       if(associated(TWINC2  )) TWINC2   = (1.-HW_TMP/HH(:,WATER))*(TFfresh-TS_TMP) &
                                           + DT*(FHOCNL/(SALTWATERCAP*HH(:,WATER)))
       if(associated(TWINCT  )) TWINCT   = DTSACCUM + (TS(:, WATER) - TS_TMP)

       if(associated(SNOICEO)) SNOICEO = SNOICE / DT ! m per step -> m s-1
       if(associated(FRESH  )) FRESH   = FRESHL
       if(associated(FSALT  )) FSALT   = FSALTL
       if(associated(FHOCN  )) FHOCN   = FHOCNL

       TW = TS(:,WATER)
       if(associated(TW1))   TW1  = TS(:,WATER) 
       if(associated(HW1))   HW1  = HH(:,WATER) 

       ! account for flux of salt (>0) under melting conditions or negative flux when sea water is freezing                
       ! multiply by 1000 to account for g->kg conversion

       SW = (SS(:,WATER)+DT*1.e3*FSALTL)/HH(:,WATER)
       where (SLMASK > 0.5)
          SW = max(min(SW,MAXSALINITY),MINSALINITY)
       endwhere
       if(associated(SSKINW2)) SSKINW2 = SW

       if(associated(ISTSFC)) then
          ! to be consisten with CICE (unit in degC)
          ISTSFC = sum((TS(:,ICE:)-TFfresh)*FR8(:,ICE:),dim=2)
          where(FRCICE > 0.0)
             ISTSFC = ISTSFC / FRCICE
          elsewhere
             ISTSFC = MAPL_UNDEF
          end where
       endif

       if(associated(IAGE)) then
          ! here ice age is treated as an ice area tracer
          IAGE = sum(TAUAGE(:,ICE:)*FR8(:,ICE:),dim=2) * iage_converter
          where(FRCICE > 0.0)
             IAGE = IAGE / FRCICE
          elsewhere
             IAGE = MAPL_UNDEF
          end where
       endif

       ! the mean ice/snow thickness is computed as: sum_n_over_ice_categories(FR(n)*H(n)) which is simply 
       ! sum_n_over_ice_categories(VOL(n)) 

       if(associated(HICE  )) HICE    =  sum(VOLICE(:,:),dim=2)
       if(associated(HSNO  )) HSNO    =  sum(VOLSNO(:,:),dim=2)
       if(associated(MELTL )) MELTL   =  MELTLN / DT                   ! m per step -> m s-1
       if(associated(FRAZIL)) FRAZIL  =  FRAZLN / DT                   ! m per step -> m s-1
       if(associated(FRI ))   FRI     =  FRCICE

       if(associated(DVIDTT))  then
          DVIDTT  = (sum(VOLICE,dim=2) - VOLICE_OLD) / DT * 8640000
       end if

       if(associated(GRFRAZIL_C5)) then
          GRFRAZIL_C5  =  MAPL_RHO_SEAICE * FRAZLN / DT ! kg m-2 s-1
          where(FRCICE == 0.0)
             GRFRAZIL_C5 = 0.0
          end where
       end if
    end if cice_th2_
    
    call MAPL_TimerOff(MAPL,   "-Thermo2")

!xxxxxxxxxxxxxxxxxxxxxxxxxxLANL CICE: 2 step update procedure-- ENDS xxxxxxxxxxxxxxxxxxxxxxxxxxx

!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    call MAPL_TimerOn(MAPL,    "-Albedo")

    if(solalarmison) then
       call MAPL_SunGetInsolation(LONS, LATS,      &
            ORBIT, ZTH, SLR,                       &
            INTV = TINT,                           &
            currTime=CURRENT_TIME+DELT,            &
            RC=STATUS )
       VERIFY_(STATUS)

       ZTH = max(0.0,ZTH)

       call ALBSEA    (ALBVRO,ALBVFO,ALBNRO,ALBNFO,ZTH)
          
       which_cice_: if ( DO_CICE_THERMO == 0) then
          call ALBSEAICE (ALBVRI,ALBVFI,ALBNRI,ALBNFI,ZTH,LATS,CURRENT_TIME)   ! GEOS CICE

          ALBVR = ALBVRO*FR(:,WATER) + ALBVRI*FR(:,ICE)
          ALBVF = ALBVFO*FR(:,WATER) + ALBVFI*FR(:,ICE)
          ALBNR = ALBNRO*FR(:,WATER) + ALBNRI*FR(:,ICE)
          ALBNF = ALBNFO*FR(:,WATER) + ALBNFI*FR(:,ICE)

          if(associated(PENUVR)) PENUVR  =  FR(:,WATER) * PUR
          if(associated(PENUVF)) PENUVF  =  FR(:,WATER) * PUF
          if(associated(PENPAR)) PENPAR  =  FR(:,WATER) * PPR
          if(associated(PENPAF)) PENPAF  =  FR(:,WATER) * PPF
       else
          ! LANL CICE
          call CICE_ALBSEAICE (ICE,NUM_ICE_CATEGORIES,NUM_ICE_LAYERS,NUM_SNOW_LAYERS,NT,DO_POND,LATSO,LONSO,LATS,LONS,ZTH,FR8,TS,&
                               DRPAR,DFPAR,DRNIR,DFNIR,DRUVR,DFUVR,VSUVR,VSUVF,VOLICE,VOLSNO,APONDN,HPONDN,                      &
                               ISWABS,FSWSFC, FSWINT,FSWTHRU,SSWABS,ALBIN,ALBSN,ALBPND,ALBVRN,ALBVFN,ALBNRN,ALBNFN,              &
                               DRUVRTHRU,DFUVRTHRU,DRPARTHRU,DFPARTHRU)

          do N=1,NUM_ICE_CATEGORIES
             do K=1,NT
                if(FR8(K,N+1) > puny) then
                   if(associated(IALB_C5))   IALB_C5(K)   = IALB_C5(K) + FR8(K,N+1) * ALBIN(K,N)
                end if
             end do
          end do

          ! report "missing" if there is no sunlight or free of ice 
          if(associated(IALB_C5)) then
             where(FRCICE > puny)
                IALB_C5 =  IALB_C5 / FRCICE
             endwhere
             where(FRCICE <= puny)
                IALB_C5 = MAPL_UNDEF
             endwhere
             where(ZTH < 1.e-6)
                IALB_C5 = MAPL_UNDEF
             endwhere
          endif

          ALBVRI = sum(ALBVRN(:,:)*FR8(:,ICE:),dim=2)
          ALBVFI = sum(ALBVFN(:,:)*FR8(:,ICE:),dim=2)
          ALBNRI = sum(ALBNRN(:,:)*FR8(:,ICE:),dim=2)
          ALBNFI = sum(ALBNFN(:,:)*FR8(:,ICE:),dim=2)

          if(associated(RSUSSI_C5)) then
             RSUSSI_C5 =  ALBVRI*VSUVR + ALBVFI*VSUVF  + ALBNRI*DRNIR + ALBNFI*DFNIR
          endif

          if(associated(SIALB)) then
             where(FRCICE > puny)
                SIALB = (awtvdr*ALBVRI+awtvdf*ALBVFI+awtidr*ALBNRI+awtidf*ALBNFI)/FRCICE
             endwhere
             where(FRCICE <= puny)
                SIALB = MAPL_UNDEF
             endwhere
             where(ZTH < 1.e-6)
                SIALB = MAPL_UNDEF
             endwhere
          endif

          ALBVR = ALBVRO*FR8(:,WATER) + ALBVRI
          ALBVF = ALBVFO*FR8(:,WATER) + ALBVFI
          ALBNR = ALBNRO*FR8(:,WATER) + ALBNRI
          ALBNF = ALBNFO*FR8(:,WATER) + ALBNFI
          
       endif which_cice_
    endif

    call MAPL_TimerOff(MAPL,    "-Albedo")
!xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


! Fill the pointers to (CICE) exports
! ------------------------------------

    if  (  associated(CO2SCEX)      )  CO2SCEX      =  CO2SC
    if  (  associated(DUDPEX)       )  DUDPEX       =  DUDP
    if  (  associated(DUWTEX)       )  DUWTEX       =  DUWT
    if  (  associated(DUSDEX)       )  DUSDEX       =  DUSD
    if  (  associated(BCDPEX)       )  BCDPEX       =  BCDP
    if  (  associated(BCWTEX)       )  BCWTEX       =  BCWT
    if  (  associated(OCDPEX)       )  OCDPEX       =  OCDP
    if  (  associated(OCWTEX)       )  OCWTEX       =  OCWT
    if  (  associated(FSWBANDEX)    )  FSWBANDEX    =  FSWBAND
    if  (  associated(FSWBANDNAEX)  )  FSWBANDNAEX  =  FSWBANDNA

    deallocate(TS)
    deallocate(HH)
    deallocate(SS)
    deallocate(FR)

    if (DO_CICE_THERMO /= 0) then
       deallocate(TRCRTYPE)
       deallocate(TRACERS)
       deallocate(TF)
       deallocate(FRZMLT)
       deallocate(MELTLN)
       deallocate(FRAZLN)
       deallocate(FRESHN)
       deallocate(FRESHL)
       deallocate(FSALTN)
       deallocate(FSALTL)
       deallocate(FHOCNN)
       deallocate(FHOCNL)
       deallocate(RSIDE)
       deallocate(FSWTHRU)
       deallocate(FCOND)
       deallocate(FCONDBOT)
       deallocate(FSWTHRUWTR)
       deallocate(TBOT)
       deallocate(FBOT)
       deallocate(ALBVRN)
       deallocate(ALBNRN)
       deallocate(ALBVFN)
       deallocate(ALBNFN)
       deallocate(FSWSFC)
       deallocate(FSWINT)
       deallocate(ISWABS)
       deallocate(SSWABS)
       deallocate(MELTT)
       deallocate(MELTS)
       deallocate(MELTB)
       deallocate(CONGEL)
       deallocate(SNOICE)
       deallocate(DTSACCUM)
       deallocate(HW_TMP)
       deallocate(TS_TMP)
       deallocate(TS_OLD)
       deallocate(ALBIN)
       deallocate(ALBSN)
       deallocate(ALBPND)
       deallocate(DRUVRTHRU)
       deallocate(DFUVRTHRU)
       deallocate(DRPARTHRU)
       deallocate(DFPARTHRU)
       deallocate(FRESHL_OLD)
       deallocate(TOTALFLUX)
       deallocate(NEWICEERG)
       deallocate(SBLX)
       deallocate(AICENINIT)
       deallocate(VICENINIT)
       deallocate(FR8TMP)
       deallocate(FRCICE)
       deallocate(FR_OLD)
       deallocate(VOLSNO_OLD)
       deallocate(VOLICE_OLD)
       deallocate(VOLICE_DELTA)

       if (DIAG_ICE_BUDGET /= 0) then
          deallocate(HW_OLD)
          deallocate(TW_OLD)
          deallocate(DENTH)
          deallocate(TILEAREA)
       endif
    endif

!  All done
!-----------

    RETURN_(ESMF_SUCCESS)
             
  end subroutine SALTWATERCORE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: ALBSEA - Computes albedos as a function of $cos(\zeta)$ over ocean surfaces

! !INTERFACE:

  subroutine ALBSEA (ALBVR,ALBVF,ALBNR,ALBNF,ZTH)

! !ARGUMENTS:

    real,    intent(IN)  :: ZTH  (:)
    real,    intent(OUT) :: ALBVR(:) ! visible beam    albedo
    real,    intent(OUT) :: ALBVF(:) ! visible diffuse albedo
    real,    intent(OUT) :: ALBNR(:) ! nearIr  beam    albedo
    real,    intent(OUT) :: ALBNF(:) ! nearIr  diffuse albedo

!  !DESCRIPTION:
!        Compute albedo for ocean points
!          based on ceres

!  CERES ocean albedo at zth=.5 is 0.052. Our formulation gives .077
!    thus the scaling. The diffuse albedo is given by computing
!    the zth weighted average of the albedo over the hemisphere and
!    then applying the same scaling to match CERES.


!LLT: CERESFAC = 1           reduces to old formulation 1-5-05
!     CERESFAC = 0.052/0.077 is the Original CERES Factor
!     CERESFAC = 0.068/0.077 is the EROS Tuned Value

    real, parameter :: CERESFAC   = 0.068/0.077
!   real, parameter :: CERESFAC   = 1.0

    real, parameter :: OCNALBVF   = .08*CERESFAC
    real, parameter :: OCNALBNF   = .08*CERESFAC

    real, parameter :: A0         = 0.40670980*CERESFAC
    real, parameter :: A1         =-1.23236340*CERESFAC
    real, parameter :: A2         = 1.42240510*CERESFAC
    real, parameter :: A3         =-0.55573341*CERESFAC

! Beam albedos
!-------------

    ALBVR = A0+(A1+(A2+A3*ZTH)*ZTH)*ZTH
    ALBNR = ALBVR

! Diffuse albedos
!----------------

    ALBVF = OCNALBVF
    ALBNF = OCNALBNF

   RETURN_(ESMF_SUCCESS)
  end subroutine ALBSEA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: ALBSEAICE - Computes albedos as a function of  $cos(\zeta)$ over sea-ice surfaces

! !INTERFACE:

  subroutine ALBSEAICE (ALBVR,ALBVF,ALBNR,ALBNF,ZTH,LATS,currTime)

! !ARGUMENTS:

    type(ESMF_Time), intent(INOUT)    :: currTime
    real,    intent(IN)  :: LATS(:)
    real,    intent(IN)  :: ZTH  (:)
    real,    intent(OUT) :: ALBVR(:) ! visible beam    albedo
    real,    intent(OUT) :: ALBVF(:) ! visible diffuse albedo
    real,    intent(OUT) :: ALBNR(:) ! nearIr  beam    albedo
    real,    intent(OUT) :: ALBNF(:) ! nearIr  diffuse albedo

!  !DESCRIPTION:
!        Compute albedo for ocean points
!          based on Heracles GEOS-AGCM

      !real, parameter :: SEAICEALBVR  = .60
      !real, parameter :: SEAICEALBVF  = .60
      !real, parameter :: SEAICEALBNR  = .60
      !real, parameter :: SEAICEALBNF  = .60
      real                  :: SEAICEALBVRN
      real                  :: SEAICEALBVFN
      real                  :: SEAICEALBNRN
      real                  :: SEAICEALBNFN

      real                  :: SEAICEALBVRS
      real                  :: SEAICEALBVFS
      real                  :: SEAICEALBNRS
      real                  :: SEAICEALBNFS

      real, dimension(0:13) :: shebavis
      real, dimension(0:13) :: shebanir
      real, dimension(0:13) :: nday
      real                  :: afracv,aslopev,abasev
      real                  :: afraci,aslopei,abasei

      character(len=ESMF_MAXSTR)     :: string
      integer               :: YEAR,MONTH,DAY


      shebavis = (/0.820,0.820,0.820,0.820,0.820,0.820,0.751, &
       0.467,0.663,0.820,0.820,0.820,0.820,0.820/)
      shebanir = (/0.820,0.820,0.820,0.820,0.820,0.820,0.751, &
       0.467,0.663,0.820,0.820,0.820,0.820,0.820/)

      !attempt to use spec albedoes had poor results
      !shebavis = (/0.826,0.826,0.826,0.826,0.826,0.762,0.499, &
      ! 0.681,0.719,0.826,0.826,0.826,0.826,0.826/)
      !shebanir = (/0.809,0.809,0.809,0.809,0.809,0.731,0.411, &
      ! 0.632,0.678,0.809,0.809,0.809,0.809,0.809/)
      !Rotate albedoes by 6 months for S Hemis. -not a good idea
      !shem = (/0.751,0.467,0.663,0.820,0.820,0.820,0.820, &
      ! 0.820,0.820,0.820,0.820,0.820,0.751,0.467/)

      nday = (/31.,31.,29.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.,31./)

      call ESMF_TimeGet  ( currTime, TimeString=string  ,rc=STATUS ) ; VERIFY_(STATUS)
      read(string( 1: 4),'(i4.4)') YEAR
      read(string( 6: 7),'(i2.2)') MONTH
      read(string( 9:10),'(i2.2)') DAY

      if(mod(YEAR,4).eq.0) then
        nday(2)=29.
      else
        nday(2)=28.
      endif



      if(DAY.ge.15) then
        afracv=(float(DAY)-15.)/nday(MONTH)
        aslopev=(shebavis(MONTH+1)-shebavis(MONTH))/nday(MONTH)
        abasev=shebavis(MONTH)
        afraci=(float(DAY)-15.)/nday(MONTH)
        aslopei=(shebanir(MONTH+1)-shebanir(MONTH))/nday(MONTH)
        abasei=shebanir(MONTH)
      else
        afracv=(float(DAY)+nday(MONTH-1)-15.)/nday(MONTH-1)
        aslopev=(shebavis(MONTH)-shebavis(MONTH-1))/nday(MONTH-1)
        abasev=shebavis(MONTH-1)
        afraci=(float(DAY)+nday(MONTH-1)-15.)/nday(MONTH-1)
        aslopei=(shebanir(MONTH)-shebanir(MONTH-1))/nday(MONTH-1)
        abasei=shebanir(MONTH-1)
      endif

  
      SEAICEALBVRN=abasev+aslopev*afracv
      SEAICEALBVFN=abasev+aslopev*afracv
      SEAICEALBNRN=abasei+aslopei*afraci
      SEAICEALBNFN=abasei+aslopei*afraci

      SEAICEALBVRS=0.6
      SEAICEALBVFS=0.6
      SEAICEALBNRS=0.6
      SEAICEALBNFS=0.6

      where(LATS.ge.0.)
! Beam albedos
!-------------
        ALBVR = SEAICEALBVRN
        ALBNR = SEAICEALBNRN

! Diffuse albedos
!----------------
        ALBVF = SEAICEALBVFN
        ALBNF = SEAICEALBNFN
      elsewhere
! Beam albedos
!-------------
        ALBVR = SEAICEALBVRS
        ALBNR = SEAICEALBNRS

! Diffuse albedos
!----------------
        ALBVF = SEAICEALBVFS
        ALBNF = SEAICEALBNFS
      end where


   RETURN_(ESMF_SUCCESS)
  end subroutine ALBSEAICE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_PREP_THERMO - Initializes for CICE Thermodynamics

! !INTERFACE:

  subroutine CICE_PREP_THERMO(TF,TRCRTYPE,TRACERS,FRZMLT,MELTLN,FRAZLN,FRESHN,FRESHL,FSALTN,FSALTL,FHOCNN,FHOCNL,RSIDE,  &
                              FSWTHRU,FCOND,FCONDBOT,FSWTHRUWTR,TBOT,FBOT,ALBIN,ALBSN,ALBPND,ALBVRN,ALBNRN,ALBVFN,ALBNFN,FSWSFC,FSWINT,     &
                              ISWABS,SSWABS,FSWABS,MELTT,MELTS,MELTB,CONGEL,SNOICE,UW,VW,SLMASK,LATS,LONS,LATSO,LONSO,   &
                              FR ,FRCICE,SW,TAUAGE,ICE,WATER,NT,VOLPOND,DT,VOLICE,VOLSNO,ERGICE,ERGSNO,TS,VOLICE_DELTA, NEWICEERG, SBLX)
! not passing TFfresh

  use ice_constants,      only: depressT

! !ARGUMENTS:

    integer, intent(IN)   :: NT                 ! number of tiles
    integer, intent(IN)   :: ICE                ! integer number of 1st ice subtile(s)
    integer, intent(IN)   :: WATER              ! integer number of water subtile
    real,    intent(IN)   :: UW         (:)     ! u-current
    real,    intent(IN)   :: VW         (:)     ! v-current
    real,    intent(INOUT):: SLMASK     (:)     ! mask for cice- relating open water and sea-ice
    real,    intent(IN)   :: LATS       (:)     ! lat
    real,    intent(IN)   :: LONS       (:)     ! lon
    real,    intent(IN)   :: LATSO              ! trace CICE computations at this latitude
    real,    intent(IN)   :: LONSO              ! trace CICE computations at this longitude
    real,    intent(IN)   :: SW         (:)     ! Sea Water salinity

    real,    intent(OUT)  :: TF         (:)     ! Sea Water freezing temperature in degrees C
    integer, intent(OUT)  :: TRCRTYPE   (:)     ! CICE ice tracer type
    real,    intent(OUT)  :: TRACERS    (:,:)   ! CICE ice tracers
    real,    intent(OUT)  :: FRZMLT     (:)     ! ?
    real,    intent(OUT)  :: MELTLN     (:)     ! ?
    real,    intent(OUT)  :: FRAZLN     (:)     ! ?
    real,    intent(OUT)  :: FRESHN     (:)     ! ?
    real,    intent(OUT)  :: FRESHL     (:)     ! ?
    real,    intent(OUT)  :: FSALTN     (:)     ! ?
    real,    intent(OUT)  :: FSALTL     (:)     ! ?
    real,    intent(OUT)  :: FHOCNN     (:)     ! ?
    real,    intent(OUT)  :: FHOCNL     (:)     ! ?
    real,    intent(OUT)  :: RSIDE      (:)     ! ?
    real,    intent(OUT)  :: FSWTHRU    (:,:)   ! SW_flux_thru_ice_to_ocean 
    real,    intent(OUT)  :: FCOND      (:,:)   ! ?
    real,    intent(OUT)  :: FCONDBOT   (:,:)   ! ?
    real,    intent(OUT)  :: FSWTHRUWTR (:,:)   ! shortwave flux into water, passed thru ice. Bin, why FSWTHRU .ne. FSWTHRU?
    real,    intent(OUT)  :: TBOT       (:)     ! ?
    real,    intent(OUT)  :: FBOT       (:)     ! ?
    real,    intent(OUT)  :: ALBIN      (:,:)   !
    real,    intent(OUT)  :: ALBSN      (:,:)   !
    real,    intent(OUT)  :: ALBPND     (:,:)   !
    real,    intent(OUT)  :: ALBVRN     (:,:)   ! Albedos: 
    real,    intent(OUT)  :: ALBNRN     (:,:)   ! Albedos: near IR
    real,    intent(OUT)  :: ALBVFN     (:,:)   ! Albedos:
    real,    intent(OUT)  :: ALBNFN     (:,:)   ! Albedos: near IR
    real,    intent(OUT)  :: FSWSFC     (:,:)   ! ?
    real,    intent(OUT)  :: FSWINT     (:,:)   ! ?
    real,    intent(OUT)  :: ISWABS     (:,:,:) ! ?
    real,    intent(OUT)  :: SSWABS     (:,:,:) ! ?
    real,    intent(OUT)  :: FSWABS     (:)     ! of dimension(1)
    real,    intent(OUT)  :: MELTT      (:)     ! ?
    real,    intent(OUT)  :: MELTS      (:)     ! ?
    real,    intent(OUT)  :: MELTB      (:)     ! ?
    real,    intent(OUT)  :: CONGEL     (:)     ! ?
    real,    intent(OUT)  :: SNOICE     (:)     ! ?
    real,    intent(OUT)  :: NEWICEERG  (:)     ! ?
    real,    intent(OUT)  :: SBLX       (:)     ! ?
    real,    intent(INOUT):: TS         (:,:)   ! skin temperature

    real,    intent(IN)   :: DT
    real(kind=MAPL_R8)    :: DTDB               ! DT (time step) in R8 for CICE
    real(kind=MAPL_R8), intent(INOUT)  :: FRCICE       (:)
    real(kind=MAPL_R8), intent(INOUT)  :: FR           (:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: VOLICE       (:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: VOLSNO       (:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: VOLPOND      (:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: ERGICE       (:,:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: ERGSNO       (:,:,:)
    real(kind=MAPL_R8), intent(INOUT)  :: VOLICE_DELTA (:,:)
    real,               intent(INOUT)  :: TAUAGE       (:,:)

!  !LOCAL VARIABLES
  
    integer                               :: PRES_ICE, K
    logical                               :: L_STOP
    integer                               :: IDUM, JDUM
    logical,            dimension(1)      :: OBSERVE
    real,               dimension(1)      :: LATSD, LONSD
    real(kind=MAPL_R8), dimension(1)      :: FRWATERDB, FHOCNLDB, FRESHLDB, FSALTLDB, FRCICEDB

    real(kind=MAPL_R8), dimension(NUM_3D_ICE_TRACERS, NUM_ICE_CATEGORIES) :: TRACERSDB2

! !DESCRIPTION:

    DTDB = REAL(DT, kind=MAPL_R8)       ! Convert DT precision: Real4 to Real8 for usage in CICE

!   PRESCRIBED ICE. 1:AMIP mode, 0: coupled mode
    call MAPL_GetResource ( MAPL, PRES_ICE, Label="PRESCRIBED_ICE:" , DEFAULT=1,    RC=STATUS)
    VERIFY_(STATUS)

    if (PRES_ICE == 1) then
       TF = -1.8            ! constant freezing temp (C)     
    else
       TF = -depressT * SW  ! CICE default mode: TF = -depressT * SSS. depressT is CICE constant.
    endif

    TRCRTYPE(nt_tsfc)  = 0  ! ice/snow surface temperature
    TRCRTYPE(nt_iage)  = 1  ! volume-weighted ice age
    TRCRTYPE(nt_volpn) = 0  ! melt pond volume

    TRACERS            = 0.0      

    FRZMLT             = 0.0
    MELTLN             = 0.0
    FRAZLN             = 0.0
    FRESHN             = 0.0
    FRESHL             = 0.0
    FSALTN             = 0.0
    FSALTL             = 0.0
    FHOCNN             = 0.0
    FHOCNL             = 0.0
    RSIDE              = 0.0
    FSWTHRU            = 0.0
    FCOND              = 0.0
    FCONDBOT           = 0.0
    FSWTHRUWTR         = 0.0

    TBOT               = 0.0
    FBOT               = 0.0
    ALBIN              = 0.0
    ALBSN              = 0.0
    ALBPND             = 0.0
    ALBVRN             = 0.0
    ALBNRN             = 0.0
    ALBVFN             = 0.0
    ALBNFN             = 0.0
    FSWSFC             = 0.0  
    FSWINT             = 0.0
    ISWABS             = 0.0
    SSWABS             = 0.0
    FSWABS             = 0.0
    MELTT              = 0.0
    MELTS              = 0.0
    MELTB              = 0.0
    CONGEL             = 0.0  
    SNOICE             = 0.0
    NEWICEERG          = 0.0
    SBLX               = 0.0
    VOLICE_DELTA       = 0.0

! determine those tiles where there is no open ocean connection. See note for Atanas.
    where(abs(UW) >  0.0 .or. abs(VW) > 0.0) 
        SLMASK = 0.0
    elsewhere
        SLMASK = 1.0
    endwhere

! do a cleanup here, in case transformation from tripolar to tile induces round-off errors
    FRCICE = sum(FR (:,ICE:), dim=2)
    do k=1, NT
     
       call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)

       TRACERSDB2(nt_tsfc,:) = REAL(TS(K,ICE:)  - TFfresh, kind=MAPL_R8)
       TRACERSDB2(nt_iage,:) = REAL(TAUAGE(K,:),           kind=MAPL_R8)

       TRACERSDB2(nt_volpn,:)= VOLPOND(K,:)
       FRWATERDB             = FR (K,WATER)

       FHOCNLDB              = REAL(FHOCNL(K),             kind=MAPL_R8)
       FRESHLDB              = REAL(FRESHL(K),             kind=MAPL_R8)
       FSALTLDB              = REAL(FSALTL(K),             kind=MAPL_R8)
       FRCICEDB              = REAL(FRCICE(K),             kind=MAPL_R8)

       call cleanup_itd (1,1,1,1,1,1,DTDB, &
            FR (K,ICE:),   TRACERSDB2,     &     
            VOLICE(K,:),   VOLSNO(K,:),    &
            ERGICE(K,:,:), ERGSNO(K,:,:),  &
            FRWATERDB,     FRCICEDB,       &    
            TRCRTYPE,      FRESHLDB,       &
            FSALTLDB,      FHOCNLDB,       &    
            .true.,        L_STOP,         &    
            IDUM,            JDUM,         &    
            limit_aice_in=.true.)

       if(L_STOP) then
          print*, 'CICE_PREP_THERMO: Failing at LAT = ', LATSD, 'LON = ', LONSD
       endif

       ASSERT_(.not.L_STOP)

       FR (K,WATER) = REAL(FRWATERDB(1),                    kind=MAPL_R4)
       FRESHL(K)    = REAL(FRESHLDB(1),                     kind=MAPL_R4)
       FSALTL(K)    = REAL(FSALTLDB(1),                     kind=MAPL_R4)
       FHOCNL(K)    = REAL(FHOCNLDB(1),                     kind=MAPL_R4)

       TS(K,ICE:)   = REAL(TRACERSDB2(nt_tsfc,:) + TFfresh, kind=MAPL_R4)
       TAUAGE(K,:)  = REAL(TRACERSDB2(nt_iage,:),           kind=MAPL_R4)
       VOLPOND(K,:) = REAL(TRACERSDB2(nt_volpn,:),          kind=MAPL_R4)
    enddo

! freshwater, salt and heat flux accumulated previously is not counted
! because they are not **physical**  
    FRESHL = 0.0
    FHOCNL = 0.0
    FSALTL = 0.0

! these lines for fr are efectively same in cmip & amip modes. 
!*** FR(:,ICE:) returned from CICEDyna or Data Sea Ice
!*** update FRWATER accordingly 
    FRCICE      = sum(FR (:,ICE:), dim=2)
    FR (:,WATER)= max(1.0-FRCICE, 0.0)

    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_PREP_THERMO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_THERMO1 - Computes 1st step of CICE Thermodynamics

! !INTERFACE:

  subroutine CICE_THERMO1 (N,NSUB,NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,TF,FR,TS,HH,   &
                           ERGICE,ERGSNO,TAUXBOT,TAUYBOT,TBOT,ISWABS,SSWABS,             &
                           DO_POND,FRZMLT,FBOT,RSIDE,PCU,PLS,                            &
                           FSWTHRU,FCOND,FCONDBOT,EVP,FRESHN,FSALTN,FHOCNN,              &
                           MELTT,MELTS,MELTB,CONGEL,SNOICE,VOLICE,VOLSNO,SHF,LHF,        &
                           VOLPOND,APONDN,HPONDN,SALTWATERCAP,TAUAGE,TRACERS,ALW,BLW,    &
                           FSWSFC,FSWINT,FSWABS,LWDNSRF,EVD,SHD,SNO,SBLX)
! not passing TFfresh,saltwatercap,nt_tsfc,nt_iage,nt_volpn

! !ARGUMENTS:

    integer, intent(IN)  ::  N                 ! number of subtile type (or ice category)
    integer, intent(IN)  ::  NSUB              ! number of tiles
    integer, intent(IN)  ::  NT                ! number of tiles
    integer, intent(IN)  ::  ICE               ! subtiles number assigned to surface type: "ICE" 
    integer, intent(IN)   :: WATER             ! integer number of water subtile
    integer, intent(IN)  ::  DO_POND           ! doing computations for melt ponds explicitly?

    real,    intent(IN)  :: SALTWATERCAP
    real,    intent(IN)  :: LATS       (:)     ! lat
    real,    intent(IN)  :: LONS       (:)     ! lon
    real,    intent(IN)  :: LATSO              ! trace CICE computations at this latitude
    real,    intent(IN)  :: LONSO              ! trace CICE computations at this longitude

    real,    intent(IN)  :: TF         (:)     ! sea Water freezing temperature in degrees C
    real,    intent(IN)  :: HH         (:,:)   ! mass of skin layer
    real,    intent(IN)  :: TAUXBOT    (:)     ! zonal      stress at base of sea ice
    real,    intent(IN)  :: TAUYBOT    (:)     ! meridional stress at base of sea ice
    real,    intent(IN)  :: ISWABS     (:,:,:) ! ?
    real,    intent(IN)  :: SSWABS     (:,:,:) ! ?
    real,    intent(IN)  :: PCU        (:)     ! liquid water convective scale
    real,    intent(IN)  :: PLS        (:)     ! liquid water large      scale
    real,    intent(IN)  :: ALW        (:)     ! linearization of \sigma T^4
    real,    intent(IN)  :: BLW        (:)     ! linearization of \sigma T^4
    real,    intent(IN)  :: FSWSFC     (:,:)   ! ?
    real,    intent(IN)  :: FSWINT     (:,:)   ! ?
    real,    intent(IN)  :: FSWABS     (:)
    real,    intent(IN)  :: LWDNSRF    (:)     ! longwave at surface
    real,    intent(IN)  :: EVD        (:)     ! related to evap
    real,    intent(IN)  :: SHD        (:)     ! related to sensible heat 
    real,    intent(IN)  :: SNO        (:)     ! ?

    real,    intent(INOUT)  :: EVP     (:)     ! evaporation
    real,    intent(INOUT)  :: FRZMLT  (:)
    real,    intent(INOUT)  :: FBOT    (:)     ! ?
    real,    intent(INOUT)  :: RSIDE   (:)     ! ?
    real,    intent(INOUT)  :: FRESHN  (:)     ! ?
    real,    intent(INOUT)  :: FSALTN  (:)     ! ?
    real,    intent(INOUT)  :: FHOCNN  (:)     ! ?
    real,    intent(INOUT)  :: MELTT   (:)     ! ?
    real,    intent(INOUT)  :: MELTS   (:)     ! ?
    real,    intent(INOUT)  :: MELTB   (:)     ! ?
    real,    intent(INOUT)  :: CONGEL  (:)     ! ?
    real,    intent(INOUT)  :: SNOICE  (:)     ! ?
    real,    intent(INOUT)  :: SHF     (:)     ! sensible heat flux
    real,    intent(INOUT)  :: LHF     (:)     ! latent   heat flux
    real,    intent(INOUT)  :: TBOT    (:)     ! ?
    real,    intent(INOUT)  :: SBLX    (:)     ! ?
    real,    intent(INOUT)  :: TAUAGE  (:,:)   ! ?
    real,    intent(INOUT)  :: TRACERS (:,:)   ! ?
    real,    intent(INOUT)  :: TS      (:,:)   ! skin temperature

    real(kind=MAPL_R8),    intent(INOUT)  :: FR      (:,:)   ! fractions of water, ice types
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLICE  (:,:)   ! volume of ice
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLSNO  (:,:)   ! volume of snow
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLPOND (:,:)   ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: APONDN  (:,:)   ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: HPONDN  (:,:)   ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGICE  (:,:,:) ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGSNO  (:,:,:) ! ?

    real,    intent(INOUT)  :: FSWTHRU (:,:)   ! shortwave thru water, how can it be modified?
    real,    intent(INOUT)  :: FCOND   (:,:)   ! ?
    real,    intent(INOUT)  :: FCONDBOT(:,:)   ! ?

    real,    intent(IN)     :: DT 
    real(kind=MAPL_R8)      :: DTDB               ! DT (time step) in R8 for CICE

! !LOCAL VARIABLES

    integer   :: K
    logical   :: OBSERVE(1)                    ! could be (1,1) to match cice input
    integer   :: IDUM, JDUM
    logical   :: L_STOP
    real      :: FRZMLT_MAX
    real      :: LWUPSRF, DLHDT, DHLAT, DFSDT, LWUP0

    real,                    dimension(1)  :: LATSD, LONSD, FSURF, SHF0, LHF0
    !real,                    dimension(NT) :: FRZMLT
    character(len=ESMF_MAXSTR)             :: SHORTWAVE

    real(kind=MAPL_R8)                :: YDAYDB
    real(kind=MAPL_R8), dimension(1)  :: FRZMLTDB, TSCDB, TFDB, TAUXBOTDB, TAUYBOTDB, &
                                         TBOTDB, FBOTDB, RSIDEDB, FSURFDB, DLHDTDB,   &
                                         DFSDTDB, SHF0DB, LHF0DB, LWUP0DB, FSWSFCDB,  &
                                         FSWINTDB, LWDNSRFDB, SNODB, FSWABSDB,        &
                                         FSWTHRUDB, FCONDDB, FCONDBOTDB, EVPDB,       &
                                         FRESHNDB, FSALTNDB, FHOCNNDB,                &
                                         MELTTDB, MELTSDB, MELTBDB, CONGELDB,SNOICEDB,&
                                         DSHDB, BLWDB, LATSDB, LONSDB, MLT_ONSETDB,   &
                                         FRZ_ONSETDB, FRDB, VOLICEDB, VOLSNODB,       &
                                         SBLXDB,                                      & 
                                         APONDNDB, HPONDNDB, RDUMDB, FRAINDB           

    real(kind=MAPL_R8), dimension(NT)                  :: FRCICE
    real(kind=MAPL_R8), dimension(NUM_3D_ICE_TRACERS)  :: TRACERSDB
    real(kind=MAPL_R8), dimension(NUM_ICE_LAYERS)      :: ISWABSDB
    real(kind=MAPL_R8), dimension(NUM_SNOW_LAYERS)     :: SSWABSDB

!  !DESCRIPTION:
!        Compute update to TS, FR, SHF, LHF, ... 
!          based on CICE Thermodynamics

    DTDB = REAL(DT, kind=MAPL_R8)       ! Convert DT precision: Real4 to Real8 for usage in CICE

    call MAPL_GetResource ( MAPL, FRZMLT_MAX, Label="CICE_FRZMLT_MAX:" , DEFAULT=1000.,    RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, SHORTWAVE,  Label="CICE_SHORTWAVE:" ,  DEFAULT="shortwave_ccsm" , RC=STATUS)
    VERIFY_(STATUS)

! Loop over all tiles
!-----------------------

    TILES: do k=1, NT ! loop over all tiles

       call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)

       HAVE_ICE: if(FR(K,N) > puny) then

          TAUAGE(K,NSUB) = TAUAGE(K,NSUB) + DT

          TRACERS(nt_tsfc, NSUB) = TS(K,N) - TFfresh
          TRACERS(nt_iage, NSUB) = TAUAGE(K, NSUB)
          TRACERS(nt_volpn,NSUB) = VOLPOND(K,NSUB)

          LWUPSRF        = ALW(K) + BLW(K)*TS(K,N) ! use TS (in Kelvin) here
          FSURF(1)       = FSWSFC(K,NSUB) - SHF(K) - LHF(K) + LWDNSRF(K) - LWUPSRF
          DLHDT          = EVD(K) * MAPL_ALHS
          DHLAT          = EVD(K) * MAPL_ALHS
          DFSDT          = (SHD(K) + DHLAT + BLW(K))

          SHF0           = -SHF(K)
          LHF0           = -LHF(K)
          LWUP0          = -LWUPSRF
          FSWSFCDB       =  FSWSFC(K,NSUB)
          FSWINTDB       =  FSWINT(K,NSUB)
          FSURFDB(1)     =  FSWSFCDB(1) - SHF(K) - LHF(K) + LWDNSRF(K) - LWUPSRF
          DFSDTDB        =  DFSDT
          SHF0DB         = -SHF(K)
          LHF0DB         = -LHF(K)
          LWUP0DB        = -LWUPSRF

          TRACERSDB      =  TRACERS(:,NSUB)
          LWDNSRFDB      =  LWDNSRF(K)
          SNODB          =  SNO(K)
          TBOTDB         =  TBOT(K)
          FBOTDB         =  FBOT(K)
          FSWABSDB       =  FSWABS
          FSWTHRUDB      =  FSWTHRU(K,N-1)
          ISWABSDB       =  ISWABS(K,:,NSUB)
          SSWABSDB       =  SSWABS(K,:,NSUB)
          FCONDDB        =  FCOND(K,NSUB)
          FCONDBOTDB     =  FCONDBOT(K,NSUB)
          EVPDB          =  EVP(K)
          FRESHNDB       =  FRESHN(K)
          FSALTNDB       =  FSALTN(K)
          FHOCNNDB       =  FHOCNN(K)
          MELTTDB        =  MELTT(K)
          MELTSDB        =  MELTS(K)
          MELTBDB        =  MELTB(K)
          CONGELDB       =  CONGEL(K)
          SNOICEDB       =  SNOICE(K)
          DSHDB          =  SHD(K)
          DLHDTDB        =  DLHDT
          BLWDB          =  BLW(K)
          LATSDB         =  LATSD
          LONSDB         =  LONSD
          MLT_ONSETDB    =  0.0
          FRZ_ONSETDB    =  0.0
          YDAYDB         =  0.0
          SBLXDB         =  0.0
          FRDB           =  FR(K,N)
          VOLICEDB       = VOLICE(K,NSUB)
          VOLSNODB       = VOLSNO(K,NSUB)

          call thermo_vertical(                &
               1,1,DTDB,1,(/1/),(/1/),         &
               FRDB,                           &
               TRACERSDB,                      &
               VOLICEDB,     VOLSNODB,         &
               ERGICE(K,:,NSUB),   ERGSNO(K,:,NSUB),   &
               LWDNSRFDB,     RDUMDB,          &
               RDUMDB,        RDUMDB,          &
               SNODB,                          &
               FBOTDB,        TBOTDB,          &
               RDUMDB,        RDUMDB,          &
               FSWSFCDB,      FSWINTDB,        &
               FSWTHRUDB,                      &
               SSWABSDB,                       &
               ISWABSDB,                       &
  
               FSURFDB,       FCONDDB,         &
               SHF0DB,        LHF0DB,          &
               FSWABSDB,      LWUP0DB,         &
               EVPDB,         FRESHNDB,        &
               FSALTNDB,      FHOCNNDB,        &
               MELTTDB,       MELTSDB,         &
               MELTBDB,                        &
               CONGELDB,      SNOICEDB,        &

               DFSDTDB,-DSHDB,-DLHDTDB,-BLWDB, &
               LATSDB, LONSDB, OBSERVE,        &
               FCONDBOTDB,    SBLXDB,          &

               MLT_ONSETDB,   FRZ_ONSETDB,     &
               YDAYDB,          L_STOP,        &
               IDUM,          JDUM             )

          if(L_STOP) then
             print*, 'CICE_THERMO1: Failing at LAT = ', LATSD, 'LON = ', LONSD
          endif

          ASSERT_(.not.L_STOP)

          SHF0               =  SHF0DB
          LHF0               =  LHF0DB
          TRACERS(:, NSUB)   =  TRACERSDB
          FSWTHRU(K,N-1)     =  FSWTHRUDB(1)
          FCOND(K,NSUB)      =  FCONDDB(1)
          FCONDBOT(K,NSUB)   =  FCONDBOTDB(1)
          !*** EVP computed by CICE has an opposite sign:
          !*** condensation > 0, water vapor goes down
          !*** sublimation  < 0, water vapor goes up
          EVP(K)             =  -EVPDB(1)
          SBLX(K)            =  SBLXDB(1)
          FRESHN(K)          =  FRESHNDB(1)
          FSALTN(K)          =  FSALTNDB(1)
          FHOCNN(K)          =  FHOCNNDB(1)
          MELTT(K)           =  MELTTDB(1)
          MELTS(K)           =  MELTSDB(1)
          MELTB(K)           =  MELTBDB(1)
          CONGEL(K)          =  CONGELDB(1)
          SNOICE(K)          =  SNOICEDB(1)
          FR(K,N)            =  FRDB(1)
          VOLICE(K,NSUB)     =  VOLICEDB(1)
          VOLSNO(K,NSUB)     =  VOLSNODB(1)
          ! need to update these for aggregation later
          SHF(K)             = -SHF0(1)
          LHF(K)             = -LHF0(1)

          TS(K,N)            =  TRACERS(nt_tsfc,NSUB) + TFfresh
          TAUAGE(K,NSUB)     =  TRACERS(nt_iage,NSUB)

          if ( (DO_POND==1) .and. trim(SHORTWAVE) == 'dEdd') then
             MELTTDB         =  MELTT(K)
             MELTSDB         =  MELTS(K)
             FRDB            =  FR(K,N)
             VOLICEDB        =  VOLICE(K,NSUB)
             VOLSNODB        =  VOLSNO(K,NSUB)
             APONDNDB        =  APONDN(K,NSUB)
             HPONDNDB        =  HPONDN(K,NSUB)
             FRAINDB         =  PCU(K) + PLS(K)
             call compute_ponds(1, 1,                      &
                           1, 1, 1, 1,                     &
                           MELTTDB, MELTSDB, FRAINDB,      &
                           FRDB, VOLICEDB,                 &
                           VOLSNODB, TRACERSDB,            &
                           APONDNDB, HPONDNDB)
             TRACERS(:,NSUB) = TRACERSDB
             VOLPOND(K,NSUB) = TRACERS(nt_volpn,NSUB)
             APONDN (K,NSUB) = APONDNDB(1)
             HPONDN (K,NSUB) = HPONDNDB(1)
          endif

       end if HAVE_ICE
    end do TILES ! K loop

    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_THERMO1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_THERMO2_STEP1 - Computes part 1 of-- 2nd update of LANL CICE Thermodynamics

! !INTERFACE:

  subroutine CICE_THERMO2_STEP1 (NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,TF,FR,TS,     &
                                 VOLICE,VOLSNO,VOLPOND,ERGICE,ERGSNO,                    &
                                 AICENINIT,VICENINIT,TRCRTYPE,FRCICE,FRZMLT,FRAZLN,      &
                                 FRESHL,FSALTL,FHOCNL,RSIDE,MELTLN,VOLICE_DELTA,         &
                                 TRACERS,TAUAGE,SNOICE,HH,SS,HW,SW)
! not passing TFfresh,saltwatercap,nt_tsfc,nt_iage,nt_volpn,tauage

! !ARGUMENTS:

    integer, intent(IN)     :: NT               ! number of tiles
    integer, intent(IN)     :: ICE              ! subtiles number assigned to surface type: "ICE" 
    integer, intent(IN)     :: WATER            ! integer number of water subtile

    real,    intent(IN)     :: LATS     (:)     ! lat
    real,    intent(IN)     :: LONS     (:)     ! lon
    real,    intent(IN)     :: LATSO            ! trace LANL CICE computations at this latitude
    real,    intent(IN)     :: LONSO            ! trace LANL CICE computations at this longitude
    real,    intent(IN)     :: TF       (:)     ! sea Water freezing temperature in degrees C



    real,    intent(INOUT)  :: HH       (:,:)   ! ?
    real,    intent(INOUT)  :: SS       (:,:)   ! ?
    real,    intent(INOUT)  :: HW       (:)     ! ?
    real,    intent(INOUT)  :: SW       (:)     ! ?

    real,    intent(INOUT)  :: TS       (:,:)   ! skin temperature
    real,    intent(INOUT)  :: TRACERS  (:,:)   ! ?
    real,    intent(INOUT)  :: TAUAGE   (:,:)    ! ?

    integer, intent(INOUT)  :: TRCRTYPE (:)     ! ?
    real,    intent(INOUT)  :: FRZMLT   (:)     ! ?
    real,    intent(INOUT)  :: FRAZLN   (:)     ! ?
    real,    intent(INOUT)  :: FRESHL   (:)     ! ?
    real,    intent(INOUT)  :: FSALTL   (:)     ! ?
    real,    intent(INOUT)  :: FHOCNL   (:)     ! ?
    real,    intent(INOUT)  :: RSIDE    (:)     ! ?
    real,    intent(INOUT)  :: MELTLN   (:)     ! ?
    real,    intent(INOUT)  :: SNOICE   (:)     ! ?

    real,    intent(IN)     :: DT
    real(kind=MAPL_R8)      :: DTDB             ! DT (time step) in R8 for CICE
    real(kind=MAPL_R8),    intent(INOUT)  :: FR       (:,:)   ! fractions of water, ice types
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLICE   (:,:)   ! volume of ice
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLSNO   (:,:)   ! volume of snow
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLPOND  (:,:)   ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGICE   (:,:,:) ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGSNO   (:,:,:) ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: AICENINIT(:,:)   ! initial (after cice_init_thermo) ice concentration
    real(kind=MAPL_R8),    intent(INOUT)  :: VICENINIT(:,:)   ! initial (after cice_init_thermo) volume of ice
    real(kind=MAPL_R8),    intent(INOUT)  :: FRCICE   (:)     ! fraction of ice, surface type
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLICE_DELTA  (:,:)! change in volume of ice

! !LOCAL VARIABLES

    integer                               :: K
    logical                               :: OBSERVE(1)        ! could be (1,1) to match cice input
    real,               dimension(1)      :: LATSD, LONSD
    logical                               :: L_STOP
    integer                               :: IDUM, JDUM
    real                                  :: MINSWFRESH

    real(kind=MAPL_R8)                    :: YDAYDB
    real(kind=MAPL_R8), dimension(1)      :: FRESH0, FSALT0
    real(kind=MAPL_R8), dimension(1)      :: FRWATERDB, FRZMLTDB, FRAZLNDB, FRESHLDB, FSALTLDB, TFDB, &
                                             RDUMDB, MELTLNDB, FHOCNLDB, RSIDEDB, SNOICEDB, FRCICEDB

    real(kind=MAPL_R8), dimension(NUM_3D_ICE_TRACERS, NUM_ICE_CATEGORIES) :: TRACERSDB2


!  !DESCRIPTION:
!        Compute ...??
!          based on CICE

    DTDB = REAL(DT, kind=MAPL_R8)       ! Convert DT precision: Real4 to Real8 for usage in CICE
    call MAPL_GetResource ( MAPL, MINSWFRESH, Label="FRESH_NEW_ICE_MIN_SALINITY:" , DEFAULT=5.0,    RC=STATUS)
    VERIFY_(STATUS)

! Loop over all tiles
!-----------------------

    TILES_1: do k=1, NT ! loop over all tiles

       call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)

       TRACERS(nt_tsfc,:) = TS(K,ICE:) - TFfresh
       TRACERS(nt_iage,:) = TAUAGE(K,:)
       TRACERS(nt_volpn,:)= VOLPOND(K,:)
       TRACERSDB2         = TRACERS
       FRCICEDB           = REAL(FRCICE(K),             kind=MAPL_R8)

       if(FRCICE(K) > 0.0) then 
          FRWATERDB  =  FR(K,WATER)
          call linear_itd (1,1,1,(/1/),(/1/), &
                           TRCRTYPE,          &    
                           AICENINIT(K,:),    &
                           VICENINIT(K,:),    &
                           FR(K,ICE:),        &    
                           TRACERSDB2,        &    
                           VOLICE(K,:),  VOLSNO(K,:), & 
                           ERGICE(K,:,:),     &    
                           ERGSNO(K,:,:),     &    
                           FRCICEDB,          &    
                           FRWATERDB,         &    
                           LATSD, LONSD,      &  
                           L_STOP,            &    
                           IDUM, JDUM )

          if(L_STOP) then
             print*, 'CICE_THERMO2_STEP1: after linear_itd. Failing at LAT = ', LATSD, 'LON = ', LONSD
          endif

          ASSERT_(.not.L_STOP)

          FR(K,WATER) =  FRWATERDB(1)    
       endif 
       
       FRZMLTDB       =  FRZMLT(K)
       FRAZLNDB       =  FRAZLN(K)
       TFDB           =  TF(K)
       RDUMDB         =  0.0
       YDAYDB         =  0.0
       FRWATERDB      =  FR(K,WATER)
       FRESH0(1)      =  REAL(0.0, kind=MAPL_R8)  
       FSALT0(1)      =  REAL(0.0, kind=MAPL_R8)  

       call add_new_ice (1,1,1,(/1/),(/1/),(/.true./), DTDB, &
                         FR(K,ICE:),                         &    
                         TRACERSDB2,                         &    
                         VOLICE(K,:),                        &
                         ERGICE(K,:,:),                      &
                         FRWATERDB,                          &    
                         FRCICEDB ,                          &    
                         FRZMLTDB,                           &    
                         FRAZLNDB,                           &    
                         SW(K) < MINSWFRESH,                 &
                         RDUMDB, YDAYDB,                     &    
                         FRESH0,                             &    
                         FSALT0,                             &    
                         TFDB, L_STOP,                       &
                         IDUM, JDUM)
       if(L_STOP) then
          print*, 'CICE_THERMO2_STEP1: after add_new_ice. Failing at LAT = ', LATSD, 'LON = ', LONSD
       endif

       ASSERT_(.not.L_STOP)

       FRAZLN(K)   =  FRAZLNDB (1)     
       FR(K,WATER) =  FRWATERDB(1)    
       HH(K,WATER) =  HH(K,WATER) + DT*FRESH0(1)
       HW(K)       =  HH(K,WATER)
       SS(K,WATER) =  SS(K,WATER) + DT*1.e3*FSALT0(1)
       SW(K)       =  SS(K,WATER)/HH(K,WATER)

       !if(FRZMLT(K) > 0.) NEWICEERG(K) = NEWICEERG(K) - FRZMLT(K)

       VOLICE_DELTA(k,:)  =  VOLICE(K,:)
       FHOCNLDB      =  FHOCNL(K)
       FRESHLDB      =  FRESHL(K)
       FSALTLDB      =  FSALTL(K)
       RSIDEDB       =  RSIDE(K)
       MELTLNDB      =  MELTLN(K)

       call lateral_melt (1,1,1,1,1,1,DTDB, &
                          FRESHLDB,         &
                          FSALTLDB,         &
                          FHOCNLDB,         &
                          RSIDEDB,          &
                          MELTLNDB,         &
                          FR(K,ICE:),       &
                          VOLICE(K,:),      &
                          VOLSNO(k,:),      &
                          ERGICE(K,:,:),    &
                          ERGSNO(K,:,:) )

       VOLICE_DELTA(k,:)  = VOLICE_DELTA(k,:) - VOLICE(K,:)
       SNOICEDB      = 0.0

       call freeboard_ccsm (1,1,1,1,1,1, DTDB, &
                            FR(K,ICE:),  &
                            VOLICE(K,:),  VOLSNO(K,:),   &
                            ERGICE(K,:,:), &
                            ERGSNO(K,:,:), &
                            SNOICEDB,   &
                            FSALTLDB)         
                                 
       SNOICE(K) = SNOICEDB(1)
       FRWATERDB = FR(K,WATER)

       call cleanup_itd (1,1,1,1,1,1,DTDB, &
                         FR(K,ICE:),       &
                         TRACERSDB2,       &
                         VOLICE(K,:),      &
                         VOLSNO(K,:),      &
                         ERGICE(K,:,:),    &
                         ERGSNO(K,:,:),    &
                         FRWATERDB,        &
                         FRCICEDB ,        &    
                         TRCRTYPE,         &    
                         FRESHLDB,         &
                         FSALTLDB,         &    
                         FHOCNLDB,         &    
                         .true., L_STOP,   &    
                         IDUM, JDUM,       &    
                         limit_aice_in=.true.)

       ASSERT_(.not.L_STOP)

       TRACERS       = TRACERSDB2
       FR(K,WATER)   = FRWATERDB(1)    
       FRESHL(K)     = FRESHLDB (1)     
       FSALTL(K)     = FSALTLDB (1)     
       FHOCNL(K)     = FHOCNLDB (1)     
       MELTLN(K)     = MELTLNDB (1)     

       TS(K,ICE:)    = TRACERS(nt_tsfc,:) + TFfresh
       TAUAGE(K,:)   = TRACERS(nt_iage,:) 
       VOLPOND(K,:)  = TRACERS(nt_volpn,:)

    end do TILES_1 ! K loop


    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_THERMO2_STEP1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_THERMO2_STEP2 - Computes 2st step of CICE Thermodynamics

! !INTERFACE:

  subroutine CICE_THERMO2_STEP2 (NT,ICE,WATER,LATS,LONS,LATSO,LONSO,DT,FR,TS,                &
                                 VOLICE,VOLSNO,VOLPOND,ERGICE,ERGSNO,                          &
                                 TRCRTYPE,FRCICE,SLMASK,TRACERS,TAUAGE)
! not passing TFfresh,puny,tauage,tracers

! !ARGUMENTS:

    integer, intent(IN)     :: NT               ! number of tiles
    integer, intent(IN)     :: ICE              ! subtiles number assigned to surface type: "ICE" 
    integer, intent(IN)     :: WATER            ! subtile  number assigned to surface type: "WATER" 

    real,    intent(IN)     :: LATS     (:)     ! lat
    real,    intent(IN)     :: LONS     (:)     ! lon
    real,    intent(IN)     :: LATSO            ! trace CICE computations at this latitude
    real,    intent(IN)     :: LONSO            ! trace CICE computations at this longitude
    real,    intent(IN)     :: SLMASK   (:)     ! "salt water lake mask"

    real,    intent(INOUT)  :: TS       (:,:)   ! skin temperature
    real,    intent(INOUT)  :: TRACERS  (:,:)   ! ?
    real,    intent(INOUT)  :: TAUAGE   (:,:)   ! ?

    integer, intent(INOUT)  :: TRCRTYPE (:)     ! ?

    real,    intent(IN)     :: DT
    real(kind=MAPL_R8)      :: DTDB             ! DT (time step) in R8 for CICE
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLICE   (:,:)   ! volume of ice
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLSNO   (:,:)   ! volume of snow
    real(kind=MAPL_R8),    intent(INOUT)  :: FR       (:,:)   ! fractions of water, ice types
    real(kind=MAPL_R8),    intent(INOUT)  :: FRCICE   (:)     ! fraction of ice, surface type
    real(kind=MAPL_R8),    intent(INOUT)  :: VOLPOND  (:,:)   ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGICE   (:,:,:) ! ?
    real(kind=MAPL_R8),    intent(INOUT)  :: ERGSNO   (:,:,:) ! ?

! !LOCAL VARIABLES

    logical                :: OBSERVE(1)        ! could be (1,1) to match cice input
    real,  dimension(1)    :: LATSD, LONSD
    logical                :: L_STOP
    integer                :: IDUM, JDUM
    integer                :: k,l,n
    real                   :: ICE_THICKNESS_THRESH
    real                   :: ICE_ARTIFICIAL_MELT

    real(kind=MAPL_R8)     :: hid, hi, hs, hsn

    real(kind=MAPL_R8)     :: qin_save   (NUM_ICE_CATEGORIES, NUM_ICE_LAYERS)
    real(kind=MAPL_R8)     :: qsn_save   (NUM_ICE_CATEGORIES, NUM_SNOW_LAYERS)
    real(kind=MAPL_R8)     :: TRACERSDB2 (NUM_3D_ICE_TRACERS, NUM_ICE_CATEGORIES)

    real(kind=MAPL_R8), dimension(1)  :: FRWATERDB, FRESHLDB, FSALTLDB, FHOCNLDB, FRCICEDB

!  !DESCRIPTION:
!        Compute ...??
!          based on CICE
    
    DTDB = REAL(DT, kind=MAPL_R8)       ! Convert DT precision: Real4 to Real8 for usage in CICE

    call MAPL_GetResource ( MAPL, ICE_THICKNESS_THRESH, Label="CICE_ICE_THICKNESS_THRESH:", DEFAULT=1.5, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ICE_ARTIFICIAL_MELT,  Label="CICE_ICE_ARTIFICIAL_MELT:" , DEFAULT=0.1, RC=STATUS)
    VERIFY_(STATUS)
    ! the units of ICE_ARTIFICIAL_MEL are cm/day and it is converted to m/time step 
    hid = real(ICE_ARTIFICIAL_MELT*1.e-2*DT/86400.0, kind=8)

! Loop over all tiles
!-----------------------

    TILES_2: do k=1, NT ! loop over all tiles

       FRCICEDB = REAL(FRCICE(k),             kind=MAPL_R8)

       if( (SLMASK(K) > 0.5) .and. (FRCICE(K) > 0.0) .and. &
           (sum(VOLICE(K,:)) > ICE_THICKNESS_THRESH)) then

          call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)

          qin_save(:,:) = 0.0
          qsn_save(:,:) = 0.0

          loop_over_ice_cat: do n=1, NUM_ICE_CATEGORIES
             hi = 0.0
             hs = 0.0
             if(VOLICE(k,n) > puny) then

                hi = VOLICE(k,n) / FR(k,n+1)
                do l=1,NUM_ICE_LAYERS
                   qin_save(n,l) = ERGICE(k,l,n)*REAL(NUM_ICE_LAYERS,kind=MAPL_R8)/VOLICE(k,n)
                enddo

                if(VOLSNO(k,n) > puny) then
                   hs = VOLSNO(k,n) / FR(k,n+1)
                   do l=1,NUM_SNOW_LAYERS
                      qsn_save(n,l) = ERGSNO(k,l,n)*REAL(NUM_SNOW_LAYERS,kind=MAPL_R8)/VOLSNO(k,n)
                   enddo
                endif

                if(hi > hid) hi = hi - hid
                hsn = (MAPL_RHO_SEAWATER-MAPL_RHO_SEAICE)/MAPL_RHO_SNOW *hi
                if(hs > hsn) hs = hsn

                VOLICE(k,n) = hi * FR(k,n+1)
                VOLSNO(k,n) = hs * FR(k,n+1)

                do l=1,NUM_ICE_LAYERS
                   ERGICE(k,l,n) = qin_save(n,l)*VOLICE(k,n)/real(NUM_ICE_LAYERS,kind=MAPL_R8)
                enddo

                if(VOLSNO(k,n) > puny) then
                   do l=1,NUM_SNOW_LAYERS
                         ERGSNO(k,l,n) = qsn_save(n,l)*VOLSNO(k,n)/real(NUM_SNOW_LAYERS,kind=MAPL_R8)
                   enddo
                endif
             endif
          enddo loop_over_ice_cat

          TRACERS(nt_tsfc,:) = TS(K,ICE:)-TFfresh
          TRACERS(nt_iage,:) = TAUAGE(K,:)
          TRACERS(nt_volpn,:)= VOLPOND(K,:)

          FRWATERDB          = FR(K,WATER)
          TRACERSDB2         = TRACERS

          call cleanup_itd (1,1,1,1,1,1,DTDB, &
                            FR(K,ICE:),       &
                            TRACERSDB2,       &
                            VOLICE(K,:),      &
                            VOLSNO(K,:),      &
                            ERGICE(K,:,:),    &
                            ERGSNO(K,:,:),    &
                            FRWATERDB,        &
                            FRCICEDB,         &
                            TRCRTYPE,         &
                            FRESHLDB,         &
                            FSALTLDB,         &
                            FHOCNLDB,         &
                            .true., L_STOP,   &
                            IDUM, JDUM,       &
                            limit_aice_in=.true.)

          if(L_STOP) then
             print*, 'CICE_THERMO2_STEP2: Failing at LAT = ', LATSD, 'LON = ', LONSD
          endif

          ASSERT_(.not.L_STOP)

          FR(K,WATER)  = FRWATERDB(1)
          TRACERS      = TRACERSDB2

          TS(K,ICE:)   = TRACERS(nt_tsfc,:) + TFfresh
          TAUAGE(K,:)  = TRACERS(nt_iage,:)
          VOLPOND(K,:) = TRACERS(nt_volpn,:)
       endif
    end do TILES_2 ! K loop


    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_THERMO2_STEP2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_ALBSEAICE - Computes albedos using LANL CICE 

! !INTERFACE:

  subroutine CICE_ALBSEAICE (ICE,NUM_ICE_CATEGORIES,NUM_ICE_LAYERS,NUM_SNOW_LAYERS,NT,DO_POND,LATSO,LONSO,LATS,LONS,ZTH,FR,TS, &
                             DRPAR,DFPAR,DRNIR,DFNIR,DRUVR,DFUVR,VSUVR,VSUVF,VOLICE,VOLSNO,APONDN,HPONDN,                      &
                             ISWABS,FSWSFC, FSWINT,FSWTHRU,SSWABS,ALBIN,ALBSN,ALBPND,ALBVRN,ALBVFN,ALBNRN,ALBNFN,              &
                             DRUVRTHRU,DFUVRTHRU,DRPARTHRU,DFPARTHRU)

  use ice_shortwave,      only: shortwave_ccsm3, shortwave_dEdd_set_snow, &
                                shortwave_dEdd_set_pond,                  &
                                shortwave_dEdd

! !ARGUMENTS:

    integer, intent(IN)  :: ICE                  ! starting index of ICE tiles
    integer, intent(IN)  :: NUM_ICE_CATEGORIES   ! # of ice categories
    integer, intent(IN)  :: NUM_ICE_LAYERS       ! # of ice  layers
    integer, intent(IN)  :: NUM_SNOW_LAYERS      ! # of snow layers
    integer, intent(IN)  :: NT                   ! # tiles in each type
    integer, intent(IN)  :: DO_POND              ! cice_do_pond resource parameter
    
    real                 :: LATSO                ! trace cice computations at Lat: LATSO
    real                 :: LONSO                ! trace cice computations at Lon: LONSO

    real,    intent(IN)  :: LATS(:)              ! latitudes
    real,    intent(IN)  :: LONS(:)              ! longitudes
    real,    intent(IN)  :: ZTH (:)              ! cosine of solar zenith angle
    real,    intent(IN)  :: TS (:,:)             ! Skin temperature
    real,    intent(IN)  :: DRPAR (:)
    real,    intent(IN)  :: DFPAR (:)
    real,    intent(IN)  :: DRNIR (:)
    real,    intent(IN)  :: DFNIR (:)
    real,    intent(IN)  :: DRUVR (:)
    real,    intent(IN)  :: DFUVR (:)
    real,    intent(IN)  :: VSUVR (:)
    real,    intent(IN)  :: VSUVF (:)

    real,    intent(OUT)  :: ISWABS (:,:,:)
    real,    intent(OUT)  :: SSWABS (:,:,:)
    real,    intent(OUT)  :: FSWSFC (:,:)
    real,    intent(OUT)  :: FSWINT (:,:)
    real,    intent(OUT)  :: FSWTHRU(:,:)

    real,    intent(INOUT)  :: ALBIN (:,:)
    real,    intent(INOUT)  :: ALBSN (:,:)
    real,    intent(INOUT)  :: ALBPND(:,:)

    real,    intent(OUT)  :: ALBVRN (:,:)       ! visible direct  albedo 
    real,    intent(OUT)  :: ALBVFN (:,:)       ! visible diffuse albedo
    real,    intent(OUT)  :: ALBNRN (:,:)       ! nearIr  direct  albedo
    real,    intent(OUT)  :: ALBNFN (:,:)       ! nearIr  diffuse albedo

    real,    intent(OUT)  :: DRUVRTHRU (:,:)    ! direct  UV  ??
    real,    intent(OUT)  :: DFUVRTHRU (:,:)    ! diffuse UV  ??
    real,    intent(OUT)  :: DRPARTHRU (:,:)    ! direct  PAR ??
    real,    intent(OUT)  :: DFPARTHRU (:,:)    ! diffuse PAR ??

    real(kind=MAPL_R8),    intent(IN)  :: FR     (:,:)             ! Fraction of ice in each category
    real(kind=MAPL_R8),    intent(IN)  :: VOLICE (:,:)
    real(kind=MAPL_R8),    intent(IN)  :: VOLSNO (:,:)
    real(kind=MAPL_R8),    intent(IN)  :: APONDN (:,:)
    real(kind=MAPL_R8),    intent(IN)  :: HPONDN (:,:)

!  !LOCAL VARIABLES
    
    integer               :: N, NSUB, K

    real(kind=MAPL_R8), dimension(NUM_ICE_LAYERS)  :: ISWABSDB
    real(kind=MAPL_R8), dimension(NUM_SNOW_LAYERS) :: SSWABSDB
    real(kind=MAPL_R8), dimension(NUM_SNOW_LAYERS) :: RHOSNWN
    real(kind=MAPL_R8), dimension(NUM_SNOW_LAYERS) :: RSNWN

    real(kind=MAPL_R8), dimension(1) ::                                 &
                                           TSCDB,                       &    
                                           DRPARDB,       DFPARDB,      &
                                           DRNIRDB,       DFNIRDB,      &
                                           DRUVRDB,       DFUVRDB,      &
                                           VSUVRDB,       VSUVFDB,      &
                                           ALBVRNDB,      ALBNRNDB,     &
                                           ALBVFNDB,      ALBNFNDB,     &
                                           FSWSFCDB,      FSWINTDB,     &
                                           FSWTHRUDB,                   &
                                           ALBINDB,       ALBSNDB,      &
                                           ALBPNDDB,                    &
                                           FRDB,                        & 
                                           VOLICEDB,      VOLSNODB,     &
                                           DRUVRTHRUDB,   DFUVRTHRUDB,  &
                                           DRPARTHRUDB,   DFPARTHRUDB,  &
                                           COSZTH,        FSN,          &
                                           FPN,           HPN            

    real,               dimension(1)      :: LATSD, LONSD
    logical,            dimension(1)      :: OBSERVE           ! could be (1,1) to match cice input
    logical                               :: TR_POND
    character(len=ESMF_MAXSTR)            :: SHORTWAVE
    
!  !DESCRIPTION:
!        Compute albedo over sea-ice using: Delta-Eddington or CCSM3 (default)
!          based on CICE

    call MAPL_GetResource ( MAPL, SHORTWAVE, Label="CICE_SHORTWAVE:" , DEFAULT="shortwave_ccsm" , RC=STATUS)
    VERIFY_(STATUS)

    if (DO_POND == 1) then 
       TR_POND = .true.
    else 
       TR_POND = .false.
    endif 

!  Initialize output 
!-------------------

    ALBVRN     = 0.0
    ALBNRN     = 0.0
    ALBVFN     = 0.0
    ALBNFN     = 0.0
 
    DRUVRTHRU  = 0.0
    DFUVRTHRU  = 0.0
    DRPARTHRU  = 0.0
    DFPARTHRU  = 0.0

! Loop over all subtiles
!-----------------------

    EVERY_SUBTILE: do N=1, NUM_ICE_CATEGORIES
       EVERY_TILE: do K=1, NT

         call CICE_INQUIRE_TILE(LATS(K), LONS(K), LATSO, LONSO, OBSERVE, LATSD, LONSD)

         if(FR(K,N+1) > puny) then

             TSCDB      =  REAL(TS(K,N+1)  - TFfresh, kind=MAPL_R8)  ! Convert Inputs precision: Real4 to Real8
             DRPARDB    =  REAL(DRPAR(K),             kind=MAPL_R8)
             DFPARDB    =  REAL(DFPAR(K),             kind=MAPL_R8)
             DRNIRDB    =  REAL(DRNIR(K),             kind=MAPL_R8)
             DFNIRDB    =  REAL(DFNIR(K),             kind=MAPL_R8)
             DRUVRDB    =  REAL(DRUVR(K),             kind=MAPL_R8)
             DFUVRDB    =  REAL(DFUVR(K),             kind=MAPL_R8)
             VSUVRDB    =  REAL(VSUVR(K),             kind=MAPL_R8)
             VSUVFDB    =  REAL(VSUVF(K),             kind=MAPL_R8)

             ALBVRNDB   =  REAL(ALBVRN(K,N),          kind=MAPL_R8)        ! Initialize R8 (Double Precision) outputs
             ALBNRNDB   =  REAL(ALBNRN(K,N),          kind=MAPL_R8)
             ALBVFNDB   =  REAL(ALBVFN(K,N),          kind=MAPL_R8)
             ALBNFNDB   =  REAL(ALBNFN(K,N),          kind=MAPL_R8)

             FSWSFCDB   =  REAL(FSWSFC(K,N),          kind=MAPL_R8) 
             FSWINTDB   =  REAL(FSWINT(K,N),          kind=MAPL_R8)
             FSWTHRUDB  =  REAL(FSWTHRU(K,N),         kind=MAPL_R8)
             ISWABSDB   =  REAL(ISWABS(K,:,N),        kind=MAPL_R8)

             ALBINDB    =  REAL(ALBIN(K,N),           kind=MAPL_R8)
             ALBSNDB    =  REAL(ALBSN(K,N),           kind=MAPL_R8)
             ALBPNDDB   =  REAL(ALBPND(K,N),          kind=MAPL_R8)

             FRDB       =  REAL(FR(K,N+1),            kind=MAPL_R8)

             VOLICEDB   =  REAL(VOLICE(K,N),          kind=MAPL_R8)
             VOLSNODB   =  REAL(VOLSNO(K,N),          kind=MAPL_R8)

             if (trim(SHORTWAVE) == 'dEdd') then 
                SSWABSDB   =  SSWABS(K,:,N)
                COSZTH     =  ZTH(K)                         !*** ZTH is actually cos() of solar zenith angle
                call shortwave_dEdd_set_snow(1, 1,        &  ! set snow properties
                              1, (/1/),(/1/),             &    
                              FRDB,     VOLSNODB,         &    
                              TSCDB,    FSN,              &    
                              RHOSNWN,  RSNWN)
                if (.not. TR_POND) then 
                   call shortwave_dEdd_set_pond(1, 1,     &  ! set pond properties
                                 1,  (/1/),(/1/),         &    
                                 FRDB,   TSCDB,           &    
                                 FSN,    FPN,             &    
                                 HPN) 
                else 
                   FPN = REAL(APONDN(K, N), kind=MAPL_R8)
                   HPN = REAL(HPONDN(K, N), kind=MAPL_R8)
                endif 
                call shortwave_dEdd(1,        1,          &    
                                1, (/1/),(/1/),           &    
                                COSZTH,                   &  ! Inputs
                                FRDB,      VOLICEDB,      &    
                                VOLSNODB,  FSN,           &    
                                RHOSNWN,   RSNWN,         &    
                                FPN,       HPN,           &    
                                OBSERVE,                  &    
                                DRUVRDB,   DFUVRDB,       &    
                                DRPARDB,   DFPARDB,       &    
                                VSUVRDB,   VSUVFDB,       &    
                                DRNIRDB,   DFNIRDB,       &    
                                ALBVRNDB,  ALBVFNDB,      &  ! Outputs: order of the following 4 parms is different from shortwave_ccsm3
                                ALBNRNDB,  ALBNFNDB,      &    
                                FSWSFCDB,  FSWINTDB,      &    
                                FSWTHRUDB, SSWABSDB,      &    
                                           ISWABSDB,      &    
                                DRUVRTHRUDB,   DFUVRTHRUDB, &
                                DRPARTHRUDB,   DFPARTHRUDB, &
                                ALBINDB,  ALBSNDB,   ALBPNDDB  )

                  SSWABS(K,:,N)     =  REAL(SSWABSDB, kind=MAPL_R4)
                  ALBPND(K,N)       =  REAL(ALBPNDDB(1), kind=MAPL_R4)
             else
                call shortwave_ccsm3 (1,      1,          &
                                1,(/1/),(/1/),            &
                                OBSERVE,                  &  ! Inputs
                                DRUVRDB,       DFUVRDB,   &
                                DRPARDB,       DFPARDB,   &
                                FRDB,          VOLICEDB,  &
                                VOLSNODB,      TSCDB,     &
                                VSUVRDB,       VSUVFDB,   &
                                DRNIRDB,       DFNIRDB,   &
                                ALBVRNDB,      ALBNRNDB,  &  ! Outputs
                                ALBVFNDB,      ALBNFNDB,  &
                                FSWSFCDB,      FSWINTDB,  &
                                FSWTHRUDB,     ISWABSDB,  &
                                DRUVRTHRUDB,   DFUVRTHRUDB, &
                                DRPARTHRUDB,   DFPARTHRUDB, &
                                ALBINDB,       ALBSNDB         )
             endif

             ALBVRN(K,N)       = REAL(ALBVRNDB(1),    kind=MAPL_R4)
             ALBNRN(K,N)       = REAL(ALBNRNDB(1),    kind=MAPL_R4)
             ALBVFN(K,N)       = REAL(ALBVFNDB(1),    kind=MAPL_R4)
             ALBNFN(K,N)       = REAL(ALBNFNDB(1),    kind=MAPL_R4)

             DRUVRTHRU(K,N)    = REAL(DRUVRTHRUDB(1), kind=MAPL_R4)
             DFUVRTHRU(K,N)    = REAL(DFUVRTHRUDB(1), kind=MAPL_R4)
             DRPARTHRU(K,N)    = REAL(DRPARTHRUDB(1), kind=MAPL_R4)
             DFPARTHRU(K,N)    = REAL(DFPARTHRUDB(1), kind=MAPL_R4)

             FSWSFC (K,N)      = REAL(FSWSFCDB(1),    kind=MAPL_R4)
             FSWINT (K,N)      = REAL(FSWINTDB(1),    kind=MAPL_R4)
             FSWTHRU(K,N)      = REAL(FSWTHRUDB(1),   kind=MAPL_R4)
             ISWABS (K,:,N)    = REAL(ISWABSDB,       kind=MAPL_R4)

             ALBIN(K,N)        = REAL(ALBINDB(1),     kind=MAPL_R4)
             ALBSN(K,N)        = REAL(ALBSNDB(1),     kind=MAPL_R4)
         end if
       end do EVERY_TILE
    end do EVERY_SUBTILE


    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_ALBSEAICE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_ICE_BUDGET - Computes and prints: flux of total mass and fresh water 

! !INTERFACE:

  subroutine CICE_ICE_BUDGET (DT, NT, thermo_, VMG, TILEAREA, TOTALFLUX,                      &
                              SLMASK, VOLICE, VOLICE_OLD, VOLSNO, VOLSNO_OLD)

! !ARGUMENTS:

    integer, intent(IN)     :: thermo_
    real,    intent(IN)     :: DT               ! time-step
    integer, intent(IN)     :: NT               ! number of tiles
    type(ESMF_VM), intent(IN) :: VMG
    real,    intent(IN)     :: TILEAREA  (:)
    real,    intent(IN)     :: TOTALFLUX (:)
    real,    intent(IN)     :: SLMASK   (:)     ! "salt water lake mask"

    real(kind=MAPL_R8), intent(IN)  :: VOLICE       (:,:)
    real(kind=MAPL_R8), intent(IN)  :: VOLSNO       (:,:)
    real(kind=MAPL_R8), intent(IN)  :: VOLICE_OLD   (:)
    real(kind=MAPL_R8), intent(IN)  :: VOLSNO_OLD   (:)
    
!  !LOCAL VARIABLES
    real                    :: TOTALAREA, ALLTOTALAREA

    real(kind=MAPL_R8), dimension(NT)  :: CICEDMASS

!  !DESCRIPTION:
!        Compute total mass flux and fresh water flux based on CICE Thermodynamics
   
    CICEDMASS = MAPL_RHO_SEAICE*(sum(VOLICE,dim=2)-VOLICE_OLD) + MAPL_RHO_SNOW*(sum(VOLSNO,dim=2)-VOLSNO_OLD)
    where(SLMASK > 0.5)
       CICEDMASS = 0.0
    endwhere
    TOTALAREA = sum(CICEDMASS*TILEAREA)

    call ESMF_VMBarrier(VMG, rc=status)
    VERIFY_(STATUS)
    call MAPL_CommsAllReduceSum(VMG, TOTALAREA, ALLTOTALAREA, 1, RC=STATUS)
    VERIFY_(STATUS)

    if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' After Thermo ', thermo_, '******************* '
    if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' total ice+sno mass change = ', &
                                 ALLTOTALAREA

    TOTALAREA = sum(TOTALFLUX * DT*TILEAREA)

    call ESMF_VMBarrier(VMG, rc=status)
    VERIFY_(STATUS)
    call MAPL_CommsAllReduceSum(VMG, TOTALAREA, ALLTOTALAREA, 1, RC=STATUS)
    VERIFY_(STATUS)

    if(MAPL_AM_I_ROOT()) print*, trim(Iam), ' total freshwaterflux * dt = ', &
                                 ALLTOTALAREA
    
    RETURN_(ESMF_SUCCESS)
  end subroutine CICE_ICE_BUDGET

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: CICE_INQUIRE_TILE - 
!  Computes OBSERVE which is useful to trace thru LANL CICE computations at a single lat/lon location
!  that location is specified by latso, lonso (resource paramater controlled)

! !INTERFACE:

  subroutine CICE_INQUIRE_TILE(LAT, LON, LATSO, LONSO, OBSERVE, LATSD, LONSD)

  use ice_constants,      only: rad_to_deg

! !ARGUMENTS:

    real,    intent(IN)  :: LAT
    real,    intent(IN)  :: LON
    real,    intent(IN)  :: LATSO
    real,    intent(IN)  :: LONSO

    logical, intent(OUT) :: OBSERVE(:)
    real,    intent(OUT) :: LATSD(:), LONSD(:)

!  !LOCAL VARIABLES

!  !DESCRIPTION:

    LATSD = LAT *  rad_to_deg
    LONSD = LON *  rad_to_deg
    OBSERVE = (abs(LATSD-LATSO) < 1.e-3) .and. (abs(LONSD-LONSO) < 1.e-3)

   RETURN_(ESMF_SUCCESS)
  end subroutine CICE_INQUIRE_TILE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: SIMPLE_SW_ABS - 
!  Implements two simple ways for the absorption of shortwave radiation,
!  as an alternative (for "TESTING" purposes) to using KPAR & KUVR based on PEN,
!  into an interface layer of typical depth = HW/RHO_SEA_WATER = 2m, that is also called "depth of skin layer"

! !INTERFACE:

  subroutine SIMPLE_SW_ABS(NT, USE_KPAR, depth, ZTH, SWN, PEN)

! !ARGUMENTS:

    integer, intent(IN)    :: NT        ! dimension of array
    integer, intent(IN)    :: USE_KPAR  ! absorption profile option
    real,    intent(IN)    :: ZTH  (:)  ! cosine of solar zenith angle
    real,    intent(IN)    :: depth(:)  ! depth up to which shortwave needs to be absorbed
    real,    intent(IN)    :: SWN(:)    ! net shortwave at surface of ocean, or @ top of air/sea interface
    real,    intent(INOUT) :: PEN(:)    ! shortwave penetrated below the depth    

!  local variables
    real, dimension(NT)  :: fW

    fW  = 0.0
    PEN = 0.0         ! zero it out -- to be safe.

    if (USE_KPAR == -1) then
       ! Soloviev, 1982 shortwave absorption profile
       ! --------------------------------------------
       fW = 0.28*exp(-71.5*depth) + 0.27*exp(-2.8*depth) + 0.45*exp(-0.07*depth)

    else if (USE_KPAR == -2) then
       ! Paulson & Simpson, 1981- Taken from Gentemann et al, 2009
       ! ----------------------------------------------------------
       fW    = 0.237*exp(-(depth*ZTH)/34.84)  +  0.36*exp(-(depth*ZTH)/2.266)   + &
               0.179*exp(-(depth*ZTH)/0.0315) + 0.087*exp(-(depth*ZTH)/0.0055)  + &
                0.08*exp(-(depth*ZTH)/8.32e-4)+ 0.025*exp(-(depth*ZTH)/1.26e-4) + &
               0.025*exp(-(depth*ZTH)/3.13e-4)+ 0.007*exp(-(depth*ZTH)/7.82e-4) + &
              0.0004*exp(-(depth*ZTH)/1.44e-5)
    else
       if(MAPL_AM_I_ROOT()) print *, 'ERROR! Unknown use_kpar option: ', USE_KPAR
    end if

    PEN   = SWN * fW

   RETURN_(ESMF_SUCCESS)
  end subroutine SIMPLE_SW_ABS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: SKIN_SST - Computes changes to SST in interface layer due to Cool Skin & Diurnal Warming 

! !INTERFACE:

  subroutine SKIN_SST (DO_SKIN_LAYER,NT,CM,UUA,VVA,UW,VW,HW,SWN,LHF,SHF,LWDNSRF,                   &
                       ALW,BLW,PEN,STOKES_SPEED,DT,MUSKIN,TS_FOUNDi,DWARM_,TBAR_,TXW,TYW,USTARW_,  &
                       DCOOL_,TDROP_,SWCOOL_,QCOOL_,BCOOL_,LCOOL_,TDEL_,SWWARM_,QWARM_,ZETA_W_,    &
                       PHIW_,LANGM_,TAUTW_,uStokes_,TS,TWMTS,TW,WATER,FR,n_iter_cool,fr_ice_thresh)

! !ARGUMENTS:

    integer, intent(IN)    :: DO_SKIN_LAYER  ! 0: No interface layer,     1: active, and accounts for change in SST
    integer, intent(IN)    :: NT             ! number of tiles
    real,    intent(IN)    :: FR     (:,:)   ! fraction of surface (water/ice)
    integer, intent(IN)    :: WATER          ! subtile  number assigned to surface type: "WATER" 
    real,    intent(IN)    :: CM     (:,:)   ! transfer coefficient for wind
    real,    intent(IN)    :: UUA    (:)     ! zonal       wind
    real,    intent(IN)    :: VVA    (:)     ! meridional  wind
    real,    intent(IN)    :: UW     (:)     ! u-current
    real,    intent(IN)    :: VW     (:)     ! v-current
    real,    intent(IN)    :: HW     (:)     ! mass  of skin layer
    real,    intent(IN)    :: SWN    (:)     ! net shortwave radiation incident at surface
    real,    intent(IN)    :: LHF    (:)     ! latent   heat flux
    real,    intent(IN)    :: SHF    (:)     ! sensible heat flux
    real,    intent(IN)    :: LWDNSRF(:)     ! longwave at surface
    real,    intent(IN)    :: ALW    (:)     ! for linearized \sigma T^4
    real,    intent(IN)    :: BLW    (:)     ! for linearized \sigma T^4
    real,    intent(IN)    :: PEN    (:)     ! shortwave radiation that penetrates below interface layer
    real,    intent(IN)    :: STOKES_SPEED   ! scalar value set for Stokes speed- place holder for output from Wave model
    real,    intent(IN)    :: DT             ! time-step
    real,    intent(IN)    :: MUSKIN         ! exponent of temperature: T(z) profile in warm layer
    real,    intent(IN)    :: TS_FOUNDi(:)   ! bulk SST (temperature at base of warm layer)
    integer, intent(IN)    :: n_iter_cool    ! number of iterations to compute cool-skin layer 
    real,    intent(IN)    :: fr_ice_thresh  ! threshold on ice fraction, sort of defines Marginal Ice Zone

    real,    intent(OUT)   :: DWARM_ (:)     ! depth of skin layer
    real,    intent(OUT)   :: TBAR_  (:)     ! copy of TW (also internal state) to export out
    real,    intent(OUT)   :: USTARW_(:)     ! u_{*,w} 
    real,    intent(OUT)   :: DCOOL_ (:)     ! depth of cool-skin layer
    real,    intent(OUT)   :: TDROP_ (:)     ! temperature drop across cool-skin
    real,    intent(OUT)   :: SWCOOL_(:)     ! shortwave radiation absorbed in cool-skin 
    real,    intent(OUT)   :: QCOOL_ (:)     ! net heat flux in cool layer
    real,    intent(OUT)   :: BCOOL_ (:)     ! bouyancy in cool layer
    real,    intent(OUT)   :: LCOOL_ (:)     ! Saunder's parameter in cool layer

    real,    intent(OUT)   :: TDEL_  (:)     ! temperature at top of warm layer
    real,    intent(OUT)   :: SWWARM_(:)     ! shortwave radiation absorbed in warm layer
    real,    intent(OUT)   :: QWARM_ (:)     ! net heat flux in warm layer
    real,    intent(OUT)   :: ZETA_W_(:)     ! stability parameter = dwarm/(Obukhov length)
    real,    intent(OUT)   :: PHIW_  (:)     ! similarity function
    real,    intent(OUT)   :: LANGM_ (:)     ! Langmuir number
    real,    intent(OUT)   :: TAUTW_ (:)     ! time-scale of relaxation to bulk SST (i.e., TS_FOUND)
    real,    intent(OUT)   :: uStokes_(:)    ! Stokes speed

    real,    intent(INOUT) :: TXW    (:)     ! zonal      stress
    real,    intent(INOUT) :: TYW    (:)     ! meridional stress
    real,    intent(INOUT) :: TWMTS  (:)     ! "internal state" variable that has: TW - TS
    real,    intent(INOUT) :: TW     (:)     ! "internal state" variable that has: TW
    real,    intent(INOUT) :: TS     (:,:)   ! skin temperature

!  !LOCAL VARIABLES

    integer         :: N, iter_cool
    real            :: ALPH, Qb, fC, fLA, X1, X2

    real, parameter :: RHO_SEAWATER    = 1022.0  ! sea water density             [kg/m^3]    ! Replace Usage of RHO_SEAWATER with MAPL_RHO_SEAWATER
    real, parameter :: NU_WATER        = 1.0E-6  ! kinematic viscosity of water  [m^2/s]
    real, parameter :: TherCond_WATER  = 0.563   ! Thermal conductivity of water [W/m/ K]
    real, parameter :: bigC            = &
          (16.0 * (MAPL_CAPWTR*MAPL_RHOWTR)**2 * NU_WATER**3) / TherCond_WATER**2

!  !DESCRIPTION:
!        Based on Fairall et al, 1996 for Cool Skin Layer and Takaya et al, 2010 for Warm Layer

! Open water conditions, including computation of skin layer parameters
!----------------------------------------------------------------------

    do N = 1, NT  ! N is now looping over all tiles (NOT sub-tiles).

! Stress over "open" water (or Marginal Ice Zone) depends on ocean currents
!--------------------------------------------------------------------------

       TXW(N) = CM(N,WATER)*(UUA(N) - UW(N))
       TYW(N) = CM(N,WATER)*(VVA(N) - VW(N))

       if( FR(N, WATER) > fr_ice_thresh) then 

! Depth and mean temperature of interface layer
!----------------------------------------------

          DWARM_(N) = HW(N)/MAPL_RHOWTR                                                   ! replace MAPL_RHOWTR with MAPL_RHO_SEAWATER
          TBAR_(N)  = TS(N,WATER) + TWMTS(N)

! Ustar in water has a floor of 2 \mu m/s
!----------------------------------------

          USTARW_(N) = max( 2.e-6, sqrt(sqrt(TXW(N)*TXW(N)+TYW(N)*TYW(N))/MAPL_RHOWTR) )  ! replace MAPL_RHOWTR with MAPL_RHO_SEAWATER

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Cool skin layer- heat loss and temperature drop  @ top of interface layer !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          DCOOL_(N)  = 1.e-3           ! initial guess for cool-skin layer thickness
          TDROP_(N)  = 0.2             ! guess for cool-skin tdrop. FINAL TDROP IS SENSITIVE TO INITIAL CHOICE. 3 ITER ENOUGH?

          COOL_SKIN: do iter_cool = 1, n_iter_cool

! Short wave absorbed in the cool layer. This is a modified version of Zhang and Beljaars, 2005
!----------------------------------------------------------------------------------------------

             fC  = 0.0685 + 11.0*DCOOL_(N) - (3.3e-5/DCOOL_(N))*(1.0-exp(-DCOOL_(N)/8.0E-4))
             fC  = max( fC, 0.01)        ! absorb at least 1% of shortwave in cool layer
             SWCOOL_(N) = SWN(N)*fC

! Heat loss at top of skin (cool) layer
!--------------------------------------

             X1 = LHF(N) + SHF(N) - ( LWDNSRF(N) -(ALW(N) + BLW(N)*( TS(N,WATER)-TDROP_(N))))
             QCOOL_(N)  = X1 - SWCOOL_(N)

! Bouyancy production in cool layer depends on surface cooling
! and evap-salinity effect from surface. It does not depend on solar
! heating, which is assumed to be uniform in cool layer. This last assumption
! could be improved by including some NIR. For this calculation, we include
! temperature dependence of the thermal expansion coefficient.
!-------------------------------------------------------------------------------

             ALPH   = (0.6 + 0.0935*(TBAR_(N)-MAPL_TICE))*1.E-4
             Qb     = QCOOL_(N) + ( (0.026*MAPL_CAPWTR)/(ALPH*MAPL_ALHL) )*LHF(N)
             BCOOL_(N) = (ALPH*MAPL_GRAV*Qb) / (RHO_SEAWATER*MAPL_CAPWTR)                 ! replace RHO_SEAWATER with MAPL_RHO_SEAWATER

! Saunders parameter
! BigC = (16.0 * (MAPL_CAPWTR*MAPL_RHO_SEAWATER)**2 * NU_WATER**3) / TherCond_WATER**2  
!-------------------------------------------------------------------------------

             if ( BCOOL_(N) > 0.0) then  ! Eqn(14) of F96
                LCOOL_(N)  = 6.0/( 1.0 + ( BCOOL_(N)*bigC / USTARW_(N)**4 )**0.75 )**(1./3.)
                DCOOL_(N)  = LCOOL_(N)*NU_WATER/USTARW_(N)
             else 
                LCOOL_(N)  = 6.0
                DCOOL_(N)  = min( LCOOL_(N)*NU_WATER/USTARW_(N), 1.e-2)  ! Prevent very thick cool layer depth
             end if

             TDROP_(N)    = max( 0.0, DCOOL_(N)*QCOOL_(N)/TherCond_WATER ) ! Eqn(4) & (13) of F96

          end do COOL_SKIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Done with Cool skin layer.  Now turbluent heat flux at base of interface layer  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          WARM_LAYER: if(DO_SKIN_LAYER==0) then   ! Warm layer temperature increase calculated based on definition of mean interface temperature.

             TDEL_(N)    = TS(N,WATER) + TDROP_(N)
             TWMTS(N)    = TBAR_(N)    - TS(N,WATER)

!            fill up with mapl_undef - so that LocStreamMod does NOT die while exporting
             SWWARM_(N)  = MAPL_UNDEF
             QWARM_ (N)  = MAPL_UNDEF
             ZETA_W_(N)  = MAPL_UNDEF
             PHIW_(N)    = MAPL_UNDEF
             LANGM_(N)   = MAPL_UNDEF
             TAUTW_(N)   = MAPL_UNDEF

          else  ! use Takaya et al 2012

! Compute warm layer temperature increase based on Takaya et al, 2012
!--------------------------------------------------------------------

             ALPH        = (0.6 + 0.0935*(TBAR_(N)-MAPL_TICE))*1.E-4

! Short wave absorbed in the warm layer.
!--------------------------------------

             X1         = LHF(N) + SHF(N) - ( LWDNSRF(N) -(ALW(N) + BLW(N)*TS(N,WATER)))
             SWWARM_(N) = SWN(N) - PEN(N)
             QWARM_(N)  = SWWARM_(N) - X1

! Stability parameter & Similarity function
!------------------------------------------

             ZETA_W_(N)  = (DWARM_(N)*MAPL_KARMAN*MAPL_GRAV*ALPH*QWARM_(N)) / &           ! zeta_w = dwarm/obukhov length
                           (RHO_SEAWATER*MAPL_CAPWTR*USTARW_(N)**3)                       ! replace RHO_SEAWATER with MAPL_RHO_SEAWATER

             if ( ZETA_W_(N) >= 0.0) then   ! Takaya: Eqn(5)
                PHIW_(N) = 1. + (5*ZETA_W_(N) + 4.*ZETA_W_(N)**2)/(1+3.*ZETA_W_(N)+0.25*ZETA_W_(N)**2)
             else
                PHIW_(N) = 1.0/sqrt(1.-16.*ZETA_W_(N))
             end if

! Langmuir number- need imports from Wave Model
!----------------------------------------------

             uStokes_(N) = STOKES_SPEED
             LANGM_(N)   = sqrt(USTARW_(N)/uStokes_(N))
             fLA         = LANGM_(N)**(-0.66667)           ! Takaya: Eqn(6)

             IF (fLA       <= 1.0) fLA = 1.0               ! Limit range of fLa to be >=1
             IF (ZETA_W_(N)<= 0.0) fLA = 1.0               ! Apply fLa to stable conditions only

             TAUTW_(N)   = &
                  (DWARM_(N)*PHIW_(N))/(MAPL_KARMAN*USTARW_(N)*fLA*(MUSKIN+1.))

             X2          = DT * &
                  ( (MAPL_KARMAN*USTARW_(N)*fLA*(MUSKIN+1.))/(DWARM_(N)*PHIW_(N)) )

! We DO NOT include cool-skin tdrop in TW, therefore, we now save TW

             TW(N)       = TS_FOUNDi(N) + ( 1.0/(1.+X2))        *    (TBAR_(N) - TS_FOUNDi(N))
             TS(N,WATER) = TS(N,WATER)  + ((1.0+MUSKIN)/MUSKIN) *    (TW(N)    - TBAR_(N))

             TDEL_(N)    = TS_FOUNDi(N) + ((1.0+MUSKIN)/MUSKIN) * MAX(TW(N)    - TS_FOUNDi(N), 0.0)
             TBAR_(N)    = TW(N)

             TS(N,WATER) = TDEL_(N) - TDROP_(N)
             TWMTS(N)    = TW(N) - TS(N,WATER)
          end if WARM_LAYER

       else            ! FR(N, WATER) <= fr_ice_thresh
          DCOOL_ (N)     = MAPL_UNDEF
          LCOOL_ (N)     = MAPL_UNDEF
          DWARM_ (N)     = MAPL_UNDEF
          TBAR_  (N)     = MAPL_UNDEF
          TDROP_ (N)     = MAPL_UNDEF
          QCOOL_ (N)     = MAPL_UNDEF
          USTARW_(N)     = MAPL_UNDEF
          SWCOOL_(N)     = MAPL_UNDEF
          BCOOL_ (N)     = MAPL_UNDEF
          TDEL_  (N)     = MAPL_UNDEF
          TWMTS  (N)     = 0.0
          QWARM_ (N)     = MAPL_UNDEF
          SWWARM_(N)     = MAPL_UNDEF
          PHIW_  (N)     = MAPL_UNDEF
          LANGM_ (N)     = MAPL_UNDEF
          TAUTW_ (N)     = MAPL_UNDEF
          ZETA_W_(N)     = MAPL_UNDEF
       end if
    end do


   RETURN_(ESMF_SUCCESS)
  end subroutine SKIN_SST

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine RUN2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: Finalize        -- Finalize method for CICEThermo wrapper

! !INTERFACE:

  subroutine Finalize ( gc, import, export, clock, rc ) 

! !ARGUMENTS:

  type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: import ! Import state
  type(ESMF_State),    intent(INOUT) :: export ! Export state
  type(ESMF_Clock),    intent(INOUT) :: clock  ! The supervisor clock
  integer, optional,   intent(  OUT) :: rc     ! Error code:

!EOP

    type (MAPL_MetaComp), pointer:: MAPL 

! ErrLog Variables

    character(len=ESMF_MAXSTR)       :: IAm
    integer                          :: STATUS
    character(len=ESMF_MAXSTR)       :: COMP_NAME

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Finalize"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=status )
    VERIFY_(STATUS)
    Iam = trim(comp_name) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=status)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"   )
    call MAPL_TimerOn(MAPL,"FINALIZE")

    if (DO_CICE_THERMO /= 0) call dealloc_column_physics( MAPL_AM_I_Root(), Iam )

    call MAPL_TimerOff(MAPL,"FINALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"   )

! Generic Finalize
! ------------------
    
    call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)

! All Done
!---------

    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GEOS_SaltwaterGridCompMod


