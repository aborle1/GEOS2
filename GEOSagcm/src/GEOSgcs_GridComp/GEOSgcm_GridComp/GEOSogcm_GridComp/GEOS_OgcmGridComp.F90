!  $Id$
#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_Ogcm   -- A composite component for the ogcm components.

! !INTERFACE:

module GEOS_OgcmGridCompMod

! !USES:

  use ESMF
  use MAPL_Mod

  use GEOS_OceanBioGeoChemGridCompMod,   only : ObioSetServices   => SetServices
  use GEOS_OceanBioGridCompMod,          only : ObioSimpleSetServices => SetServices
  use GEOS_OradBioGridCompMod,           only : OradBioSetServices   => SetServices
  use GEOS_OradGridCompMod,              only : OradSetServices   => SetServices

  use GuestOcean_GridCompMod,            only : GuestOceanSetServices  => SetServices
  use GEOS_DataSeaGridCompMod,           only : DataSeaSetServices  => SetServices
  use GEOS_CICEDynaGridCompMod,          only : SeaIceSetServices => SetServices
  use GEOS_DataSeaIceGridCompMod,        only : DataSeaIceSetServices => SetServices

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION:
! 
!   {\tt GEOS\_Ogcm} is a light-weight gridded component that implements the
!      interface to the ogcm components. The ogcm computational components
!      (Poseidon, OceanRadiation, OceanBioGeochemistry, etc) are its children.
!      This component currently serves as an interface between the exchange
!      grid and the ocean's grid. Its ``natural'' grid is the ocean part of the 
!      exchange grid, and all its imports and exports are on this grid. The natural
!      grid of all of its children is currently the ocean's rectangular grid.
!      The ESMF grid that is in the gridded component is created by the parent
!      and it is the ocean's rectangular grid. At present the exchange grid information
!      is kept in the generic state.
!
!      The fact that some of these are friendlies---all the ``skin'' 
!      components---means that it cannot be a ``no-work-no-change'' component.
!      The interpolation of these to the ocean grid leave an ocean grid imprint
!      on them. No such happens on the atmospheric side. So we should think of these
!      exchange grid friendlies as ocean variables.  
!
!EOP

  integer, parameter :: NUM_3D_ICE_TRACERS=3
  integer, parameter :: NUM_SNOW_LAYERS=1
  integer            :: NUM_ICE_CATEGORIES
  integer            :: NUM_ICE_LAYERS
  integer, parameter :: NUM_DUDP = 5
  integer, parameter :: NUM_DUWT = 5
  integer, parameter :: NUM_DUSD = 5
  integer, parameter :: NUM_BCDP = 2
  integer, parameter :: NUM_BCWT = 2
  integer, parameter :: NUM_OCDP = 2
  integer, parameter :: NUM_OCWT = 2
  integer, parameter :: NB_CHOU_UV   = 5 ! Number of UV bands
  integer, parameter :: NB_CHOU_NIR  = 3 ! Number of near-IR bands
  integer, parameter :: NB_CHOU      = NB_CHOU_UV + NB_CHOU_NIR ! Total number of bands
  integer            :: DO_CICE_THERMO
  integer            :: DO_GUEST
  integer            :: DO_OBIO
  integer            :: DO_DATAATM

!=============================================================================

  integer ::        OBIO
  integer ::        ORAD
  integer ::      SEAICE
  integer ::       OCEAN

!ALT: if next statement needs to be changed, attention is required
!     for the size of the array, and length of the string
  character(len=2) :: FRIENDLY_IMPORTS(3) = (/'TW',  &
                                              'HW',  &
                                              'SW'/)

  type T_OGCM_STATE
     private
     logical :: useInterp = .false.
  end type T_OGCM_STATE

! Wrapper for extracting internal state
! -------------------------------------
  type OGCM_WRAP
     type (T_OGCM_STATE), pointer :: PTR => null()
  end type OGCM_WRAP

contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices, which in addition
!                to setting default IRF methods, also allocates
!   our instance of a generic state and puts it in the 
!   gridded component (GC). Here we override the Initialize and Run methods.

!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    type (MAPL_MetaComp),  pointer          :: MAPL
    type (ESMF_Config)                      :: CF

    type (T_OGCM_STATE), pointer            :: ogcm_internal_state => null() 
    type (OGCM_wrap)                        :: wrap

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Set the Run and initialize entry points
!----------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run       , RC=STATUS )
    VERIFY_(STATUS)

! Set the state variable specs.
! -----------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Get constants from CF
! ---------------------

    call MAPL_GetResource ( MAPL,       DO_CICE_THERMO,     Label="USE_CICE_Thermo:" ,       DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

    if (DO_CICE_THERMO /= 0) then     ! Before merging CICEthermo with SaltWater, following were set via makefile.
       call ESMF_ConfigGetAttribute(CF, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" , RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_ConfigGetAttribute(CF, NUM_ICE_LAYERS,     Label="CICE_N_ICE_LAYERS:" ,     RC=STATUS)
       VERIFY_(STATUS)
    else
       NUM_ICE_CATEGORIES = 1
       NUM_ICE_LAYERS     = 1
    endif

    call MAPL_GetResource ( MAPL, DO_GUEST, Label="USE_GUEST_OCEAN:" , DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DO_OBIO,        Label="USE_OCEANOBIOGEOCHEM:",DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DO_DATAATM,     Label="USE_DATAATM:" ,     DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)
    
    if (DO_DATAATM/=0) then
       ASSERT_(DO_GUEST/=0)
    end if
    if (DO_GUEST/=0) then
       ASSERT_(DO_CICE_THERMO/=0)
    end if

! Create childrens gridded components and invoke their SetServices
! ----------------------------------------------------------------

    if (DO_OBIO/=0) then
       ASSERT_(DO_GUEST/=0)
       OBIO = MAPL_AddChild(GC, NAME='OBIO', SS=ObioSetServices, RC=STATUS)
       VERIFY_(STATUS)
       ORAD = MAPL_AddChild(GC, NAME='ORAD', SS=OradBioSetServices, RC=STATUS)
       VERIFY_(STATUS)
    else
       OBIO = MAPL_AddChild(GC, NAME='OBIO', SS=ObioSimpleSetServices, RC=STATUS)
       VERIFY_(STATUS)
       ORAD = MAPL_AddChild(GC, NAME='ORAD', SS=OradSetServices, RC=STATUS)
       VERIFY_(STATUS)
    end if
    
    if (DO_GUEST==0) then
       SEAICE = MAPL_AddChild(GC, NAME='SEAICE', SS=DataSeaIceSetServices, RC=STATUS)
       VERIFY_(STATUS)
       OCEAN = MAPL_AddChild(GC, NAME='OCEAN', SS=DataSeaSetServices, RC=STATUS) 
       VERIFY_(STATUS)
    else
       SEAICE = MAPL_AddChild(GC, NAME='SEAICE', SS=SeaIceSetServices, RC=STATUS)
       VERIFY_(STATUS)
       OCEAN = MAPL_AddChild(GC, NAME='OCEAN', SS=GuestOceanSetServices, RC=STATUS) 
       VERIFY_(STATUS)
    end if
    
! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'eastward_stress_on_ocean'          ,&
    UNITS              = 'N m-2'                             ,&
    SHORT_NAME         = 'TAUXW'                             ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'northward_stress_on_ocean',         &
    UNITS              = 'N m-2'                             ,&
    SHORT_NAME         = 'TAUYW'                             ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'eastward_stress_on_ice'            ,&
    UNITS              = 'N m-2'                             ,&
    SHORT_NAME         = 'TAUXI'                             ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'northward_stress_on_ice',           &
    UNITS              = 'N m-2'                             ,&
    SHORT_NAME         = 'TAUYI'                             ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'ocean_ustar_cubed',                 &
    UNITS              = 'm+3 s-3'                           ,&
    SHORT_NAME         = 'OUSTAR3'                           ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME          = 'surface_wind_speed'                ,&
    UNITS              = 'm s-1'                             ,&
    SHORT_NAME         = 'UU'                                ,&
    DIMS               = MAPL_DimsTileOnly                   ,&
    VLOCATION          = MAPL_VLocationNone                  ,&
                                           RC=STATUS          ) 
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    LONG_NAME           = 'surface_air_pressure',             &
    UNITS               = 'Pa',                               &
    SHORT_NAME          = 'PS',                               &
    DIMS                = MAPL_DimsTileOnly,                  &
    VLOCATION           = MAPL_VLocationNone,                 &
    DEFAULT             = 100000.,                            &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                             &
       SHORT_NAME         = 'PENUVR',                            &
       LONG_NAME          = 'net_downward_penetrating_direct_UV_flux',  &
       UNITS              = 'W m-2',                             &
       DIMS               = MAPL_DimsTileOnly,                   &
       VLOCATION          = MAPL_VLocationNone,                  &
       RC=STATUS  )
  VERIFY_(STATUS)
  
  call MAPL_AddImportSpec(GC,                             &
       SHORT_NAME         = 'PENPAR',                            &
       LONG_NAME          = 'net_downward_penetrating_direct_PAR_flux', &
       UNITS              = 'W m-2',                             &
       DIMS               = MAPL_DimsTileOnly,                   &
       VLOCATION          = MAPL_VLocationNone,                  &
       RC=STATUS  )
  VERIFY_(STATUS)
  
  call MAPL_AddImportSpec(GC,                             &
       SHORT_NAME         = 'PENUVF',                            &
       LONG_NAME          = 'net_downward_penetrating_diffuse_UV_flux',  &
       UNITS              = 'W m-2',                             &
       DIMS               = MAPL_DimsTileOnly,                   &
       VLOCATION          = MAPL_VLocationNone,                  &
       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                             &
       SHORT_NAME         = 'PENPAF',                            &
       LONG_NAME          = 'net_downward_penetrating_diffuse_PAR_flux', &
       UNITS              = 'W m-2',                             &
       DIMS               = MAPL_DimsTileOnly,                   &
       VLOCATION          = MAPL_VLocationNone,                  &
       RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                    &
       LONG_NAME          = 'river_discharge_at_ocean_points',&
       UNITS              = 'kg m-2 s-1'                ,&
       SHORT_NAME         = 'DISCHRG'                   ,&
       DIMS               = MAPL_DimsTileOnly           ,&
       VLOCATION          = MAPL_VLocationNone          ,&
       RESTART            = MAPL_RestartSkip            ,&
       RC=STATUS  ) 
  VERIFY_(STATUS)

  if(DO_DATAATM==0) then
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'CO2 Surface Concentration Bin 001', &
          UNITS              = '1e-6'                       ,&
          SHORT_NAME         = 'CO2SC'                      ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'Dust Dry Deposition'        ,&
          UNITS              = 'kg m-2 s-1'                 ,&
          SHORT_NAME         = 'DUDP'                       ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NUM_DUDP/)                 ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'Dust Wet Deposition'        ,&
          UNITS              = 'kg m-2 s-1'                 ,&
          SHORT_NAME         = 'DUWT'                       ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NUM_DUWT/)                 ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'Dust Sedimentation'         ,&
          UNITS              = 'kg m-2 s-1'                 ,&
          SHORT_NAME         = 'DUSD'                       ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NUM_DUSD/)                 ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'Black Carbon Dry Deposition',&
          UNITS              = 'kg m-2 s-1'                 ,&
          SHORT_NAME         = 'BCDP'                       ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NUM_BCDP/)                 ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'Black Carbon Wet Deposition',&
          UNITS              = 'kg m-2 s-1'                 ,&
          SHORT_NAME         = 'BCWT'                       ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NUM_BCWT/)                 ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                               &
          LONG_NAME          = 'Organic Carbon Dry Deposition',&
          UNITS              = 'kg m-2 s-1'                   ,&
          SHORT_NAME         = 'OCDP'                         ,&
          DIMS               = MAPL_DimsTileOnly              ,&
          UNGRIDDED_DIMS     = (/NUM_OCDP/)                   ,&
          VLOCATION          = MAPL_VLocationNone             ,&
          RESTART            = MAPL_RestartSkip               ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                               &
          LONG_NAME          = 'Organic Carbon Wet Deposition',&
          UNITS              = 'kg m-2 s-1'                   ,&
          SHORT_NAME         = 'OCWT'                         ,&
          DIMS               = MAPL_DimsTileOnly              ,&
          UNGRIDDED_DIMS     = (/NUM_OCWT/)                   ,&
          VLOCATION          = MAPL_VLocationNone             ,&
          RESTART            = MAPL_RestartSkip               ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air',&
          UNITS              = 'W m-2'                      ,&
          SHORT_NAME         = 'FSWBAND'                    ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                  ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
     
     call MAPL_AddImportSpec(GC,                             &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol',&
          UNITS              = 'W m-2'                      ,&
          SHORT_NAME         = 'FSWBANDNA'                  ,&
          DIMS               = MAPL_DimsTileOnly            ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                  ,&
          VLOCATION          = MAPL_VLocationNone           ,&
          RESTART            = MAPL_RestartSkip             ,&
          RC=STATUS  ) 
     VERIFY_(STATUS)
  end if
  
     ! These six are supposed to be friendly to us
!----------------------------------------------

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'HW',                                &
    LONG_NAME          = 'water_skin_layer_mass',             &
    UNITS              = 'kg',                                &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 5.0,                                 &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'TW',                                &
    LONG_NAME          = 'water_skin_temperature',            &
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 280.0,                               &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'SW',                                &
    LONG_NAME          = 'water_skin_salinity',               &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 30.0,                                &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'HI',                                &
    LONG_NAME          = 'seaice_skin_layer_mass',            &
    UNITS              = 'kg',                                &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 0.0,                                 &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddImportSpec(GC,                            &
    SHORT_NAME         = 'SI',                                &
    LONG_NAME          = 'seaice_skin_salinity',              &
    UNITS              = 'psu',                               &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
    DEFAULT            = 0.0,                                 &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  if (DO_CICE_THERMO /= 0) then  
     call MAPL_AddImportSpec(GC,                            &
          SHORT_NAME         = 'FRACICE',                         &
          LONG_NAME          = 'fractional_cover_of_seaice',        &
          UNITS              = '1',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          VLOCATION          = MAPL_VLocationNone,                  &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                            &
          SHORT_NAME         = 'TI',                                &
          LONG_NAME          = 'seaice_skin_temperature',           &
          UNITS              = 'K',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = MAPL_TICE,                           &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'VOLICE',                            &
          LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
          UNITS              = 'm',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'VOLSNO',                            &
          LONG_NAME          = 'sno_category_volume_per_unit_area_of_grid_cell',&
          UNITS              = 'm',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'ERGICE',                            &
          LONG_NAME          = 'ice_category_layer_internal_energy',&
          UNITS              = 'J m-2',                             &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS,NUM_ICE_CATEGORIES/),&
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'ERGSNO',                            &
          LONG_NAME          = 'snow_category_layer_internal_energy',&
          UNITS              = 'J m-2',                             &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS,NUM_ICE_CATEGORIES/),&
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'TAUAGE',                            &
          LONG_NAME          = 'volume_weighted_mean_ice_age',      &
          UNITS              = 's',                                 &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                &
          SHORT_NAME         = 'MPOND',                            &
          LONG_NAME          = 'pond_volume',                       &
          UNITS              = 'm',                                 &
          UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = 0.0,                                 &
          RC=STATUS  )
     VERIFY_(STATUS)
  else
     call MAPL_AddImportSpec(GC,                            &
          SHORT_NAME         = 'TI',                                &
          LONG_NAME          = 'seaice_skin_temperature',           &
          UNITS              = 'K',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          DEFAULT            = MAPL_TICE,                           &
          RC=STATUS  )
     VERIFY_(STATUS)
  endif

!  !EXPORT STATE:

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'UW',                                &
    LONG_NAME          = 'zonal_velocity_of_surface_water',   &
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'VW',                                &
    LONG_NAME          = 'meridional_velocity_of_surface_water',&
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'UI',                                &
    LONG_NAME          = 'zonal_velocity_of_surface_seaice',  &
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'VI',                                &
    LONG_NAME          = 'meridional_velocity_of_surface_seaice',&
    UNITS              = 'm s-1 ',                            &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'TILELONS',                          &
    LONG_NAME          = 'longitude',                         &
    UNITS              = 'degrees',                           &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)


  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'TILELATS',                          &
    LONG_NAME          = 'latitude',                          &
    UNITS              = 'degrees',                           &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                            &
    SHORT_NAME         = 'KPAR',                              &
    LONG_NAME          = 'PAR_extinction_coefficient',        &
    UNITS              = 'm-1 ',                              &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC,                                 &
    SHORT_NAME         = 'TS_FOUND',                          &
    LONG_NAME          = 'foundation_temperature_for_interface_layer',&
    UNITS              = 'K',                                 &
    DIMS               = MAPL_DimsTileOnly,                   &
    VLOCATION          = MAPL_VLocationNone,                  &
                                                   RC=STATUS  )
  VERIFY_(STATUS)



  if (DO_CICE_THERMO == 0) then  
     call MAPL_AddExportSpec(GC,                            &
          SHORT_NAME         = 'FRACICE',                           &
          LONG_NAME          = 'fractional_cover_of_seaice',        &
          UNITS              = '1',                                 &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          RC=STATUS  )
     VERIFY_(STATUS)
  else
     call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME         = 'TAUXIBOT',                           &
          LONG_NAME          = 'eastward_stress_at_base_of_ice',    &
          UNITS              = 'N m-2',                             &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME         = 'TAUYIBOT',                           &
          LONG_NAME          = 'northward_stress_at_base_of_ice',   &
          UNITS              = 'N m-2',                             &
          DIMS               = MAPL_DimsTileOnly,                   &
          VLOCATION          = MAPL_VLocationNone,                  &
          RC=STATUS  )
     VERIFY_(STATUS)
  end if
  
#ifdef USE_ODAS
    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'MOM_3D_MASK',                          &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'skin_T',                               &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'skin_S',                               &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'T',                                    &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'S',                                    &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'U',                                    &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'V',                                    &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'Z',                                    &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'RHO',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'SLV',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'SSH',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)


    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'PBO',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'TX',                                   &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'TY',                                   &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'MLD',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec ( GC   ,                          &
         SHORT_NAME = 'PSI',                                  &
         CHILD_ID   = OCEAN,                                  &
                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                               &
         SHORT_NAME = 'AICE',                                 &
         CHILD_ID   = SEAICE,                                 &

                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                               &
         SHORT_NAME = 'HICE',                                 &
         CHILD_ID   = SEAICE,                                 &

                                                        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                               &
         SHORT_NAME = 'OCEANCOLOR',                           &
         CHILD_ID   = OBIO,                                   &

                                                        RC=STATUS  )
    VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC,                               &
         SHORT_NAME = 'CHLOROPHYLL',                          &
         CHILD_ID   = OBIO,                                   &

                                                        RC=STATUS  )
    VERIFY_(STATUS)
#endif

!EOS

! Connections between the children
!---------------------------------

  if(DO_GUEST/=0) then
     ! Radiation to Ocean
     call MAPL_AddConnectivity ( GC,  &
          SHORT_NAME  = (/'SWHEAT'/), &
          DST_ID = OCEAN,             &
          SRC_ID = ORAD,              &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     ! Ocean to Radiation
     call MAPL_AddConnectivity ( GC,  &
          SHORT_NAME  = (/'DH'/),     &
          DST_ID = ORAD,              &
          SRC_ID = OCEAN,             &
          RC=STATUS  )
     VERIFY_(STATUS)
  end if

  if(DO_OBIO/=0) then

     ! Ocean to OceanBio
     call MAPL_AddConnectivity ( GC,   &
          SHORT_NAME  = (/'DH', 'T ', 'S '/),     &
          DST_ID = OBIO,               &
          SRC_ID = OCEAN,              &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     ! OceanRad to OceanBio
     call MAPL_AddConnectivity ( GC,   &
          SHORT_NAME  = (/'TIRRQ'/),   &
          DST_ID = OBIO,               &
          SRC_ID = ORAD,               &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     ! OceanBio to OceanRad
     call MAPL_AddConnectivity ( GC,   &
          SHORT_NAME  = (/'DIATOM','CHLORO','CYANO ','COCCO ','CDET  ','PIC   ','AVGQ  '/), &
          DST_ID = ORAD,               &
          SRC_ID = OBIO,               &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     ! Seaice to OceanBio
     call MAPL_AddConnectivity ( GC,   &
          SHORT_NAME  = (/'FRACICE'/), &
          DST_ID = OBIO,               &
          SRC_ID = SEAICE,             &
          RC=STATUS  )
     VERIFY_(STATUS)
     
  end if
  
  if(DO_GUEST/=0) then
     call MAPL_AddConnectivity ( GC,  &
          SHORT_NAME  = (/'UWB','VWB','UW ','VW ','SLV'/), &
          SRC_ID = OCEAN,             &
          DST_ID = SEAICE,             &
          RC=STATUS  )
     VERIFY_(STATUS)
     
     call MAPL_AddConnectivity ( GC,  &
          SHORT_NAME  = (/'TAUXBOT','TAUYBOT','FRACICE', 'HICE   ', 'HSNO   '/), &
          DST_ID = OCEAN,             &
          SRC_ID = SEAICE,            &
          RC=STATUS  )
     VERIFY_(STATUS)
  end if

#if defined(USE_ODAS) && (! defined(USE_OBIO))
  call MAPL_AddConnectivity(gc, short_name = ['CHLOROPHYLL'], dst_id = ORAD, src_id = OBIO, __RC__)
  call MAPL_AddConnectivity(gc, short_name = ['AICE'], dst_id = ORAD, src_id = SEAICE, __RC__)
  call MAPL_AddConnectivity(gc, short_name = ['DH'], dst_id = OBIO, src_id = OCEAN, __RC__)
#endif

! Children's imports are in the ocean grid and are all satisfied
!   by OGCM from exchange grid quantities.

#ifdef USE_ODAS
#ifdef USE_OBIO
  call MAPL_TerminateImport(GC, SHORT_NAME = ['PS', 'OUSTAR3', 'UU', 'CO2SC', 'DUDP', 'DUWT', 'DUSD', 'BCDP', 'BCWT', 'OCDP', 'OCWT'], CHILD=OBIO, RC=STATUS)
#endif
  VERIFY_(STATUS)
  call MAPL_TerminateImport    ( GC, CHILD=ORAD, RC=STATUS  )
  VERIFY_(STATUS)
  call MAPL_TerminateImport(GC, SHORT_NAME = ['FRACICE', 'TI', 'SI', 'VOLICE', 'VOLSNO', 'ERGICE', 'ERGSNO', 'MPOND', 'HW', 'TW', 'SW', 'TAUAGE'], CHILD = SEAICE, RC=STATUS)
  VERIFY_(STATUS)
  call MAPL_TerminateImport(GC, SHORT_NAME = ['PENUVR', 'PENPAR', 'PENUVF', 'PENPAF', 'TR', 'TRFLUX', 'HW', 'TW', 'SW', 'PS', 'DISCHARGE'], CHILD = OCEAN, RC=STATUS)
  VERIFY_(STATUS)
#else
  call MAPL_TerminateImport    ( GC, ALL=.true., RC=STATUS  )
#endif

! Set the Profiling timers
! ------------------------

  call MAPL_TimerAdd(GC,    name="INITIALIZE"   ,RC=STATUS)
  VERIFY_(STATUS)
  call MAPL_TimerAdd(GC,    name="RUN"          ,RC=STATUS)
  VERIFY_(STATUS)

  call MAPL_TimerAdd(GC, name="InitChild"    ,RC=STATUS)
  VERIFY_(STATUS)

! Allocate this instance of the internal state and put it in wrapper.
! -------------------------------------------------------------------

    allocate( ogcm_internal_state, stat=status )
    VERIFY_(STATUS)
    wrap%ptr => ogcm_internal_state

! Save pointer to the wrapped internal state in the GC
! ----------------------------------------------------

    call ESMF_UserCompSetInternalState ( GC, 'OGCM_state',wrap,status )
    VERIFY_(STATUS)

! Call SetServices 
!------------------

    call MAPL_GenericSetServices    ( GC, RC=STATUS )
    VERIFY_(STATUS)
 
    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: Initialize -- Initialize method for the GEOS Ogcm component

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the Ogcm Composite Gridded Component.
!   It reads the tiling file that defines the exchange grid 
!   It then does a Generic\_Initialize

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm 
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME
    
! Local derived type aliases

    type (MAPL_MetaComp    ), pointer   :: MAPL => null()
    type (MAPL_LocStream       )            :: EXCH
    type (ESMF_State           ), pointer   :: GIM(:) => null()
    type (ESMF_GridComp        ), pointer   :: GCS(:) => null()
    type(ESMF_FIELDBUNDLE      )            :: BUNDLE
    type(ESMF_FIELD            )            :: FIELD
    type(ESMF_Grid             )            :: grid
    integer, pointer, dimension(:)          :: TYPE => null()
    integer                                 :: I
    integer                                 :: J
    integer                                 :: N_CHILDREN
    integer                                 :: COUNTS(3)

    real, pointer                           :: FROCEAN  (:,:) => null()
    real, pointer                           :: LONS     (:  ) => null()
    real, pointer                           :: LATS     (:  ) => null()
    real, pointer                           :: TLONS    (:  ) => null()
    real, pointer                           :: TLATS    (:  ) => null()

    real, pointer                           :: PTR2d   (:,:) => null()
    logical                                 :: found
    logical, allocatable                    :: hasThisImport(:)

    integer                     :: iUseInterp
    logical                     :: UseInterp
    logical :: ACUBE, OCUBE
    integer :: NGRIDS
    integer :: A_IDX, O_IDX
    integer :: ARES, ORES
    integer :: iInterp
    integer, pointer :: GRIDIM(:)=> null()
    integer, pointer :: GRIDJM(:)=> null()
    character(len=ESMF_MAXSTR)          :: GRIDNAME
    character(len=ESMF_MAXSTR), pointer :: GNAMES(:)=> null()

    type (T_OGCM_STATE), pointer        :: ogcm_internal_state => null() 
    type (OGCM_wrap)                    :: wrap

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

    call MAPL_TimerOn(MAPL,"TOTAL"     )
    call MAPL_TimerOn(MAPL,"INITIALIZE")

    call ESMF_UserCompGetInternalState(gc, 'OGCM_state', wrap, status)
    VERIFY_(STATUS)
    ogcm_internal_state => wrap%ptr

! Get the Ocean part of the Xchg grid
!------------------------------------

    call MAPL_GenericMakeXchgNatural(MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(MAPL, GCS = GCS, RC=STATUS )
    VERIFY_(STATUS)

    N_CHILDREN = size(GCS)
    DO I = 1,N_CHILDREN
       DO J = 1, size(FRIENDLY_IMPORTS)
          call MAPL_DoNotAllocateImport(GCS(I), NAME=FRIENDLY_IMPORTS(J))
!ALT we do not pass nor check status since we allow this call for fail
!    for children that do not have this particular import
       END DO
    END DO


! Call Initialize for every Child
!--------------------------------

    call MAPL_TimerOff(MAPL,"TOTAL"     )
    call MAPL_TimerOn(MAPL,"InitChild") 
    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerOff(MAPL,"InitChild")
    call MAPL_TimerOn (MAPL,"TOTAL"     )

! Get info from the Generic state
!--------------------------------

    call MAPL_Get(MAPL,             &
         TILETYPES = TYPE,                       &
         TILELONS  = LONS,                       &
         TILELATS  = LATS,                       &
         GIM       = GIM,                        &
                                       RC=STATUS )
    VERIFY_(STATUS)


! These are static exports
!-------------------------

    call MAPL_GetPointer(EXPORT, TLONS, 'TILELONS',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, TLATS, 'TILELATS',  RC=STATUS)
    VERIFY_(STATUS)

    if(associated(TLONS)) TLONS = LONS
    if(associated(TLATS)) TLATS = LATS

! Manipulate friendly imports
!    get grid and sizes
    call ESMF_GridCompGet(GC, grid=grid, RC=status) 
    VERIFY_(STATUS)
    call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, RC=STATUS)
    VERIFY_(STATUS)

    allocate(hasThisImport(N_CHILDREN), stat=status)
    VERIFY_(STATUS)

    DO J = 1, size(FRIENDLY_IMPORTS)
!      check if this Friendly Import exists in at least one component
       found = .false.
       hasThisImport = .false.

       DO I = 1,N_CHILDREN
          call ESMF_StateGet(GIM(I), FRIENDLY_IMPORTS(J), FIELD, RC=STATUS)
          if (status == ESMF_SUCCESS) then
             found = .true.
             hasThisImport(I) = .true.
          end if
       END DO

       if (.not. found) cycle
       allocate(PTR2D(COUNTS(1), COUNTS(2)), stat=status)
       VERIFY_(STATUS)
       DO I = 1,N_CHILDREN
          if (hasThisImport(I)) then
             call MAPL_SetPointer(GIM(I), PTR2D, NAME=FRIENDLY_IMPORTS(J), RC=STATUS)
             VERIFY_(STATUS)
          end if
       END DO
    END DO
    deallocate(hasThisImport)

! Tag ocean's friendly imports
!-----------------------------

    call ESMF_StateGet (GIM(OCEAN), 'TW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(OCEAN), 'HW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(OCEAN), 'SW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

! Tag seaice's friendly imports
!-----------------------------

    call ESMF_StateGet (GIM(SEAICE), 'TI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    if(DO_CICE_THERMO/=0) then
       call ESMF_StateGet (GIM(SEAICE), 'FRACICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'VOLICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'VOLSNO', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'ERGICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'ERGSNO', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'TAUAGE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_StateGet (GIM(SEAICE), 'MPOND',  FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    call ESMF_StateGet (GIM(SEAICE), 'HI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(SEAICE), 'TW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(SEAICE), 'HW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(SEAICE), 'SW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_StateGet (GIM(SEAICE), 'SI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeSet  (FIELD, NAME="FriendlyToSEAICE", VALUE=.true., RC=STATUS)
    VERIFY_(STATUS)

! Fill the ocean fraction exposed to atmosphere (skin area) 
!   in the childrens import, if they want it
!----------------------------------------------------------

    call MAPL_Get (MAPL, ExchangeGrid=EXCH,   RC=STATUS )
    VERIFY_(STATUS)

    do I = 1, size(GIM)
       call ESMF_StateGet(GIM(I), 'FROCEAN', FIELD, RC=STATUS)
       if (STATUS == ESMF_SUCCESS) then
          call MAPL_GetPointer(GIM(I), FROCEAN, 'FROCEAN',   RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_LocStreamFracArea( EXCH, MAPL_OCEAN, FROCEAN, RC=STATUS) 
          VERIFY_(STATUS)
       end if
    end do

! Put OBIO tracers into the OCEAN's tracer bundle.
!-------------------------------------------------

    if (DO_GUEST/=0) then
       call ESMF_StateGet(GIM(OCEAN), 'TR', BUNDLE, RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GridCompGetFriendlies(GCS(OBIO),"OCEAN", BUNDLE, RC=STATUS )
       VERIFY_(STATUS)
    end if

!   The section below attempts to make an intellegent guess of the default
!   for INTERPOLATE_SST 

!   Get the name of the ocean grid
    call ESMF_GridGet(GRID, name=gridname, rc=status)
    VERIFY_(STATUS)
!   first query the exchange grid for the names of the 2 grids (ATM and OCN)

    call MAPL_LocStreamGet(Exch, GRIDNAMES = GNAMES, &
         GRIDIM=GRIDIM, GRIDJM=GRIDJM, RC=STATUS)
    VERIFY_(STATUS)
!   query exchange grid for ngrids
    ngrids = size(gnames)
    ASSERT_(ngrids==2)

    
!   validate that gridname is there
    found = .false.
    DO I = 1, NGRIDS
       IF (GNAMES(I) == GRIDNAME) THEN
          FOUND = .TRUE.
          exit
       ENDIF
    ENDDO
    ASSERT_(FOUND)

    O_IDX = I
    A_IDX = 3-I
! we pick the "other" gridname (i.e. ATM). 
! this logic works only when ngrids==2; 3-1=2;3-2=1

! Check if any of the grids is on a cubed-sphere
    OCUBE = (  GRIDIM(O_IDX)==6*GRIDJM(O_IDX)) ! MIT Ocean uses "different" cubed-sphere (it is fatter in the IM direction)
    ACUBE = (6*GRIDIM(A_IDX)==  GRIDJM(A_IDX))
    ARES = GRIDIM(A_IDX)
    if( ACUBE ) ARES = 4*ARES
    ORES = GRIDIM(O_IDX)
    if( OCUBE ) then
       ORES = 4*(ORES/6)
    end if

#ifdef __GFORTRAN__
    deallocate(gnames, gridim, gridjm)
#endif

    if (ARES > ORES) then
       ! Ocean grid is coarser; we should interpolate
       iInterp = 1
    else
       ! No interpolation needed
       iInterp = 0
    end if

    call MAPL_GetResource(MAPL, iUseInterp, 'INTERPOLATE_SST:', &
         default=iInterp, RC=STATUS )
    VERIFY_(STATUS)
    useInterp = (iUseInterp /= 0)
    ogcm_internal_state%useInterp = useInterp

! All Done
!---------

    call MAPL_TimerOff(MAPL,"INITIALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"     )

    RETURN_(ESMF_SUCCESS)
  end subroutine Initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!BOP

! !IROUTINE: RUN -- Run method for the Ogcm component

! !INTERFACE:

  subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the ozone mixing ratios.

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

    type (MAPL_MetaComp),      pointer  :: MAPL   => null()
    type (ESMF_GridComp),      pointer  :: GCS(:) => null()
    type (ESMF_State),         pointer  :: GIM(:) => null()
    type (ESMF_State),         pointer  :: GEX(:) => null()
    type (MAPL_LocStream)               :: EXCHGrid
    logical                             :: FRIENDLY
    type(ESMF_FIELD)                    :: FIELD

! Pointers to imports (All tiled)

    real, pointer, dimension(:) :: TAUXW => null()
    real, pointer, dimension(:) :: TAUYW => null()
    real, pointer, dimension(:) :: TAUXI => null()
    real, pointer, dimension(:) :: TAUYI => null()
    real, pointer, dimension(:) :: USTR3 => null()
    real, pointer, dimension(:) :: UU => null()
    real, pointer, dimension(:) :: PS => null()
    real, pointer, dimension(:) :: PENUVR => null()
    real, pointer, dimension(:) :: PENUVF => null()
    real, pointer, dimension(:) :: PENPAR => null()
    real, pointer, dimension(:) :: PENPAF => null()
    real, pointer, dimension(:) :: TW => null()
    real, pointer, dimension(:) :: HW => null()
    real, pointer, dimension(:) :: SW => null()
    real, pointer, dimension(:) :: HI => null()
    real, pointer, dimension(:) :: SI => null()
    real, pointer, dimension(:) :: DISCHARGE => null() 
    real, pointer, dimension(:) :: CO2SC => null()
    real, pointer, dimension(:,:) :: DUDP => null()
    real, pointer, dimension(:,:) :: DUWT => null()
    real, pointer, dimension(:,:) :: DUSD => null()
    real, pointer, dimension(:,:) :: BCDP => null()
    real, pointer, dimension(:,:) :: BCWT => null()
    real, pointer, dimension(:,:) :: OCDP => null()
    real, pointer, dimension(:,:) :: OCWT => null()
    real, pointer, dimension(:,:) :: FSWBAND => null()
    real, pointer, dimension(:,:) :: FSWBANDNA => null()
    real, pointer, dimension(:)   :: TI => null()
    real, pointer, dimension(:)   :: FR => null()
    real, pointer, dimension(:,:) :: TI8 => null()
    real, pointer, dimension(:,:) :: FR8 => null()
    real, pointer, dimension(:,:) :: VOLICE => null()
    real, pointer, dimension(:,:) :: VOLSNO => null()
    real, pointer, dimension(:,:) :: TAUAGE => null()
    real, pointer, dimension(:,:) :: MPOND => null()

    real, pointer, dimension(:,:,:) :: ERGICE => null()
    real, pointer, dimension(:,:,:) :: ERGSNO => null()

    real, pointer, dimension(:) :: TAUXIBOT => null() 
    real, pointer, dimension(:) :: TAUYIBOT => null() 

! Pointers to ocn grid versions

    real, pointer, dimension(:,:) :: TAUXIO => null()
    real, pointer, dimension(:,:) :: TAUYIO => null()
    real, pointer, dimension(:,:) :: TAUXWO => null()
    real, pointer, dimension(:,:) :: TAUYWO => null()
    real, pointer, dimension(:,:) :: USTR3O => null()
    real, pointer, dimension(:,:) :: PSO    => null()
    real, pointer, dimension(:,:) :: USTR3B => null()
    real, pointer, dimension(:,:) :: UUB    => null()
    real, pointer, dimension(:,:) :: PSB    => null()
    real, pointer, dimension(:,:) :: CO2SCB => null()
    real, pointer, dimension(:,:,:) :: DUDPB => null()
    real, pointer, dimension(:,:,:) :: DUWTB => null()
    real, pointer, dimension(:,:,:) :: DUSDB => null()
    real, pointer, dimension(:,:,:) :: BCDPB => null()
    real, pointer, dimension(:,:,:) :: BCWTB => null()
    real, pointer, dimension(:,:,:) :: OCDPB => null()
    real, pointer, dimension(:,:,:) :: OCWTB => null()
    real, pointer, dimension(:,:,:) :: FSWBANDR   => null()
    real, pointer, dimension(:,:,:) :: FSWBANDNAR => null()
    real, pointer, dimension(:,:) :: PENUVRO => null()
    real, pointer, dimension(:,:) :: PENUVFO => null()
    real, pointer, dimension(:,:) :: PENPARO => null()
    real, pointer, dimension(:,:) :: PENPAFO => null()

    real, pointer, dimension(:,:) :: PENUVRM    => null()
    real, pointer, dimension(:,:) :: PENUVFM    => null()
    real, pointer, dimension(:,:) :: PENPARM    => null()
    real, pointer, dimension(:,:) :: PENPAFM    => null()
    real, pointer, dimension(:,:) :: DISCHARGEO => null()
    real, pointer, dimension(:,:) :: TWI => null()
    real, pointer, dimension(:,:) :: HWI => null()
    real, pointer, dimension(:,:) :: SWI => null()
    real, pointer, dimension(:,:) :: TWO => null()
    real, pointer, dimension(:,:) :: HWO => null()
    real, pointer, dimension(:,:) :: SWO => null()
    real, pointer, dimension(:,:) :: UWO => null()
    real, pointer, dimension(:,:) :: VWO => null()

    real, pointer, dimension(:,:) :: HIO => null()
    real, pointer, dimension(:,:) :: SIO => null()
    real, pointer, dimension(:,:) :: UIO => null()
    real, pointer, dimension(:,:) :: VIO => null()
    real, pointer, dimension(:,:) :: KPARO => null()
    real, pointer, dimension(:,:) :: TS_FOUNDO => null()
    real, pointer, dimension(:,:) :: TAUXIBOTO => null() 
    real, pointer, dimension(:,:) :: TAUYIBOTO => null() 
    real, pointer, dimension(:,:) :: UWBO => null()
    real, pointer, dimension(:,:) :: VWBO => null()

    real, pointer, dimension(:,:,:) :: TIO8 => null()
    real, pointer, dimension(:,:,:) :: FRI => null()
    real, pointer, dimension(:,:,:) :: FRO8 => null()
    real, pointer, dimension(:,:)   :: FRO => null()
    real, pointer, dimension(:,:)   :: TIO => null()
    real, pointer, dimension(:,:,:) :: VOLICEO => null()
    real, pointer, dimension(:,:,:) :: VOLSNOO => null()
    real, pointer, dimension(:,:,:) :: TAUAGEO => null()
    real, pointer, dimension(:,:,:) :: MPONDO => null()
    real, pointer, dimension(:,:,:) :: ERGICEO => null()
    real, pointer, dimension(:,:,:) :: ERGSNOO => null()

! Pointers to exports

    real, pointer, dimension(:) :: UW       => null()
    real, pointer, dimension(:) :: VW       => null()
    real, pointer, dimension(:) :: UI       => null()
    real, pointer, dimension(:) :: VI       => null()
    real, pointer, dimension(:) :: KPAR     => null()
    real, pointer, dimension(:) :: TS_FOUND => null()


    integer                     :: N, K
    integer                     :: iUseInterp
    logical                     :: UseInterp

    type (T_OGCM_STATE), pointer        :: ogcm_internal_state => null() 
    type (OGCM_wrap)                    :: wrap

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = 'Run'
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
    call MAPL_TimerOn(MAPL,"RUN"  )

! Get parameters from generic state.
!-----------------------------------

    call MAPL_Get(MAPL,             &
         ExchangeGrid  = ExchGrid,               &
         GIM       = GIM,                        &
         GEX       = GEX,                        &
         GCS       = GCS,                        &
                                       RC=STATUS )
    VERIFY_(STATUS)

! Pointers to imports
!--------------------

    call MAPL_GetPointer(IMPORT, TAUXW   ,  'TAUXW'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TAUYW   ,  'TAUYW'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TAUXI   ,  'TAUXI'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TAUYI   ,  'TAUYI'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, USTR3   ,  'OUSTAR3', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, UU      ,  'UU',      RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, PS      ,  'PS'  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, PENUVR  ,  'PENUVR' , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, PENUVF  ,  'PENUVF' , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, PENPAR  ,  'PENPAR' , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, PENPAF  ,  'PENPAF' , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, DISCHARGE, 'DISCHRG', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, TW      ,  'TW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, HW      ,  'HW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, SW      ,  'SW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, HI      ,  'HI'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, SI      ,  'SI'     , RC=STATUS)
    VERIFY_(STATUS)

    if (DO_CICE_THERMO /= 0) then  
       call MAPL_GetPointer(IMPORT, TI8      ,  'TI'     , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, FR8     , 'FRACICE' , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, VOLICE  , 'VOLICE'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, VOLSNO  , 'VOLSNO'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, ERGICE  , 'ERGICE'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, ERGSNO  , 'ERGSNO'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, TAUAGE  , 'TAUAGE'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, MPOND   , 'MPOND'   , RC=STATUS)
       VERIFY_(STATUS)
    else
       call MAPL_GetPointer(IMPORT, TI       ,  'TI'     , RC=STATUS)
       VERIFY_(STATUS)
    endif 
    
    if(DO_DATAATM==0) then
       call MAPL_GetPointer(IMPORT, CO2SC   ,  'CO2SC'  , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, DUDP    ,  'DUDP'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, DUWT    ,  'DUWT'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, DUSD    ,  'DUSD'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, BCDP    ,  'BCDP'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, BCWT    ,  'BCWT'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, OCDP    ,  'OCDP'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, OCWT    ,  'OCWT'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, FSWBAND   , 'FSWBAND'   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(IMPORT, FSWBANDNA , 'FSWBANDNA' , RC=STATUS)
       VERIFY_(STATUS)
    end if

! Verify that the saltwater variables are friendly to both ocean and seaice
!--------------------------------------------------------------------------

    call ESMF_StateGet (IMPORT, 'TW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'HW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'SW', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToOCEAN" , VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)


! Verify that the saltwater ice variables are friendly to seaice
!---------------------------------------------------------------

    call ESMF_StateGet (IMPORT, 'TI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'SI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    call ESMF_StateGet (IMPORT, 'HI', FIELD, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
    VERIFY_(STATUS)
    ASSERT_(FRIENDLY)

    if(DO_CICE_THERMO/=0) then
       call ESMF_StateGet (IMPORT, 'FRACICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'VOLICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'VOLSNO', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'ERGICE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'ERGSNO', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'TAUAGE', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)

       call ESMF_StateGet (IMPORT, 'MPOND', FIELD, RC=STATUS)
       VERIFY_(STATUS)
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyToSEAICE", VALUE=FRIENDLY, RC=STATUS)
       VERIFY_(STATUS)
       ASSERT_(FRIENDLY)
    end if
    
! Children's Imports
!-------------------

    call MAPL_GetPointer(GIM(SEAICE), TAUXIO  ,  'TAUX', notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(SEAICE), TAUYIO  ,  'TAUY', notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)

    call MAPL_GetPointer(GIM(OCEAN ), TAUXWO  ,  'TAUX',  notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OCEAN ), TAUYWO  ,  'TAUY',  notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OCEAN ), USTR3O  ,  'OUSTAR3', notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OCEAN ), PSO     ,  'PS'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)

    call MAPL_GetPointer(GIM(OBIO ), USTR3B  ,  'OUSTAR3'  , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OBIO ), UUB     ,  'UU'       , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OBIO ), PSB     ,  'PS'       , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)

    if(DO_DATAATM==0) then
       call MAPL_GetPointer(GIM(OBIO ), CO2SCB  ,  'CO2SC'    , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), DUDPB   ,  'DUDP'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), DUWTB   ,  'DUWT'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), DUSDB   ,  'DUSD'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), BCDPB   ,  'BCDP'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), BCWTB   ,  'BCWT'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), OCDPB   ,  'OCDP'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OBIO ), OCWTB   ,  'OCWT'     , notfoundOK=.true., RC=STATUS); VERIFY_(STATUS)

       call MAPL_GetPointer(GIM(ORAD ), FSWBANDR   , 'FSWBAND'   , notfoundOK=.true.,  RC=STATUS); VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(ORAD ), FSWBANDNAR , 'FSWBANDNA' , notfoundOK=.true.,  RC=STATUS); VERIFY_(STATUS)
    end if
    
    if(DO_GUEST/=0) then
       call MAPL_GetPointer(GIM(OCEAN  ), PENUVRM ,  'PENUVR',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OCEAN  ), PENUVFM ,  'PENUVF',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OCEAN  ), PENPARM ,  'PENPAR',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OCEAN  ), PENPAFM ,  'PENPAF',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(OCEAN ), DISCHARGEO, 'DISCHARGE',  RC=STATUS)
       VERIFY_(STATUS)
    end if
    
    call MAPL_GetPointer(GIM(OCEAN ), TWO     ,  'TW'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OCEAN ), HWO     ,  'HW'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(OCEAN ), SWO     ,  'SW'    ,  RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetPointer(GIM(ORAD  ), PENUVRO ,  'PENUVR',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(ORAD  ), PENUVFO ,  'PENUVF',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(ORAD  ), PENPARO ,  'PENPAR',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(ORAD  ), PENPAFO ,  'PENPAF',  RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetPointer(GIM(SEAICE), TWI     ,  'TW'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(SEAICE), HWI     ,  'HW'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(SEAICE), SWI     ,  'SW'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(SEAICE), HIO     ,  'HI'    ,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GIM(SEAICE), SIO     ,  'SI'    ,  RC=STATUS)
    VERIFY_(STATUS)
    if (DO_CICE_THERMO == 0) then  
       call MAPL_GetPointer(GIM(SEAICE), TIO     ,  'TI'    ,  RC=STATUS)
       VERIFY_(STATUS)
    else
       call MAPL_GetPointer(GIM(SEAICE), TIO8    ,  'TI'    ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), FRO8    , 'FRACICE',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), VOLICEO , 'VOLICE' ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), VOLSNOO , 'VOLSNO' ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), ERGICEO , 'ERGICE' ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), ERGSNOO , 'ERGSNO' ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), TAUAGEO , 'TAUAGE' ,  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(GIM(SEAICE), MPONDO  , 'MPOND'  ,  RC=STATUS)
       VERIFY_(STATUS)
    endif

! Transform imports to the ocean grid
!------------------------------------

    if(associated(TAUXIO)) then
       call MAPL_LocStreamTransform( ExchGrid, TAUXIO  ,  TAUXI  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(TAUYIO)) then
       call MAPL_LocStreamTransform( ExchGrid, TAUYIO  ,  TAUYI  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(TAUXWO)) then
       call MAPL_LocStreamTransform( ExchGrid, TAUXWO  ,  TAUXW  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(TAUYWO)) then
       call MAPL_LocStreamTransform( ExchGrid, TAUYWO  ,  TAUYW  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(USTR3O)) then
       call MAPL_LocStreamTransform( ExchGrid, USTR3O  ,  USTR3  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(PSO)) then
       call MAPL_LocStreamTransform( ExchGrid, PSO     ,  PS     , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(USTR3B)) then
       call MAPL_LocStreamTransform( ExchGrid, USTR3B  ,  USTR3  , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(UUB)) then
       call MAPL_LocStreamTransform( ExchGrid, UUB     ,  UU     , RC=STATUS) 
       VERIFY_(STATUS)
    endif
    if(associated(PSB)) then
       call MAPL_LocStreamTransform( ExchGrid, PSB     ,  PS     , RC=STATUS) 
       VERIFY_(STATUS)
    endif

    if(DO_DATAATM==0) then
       if(associated(CO2SCB)) then
          call MAPL_LocStreamTransform( ExchGrid, CO2SCB  ,  CO2SC  , RC=STATUS) 
          VERIFY_(STATUS)
       endif
       if(associated(DUDPB)) then
          do N = 1, NUM_DUDP
             call MAPL_LocStreamTransform( ExchGrid, DUDPB(:,:,N), DUDP(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(DUWTB)) then
          do N = 1, NUM_DUWT
             call MAPL_LocStreamTransform( ExchGrid, DUWTB(:,:,N), DUWT(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(DUSDB)) then
          do N = 1, NUM_DUSD
             call MAPL_LocStreamTransform( ExchGrid, DUSDB(:,:,N), DUSD(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(BCDPB)) then
          do N = 1, NUM_BCDP
             call MAPL_LocStreamTransform( ExchGrid, BCDPB(:,:,N), BCDP(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(BCWTB)) then
          do N = 1, NUM_BCWT
             call MAPL_LocStreamTransform( ExchGrid, BCWTB(:,:,N), BCWT(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(OCDPB)) then
          do N = 1, NUM_OCDP
             call MAPL_LocStreamTransform( ExchGrid, OCDPB(:,:,N), OCDP(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(OCWTB)) then
          do N = 1, NUM_OCWT
             call MAPL_LocStreamTransform( ExchGrid, OCWTB(:,:,N), OCWT(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(FSWBANDR)) then
          do N = 1, NB_CHOU
             call MAPL_LocStreamTransform( ExchGrid, FSWBANDR(:,:,N),   FSWBAND(:,N),   RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
       if(associated(FSWBANDNAR)) then
          do N = 1, NB_CHOU
             call MAPL_LocStreamTransform( ExchGrid, FSWBANDNAR(:,:,N), FSWBANDNA(:,N), RC=STATUS )
             VERIFY_(STATUS)
          end do
       endif
    end if
    
    call MAPL_LocStreamTransform( ExchGrid, PENUVRO,  PENUVR, RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, PENUVFO,  PENUVF, RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, PENPARO,  PENPAR, RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, PENPAFO,  PENPAF, RC=STATUS) 
    VERIFY_(STATUS)

    if(DO_GUEST/=0) then
       call MAPL_LocStreamTransform( ExchGrid, DISCHARGEO, DISCHARGE, RC=STATUS) 
       VERIFY_(STATUS)
       
       PENUVRM= PENUVRO
       PENUVFM= PENUVFO
       PENPARM= PENPARO 
       PENPAFM= PENPAFO
    end if

    call MAPL_LocStreamTransform( ExchGrid, TWO    ,  TW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, HWO    ,  HW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, SWO    ,  SW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, TWI    ,  TW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, HWI    ,  HW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, SWI    ,  SW    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, SIO    ,  SI    , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, HIO    ,  HI    , RC=STATUS)
    VERIFY_(STATUS)

    
    if (DO_CICE_THERMO == 0) then  
      call MAPL_LocStreamTransform( ExchGrid, TIO    ,  TI    , RC=STATUS)
      VERIFY_(STATUS)
    else
       do n=1,NUM_ICE_CATEGORIES 
          call MAPL_LocStreamTransform( ExchGrid, TIO8(:,:,N),  TI8(:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, FRO8(:,:,N),  FR8(:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, VOLICEO(:,:,N),  &
               VOLICE (:,  N),  & 
               RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, VOLSNOO(:,:,N),  &
               VOLSNO (:,  N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, TAUAGEO(:,:,N),  &
               TAUAGE (:,  N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, MPONDO(:,:,N),  &
               MPOND (:,  N), RC=STATUS) 
          VERIFY_(STATUS)
          do k=1,NUM_ICE_LAYERS 
             call MAPL_LocStreamTransform( ExchGrid,                             &  
                  ERGICEO(:,:,NUM_ICE_LAYERS*(N-1)+K),  &
                  ERGICE (:,  K,N), RC=STATUS) 
             VERIFY_(STATUS)
          enddo
          do k=1,NUM_SNOW_LAYERS 
             call MAPL_LocStreamTransform( ExchGrid,                              &
                  ERGSNOO(:,:,NUM_SNOW_LAYERS*(N-1)+K),  &
                  ERGSNO (:,  K,N), RC=STATUS) 
             VERIFY_(STATUS)
          enddo
       enddo
    endif

! Pointers to tile outputs
!-------------------------
    if (DO_CICE_THERMO == 0) then  
       call MAPL_GetPointer(EXPORT, FR      ,  'FRACICE', RC=STATUS)
       VERIFY_(STATUS)
    else
       call MAPL_GetPointer(EXPORT, TAUXIBOT,  'TAUXIBOT', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, TAUYIBOT,  'TAUYIBOT', RC=STATUS)
       VERIFY_(STATUS)
    end if

    call MAPL_GetPointer(EXPORT, UW      ,  'UW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, VW      ,  'VW'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, UI      ,  'UI'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, VI      ,  'VI'     , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, KPAR    ,  'KPAR'   , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT, TS_FOUND  ,  'TS_FOUND' , RC=STATUS)
    VERIFY_(STATUS)

! Mark as needed in the children by allocating
!---------------------------------------------

    if(associated(UW)) then
       call MAPL_GetPointer(GEX(OCEAN ), UWO ,  'UW'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(VW)) then
       call MAPL_GetPointer(GEX(OCEAN ), VWO ,  'VW'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(UI)) then
       call MAPL_GetPointer(GEX(SEAICE), UIO ,  'UI'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(VI)) then
       call MAPL_GetPointer(GEX(SEAICE), VIO ,  'VI'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(KPAR)) then
       call MAPL_GetPointer(GEX(ORAD ), KPARO ,  'KPAR' , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(TS_FOUND)) then
       call MAPL_GetPointer(GEX(OCEAN ), TS_FOUNDO ,  'TS_FOUND' , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(DO_GUEST/=0) then
       call MAPL_GetPointer(GEX(OCEAN ), UWBO ,  'UWB'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
       
       call MAPL_GetPointer(GEX(OCEAN ), VWBO ,  'VWB'    , alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    end if
    
    if (DO_CICE_THERMO == 0) then  
       call MAPL_GetPointer(GEX(SEAICE), FRO  ,  'FRACICE', alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
    else
       call MAPL_GetPointer(GEX(SEAICE), FRI  ,  'FRACICE', alloc=.true., RC=STATUS)
       VERIFY_(STATUS)
       if(associated(TAUXIBOT)) then
          call MAPL_GetPointer(GEX(SEAICE), TAUXIBOTO , 'TAUXBOT' , alloc=.true., RC=STATUS)
          VERIFY_(STATUS)
       end if
       
       if(associated(TAUYIBOT)) then
          call MAPL_GetPointer(GEX(SEAICE), TAUYIBOTO , 'TAUYBOT' , alloc=.true., RC=STATUS)
          VERIFY_(STATUS)
       end if
    endif

    call MAPL_TimerOff(MAPL,"TOTAL"     )
    call MAPL_GenericRun ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerOn (MAPL,"TOTAL"     )

    call ESMF_UserCompGetInternalState(gc, 'OGCM_state', wrap, status)
    VERIFY_(STATUS)
    ogcm_internal_state => wrap%ptr

    useInterp = ogcm_internal_state%useInterp

    call MAPL_LocStreamTransform( ExchGrid, TW     ,  TWO   , &
         INTERP=useInterp, RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, HW     ,  HWO   , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, SW     ,  SWO   , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, SI     ,  SIO   , RC=STATUS) 
    VERIFY_(STATUS)
    call MAPL_LocStreamTransform( ExchGrid, HI     ,  HIO   , RC=STATUS)
    VERIFY_(STATUS)

    if (DO_CICE_THERMO == 0) then  
       call MAPL_LocStreamTransform( ExchGrid, TI     ,  TIO   , RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_LocStreamTransform( ExchGrid, FR     ,  FRO   , &
            INTERP=useInterp, RC=STATUS)
       VERIFY_(STATUS)
    else
       do n=1,NUM_ICE_CATEGORIES
          call MAPL_LocStreamTransform( ExchGrid, TI8(:,N),  TIO8(:,:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, FR8(:,N),  FRO8(:,:,N),  & 
               INTERP=useInterp, RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, VOLICE (:,  N), &
               VOLICEO(:,:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, VOLSNO (:,  N), &
               VOLSNOO(:,:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, TAUAGE (:,  N), &
               TAUAGEO(:,:,N), RC=STATUS) 
          VERIFY_(STATUS)
          call MAPL_LocStreamTransform( ExchGrid, MPOND (:,  N), &
               MPONDO(:,:,N), RC=STATUS) 
          VERIFY_(STATUS)
          do k=1,NUM_ICE_LAYERS 
             call MAPL_LocStreamTransform( ExchGrid, ERGICE (:,  K,N),  &
                  ERGICEO(:,:,NUM_ICE_LAYERS*(N-1)+K),RC=STATUS) 
             VERIFY_(STATUS)
          enddo
          do k=1,NUM_SNOW_LAYERS 
             call MAPL_LocStreamTransform( ExchGrid, ERGSNO (:,  K,N),  &
                  ERGSNOO(:,:,NUM_SNOW_LAYERS*(N-1)+K),RC=STATUS) 
             VERIFY_(STATUS)
          enddo
       enddo
       
       if(associated(TAUXIBOT)) then
          call MAPL_LocStreamTransform( ExchGrid, TAUXIBOT, TAUXIBOTO, RC=STATUS) 
          VERIFY_(STATUS)
       end if
       
       if(associated(TAUYIBOT)) then
          call MAPL_LocStreamTransform( ExchGrid, TAUYIBOT, TAUYIBOTO, RC=STATUS) 
          VERIFY_(STATUS)
       end if
    endif

    call MAPL_GetResource(MAPL, iUseInterp, 'INTERPOLATE_OCEAN_ICE_CURRENTS:', &
         default=0, RC=STATUS )
    VERIFY_(STATUS)
    useInterp = (iUseInterp /= 0)

    if(associated(UW)) then
       call MAPL_LocStreamTransform( ExchGrid, UW     ,  UWO   , &
         INTERP=useInterp, RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(VW)) then
       call MAPL_LocStreamTransform( ExchGrid, VW     ,  VWO   , &
         INTERP=useInterp, RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(UI)) then
       call MAPL_LocStreamTransform( ExchGrid, UI     ,  UIO   , &
         INTERP=useInterp, RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(VI)) then
       call MAPL_LocStreamTransform( ExchGrid, VI     ,  VIO   , &
         INTERP=useInterp, RC=STATUS)
       VERIFY_(STATUS)
    end if

    if(associated(KPAR)) then
       call MAPL_LocStreamTransform( ExchGrid, KPAR   ,  KPARO , RC=STATUS) 
       VERIFY_(STATUS)
    end if

    if(associated(TS_FOUND)) then
       call MAPL_LocStreamTransform( ExchGrid, TS_FOUND   ,  TS_FOUNDO , &
         INTERP=useInterp, RC=STATUS) 
       VERIFY_(STATUS)
    end if

!  All done
!-----------

    call MAPL_TimerOff(MAPL,"RUN" )
    call MAPL_TimerOff(MAPL,"TOTAL")

    RETURN_(ESMF_SUCCESS)

  end subroutine RUN

end module GEOS_OgcmGridCompMod

