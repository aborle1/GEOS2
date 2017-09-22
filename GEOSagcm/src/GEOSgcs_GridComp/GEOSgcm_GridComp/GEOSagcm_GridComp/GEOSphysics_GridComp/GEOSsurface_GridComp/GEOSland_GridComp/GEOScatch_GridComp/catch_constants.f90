
module catch_constants

  ! reichle+koster, 2007
  ! reichle, 10 Oct 2008 - added "echo_catch_constants()"
  ! reichle, 28 Oct 2010 - moved DZ, SHR, PHI, FSN from subroutines gndtp0() and gndtmp()
  !                      - moved FWETL, FWETC from subroutine interc()
  !                      - renamed N_gndtmp -> N_gt
  ! reichle, 23 Nov 2010 - replaced PHIGT with POROS(N), ALHMGT with ALHM
  !                      - replaced constants with values from MAPL_Constants.F90
  !                        where possible
  ! reichle, 30 Nov 2010 - zero-diff revisions and clean-up for off-line (land-only) MERRA
  !                        replay capability
  !                         - restored PHIGT, ALHMGT 
  !                         - moved MIN_SNOW_MASS->MINSWE, DZ1MAX, and SATCAPFR to 
  !                            catch_constants
  !                         - moved "small" back from catch_constants() into snowrt()
  ! reichle, 14 Aug 2014 - moved constants that are only needed in subroutine catchment() 
  !                         to catchment.F90 where they are now private to the F90 module 
  !                         "catchment_model"
  !                      - removed all constants that come straight from MAPL_Constants
  !                      - renamed all remaining public constants by prefacing with "catch_*"
  !                      - moved "echo_catch_constants()" to catchment.F90 (and renamed)
  
  implicit none
  private
  
  ! ---------------------------------------------------------------------------

  INTEGER, PARAMETER, PUBLIC :: CATCH_N_SNOW   = 3      ! # layers in snow model
  INTEGER, PARAMETER, PUBLIC :: CATCH_N_GT     = 6      ! # layers in ground temperature model

  ! ---------------------------------------------------------------------------
  !
  ! constants for use with snowrt() and snow_albedo() of module StieglitzSnow
  
  REAL,    PARAMETER, PUBLIC :: CATCH_SNWALB_RHOFS  = 150.    ! kg/m^3  
  REAL,    PARAMETER, PUBLIC :: CATCH_SNWALB_VISMAX = 0.7     !  
  REAL,    PARAMETER, PUBLIC :: CATCH_SNWALB_NIRMAX = 0.5     ! 
  REAL,    PARAMETER, PUBLIC :: CATCH_SNWALB_SLOPE  = -0.0006 ! 
  REAL,    PARAMETER, PUBLIC :: CATCH_MAXSNDEPTH    = 1.e20   ! 
  REAL,    PARAMETER, PUBLIC :: CATCH_DZ1MAX        = 0.08    ! m   
  
  ! ---------------------------------------------------------------------------
  !
  ! constants for snow constituents (dust, carbon, etc.)

  INTEGER, PARAMETER, PUBLIC :: CATCH_N_CONSTIT = 9  ! number of constituents followed
  
! absorption coefs, visible
  REAL,    PARAMETER, DIMENSION(CATCH_N_CONSTIT), PUBLIC :: CATCH_ABVIS = & 
                       (/ 0.045,      &   ! Dust1 
                          0.041,      &   ! Dust2
                          0.038,      &   ! Dust3
                          0.032,      &   ! Dust4
                          0.026,      &   ! Dust5
                          7.718,      &   ! Black carbon hydrophobic
                         11.646,      &   ! Black carbon hydrophilic
                          0.133,      &   ! Organic carbon hydrophobic
                          0.118/)         ! Organic carbon hydrophic

! absorption coefs, near-infrared
  REAL,    PARAMETER, DIMENSION(CATCH_N_CONSTIT), PUBLIC :: CATCH_ABNIR = &
                       (/ 0.011,      &   ! Dust1   	
                          0.012,      &   ! Dust2
                          0.013,      &   ! Dust3
                          0.013,      &   ! Dust4
                          0.010,      &   ! Dust5
                          3.804,      &   ! Black carbon hydrophobic
                          5.473,      &   ! Black carbon hydrophilic
                          0.105,      &   ! Organic carbon hydrophobic
                          0.132 /)        ! Organic carbon hydrophic

end module catch_constants
