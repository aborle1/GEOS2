!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------
!
! ****************************************************************************
! *                                                                          *
! *                             RRTMG_SW                                     *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                 a rapid radiative transfer model                         *
! *                  for the solar spectral region                           *
! *           for application to general circulation models                  *
! *                                                                          *
! *                                                                          *
! *           Atmospheric and Environmental Research, Inc.                   *
! *                       131 Hartwell Avenue                                *
! *                       Lexington, MA 02421                                *
! *                                                                          *
! *                                                                          *
! *                          Eli J. Mlawer                                   *
! *                       Jennifer S. Delamere                               *
! *                        Michael J. Iacono                                 *
! *                        Shepard A. Clough                                 *
! *                       David M. Berthiaume                                *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                                                                          *
! *                      email:  miacono@aer.com                             *
! *                      email:  emlawer@aer.com                             *
! *                      email:  jdelamer@aer.com                            *
! *                                                                          *
! *       The authors wish to acknowledge the contributions of the           *
! *       following people:  Steven J. Taubman, Patrick D. Brown,            *
! *       Ronald E. Farren, Luke Chen, Robert Bergstrom.                     *
! *                                                                          *
! ****************************************************************************
    
    
    
#ifdef _CUDA
#define gpu_device ,device
#else
#define gpu_device 
#endif
    
      module rrtmg_sw_rad

! --------- Modules ---------

      use rrsw_vsn
      use mcica_subcol_gen_sw, only: mcica_sw
      use rrtmg_sw_cldprmc, only: cldprmc_sw
      use rrtmg_sw_setcoef, only: setcoef_sw
      use rrtmg_sw_spcvmc, only: spcvmc_sw

      implicit none


      public :: rrtmg_sw,  earth_sun


    contains
    
     subroutine rrtmg_sw &
            (rpart, ncol    ,nlay    ,icld    , iaer, &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cld, &
             tauc ,ssac ,asmc ,fsfc , &
             ciwp ,clwp ,rei ,rel , &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, &
             nirr    ,nirf    ,parr    ,parf    ,uvrr    ,uvrf , normFlx, &
             zm, alat, numCPUs )




      use parrrsw, only : nbndsw, ngptsw, naerec, nstr, nmol, mxmol, &
                          jpband, jpb1, jpb2, rrsw_scon
      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya
      use rrsw_con, only : heatfac, oneminus, pi,  grav, avogad
      use rrsw_wvn, only : wavenum1, wavenum2
      use rrsw_cld, only : extliq1, ssaliq1, asyliq1, &
                           extice2, ssaice2, asyice2, &
                           extice3, ssaice3, asyice3, fdlice3, &
                           extice4, ssaice4, asyice4, &
                           abari, bbari, cbari, dbari, ebari, fbari
      use rrsw_wvn, only : wavenum2, ngb
      use rrsw_ref, only : preflog, tref
#ifdef _CUDA
      use cudafor
#endif 

      

! ------- Declarations

      integer , intent(in) :: rpart
      integer , intent(in) :: ncol            ! Number of horizontal columns     
      integer , intent(in) :: nlay            ! Number of model layers
      integer , intent(inout) :: icld         ! Cloud overlap method
                                              !    0: Clear only
                                              !    1: Random
                                              !    2: Maximum/random
                                              !    3: Maximum
      integer , intent(in) :: iaer
      real , intent(in) :: play(:,:)          ! Layer pressures (hPa, mb)
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: plev(:,:)          ! Interface pressures (hPa, mb)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(in) :: tlay(:,:)          ! Layer temperatures (K)
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: tlev(:,:)          ! Interface temperatures (K)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(in) :: tsfc(:)            ! Surface temperature (K)
                                              !    Dimensions: (ncol)
      real , intent(in) :: h2ovmr(:,:)        ! H2O volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: o3vmr(:,:)         ! O3 volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: co2vmr(:,:)        ! CO2 volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: ch4vmr(:,:)        ! Methane volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: n2ovmr(:,:)        ! Nitrous oxide volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: o2vmr(:,:)         ! Oxygen volume mixing ratio
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: asdir(:)           ! UV/vis surface albedo direct rad
                                              !    Dimensions: (ncol)
      real , intent(in) :: aldir(:)           ! Near-IR surface albedo direct rad
                                              !    Dimensions: (ncol)
      real , intent(in) :: asdif(:)           ! UV/vis surface albedo: diffuse rad
                                              !    Dimensions: (ncol)
      real , intent(in) :: aldif(:)           ! Near-IR surface albedo: diffuse rad
                                              !    Dimensions: (ncol)

      integer , intent(in) :: dyofyr          ! Day of the year (used to get Earth/Sun
                                              !  distance if adjflx not provided)
      real , intent(in) :: adjes              ! Flux adjustment for Earth/Sun distance
      real , intent(in) :: coszen(:)          ! Cosine of solar zenith angle
                                              !    Dimensions: (ncol)
      real , intent(in) :: scon               ! Solar constant (W/m2)

      integer , intent(in) :: inflgsw         ! Flag for cloud optical properties
      integer , intent(in) :: iceflgsw        ! Flag for ice particle specification
      integer , intent(in) :: liqflgsw        ! Flag for liquid droplet specification

      real , intent(in) :: cld(:,:)           ! Cloud fraction
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: tauc(:,:,:)        ! In-cloud optical depth
                                              !    Dimensions: (ncol,nlay,nbndlw)
      real , intent(in) :: ssac(:,:,:)        ! In-cloud single scattering albedo
                                              !    Dimensions: (ncol,nlay,nbndlw)
      real , intent(in) :: asmc(:,:,:)        ! In-cloud asymmetry parameter
                                              !    Dimensions: (ncol,nlay,nbndlw)
      real , intent(in) :: fsfc(:,:,:)        ! In-cloud forward scattering fraction
                                              !    Dimensions: (ncol,nlay,nbndlw)
      real , intent(in) :: ciwp(:,:)          ! In-cloud ice water path (g/m2)
                                              !    Dimensions: (ncol, nlay)
      real , intent(in) :: clwp(:,:)          ! In-cloud liquid water path (g/m2)
                                              !    Dimensions: (ncol, nlay)
      real , intent(in) :: rei(:,:)           ! Cloud ice effective radius (microns)
                                              !    Dimensions: (ncol, nlay)

      real , intent(in) :: rel(:,:)           ! Cloud water drop effective radius (microns)
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: tauaer(:,:,:)      ! Aerosol optical depth (iaer=10 only)
                                              !    Dimensions: (ncol,nlay,nbndsw)
                                              !  (non-delta scaled)      
      real , intent(in) :: ssaaer(:,:,:)      ! Aerosol single scattering albedo (iaer=10 only)
                                              !    Dimensions: (ncol,nlay,nbndsw)
                                              !  (non-delta scaled)      
      real , intent(in) :: asmaer(:,:,:)      ! Aerosol asymmetry parameter (iaer=10 only)
                                              !    Dimensions: (ncol,nlay,nbndsw)
                                              !  (non-delta scaled)      
      real , intent(in) :: ecaer(:,:,:)       ! Aerosol optical depth at 0.55 micron (iaer=6 only)
                                              !    Dimensions: (ncol,nlay,naerec)
                                              !  (non-delta scaled)      
      integer , intent(in) :: normFlx         ! Normalize fluxes flag
                                              !  0 = no normalization
                                              !  1 = normalize fluxes ( / (scon * coszen) )
      real , intent(in) :: zm(:,:)            ! Heights of level midpoints
                                              !    Dimensions: (ncol,nlay)
      real , intent(in) :: alat(:)            ! Latitude of column
                                              !    Dimensions: (ncol)
      integer , intent(in) :: numCPUs         ! Number of cores per node
                                              
                                              
! ----- Output -----

      real , intent(out) :: swuflx(:,:)       ! Total sky shortwave upward flux (W/m2)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swdflx(:,:)       ! Total sky shortwave downward flux (W/m2)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swhr(:,:)         ! Total sky shortwave radiative heating rate (K/d)
                                              !    Dimensions: (ncol,nlay)
      real , intent(out) :: swuflxc(:,:)      ! Clear sky shortwave upward flux (W/m2)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swdflxc(:,:)      ! Clear sky shortwave downward flux (W/m2)
                                              !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swhrc(:,:)        ! Clear sky shortwave radiative heating rate (K/d)
                                              !    Dimensions: (ncol,nlay)
   ! Output added for Land/Surface process
      real , intent(out) :: nirr(:)           ! Near-IR direct downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)
      real , intent(out) :: nirf(:)           ! Near-IR diffuse downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)
      real , intent(out) :: parr(:)           ! Visible direct downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)
      real , intent(out) :: parf(:)           ! Visible diffuse downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)
      real , intent(out) :: uvrr(:)           ! UV direct downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)
      real , intent(out) :: uvrf(:)           ! UV diffuse downward shortwave flux (w/m2)
                                              !    Dimensions: (ncol)


      integer :: npart, pncol
      

#ifdef _CUDA
      type(cudadeviceprop) :: prop
      real :: gmem
      integer :: err
      integer :: munits
      integer :: numDevices, numCPUsPerGPU
      real :: maxmem
#endif
      
      if (rpart > 0) then
         pncol = rpart
      else

#ifdef _CUDA
 
      err = cudaGetDeviceProperties( prop, 0)
      gmem = prop%totalGlobalMem / (1024.0 * 1024.0)
      !print *, "total GPU global memory is ", gmem , "MB"

      err = cudaGetDeviceCount(numDevices)
      !print *, "total number of GPUs is ", numDevices

      numCPUsPerGPU = ceiling( real(numCPUs) / real(numDevices) )
      !print *, "number of CPUs per GPU is ", numCPUsPerGPU

      maxmem = gmem/real(numCPUsPerGPU)
      !print *, "available GPU global memory per CPU is ", maxmem , "MB"
      
      ! dmb 2013
      ! Here 
      ! The optimal partition size is determined by the following conditions
      ! 1. Powers of 2 are the most efficient.
      ! 2. The second to largest power of 2 that can fit on 
      !    the GPU is most efficient.
      ! 3. Having a small remainder for the final partiion is inefficient.
      
      if (gmem > 5000) then
         pncol = 4096
      else if (gmem > 3000) then
         pncol = 2048
      else if (gmem > 1000) then
         pncol = 1024
      else 
         pncol = 512
      end if

      !print *,"pncol based on gmem is: ", pncol 

      pncol = pncol / numCPUsPerGPU

      !print *,"pncol based on gmem per numCPUsPerGPU is: ", pncol 

      ! the smallest allowed partition size is 32
      do err = 1, 6
          if (pncol > ncol .and. pncol>32) then 
              pncol = pncol/2
          end if
      end do
      
      ! if we have a very large number of columns, account for the 
      ! static ncol memory requirement 
      if (ncol>29000 .and. pncol>4000) then
          pncol = pncol/2
      end if

#else
      pncol = 2
      
#endif 


      !print *, "Final partition size is ", pncol
      end if
      
      
                                                      
      call rrtmg_sw_sub &
            (pncol, ncol, nlay    ,icld    , iaer, &
             play    ,plev    ,tlay    ,tlev    ,tsfc   , &
             h2ovmr , o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr ,o2vmr , &
             asdir   ,asdif   ,aldir   ,aldif   , &
             coszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,cld , &
             tauc ,ssac ,asmc ,fsfc , &
             ciwp ,clwp ,rei ,rel , &
             tauaer  ,ssaaer  ,asmaer  ,ecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, &
             nirr    ,nirf    ,parr    ,parf    ,uvrr    ,uvrf , normFlx, zm, alat )
                               


                                                      
      end subroutine rrtmg_sw                                                     


      subroutine rrtmg_sw_sub &
            (ncol ,gncol,  nlay    ,icld    , iaer, &
             gplay    ,gplev    ,gtlay    ,gtlev    ,gtsfc   , &
             gh2ovmr , go3vmr   ,gco2vmr  ,gch4vmr  ,gn2ovmr ,go2vmr , &
             gasdir   ,gasdif   ,galdir   ,galdif   , &
             gcoszen  ,adjes   ,dyofyr  ,scon    , &
             inflgsw ,iceflgsw,liqflgsw,gcld , &
             gtauc ,gssac ,gasmc ,gfsfc , &
             gciwp ,gclwp ,grei ,grel , &
             gtauaer  ,gssaaer  ,gasmaer  ,gecaer   , &
             swuflx  ,swdflx  ,swhr    ,swuflxc ,swdflxc ,swhrc, &
             nirr    ,nirf    ,parr    ,parf    ,uvrr    ,uvrf  , normFlx, gzm, galat)



      use parrrsw, only : nbndsw, ngptsw, naerec, nstr, nmol, mxmol, &
                          jpband, jpb1, jpb2, rrsw_scon
      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya
      use rrsw_con, only : heatfac, oneminus, pi,  grav, avogad
      use rrsw_wvn, only : wavenum1, wavenum2
      use rrsw_cld, only : extliq1, ssaliq1, asyliq1, &
                           extice2, ssaice2, asyice2, &
                           extice3, ssaice3, asyice3, fdlice3, &
                           extice4, ssaice4, asyice4, &
                           abari, bbari, cbari, dbari, ebari, fbari
      use rrsw_wvn, only : wavenum2, ngb, icxa, nspa, nspb
      use rrsw_ref, only : preflog, tref
      use tab_xcw

      use rrsw_kg16, kao16 => kao, kbo16 => kbo, selfrefo16 => selfrefo, forrefo16 => forrefo, sfluxrefo16 => sfluxrefo
      use rrsw_kg16, ka16 => ka, kb16 => kb, selfref16 => selfref, forref16 => forref, sfluxref16 => sfluxref

      use rrsw_kg17, kao17 => kao, kbo17 => kbo, selfrefo17 => selfrefo, forrefo17 => forrefo, sfluxrefo17 => sfluxrefo
      use rrsw_kg17, ka17 => ka, kb17 => kb, selfref17 => selfref, forref17 => forref, sfluxref17 => sfluxref

      use rrsw_kg18, kao18 => kao, kbo18 => kbo, selfrefo18 => selfrefo, forrefo18 => forrefo, sfluxrefo18 => sfluxrefo
      use rrsw_kg18, ka18 => ka, kb18 => kb, selfref18 => selfref, forref18 => forref, sfluxref18 => sfluxref

      use rrsw_kg19, kao19 => kao, kbo19 => kbo, selfrefo19 => selfrefo, forrefo19 => forrefo, sfluxrefo19 => sfluxrefo
      use rrsw_kg19, ka19 => ka, kb19 => kb, selfref19 => selfref, forref19 => forref, sfluxref19 => sfluxref

      use rrsw_kg20, kao20 => kao, kbo20 => kbo, selfrefo20 => selfrefo, forrefo20 => forrefo, &
         sfluxrefo20 => sfluxrefo, absch4o20 => absch4o
      use rrsw_kg20, ka20 => ka, kb20 => kb, selfref20 => selfref, forref20 => forref, &
        sfluxref20 => sfluxref, absch420 => absch4

      use rrsw_kg21, kao21 => kao, kbo21 => kbo, selfrefo21 => selfrefo, forrefo21 => forrefo, sfluxrefo21 => sfluxrefo
      use rrsw_kg21, ka21 => ka, kb21 => kb, selfref21 => selfref, forref21 => forref, sfluxref21 => sfluxref

      use rrsw_kg22, kao22 => kao, kbo22 => kbo, selfrefo22 => selfrefo, forrefo22 => forrefo, sfluxrefo22 => sfluxrefo
      use rrsw_kg22, ka22 => ka, kb22 => kb, selfref22 => selfref, forref22 => forref, sfluxref22 => sfluxref

      use rrsw_kg23, kao23 => kao, selfrefo23 => selfrefo, forrefo23 => forrefo, sfluxrefo23 => sfluxrefo, raylo23 => raylo
      use rrsw_kg23, ka23 => ka, selfref23 => selfref, forref23 => forref, sfluxref23 => sfluxref, rayl23 => rayl

      use rrsw_kg24, kao24 => kao, kbo24 => kbo, selfrefo24 => selfrefo, forrefo24 => forrefo, sfluxrefo24 => sfluxrefo
      use rrsw_kg24, abso3ao24 => abso3ao, abso3bo24 => abso3bo, raylao24 => raylao, raylbo24 => raylbo
      use rrsw_kg24, ka24 => ka, kb24 => kb, selfref24 => selfref, forref24 => forref, sfluxref24 => sfluxref
      use rrsw_kg24, abso3a24 => abso3a, abso3b24 => abso3b, rayla24 => rayla, raylb24 => raylb

      use rrsw_kg25, kao25 => kao, sfluxrefo25=>sfluxrefo
      use rrsw_kg25, abso3ao25 => abso3ao, abso3bo25 => abso3bo, raylo25 => raylo
      use rrsw_kg25, ka25 => ka, sfluxref25=>sfluxref
      use rrsw_kg25, abso3a25 => abso3a, abso3b25 => abso3b, rayl25 => rayl
     
      use rrsw_kg26, sfluxrefo26 => sfluxrefo
      use rrsw_kg26, sfluxref26 => sfluxref

      use rrsw_kg27, kao27 => kao, kbo27 => kbo, sfluxrefo27 => sfluxrefo, rayl27=>rayl
      use rrsw_kg27, ka27 => ka, kb27 => kb, sfluxref27 => sfluxref, raylo27=>raylo

      use rrsw_kg28, kao28 => kao, kbo28 => kbo, sfluxrefo28 => sfluxrefo
      use rrsw_kg28, ka28 => ka, kb28 => kb, sfluxref28 => sfluxref

      use rrsw_kg29, kao29 => kao, kbo29 => kbo, selfrefo29 => selfrefo, forrefo29 => forrefo, sfluxrefo29 => sfluxrefo
      use rrsw_kg29, absh2oo29 => absh2oo, absco2o29 => absco2o
      use rrsw_kg29, ka29 => ka, kb29 => kb, selfref29 => selfref, forref29 => forref, sfluxref29 => sfluxref
      use rrsw_kg29, absh2o29 => absh2o, absco229 => absco2

! ------- Declarations



      integer , intent(in) :: ncol
      integer , intent(in) :: gncol                   ! Number of horizontal columns     
      integer , intent(in) :: nlay                    ! Number of model layers
      integer , intent(inout) :: icld                 ! Cloud overlap method
                                                      !    0: Clear only
                                                      !    1: Random
                                                      !    2: Maximum/random
                                                      !    3: Maximum
      integer , intent(in) :: iaer
      integer , intent(in) :: dyofyr                  ! Day of the year (used to get Earth/Sun
                                                      !  distance if adjflx not provided)                                                      
      real , intent(in) :: adjes                      ! Flux adjustment for Earth/Sun distance
      real , intent(in) :: scon                       ! Solar constant (W/m2)

      integer , intent(in) :: inflgsw                 ! Flag for cloud optical properties
      integer , intent(in) :: iceflgsw                ! Flag for ice particle specification
      integer , intent(in) :: liqflgsw                ! Flag for liquid droplet specification
      
      real , intent(in) :: gcld(gncol, nlay)          ! Cloud fraction
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gtauc(gncol,nlay,nbndsw)   ! In-cloud optical depth
                                                      !    Dimensions: (ncol,nlay,nbndsw)
      real , intent(in) :: gssac(gncol,nlay,nbndsw)   ! In-cloud single scattering albedo
                                                      !    Dimensions: (ncol,nlay,nbndsw)
      real , intent(in) :: gasmc(gncol,nlay,nbndsw)   ! In-cloud asymmetry parameter
                                                      !    Dimensions: (ncol,nlay,nbndsw)
      real , intent(in) :: gfsfc(gncol,nlay,nbndsw)   ! In-cloud forward scattering fraction
                                                      !    Dimensions: (ncol,nlay,nbndsw)
      real , intent(in) :: gciwp(gncol, nlay)         ! In-cloud ice water path (g/m2)
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gclwp(gncol, nlay)         ! In-cloud liquid water path (g/m2)
                                                      !    Dimensions: (ncol,nlay)
                                                      
      real , intent(in) :: grei(gncol, nlay)          ! Cloud ice effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)

      real , intent(in) :: grel(gncol, nlay)          ! Cloud water drop effective radius (microns)
                                                      !    Dimensions: (ncol,nlay)
                                                      
      
      real , intent(in) :: gplay(gncol,nlay)          ! Layer pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gplev(gncol,nlay+1)        ! Interface pressures (hPa, mb)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(in) :: gtlay(gncol,nlay)          ! Layer temperatures (K)
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gtlev(gncol,nlay+1)        ! Interface temperatures (K)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(in) :: gtsfc(gncol)               ! Surface temperature (K)
                                                      !    Dimensions: (ncol)
      real , intent(in) :: gh2ovmr(gncol,nlay)        ! H2O volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: go3vmr(gncol,nlay)         ! O3 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gco2vmr(gncol,nlay)        ! CO2 volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gch4vmr(gncol,nlay)        ! Methane volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gn2ovmr(gncol,nlay)        ! Nitrous oxide volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: go2vmr(gncol,nlay)         ! Oxygen volume mixing ratio
                                                      !    Dimensions: (ncol,nlay)
      real , intent(in) :: gasdir(gncol)              ! UV/vis surface albedo direct rad
                                                      !    Dimensions: (ncol)
      real , intent(in) :: galdir(gncol)              ! Near-IR surface albedo direct rad
                                                      !    Dimensions: (ncol)
      real , intent(in) :: gasdif(gncol)              ! UV/vis surface albedo: diffuse rad
                                                      !    Dimensions: (ncol)
      real , intent(in) :: galdif(gncol)              ! Near-IR surface albedo: diffuse rad
                                                      !    Dimensions: (ncol)

      
      real , intent(in) :: gcoszen(gncol)             ! Cosine of solar zenith angle
                                                      !    Dimensions: (ncol)
    
      real , intent(in) :: gtauaer(gncol,nlay,nbndsw) ! Aerosol optical depth (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real , intent(in) :: gssaaer(gncol,nlay,nbndsw) ! Aerosol single scattering albedo (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real , intent(in) :: gasmaer(gncol,nlay,nbndsw) ! Aerosol asymmetry parameter (iaer=10 only)
                                                      !    Dimensions: (ncol,nlay,nbndsw)
                                                      ! (non-delta scaled)      
      real , intent(in) :: gecaer(gncol,nlay,naerec)  ! Aerosol optical depth at 0.55 micron (iaer=6 only)
                                                      !    Dimensions: (ncol,nlay,naerec)
                                                      ! (non-delta scaled)      
      integer , intent(in) :: normFlx                 ! Normalize fluxes flag
                                                      !  0 = no normalization
                                                      !  1 = normalize fluxes ( / (scon * coszen) )
      real, intent(in) :: gzm(gncol, nlay)            ! Heights of level midpoints
                                                      !    Dimensions: (ncol,nlay)
      real, intent(in) :: galat(gncol)                ! Latitudes of columns
                                                      !    Dimensions: (ncol)
                                              
! ----- Output -----

      real , intent(out) :: swuflx(:,:)               ! Total sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swdflx(:,:)               ! Total sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swhr(:,:)                 ! Total sky shortwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)
      real , intent(out) :: swuflxc(:,:)              ! Clear sky shortwave upward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swdflxc(:,:)              ! Clear sky shortwave downward flux (W/m2)
                                                      !    Dimensions: (ncol,nlay+1)
      real , intent(out) :: swhrc(:,:)                ! Clear sky shortwave radiative heating rate (K/d)
                                                      !    Dimensions: (ncol,nlay)


   ! Output added for Land/Surface process
      real , intent(out) :: nirr(:)                   ! Near-IR direct downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)
      real , intent(out) :: nirf(:)                   ! Near-IR diffuse downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)
      real , intent(out) :: parr(:)                   ! Visible direct downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)
      real , intent(out) :: parf(:)                   ! Visible diffuse downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)
      real , intent(out) :: uvrr(:)                   ! UV direct downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)
      real , intent(out) :: uvrf(:)                   ! UV diffuse downward shortwave flux (w/m2)
                                                      !    Dimensions: (ncol)

! ----- Local -----

! Control
     
      integer  :: istart              ! beginning band of calculation
      integer  :: iend                ! ending band of calculation
      integer  :: icpr                ! cldprop/cldprmc use flag
      integer  :: iout                ! output option flag
  
      integer  :: idelm               ! delta-m scaling flag
                                      ! [0 = direct and diffuse fluxes are unscaled]
                                      ! [1 = direct and diffuse fluxes are scaled]
                                      ! (total downward fluxes are always delta scaled)
      integer  :: isccos              ! instrumental cosine response flag (inactive)
      integer  :: iplon               ! column loop index
      integer  :: i                   ! layer loop index                       ! jk
      integer  :: ib                  ! band loop index                        ! jsw
      integer  :: ia, ig              ! indices
      integer  :: k                   ! layer loop index
      integer  :: ims                 ! value for changing mcica permute seed
      integer  :: imca                ! flag for mcica [0=off, 1=on]

      real  :: zepsec, zepzen         ! epsilon
      real  :: zdpgcp                 ! flux to heating conversion ratio

! Atmosphere


      real  :: coldry(ncol,nlay+1)          ! dry air column amount
      real  :: wkl(ncol,mxmol,nlay)         ! molecular amounts (mol/cm-2)

      real  :: cossza(ncol)                 ! Cosine of solar zenith angle
      real  :: adjflux(jpband)              ! adjustment for current Earth/Sun distance
      
                                            !  default value of 1368.22 Wm-2 at 1 AU
      real  :: albdir(ncol,nbndsw)          ! surface albedo, direct          ! zalbp
      real  :: albdif(ncol,nbndsw)          ! surface albedo, diffuse         ! zalbd
      
      real  :: rdl(ncol), adl(ncol)

      real  :: CDF(ncol,nlay,ngptsw)
      real  :: CDF2(ncol,nlay,ngptsw)
      real  :: CDF3(ncol,nlay,ngptsw)
      real  :: alpha(ncol,nlay)

 
! Atmosphere - setcoef
      integer  :: laytrop(ncol)             ! tropopause layer index
      integer  :: layswtch(ncol)            ! tropopause layer index
      integer  :: laylow(ncol)              ! tropopause layer index
      integer  :: jp(ncol,nlay+1)           ! 
      integer  :: jt(ncol,nlay+1)           !
      integer  :: jt1(ncol,nlay+1)          !

      real  :: colh2o(ncol,nlay+1)          ! column amount (h2o)
      real  :: colco2(ncol,nlay+1)          ! column amount (co2)
      real  :: colo3(ncol,nlay+1)           ! column amount (o3)
      real  :: coln2o(ncol,nlay+1)          ! column amount (n2o)
      real  :: colch4(ncol,nlay+1)          ! column amount (ch4)
      real  :: colo2(ncol,nlay+1)           ! column amount (o2)
      real  :: colmol(ncol,nlay+1)          ! column amount
      real  :: co2mult(ncol,nlay+1)         ! column amount 

      integer  :: indself(ncol,nlay+1) 
      integer  :: indfor(ncol,nlay+1) 
      real  :: selffac(ncol,nlay+1) 
      real  :: selffrac(ncol,nlay+1) 
      real  :: forfac(ncol,nlay+1) 
      real  :: forfrac(ncol,nlay+1) 

      real  :: fac00(ncol,nlay+1) , fac01(ncol,nlay+1) , &
               fac10(ncol,nlay+1) , fac11(ncol,nlay+1)  
      
      real :: play(ncol,nlay)               ! Layer pressures (hPa, mb)
                                            !    Dimensions: (ncol,nlay)
      real :: plev(ncol,nlay+1)             ! Interface pressures (hPa, mb)
                                            !    Dimensions: (ncol,nlay+1)
      real :: tlay(ncol,nlay)               ! Layer temperatures (K)
                                            !    Dimensions: (ncol,nlay)
      real :: tlev(ncol,nlay+1)             ! Interface temperatures (K)
                                            !    Dimensions: (ncol,nlay+1)
      real :: tsfc(ncol)                    ! Surface temperature (K)
                                            !    Dimensions: (ncol)
                                                      
      real :: coszen(ncol)   

! Atmosphere/clouds - cldprop
      integer  :: ncbands                   ! number of cloud spectral bands
 

      real   :: cld(ncol,nlay)              ! Cloud fraction
      real   :: tauc(ncol,nlay,nbndsw)      ! In-cloud optical depth
      real   :: ssac(ncol,nlay,nbndsw)      ! In-cloud single scattering 
      real   :: asmc(ncol,nlay,nbndsw)      ! In-cloud asymmetry parameter
      real   :: fsfc(ncol,nlay,nbndsw)      ! In-cloud forward scattering fraction
      real   :: ciwp(ncol,nlay)             ! In-cloud ice water path (g/m2)
      real   :: clwp(ncol,nlay)             ! In-cloud liquid water path (g/m2)
      real   :: rei(ncol,nlay)              ! Cloud ice effective radius (microns)
      real   :: rel(ncol,nlay)              ! Cloud water drop effective radius (microns)
      
      real   :: alat(ncol)
      real   :: zm(ncol, nlay)
                                                      
      real, dimension(ncol) :: znirr,znirf,zparr,zparf,zuvrr,zuvrf
      
      real  :: taucmc(ncol,nlay+1,ngptsw)    ! in-cloud optical depth [mcica]
      real  :: taormc(ncol,nlay+1,ngptsw)    ! unscaled in-cloud optical depth [mcica]
      real  :: ssacmc(ncol,nlay+1,ngptsw)    ! in-cloud single scattering albedo [mcica]
      real  :: asmcmc(ncol,nlay+1,ngptsw)    ! in-cloud asymmetry parameter [mcica]
      real  :: fsfcmc(ncol,nlay+1,ngptsw)    ! in-cloud forward scattering fraction [mcica]
      
      
      real :: cldfmcl(ncol,nlay+1,ngptsw)    ! cloud fraction [mcica]
      real :: ciwpmcl(ncol,nlay+1,ngptsw)    ! in-cloud ice water path [mcica]
      real :: clwpmcl(ncol,nlay+1,ngptsw)    ! in-cloud liquid water path [mcica]
                                                     


! Atmosphere/clouds/aerosol - spcvrt,spcvmc
      real  :: ztauc(ncol,nlay+1,nbndsw)     ! cloud optical depth
      real  :: ztaucorig(ncol,nlay+1,nbndsw) ! unscaled cloud optical depth
      real  :: zasyc(ncol,nlay+1,nbndsw)     ! cloud asymmetry parameter 
                                             !  (first moment of phase function)
      real  :: zomgc(ncol,nlay+1,nbndsw)     ! cloud single scattering albedo
   
      real  :: taua(ncol, nlay+1, nbndsw)
      real  :: asya(ncol, nlay+1, nbndsw)
      real  :: omga(ncol, nlay+1, nbndsw)
   

      real  :: zbbfu(ncol,nlay+2)            ! temporary upward shortwave flux (w/m2)
      real  :: zbbfd(ncol,nlay+2)            ! temporary downward shortwave flux (w/m2)
      real  :: zbbcu(ncol,nlay+2)            ! temporary clear sky upward shortwave flux (w/m2)
      real  :: zbbcd(ncol,nlay+2)            ! temporary clear sky downward shortwave flux (w/m2)
      real  :: zbbfddir(ncol,nlay+2)         ! temporary downward direct shortwave flux (w/m2)
      real  :: zbbcddir(ncol,nlay+2)         ! temporary clear sky downward direct shortwave flux (w/m2)
      real  :: zuvfd(ncol,nlay+2)            ! temporary UV downward shortwave flux (w/m2)
      real  :: zuvcd(ncol,nlay+2)            ! temporary clear sky UV downward shortwave flux (w/m2)
      real  :: zuvfddir(ncol,nlay+2)         ! temporary UV downward direct shortwave flux (w/m2)
      real  :: zuvcddir(ncol,nlay+2)         ! temporary clear sky UV downward direct shortwave flux (w/m2)
      real  :: znifd(ncol,nlay+2)            ! temporary near-IR downward shortwave flux (w/m2)
      real  :: znicd(ncol,nlay+2)            ! temporary clear sky near-IR downward shortwave flux (w/m2)
      real  :: znifddir(ncol,nlay+2)         ! temporary near-IR downward direct shortwave flux (w/m2)
      real  :: znicddir(ncol,nlay+2)         ! temporary clear sky near-IR downward direct shortwave flux (w/m2)

! Optional output fields 
      real  :: swnflx(ncol,nlay+2)           ! Total sky shortwave net flux (W/m2)
      real  :: swnflxc(ncol,nlay+2)          ! Clear sky shortwave net flux (W/m2)
      real  :: dirdflux(ncol,nlay+2)         ! Direct downward shortwave surface flux
      real  :: difdflux(ncol,nlay+2)         ! Diffuse downward shortwave surface flux
      real  :: uvdflx(ncol,nlay+2)           ! Total sky downward shortwave flux, UV/vis  
      real  :: nidflx(ncol,nlay+2)           ! Total sky downward shortwave flux, near-IR 
      real  :: dirdnuv(ncol,nlay+2)          ! Direct downward shortwave flux, UV/vis
      real  :: difdnuv(ncol,nlay+2)          ! Diffuse downward shortwave flux, UV/vis
      real  :: dirdnir(ncol,nlay+2)          ! Direct downward shortwave flux, near-IR
      real  :: difdnir(ncol,nlay+2)          ! Diffuse downward shortwave flux, near-IR
      
      real gpu_device :: zgco(ncol,ngptsw,nlay+1), zomco(ncol,ngptsw,nlay+1)  
      real gpu_device :: zrdnd(ncol,ngptsw,nlay+1) 
      real gpu_device :: zref(ncol,ngptsw,nlay+1)  , zrefo(ncol,ngptsw,nlay+1)  
      real gpu_device :: zrefd(ncol,ngptsw,nlay+1)  , zrefdo(ncol,ngptsw,nlay+1)  
      real gpu_device :: ztauo(ncol,ngptsw,nlay)  
      real gpu_device :: zdbt(ncol,ngptsw,nlay+1)  ,ztdbt(ncol,ngptsw,nlay+1)   
      real gpu_device :: ztra(ncol,ngptsw,nlay+1)  , ztrao(ncol,ngptsw,nlay+1)  
      real gpu_device :: ztrad(ncol,ngptsw,nlay+1)  , ztrado(ncol,ngptsw,nlay+1)  
      real gpu_device :: zfd(ncol,ngptsw,nlay+1)  , zfu(ncol,ngptsw,nlay+1)  
      real gpu_device :: zsflxzen(ncol,ngptsw)
      real gpu_device :: ztaur(ncol,nlay,ngptsw), ztaug(ncol,nlay,ngptsw) 

      integer :: npartc, npart, npartb, cldflag(gncol), profic(gncol), profi(gncol)

      real , parameter :: amd = 28.9660     ! Effective molecular weight of dry air (g/mol)
      real , parameter :: amw = 18.0160     ! Molecular weight of water vapor (g/mol)


! Set molecular weight ratios (for converting mmr to vmr)
!  e.g. h2ovmr = h2ommr * amdw)
      real , parameter :: amdw = 1.607793   ! Molecular weight of dry air / water vapor
      real , parameter :: amdc = 0.658114   ! Molecular weight of dry air / carbon dioxide
      real , parameter :: amdo = 0.603428   ! Molecular weight of dry air / ozone
      real , parameter :: amdm = 1.805423   ! Molecular weight of dry air / methane
      real , parameter :: amdn = 0.658090   ! Molecular weight of dry air / nitrous oxide
      real , parameter :: amdo2 = 0.905140  ! Molecular weight of dry air / oxygen

      real , parameter :: sbc = 5.67e-08    ! Stefan-Boltzmann constant (W/m2K4)

      integer  :: isp, l, ix, n, imol       ! Loop indices
      real  :: amm, summol                  ! 
      real  :: adjflx                       ! flux adjustment for Earth/Sun distance
      integer :: prt
      integer :: piplon
      
      integer :: ipart, cols, cole, colr, ncolc, ncolb
      integer :: irng, cc, ncolst
      real :: tt1, tt2

! Initializations
      
      zepsec = 1.e-06 
      zepzen = 1.e-10 
      oneminus = 1.0  - zepsec
      pi = 2.  * asin(1. )
      irng = 0

      istart = jpb1
      iend = jpb2
      iout = 0
      icpr = 1
      ims = 2
      
      adjflx = adjes

      ! MATMAT We supply dyofyr for MCICA exponential cloud
      !        overlap purposes. We are adjusting in the 
      !        Solar Grid Comp. Comment out this section so
      !        that we can have a dyofyr > 0, but not adjust
      !        the solar flux.
      !
      ! MATMAT if (dyofyr .gt. 0) then
      ! MATMAT    adjflx = earth_sun(dyofyr)
      ! MATMAT endif
  
      do ib = jpb1, jpb2
         adjflux(ib) = adjflx * scon / rrsw_scon
      end do

      
    

      if (icld.lt.0.or.icld.gt.4) icld = 1
      
      
    ! determine cloud profile
    cldflag=0
    do iplon = 1, gncol
        if (any(gcld(iplon,:) > 0)) cldflag(iplon)=1
    end do



    ! build profile separation
    cols = 0
    cole = 0


    do iplon = 1, gncol
        if (cldflag(iplon)==1) then
            cole=cole+1
            profi(cole) = iplon
        else
            cols=cols+1
            profic(cols) = iplon
        end if
    end do
    

if (icld==4) then
    call TABULATE_XCW_BETA
end if

        
    
!$acc data copyout(swuflxc, swdflxc, swuflx, swdflx, swnflxc, swnflx, swhrc, swhr) &
!$acc create(laytrop, layswtch, laylow, jp, jt, jt1, &
!$acc co2mult, colch4, colco2, colh2o, colmol, coln2o, &
!$acc colo2, colo3, fac00, fac01, fac10, fac11, &
!$acc selffac, selffrac, indself, forfac, forfrac, indfor, &
!$acc zbbfu, zbbfd, zbbcu, zbbcd,zbbfddir, zbbcddir, zuvfd, zuvcd, zuvfddir, &
!$acc zuvcddir, znifd, znicd, znifddir,znicddir, &
!$acc cldfmcl, ciwpmcl, clwpmcl,  &
!$acc taormc, taucmc, ssacmc, asmcmc, fsfcmc) &
!$acc deviceptr(zref,zrefo,zrefd,zrefdo,&
!$acc ztauo,ztdbt,&
!$acc ztra,ztrao,ztrad,ztrado,&
!$acc zfd,zfu,zdbt,zgco,&
!$acc zomco,zrdnd,ztaug, ztaur,zsflxzen)&
!$acc create(ciwp, clwp, cld, tauc, ssac, asmc, fsfc, rei, rel, rdl, adl) &
!$acc create(play, tlay, plev, tlev, tsfc, cldflag, coszen) &
!$acc create(coldry, wkl, znirr,znirf,zparr,zparf,zuvrr,zuvrf) &
!$acc create(extliq1, ssaliq1, asyliq1, extice2, ssaice2, asyice2) &
!$acc create(extice3, ssaice3, asyice3, fdlice3, abari, bbari, cbari, dbari, ebari, fbari) &
!$acc create(taua, asya, omga,gtauaer,gssaaer,gasmaer, zm, alat) &
!$acc create(CDF, CDF2, CDF3, alpha) &
!$acc copyin(wavenum2, ngb) &
!$acc copyin(tref, preflog, albdif, albdir, cossza)&
!$acc copyin(icxa, adjflux, nspa, nspb)&
!$acc copyin(kao16,kbo16,selfrefo16,forrefo16,sfluxrefo16)&
!$acc copyin(ka16,kb16,selfref16,forref16,sfluxref16)&
!$acc copyin(kao17,kbo17,selfrefo17,forrefo17,sfluxrefo17)&
!$acc copyin(ka17,kb17,selfref17,forref17,sfluxref17)&
!$acc copyin(kao18,kbo18,selfrefo18,forrefo18,sfluxrefo18)&
!$acc copyin(ka18,kb18,selfref18,forref18,sfluxref18)&
!$acc copyin(kao19,kbo19,selfrefo19,forrefo19,sfluxrefo19)&
!$acc copyin(ka19,kb19,selfref19,forref19,sfluxref19)&
!$acc copyin(kao20,kbo20,selfrefo20,forrefo20,sfluxrefo20,absch4o20)&
!$acc copyin(ka20,kb20,selfref20,forref20,sfluxref20,absch420)&
!$acc copyin(kao21,kbo21,selfrefo21,forrefo21,sfluxrefo21)&
!$acc copyin(ka21,kb21,selfref21,forref21,sfluxref21)&
!$acc copyin(kao22,kbo22,selfrefo22,forrefo22,sfluxrefo22)&
!$acc copyin(ka22,kb22,selfref22,forref22,sfluxref22)&
!$acc copyin(kao23,selfrefo23,forrefo23,sfluxrefo23,raylo23)&
!$acc copyin(ka23,selfref23,forref23,sfluxref23,rayl23)&
!$acc copyin(kao24,kbo24,selfrefo24,forrefo24,sfluxrefo24,abso3ao24,abso3bo24,raylao24,raylbo24)&
!$acc copyin(ka24,kb24,selfref24,forref24,sfluxref24,abso3a24,abso3b24,rayla24,raylb24)&
!$acc copyin(kao25,sfluxrefo25,abso3ao25,abso3bo25,raylo25)&
!$acc copyin(ka25,sfluxref25,abso3a25,abso3b25,rayl25)&
!$acc copyin(sfluxrefo26)&
!$acc copyin(sfluxref26,gzm,galat)&
!$acc copyin(kao27,kbo27,sfluxrefo27, raylo27)&
!$acc copyin(ka27,kb27,sfluxref27, rayl27)&
!$acc copyin(kao28,kbo28,sfluxrefo28)&
!$acc copyin(ka28,kb28,sfluxref28,gtauc, gssac, gasmc, gfsfc)&
!$acc copyin(kao29,kbo29,selfrefo29,forrefo29,sfluxrefo29,absh2oo29,absco2o29)&
!$acc copyin(ka29,kb29,selfref29,forref29,sfluxref29,absh2o29,absco229)&
!$acc copyin(gh2ovmr, gco2vmr, go3vmr, gn2ovmr, gch4vmr, go2vmr)&
!$acc copyin(gcld, gciwp, gclwp, grei, grel, gplay, gplev, gtlay, gtlev, gtsfc)&
!$acc copyin(gasdir, galdir, gasdif, galdif,profi,profic,gcoszen)&
!$acc copyout(nirr,nirf,parr,parf,uvrr,uvrf)


!$acc data copyin(XCW) if(icld==4)

!$acc update device(extliq1, ssaliq1, asyliq1, extice2, ssaice2, asyice2) &
!$acc device(extice3, ssaice3, asyice3, fdlice3, abari, bbari, cbari, dbari, ebari, fbari) &
!$acc device(preflog)


ncolc = cols
ncolb = cole

npartc = ceiling( real(ncolc) / real(ncol) )
npartb = ceiling( real(ncolb) / real(ncol) )

!$acc kernels    
    cldfmcl = 0.0
    ciwpmcl = 0.0
    clwpmcl = 0.0     
!$acc end kernels
  
idelm = 1
      

!$acc kernels
taua = 0.0
asya = 0.0
omga = 1.0
!$acc end kernels

if (iaer==10) then

!$acc update device(gtauaer,gssaaer,gasmaer)

end if





      


! PARTITION LOOP ----------------------------------------------------------------------------
do cc = 1, 2

     if (cc==1) then 
         
         npart = npartc
         ncolst = ncolc
     else
        
         npart = npartb
         ncolst = ncolb
         
     end if
     
    
   
      do ipart = 0,npart-1
        cols = ipart * ncol + 1
        cole = (ipart + 1) * ncol
        if (cole>ncolst) cole=ncolst
        colr = cole - cols + 1

!$acc kernels            
            taormc = 0.0 
            taucmc = 0.0
            ssacmc = 1.0
            asmcmc = 0.0
            fsfcmc = 0.0
!$acc end kernels            
 
! Clear cases
      if (cc==1) then    
 !$acc kernels loop private(piplon)
 do iplon = 1, colr
      piplon = profic(iplon + cols - 1)
     
      do ib=1,8
         albdir(iplon,ib)  = galdir(piplon)
         albdif(iplon,ib)  = galdif(piplon)
      enddo
         albdir(iplon,nbndsw)  = galdir(piplon)
         albdif(iplon,nbndsw)  = galdif(piplon)
        !  UV/visible bands 25-28 (10-13), 16000-50000 cm-1, 0.200-0.625 micron
     
      do ib=10,13
         albdir(iplon,ib)  = gasdir(piplon)
         albdif(iplon,ib)  = gasdif(piplon)
      enddo

!  Transition band 9, 12850-16000 cm-1, 0.625-0.778 micron, Take average, dmlee
       albdir(iplon, 9) = (gasdir(piplon)+galdir(piplon))/2.
       albdif(iplon, 9) = (gasdif(piplon)+galdif(piplon))/2.

end do
!$acc end kernels      

!$acc kernels 
do iplon = 1, colr
   
     piplon = profic(iplon + cols - 1)
    
     play(iplon,:) = gplay(piplon, 1:nlay)
     plev(iplon,:) = gplev(piplon, 1:nlay+1)
     tlay(iplon,:) = gtlay(piplon, 1:nlay)
     tlev(iplon,:) = gtlev(piplon, 1:nlay+1)
     tsfc(iplon)   = gtsfc(piplon)
   
end do
!$acc end kernels

if (iaer==10) then
    
!$acc kernels
    do iplon = 1, colr
     piplon = profic(iplon + cols - 1)
     taua(iplon, 1:nlay, :) = gtauaer(piplon, 1:nlay, :)
     asya(iplon, 1:nlay, :) = gasmaer(piplon, 1:nlay, :)
     omga(iplon, 1:nlay, :) = gssaaer(piplon, 1:nlay, :)
   
    end do
!$acc end kernels
    
end if


     
!$acc kernels
do iplon = 1, colr
     piplon = profic(iplon + cols - 1)
     wkl(iplon,1,:) = gh2ovmr(piplon,1:nlay)
     wkl(iplon,2,:) = gco2vmr(piplon,1:nlay)
     wkl(iplon,3,:) = go3vmr(piplon,1:nlay)
     wkl(iplon,4,:) = gn2ovmr(piplon,1:nlay)
     wkl(iplon,5,:) = 0.0
     wkl(iplon,6,:) = gch4vmr(piplon,1:nlay)
     wkl(iplon,7,:) = go2vmr(piplon,1:nlay)   
     coszen(iplon)  = gcoszen(piplon)
     
     
  
   
end do
!$acc end kernels
!************** cloudy cases ***************
      else   
          
 !$acc kernels loop private(piplon)
 do iplon = 1, colr
      piplon = profi(iplon + cols - 1)
     
      do ib=1,8
         albdir(iplon,ib)  = galdir(piplon)
         albdif(iplon,ib)  = galdif(piplon)
      enddo
         albdir(iplon,nbndsw)  = galdir(piplon)
         albdif(iplon,nbndsw)  = galdif(piplon)
        !  UV/visible bands 25-28 (10-13), 16000-50000 cm-1, 0.200-0.625 micron
     
      do ib=10,13
         albdir(iplon,ib)  = gasdir(piplon)
         albdif(iplon,ib)  = gasdif(piplon)
      enddo

!  Transition band 9, 12850-16000 cm-1, 0.625-0.778 micron, Take average, dmlee
       albdir(iplon, 9) = (gasdir(piplon)+galdir(piplon))/2.
       albdif(iplon, 9) = (gasdif(piplon)+galdif(piplon))/2.

end do
!$acc end kernels               
          
!$acc kernels 
do iplon = 1, colr
   
     piplon = profi(iplon + cols - 1)
     
     play(iplon,:) = gplay(piplon, 1:nlay)
     plev(iplon,:) = gplev(piplon, 1:nlay+1)
     tlay(iplon,:) = gtlay(piplon, 1:nlay)
     tlev(iplon,:) = gtlev(piplon, 1:nlay+1)
     tsfc(iplon) = gtsfc(piplon)
     cld(iplon,:) = gcld(piplon, 1:nlay)
     ciwp(iplon,:) = gciwp(piplon, 1:nlay)
     clwp(iplon,:) = gclwp(piplon, 1:nlay)
     rei(iplon,:) = grei(piplon, 1:nlay) 
     rel(iplon,:) = grel(piplon, 1:nlay)
     zm(iplon,:) = gzm(piplon, 1:nlay)
     alat(iplon) = galat(piplon)
end do
!$acc end kernels

if (iaer==10) then

!$acc kernels    
    do iplon = 1, colr
     piplon = profi(iplon + cols - 1)
     taua(iplon, 1:nlay, :) = gtauaer(piplon, 1:nlay, :)
     asya(iplon, 1:nlay, :) = gasmaer(piplon, 1:nlay, :)
     omga(iplon, 1:nlay, :) = gssaaer(piplon, 1:nlay, :)
   
    end do
!$acc end kernels

end if


! Copy the direct cloud optical properties over to the temp arrays
! and then onto the GPU
! We are on the CPU here


!$acc kernels 
do iplon = 1, colr
    piplon = profi(iplon + cols - 1)
     tauc(iplon, 1:nlay, :) = gtauc(piplon, 1:nlay, :)
     ssac(iplon, 1:nlay, :) = gssac(piplon, 1:nlay, :)
     asmc(iplon, 1:nlay, :) = gasmc(piplon, 1:nlay, :)
     fsfc(iplon, 1:nlay, :) = gfsfc(piplon, 1:nlay, :)
end do
!$acc end kernels



!$acc kernels
do iplon = 1, colr
     piplon = profi(iplon + cols - 1)
     wkl(iplon,1,:) = gh2ovmr(piplon,1:nlay)
     wkl(iplon,2,:) = gco2vmr(piplon,1:nlay)
     wkl(iplon,3,:) = go3vmr(piplon,1:nlay)
     wkl(iplon,4,:) = gn2ovmr(piplon,1:nlay)
     wkl(iplon,5,:) = 0.0
     wkl(iplon,6,:) = gch4vmr(piplon,1:nlay)
     wkl(iplon,7,:) = go2vmr(piplon,1:nlay)  
     coszen(iplon)  = gcoszen(piplon)
     
end do
!$acc end kernels
end if


!$acc kernels
do iplon = 1, colr
     cossza(iplon) = max(zepzen,coszen(iplon))
end do
!$acc end kernels  


!$acc kernels
  do iplon = 1,colr
      
      do l = 1,nlay

         coldry(iplon, l) = (plev(iplon, l)-plev(iplon, l+1)) * 1.e3  * avogad / &
                     (1.e2  * grav * ((1.  - wkl(iplon, 1,l)) * amd + wkl(iplon, 1,l) * amw) * &
                     (1.  + wkl(iplon, 1,l)))
    
      end do
    end do
!$acc end kernels

!$acc kernels
  do iplon = 1,colr
      
      do l = 1,nlay
        do imol = 1, nmol
           wkl(iplon,imol,l) = coldry(iplon,l) * wkl(iplon,imol,l)
        end do
       end do
    end do
!$acc end kernels
        

   if (cc==2) then
   call mcica_sw(colr, nlay, ngptsw, icld,irng, play, &
                       cld, clwp, ciwp, tauc, ssac, asmc, fsfc, &
                       cldfmcl, clwpmcl, ciwpmcl, &
                       taucmc, ssacmc, asmcmc, fsfcmc,1,CDF, CDF2, CDF3, alpha, zm, &
                       alat, dyofyr, rdl, adl)
   end if   
  

  
   if (cc==2) then
   call cldprmc_sw(colr, nlay, inflgsw, iceflgsw, liqflgsw,  &
                         cldfmcl , ciwpmcl , clwpmcl , rei, rel, &
                         taormc, taucmc, ssacmc, asmcmc, fsfcmc)
   end if
  


   call setcoef_sw(colr, nlay, play , tlay , plev , tlev , tsfc , &
                        coldry , wkl , &
                         laytrop, layswtch, laylow, jp , jt , jt1 , &
                         co2mult , colch4 , colco2 , colh2o , colmol , coln2o , &
                         colo2 , colo3 , fac00 , fac01 , fac10 , fac11 , &
                         selffac , selffrac , indself , forfac , forfrac , indfor )


   
   
   call spcvmc_sw(cc,ncol, colr, nlay, istart, iend, icpr, idelm, iout, &
              play, tlay, plev, tlev, &
              tsfc, albdif, albdir, &
              cldfmcl, taucmc, asmcmc, ssacmc, taormc, &
              taua, asya, omga,cossza, coldry, adjflux, &
              laytrop, layswtch, laylow, jp, jt, jt1, &
              co2mult, colch4, colco2, colh2o, colmol, &
              coln2o, colo2, colo3, &
              fac00, fac01, fac10, fac11, &
              selffac, selffrac, indself, forfac, forfrac, indfor, &
              zbbfd, zbbfu, zbbcd, zbbcu, zuvfd, &
              zuvcd, znifd, znicd, &
              zbbfddir, zbbcddir, zuvfddir, zuvcddir, znifddir, znicddir,&
              zgco,zomco,zrdnd,zref,zrefo,zrefd,zrefdo,ztauo,zdbt,ztdbt,&
              ztra,ztrao,ztrad,ztrado,zfd,zfu,ztaug, ztaur, zsflxzen,&
              znirr,znirf,zparr,zparf,zuvrr,zuvrf)



   
! Transfer up and down, clear and total sky fluxes to output arrays.
! Vertical indexing goes from bottom to top; reverse here for GCM if necessary.



if (cc==1) then
!$acc kernels loop independent
    do iplon = 1, colr
         piplon = profic(iplon + cols - 1)
        
         do i = 1, nlay+1
             

            swuflxc(piplon,i) = zbbcu(iplon,i) 
            swdflxc(piplon,i) = zbbcd(iplon,i) 
            swuflx(piplon,i) = zbbfu(iplon,i) 
            swdflx(piplon,i) = zbbfd(iplon,i) 

         enddo

!  Total and clear sky net fluxes

         do i = 1, nlay+1
            swnflxc(iplon,i)  = swdflxc(piplon,i) - swuflxc(piplon,i)
            swnflx(iplon,i)  = swdflx(piplon,i) - swuflx(piplon,i)
         enddo

!  Total and clear sky heating rates

         do i = 1, nlay
            zdpgcp = heatfac / (plev(iplon, i) - plev(iplon, i+1))
            swhrc(piplon,i) = (swnflxc(iplon,i+1)  - swnflxc(iplon,i) ) * zdpgcp
            swhr(piplon,i) = (swnflx(iplon,i+1)  - swnflx(iplon,i) ) * zdpgcp
         enddo
         swhrc(piplon,nlay) = 0. 
         swhr(piplon,nlay) = 0. 
       
! End longitude loop
      enddo
!$acc end kernels 

!$acc kernels loop independent
do iplon = 1, colr
         piplon = profic(iplon + cols - 1)
         nirr(piplon) = znirr(iplon)
         nirf(piplon) = znirf(iplon) - znirr(iplon)
         parr(piplon) = zparr(iplon)
         parf(piplon) = zparf(iplon) - zparr(iplon)
         uvrr(piplon) = zuvrr(iplon)
         uvrf(piplon) = zuvrf(iplon) - zuvrr(iplon)
         
end do
!$acc end kernels 
else
!$acc kernels loop independent
    do iplon = 1, colr
         piplon = profi(iplon + cols - 1)

         do i = 1, nlay+1
             

            swuflxc(piplon,i) = zbbcu(iplon,i) 
            swdflxc(piplon,i) = zbbcd(iplon,i) 
            swuflx(piplon,i) = zbbfu(iplon,i) 
            swdflx(piplon,i) = zbbfd(iplon,i) 

         enddo

!  Total and clear sky net fluxes

         do i = 1, nlay+1
            swnflxc(iplon,i)  = swdflxc(piplon,i) - swuflxc(piplon,i)
            swnflx(iplon,i)  = swdflx(piplon,i) - swuflx(piplon,i)
         enddo

!  Total and clear sky heating rates

         do i = 1, nlay
            zdpgcp = heatfac / (plev(iplon, i) - plev(iplon, i+1))
            swhrc(piplon,i) = (swnflxc(iplon,i+1)  - swnflxc(iplon,i) ) * zdpgcp
            swhr(piplon,i) = (swnflx(iplon,i+1)  - swnflx(iplon,i) ) * zdpgcp
         enddo
         swhrc(piplon,nlay) = 0. 
         swhr(piplon,nlay) = 0. 
         
! End longitude loop
      enddo
!$acc end kernels 

!$acc kernels loop independent
do iplon = 1, colr
         piplon = profi(iplon + cols - 1)
         nirr(piplon) = znirr(iplon)
         nirf(piplon) = znirf(iplon) - znirr(iplon)
         parr(piplon) = zparr(iplon)
         parf(piplon) = zparf(iplon) - zparr(iplon)
         uvrr(piplon) = zuvrr(iplon)
         uvrf(piplon) = zuvrf(iplon) - zuvrr(iplon)
         
end do
!$acc end kernels 
end if

        

     
end do
        

     
end do

! If the user requested 'normalized' fluxes, then here we
! divide the fluxes by the solar constant divided by coszen

if (normFlx==1) then
!$acc kernels
do k = 1, nlay+1
swuflxc(:,k)=swuflxc(:,k)/(scon*max(zepzen,gcoszen(:)))
swdflxc(:,k)=swdflxc(:,k)/(scon*max(zepzen,gcoszen(:)))
swuflx(:,k)=swuflx(:,k)/(scon*max(zepzen,gcoszen(:)))
swdflx(:,k)=swdflx(:,k)/(scon*max(zepzen,gcoszen(:)))
end do
nirr(:)=nirr(:)/(scon*max(zepzen,gcoszen(:)))
nirf(:)=nirf(:)/(scon*max(zepzen,gcoszen(:)))
parr(:)=parr(:)/(scon*max(zepzen,gcoszen(:)))
parf(:)=parf(:)/(scon*max(zepzen,gcoszen(:)))
uvrr(:)=uvrr(:)/(scon*max(zepzen,gcoszen(:)))
uvrf(:)=uvrf(:)/(scon*max(zepzen,gcoszen(:)))
!$acc end kernels
end if

! end of data statement for xcw with icld==4
!$acc end data

!$acc end data

end subroutine rrtmg_sw_sub

!*************************************************************************
      real  function earth_sun(idn)
!*************************************************************************
!
!  Purpose: Function to calculate the correction factor of Earth's orbit
!  for current day of the year

!  idn        : Day of the year
!  earth_sun  : square of the ratio of mean to actual Earth-Sun distance

! ------- Modules -------

      use rrsw_con, only : pi

      integer , intent(in) :: idn

      real  :: gamma

      gamma = 2. *pi*(idn-1)/365. 

! Use Iqbal's equation 1.2.1

      earth_sun = 1.000110  + .034221  * cos(gamma) + .001289  * sin(gamma) + &
                   .000719  * cos(2. *gamma) + .000077  * sin(2. *gamma)

      end function earth_sun

      end module rrtmg_sw_rad


