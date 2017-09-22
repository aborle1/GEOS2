!     path:      $Source$
!     author:    $Author$
!     revision:  $Revision$
!     created:   $Date$

      module rrtmg_sw_init

!  --------------------------------------------------------------------------
! |                                                                          |
! |  Copyright 2002-2009, Atmospheric & Environmental Research, Inc. (AER).  |
! |  This software may be used, copied, or redistributed as long as it is    |
! |  not sold and this copyright notice is reproduced on each copy made.     |
! |  This model is provided as is without any express or implied warranties. |
! |                       (http://www.rtweb.aer.com/)                        |
! |                                                                          |
!  --------------------------------------------------------------------------

! ------- Modules -------
      !use parkind, only : im => kind , rb => kind 
      use rrsw_wvn
      use rrtmg_sw_setcoef, only: swatmref
      
      implicit none

      public rrtmg_sw_ini
      
      contains

! **************************************************************************
      subroutine rrtmg_sw_ini(cpdair)
! **************************************************************************
!
!  Original version:   Michael J. Iacono; February, 2004
!  Revision for F90 formatting:  M. J. Iacono, July, 2006
!
!  This subroutine performs calculations necessary for the initialization
!  of the shortwave model.  Lookup tables are computed for use in the SW
!  radiative transfer, and input absorption coefficient data for each
!  spectral band are reduced from 224 g-point intervals to 112.
! **************************************************************************

      use parrrsw, only : mg, nbndsw, ngptsw
      use rrsw_tbl, only: ntbl, tblint, pade, bpade, tau_tbl, exp_tbl
      use rrsw_vsn, only: hvrini, hnamini

      real , intent(in) :: cpdair     ! Specific heat capacity of dry air
                                              ! at constant pressure at 273 K
                                              ! (J kg-1 K-1)

! ------- Local -------

      integer  :: ibnd, igc, ig, ind, ipr
      integer  :: igcsm, iprsm
      integer  :: itr

      real  :: wtsum, wtsm(mg)
      real  :: tfn

      real , parameter :: expeps = 1.e-20    ! Smallest value for exponential table

! ------- Definitions -------
!     Arrays for 10000-point look-up tables:
!     TAU_TBL  Clear-sky optical depth 
!     EXP_TBL  Exponential lookup table for transmittance
!     PADE     Pade approximation constant (= 0.278)
!     BPADE    Inverse of the Pade approximation constant
!

      hvrini = '$Revision$'

! Initialize model data
      call swdatinit(cpdair)
      call swcmbdat              ! g-point interval reduction data
      call swaerpr               ! aerosol optical properties
      call swcldpr               ! cloud optical properties
      call swatmref              ! reference MLS profile
      call sw_kgb16              ! molecular absorption coefficients
      call sw_kgb17
      call sw_kgb18
      call sw_kgb19
      call sw_kgb20
      call sw_kgb21
      call sw_kgb22
      call sw_kgb23
      call sw_kgb24
      call sw_kgb25
      call sw_kgb26
      call sw_kgb27
      call sw_kgb28
      call sw_kgb29

! Define exponential lookup tables for transmittance. Tau is
! computed as a function of the tau transition function, and transmittance 
! is calculated as a function of tau.  All tables are computed at intervals 
! of 0.0001.  The inverse of the constant used in the Pade approximation to 
! the tau transition function is set to bpade.

      exp_tbl(0) = 1.0 
      exp_tbl(ntbl) = expeps
      bpade = 1.0  / pade
      do itr = 1, ntbl-1
         tfn = float(itr) / float(ntbl)
         tau_tbl = bpade * tfn / (1.  - tfn)
         exp_tbl(itr) = exp(-tau_tbl)
         if (exp_tbl(itr) .le. expeps) exp_tbl(itr) = expeps
      enddo

! Perform g-point reduction from 16 per band (224 total points) to
! a band dependent number (112 total points) for all absorption
! coefficient input data and Planck fraction input data.
! Compute relative weighting for new g-point combinations.

      igcsm = 0
      do ibnd = 1,nbndsw
         iprsm = 0
         if (ngc(ibnd).lt.mg) then
            do igc = 1,ngc(ibnd)
               igcsm = igcsm + 1
               wtsum = 0.
               do ipr = 1, ngn(igcsm)
                  iprsm = iprsm + 1
                  wtsum = wtsum + wt(iprsm)
               enddo
               wtsm(igc) = wtsum
            enddo
            do ig = 1, ng(ibnd+15)
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = wt(ig)/wtsm(ngm(ind))
            enddo
         else
            do ig = 1, ng(ibnd+15)
               igcsm = igcsm + 1
               ind = (ibnd-1)*mg + ig
               rwgt(ind) = 1.0 
            enddo
         endif
      enddo

! Reduce g-points for absorption coefficient data in each LW spectral band.

      call cmbgb16s
      call cmbgb17
      call cmbgb18
      call cmbgb19
      call cmbgb20
      call cmbgb21
      call cmbgb22
      call cmbgb23
      call cmbgb24
      call cmbgb25
      call cmbgb26
      call cmbgb27
      call cmbgb28
      call cmbgb29

      end subroutine rrtmg_sw_ini

!***************************************************************************
      subroutine swdatinit(cpdair)
!***************************************************************************

! --------- Modules ----------

      use rrsw_con, only: heatfac, grav, planck, boltz, &
                          clight, avogad, alosmt, gascon, radcn1, radcn2, &
                          sbcnst, secdy 
      use rrsw_vsn

      save 
 
      real , intent(in) :: cpdair     ! Specific heat capacity of dry air
                                              ! at constant pressure at 273 K
                                              ! (J kg-1 K-1)

! Shortwave spectral band limits (wavenumbers)
      wavenum1(:) = (/2600. , 3250. , 4000. , 4650. , 5150. , 6150. , 7700. , &
                      8050. ,12850. ,16000. ,22650. ,29000. ,38000. ,  820. /)
      wavenum2(:) = (/3250. , 4000. , 4650. , 5150. , 6150. , 7700. , 8050. , &
                     12850. ,16000. ,22650. ,29000. ,38000. ,50000. , 2600. /)
      delwave(:) =  (/ 650. ,  750. ,  650. ,  500. , 1000. , 1550. ,  350. , &
                      4800. , 3150. , 6650. , 6350. , 9000. ,12000. , 1780. /)
     
      
      icxa(:) = (/ 5 ,5 ,4 ,4 ,3 ,3 ,2 ,2 ,1 ,1 ,1 ,1 ,1 ,5/)
! Spectral band information
      ng(:) = (/16,16,16,16,16,16,16,16,16,16,16,16,16,16/)
      nspa(:) = (/9,9,9,9,1,9,9,1,9,1,0,1,9,1/)
      nspb(:) = (/1,5,1,1,1,5,1,0,1,0,0,1,5,1/)

! Fundamental physical constants from NIST 2002

      grav = 9.8066                         ! Acceleration of gravity
                                              ! (m s-2)
      planck = 6.62606876e-27               ! Planck constant
                                              ! (ergs s; g cm2 s-1)
      boltz = 1.3806503e-16                 ! Boltzmann constant
                                              ! (ergs K-1; g cm2 s-2 K-1)
      clight = 2.99792458e+10               ! Speed of light in a vacuum  
                                              ! (cm s-1)
      avogad = 6.02214199e+23               ! Avogadro constant
                                              ! (mol-1)
      alosmt = 2.6867775e+19                ! Loschmidt constant
                                              ! (cm-3)
      gascon = 8.31447200e+07               ! Molar gas constant
                                              ! (ergs mol-1 K-1)
      radcn1 = 1.191042772e-12              ! First radiation constant
                                              ! (W cm2 sr-1)
      radcn2 = 1.4387752                    ! Second radiation constant
                                              ! (cm K)
      sbcnst = 5.670400e-04                 ! Stefan-Boltzmann constant
                                              ! (W cm-2 K-4)
      secdy = 8.6400e4                      ! Number of seconds per day
                                              ! (s d-1)
!
!     units are generally cgs
!
!     The first and second radiation constants are taken from NIST.
!     They were previously obtained from the relations:
!          radcn1 = 2.*planck*clight*clight*1.e-07
!          radcn2 = planck*clight/boltz

!     Heatfac is the factor by which delta-flux / delta-pressure is
!     multiplied, with flux in W/m-2 and pressure in mbar, to get 
!     the heating rate in units of degrees/day.  It is equal to:
!     Original value:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.004) is in units of J g-1 K-1, and the 
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.8066)(86400)(1e-5)/(1.004)
!      heatfac = 8.4391 
!
!     Modified value for consistency with CAM3:
!           (g)x(#sec/day)x(1e-5)/(specific heat of air at const. p)
!           Here, cpdair (1.00464) is in units of J g-1 K-1, and the
!           constant (1.e-5) converts mb to Pa and g-1 to kg-1.
!        =  (9.80616)(86400)(1e-5)/(1.00464)
!      heatfac = 8.43339130434 
!
!     Calculated value (from constants above and input cpdair)
!        (grav) x (#sec/day) / (specific heat of dry air at const. p x 1.e2)
!           Here, cpdair is in units of J kg-1 K-1, and the constant (1.e2) 
!           converts mb to Pa when heatfac is multiplied by W m-2 mb-1. 
      heatfac = grav * secdy / (cpdair * 1.e2 )

      end subroutine swdatinit

!***************************************************************************
      subroutine swcmbdat
!***************************************************************************

      save
 
! ------- Definitions -------
!     Arrays for the g-point reduction from 224 to 112 for the 16 LW bands:
!     This mapping from 224 to 112 points has been carefully selected to 
!     minimize the effect on the resulting fluxes and cooling rates, and
!     caution should be used if the mapping is modified.  The full 224
!     g-point set can be restored with ngpt=224, ngc=16*16, ngn=224*1., etc.
!     ngpt    The total number of new g-points
!     ngc     The number of new g-points in each band
!     ngs     The cumulative sum of new g-points for each band
!     ngm     The index of each new g-point relative to the original
!             16 g-points for each band.  
!     ngn     The number of original g-points that are combined to make
!             each new g-point in each band.
!     ngb     The band index for each new g-point.
!     wt      RRTM weights for 16 g-points.

! Use this set for 112 quadrature point (g-point) model
! ------- Data statements -------
      ngc(:) = (/ 6,12, 8, 8,10,10, 2,10, 8, 6, 6, 8, 6,12 /)
      ngs(:) = (/ 6,18,26,34,44,54,56,66,74,80,86,94,100,112 /)
      ngm(:) = (/ 1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6, &           ! band 16
                  1,2,3,4,5,6,6,7,8,8,9,10,10,11,12,12, &      ! band 17
                  1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8, &           ! band 18
                  1,2,3,4,5,5,6,6,7,7,7,7,8,8,8,8, &           ! band 19
                  1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10, &     ! band 20
                  1,2,3,4,5,6,7,8,9,9,10,10,10,10,10,10, &     ! band 21
                  1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2, &           ! band 22
                  1,1,2,2,3,4,5,6,7,8,9,9,10,10,10,10, &       ! band 23
                  1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8, &           ! band 24
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 25
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 26
                  1,2,3,4,5,6,7,7,7,7,8,8,8,8,8,8, &           ! band 27
                  1,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6, &           ! band 28
                  1,2,3,4,5,5,6,6,7,7,8,8,9,10,11,12 /)        ! band 29
      ngn(:) = (/ 2,2,2,2,4,4, &                               ! band 16
                  1,1,1,1,1,2,1,2,1,2,1,2, &                   ! band 17
                  1,1,1,1,2,2,4,4, &                           ! band 18
                  1,1,1,1,2,2,4,4, &                           ! band 19
                  1,1,1,1,1,1,1,1,2,6, &                       ! band 20
                  1,1,1,1,1,1,1,1,2,6, &                       ! band 21
                  8,8, &                                       ! band 22
                  2,2,1,1,1,1,1,1,2,4, &                       ! band 23
                  2,2,2,2,2,2,2,2, &                           ! band 24
                  1,1,2,2,4,6, &                               ! band 25
                  1,1,2,2,4,6, &                               ! band 26
                  1,1,1,1,1,1,4,6, &                           ! band 27
                  1,1,2,2,4,6, &                               ! band 28
                  1,1,1,1,2,2,2,2,1,1,1,1 /)                   ! band 29
      ngb(:) = (/ 16,16,16,16,16,16, &                         ! band 16
                  17,17,17,17,17,17,17,17,17,17,17,17, &       ! band 17
                  18,18,18,18,18,18,18,18, &                   ! band 18
                  19,19,19,19,19,19,19,19, &                   ! band 19
                  20,20,20,20,20,20,20,20,20,20, &             ! band 20
                  21,21,21,21,21,21,21,21,21,21, &             ! band 21
                  22,22, &                                     ! band 22
                  23,23,23,23,23,23,23,23,23,23, &             ! band 23
                  24,24,24,24,24,24,24,24, &                   ! band 24
                  25,25,25,25,25,25, &                         ! band 25
                  26,26,26,26,26,26, &                         ! band 26
                  27,27,27,27,27,27,27,27, &                   ! band 27
                  28,28,28,28,28,28, &                         ! band 28
                  29,29,29,29,29,29,29,29,29,29,29,29 /)       ! band 29

! Use this set for full 224 quadrature point (g-point) model
! ------- Data statements -------
!      ngc(:) = (/ 16,16,16,16,16,16,16,16,16,16,16,16,16,16 /)
!      ngs(:) = (/ 16,32,48,64,80,96,112,128,144,160,176,192,208,224 /)
!      ngm(:) = (/ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 16
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 17
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 18
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 19
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 20
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 21
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 22
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 23
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 24
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 25
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 26
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 27
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, &    ! band 28
!                  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 /)    ! band 29
!      ngn(:) = (/ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 16
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 17
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 18
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 19
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 20
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 21
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 22
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 23
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 24
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 25
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 26
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 27
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, &           ! band 28
!                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)           ! band 29
!      ngb(:) = (/ 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16, &   ! band 16
!                  17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17, &   ! band 17
!                  18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18, &   ! band 18
!                  19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19, &   ! band 19
!                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20, &   ! band 20
!                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21, &   ! band 21
!                  22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22, &   ! band 22
!                  23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23, &   ! band 23
!                  24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24, &   ! band 24
!                  25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25, &   ! band 25
!                  26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26, &   ! band 26
!                  27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27, &   ! band 27
!                  28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28, &   ! band 28
!                  29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29 /)   ! band 29


      wt(:) =  (/ 0.1527534276 , 0.1491729617 , 0.1420961469 , &
                  0.1316886544 , 0.1181945205 , 0.1019300893 , &
                  0.0832767040 , 0.0626720116 , 0.0424925000 , &
                  0.0046269894 , 0.0038279891 , 0.0030260086 , &
                  0.0022199750 , 0.0014140010 , 0.0005330000 , &
                  0.0000750000  /)

      end subroutine swcmbdat

!***************************************************************************
      subroutine swaerpr
!***************************************************************************

! Purpose: Define spectral aerosol properties for six ECMWF aerosol types
! as used in the ECMWF IFS model (see module rrsw_aer.F90 for details)
!
! Original: Defined for rrtmg_sw 14 spectral bands, JJMorcrette, ECMWF Feb 2003
! Revision: Reformatted for consistency with rrtmg_lw, MJIacono, AER, Jul 2006

      use rrsw_aer, only : rsrtaua, rsrpiza, rsrasya

      save

      rsrtaua( 1, :) = (/ &
        0.10849 , 0.66699 , 0.65255 , 0.11600 , 0.06529 , 0.04468 /)
      rsrtaua( 2, :) = (/ &
        0.10849 , 0.66699 , 0.65255 , 0.11600 , 0.06529 , 0.04468 /)
      rsrtaua( 3, :) = (/ &
        0.20543 , 0.84642 , 0.84958 , 0.21673 , 0.28270 , 0.10915 /)
      rsrtaua( 4, :) = (/ &
        0.20543 , 0.84642 , 0.84958 , 0.21673 , 0.28270 , 0.10915 /)
      rsrtaua( 5, :) = (/ &
        0.20543 , 0.84642 , 0.84958 , 0.21673 , 0.28270 , 0.10915 /)
      rsrtaua( 6, :) = (/ &
        0.20543 , 0.84642 , 0.84958 , 0.21673 , 0.28270 , 0.10915 /)
      rsrtaua( 7, :) = (/ &
        0.20543 , 0.84642 , 0.84958 , 0.21673 , 0.28270 , 0.10915 /)
      rsrtaua( 8, :) = (/ &
        0.52838 , 0.93285 , 0.93449 , 0.53078 , 0.67148 , 0.46608 /)
      rsrtaua( 9, :) = (/ &
        0.52838 , 0.93285 , 0.93449 , 0.53078 , 0.67148 , 0.46608 /)
      rsrtaua(10, :) = (/ &
        1.69446 , 1.11855 , 1.09212 , 1.72145 , 1.03858 , 1.12044 /)
      rsrtaua(11, :) = (/ &
        1.69446 , 1.11855 , 1.09212 , 1.72145 , 1.03858 , 1.12044 /)
      rsrtaua(12, :) = (/ &
        1.69446 , 1.11855 , 1.09212 , 1.72145 , 1.03858 , 1.12044 /)
      rsrtaua(13, :) = (/ &
        1.69446 , 1.11855 , 1.09212 , 1.72145 , 1.03858 , 1.12044 /)
      rsrtaua(14, :) = (/ &
        0.10849 , 0.66699 , 0.65255 , 0.11600 , 0.06529 , 0.04468 /)
 
      rsrpiza( 1, :) = (/ &
        .5230504 , .7868518 , .8531531 , .4048149 , .8748231 , .2355667 /)
      rsrpiza( 2, :) = (/ &
        .5230504 , .7868518 , .8531531 , .4048149 , .8748231 , .2355667 /)
      rsrpiza( 3, :) = (/ &
        .8287144 , .9949396 , .9279543 , .6765051 , .9467578 , .9955938 /)
      rsrpiza( 4, :) = (/ &
        .8287144 , .9949396 , .9279543 , .6765051 , .9467578 , .9955938 /)
      rsrpiza( 5, :) = (/ &
        .8287144 , .9949396 , .9279543 , .6765051 , .9467578 , .9955938 /)
      rsrpiza( 6, :) = (/ &
        .8287144 , .9949396 , .9279543 , .6765051 , .9467578 , .9955938 /)
      rsrpiza( 7, :) = (/ &
        .8287144 , .9949396 , .9279543 , .6765051 , .9467578 , .9955938 /)
      rsrpiza( 8, :) = (/ &
        .8970131 , .9984940 , .9245594 , .7768385 , .9532763 , .9999999 /)
      rsrpiza( 9, :) = (/ &
        .8970131 , .9984940 , .9245594 , .7768385 , .9532763 , .9999999 /)
      rsrpiza(10, :) = (/ &
        .9148907 , .9956173 , .7504584 , .8131335 , .9401905 , .9999999 /)
      rsrpiza(11, :) = (/ &
        .9148907 , .9956173 , .7504584 , .8131335 , .9401905 , .9999999 /)
      rsrpiza(12, :) = (/ &
        .9148907 , .9956173 , .7504584 , .8131335 , .9401905 , .9999999 /)
      rsrpiza(13, :) = (/ &
        .9148907 , .9956173 , .7504584 , .8131335 , .9401905 , .9999999 /)
      rsrpiza(14, :) = (/ &
        .5230504 , .7868518 , .8531531 , .4048149 , .8748231 , .2355667 /)

      rsrasya( 1, :) = (/ &
        0.700610 , 0.818871 , 0.702399 , 0.689886 , .4629866 , .1907639 /)
      rsrasya( 2, :) = (/ &
        0.700610 , 0.818871 , 0.702399 , 0.689886 , .4629866 , .1907639 /)
      rsrasya( 3, :) = (/ &
        0.636342 , 0.802467 , 0.691305 , 0.627497 , .6105750 , .4760794 /)
      rsrasya( 4, :) = (/ &
        0.636342 , 0.802467 , 0.691305 , 0.627497 , .6105750 , .4760794 /)
      rsrasya( 5, :) = (/ &
        0.636342 , 0.802467 , 0.691305 , 0.627497 , .6105750 , .4760794 /)
      rsrasya( 6, :) = (/ &
        0.636342 , 0.802467 , 0.691305 , 0.627497 , .6105750 , .4760794 /)
      rsrasya( 7, :) = (/ &
        0.636342 , 0.802467 , 0.691305 , 0.627497 , .6105750 , .4760794 /)
      rsrasya( 8, :) = (/ &
        0.668431 , 0.788530 , 0.698682 , 0.657422 , .6735182 , .6519706 /)
      rsrasya( 9, :) = (/ &
        0.668431 , 0.788530 , 0.698682 , 0.657422 , .6735182 , .6519706 /)
      rsrasya(10, :) = (/ &
        0.729019 , 0.803129 , 0.784592 , 0.712208 , .7008249 , .7270548 /)
      rsrasya(11, :) = (/ &
        0.729019 , 0.803129 , 0.784592 , 0.712208 , .7008249 , .7270548 /)
      rsrasya(12, :) = (/ &
        0.729019 , 0.803129 , 0.784592 , 0.712208 , .7008249 , .7270548 /)
      rsrasya(13, :) = (/ &
        0.729019 , 0.803129 , 0.784592 , 0.712208 , .7008249 , .7270548 /)
      rsrasya(14, :) = (/ &
        0.700610 , 0.818871 , 0.702399 , 0.689886 , .4629866 , .1907639 /)

      end subroutine swaerpr
 
!***************************************************************************
      subroutine cmbgb16s
!***************************************************************************
!
!  Original version:       MJIacono; July 1998
!  Revision for RRTM_SW:   MJIacono; November 2002
!  Revision for RRTMG_SW:  MJIacono; December 2003
!  Revision for F90 reformatting:  MJIacono; July 2006
!
!  The subroutines CMBGB16->CMBGB29 input the absorption coefficient
!  data for each band, which are defined for 16 g-points and 14 spectral
!  bands. The data are combined with appropriate weighting following the
!  g-point mapping arrays specified in RRTMG_SW_INIT.  Solar source 
!  function data in array SFLUXREF are combined without weighting.  All
!  g-point reduced data are put into new arrays for use in RRTMG_SW.
!
!  band 16:  2600-3250 cm-1 (low key- h2o,ch4; high key - ch4)
!
!-----------------------------------------------------------------------

      use rrsw_kg16, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(1)
                  sumk = 0.
                  do ipr = 1, ngn(igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(1)
               sumk = 0.
               do ipr = 1, ngn(igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(1)
            sumk = 0.
            do ipr = 1, ngn(igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(1)
         sumf = 0.
         do ipr = 1, ngn(igc)
            iprsm = iprsm + 1
            sumf = sumf + sfluxrefo(iprsm)
         enddo
         sfluxref(igc) = sumf
      enddo

      end subroutine cmbgb16s

!***************************************************************************
      subroutine cmbgb17
!***************************************************************************
!
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)
!-----------------------------------------------------------------------

      use rrsw_kg17, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(2)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(1)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+16)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(2)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(1)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+16)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(2)
            sumk = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+16)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(2)
            sumf = 0.
            do ipr = 1, ngn(ngs(1)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb17

!***************************************************************************
      subroutine cmbgb18
!***************************************************************************
!
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)
!-----------------------------------------------------------------------

      use rrsw_kg18, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(3)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(2)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+32)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(3)
               sumk = 0.
               do ipr = 1, ngn(ngs(2)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+32)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(3)
            sumk = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+32)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(3)
            sumf = 0.
            do ipr = 1, ngn(ngs(2)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb18

!***************************************************************************
      subroutine cmbgb19
!***************************************************************************
!
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)
!-----------------------------------------------------------------------

      use rrsw_kg19, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(4)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(3)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+48)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(4)
               sumk = 0.
               do ipr = 1, ngn(ngs(3)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+48)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(4)
            sumk = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+48)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(4)
            sumf = 0.
            do ipr = 1, ngn(ngs(3)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb19

!***************************************************************************
      subroutine cmbgb20
!***************************************************************************
!
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)
!-----------------------------------------------------------------------

      use rrsw_kg20, only : kao, kbo, selfrefo, forrefo, sfluxrefo, absch4o, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, absch4

! ------- Local -------
      integer  :: jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(5)
               sumk = 0.
               do ipr = 1, ngn(ngs(4)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+64)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(5)
               sumk = 0.
               do ipr = 1, ngn(ngs(4)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+64)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(5)
            sumk = 0.
            do ipr = 1, ngn(ngs(4)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+64)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(5)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(4)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + absch4o(iprsm)*rwgt(iprsm+64)
         enddo
         sfluxref(igc) = sumf1
         absch4(igc) = sumf2
      enddo

      end subroutine cmbgb20

!***************************************************************************
      subroutine cmbgb21
!***************************************************************************
!
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)
!-----------------------------------------------------------------------

      use rrsw_kg21, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(6)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(5)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+80)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(6)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(5)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+80)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(6)
            sumk = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+80)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(6)
            sumf = 0.
            do ipr = 1, ngn(ngs(5)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb21

!***************************************************************************
      subroutine cmbgb22
!***************************************************************************
!
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)
!-----------------------------------------------------------------------

      use rrsw_kg22, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(7)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(6)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+96)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(7)
               sumk = 0.
               do ipr = 1, ngn(ngs(6)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+96)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(7)
            sumk = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+96)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(7)
            sumf = 0.
            do ipr = 1, ngn(ngs(6)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb22

!***************************************************************************
      subroutine cmbgb23
!***************************************************************************
!
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg23, only : kao, selfrefo, forrefo, sfluxrefo, raylo, &
                            absa, ka, selfref, forref, sfluxref, rayl

! ------- Local -------
      integer  :: jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(8)
               sumk = 0.
               do ipr = 1, ngn(ngs(7)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+112)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(8)
            sumk = 0.
            do ipr = 1, ngn(ngs(7)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+112)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(8)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(7)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+112)
         enddo
         sfluxref(igc) = sumf1
         rayl(igc) = sumf2
      enddo

      end subroutine cmbgb23

!***************************************************************************
      subroutine cmbgb24
!***************************************************************************
!
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)
!-----------------------------------------------------------------------

      use rrsw_kg24, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            abso3ao, abso3bo, raylao, raylbo, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, &
                            abso3a, abso3b, rayla, raylb

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2, sumf3


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(9)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(8)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+128)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jt = 1,5
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(9)
               sumk = 0.
               do ipr = 1, ngn(ngs(8)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+128)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,3
         iprsm = 0
         do igc = 1,ngc(9)
            sumk = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+128)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(9)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         do ipr = 1, ngn(ngs(8)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + raylbo(iprsm)*rwgt(iprsm+128)
            sumf2 = sumf2 + abso3ao(iprsm)*rwgt(iprsm+128)
            sumf3 = sumf3 + abso3bo(iprsm)*rwgt(iprsm+128)
         enddo
         raylb(igc) = sumf1
         abso3a(igc) = sumf2
         abso3b(igc) = sumf3
      enddo

      do jp = 1,9
         iprsm = 0
         do igc = 1,ngc(9)
            sumf1 = 0.
            sumf2 = 0.
            do ipr = 1, ngn(ngs(8)+igc)
               iprsm = iprsm + 1
               sumf1 = sumf1 + sfluxrefo(iprsm,jp)
               sumf2 = sumf2 + raylao(iprsm,jp)*rwgt(iprsm+128)
            enddo
            sfluxref(igc,jp) = sumf1
            rayla(igc,jp) = sumf2
         enddo
      enddo

      end subroutine cmbgb24

!***************************************************************************
      subroutine cmbgb25
!***************************************************************************
!
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg25, only : kao, sfluxrefo, &
                            abso3ao, abso3bo, raylo, &
                            absa, ka, sfluxref, &
                            abso3a, abso3b, rayl

! ------- Local -------
      integer  :: jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2, sumf3, sumf4


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(10)
               sumk = 0.
               do ipr = 1, ngn(ngs(9)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+144)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(10)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         sumf4 = 0.
         do ipr = 1, ngn(ngs(9)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + abso3ao(iprsm)*rwgt(iprsm+144)
            sumf3 = sumf3 + abso3bo(iprsm)*rwgt(iprsm+144)
            sumf4 = sumf4 + raylo(iprsm)*rwgt(iprsm+144)
         enddo
         sfluxref(igc) = sumf1
         abso3a(igc) = sumf2
         abso3b(igc) = sumf3
         rayl(igc) = sumf4
      enddo

      end subroutine cmbgb25

!***************************************************************************
      subroutine cmbgb26
!***************************************************************************
!
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)
!-----------------------------------------------------------------------

      use rrsw_kg26, only : sfluxrefo, raylo, &
                            sfluxref, rayl

! ------- Local -------
      integer  :: igc, ipr, iprsm
      real  :: sumf1, sumf2


      iprsm = 0
      do igc = 1,ngc(11)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(10)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + raylo(iprsm)*rwgt(iprsm+160)
            sumf2 = sumf2 + sfluxrefo(iprsm)
         enddo
         rayl(igc) = sumf1
         sfluxref(igc) = sumf2
      enddo

      end subroutine cmbgb26

!***************************************************************************
      subroutine cmbgb27
!***************************************************************************
!
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)
!-----------------------------------------------------------------------

      use rrsw_kg27, only : kao, kbo, sfluxrefo, raylo, &
                            absa, ka, absb, kb, sfluxref, rayl

! ------- Local -------
      integer  :: jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(12)
               sumk = 0.
               do ipr = 1, ngn(ngs(11)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+176)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(12)
               sumk = 0.
               do ipr = 1, ngn(ngs(11)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+176)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(12)
         sumf1 = 0.
         sumf2 = 0.
         do ipr = 1, ngn(ngs(11)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + raylo(iprsm)*rwgt(iprsm+176)
         enddo
         sfluxref(igc) = sumf1
         rayl(igc) = sumf2
      enddo

      end subroutine cmbgb27

!***************************************************************************
      subroutine cmbgb28
!***************************************************************************
!
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)
!-----------------------------------------------------------------------

      use rrsw_kg28, only : kao, kbo, sfluxrefo, &
                            absa, ka, absb, kb, sfluxref

! ------- Local -------
      integer  :: jn, jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf


      do jn = 1,9
         do jt = 1,5
            do jp = 1,13
               iprsm = 0
               do igc = 1,ngc(13)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(12)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kao(jn,jt,jp,iprsm)*rwgt(iprsm+192)
                  enddo
                  ka(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jn = 1,5
         do jt = 1,5
            do jp = 13,59
               iprsm = 0
               do igc = 1,ngc(13)
                  sumk = 0.
                  do ipr = 1, ngn(ngs(12)+igc)
                     iprsm = iprsm + 1
                     sumk = sumk + kbo(jn,jt,jp,iprsm)*rwgt(iprsm+192)
                  enddo
                  kb(jn,jt,jp,igc) = sumk
               enddo
            enddo
         enddo
      enddo

      do jp = 1,5
         iprsm = 0
         do igc = 1,ngc(13)
            sumf = 0.
            do ipr = 1, ngn(ngs(12)+igc)
               iprsm = iprsm + 1
               sumf = sumf + sfluxrefo(iprsm,jp)
            enddo
            sfluxref(igc,jp) = sumf
         enddo
      enddo

      end subroutine cmbgb28

!***************************************************************************
      subroutine cmbgb29
!***************************************************************************
!
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)
!-----------------------------------------------------------------------

      use rrsw_kg29, only : kao, kbo, selfrefo, forrefo, sfluxrefo, &
                            absh2oo, absco2o, &
                            absa, ka, absb, kb, selfref, forref, sfluxref, &
                            absh2o, absco2

! ------- Local -------
      integer  :: jt, jp, igc, ipr, iprsm
      real  :: sumk, sumf1, sumf2, sumf3


      do jt = 1,5
         do jp = 1,13
            iprsm = 0
            do igc = 1,ngc(14)
               sumk = 0.
               do ipr = 1, ngn(ngs(13)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kao(jt,jp,iprsm)*rwgt(iprsm+208)
               enddo
               ka(jt,jp,igc) = sumk
            enddo
         enddo
         do jp = 13,59
            iprsm = 0
            do igc = 1,ngc(14)
               sumk = 0.
               do ipr = 1, ngn(ngs(13)+igc)
                  iprsm = iprsm + 1
                  sumk = sumk + kbo(jt,jp,iprsm)*rwgt(iprsm+208)
               enddo
               kb(jt,jp,igc) = sumk
            enddo
         enddo
      enddo

      do jt = 1,10
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + selfrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            selfref(jt,igc) = sumk
         enddo
      enddo

      do jt = 1,4
         iprsm = 0
         do igc = 1,ngc(14)
            sumk = 0.
            do ipr = 1, ngn(ngs(13)+igc)
               iprsm = iprsm + 1
               sumk = sumk + forrefo(jt,iprsm)*rwgt(iprsm+208)
            enddo
            forref(jt,igc) = sumk
         enddo
      enddo

      iprsm = 0
      do igc = 1,ngc(14)
         sumf1 = 0.
         sumf2 = 0.
         sumf3 = 0.
         do ipr = 1, ngn(ngs(13)+igc)
            iprsm = iprsm + 1
            sumf1 = sumf1 + sfluxrefo(iprsm)
            sumf2 = sumf2 + absco2o(iprsm)*rwgt(iprsm+208)
            sumf3 = sumf3 + absh2oo(iprsm)*rwgt(iprsm+208)
         enddo
         sfluxref(igc) = sumf1
         absco2(igc) = sumf2
         absh2o(igc) = sumf3
      enddo

      end subroutine cmbgb29

!***********************************************************************
      subroutine swcldpr
!***********************************************************************

! Purpose: Define cloud extinction coefficient, single scattering albedo
!          and asymmetry parameter data.
!

! ------- Modules -------

      use rrsw_cld, only : extliq1, ssaliq1, asyliq1, &
                           extice2, ssaice2, asyice2, &
                           extice3, ssaice3, asyice3, fdlice3, &
                           extice4, ssaice4, asyice4, &
                           abari, bbari, cbari, dbari, ebari, fbari

      save

!-----------------------------------------------------------------------
!
! Explanation of the method for each value of INFLAG.  A value of
!  0 for INFLAG do not distingish being liquid and ice clouds.
!  INFLAG = 2 does distinguish between liquid and ice clouds, and
!    requires further user input to specify the method to be used to 
!    compute the aborption due to each.
!  INFLAG = 0:  For each cloudy layer, the cloud fraction, the cloud optical
!    depth, the cloud single-scattering albedo, and the
!    moments of the phase function (0:NSTREAM).  Note
!    that these values are delta-m scaled within this
!    subroutine.

!  INFLAG = 2:  For each cloudy layer, the cloud fraction, cloud 
!    water path (g/m2), and cloud ice fraction are input.
!  ICEFLAG = 2:  The ice effective radius (microns) is input and the
!    optical properties due to ice clouds are computed from
!    the optical properties stored in the RT code, STREAMER v3.0 
!    (Reference: Key. J., Streamer User's Guide, Cooperative 
!    Institute for Meteorological Satellite Studies, 2001, 96 pp.).
!    Valid range of values for re are between 5.0 and
!    131.0 micron.
!    This version uses Ebert and Curry, JGR, (1992) method for 
!    ice particles larger than 131.0 microns. 
!  ICEFLAG = 3:  The ice generalized effective size (dge) is input
!    and the optical depths, single-scattering albedo,
!    and phase function moments are calculated as in
!    Q. Fu, J. Climate, (1996). Q. Fu provided high resolution
!    tables which were appropriately averaged for the
!    bands in RRTM_SW.  Linear interpolation is used to
!    get the coefficients from the stored tables.
!    Valid range of values for dge are between 5.0 and
!    140.0 micron. 
!    This version uses Ebert and Curry, JGR, (1992) method for 
!    ice particles larger than 140.0 microns. 
!  LIQFLAG = 1:  The water droplet effective radius (microns) is input 
!    and the optical depths due to water clouds are computed 
!    as in Hu and Stamnes, J., Clim., 6, 728-742, (1993).
!    The values for absorption coefficients appropriate for
!    the spectral bands in RRTM have been obtained for a 
!    range of effective radii by an averaging procedure 
!    based on the work of J. Pinto (private communication).
!    Linear interpolation is used to get the absorption 
!    coefficients for the input effective radius.
!
!     ------------------------------------------------------------------

! Everything below is for INFLAG = 2.

! Coefficients for Ebert and Curry method
      abari(:) = (/ &
        & 3.448e-03 ,3.448e-03 ,3.448e-03 ,3.448e-03 ,3.448e-03  /)
      bbari(:) = (/ &
        & 2.431e+00 ,2.431e+00 ,2.431e+00 ,2.431e+00 ,2.431e+00  /)
      cbari(:) = (/ &
        & 1.000e-05 ,1.100e-04 ,1.240e-02 ,3.779e-02 ,4.666e-01  /)
      dbari(:) = (/ &
        & 0.000e+00 ,1.405e-05 ,6.867e-04 ,1.284e-03 ,2.050e-05  /)
      ebari(:) = (/ &
        & 7.661e-01 ,7.730e-01 ,7.865e-01 ,8.172e-01 ,9.595e-01  /)
      fbari(:) = (/ &
        & 5.851e-04 ,5.665e-04 ,7.204e-04 ,7.463e-04 ,1.076e-04  /)

! Extinction coefficient
      extliq1(:, 16) = (/ &
        & 8.981463e-01 ,6.317895e-01 ,4.557508e-01 ,3.481624e-01 ,2.797950e-01 ,&
        & 2.342753e-01 ,2.026934e-01 ,1.800102e-01 ,1.632408e-01 ,1.505384e-01 ,&
        & 1.354524e-01 ,1.246520e-01 ,1.154342e-01 ,1.074756e-01 ,1.005353e-01 ,&
        & 9.442987e-02 ,8.901760e-02 ,8.418693e-02 ,7.984904e-02 ,7.593229e-02 ,&
        & 7.237827e-02 ,6.913887e-02 ,6.617415e-02 ,6.345061e-02 ,6.094001e-02 ,&
        & 5.861834e-02 ,5.646506e-02 ,5.446250e-02 ,5.249596e-02 ,5.081114e-02 ,&
        & 4.922243e-02 ,4.772189e-02 ,4.630243e-02 ,4.495766e-02 ,4.368189e-02 ,&
        & 4.246995e-02 ,4.131720e-02 ,4.021941e-02 ,3.917276e-02 ,3.817376e-02 ,&
        & 3.721926e-02 ,3.630635e-02 ,3.543237e-02 ,3.459491e-02 ,3.379171e-02 ,&
        & 3.302073e-02 ,3.228007e-02 ,3.156798e-02 ,3.088284e-02 ,3.022315e-02 ,&
        & 2.958753e-02 ,2.897468e-02 ,2.838340e-02 ,2.781258e-02 ,2.726117e-02 ,&
        & 2.672821e-02 ,2.621278e-02 ,2.5714e-02  /)
      extliq1(:, 17) = (/ &
        & 8.293797e-01 ,6.048371e-01 ,4.465706e-01 ,3.460387e-01 ,2.800064e-01 ,&
        & 2.346584e-01 ,2.022399e-01 ,1.782626e-01 ,1.600153e-01 ,1.457903e-01 ,&
        & 1.334061e-01 ,1.228548e-01 ,1.138396e-01 ,1.060486e-01 ,9.924856e-02 ,&
        & 9.326208e-02 ,8.795158e-02 ,8.320883e-02 ,7.894750e-02 ,7.509792e-02 ,&
        & 7.160323e-02 ,6.841653e-02 ,6.549889e-02 ,6.281763e-02 ,6.034516e-02 ,&
        & 5.805802e-02 ,5.593615e-02 ,5.396226e-02 ,5.202302e-02 ,5.036246e-02 ,&
        & 4.879606e-02 ,4.731610e-02 ,4.591565e-02 ,4.458852e-02 ,4.332912e-02 ,&
        & 4.213243e-02 ,4.099390e-02 ,3.990941e-02 ,3.887522e-02 ,3.788792e-02 ,&
        & 3.694440e-02 ,3.604183e-02 ,3.517760e-02 ,3.434934e-02 ,3.355485e-02 ,&
        & 3.279211e-02 ,3.205925e-02 ,3.135458e-02 ,3.067648e-02 ,3.002349e-02 ,&
        & 2.939425e-02 ,2.878748e-02 ,2.820200e-02 ,2.763673e-02 ,2.709062e-02 ,&
        & 2.656272e-02 ,2.605214e-02 ,2.5558e-02  /)
      extliq1(:, 18) = (/ &
        & 9.193685e-01 ,6.128292e-01 ,4.344150e-01 ,3.303048e-01 ,2.659500e-01 ,&
        & 2.239727e-01 ,1.953457e-01 ,1.751012e-01 ,1.603515e-01 ,1.493360e-01 ,&
        & 1.323791e-01 ,1.219335e-01 ,1.130076e-01 ,1.052926e-01 ,9.855839e-02 ,&
        & 9.262925e-02 ,8.736918e-02 ,8.267112e-02 ,7.844965e-02 ,7.463585e-02 ,&
        & 7.117343e-02 ,6.801601e-02 ,6.512503e-02 ,6.246815e-02 ,6.001806e-02 ,&
        & 5.775154e-02 ,5.564872e-02 ,5.369250e-02 ,5.176284e-02 ,5.011536e-02 ,&
        & 4.856099e-02 ,4.709211e-02 ,4.570193e-02 ,4.438430e-02 ,4.313375e-02 ,&
        & 4.194529e-02 ,4.081443e-02 ,3.973712e-02 ,3.870966e-02 ,3.772866e-02 ,&
        & 3.679108e-02 ,3.589409e-02 ,3.503514e-02 ,3.421185e-02 ,3.342206e-02 ,&
        & 3.266377e-02 ,3.193513e-02 ,3.123447e-02 ,3.056018e-02 ,2.991081e-02 ,&
        & 2.928502e-02 ,2.868154e-02 ,2.809920e-02 ,2.753692e-02 ,2.699367e-02 ,&
        & 2.646852e-02 ,2.596057e-02 ,2.5469e-02  /)
      extliq1(:, 19) = (/ &
        & 9.136931e-01 ,5.743244e-01 ,4.080708e-01 ,3.150572e-01 ,2.577261e-01 ,&
        & 2.197900e-01 ,1.933037e-01 ,1.740212e-01 ,1.595056e-01 ,1.482756e-01 ,&
        & 1.312164e-01 ,1.209246e-01 ,1.121227e-01 ,1.045095e-01 ,9.785967e-02 ,&
        & 9.200149e-02 ,8.680170e-02 ,8.215531e-02 ,7.797850e-02 ,7.420361e-02 ,&
        & 7.077530e-02 ,6.764798e-02 ,6.478369e-02 ,6.215063e-02 ,5.972189e-02 ,&
        & 5.747458e-02 ,5.538913e-02 ,5.344866e-02 ,5.153216e-02 ,4.989745e-02 ,&
        & 4.835476e-02 ,4.689661e-02 ,4.551629e-02 ,4.420777e-02 ,4.296563e-02 ,&
        & 4.178497e-02 ,4.066137e-02 ,3.959081e-02 ,3.856963e-02 ,3.759452e-02 ,&
        & 3.666244e-02 ,3.577061e-02 ,3.491650e-02 ,3.409777e-02 ,3.331227e-02 ,&
        & 3.255803e-02 ,3.183322e-02 ,3.113617e-02 ,3.046530e-02 ,2.981918e-02 ,&
        & 2.919646e-02 ,2.859591e-02 ,2.801635e-02 ,2.745671e-02 ,2.691599e-02 ,&
        & 2.639324e-02 ,2.588759e-02 ,2.5398e-02  /)
      extliq1(:, 20) = (/ &
        & 8.447548e-01 ,5.326840e-01 ,3.921523e-01 ,3.119082e-01 ,2.597055e-01 ,&
        & 2.228737e-01 ,1.954157e-01 ,1.741155e-01 ,1.570881e-01 ,1.431520e-01 ,&
        & 1.302034e-01 ,1.200491e-01 ,1.113571e-01 ,1.038330e-01 ,9.725657e-02 ,&
        & 9.145949e-02 ,8.631112e-02 ,8.170840e-02 ,7.756901e-02 ,7.382641e-02 ,&
        & 7.042616e-02 ,6.732338e-02 ,6.448069e-02 ,6.186672e-02 ,5.945494e-02 ,&
        & 5.722277e-02 ,5.515089e-02 ,5.322262e-02 ,5.132153e-02 ,4.969799e-02 ,&
        & 4.816556e-02 ,4.671686e-02 ,4.534525e-02 ,4.404480e-02 ,4.281014e-02 ,&
        & 4.163643e-02 ,4.051930e-02 ,3.945479e-02 ,3.843927e-02 ,3.746945e-02 ,&
        & 3.654234e-02 ,3.565518e-02 ,3.480547e-02 ,3.399088e-02 ,3.320930e-02 ,&
        & 3.245876e-02 ,3.173745e-02 ,3.104371e-02 ,3.037600e-02 ,2.973287e-02 ,&
        & 2.911300e-02 ,2.851516e-02 ,2.793818e-02 ,2.738101e-02 ,2.684264e-02 ,&
        & 2.632214e-02 ,2.581863e-02 ,2.5331e-02  /)
      extliq1(:, 21) = (/ &
        & 7.727642e-01 ,5.034865e-01 ,3.808673e-01 ,3.080333e-01 ,2.586453e-01 ,&
        & 2.224989e-01 ,1.947060e-01 ,1.725821e-01 ,1.545096e-01 ,1.394456e-01 ,&
        & 1.288683e-01 ,1.188852e-01 ,1.103317e-01 ,1.029214e-01 ,9.643967e-02 ,&
        & 9.072239e-02 ,8.564194e-02 ,8.109758e-02 ,7.700875e-02 ,7.331026e-02 ,&
        & 6.994879e-02 ,6.688028e-02 ,6.406807e-02 ,6.148133e-02 ,5.909400e-02 ,&
        & 5.688388e-02 ,5.483197e-02 ,5.292185e-02 ,5.103763e-02 ,4.942905e-02 ,&
        & 4.791039e-02 ,4.647438e-02 ,4.511453e-02 ,4.382497e-02 ,4.260043e-02 ,&
        & 4.143616e-02 ,4.032784e-02 ,3.927155e-02 ,3.826375e-02 ,3.730117e-02 ,&
        & 3.638087e-02 ,3.550013e-02 ,3.465646e-02 ,3.384759e-02 ,3.307141e-02 ,&
        & 3.232598e-02 ,3.160953e-02 ,3.092040e-02 ,3.025706e-02 ,2.961810e-02 ,&
        & 2.900220e-02 ,2.840814e-02 ,2.783478e-02 ,2.728106e-02 ,2.674599e-02 ,&
        & 2.622864e-02 ,2.572816e-02 ,2.5244e-02  /)
      extliq1(:, 22) = (/ &
        & 7.416833e-01 ,4.959591e-01 ,3.775057e-01 ,3.056353e-01 ,2.565943e-01 ,&
        & 2.206935e-01 ,1.931479e-01 ,1.712860e-01 ,1.534837e-01 ,1.386906e-01 ,&
        & 1.281198e-01 ,1.182344e-01 ,1.097595e-01 ,1.024137e-01 ,9.598552e-02 ,&
        & 9.031320e-02 ,8.527093e-02 ,8.075927e-02 ,7.669869e-02 ,7.302481e-02 ,&
        & 6.968491e-02 ,6.663542e-02 ,6.384008e-02 ,6.126838e-02 ,5.889452e-02 ,&
        & 5.669654e-02 ,5.465558e-02 ,5.275540e-02 ,5.087937e-02 ,4.927904e-02 ,&
        & 4.776796e-02 ,4.633895e-02 ,4.498557e-02 ,4.370202e-02 ,4.248306e-02 ,&
        & 4.132399e-02 ,4.022052e-02 ,3.916878e-02 ,3.816523e-02 ,3.720665e-02 ,&
        & 3.629011e-02 ,3.541290e-02 ,3.457257e-02 ,3.376685e-02 ,3.299365e-02 ,&
        & 3.225105e-02 ,3.153728e-02 ,3.085069e-02 ,3.018977e-02 ,2.955310e-02 ,&
        & 2.893940e-02 ,2.834742e-02 ,2.777606e-02 ,2.722424e-02 ,2.669099e-02 ,&
        & 2.617539e-02 ,2.567658e-02 ,2.5194e-02  /)
      extliq1(:, 23) = (/ &
        & 7.058580e-01 ,4.866573e-01 ,3.712238e-01 ,2.998638e-01 ,2.513441e-01 ,&
        & 2.161972e-01 ,1.895576e-01 ,1.686669e-01 ,1.518437e-01 ,1.380046e-01 ,&
        & 1.267564e-01 ,1.170399e-01 ,1.087026e-01 ,1.014704e-01 ,9.513729e-02 ,&
        & 8.954555e-02 ,8.457221e-02 ,8.012009e-02 ,7.611136e-02 ,7.248294e-02 ,&
        & 6.918317e-02 ,6.616934e-02 ,6.340584e-02 ,6.086273e-02 ,5.851465e-02 ,&
        & 5.634001e-02 ,5.432027e-02 ,5.243946e-02 ,5.058070e-02 ,4.899628e-02 ,&
        & 4.749975e-02 ,4.608411e-02 ,4.474303e-02 ,4.347082e-02 ,4.226237e-02 ,&
        & 4.111303e-02 ,4.001861e-02 ,3.897528e-02 ,3.797959e-02 ,3.702835e-02 ,&
        & 3.611867e-02 ,3.524791e-02 ,3.441364e-02 ,3.361360e-02 ,3.284577e-02 ,&
        & 3.210823e-02 ,3.139923e-02 ,3.071716e-02 ,3.006052e-02 ,2.942791e-02 ,&
        & 2.881806e-02 ,2.822974e-02 ,2.766185e-02 ,2.711335e-02 ,2.658326e-02 ,&
        & 2.607066e-02 ,2.557473e-02 ,2.5095e-02  /)
      extliq1(:, 24) = (/ &
        & 6.822779e-01 ,4.750373e-01 ,3.634834e-01 ,2.940726e-01 ,2.468060e-01 ,&
        & 2.125768e-01 ,1.866586e-01 ,1.663588e-01 ,1.500326e-01 ,1.366192e-01 ,&
        & 1.253472e-01 ,1.158052e-01 ,1.076101e-01 ,1.004954e-01 ,9.426089e-02 ,&
        & 8.875268e-02 ,8.385090e-02 ,7.946063e-02 ,7.550578e-02 ,7.192466e-02 ,&
        & 6.866669e-02 ,6.569001e-02 ,6.295971e-02 ,6.044642e-02 ,5.812526e-02 ,&
        & 5.597500e-02 ,5.397746e-02 ,5.211690e-02 ,5.027505e-02 ,4.870703e-02 ,&
        & 4.722555e-02 ,4.582373e-02 ,4.449540e-02 ,4.323497e-02 ,4.203742e-02 ,&
        & 4.089821e-02 ,3.981321e-02 ,3.877867e-02 ,3.779118e-02 ,3.684762e-02 ,&
        & 3.594514e-02 ,3.508114e-02 ,3.425322e-02 ,3.345917e-02 ,3.269698e-02 ,&
        & 3.196477e-02 ,3.126082e-02 ,3.058352e-02 ,2.993141e-02 ,2.930310e-02 ,&
        & 2.869732e-02 ,2.811289e-02 ,2.754869e-02 ,2.700371e-02 ,2.647698e-02 ,&
        & 2.596760e-02 ,2.547473e-02 ,2.4998e-02  /)
      extliq1(:, 25) = (/ &
        & 6.666233e-01 ,4.662044e-01 ,3.579517e-01 ,2.902984e-01 ,2.440475e-01 ,&
        & 2.104431e-01 ,1.849277e-01 ,1.648970e-01 ,1.487555e-01 ,1.354714e-01 ,&
        & 1.244173e-01 ,1.149913e-01 ,1.068903e-01 ,9.985323e-02 ,9.368351e-02 ,&
        & 8.823009e-02 ,8.337507e-02 ,7.902511e-02 ,7.510529e-02 ,7.155482e-02 ,&
        & 6.832386e-02 ,6.537113e-02 ,6.266218e-02 ,6.016802e-02 ,5.786408e-02 ,&
        & 5.572939e-02 ,5.374598e-02 ,5.189830e-02 ,5.006825e-02 ,4.851081e-02 ,&
        & 4.703906e-02 ,4.564623e-02 ,4.432621e-02 ,4.307349e-02 ,4.188312e-02 ,&
        & 4.075060e-02 ,3.967183e-02 ,3.864313e-02 ,3.766111e-02 ,3.672269e-02 ,&
        & 3.582505e-02 ,3.496559e-02 ,3.414196e-02 ,3.335198e-02 ,3.259362e-02 ,&
        & 3.186505e-02 ,3.116454e-02 ,3.049052e-02 ,2.984152e-02 ,2.921617e-02 ,&
        & 2.861322e-02 ,2.803148e-02 ,2.746986e-02 ,2.692733e-02 ,2.640295e-02 ,&
        & 2.589582e-02 ,2.540510e-02 ,2.4930e-02  /)
      extliq1(:, 26) = (/ &
        & 6.535669e-01 ,4.585865e-01 ,3.529226e-01 ,2.867245e-01 ,2.413848e-01 ,&
        & 2.083956e-01 ,1.833191e-01 ,1.636150e-01 ,1.477247e-01 ,1.346392e-01 ,&
        & 1.236449e-01 ,1.143095e-01 ,1.062828e-01 ,9.930773e-02 ,9.319029e-02 ,&
        & 8.778150e-02 ,8.296497e-02 ,7.864847e-02 ,7.475799e-02 ,7.123343e-02 ,&
        & 6.802549e-02 ,6.509332e-02 ,6.240285e-02 ,5.992538e-02 ,5.763657e-02 ,&
        & 5.551566e-02 ,5.354483e-02 ,5.170870e-02 ,4.988866e-02 ,4.834061e-02 ,&
        & 4.687751e-02 ,4.549264e-02 ,4.417999e-02 ,4.293410e-02 ,4.175006e-02 ,&
        & 4.062344e-02 ,3.955019e-02 ,3.852663e-02 ,3.754943e-02 ,3.661553e-02 ,&
        & 3.572214e-02 ,3.486669e-02 ,3.404683e-02 ,3.326040e-02 ,3.250542e-02 ,&
        & 3.178003e-02 ,3.108254e-02 ,3.041139e-02 ,2.976511e-02 ,2.914235e-02 ,&
        & 2.854187e-02 ,2.796247e-02 ,2.740309e-02 ,2.686271e-02 ,2.634038e-02 ,&
        & 2.583520e-02 ,2.534636e-02 ,2.4873e-02  /)
      extliq1(:, 27) = (/ &
        & 6.448790e-01 ,4.541425e-01 ,3.503348e-01 ,2.850494e-01 ,2.401966e-01 ,&
        & 2.074811e-01 ,1.825631e-01 ,1.629515e-01 ,1.471142e-01 ,1.340574e-01 ,&
        & 1.231462e-01 ,1.138628e-01 ,1.058802e-01 ,9.894286e-02 ,9.285818e-02 ,&
        & 8.747802e-02 ,8.268676e-02 ,7.839271e-02 ,7.452230e-02 ,7.101580e-02 ,&
        & 6.782418e-02 ,6.490685e-02 ,6.222991e-02 ,5.976484e-02 ,5.748742e-02 ,&
        & 5.537703e-02 ,5.341593e-02 ,5.158883e-02 ,4.977355e-02 ,4.823172e-02 ,&
        & 4.677430e-02 ,4.539465e-02 ,4.408680e-02 ,4.284533e-02 ,4.166539e-02 ,&
        & 4.054257e-02 ,3.947283e-02 ,3.845256e-02 ,3.747842e-02 ,3.654737e-02 ,&
        & 3.565665e-02 ,3.480370e-02 ,3.398620e-02 ,3.320198e-02 ,3.244908e-02 ,&
        & 3.172566e-02 ,3.103002e-02 ,3.036062e-02 ,2.971600e-02 ,2.909482e-02 ,&
        & 2.849582e-02 ,2.791785e-02 ,2.735982e-02 ,2.682072e-02 ,2.629960e-02 ,&
        & 2.579559e-02 ,2.530786e-02 ,2.4836e-02  /)
      extliq1(:, 28) = (/ &
        & 6.422688e-01 ,4.528453e-01 ,3.497232e-01 ,2.847724e-01 ,2.400815e-01 ,&
        & 2.074403e-01 ,1.825502e-01 ,1.629415e-01 ,1.470934e-01 ,1.340183e-01 ,&
        & 1.230935e-01 ,1.138049e-01 ,1.058201e-01 ,9.888245e-02 ,9.279878e-02 ,&
        & 8.742053e-02 ,8.263175e-02 ,7.834058e-02 ,7.447327e-02 ,7.097000e-02 ,&
        & 6.778167e-02 ,6.486765e-02 ,6.219400e-02 ,5.973215e-02 ,5.745790e-02 ,&
        & 5.535059e-02 ,5.339250e-02 ,5.156831e-02 ,4.975308e-02 ,4.821235e-02 ,&
        & 4.675596e-02 ,4.537727e-02 ,4.407030e-02 ,4.282968e-02 ,4.165053e-02 ,&
        & 4.052845e-02 ,3.945941e-02 ,3.843980e-02 ,3.746628e-02 ,3.653583e-02 ,&
        & 3.564567e-02 ,3.479326e-02 ,3.397626e-02 ,3.319253e-02 ,3.244008e-02 ,&
        & 3.171711e-02 ,3.102189e-02 ,3.035289e-02 ,2.970866e-02 ,2.908784e-02 ,&
        & 2.848920e-02 ,2.791156e-02 ,2.735385e-02 ,2.681507e-02 ,2.629425e-02 ,&
        & 2.579053e-02 ,2.530308e-02 ,2.4831e-02  /)
      extliq1(:, 29) = (/ &
        & 4.614710e-01 ,4.556116e-01 ,4.056568e-01 ,3.529833e-01 ,3.060334e-01 ,&
        & 2.658127e-01 ,2.316095e-01 ,2.024325e-01 ,1.773749e-01 ,1.556867e-01 ,&
        & 1.455558e-01 ,1.332882e-01 ,1.229052e-01 ,1.140067e-01 ,1.062981e-01 ,&
        & 9.955703e-02 ,9.361333e-02 ,8.833420e-02 ,8.361467e-02 ,7.937071e-02 ,&
        & 7.553420e-02 ,7.204942e-02 ,6.887031e-02 ,6.595851e-02 ,6.328178e-02 ,&
        & 6.081286e-02 ,5.852854e-02 ,5.640892e-02 ,5.431269e-02 ,5.252561e-02 ,&
        & 5.084345e-02 ,4.925727e-02 ,4.775910e-02 ,4.634182e-02 ,4.499907e-02 ,&
        & 4.372512e-02 ,4.251484e-02 ,4.136357e-02 ,4.026710e-02 ,3.922162e-02 ,&
        & 3.822365e-02 ,3.727004e-02 ,3.635790e-02 ,3.548457e-02 ,3.464764e-02 ,&
        & 3.384488e-02 ,3.307424e-02 ,3.233384e-02 ,3.162192e-02 ,3.093688e-02 ,&
        & 3.027723e-02 ,2.964158e-02 ,2.902864e-02 ,2.843722e-02 ,2.786621e-02 ,&
        & 2.731457e-02 ,2.678133e-02 ,2.6266e-02  /)

! Single scattering albedo     
      ssaliq1(:, 16) = (/ &
        & 8.143821e-01 ,7.836739e-01 ,7.550722e-01 ,7.306269e-01 ,7.105612e-01 ,&
        & 6.946649e-01 ,6.825556e-01 ,6.737762e-01 ,6.678448e-01 ,6.642830e-01 ,&
        & 6.679741e-01 ,6.584607e-01 ,6.505598e-01 ,6.440951e-01 ,6.388901e-01 ,&
        & 6.347689e-01 ,6.315549e-01 ,6.290718e-01 ,6.271432e-01 ,6.255928e-01 ,&
        & 6.242441e-01 ,6.229207e-01 ,6.214464e-01 ,6.196445e-01 ,6.173388e-01 ,&
        & 6.143527e-01 ,6.105099e-01 ,6.056339e-01 ,6.108290e-01 ,6.073939e-01 ,&
        & 6.043073e-01 ,6.015473e-01 ,5.990913e-01 ,5.969173e-01 ,5.950028e-01 ,&
        & 5.933257e-01 ,5.918636e-01 ,5.905944e-01 ,5.894957e-01 ,5.885453e-01 ,&
        & 5.877209e-01 ,5.870003e-01 ,5.863611e-01 ,5.857811e-01 ,5.852381e-01 ,&
        & 5.847098e-01 ,5.841738e-01 ,5.836081e-01 ,5.829901e-01 ,5.822979e-01 ,&
        & 5.815089e-01 ,5.806011e-01 ,5.795521e-01 ,5.783396e-01 ,5.769413e-01 ,&
        & 5.753351e-01 ,5.734986e-01 ,5.7141e-01  /)
      ssaliq1(:, 17) = (/ &
        & 8.165821e-01 ,8.002015e-01 ,7.816921e-01 ,7.634131e-01 ,7.463721e-01 ,&
        & 7.312469e-01 ,7.185883e-01 ,7.088975e-01 ,7.026671e-01 ,7.004020e-01 ,&
        & 7.042138e-01 ,6.960930e-01 ,6.894243e-01 ,6.840459e-01 ,6.797957e-01 ,&
        & 6.765119e-01 ,6.740325e-01 ,6.721955e-01 ,6.708391e-01 ,6.698013e-01 ,&
        & 6.689201e-01 ,6.680339e-01 ,6.669805e-01 ,6.655982e-01 ,6.637250e-01 ,&
        & 6.611992e-01 ,6.578588e-01 ,6.535420e-01 ,6.584449e-01 ,6.553992e-01 ,&
        & 6.526547e-01 ,6.501917e-01 ,6.479905e-01 ,6.460313e-01 ,6.442945e-01 ,&
        & 6.427605e-01 ,6.414094e-01 ,6.402217e-01 ,6.391775e-01 ,6.382573e-01 ,&
        & 6.374413e-01 ,6.367099e-01 ,6.360433e-01 ,6.354218e-01 ,6.348257e-01 ,&
        & 6.342355e-01 ,6.336313e-01 ,6.329935e-01 ,6.323023e-01 ,6.315383e-01 ,&
        & 6.306814e-01 ,6.297122e-01 ,6.286110e-01 ,6.273579e-01 ,6.259333e-01 ,&
        & 6.243176e-01 ,6.224910e-01 ,6.2043e-01  /)
      ssaliq1(:, 18) = (/ &
        & 9.900163e-01 ,9.854307e-01 ,9.797730e-01 ,9.733113e-01 ,9.664245e-01 ,&
        & 9.594976e-01 ,9.529055e-01 ,9.470112e-01 ,9.421695e-01 ,9.387304e-01 ,&
        & 9.344918e-01 ,9.305302e-01 ,9.267048e-01 ,9.230072e-01 ,9.194289e-01 ,&
        & 9.159616e-01 ,9.125968e-01 ,9.093260e-01 ,9.061409e-01 ,9.030330e-01 ,&
        & 8.999940e-01 ,8.970154e-01 ,8.940888e-01 ,8.912058e-01 ,8.883579e-01 ,&
        & 8.855368e-01 ,8.827341e-01 ,8.799413e-01 ,8.777423e-01 ,8.749566e-01 ,&
        & 8.722298e-01 ,8.695605e-01 ,8.669469e-01 ,8.643875e-01 ,8.618806e-01 ,&
        & 8.594246e-01 ,8.570179e-01 ,8.546589e-01 ,8.523459e-01 ,8.500773e-01 ,&
        & 8.478516e-01 ,8.456670e-01 ,8.435219e-01 ,8.414148e-01 ,8.393439e-01 ,&
        & 8.373078e-01 ,8.353047e-01 ,8.333330e-01 ,8.313911e-01 ,8.294774e-01 ,&
        & 8.275904e-01 ,8.257282e-01 ,8.238893e-01 ,8.220721e-01 ,8.202751e-01 ,&
        & 8.184965e-01 ,8.167346e-01 ,8.1499e-01  /)
      ssaliq1(:, 19) = (/ &
        & 9.999916e-01 ,9.987396e-01 ,9.966900e-01 ,9.950738e-01 ,9.937531e-01 ,&
        & 9.925912e-01 ,9.914525e-01 ,9.902018e-01 ,9.887046e-01 ,9.868263e-01 ,&
        & 9.849039e-01 ,9.832372e-01 ,9.815265e-01 ,9.797770e-01 ,9.779940e-01 ,&
        & 9.761827e-01 ,9.743481e-01 ,9.724955e-01 ,9.706303e-01 ,9.687575e-01 ,&
        & 9.668823e-01 ,9.650100e-01 ,9.631457e-01 ,9.612947e-01 ,9.594622e-01 ,&
        & 9.576534e-01 ,9.558734e-01 ,9.541275e-01 ,9.522059e-01 ,9.504258e-01 ,&
        & 9.486459e-01 ,9.468676e-01 ,9.450921e-01 ,9.433208e-01 ,9.415548e-01 ,&
        & 9.397955e-01 ,9.380441e-01 ,9.363022e-01 ,9.345706e-01 ,9.328510e-01 ,&
        & 9.311445e-01 ,9.294524e-01 ,9.277761e-01 ,9.261167e-01 ,9.244755e-01 ,&
        & 9.228540e-01 ,9.212534e-01 ,9.196748e-01 ,9.181197e-01 ,9.165894e-01 ,&
        & 9.150851e-01 ,9.136080e-01 ,9.121596e-01 ,9.107410e-01 ,9.093536e-01 ,&
        & 9.079987e-01 ,9.066775e-01 ,9.0539e-01  /)
      ssaliq1(:, 20) = (/ &
        & 9.979493e-01 ,9.964113e-01 ,9.950014e-01 ,9.937045e-01 ,9.924964e-01 ,&
        & 9.913546e-01 ,9.902575e-01 ,9.891843e-01 ,9.881136e-01 ,9.870238e-01 ,&
        & 9.859934e-01 ,9.849372e-01 ,9.838873e-01 ,9.828434e-01 ,9.818052e-01 ,&
        & 9.807725e-01 ,9.797450e-01 ,9.787225e-01 ,9.777047e-01 ,9.766914e-01 ,&
        & 9.756823e-01 ,9.746771e-01 ,9.736756e-01 ,9.726775e-01 ,9.716827e-01 ,&
        & 9.706907e-01 ,9.697014e-01 ,9.687145e-01 ,9.678060e-01 ,9.668108e-01 ,&
        & 9.658218e-01 ,9.648391e-01 ,9.638629e-01 ,9.628936e-01 ,9.619313e-01 ,&
        & 9.609763e-01 ,9.600287e-01 ,9.590888e-01 ,9.581569e-01 ,9.572330e-01 ,&
        & 9.563176e-01 ,9.554108e-01 ,9.545128e-01 ,9.536239e-01 ,9.527443e-01 ,&
        & 9.518741e-01 ,9.510137e-01 ,9.501633e-01 ,9.493230e-01 ,9.484931e-01 ,&
        & 9.476740e-01 ,9.468656e-01 ,9.460683e-01 ,9.452824e-01 ,9.445080e-01 ,&
        & 9.437454e-01 ,9.429948e-01 ,9.4226e-01  /)
      ssaliq1(:, 21) = (/ &
        & 9.988742e-01 ,9.982668e-01 ,9.976935e-01 ,9.971497e-01 ,9.966314e-01 ,&
        & 9.961344e-01 ,9.956545e-01 ,9.951873e-01 ,9.947286e-01 ,9.942741e-01 ,&
        & 9.938457e-01 ,9.933947e-01 ,9.929473e-01 ,9.925032e-01 ,9.920621e-01 ,&
        & 9.916237e-01 ,9.911875e-01 ,9.907534e-01 ,9.903209e-01 ,9.898898e-01 ,&
        & 9.894597e-01 ,9.890304e-01 ,9.886015e-01 ,9.881726e-01 ,9.877435e-01 ,&
        & 9.873138e-01 ,9.868833e-01 ,9.864516e-01 ,9.860698e-01 ,9.856317e-01 ,&
        & 9.851957e-01 ,9.847618e-01 ,9.843302e-01 ,9.839008e-01 ,9.834739e-01 ,&
        & 9.830494e-01 ,9.826275e-01 ,9.822083e-01 ,9.817918e-01 ,9.813782e-01 ,&
        & 9.809675e-01 ,9.805598e-01 ,9.801552e-01 ,9.797538e-01 ,9.793556e-01 ,&
        & 9.789608e-01 ,9.785695e-01 ,9.781817e-01 ,9.777975e-01 ,9.774171e-01 ,&
        & 9.770404e-01 ,9.766676e-01 ,9.762988e-01 ,9.759340e-01 ,9.755733e-01 ,&
        & 9.752169e-01 ,9.748649e-01 ,9.7452e-01  /)
      ssaliq1(:, 22) = (/ &
        & 9.994441e-01 ,9.991608e-01 ,9.988949e-01 ,9.986439e-01 ,9.984054e-01 ,&
        & 9.981768e-01 ,9.979557e-01 ,9.977396e-01 ,9.975258e-01 ,9.973120e-01 ,&
        & 9.971011e-01 ,9.968852e-01 ,9.966708e-01 ,9.964578e-01 ,9.962462e-01 ,&
        & 9.960357e-01 ,9.958264e-01 ,9.956181e-01 ,9.954108e-01 ,9.952043e-01 ,&
        & 9.949987e-01 ,9.947937e-01 ,9.945892e-01 ,9.943853e-01 ,9.941818e-01 ,&
        & 9.939786e-01 ,9.937757e-01 ,9.935728e-01 ,9.933922e-01 ,9.931825e-01 ,&
        & 9.929739e-01 ,9.927661e-01 ,9.925592e-01 ,9.923534e-01 ,9.921485e-01 ,&
        & 9.919447e-01 ,9.917421e-01 ,9.915406e-01 ,9.913403e-01 ,9.911412e-01 ,&
        & 9.909435e-01 ,9.907470e-01 ,9.905519e-01 ,9.903581e-01 ,9.901659e-01 ,&
        & 9.899751e-01 ,9.897858e-01 ,9.895981e-01 ,9.894120e-01 ,9.892276e-01 ,&
        & 9.890447e-01 ,9.888637e-01 ,9.886845e-01 ,9.885070e-01 ,9.883314e-01 ,&
        & 9.881576e-01 ,9.879859e-01 ,9.8782e-01  /)
      ssaliq1(:, 23) = (/ &
        & 9.999138e-01 ,9.998730e-01 ,9.998338e-01 ,9.997965e-01 ,9.997609e-01 ,&
        & 9.997270e-01 ,9.996944e-01 ,9.996629e-01 ,9.996321e-01 ,9.996016e-01 ,&
        & 9.995690e-01 ,9.995372e-01 ,9.995057e-01 ,9.994744e-01 ,9.994433e-01 ,&
        & 9.994124e-01 ,9.993817e-01 ,9.993510e-01 ,9.993206e-01 ,9.992903e-01 ,&
        & 9.992600e-01 ,9.992299e-01 ,9.991998e-01 ,9.991698e-01 ,9.991398e-01 ,&
        & 9.991098e-01 ,9.990799e-01 ,9.990499e-01 ,9.990231e-01 ,9.989920e-01 ,&
        & 9.989611e-01 ,9.989302e-01 ,9.988996e-01 ,9.988690e-01 ,9.988386e-01 ,&
        & 9.988084e-01 ,9.987783e-01 ,9.987485e-01 ,9.987187e-01 ,9.986891e-01 ,&
        & 9.986598e-01 ,9.986306e-01 ,9.986017e-01 ,9.985729e-01 ,9.985443e-01 ,&
        & 9.985160e-01 ,9.984879e-01 ,9.984600e-01 ,9.984324e-01 ,9.984050e-01 ,&
        & 9.983778e-01 ,9.983509e-01 ,9.983243e-01 ,9.982980e-01 ,9.982719e-01 ,&
        & 9.982461e-01 ,9.982206e-01 ,9.9820e-01  /)
      ssaliq1(:, 24) = (/ &
        & 9.999985e-01 ,9.999979e-01 ,9.999972e-01 ,9.999966e-01 ,9.999961e-01 ,&
        & 9.999955e-01 ,9.999950e-01 ,9.999944e-01 ,9.999938e-01 ,9.999933e-01 ,&
        & 9.999927e-01 ,9.999921e-01 ,9.999915e-01 ,9.999910e-01 ,9.999904e-01 ,&
        & 9.999899e-01 ,9.999893e-01 ,9.999888e-01 ,9.999882e-01 ,9.999877e-01 ,&
        & 9.999871e-01 ,9.999866e-01 ,9.999861e-01 ,9.999855e-01 ,9.999850e-01 ,&
        & 9.999844e-01 ,9.999839e-01 ,9.999833e-01 ,9.999828e-01 ,9.999823e-01 ,&
        & 9.999817e-01 ,9.999812e-01 ,9.999807e-01 ,9.999801e-01 ,9.999796e-01 ,&
        & 9.999791e-01 ,9.999786e-01 ,9.999781e-01 ,9.999776e-01 ,9.999770e-01 ,&
        & 9.999765e-01 ,9.999761e-01 ,9.999756e-01 ,9.999751e-01 ,9.999746e-01 ,&
        & 9.999741e-01 ,9.999736e-01 ,9.999732e-01 ,9.999727e-01 ,9.999722e-01 ,&
        & 9.999718e-01 ,9.999713e-01 ,9.999709e-01 ,9.999705e-01 ,9.999701e-01 ,&
        & 9.999697e-01 ,9.999692e-01 ,9.9997e-01  /)
      ssaliq1(:, 25) = (/ &
        & 9.999999e-01 ,9.999998e-01 ,9.999997e-01 ,9.999997e-01 ,9.999997e-01 ,&
        & 9.999996e-01 ,9.999996e-01 ,9.999995e-01 ,9.999995e-01 ,9.999994e-01 ,&
        & 9.999994e-01 ,9.999993e-01 ,9.999993e-01 ,9.999992e-01 ,9.999992e-01 ,&
        & 9.999991e-01 ,9.999991e-01 ,9.999991e-01 ,9.999990e-01 ,9.999989e-01 ,&
        & 9.999989e-01 ,9.999989e-01 ,9.999988e-01 ,9.999988e-01 ,9.999987e-01 ,&
        & 9.999987e-01 ,9.999986e-01 ,9.999986e-01 ,9.999985e-01 ,9.999985e-01 ,&
        & 9.999984e-01 ,9.999984e-01 ,9.999984e-01 ,9.999983e-01 ,9.999983e-01 ,&
        & 9.999982e-01 ,9.999982e-01 ,9.999982e-01 ,9.999981e-01 ,9.999980e-01 ,&
        & 9.999980e-01 ,9.999980e-01 ,9.999979e-01 ,9.999979e-01 ,9.999978e-01 ,&
        & 9.999978e-01 ,9.999977e-01 ,9.999977e-01 ,9.999977e-01 ,9.999976e-01 ,&
        & 9.999976e-01 ,9.999975e-01 ,9.999975e-01 ,9.999974e-01 ,9.999974e-01 ,&
        & 9.999974e-01 ,9.999973e-01 ,1.0000e+00  /)
      ssaliq1(:, 26) = (/ &
        & 9.999997e-01 ,9.999995e-01 ,9.999993e-01 ,9.999992e-01 ,9.999990e-01 ,&
        & 9.999989e-01 ,9.999988e-01 ,9.999987e-01 ,9.999986e-01 ,9.999985e-01 ,&
        & 9.999984e-01 ,9.999983e-01 ,9.999982e-01 ,9.999981e-01 ,9.999980e-01 ,&
        & 9.999978e-01 ,9.999977e-01 ,9.999976e-01 ,9.999975e-01 ,9.999974e-01 ,&
        & 9.999973e-01 ,9.999972e-01 ,9.999970e-01 ,9.999969e-01 ,9.999968e-01 ,&
        & 9.999967e-01 ,9.999966e-01 ,9.999965e-01 ,9.999964e-01 ,9.999963e-01 ,&
        & 9.999962e-01 ,9.999961e-01 ,9.999959e-01 ,9.999958e-01 ,9.999957e-01 ,&
        & 9.999956e-01 ,9.999955e-01 ,9.999954e-01 ,9.999953e-01 ,9.999952e-01 ,&
        & 9.999951e-01 ,9.999949e-01 ,9.999949e-01 ,9.999947e-01 ,9.999946e-01 ,&
        & 9.999945e-01 ,9.999944e-01 ,9.999943e-01 ,9.999942e-01 ,9.999941e-01 ,&
        & 9.999940e-01 ,9.999939e-01 ,9.999938e-01 ,9.999937e-01 ,9.999936e-01 ,&
        & 9.999935e-01 ,9.999934e-01 ,9.9999e-01  /)
      ssaliq1(:, 27) = (/ &
        & 9.999984e-01 ,9.999976e-01 ,9.999969e-01 ,9.999962e-01 ,9.999956e-01 ,&
        & 9.999950e-01 ,9.999945e-01 ,9.999940e-01 ,9.999935e-01 ,9.999931e-01 ,&
        & 9.999926e-01 ,9.999920e-01 ,9.999914e-01 ,9.999908e-01 ,9.999903e-01 ,&
        & 9.999897e-01 ,9.999891e-01 ,9.999886e-01 ,9.999880e-01 ,9.999874e-01 ,&
        & 9.999868e-01 ,9.999863e-01 ,9.999857e-01 ,9.999851e-01 ,9.999846e-01 ,&
        & 9.999840e-01 ,9.999835e-01 ,9.999829e-01 ,9.999824e-01 ,9.999818e-01 ,&
        & 9.999812e-01 ,9.999806e-01 ,9.999800e-01 ,9.999795e-01 ,9.999789e-01 ,&
        & 9.999783e-01 ,9.999778e-01 ,9.999773e-01 ,9.999767e-01 ,9.999761e-01 ,&
        & 9.999756e-01 ,9.999750e-01 ,9.999745e-01 ,9.999739e-01 ,9.999734e-01 ,&
        & 9.999729e-01 ,9.999723e-01 ,9.999718e-01 ,9.999713e-01 ,9.999708e-01 ,&
        & 9.999703e-01 ,9.999697e-01 ,9.999692e-01 ,9.999687e-01 ,9.999683e-01 ,&
        & 9.999678e-01 ,9.999673e-01 ,9.9997e-01  /)
      ssaliq1(:, 28) = (/ &
        & 9.999981e-01 ,9.999973e-01 ,9.999965e-01 ,9.999958e-01 ,9.999951e-01 ,&
        & 9.999943e-01 ,9.999937e-01 ,9.999930e-01 ,9.999924e-01 ,9.999918e-01 ,&
        & 9.999912e-01 ,9.999905e-01 ,9.999897e-01 ,9.999890e-01 ,9.999883e-01 ,&
        & 9.999876e-01 ,9.999869e-01 ,9.999862e-01 ,9.999855e-01 ,9.999847e-01 ,&
        & 9.999840e-01 ,9.999834e-01 ,9.999827e-01 ,9.999819e-01 ,9.999812e-01 ,&
        & 9.999805e-01 ,9.999799e-01 ,9.999791e-01 ,9.999785e-01 ,9.999778e-01 ,&
        & 9.999771e-01 ,9.999764e-01 ,9.999757e-01 ,9.999750e-01 ,9.999743e-01 ,&
        & 9.999736e-01 ,9.999729e-01 ,9.999722e-01 ,9.999715e-01 ,9.999709e-01 ,&
        & 9.999701e-01 ,9.999695e-01 ,9.999688e-01 ,9.999682e-01 ,9.999675e-01 ,&
        & 9.999669e-01 ,9.999662e-01 ,9.999655e-01 ,9.999649e-01 ,9.999642e-01 ,&
        & 9.999636e-01 ,9.999630e-01 ,9.999624e-01 ,9.999618e-01 ,9.999612e-01 ,&
        & 9.999606e-01 ,9.999600e-01 ,9.9996e-01  /)
      ssaliq1(:, 29) = (/ &
        & 8.505737e-01 ,8.465102e-01 ,8.394829e-01 ,8.279508e-01 ,8.110806e-01 ,&
        & 7.900397e-01 ,7.669615e-01 ,7.444422e-01 ,7.253055e-01 ,7.124831e-01 ,&
        & 7.016434e-01 ,6.885485e-01 ,6.767340e-01 ,6.661029e-01 ,6.565577e-01 ,&
        & 6.480013e-01 ,6.403373e-01 ,6.334697e-01 ,6.273034e-01 ,6.217440e-01 ,&
        & 6.166983e-01 ,6.120740e-01 ,6.077796e-01 ,6.037249e-01 ,5.998207e-01 ,&
        & 5.959788e-01 ,5.921123e-01 ,5.881354e-01 ,5.891285e-01 ,5.851143e-01 ,&
        & 5.814653e-01 ,5.781606e-01 ,5.751792e-01 ,5.724998e-01 ,5.701016e-01 ,&
        & 5.679634e-01 ,5.660642e-01 ,5.643829e-01 ,5.628984e-01 ,5.615898e-01 ,&
        & 5.604359e-01 ,5.594158e-01 ,5.585083e-01 ,5.576924e-01 ,5.569470e-01 ,&
        & 5.562512e-01 ,5.555838e-01 ,5.549239e-01 ,5.542503e-01 ,5.535420e-01 ,&
        & 5.527781e-01 ,5.519374e-01 ,5.509989e-01 ,5.499417e-01 ,5.487445e-01 ,&
        & 5.473865e-01 ,5.458466e-01 ,5.4410e-01  /)

! asymmetry parameter
      asyliq1(:, 16) = (/ &
        & 8.133297e-01 ,8.133528e-01 ,8.173865e-01 ,8.243205e-01 ,8.333063e-01 ,&
        & 8.436317e-01 ,8.546611e-01 ,8.657934e-01 ,8.764345e-01 ,8.859837e-01 ,&
        & 8.627394e-01 ,8.824569e-01 ,8.976887e-01 ,9.089541e-01 ,9.167699e-01 ,&
        & 9.216517e-01 ,9.241147e-01 ,9.246743e-01 ,9.238469e-01 ,9.221504e-01 ,&
        & 9.201045e-01 ,9.182299e-01 ,9.170491e-01 ,9.170862e-01 ,9.188653e-01 ,&
        & 9.229111e-01 ,9.297468e-01 ,9.398950e-01 ,9.203269e-01 ,9.260693e-01 ,&
        & 9.309373e-01 ,9.349918e-01 ,9.382935e-01 ,9.409030e-01 ,9.428809e-01 ,&
        & 9.442881e-01 ,9.451851e-01 ,9.456331e-01 ,9.456926e-01 ,9.454247e-01 ,&
        & 9.448902e-01 ,9.441503e-01 ,9.432661e-01 ,9.422987e-01 ,9.413094e-01 ,&
        & 9.403594e-01 ,9.395102e-01 ,9.388230e-01 ,9.383594e-01 ,9.381810e-01 ,&
        & 9.383489e-01 ,9.389251e-01 ,9.399707e-01 ,9.415475e-01 ,9.437167e-01 ,&
        & 9.465399e-01 ,9.500786e-01 ,9.5439e-01  /)
      asyliq1(:, 17) = (/ &
        & 8.794448e-01 ,8.819306e-01 ,8.837667e-01 ,8.853832e-01 ,8.871010e-01 ,&
        & 8.892675e-01 ,8.922584e-01 ,8.964666e-01 ,9.022940e-01 ,9.101456e-01 ,&
        & 8.839999e-01 ,9.035610e-01 ,9.184568e-01 ,9.292315e-01 ,9.364282e-01 ,&
        & 9.405887e-01 ,9.422554e-01 ,9.419703e-01 ,9.402759e-01 ,9.377159e-01 ,&
        & 9.348345e-01 ,9.321769e-01 ,9.302888e-01 ,9.297166e-01 ,9.310075e-01 ,&
        & 9.347080e-01 ,9.413643e-01 ,9.515216e-01 ,9.306286e-01 ,9.361781e-01 ,&
        & 9.408374e-01 ,9.446692e-01 ,9.477363e-01 ,9.501013e-01 ,9.518268e-01 ,&
        & 9.529756e-01 ,9.536105e-01 ,9.537938e-01 ,9.535886e-01 ,9.530574e-01 ,&
        & 9.522633e-01 ,9.512688e-01 ,9.501370e-01 ,9.489306e-01 ,9.477126e-01 ,&
        & 9.465459e-01 ,9.454934e-01 ,9.446183e-01 ,9.439833e-01 ,9.436519e-01 ,&
        & 9.436866e-01 ,9.441508e-01 ,9.451073e-01 ,9.466195e-01 ,9.487501e-01 ,&
        & 9.515621e-01 ,9.551185e-01 ,9.5948e-01  /)
      asyliq1(:, 18) = (/ &
        & 8.478817e-01 ,8.269312e-01 ,8.161352e-01 ,8.135960e-01 ,8.173586e-01 ,&
        & 8.254167e-01 ,8.357072e-01 ,8.461167e-01 ,8.544952e-01 ,8.586776e-01 ,&
        & 8.335562e-01 ,8.524273e-01 ,8.669052e-01 ,8.775014e-01 ,8.847277e-01 ,&
        & 8.890958e-01 ,8.911173e-01 ,8.913038e-01 ,8.901669e-01 ,8.882182e-01 ,&
        & 8.859692e-01 ,8.839315e-01 ,8.826164e-01 ,8.825356e-01 ,8.842004e-01 ,&
        & 8.881223e-01 ,8.948131e-01 ,9.047837e-01 ,8.855951e-01 ,8.911796e-01 ,&
        & 8.959229e-01 ,8.998837e-01 ,9.031209e-01 ,9.056939e-01 ,9.076609e-01 ,&
        & 9.090812e-01 ,9.100134e-01 ,9.105167e-01 ,9.106496e-01 ,9.104712e-01 ,&
        & 9.100404e-01 ,9.094159e-01 ,9.086568e-01 ,9.078218e-01 ,9.069697e-01 ,&
        & 9.061595e-01 ,9.054499e-01 ,9.048999e-01 ,9.045683e-01 ,9.045142e-01 ,&
        & 9.047962e-01 ,9.054730e-01 ,9.066037e-01 ,9.082472e-01 ,9.104623e-01 ,&
        & 9.133079e-01 ,9.168427e-01 ,9.2113e-01  /)
      asyliq1(:, 19) = (/ &
        & 8.216697e-01 ,7.982871e-01 ,7.891147e-01 ,7.909083e-01 ,8.003833e-01 ,&
        & 8.142516e-01 ,8.292290e-01 ,8.420356e-01 ,8.493945e-01 ,8.480316e-01 ,&
        & 8.212381e-01 ,8.394984e-01 ,8.534095e-01 ,8.634813e-01 ,8.702242e-01 ,&
        & 8.741483e-01 ,8.757638e-01 ,8.755808e-01 ,8.741095e-01 ,8.718604e-01 ,&
        & 8.693433e-01 ,8.670686e-01 ,8.655464e-01 ,8.652872e-01 ,8.668006e-01 ,&
        & 8.705973e-01 ,8.771874e-01 ,8.870809e-01 ,8.678284e-01 ,8.732315e-01 ,&
        & 8.778084e-01 ,8.816166e-01 ,8.847146e-01 ,8.871603e-01 ,8.890116e-01 ,&
        & 8.903266e-01 ,8.911632e-01 ,8.915796e-01 ,8.916337e-01 ,8.913834e-01 ,&
        & 8.908869e-01 ,8.902022e-01 ,8.893873e-01 ,8.885001e-01 ,8.875986e-01 ,&
        & 8.867411e-01 ,8.859852e-01 ,8.853891e-01 ,8.850111e-01 ,8.849089e-01 ,&
        & 8.851405e-01 ,8.857639e-01 ,8.868372e-01 ,8.884185e-01 ,8.905656e-01 ,&
        & 8.933368e-01 ,8.967899e-01 ,9.0098e-01  /)
      asyliq1(:, 20) = (/ &
        & 8.063610e-01 ,7.938147e-01 ,7.921304e-01 ,7.985092e-01 ,8.101339e-01 ,&
        & 8.242175e-01 ,8.379913e-01 ,8.486920e-01 ,8.535547e-01 ,8.498083e-01 ,&
        & 8.224849e-01 ,8.405509e-01 ,8.542436e-01 ,8.640770e-01 ,8.705653e-01 ,&
        & 8.742227e-01 ,8.755630e-01 ,8.751004e-01 ,8.733491e-01 ,8.708231e-01 ,&
        & 8.680365e-01 ,8.655035e-01 ,8.637381e-01 ,8.632544e-01 ,8.645665e-01 ,&
        & 8.681885e-01 ,8.746346e-01 ,8.844188e-01 ,8.648180e-01 ,8.700563e-01 ,&
        & 8.744672e-01 ,8.781087e-01 ,8.810393e-01 ,8.833174e-01 ,8.850011e-01 ,&
        & 8.861485e-01 ,8.868183e-01 ,8.870687e-01 ,8.869579e-01 ,8.865441e-01 ,&
        & 8.858857e-01 ,8.850412e-01 ,8.840686e-01 ,8.830263e-01 ,8.819726e-01 ,&
        & 8.809658e-01 ,8.800642e-01 ,8.793260e-01 ,8.788099e-01 ,8.785737e-01 ,&
        & 8.786758e-01 ,8.791746e-01 ,8.801283e-01 ,8.815955e-01 ,8.836340e-01 ,&
        & 8.863024e-01 ,8.896592e-01 ,8.9376e-01  /)
      asyliq1(:, 21) = (/ &
        & 7.885899e-01 ,7.937172e-01 ,8.020658e-01 ,8.123971e-01 ,8.235502e-01 ,&
        & 8.343776e-01 ,8.437336e-01 ,8.504711e-01 ,8.534421e-01 ,8.514978e-01 ,&
        & 8.238888e-01 ,8.417463e-01 ,8.552057e-01 ,8.647853e-01 ,8.710038e-01 ,&
        & 8.743798e-01 ,8.754319e-01 ,8.746786e-01 ,8.726386e-01 ,8.698303e-01 ,&
        & 8.667724e-01 ,8.639836e-01 ,8.619823e-01 ,8.612870e-01 ,8.624165e-01 ,&
        & 8.658893e-01 ,8.722241e-01 ,8.819394e-01 ,8.620216e-01 ,8.671239e-01 ,&
        & 8.713983e-01 ,8.749032e-01 ,8.776970e-01 ,8.798385e-01 ,8.813860e-01 ,&
        & 8.823980e-01 ,8.829332e-01 ,8.830500e-01 ,8.828068e-01 ,8.822623e-01 ,&
        & 8.814750e-01 ,8.805031e-01 ,8.794056e-01 ,8.782407e-01 ,8.770672e-01 ,&
        & 8.759432e-01 ,8.749275e-01 ,8.740784e-01 ,8.734547e-01 ,8.731146e-01 ,&
        & 8.731170e-01 ,8.735199e-01 ,8.743823e-01 ,8.757625e-01 ,8.777191e-01 ,&
        & 8.803105e-01 ,8.835953e-01 ,8.8763e-01  /)
      asyliq1(:, 22) = (/ &
        & 7.811516e-01 ,7.962229e-01 ,8.096199e-01 ,8.212996e-01 ,8.312212e-01 ,&
        & 8.393430e-01 ,8.456236e-01 ,8.500214e-01 ,8.524950e-01 ,8.530031e-01 ,&
        & 8.251485e-01 ,8.429043e-01 ,8.562461e-01 ,8.656954e-01 ,8.717737e-01 ,&
        & 8.750020e-01 ,8.759022e-01 ,8.749953e-01 ,8.728027e-01 ,8.698461e-01 ,&
        & 8.666466e-01 ,8.637257e-01 ,8.616047e-01 ,8.608051e-01 ,8.618483e-01 ,&
        & 8.652557e-01 ,8.715487e-01 ,8.812485e-01 ,8.611645e-01 ,8.662052e-01 ,&
        & 8.704173e-01 ,8.738594e-01 ,8.765901e-01 ,8.786678e-01 ,8.801517e-01 ,&
        & 8.810999e-01 ,8.815713e-01 ,8.816246e-01 ,8.813185e-01 ,8.807114e-01 ,&
        & 8.798621e-01 ,8.788290e-01 ,8.776713e-01 ,8.764470e-01 ,8.752152e-01 ,&
        & 8.740343e-01 ,8.729631e-01 ,8.720602e-01 ,8.713842e-01 ,8.709936e-01 ,&
        & 8.709475e-01 ,8.713041e-01 ,8.721221e-01 ,8.734602e-01 ,8.753774e-01 ,&
        & 8.779319e-01 ,8.811825e-01 ,8.8519e-01  /)
      asyliq1(:, 23) = (/ &
        & 7.865744e-01 ,8.093340e-01 ,8.257596e-01 ,8.369940e-01 ,8.441574e-01 ,&
        & 8.483602e-01 ,8.507096e-01 ,8.523139e-01 ,8.542834e-01 ,8.577321e-01 ,&
        & 8.288960e-01 ,8.465308e-01 ,8.597175e-01 ,8.689830e-01 ,8.748542e-01 ,&
        & 8.778584e-01 ,8.785222e-01 ,8.773728e-01 ,8.749370e-01 ,8.717419e-01 ,&
        & 8.683145e-01 ,8.651816e-01 ,8.628704e-01 ,8.619077e-01 ,8.628205e-01 ,&
        & 8.661356e-01 ,8.723803e-01 ,8.820815e-01 ,8.616715e-01 ,8.666389e-01 ,&
        & 8.707753e-01 ,8.741398e-01 ,8.767912e-01 ,8.787885e-01 ,8.801908e-01 ,&
        & 8.810570e-01 ,8.814460e-01 ,8.814167e-01 ,8.810283e-01 ,8.803395e-01 ,&
        & 8.794095e-01 ,8.782971e-01 ,8.770613e-01 ,8.757610e-01 ,8.744553e-01 ,&
        & 8.732031e-01 ,8.720634e-01 ,8.710951e-01 ,8.703572e-01 ,8.699086e-01 ,&
        & 8.698084e-01 ,8.701155e-01 ,8.708887e-01 ,8.721872e-01 ,8.740698e-01 ,&
        & 8.765957e-01 ,8.798235e-01 ,8.8381e-01  /)
      asyliq1(:, 24) = (/ &
        & 8.069513e-01 ,8.262939e-01 ,8.398241e-01 ,8.486352e-01 ,8.538213e-01 ,&
        & 8.564743e-01 ,8.576854e-01 ,8.585455e-01 ,8.601452e-01 ,8.635755e-01 ,&
        & 8.337383e-01 ,8.512655e-01 ,8.643049e-01 ,8.733896e-01 ,8.790535e-01 ,&
        & 8.818295e-01 ,8.822518e-01 ,8.808533e-01 ,8.781676e-01 ,8.747284e-01 ,&
        & 8.710690e-01 ,8.677229e-01 ,8.652236e-01 ,8.641047e-01 ,8.648993e-01 ,&
        & 8.681413e-01 ,8.743640e-01 ,8.841007e-01 ,8.633558e-01 ,8.682719e-01 ,&
        & 8.723543e-01 ,8.756621e-01 ,8.782547e-01 ,8.801915e-01 ,8.815318e-01 ,&
        & 8.823347e-01 ,8.826598e-01 ,8.825663e-01 ,8.821135e-01 ,8.813608e-01 ,&
        & 8.803674e-01 ,8.791928e-01 ,8.778960e-01 ,8.765366e-01 ,8.751738e-01 ,&
        & 8.738670e-01 ,8.726755e-01 ,8.716585e-01 ,8.708755e-01 ,8.703856e-01 ,&
        & 8.702483e-01 ,8.705229e-01 ,8.712687e-01 ,8.725448e-01 ,8.744109e-01 ,&
        & 8.769260e-01 ,8.801496e-01 ,8.8414e-01  /)
      asyliq1(:, 25) = (/ &
        & 8.252182e-01 ,8.379244e-01 ,8.471709e-01 ,8.535760e-01 ,8.577540e-01 ,&
        & 8.603183e-01 ,8.618820e-01 ,8.630578e-01 ,8.644587e-01 ,8.666970e-01 ,&
        & 8.362159e-01 ,8.536817e-01 ,8.666387e-01 ,8.756240e-01 ,8.811746e-01 ,&
        & 8.838273e-01 ,8.841191e-01 ,8.825871e-01 ,8.797681e-01 ,8.761992e-01 ,&
        & 8.724174e-01 ,8.689593e-01 ,8.663623e-01 ,8.651632e-01 ,8.658988e-01 ,&
        & 8.691064e-01 ,8.753226e-01 ,8.850847e-01 ,8.641620e-01 ,8.690500e-01 ,&
        & 8.731026e-01 ,8.763795e-01 ,8.789400e-01 ,8.808438e-01 ,8.821503e-01 ,&
        & 8.829191e-01 ,8.832095e-01 ,8.830813e-01 ,8.825938e-01 ,8.818064e-01 ,&
        & 8.807787e-01 ,8.795704e-01 ,8.782408e-01 ,8.768493e-01 ,8.754557e-01 ,&
        & 8.741193e-01 ,8.728995e-01 ,8.718561e-01 ,8.710484e-01 ,8.705360e-01 ,&
        & 8.703782e-01 ,8.706347e-01 ,8.713650e-01 ,8.726285e-01 ,8.744849e-01 ,&
        & 8.769933e-01 ,8.802136e-01 ,8.8421e-01  /)
      asyliq1(:, 26) = (/ &
        & 8.370583e-01 ,8.467920e-01 ,8.537769e-01 ,8.585136e-01 ,8.615034e-01 ,&
        & 8.632474e-01 ,8.642468e-01 ,8.650026e-01 ,8.660161e-01 ,8.677882e-01 ,&
        & 8.369760e-01 ,8.543821e-01 ,8.672699e-01 ,8.761782e-01 ,8.816454e-01 ,&
        & 8.842103e-01 ,8.844114e-01 ,8.827872e-01 ,8.798766e-01 ,8.762179e-01 ,&
        & 8.723500e-01 ,8.688112e-01 ,8.661403e-01 ,8.648758e-01 ,8.655563e-01 ,&
        & 8.687206e-01 ,8.749072e-01 ,8.846546e-01 ,8.636289e-01 ,8.684849e-01 ,&
        & 8.725054e-01 ,8.757501e-01 ,8.782785e-01 ,8.801503e-01 ,8.814249e-01 ,&
        & 8.821620e-01 ,8.824211e-01 ,8.822620e-01 ,8.817440e-01 ,8.809268e-01 ,&
        & 8.798699e-01 ,8.786330e-01 ,8.772756e-01 ,8.758572e-01 ,8.744374e-01 ,&
        & 8.730760e-01 ,8.718323e-01 ,8.707660e-01 ,8.699366e-01 ,8.694039e-01 ,&
        & 8.692271e-01 ,8.694661e-01 ,8.701803e-01 ,8.714293e-01 ,8.732727e-01 ,&
        & 8.757702e-01 ,8.789811e-01 ,8.8297e-01  /)
      asyliq1(:, 27) = (/ &
        & 8.430819e-01 ,8.510060e-01 ,8.567270e-01 ,8.606533e-01 ,8.631934e-01 ,&
        & 8.647554e-01 ,8.657471e-01 ,8.665760e-01 ,8.676496e-01 ,8.693754e-01 ,&
        & 8.384298e-01 ,8.557913e-01 ,8.686214e-01 ,8.774605e-01 ,8.828495e-01 ,&
        & 8.853287e-01 ,8.854393e-01 ,8.837215e-01 ,8.807161e-01 ,8.769639e-01 ,&
        & 8.730053e-01 ,8.693812e-01 ,8.666321e-01 ,8.652988e-01 ,8.659219e-01 ,&
        & 8.690419e-01 ,8.751999e-01 ,8.849360e-01 ,8.638013e-01 ,8.686371e-01 ,&
        & 8.726369e-01 ,8.758605e-01 ,8.783674e-01 ,8.802176e-01 ,8.814705e-01 ,&
        & 8.821859e-01 ,8.824234e-01 ,8.822429e-01 ,8.817038e-01 ,8.808658e-01 ,&
        & 8.797887e-01 ,8.785323e-01 ,8.771560e-01 ,8.757196e-01 ,8.742828e-01 ,&
        & 8.729052e-01 ,8.716467e-01 ,8.705666e-01 ,8.697250e-01 ,8.691812e-01 ,&
        & 8.689950e-01 ,8.692264e-01 ,8.699346e-01 ,8.711795e-01 ,8.730209e-01 ,&
        & 8.755181e-01 ,8.787312e-01 ,8.8272e-01  /)
      asyliq1(:, 28) = (/ &
        & 8.452284e-01 ,8.522700e-01 ,8.572973e-01 ,8.607031e-01 ,8.628802e-01 ,&
        & 8.642215e-01 ,8.651198e-01 ,8.659679e-01 ,8.671588e-01 ,8.690853e-01 ,&
        & 8.383803e-01 ,8.557485e-01 ,8.685851e-01 ,8.774303e-01 ,8.828245e-01 ,&
        & 8.853077e-01 ,8.854207e-01 ,8.837034e-01 ,8.806962e-01 ,8.769398e-01 ,&
        & 8.729740e-01 ,8.693393e-01 ,8.665761e-01 ,8.652247e-01 ,8.658253e-01 ,&
        & 8.689182e-01 ,8.750438e-01 ,8.847424e-01 ,8.636140e-01 ,8.684449e-01 ,&
        & 8.724400e-01 ,8.756589e-01 ,8.781613e-01 ,8.800072e-01 ,8.812559e-01 ,&
        & 8.819671e-01 ,8.822007e-01 ,8.820165e-01 ,8.814737e-01 ,8.806322e-01 ,&
        & 8.795518e-01 ,8.782923e-01 ,8.769129e-01 ,8.754737e-01 ,8.740342e-01 ,&
        & 8.726542e-01 ,8.713934e-01 ,8.703111e-01 ,8.694677e-01 ,8.689222e-01 ,&
        & 8.687344e-01 ,8.689646e-01 ,8.696715e-01 ,8.709156e-01 ,8.727563e-01 ,&
        & 8.752531e-01 ,8.784659e-01 ,8.8245e-01  /)
      asyliq1(:, 29) = (/ &
        & 7.800869e-01 ,8.091120e-01 ,8.325369e-01 ,8.466266e-01 ,8.515495e-01 ,&
        & 8.499371e-01 ,8.456203e-01 ,8.430521e-01 ,8.470286e-01 ,8.625431e-01 ,&
        & 8.402261e-01 ,8.610822e-01 ,8.776608e-01 ,8.904485e-01 ,8.999294e-01 ,&
        & 9.065860e-01 ,9.108995e-01 ,9.133503e-01 ,9.144187e-01 ,9.145855e-01 ,&
        & 9.143320e-01 ,9.141402e-01 ,9.144933e-01 ,9.158754e-01 ,9.187716e-01 ,&
        & 9.236677e-01 ,9.310503e-01 ,9.414058e-01 ,9.239108e-01 ,9.300719e-01 ,&
        & 9.353612e-01 ,9.398378e-01 ,9.435609e-01 ,9.465895e-01 ,9.489829e-01 ,&
        & 9.508000e-01 ,9.521002e-01 ,9.529424e-01 ,9.533860e-01 ,9.534902e-01 ,&
        & 9.533143e-01 ,9.529177e-01 ,9.523596e-01 ,9.516997e-01 ,9.509973e-01 ,&
        & 9.503121e-01 ,9.497037e-01 ,9.492317e-01 ,9.489558e-01 ,9.489356e-01 ,&
        & 9.492311e-01 ,9.499019e-01 ,9.510077e-01 ,9.526084e-01 ,9.547636e-01 ,&
        & 9.575331e-01 ,9.609766e-01 ,9.6515e-01  /)

! Spherical Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
      extice2(:, 16) = (/ &
! band 16
        & 4.101824e-01 ,2.435514e-01 ,1.713697e-01 ,1.314865e-01 ,1.063406e-01 ,&
        & 8.910701e-02 ,7.659480e-02 ,6.711784e-02 ,5.970353e-02 ,5.375249e-02 ,&
        & 4.887577e-02 ,4.481025e-02 ,4.137171e-02 ,3.842744e-02 ,3.587948e-02 ,&
        & 3.365396e-02 ,3.169419e-02 ,2.995593e-02 ,2.840419e-02 ,2.701091e-02 ,&
        & 2.575336e-02 ,2.461293e-02 ,2.357423e-02 ,2.262443e-02 ,2.175276e-02 ,&
        & 2.095012e-02 ,2.020875e-02 ,1.952199e-02 ,1.888412e-02 ,1.829018e-02 ,&
        & 1.773586e-02 ,1.721738e-02 ,1.673144e-02 ,1.627510e-02 ,1.584579e-02 ,&
        & 1.544122e-02 ,1.505934e-02 ,1.469833e-02 ,1.435654e-02 ,1.403251e-02 ,&
        & 1.372492e-02 ,1.343255e-02 ,1.315433e-02  /)
      extice2(:, 17) = (/ &
! band 17
        & 3.836650e-01 ,2.304055e-01 ,1.637265e-01 ,1.266681e-01 ,1.031602e-01 ,&
        & 8.695191e-02 ,7.511544e-02 ,6.610009e-02 ,5.900909e-02 ,5.328833e-02 ,&
        & 4.857728e-02 ,4.463133e-02 ,4.127880e-02 ,3.839567e-02 ,3.589013e-02 ,&
        & 3.369280e-02 ,3.175027e-02 ,3.002079e-02 ,2.847121e-02 ,2.707493e-02 ,&
        & 2.581031e-02 ,2.465962e-02 ,2.360815e-02 ,2.264363e-02 ,2.175571e-02 ,&
        & 2.093563e-02 ,2.017592e-02 ,1.947015e-02 ,1.881278e-02 ,1.819901e-02 ,&
        & 1.762463e-02 ,1.708598e-02 ,1.657982e-02 ,1.610330e-02 ,1.565390e-02 ,&
        & 1.522937e-02 ,1.482768e-02 ,1.444706e-02 ,1.408588e-02 ,1.374270e-02 ,&
        & 1.341619e-02 ,1.310517e-02 ,1.280857e-02  /)
      extice2(:, 18) = (/ &
! band 18
        & 4.152673e-01 ,2.436816e-01 ,1.702243e-01 ,1.299704e-01 ,1.047528e-01 ,&
        & 8.756039e-02 ,7.513327e-02 ,6.575690e-02 ,5.844616e-02 ,5.259609e-02 ,&
        & 4.781531e-02 ,4.383980e-02 ,4.048517e-02 ,3.761891e-02 ,3.514342e-02 ,&
        & 3.298525e-02 ,3.108814e-02 ,2.940825e-02 ,2.791096e-02 ,2.656858e-02 ,&
        & 2.535869e-02 ,2.426297e-02 ,2.326627e-02 ,2.235602e-02 ,2.152164e-02 ,&
        & 2.075420e-02 ,2.004613e-02 ,1.939091e-02 ,1.878296e-02 ,1.821744e-02 ,&
        & 1.769015e-02 ,1.719741e-02 ,1.673600e-02 ,1.630308e-02 ,1.589615e-02 ,&
        & 1.551298e-02 ,1.515159e-02 ,1.481021e-02 ,1.448726e-02 ,1.418131e-02 ,&
        & 1.389109e-02 ,1.361544e-02 ,1.335330e-02  /)
      extice2(:, 19) = (/ &
! band 19
        & 3.873250e-01 ,2.331609e-01 ,1.655002e-01 ,1.277753e-01 ,1.038247e-01 ,&
        & 8.731780e-02 ,7.527638e-02 ,6.611873e-02 ,5.892850e-02 ,5.313885e-02 ,&
        & 4.838068e-02 ,4.440356e-02 ,4.103167e-02 ,3.813804e-02 ,3.562870e-02 ,&
        & 3.343269e-02 ,3.149539e-02 ,2.977414e-02 ,2.823510e-02 ,2.685112e-02 ,&
        & 2.560015e-02 ,2.446411e-02 ,2.342805e-02 ,2.247948e-02 ,2.160789e-02 ,&
        & 2.080438e-02 ,2.006139e-02 ,1.937238e-02 ,1.873177e-02 ,1.813469e-02 ,&
        & 1.757689e-02 ,1.705468e-02 ,1.656479e-02 ,1.610435e-02 ,1.567081e-02 ,&
        & 1.526192e-02 ,1.487565e-02 ,1.451020e-02 ,1.416396e-02 ,1.383546e-02 ,&
        & 1.352339e-02 ,1.322657e-02 ,1.294392e-02  /)
      extice2(:, 20) = (/ &
! band 20
        & 3.784280e-01 ,2.291396e-01 ,1.632551e-01 ,1.263775e-01 ,1.028944e-01 ,&
        & 8.666975e-02 ,7.480952e-02 ,6.577335e-02 ,5.866714e-02 ,5.293694e-02 ,&
        & 4.822153e-02 ,4.427547e-02 ,4.092626e-02 ,3.804918e-02 ,3.555184e-02 ,&
        & 3.336440e-02 ,3.143307e-02 ,2.971577e-02 ,2.817912e-02 ,2.679632e-02 ,&
        & 2.554558e-02 ,2.440903e-02 ,2.337187e-02 ,2.242173e-02 ,2.154821e-02 ,&
        & 2.074249e-02 ,1.999706e-02 ,1.930546e-02 ,1.866212e-02 ,1.806221e-02 ,&
        & 1.750152e-02 ,1.697637e-02 ,1.648352e-02 ,1.602010e-02 ,1.558358e-02 ,&
        & 1.517172e-02 ,1.478250e-02 ,1.441413e-02 ,1.406498e-02 ,1.373362e-02 ,&
        & 1.341872e-02 ,1.311911e-02 ,1.283371e-02  /)
      extice2(:, 21) = (/ &
! band 21
        & 3.719909e-01 ,2.259490e-01 ,1.613144e-01 ,1.250648e-01 ,1.019462e-01 ,&
        & 8.595358e-02 ,7.425064e-02 ,6.532618e-02 ,5.830218e-02 ,5.263421e-02 ,&
        & 4.796697e-02 ,4.405891e-02 ,4.074013e-02 ,3.788776e-02 ,3.541071e-02 ,&
        & 3.324008e-02 ,3.132280e-02 ,2.961733e-02 ,2.809071e-02 ,2.671645e-02 ,&
        & 2.547302e-02 ,2.434276e-02 ,2.331102e-02 ,2.236558e-02 ,2.149614e-02 ,&
        & 2.069397e-02 ,1.995163e-02 ,1.926272e-02 ,1.862174e-02 ,1.802389e-02 ,&
        & 1.746500e-02 ,1.694142e-02 ,1.644994e-02 ,1.598772e-02 ,1.555225e-02 ,&
        & 1.514129e-02 ,1.475286e-02 ,1.438515e-02 ,1.403659e-02 ,1.370572e-02 ,&
        & 1.339124e-02 ,1.309197e-02 ,1.280685e-02  /)
      extice2(:, 22) = (/ &
! band 22
        & 3.713158e-01 ,2.253816e-01 ,1.608461e-01 ,1.246718e-01 ,1.016109e-01 ,&
        & 8.566332e-02 ,7.399666e-02 ,6.510199e-02 ,5.810290e-02 ,5.245608e-02 ,&
        & 4.780702e-02 ,4.391478e-02 ,4.060989e-02 ,3.776982e-02 ,3.530374e-02 ,&
        & 3.314296e-02 ,3.123458e-02 ,2.953719e-02 ,2.801794e-02 ,2.665043e-02 ,&
        & 2.541321e-02 ,2.428868e-02 ,2.326224e-02 ,2.232173e-02 ,2.145688e-02 ,&
        & 2.065899e-02 ,1.992067e-02 ,1.923552e-02 ,1.859808e-02 ,1.800356e-02 ,&
        & 1.744782e-02 ,1.692721e-02 ,1.643855e-02 ,1.597900e-02 ,1.554606e-02 ,&
        & 1.513751e-02 ,1.475137e-02 ,1.438586e-02 ,1.403938e-02 ,1.371050e-02 ,&
        & 1.339793e-02 ,1.310050e-02 ,1.281713e-02  /)
      extice2(:, 23) = (/ &
! band 23
        & 3.605883e-01 ,2.204388e-01 ,1.580431e-01 ,1.229033e-01 ,1.004203e-01 ,&
        & 8.482616e-02 ,7.338941e-02 ,6.465105e-02 ,5.776176e-02 ,5.219398e-02 ,&
        & 4.760288e-02 ,4.375369e-02 ,4.048111e-02 ,3.766539e-02 ,3.521771e-02 ,&
        & 3.307079e-02 ,3.117277e-02 ,2.948303e-02 ,2.796929e-02 ,2.660560e-02 ,&
        & 2.537086e-02 ,2.424772e-02 ,2.322182e-02 ,2.228114e-02 ,2.141556e-02 ,&
        & 2.061649e-02 ,1.987661e-02 ,1.918962e-02 ,1.855009e-02 ,1.795330e-02 ,&
        & 1.739514e-02 ,1.687199e-02 ,1.638069e-02 ,1.591845e-02 ,1.548276e-02 ,&
        & 1.507143e-02 ,1.468249e-02 ,1.431416e-02 ,1.396486e-02 ,1.363318e-02 ,&
        & 1.331781e-02 ,1.301759e-02 ,1.273147e-02  /)
      extice2(:, 24) = (/ &
! band 24
        & 3.527890e-01 ,2.168469e-01 ,1.560090e-01 ,1.216216e-01 ,9.955787e-02 ,&
        & 8.421942e-02 ,7.294827e-02 ,6.432192e-02 ,5.751081e-02 ,5.199888e-02 ,&
        & 4.744835e-02 ,4.362899e-02 ,4.037847e-02 ,3.757910e-02 ,3.514351e-02 ,&
        & 3.300546e-02 ,3.111382e-02 ,2.942853e-02 ,2.791775e-02 ,2.655584e-02 ,&
        & 2.532195e-02 ,2.419892e-02 ,2.317255e-02 ,2.223092e-02 ,2.136402e-02 ,&
        & 2.056334e-02 ,1.982160e-02 ,1.913258e-02 ,1.849087e-02 ,1.789178e-02 ,&
        & 1.733124e-02 ,1.680565e-02 ,1.631187e-02 ,1.584711e-02 ,1.540889e-02 ,&
        & 1.499502e-02 ,1.460354e-02 ,1.423269e-02 ,1.388088e-02 ,1.354670e-02 ,&
        & 1.322887e-02 ,1.292620e-02 ,1.263767e-02  /)
      extice2(:, 25) = (/ &
! band 25
        & 3.477874e-01 ,2.143515e-01 ,1.544887e-01 ,1.205942e-01 ,9.881779e-02 ,&
        & 8.366261e-02 ,7.251586e-02 ,6.397790e-02 ,5.723183e-02 ,5.176908e-02 ,&
        & 4.725658e-02 ,4.346715e-02 ,4.024055e-02 ,3.746055e-02 ,3.504080e-02 ,&
        & 3.291583e-02 ,3.103507e-02 ,2.935891e-02 ,2.785582e-02 ,2.650042e-02 ,&
        & 2.527206e-02 ,2.415376e-02 ,2.313142e-02 ,2.219326e-02 ,2.132934e-02 ,&
        & 2.053122e-02 ,1.979169e-02 ,1.910456e-02 ,1.846448e-02 ,1.786680e-02 ,&
        & 1.730745e-02 ,1.678289e-02 ,1.628998e-02 ,1.582595e-02 ,1.538835e-02 ,&
        & 1.497499e-02 ,1.458393e-02 ,1.421341e-02 ,1.386187e-02 ,1.352788e-02 ,&
        & 1.321019e-02 ,1.290762e-02 ,1.261913e-02  /)
      extice2(:, 26) = (/ &
! band 26
        & 3.453721e-01 ,2.130744e-01 ,1.536698e-01 ,1.200140e-01 ,9.838078e-02 ,&
        & 8.331940e-02 ,7.223803e-02 ,6.374775e-02 ,5.703770e-02 ,5.160290e-02 ,&
        & 4.711259e-02 ,4.334110e-02 ,4.012923e-02 ,3.736150e-02 ,3.495208e-02 ,&
        & 3.283589e-02 ,3.096267e-02 ,2.929302e-02 ,2.779560e-02 ,2.644517e-02 ,&
        & 2.522119e-02 ,2.410677e-02 ,2.308788e-02 ,2.215281e-02 ,2.129165e-02 ,&
        & 2.049602e-02 ,1.975874e-02 ,1.907365e-02 ,1.843542e-02 ,1.783943e-02 ,&
        & 1.728162e-02 ,1.675847e-02 ,1.626685e-02 ,1.580401e-02 ,1.536750e-02 ,&
        & 1.495515e-02 ,1.456502e-02 ,1.419537e-02 ,1.384463e-02 ,1.351139e-02 ,&
        & 1.319438e-02 ,1.289246e-02 ,1.260456e-02  /)
      extice2(:, 27) = (/ &
! band 27
        & 3.417883e-01 ,2.113379e-01 ,1.526395e-01 ,1.193347e-01 ,9.790253e-02 ,&
        & 8.296715e-02 ,7.196979e-02 ,6.353806e-02 ,5.687024e-02 ,5.146670e-02 ,&
        & 4.700001e-02 ,4.324667e-02 ,4.004894e-02 ,3.729233e-02 ,3.489172e-02 ,&
        & 3.278257e-02 ,3.091499e-02 ,2.924987e-02 ,2.775609e-02 ,2.640859e-02 ,&
        & 2.518695e-02 ,2.407439e-02 ,2.305697e-02 ,2.212303e-02 ,2.126273e-02 ,&
        & 2.046774e-02 ,1.973090e-02 ,1.904610e-02 ,1.840801e-02 ,1.781204e-02 ,&
        & 1.725417e-02 ,1.673086e-02 ,1.623902e-02 ,1.577590e-02 ,1.533906e-02 ,&
        & 1.492634e-02 ,1.453580e-02 ,1.416571e-02 ,1.381450e-02 ,1.348078e-02 ,&
        & 1.316327e-02 ,1.286082e-02 ,1.257240e-02  /)
      extice2(:, 28) = (/ &
! band 28
        & 3.416111e-01 ,2.114124e-01 ,1.527734e-01 ,1.194809e-01 ,9.804612e-02 ,&
        & 8.310287e-02 ,7.209595e-02 ,6.365442e-02 ,5.697710e-02 ,5.156460e-02 ,&
        & 4.708957e-02 ,4.332850e-02 ,4.012361e-02 ,3.736037e-02 ,3.495364e-02 ,&
        & 3.283879e-02 ,3.096593e-02 ,2.929589e-02 ,2.779751e-02 ,2.644571e-02 ,&
        & 2.522004e-02 ,2.410369e-02 ,2.308271e-02 ,2.214542e-02 ,2.128195e-02 ,&
        & 2.048396e-02 ,1.974429e-02 ,1.905679e-02 ,1.841614e-02 ,1.781774e-02 ,&
        & 1.725754e-02 ,1.673203e-02 ,1.623807e-02 ,1.577293e-02 ,1.533416e-02 ,&
        & 1.491958e-02 ,1.452727e-02 ,1.415547e-02 ,1.380262e-02 ,1.346732e-02 ,&
        & 1.314830e-02 ,1.284439e-02 ,1.255456e-02  /)
      extice2(:, 29) = (/ &
! band 29
        & 4.196611e-01 ,2.493642e-01 ,1.761261e-01 ,1.357197e-01 ,1.102161e-01 ,&
        & 9.269376e-02 ,7.992985e-02 ,7.022538e-02 ,6.260168e-02 ,5.645603e-02 ,&
        & 5.139732e-02 ,4.716088e-02 ,4.356133e-02 ,4.046498e-02 ,3.777303e-02 ,&
        & 3.541094e-02 ,3.332137e-02 ,3.145954e-02 ,2.978998e-02 ,2.828419e-02 ,&
        & 2.691905e-02 ,2.567559e-02 ,2.453811e-02 ,2.349350e-02 ,2.253072e-02 ,&
        & 2.164042e-02 ,2.081464e-02 ,2.004652e-02 ,1.933015e-02 ,1.866041e-02 ,&
        & 1.803283e-02 ,1.744348e-02 ,1.688894e-02 ,1.636616e-02 ,1.587244e-02 ,&
        & 1.540539e-02 ,1.496287e-02 ,1.454295e-02 ,1.414392e-02 ,1.376423e-02 ,&
        & 1.340247e-02 ,1.305739e-02 ,1.272784e-02  /)

! single-scattering albedo: unitless
      ssaice2(:, 16) = (/ &
! band 16
        & 6.630615e-01 ,6.451169e-01 ,6.333696e-01 ,6.246927e-01 ,6.178420e-01 ,&
        & 6.121976e-01 ,6.074069e-01 ,6.032505e-01 ,5.995830e-01 ,5.963030e-01 ,&
        & 5.933372e-01 ,5.906311e-01 ,5.881427e-01 ,5.858395e-01 ,5.836955e-01 ,&
        & 5.816896e-01 ,5.798046e-01 ,5.780264e-01 ,5.763429e-01 ,5.747441e-01 ,&
        & 5.732213e-01 ,5.717672e-01 ,5.703754e-01 ,5.690403e-01 ,5.677571e-01 ,&
        & 5.665215e-01 ,5.653297e-01 ,5.641782e-01 ,5.630643e-01 ,5.619850e-01 ,&
        & 5.609381e-01 ,5.599214e-01 ,5.589328e-01 ,5.579707e-01 ,5.570333e-01 ,&
        & 5.561193e-01 ,5.552272e-01 ,5.543558e-01 ,5.535041e-01 ,5.526708e-01 ,&
        & 5.518551e-01 ,5.510561e-01 ,5.502729e-01  /)
      ssaice2(:, 17) = (/ &
! band 17
        & 7.689749e-01 ,7.398171e-01 ,7.205819e-01 ,7.065690e-01 ,6.956928e-01 ,&
        & 6.868989e-01 ,6.795813e-01 ,6.733606e-01 ,6.679838e-01 ,6.632742e-01 ,&
        & 6.591036e-01 ,6.553766e-01 ,6.520197e-01 ,6.489757e-01 ,6.461991e-01 ,&
        & 6.436531e-01 ,6.413075e-01 ,6.391375e-01 ,6.371221e-01 ,6.352438e-01 ,&
        & 6.334876e-01 ,6.318406e-01 ,6.302918e-01 ,6.288315e-01 ,6.274512e-01 ,&
        & 6.261436e-01 ,6.249022e-01 ,6.237211e-01 ,6.225953e-01 ,6.215201e-01 ,&
        & 6.204914e-01 ,6.195055e-01 ,6.185592e-01 ,6.176492e-01 ,6.167730e-01 ,&
        & 6.159280e-01 ,6.151120e-01 ,6.143228e-01 ,6.135587e-01 ,6.128177e-01 ,&
        & 6.120984e-01 ,6.113993e-01 ,6.107189e-01  /)
      ssaice2(:, 18) = (/ &
! band 18
        & 9.956167e-01 ,9.814770e-01 ,9.716104e-01 ,9.639746e-01 ,9.577179e-01 ,&
        & 9.524010e-01 ,9.477672e-01 ,9.436527e-01 ,9.399467e-01 ,9.365708e-01 ,&
        & 9.334672e-01 ,9.305921e-01 ,9.279118e-01 ,9.253993e-01 ,9.230330e-01 ,&
        & 9.207954e-01 ,9.186719e-01 ,9.166501e-01 ,9.147199e-01 ,9.128722e-01 ,&
        & 9.110997e-01 ,9.093956e-01 ,9.077544e-01 ,9.061708e-01 ,9.046406e-01 ,&
        & 9.031598e-01 ,9.017248e-01 ,9.003326e-01 ,8.989804e-01 ,8.976655e-01 ,&
        & 8.963857e-01 ,8.951389e-01 ,8.939233e-01 ,8.927370e-01 ,8.915785e-01 ,&
        & 8.904464e-01 ,8.893392e-01 ,8.882559e-01 ,8.871951e-01 ,8.861559e-01 ,&
        & 8.851373e-01 ,8.841383e-01 ,8.831581e-01  /)
      ssaice2(:, 19) = (/ &
! band 19
        & 9.723177e-01 ,9.452119e-01 ,9.267592e-01 ,9.127393e-01 ,9.014238e-01 ,&
        & 8.919334e-01 ,8.837584e-01 ,8.765773e-01 ,8.701736e-01 ,8.643950e-01 ,&
        & 8.591299e-01 ,8.542942e-01 ,8.498230e-01 ,8.456651e-01 ,8.417794e-01 ,&
        & 8.381324e-01 ,8.346964e-01 ,8.314484e-01 ,8.283687e-01 ,8.254408e-01 ,&
        & 8.226505e-01 ,8.199854e-01 ,8.174348e-01 ,8.149891e-01 ,8.126403e-01 ,&
        & 8.103808e-01 ,8.082041e-01 ,8.061044e-01 ,8.040765e-01 ,8.021156e-01 ,&
        & 8.002174e-01 ,7.983781e-01 ,7.965941e-01 ,7.948622e-01 ,7.931795e-01 ,&
        & 7.915432e-01 ,7.899508e-01 ,7.884002e-01 ,7.868891e-01 ,7.854156e-01 ,&
        & 7.839779e-01 ,7.825742e-01 ,7.812031e-01  /)
      ssaice2(:, 20) = (/ &
! band 20
        & 9.933294e-01 ,9.860917e-01 ,9.811564e-01 ,9.774008e-01 ,9.743652e-01 ,&
        & 9.718155e-01 ,9.696159e-01 ,9.676810e-01 ,9.659531e-01 ,9.643915e-01 ,&
        & 9.629667e-01 ,9.616561e-01 ,9.604426e-01 ,9.593125e-01 ,9.582548e-01 ,&
        & 9.572607e-01 ,9.563227e-01 ,9.554347e-01 ,9.545915e-01 ,9.537888e-01 ,&
        & 9.530226e-01 ,9.522898e-01 ,9.515874e-01 ,9.509130e-01 ,9.502643e-01 ,&
        & 9.496394e-01 ,9.490366e-01 ,9.484542e-01 ,9.478910e-01 ,9.473456e-01 ,&
        & 9.468169e-01 ,9.463039e-01 ,9.458056e-01 ,9.453212e-01 ,9.448499e-01 ,&
        & 9.443910e-01 ,9.439438e-01 ,9.435077e-01 ,9.430821e-01 ,9.426666e-01 ,&
        & 9.422607e-01 ,9.418638e-01 ,9.414756e-01  /)
      ssaice2(:, 21) = (/ &
! band 21
        & 9.900787e-01 ,9.828880e-01 ,9.779258e-01 ,9.741173e-01 ,9.710184e-01 ,&
        & 9.684012e-01 ,9.661332e-01 ,9.641301e-01 ,9.623352e-01 ,9.607083e-01 ,&
        & 9.592198e-01 ,9.578474e-01 ,9.565739e-01 ,9.553856e-01 ,9.542715e-01 ,&
        & 9.532226e-01 ,9.522314e-01 ,9.512919e-01 ,9.503986e-01 ,9.495472e-01 ,&
        & 9.487337e-01 ,9.479549e-01 ,9.472077e-01 ,9.464897e-01 ,9.457985e-01 ,&
        & 9.451322e-01 ,9.444890e-01 ,9.438673e-01 ,9.432656e-01 ,9.426826e-01 ,&
        & 9.421173e-01 ,9.415684e-01 ,9.410351e-01 ,9.405164e-01 ,9.400115e-01 ,&
        & 9.395198e-01 ,9.390404e-01 ,9.385728e-01 ,9.381164e-01 ,9.376707e-01 ,&
        & 9.372350e-01 ,9.368091e-01 ,9.363923e-01  /)
      ssaice2(:, 22) = (/ &
! band 22
        & 9.986793e-01 ,9.985239e-01 ,9.983911e-01 ,9.982715e-01 ,9.981606e-01 ,&
        & 9.980562e-01 ,9.979567e-01 ,9.978613e-01 ,9.977691e-01 ,9.976798e-01 ,&
        & 9.975929e-01 ,9.975081e-01 ,9.974251e-01 ,9.973438e-01 ,9.972640e-01 ,&
        & 9.971855e-01 ,9.971083e-01 ,9.970322e-01 ,9.969571e-01 ,9.968830e-01 ,&
        & 9.968099e-01 ,9.967375e-01 ,9.966660e-01 ,9.965951e-01 ,9.965250e-01 ,&
        & 9.964555e-01 ,9.963867e-01 ,9.963185e-01 ,9.962508e-01 ,9.961836e-01 ,&
        & 9.961170e-01 ,9.960508e-01 ,9.959851e-01 ,9.959198e-01 ,9.958550e-01 ,&
        & 9.957906e-01 ,9.957266e-01 ,9.956629e-01 ,9.955997e-01 ,9.955367e-01 ,&
        & 9.954742e-01 ,9.954119e-01 ,9.953500e-01  /)
      ssaice2(:, 23) = (/ &
! band 23
        & 9.997944e-01 ,9.997791e-01 ,9.997664e-01 ,9.997547e-01 ,9.997436e-01 ,&
        & 9.997327e-01 ,9.997219e-01 ,9.997110e-01 ,9.996999e-01 ,9.996886e-01 ,&
        & 9.996771e-01 ,9.996653e-01 ,9.996533e-01 ,9.996409e-01 ,9.996282e-01 ,&
        & 9.996152e-01 ,9.996019e-01 ,9.995883e-01 ,9.995743e-01 ,9.995599e-01 ,&
        & 9.995453e-01 ,9.995302e-01 ,9.995149e-01 ,9.994992e-01 ,9.994831e-01 ,&
        & 9.994667e-01 ,9.994500e-01 ,9.994329e-01 ,9.994154e-01 ,9.993976e-01 ,&
        & 9.993795e-01 ,9.993610e-01 ,9.993422e-01 ,9.993230e-01 ,9.993035e-01 ,&
        & 9.992837e-01 ,9.992635e-01 ,9.992429e-01 ,9.992221e-01 ,9.992008e-01 ,&
        & 9.991793e-01 ,9.991574e-01 ,9.991352e-01  /)
      ssaice2(:, 24) = (/ &
! band 24
        & 9.999949e-01 ,9.999947e-01 ,9.999943e-01 ,9.999939e-01 ,9.999934e-01 ,&
        & 9.999927e-01 ,9.999920e-01 ,9.999913e-01 ,9.999904e-01 ,9.999895e-01 ,&
        & 9.999885e-01 ,9.999874e-01 ,9.999863e-01 ,9.999851e-01 ,9.999838e-01 ,&
        & 9.999824e-01 ,9.999810e-01 ,9.999795e-01 ,9.999780e-01 ,9.999764e-01 ,&
        & 9.999747e-01 ,9.999729e-01 ,9.999711e-01 ,9.999692e-01 ,9.999673e-01 ,&
        & 9.999653e-01 ,9.999632e-01 ,9.999611e-01 ,9.999589e-01 ,9.999566e-01 ,&
        & 9.999543e-01 ,9.999519e-01 ,9.999495e-01 ,9.999470e-01 ,9.999444e-01 ,&
        & 9.999418e-01 ,9.999392e-01 ,9.999364e-01 ,9.999336e-01 ,9.999308e-01 ,&
        & 9.999279e-01 ,9.999249e-01 ,9.999219e-01  /)
      ssaice2(:, 25) = (/ &
! band 25
        & 9.999997e-01 ,9.999997e-01 ,9.999997e-01 ,9.999996e-01 ,9.999996e-01 ,&
        & 9.999995e-01 ,9.999994e-01 ,9.999993e-01 ,9.999993e-01 ,9.999992e-01 ,&
        & 9.999991e-01 ,9.999989e-01 ,9.999988e-01 ,9.999987e-01 ,9.999986e-01 ,&
        & 9.999984e-01 ,9.999983e-01 ,9.999981e-01 ,9.999980e-01 ,9.999978e-01 ,&
        & 9.999976e-01 ,9.999974e-01 ,9.999972e-01 ,9.999971e-01 ,9.999969e-01 ,&
        & 9.999966e-01 ,9.999964e-01 ,9.999962e-01 ,9.999960e-01 ,9.999957e-01 ,&
        & 9.999955e-01 ,9.999953e-01 ,9.999950e-01 ,9.999947e-01 ,9.999945e-01 ,&
        & 9.999942e-01 ,9.999939e-01 ,9.999936e-01 ,9.999934e-01 ,9.999931e-01 ,&
        & 9.999928e-01 ,9.999925e-01 ,9.999921e-01  /)
      ssaice2(:, 26) = (/ &
! band 26
        & 9.999997e-01 ,9.999996e-01 ,9.999996e-01 ,9.999995e-01 ,9.999994e-01 ,&
        & 9.999993e-01 ,9.999992e-01 ,9.999991e-01 ,9.999990e-01 ,9.999989e-01 ,&
        & 9.999987e-01 ,9.999986e-01 ,9.999984e-01 ,9.999982e-01 ,9.999980e-01 ,&
        & 9.999978e-01 ,9.999976e-01 ,9.999974e-01 ,9.999972e-01 ,9.999970e-01 ,&
        & 9.999967e-01 ,9.999965e-01 ,9.999962e-01 ,9.999959e-01 ,9.999956e-01 ,&
        & 9.999954e-01 ,9.999951e-01 ,9.999947e-01 ,9.999944e-01 ,9.999941e-01 ,&
        & 9.999938e-01 ,9.999934e-01 ,9.999931e-01 ,9.999927e-01 ,9.999923e-01 ,&
        & 9.999920e-01 ,9.999916e-01 ,9.999912e-01 ,9.999908e-01 ,9.999904e-01 ,&
        & 9.999899e-01 ,9.999895e-01 ,9.999891e-01  /)
      ssaice2(:, 27) = (/ &
! band 27
        & 9.999987e-01 ,9.999987e-01 ,9.999985e-01 ,9.999984e-01 ,9.999982e-01 ,&
        & 9.999980e-01 ,9.999978e-01 ,9.999976e-01 ,9.999973e-01 ,9.999970e-01 ,&
        & 9.999967e-01 ,9.999964e-01 ,9.999960e-01 ,9.999956e-01 ,9.999952e-01 ,&
        & 9.999948e-01 ,9.999944e-01 ,9.999939e-01 ,9.999934e-01 ,9.999929e-01 ,&
        & 9.999924e-01 ,9.999918e-01 ,9.999913e-01 ,9.999907e-01 ,9.999901e-01 ,&
        & 9.999894e-01 ,9.999888e-01 ,9.999881e-01 ,9.999874e-01 ,9.999867e-01 ,&
        & 9.999860e-01 ,9.999853e-01 ,9.999845e-01 ,9.999837e-01 ,9.999829e-01 ,&
        & 9.999821e-01 ,9.999813e-01 ,9.999804e-01 ,9.999796e-01 ,9.999787e-01 ,&
        & 9.999778e-01 ,9.999768e-01 ,9.999759e-01  /)
      ssaice2(:, 28) = (/ &
! band 28
        & 9.999989e-01 ,9.999989e-01 ,9.999987e-01 ,9.999986e-01 ,9.999984e-01 ,&
        & 9.999982e-01 ,9.999980e-01 ,9.999978e-01 ,9.999975e-01 ,9.999972e-01 ,&
        & 9.999969e-01 ,9.999966e-01 ,9.999962e-01 ,9.999958e-01 ,9.999954e-01 ,&
        & 9.999950e-01 ,9.999945e-01 ,9.999941e-01 ,9.999936e-01 ,9.999931e-01 ,&
        & 9.999925e-01 ,9.999920e-01 ,9.999914e-01 ,9.999908e-01 ,9.999902e-01 ,&
        & 9.999896e-01 ,9.999889e-01 ,9.999883e-01 ,9.999876e-01 ,9.999869e-01 ,&
        & 9.999861e-01 ,9.999854e-01 ,9.999846e-01 ,9.999838e-01 ,9.999830e-01 ,&
        & 9.999822e-01 ,9.999814e-01 ,9.999805e-01 ,9.999796e-01 ,9.999787e-01 ,&
        & 9.999778e-01 ,9.999769e-01 ,9.999759e-01  /)
      ssaice2(:, 29) = (/ &
! band 29
        & 7.042143e-01 ,6.691161e-01 ,6.463240e-01 ,6.296590e-01 ,6.166381e-01 ,&
        & 6.060183e-01 ,5.970908e-01 ,5.894144e-01 ,5.826968e-01 ,5.767343e-01 ,&
        & 5.713804e-01 ,5.665256e-01 ,5.620867e-01 ,5.579987e-01 ,5.542101e-01 ,&
        & 5.506794e-01 ,5.473727e-01 ,5.442620e-01 ,5.413239e-01 ,5.385389e-01 ,&
        & 5.358901e-01 ,5.333633e-01 ,5.309460e-01 ,5.286277e-01 ,5.263988e-01 ,&
        & 5.242512e-01 ,5.221777e-01 ,5.201719e-01 ,5.182280e-01 ,5.163410e-01 ,&
        & 5.145062e-01 ,5.127197e-01 ,5.109776e-01 ,5.092766e-01 ,5.076137e-01 ,&
        & 5.059860e-01 ,5.043911e-01 ,5.028266e-01 ,5.012904e-01 ,4.997805e-01 ,&
        & 4.982951e-01 ,4.968326e-01 ,4.953913e-01  /)

! asymmetry factor: unitless
      asyice2(:, 16) = (/ &
! band 16
        & 7.946655e-01 ,8.547685e-01 ,8.806016e-01 ,8.949880e-01 ,9.041676e-01 ,&
        & 9.105399e-01 ,9.152249e-01 ,9.188160e-01 ,9.216573e-01 ,9.239620e-01 ,&
        & 9.258695e-01 ,9.274745e-01 ,9.288441e-01 ,9.300267e-01 ,9.310584e-01 ,&
        & 9.319665e-01 ,9.327721e-01 ,9.334918e-01 ,9.341387e-01 ,9.347236e-01 ,&
        & 9.352551e-01 ,9.357402e-01 ,9.361850e-01 ,9.365942e-01 ,9.369722e-01 ,&
        & 9.373225e-01 ,9.376481e-01 ,9.379516e-01 ,9.382352e-01 ,9.385010e-01 ,&
        & 9.387505e-01 ,9.389854e-01 ,9.392070e-01 ,9.394163e-01 ,9.396145e-01 ,&
        & 9.398024e-01 ,9.399809e-01 ,9.401508e-01 ,9.403126e-01 ,9.404670e-01 ,&
        & 9.406144e-01 ,9.407555e-01 ,9.408906e-01  /)
      asyice2(:, 17) = (/ &
! band 17
        & 9.078091e-01 ,9.195850e-01 ,9.267250e-01 ,9.317083e-01 ,9.354632e-01 ,&
        & 9.384323e-01 ,9.408597e-01 ,9.428935e-01 ,9.446301e-01 ,9.461351e-01 ,&
        & 9.474555e-01 ,9.486259e-01 ,9.496722e-01 ,9.506146e-01 ,9.514688e-01 ,&
        & 9.522476e-01 ,9.529612e-01 ,9.536181e-01 ,9.542251e-01 ,9.547883e-01 ,&
        & 9.553124e-01 ,9.558019e-01 ,9.562601e-01 ,9.566904e-01 ,9.570953e-01 ,&
        & 9.574773e-01 ,9.578385e-01 ,9.581806e-01 ,9.585054e-01 ,9.588142e-01 ,&
        & 9.591083e-01 ,9.593888e-01 ,9.596569e-01 ,9.599135e-01 ,9.601593e-01 ,&
        & 9.603952e-01 ,9.606219e-01 ,9.608399e-01 ,9.610499e-01 ,9.612523e-01 ,&
        & 9.614477e-01 ,9.616365e-01 ,9.618192e-01  /)
      asyice2(:, 18) = (/ &
! band 18
        & 8.322045e-01 ,8.528693e-01 ,8.648167e-01 ,8.729163e-01 ,8.789054e-01 ,&
        & 8.835845e-01 ,8.873819e-01 ,8.905511e-01 ,8.932532e-01 ,8.955965e-01 ,&
        & 8.976567e-01 ,8.994887e-01 ,9.011334e-01 ,9.026221e-01 ,9.039791e-01 ,&
        & 9.052237e-01 ,9.063715e-01 ,9.074349e-01 ,9.084245e-01 ,9.093489e-01 ,&
        & 9.102154e-01 ,9.110303e-01 ,9.117987e-01 ,9.125253e-01 ,9.132140e-01 ,&
        & 9.138682e-01 ,9.144910e-01 ,9.150850e-01 ,9.156524e-01 ,9.161955e-01 ,&
        & 9.167160e-01 ,9.172157e-01 ,9.176959e-01 ,9.181581e-01 ,9.186034e-01 ,&
        & 9.190330e-01 ,9.194478e-01 ,9.198488e-01 ,9.202368e-01 ,9.206126e-01 ,&
        & 9.209768e-01 ,9.213301e-01 ,9.216731e-01  /)
      asyice2(:, 19) = (/ &
! band 19
        & 8.116560e-01 ,8.488278e-01 ,8.674331e-01 ,8.788148e-01 ,8.865810e-01 ,&
        & 8.922595e-01 ,8.966149e-01 ,9.000747e-01 ,9.028980e-01 ,9.052513e-01 ,&
        & 9.072468e-01 ,9.089632e-01 ,9.104574e-01 ,9.117713e-01 ,9.129371e-01 ,&
        & 9.139793e-01 ,9.149174e-01 ,9.157668e-01 ,9.165400e-01 ,9.172473e-01 ,&
        & 9.178970e-01 ,9.184962e-01 ,9.190508e-01 ,9.195658e-01 ,9.200455e-01 ,&
        & 9.204935e-01 ,9.209130e-01 ,9.213067e-01 ,9.216771e-01 ,9.220262e-01 ,&
        & 9.223560e-01 ,9.226680e-01 ,9.229636e-01 ,9.232443e-01 ,9.235112e-01 ,&
        & 9.237652e-01 ,9.240074e-01 ,9.242385e-01 ,9.244594e-01 ,9.246708e-01 ,&
        & 9.248733e-01 ,9.250674e-01 ,9.252536e-01  /)
      asyice2(:, 20) = (/ &
! band 20
        & 8.047113e-01 ,8.402864e-01 ,8.570332e-01 ,8.668455e-01 ,8.733206e-01 ,&
        & 8.779272e-01 ,8.813796e-01 ,8.840676e-01 ,8.862225e-01 ,8.879904e-01 ,&
        & 8.894682e-01 ,8.907228e-01 ,8.918019e-01 ,8.927404e-01 ,8.935645e-01 ,&
        & 8.942943e-01 ,8.949452e-01 ,8.955296e-01 ,8.960574e-01 ,8.965366e-01 ,&
        & 8.969736e-01 ,8.973740e-01 ,8.977422e-01 ,8.980820e-01 ,8.983966e-01 ,&
        & 8.986889e-01 ,8.989611e-01 ,8.992153e-01 ,8.994533e-01 ,8.996766e-01 ,&
        & 8.998865e-01 ,9.000843e-01 ,9.002709e-01 ,9.004474e-01 ,9.006146e-01 ,&
        & 9.007731e-01 ,9.009237e-01 ,9.010670e-01 ,9.012034e-01 ,9.013336e-01 ,&
        & 9.014579e-01 ,9.015767e-01 ,9.016904e-01  /)
      asyice2(:, 21) = (/ &
! band 21
        & 8.179122e-01 ,8.480726e-01 ,8.621945e-01 ,8.704354e-01 ,8.758555e-01 ,&
        & 8.797007e-01 ,8.825750e-01 ,8.848078e-01 ,8.865939e-01 ,8.880564e-01 ,&
        & 8.892765e-01 ,8.903105e-01 ,8.911982e-01 ,8.919689e-01 ,8.926446e-01 ,&
        & 8.932419e-01 ,8.937738e-01 ,8.942506e-01 ,8.946806e-01 ,8.950702e-01 ,&
        & 8.954251e-01 ,8.957497e-01 ,8.960477e-01 ,8.963223e-01 ,8.965762e-01 ,&
        & 8.968116e-01 ,8.970306e-01 ,8.972347e-01 ,8.974255e-01 ,8.976042e-01 ,&
        & 8.977720e-01 ,8.979298e-01 ,8.980784e-01 ,8.982188e-01 ,8.983515e-01 ,&
        & 8.984771e-01 ,8.985963e-01 ,8.987095e-01 ,8.988171e-01 ,8.989195e-01 ,&
        & 8.990172e-01 ,8.991104e-01 ,8.991994e-01  /)
      asyice2(:, 22) = (/ &
! band 22
        & 8.169789e-01 ,8.455024e-01 ,8.586925e-01 ,8.663283e-01 ,8.713217e-01 ,&
        & 8.748488e-01 ,8.774765e-01 ,8.795122e-01 ,8.811370e-01 ,8.824649e-01 ,&
        & 8.835711e-01 ,8.845073e-01 ,8.853103e-01 ,8.860068e-01 ,8.866170e-01 ,&
        & 8.871560e-01 ,8.876358e-01 ,8.880658e-01 ,8.884533e-01 ,8.888044e-01 ,&
        & 8.891242e-01 ,8.894166e-01 ,8.896851e-01 ,8.899324e-01 ,8.901612e-01 ,&
        & 8.903733e-01 ,8.905706e-01 ,8.907545e-01 ,8.909265e-01 ,8.910876e-01 ,&
        & 8.912388e-01 ,8.913812e-01 ,8.915153e-01 ,8.916419e-01 ,8.917617e-01 ,&
        & 8.918752e-01 ,8.919829e-01 ,8.920851e-01 ,8.921824e-01 ,8.922751e-01 ,&
        & 8.923635e-01 ,8.924478e-01 ,8.925284e-01  /)
      asyice2(:, 23) = (/ &
! band 23
        & 8.387642e-01 ,8.569979e-01 ,8.658630e-01 ,8.711825e-01 ,8.747605e-01 ,&
        & 8.773472e-01 ,8.793129e-01 ,8.808621e-01 ,8.821179e-01 ,8.831583e-01 ,&
        & 8.840361e-01 ,8.847875e-01 ,8.854388e-01 ,8.860094e-01 ,8.865138e-01 ,&
        & 8.869634e-01 ,8.873668e-01 ,8.877310e-01 ,8.880617e-01 ,8.883635e-01 ,&
        & 8.886401e-01 ,8.888947e-01 ,8.891298e-01 ,8.893477e-01 ,8.895504e-01 ,&
        & 8.897393e-01 ,8.899159e-01 ,8.900815e-01 ,8.902370e-01 ,8.903833e-01 ,&
        & 8.905214e-01 ,8.906518e-01 ,8.907753e-01 ,8.908924e-01 ,8.910036e-01 ,&
        & 8.911094e-01 ,8.912101e-01 ,8.913062e-01 ,8.913979e-01 ,8.914856e-01 ,&
        & 8.915695e-01 ,8.916498e-01 ,8.917269e-01  /)
      asyice2(:, 24) = (/ &
! band 24
        & 8.522208e-01 ,8.648132e-01 ,8.711224e-01 ,8.749901e-01 ,8.776354e-01 ,&
        & 8.795743e-01 ,8.810649e-01 ,8.822518e-01 ,8.832225e-01 ,8.840333e-01 ,&
        & 8.847224e-01 ,8.853162e-01 ,8.858342e-01 ,8.862906e-01 ,8.866962e-01 ,&
        & 8.870595e-01 ,8.873871e-01 ,8.876842e-01 ,8.879551e-01 ,8.882032e-01 ,&
        & 8.884316e-01 ,8.886425e-01 ,8.888380e-01 ,8.890199e-01 ,8.891895e-01 ,&
        & 8.893481e-01 ,8.894968e-01 ,8.896366e-01 ,8.897683e-01 ,8.898926e-01 ,&
        & 8.900102e-01 ,8.901215e-01 ,8.902272e-01 ,8.903276e-01 ,8.904232e-01 ,&
        & 8.905144e-01 ,8.906014e-01 ,8.906845e-01 ,8.907640e-01 ,8.908402e-01 ,&
        & 8.909132e-01 ,8.909834e-01 ,8.910507e-01  /)
      asyice2(:, 25) = (/ &
! band 25
        & 8.578202e-01 ,8.683033e-01 ,8.735431e-01 ,8.767488e-01 ,8.789378e-01 ,&
        & 8.805399e-01 ,8.817701e-01 ,8.827485e-01 ,8.835480e-01 ,8.842152e-01 ,&
        & 8.847817e-01 ,8.852696e-01 ,8.856949e-01 ,8.860694e-01 ,8.864020e-01 ,&
        & 8.866997e-01 ,8.869681e-01 ,8.872113e-01 ,8.874330e-01 ,8.876360e-01 ,&
        & 8.878227e-01 ,8.879951e-01 ,8.881548e-01 ,8.883033e-01 ,8.884418e-01 ,&
        & 8.885712e-01 ,8.886926e-01 ,8.888066e-01 ,8.889139e-01 ,8.890152e-01 ,&
        & 8.891110e-01 ,8.892017e-01 ,8.892877e-01 ,8.893695e-01 ,8.894473e-01 ,&
        & 8.895214e-01 ,8.895921e-01 ,8.896597e-01 ,8.897243e-01 ,8.897862e-01 ,&
        & 8.898456e-01 ,8.899025e-01 ,8.899572e-01  /)
      asyice2(:, 26) = (/ &
! band 26
        & 8.625615e-01 ,8.713831e-01 ,8.755799e-01 ,8.780560e-01 ,8.796983e-01 ,&
        & 8.808714e-01 ,8.817534e-01 ,8.824420e-01 ,8.829953e-01 ,8.834501e-01 ,&
        & 8.838310e-01 ,8.841549e-01 ,8.844338e-01 ,8.846767e-01 ,8.848902e-01 ,&
        & 8.850795e-01 ,8.852484e-01 ,8.854002e-01 ,8.855374e-01 ,8.856620e-01 ,&
        & 8.857758e-01 ,8.858800e-01 ,8.859759e-01 ,8.860644e-01 ,8.861464e-01 ,&
        & 8.862225e-01 ,8.862935e-01 ,8.863598e-01 ,8.864218e-01 ,8.864800e-01 ,&
        & 8.865347e-01 ,8.865863e-01 ,8.866349e-01 ,8.866809e-01 ,8.867245e-01 ,&
        & 8.867658e-01 ,8.868050e-01 ,8.868423e-01 ,8.868778e-01 ,8.869117e-01 ,&
        & 8.869440e-01 ,8.869749e-01 ,8.870044e-01  /)
      asyice2(:, 27) = (/ &
! band 27
        & 8.587495e-01 ,8.684764e-01 ,8.728189e-01 ,8.752872e-01 ,8.768846e-01 ,&
        & 8.780060e-01 ,8.788386e-01 ,8.794824e-01 ,8.799960e-01 ,8.804159e-01 ,&
        & 8.807660e-01 ,8.810626e-01 ,8.813175e-01 ,8.815390e-01 ,8.817335e-01 ,&
        & 8.819057e-01 ,8.820593e-01 ,8.821973e-01 ,8.823220e-01 ,8.824353e-01 ,&
        & 8.825387e-01 ,8.826336e-01 ,8.827209e-01 ,8.828016e-01 ,8.828764e-01 ,&
        & 8.829459e-01 ,8.830108e-01 ,8.830715e-01 ,8.831283e-01 ,8.831817e-01 ,&
        & 8.832320e-01 ,8.832795e-01 ,8.833244e-01 ,8.833668e-01 ,8.834071e-01 ,&
        & 8.834454e-01 ,8.834817e-01 ,8.835164e-01 ,8.835495e-01 ,8.835811e-01 ,&
        & 8.836113e-01 ,8.836402e-01 ,8.836679e-01  /)
      asyice2(:, 28) = (/ &
! band 28
        & 8.561110e-01 ,8.678583e-01 ,8.727554e-01 ,8.753892e-01 ,8.770154e-01 ,&
        & 8.781109e-01 ,8.788949e-01 ,8.794812e-01 ,8.799348e-01 ,8.802952e-01 ,&
        & 8.805880e-01 ,8.808300e-01 ,8.810331e-01 ,8.812058e-01 ,8.813543e-01 ,&
        & 8.814832e-01 ,8.815960e-01 ,8.816956e-01 ,8.817839e-01 ,8.818629e-01 ,&
        & 8.819339e-01 ,8.819979e-01 ,8.820560e-01 ,8.821089e-01 ,8.821573e-01 ,&
        & 8.822016e-01 ,8.822425e-01 ,8.822801e-01 ,8.823150e-01 ,8.823474e-01 ,&
        & 8.823775e-01 ,8.824056e-01 ,8.824318e-01 ,8.824564e-01 ,8.824795e-01 ,&
        & 8.825011e-01 ,8.825215e-01 ,8.825408e-01 ,8.825589e-01 ,8.825761e-01 ,&
        & 8.825924e-01 ,8.826078e-01 ,8.826224e-01  /)
      asyice2(:, 29) = (/ &
! band 29
        & 8.311124e-01 ,8.688197e-01 ,8.900274e-01 ,9.040696e-01 ,9.142334e-01 ,&
        & 9.220181e-01 ,9.282195e-01 ,9.333048e-01 ,9.375689e-01 ,9.412085e-01 ,&
        & 9.443604e-01 ,9.471230e-01 ,9.495694e-01 ,9.517549e-01 ,9.537224e-01 ,&
        & 9.555057e-01 ,9.571316e-01 ,9.586222e-01 ,9.599952e-01 ,9.612656e-01 ,&
        & 9.624458e-01 ,9.635461e-01 ,9.645756e-01 ,9.655418e-01 ,9.664513e-01 ,&
        & 9.673098e-01 ,9.681222e-01 ,9.688928e-01 ,9.696256e-01 ,9.703237e-01 ,&
        & 9.709903e-01 ,9.716280e-01 ,9.722391e-01 ,9.728258e-01 ,9.733901e-01 ,&
        & 9.739336e-01 ,9.744579e-01 ,9.749645e-01 ,9.754546e-01 ,9.759294e-01 ,&
        & 9.763901e-01 ,9.768376e-01 ,9.772727e-01  /)

! Hexagonal Ice Particle Parameterization
! extinction units (ext coef/iwc): [(m^-1)/(g m^-3)]
      extice3(:, 16) = (/ &
! band 16
        & 5.194013e-01 ,3.215089e-01 ,2.327917e-01 ,1.824424e-01 ,1.499977e-01 ,&
        & 1.273492e-01 ,1.106421e-01 ,9.780982e-02 ,8.764435e-02 ,7.939266e-02 ,&
        & 7.256081e-02 ,6.681137e-02 ,6.190600e-02 ,5.767154e-02 ,5.397915e-02 ,&
        & 5.073102e-02 ,4.785151e-02 ,4.528125e-02 ,4.297296e-02 ,4.088853e-02 ,&
        & 3.899690e-02 ,3.727251e-02 ,3.569411e-02 ,3.424393e-02 ,3.290694e-02 ,&
        & 3.167040e-02 ,3.052340e-02 ,2.945654e-02 ,2.846172e-02 ,2.753188e-02 ,&
        & 2.666085e-02 ,2.584322e-02 ,2.507423e-02 ,2.434967e-02 ,2.366579e-02 ,&
        & 2.301926e-02 ,2.240711e-02 ,2.182666e-02 ,2.127551e-02 ,2.075150e-02 ,&
        & 2.025267e-02 ,1.977725e-02 ,1.932364e-02 ,1.889035e-02 ,1.847607e-02 ,&
        & 1.807956e-02  /)
      extice3(:, 17) = (/ &
! band 17
        & 4.901155e-01 ,3.065286e-01 ,2.230800e-01 ,1.753951e-01 ,1.445402e-01 ,&
        & 1.229417e-01 ,1.069777e-01 ,9.469760e-02 ,8.495824e-02 ,7.704501e-02 ,&
        & 7.048834e-02 ,6.496693e-02 ,6.025353e-02 ,5.618286e-02 ,5.263186e-02 ,&
        & 4.950698e-02 ,4.673585e-02 ,4.426164e-02 ,4.203904e-02 ,4.003153e-02 ,&
        & 3.820932e-02 ,3.654790e-02 ,3.502688e-02 ,3.362919e-02 ,3.234041e-02 ,&
        & 3.114829e-02 ,3.004234e-02 ,2.901356e-02 ,2.805413e-02 ,2.715727e-02 ,&
        & 2.631705e-02 ,2.552828e-02 ,2.478637e-02 ,2.408725e-02 ,2.342734e-02 ,&
        & 2.280343e-02 ,2.221264e-02 ,2.165242e-02 ,2.112043e-02 ,2.061461e-02 ,&
        & 2.013308e-02 ,1.967411e-02 ,1.923616e-02 ,1.881783e-02 ,1.841781e-02 ,&
        & 1.803494e-02  /)
      extice3(:, 18) = (/ &
! band 18
        & 5.056264e-01 ,3.160261e-01 ,2.298442e-01 ,1.805973e-01 ,1.487318e-01 ,&
        & 1.264258e-01 ,1.099389e-01 ,9.725656e-02 ,8.719819e-02 ,7.902576e-02 ,&
        & 7.225433e-02 ,6.655206e-02 ,6.168427e-02 ,5.748028e-02 ,5.381296e-02 ,&
        & 5.058572e-02 ,4.772383e-02 ,4.516857e-02 ,4.287317e-02 ,4.079990e-02 ,&
        & 3.891801e-02 ,3.720217e-02 ,3.563133e-02 ,3.418786e-02 ,3.285686e-02 ,&
        & 3.162569e-02 ,3.048352e-02 ,2.942104e-02 ,2.843018e-02 ,2.750395e-02 ,&
        & 2.663621e-02 ,2.582160e-02 ,2.505539e-02 ,2.433337e-02 ,2.365185e-02 ,&
        & 2.300750e-02 ,2.239736e-02 ,2.181878e-02 ,2.126937e-02 ,2.074699e-02 ,&
        & 2.024968e-02 ,1.977567e-02 ,1.932338e-02 ,1.889134e-02 ,1.847823e-02 ,&
        & 1.808281e-02  /)
      extice3(:, 19) = (/ &
! band 19
        & 4.881605e-01 ,3.055237e-01 ,2.225070e-01 ,1.750688e-01 ,1.443736e-01 ,&
        & 1.228869e-01 ,1.070054e-01 ,9.478893e-02 ,8.509997e-02 ,7.722769e-02 ,&
        & 7.070495e-02 ,6.521211e-02 ,6.052311e-02 ,5.647351e-02 ,5.294088e-02 ,&
        & 4.983217e-02 ,4.707539e-02 ,4.461398e-02 ,4.240288e-02 ,4.040575e-02 ,&
        & 3.859298e-02 ,3.694016e-02 ,3.542701e-02 ,3.403655e-02 ,3.275444e-02 ,&
        & 3.156849e-02 ,3.046827e-02 ,2.944481e-02 ,2.849034e-02 ,2.759812e-02 ,&
        & 2.676226e-02 ,2.597757e-02 ,2.523949e-02 ,2.454400e-02 ,2.388750e-02 ,&
        & 2.326682e-02 ,2.267909e-02 ,2.212176e-02 ,2.159253e-02 ,2.108933e-02 ,&
        & 2.061028e-02 ,2.015369e-02 ,1.971801e-02 ,1.930184e-02 ,1.890389e-02 ,&
        & 1.852300e-02  /)
      extice3(:, 20) = (/ &
! band 20
        & 5.103703e-01 ,3.188144e-01 ,2.317435e-01 ,1.819887e-01 ,1.497944e-01 ,&
        & 1.272584e-01 ,1.106013e-01 ,9.778822e-02 ,8.762610e-02 ,7.936938e-02 ,&
        & 7.252809e-02 ,6.676701e-02 ,6.184901e-02 ,5.760165e-02 ,5.389651e-02 ,&
        & 5.063598e-02 ,4.774457e-02 ,4.516295e-02 ,4.284387e-02 ,4.074922e-02 ,&
        & 3.884792e-02 ,3.711438e-02 ,3.552734e-02 ,3.406898e-02 ,3.272425e-02 ,&
        & 3.148038e-02 ,3.032643e-02 ,2.925299e-02 ,2.825191e-02 ,2.731612e-02 ,&
        & 2.643943e-02 ,2.561642e-02 ,2.484230e-02 ,2.411284e-02 ,2.342429e-02 ,&
        & 2.277329e-02 ,2.215686e-02 ,2.157231e-02 ,2.101724e-02 ,2.048946e-02 ,&
        & 1.998702e-02 ,1.950813e-02 ,1.905118e-02 ,1.861468e-02 ,1.819730e-02 ,&
        & 1.779781e-02  /)
      extice3(:, 21) = (/ &
! band 21
        & 5.031161e-01 ,3.144511e-01 ,2.286942e-01 ,1.796903e-01 ,1.479819e-01 ,&
        & 1.257860e-01 ,1.093803e-01 ,9.676059e-02 ,8.675183e-02 ,7.861971e-02 ,&
        & 7.188168e-02 ,6.620754e-02 ,6.136376e-02 ,5.718050e-02 ,5.353127e-02 ,&
        & 5.031995e-02 ,4.747218e-02 ,4.492952e-02 ,4.264544e-02 ,4.058240e-02 ,&
        & 3.870979e-02 ,3.700242e-02 ,3.543933e-02 ,3.400297e-02 ,3.267854e-02 ,&
        & 3.145345e-02 ,3.031691e-02 ,2.925967e-02 ,2.827370e-02 ,2.735203e-02 ,&
        & 2.648858e-02 ,2.567798e-02 ,2.491555e-02 ,2.419710e-02 ,2.351893e-02 ,&
        & 2.287776e-02 ,2.227063e-02 ,2.169491e-02 ,2.114821e-02 ,2.062840e-02 ,&
        & 2.013354e-02 ,1.966188e-02 ,1.921182e-02 ,1.878191e-02 ,1.837083e-02 ,&
        & 1.797737e-02  /)
      extice3(:, 22) = (/ &
! band 22
        & 4.949453e-01 ,3.095918e-01 ,2.253402e-01 ,1.771964e-01 ,1.460446e-01 ,&
        & 1.242383e-01 ,1.081206e-01 ,9.572235e-02 ,8.588928e-02 ,7.789990e-02 ,&
        & 7.128013e-02 ,6.570559e-02 ,6.094684e-02 ,5.683701e-02 ,5.325183e-02 ,&
        & 5.009688e-02 ,4.729909e-02 ,4.480106e-02 ,4.255708e-02 ,4.053025e-02 ,&
        & 3.869051e-02 ,3.701310e-02 ,3.547745e-02 ,3.406631e-02 ,3.276512e-02 ,&
        & 3.156153e-02 ,3.044494e-02 ,2.940626e-02 ,2.843759e-02 ,2.753211e-02 ,&
        & 2.668381e-02 ,2.588744e-02 ,2.513839e-02 ,2.443255e-02 ,2.376629e-02 ,&
        & 2.313637e-02 ,2.253990e-02 ,2.197428e-02 ,2.143718e-02 ,2.092649e-02 ,&
        & 2.044032e-02 ,1.997694e-02 ,1.953478e-02 ,1.911241e-02 ,1.870855e-02 ,&
        & 1.832199e-02  /)
      extice3(:, 23) = (/ &
! band 23
        & 5.052816e-01 ,3.157665e-01 ,2.296233e-01 ,1.803986e-01 ,1.485473e-01 ,&
        & 1.262514e-01 ,1.097718e-01 ,9.709524e-02 ,8.704139e-02 ,7.887264e-02 ,&
        & 7.210424e-02 ,6.640454e-02 ,6.153894e-02 ,5.733683e-02 ,5.367116e-02 ,&
        & 5.044537e-02 ,4.758477e-02 ,4.503066e-02 ,4.273629e-02 ,4.066395e-02 ,&
        & 3.878291e-02 ,3.706784e-02 ,3.549771e-02 ,3.405488e-02 ,3.272448e-02 ,&
        & 3.149387e-02 ,3.035221e-02 ,2.929020e-02 ,2.829979e-02 ,2.737397e-02 ,&
        & 2.650663e-02 ,2.569238e-02 ,2.492651e-02 ,2.420482e-02 ,2.352361e-02 ,&
        & 2.287954e-02 ,2.226968e-02 ,2.169136e-02 ,2.114220e-02 ,2.062005e-02 ,&
        & 2.012296e-02 ,1.964917e-02 ,1.919709e-02 ,1.876524e-02 ,1.835231e-02 ,&
        & 1.795707e-02  /)
      extice3(:, 24) = (/ &
! band 24
        & 5.042067e-01 ,3.151195e-01 ,2.291708e-01 ,1.800573e-01 ,1.482779e-01 ,&
        & 1.260324e-01 ,1.095900e-01 ,9.694202e-02 ,8.691087e-02 ,7.876056e-02 ,&
        & 7.200745e-02 ,6.632062e-02 ,6.146600e-02 ,5.727338e-02 ,5.361599e-02 ,&
        & 5.039749e-02 ,4.754334e-02 ,4.499500e-02 ,4.270580e-02 ,4.063815e-02 ,&
        & 3.876135e-02 ,3.705016e-02 ,3.548357e-02 ,3.404400e-02 ,3.271661e-02 ,&
        & 3.148877e-02 ,3.034969e-02 ,2.929008e-02 ,2.830191e-02 ,2.737818e-02 ,&
        & 2.651279e-02 ,2.570039e-02 ,2.493624e-02 ,2.421618e-02 ,2.353650e-02 ,&
        & 2.289390e-02 ,2.228541e-02 ,2.170840e-02 ,2.116048e-02 ,2.063950e-02 ,&
        & 2.014354e-02 ,1.967082e-02 ,1.921975e-02 ,1.878888e-02 ,1.837688e-02 ,&
        & 1.798254e-02  /)
      extice3(:, 25) = (/ &
! band 25
        & 5.022507e-01 ,3.139246e-01 ,2.283218e-01 ,1.794059e-01 ,1.477544e-01 ,&
        & 1.255984e-01 ,1.092222e-01 ,9.662516e-02 ,8.663439e-02 ,7.851688e-02 ,&
        & 7.179095e-02 ,6.612700e-02 ,6.129193e-02 ,5.711618e-02 ,5.347351e-02 ,&
        & 5.026796e-02 ,4.742530e-02 ,4.488721e-02 ,4.260724e-02 ,4.054790e-02 ,&
        & 3.867866e-02 ,3.697435e-02 ,3.541407e-02 ,3.398029e-02 ,3.265824e-02 ,&
        & 3.143535e-02 ,3.030085e-02 ,2.924551e-02 ,2.826131e-02 ,2.734130e-02 ,&
        & 2.647939e-02 ,2.567026e-02 ,2.490919e-02 ,2.419203e-02 ,2.351509e-02 ,&
        & 2.287507e-02 ,2.226903e-02 ,2.169434e-02 ,2.114862e-02 ,2.062975e-02 ,&
        & 2.013578e-02 ,1.966496e-02 ,1.921571e-02 ,1.878658e-02 ,1.837623e-02 ,&
        & 1.798348e-02  /)
      extice3(:, 26) = (/ &
! band 26
        & 5.068316e-01 ,3.166869e-01 ,2.302576e-01 ,1.808693e-01 ,1.489122e-01 ,&
        & 1.265423e-01 ,1.100080e-01 ,9.728926e-02 ,8.720201e-02 ,7.900612e-02 ,&
        & 7.221524e-02 ,6.649660e-02 ,6.161484e-02 ,5.739877e-02 ,5.372093e-02 ,&
        & 5.048442e-02 ,4.761431e-02 ,4.505172e-02 ,4.274972e-02 ,4.067050e-02 ,&
        & 3.878321e-02 ,3.706244e-02 ,3.548710e-02 ,3.403948e-02 ,3.270466e-02 ,&
        & 3.146995e-02 ,3.032450e-02 ,2.925897e-02 ,2.826527e-02 ,2.733638e-02 ,&
        & 2.646615e-02 ,2.564920e-02 ,2.488078e-02 ,2.415670e-02 ,2.347322e-02 ,&
        & 2.282702e-02 ,2.221513e-02 ,2.163489e-02 ,2.108390e-02 ,2.056002e-02 ,&
        & 2.006128e-02 ,1.958591e-02 ,1.913232e-02 ,1.869904e-02 ,1.828474e-02 ,&
        & 1.788819e-02  /)
      extice3(:, 27) = (/ &
! band 27
        & 5.077707e-01 ,3.172636e-01 ,2.306695e-01 ,1.811871e-01 ,1.491691e-01 ,&
        & 1.267565e-01 ,1.101907e-01 ,9.744773e-02 ,8.734125e-02 ,7.912973e-02 ,&
        & 7.232591e-02 ,6.659637e-02 ,6.170530e-02 ,5.748120e-02 ,5.379634e-02 ,&
        & 5.055367e-02 ,4.767809e-02 ,4.511061e-02 ,4.280423e-02 ,4.072104e-02 ,&
        & 3.883015e-02 ,3.710611e-02 ,3.552776e-02 ,3.407738e-02 ,3.274002e-02 ,&
        & 3.150296e-02 ,3.035532e-02 ,2.928776e-02 ,2.829216e-02 ,2.736150e-02 ,&
        & 2.648961e-02 ,2.567111e-02 ,2.490123e-02 ,2.417576e-02 ,2.349098e-02 ,&
        & 2.284354e-02 ,2.223049e-02 ,2.164914e-02 ,2.109711e-02 ,2.057222e-02 ,&
        & 2.007253e-02 ,1.959626e-02 ,1.914181e-02 ,1.870770e-02 ,1.829261e-02 ,&
        & 1.789531e-02  /)
      extice3(:, 28) = (/ &
! band 28
        & 5.062281e-01 ,3.163402e-01 ,2.300275e-01 ,1.807060e-01 ,1.487921e-01 ,&
        & 1.264523e-01 ,1.099403e-01 ,9.723879e-02 ,8.716516e-02 ,7.898034e-02 ,&
        & 7.219863e-02 ,6.648771e-02 ,6.161254e-02 ,5.740217e-02 ,5.372929e-02 ,&
        & 5.049716e-02 ,4.763092e-02 ,4.507179e-02 ,4.277290e-02 ,4.069649e-02 ,&
        & 3.881175e-02 ,3.709331e-02 ,3.552008e-02 ,3.407442e-02 ,3.274141e-02 ,&
        & 3.150837e-02 ,3.036447e-02 ,2.930037e-02 ,2.830801e-02 ,2.738037e-02 ,&
        & 2.651132e-02 ,2.569547e-02 ,2.492810e-02 ,2.420499e-02 ,2.352243e-02 ,&
        & 2.287710e-02 ,2.226604e-02 ,2.168658e-02 ,2.113634e-02 ,2.061316e-02 ,&
        & 2.011510e-02 ,1.964038e-02 ,1.918740e-02 ,1.875471e-02 ,1.834096e-02 ,&
        & 1.794495e-02  /)
      extice3(:, 29) = (/ &
! band 29
        & 1.338834e-01 ,1.924912e-01 ,1.755523e-01 ,1.534793e-01 ,1.343937e-01 ,&
        & 1.187883e-01 ,1.060654e-01 ,9.559106e-02 ,8.685880e-02 ,7.948698e-02 ,&
        & 7.319086e-02 ,6.775669e-02 ,6.302215e-02 ,5.886236e-02 ,5.517996e-02 ,&
        & 5.189810e-02 ,4.895539e-02 ,4.630225e-02 ,4.389823e-02 ,4.171002e-02 ,&
        & 3.970998e-02 ,3.787493e-02 ,3.618537e-02 ,3.462471e-02 ,3.317880e-02 ,&
        & 3.183547e-02 ,3.058421e-02 ,2.941590e-02 ,2.832256e-02 ,2.729724e-02 ,&
        & 2.633377e-02 ,2.542675e-02 ,2.457136e-02 ,2.376332e-02 ,2.299882e-02 ,&
        & 2.227443e-02 ,2.158707e-02 ,2.093400e-02 ,2.031270e-02 ,1.972091e-02 ,&
        & 1.915659e-02 ,1.861787e-02 ,1.810304e-02 ,1.761055e-02 ,1.713899e-02 ,&
        & 1.668704e-02  /)

! single-scattering albedo: unitless
      ssaice3(:, 16) = (/ &
! band 16
        & 6.749442e-01 ,6.649947e-01 ,6.565828e-01 ,6.489928e-01 ,6.420046e-01 ,&
        & 6.355231e-01 ,6.294964e-01 ,6.238901e-01 ,6.186783e-01 ,6.138395e-01 ,&
        & 6.093543e-01 ,6.052049e-01 ,6.013742e-01 ,5.978457e-01 ,5.946030e-01 ,&
        & 5.916302e-01 ,5.889115e-01 ,5.864310e-01 ,5.841731e-01 ,5.821221e-01 ,&
        & 5.802624e-01 ,5.785785e-01 ,5.770549e-01 ,5.756759e-01 ,5.744262e-01 ,&
        & 5.732901e-01 ,5.722524e-01 ,5.712974e-01 ,5.704097e-01 ,5.695739e-01 ,&
        & 5.687747e-01 ,5.679964e-01 ,5.672238e-01 ,5.664415e-01 ,5.656340e-01 ,&
        & 5.647860e-01 ,5.638821e-01 ,5.629070e-01 ,5.618452e-01 ,5.606815e-01 ,&
        & 5.594006e-01 ,5.579870e-01 ,5.564255e-01 ,5.547008e-01 ,5.527976e-01 ,&
        & 5.507005e-01  /)
      ssaice3(:, 17) = (/ &
! band 17
        & 7.628550e-01 ,7.567297e-01 ,7.508463e-01 ,7.451972e-01 ,7.397745e-01 ,&
        & 7.345705e-01 ,7.295775e-01 ,7.247881e-01 ,7.201945e-01 ,7.157894e-01 ,&
        & 7.115652e-01 ,7.075145e-01 ,7.036300e-01 ,6.999044e-01 ,6.963304e-01 ,&
        & 6.929007e-01 ,6.896083e-01 ,6.864460e-01 ,6.834067e-01 ,6.804833e-01 ,&
        & 6.776690e-01 ,6.749567e-01 ,6.723397e-01 ,6.698109e-01 ,6.673637e-01 ,&
        & 6.649913e-01 ,6.626870e-01 ,6.604441e-01 ,6.582561e-01 ,6.561163e-01 ,&
        & 6.540182e-01 ,6.519554e-01 ,6.499215e-01 ,6.479099e-01 ,6.459145e-01 ,&
        & 6.439289e-01 ,6.419468e-01 ,6.399621e-01 ,6.379686e-01 ,6.359601e-01 ,&
        & 6.339306e-01 ,6.318740e-01 ,6.297845e-01 ,6.276559e-01 ,6.254825e-01 ,&
        & 6.232583e-01  /)
      ssaice3(:, 18) = (/ &
! band 18
        & 9.924147e-01 ,9.882792e-01 ,9.842257e-01 ,9.802522e-01 ,9.763566e-01 ,&
        & 9.725367e-01 ,9.687905e-01 ,9.651157e-01 ,9.615104e-01 ,9.579725e-01 ,&
        & 9.544997e-01 ,9.510901e-01 ,9.477416e-01 ,9.444520e-01 ,9.412194e-01 ,&
        & 9.380415e-01 ,9.349165e-01 ,9.318421e-01 ,9.288164e-01 ,9.258373e-01 ,&
        & 9.229027e-01 ,9.200106e-01 ,9.171589e-01 ,9.143457e-01 ,9.115688e-01 ,&
        & 9.088263e-01 ,9.061161e-01 ,9.034362e-01 ,9.007846e-01 ,8.981592e-01 ,&
        & 8.955581e-01 ,8.929792e-01 ,8.904206e-01 ,8.878803e-01 ,8.853562e-01 ,&
        & 8.828464e-01 ,8.803488e-01 ,8.778616e-01 ,8.753827e-01 ,8.729102e-01 ,&
        & 8.704421e-01 ,8.679764e-01 ,8.655112e-01 ,8.630445e-01 ,8.605744e-01 ,&
        & 8.580989e-01  /)
      ssaice3(:, 19) = (/ &
! band 19
        & 9.629413e-01 ,9.517182e-01 ,9.409209e-01 ,9.305366e-01 ,9.205529e-01 ,&
        & 9.109569e-01 ,9.017362e-01 ,8.928780e-01 ,8.843699e-01 ,8.761992e-01 ,&
        & 8.683536e-01 ,8.608204e-01 ,8.535873e-01 ,8.466417e-01 ,8.399712e-01 ,&
        & 8.335635e-01 ,8.274062e-01 ,8.214868e-01 ,8.157932e-01 ,8.103129e-01 ,&
        & 8.050336e-01 ,7.999432e-01 ,7.950294e-01 ,7.902798e-01 ,7.856825e-01 ,&
        & 7.812250e-01 ,7.768954e-01 ,7.726815e-01 ,7.685711e-01 ,7.645522e-01 ,&
        & 7.606126e-01 ,7.567404e-01 ,7.529234e-01 ,7.491498e-01 ,7.454074e-01 ,&
        & 7.416844e-01 ,7.379688e-01 ,7.342485e-01 ,7.305118e-01 ,7.267468e-01 ,&
        & 7.229415e-01 ,7.190841e-01 ,7.151628e-01 ,7.111657e-01 ,7.070811e-01 ,&
        & 7.028972e-01  /)
      ssaice3(:, 20) = (/ &
! band 20
        & 9.942270e-01 ,9.909206e-01 ,9.876775e-01 ,9.844960e-01 ,9.813746e-01 ,&
        & 9.783114e-01 ,9.753049e-01 ,9.723535e-01 ,9.694553e-01 ,9.666088e-01 ,&
        & 9.638123e-01 ,9.610641e-01 ,9.583626e-01 ,9.557060e-01 ,9.530928e-01 ,&
        & 9.505211e-01 ,9.479895e-01 ,9.454961e-01 ,9.430393e-01 ,9.406174e-01 ,&
        & 9.382288e-01 ,9.358717e-01 ,9.335446e-01 ,9.312456e-01 ,9.289731e-01 ,&
        & 9.267255e-01 ,9.245010e-01 ,9.222980e-01 ,9.201147e-01 ,9.179496e-01 ,&
        & 9.158008e-01 ,9.136667e-01 ,9.115457e-01 ,9.094359e-01 ,9.073358e-01 ,&
        & 9.052436e-01 ,9.031577e-01 ,9.010763e-01 ,8.989977e-01 ,8.969203e-01 ,&
        & 8.948423e-01 ,8.927620e-01 ,8.906778e-01 ,8.885879e-01 ,8.864907e-01 ,&
        & 8.843843e-01  /)
      ssaice3(:, 21) = (/ &
! band 21
        & 9.934014e-01 ,9.899331e-01 ,9.865537e-01 ,9.832610e-01 ,9.800523e-01 ,&
        & 9.769254e-01 ,9.738777e-01 ,9.709069e-01 ,9.680106e-01 ,9.651862e-01 ,&
        & 9.624315e-01 ,9.597439e-01 ,9.571212e-01 ,9.545608e-01 ,9.520605e-01 ,&
        & 9.496177e-01 ,9.472301e-01 ,9.448954e-01 ,9.426111e-01 ,9.403749e-01 ,&
        & 9.381843e-01 ,9.360370e-01 ,9.339307e-01 ,9.318629e-01 ,9.298313e-01 ,&
        & 9.278336e-01 ,9.258673e-01 ,9.239302e-01 ,9.220198e-01 ,9.201338e-01 ,&
        & 9.182700e-01 ,9.164258e-01 ,9.145991e-01 ,9.127874e-01 ,9.109884e-01 ,&
        & 9.091999e-01 ,9.074194e-01 ,9.056447e-01 ,9.038735e-01 ,9.021033e-01 ,&
        & 9.003320e-01 ,8.985572e-01 ,8.967766e-01 ,8.949879e-01 ,8.931888e-01 ,&
        & 8.913770e-01  /)
      ssaice3(:, 22) = (/ &
! band 22
        & 9.994833e-01 ,9.992055e-01 ,9.989278e-01 ,9.986500e-01 ,9.983724e-01 ,&
        & 9.980947e-01 ,9.978172e-01 ,9.975397e-01 ,9.972623e-01 ,9.969849e-01 ,&
        & 9.967077e-01 ,9.964305e-01 ,9.961535e-01 ,9.958765e-01 ,9.955997e-01 ,&
        & 9.953230e-01 ,9.950464e-01 ,9.947699e-01 ,9.944936e-01 ,9.942174e-01 ,&
        & 9.939414e-01 ,9.936656e-01 ,9.933899e-01 ,9.931144e-01 ,9.928390e-01 ,&
        & 9.925639e-01 ,9.922889e-01 ,9.920141e-01 ,9.917396e-01 ,9.914652e-01 ,&
        & 9.911911e-01 ,9.909171e-01 ,9.906434e-01 ,9.903700e-01 ,9.900967e-01 ,&
        & 9.898237e-01 ,9.895510e-01 ,9.892784e-01 ,9.890062e-01 ,9.887342e-01 ,&
        & 9.884625e-01 ,9.881911e-01 ,9.879199e-01 ,9.876490e-01 ,9.873784e-01 ,&
        & 9.871081e-01  /)
      ssaice3(:, 23) = (/ &
! band 23
        & 9.999343e-01 ,9.998917e-01 ,9.998492e-01 ,9.998067e-01 ,9.997642e-01 ,&
        & 9.997218e-01 ,9.996795e-01 ,9.996372e-01 ,9.995949e-01 ,9.995528e-01 ,&
        & 9.995106e-01 ,9.994686e-01 ,9.994265e-01 ,9.993845e-01 ,9.993426e-01 ,&
        & 9.993007e-01 ,9.992589e-01 ,9.992171e-01 ,9.991754e-01 ,9.991337e-01 ,&
        & 9.990921e-01 ,9.990505e-01 ,9.990089e-01 ,9.989674e-01 ,9.989260e-01 ,&
        & 9.988846e-01 ,9.988432e-01 ,9.988019e-01 ,9.987606e-01 ,9.987194e-01 ,&
        & 9.986782e-01 ,9.986370e-01 ,9.985959e-01 ,9.985549e-01 ,9.985139e-01 ,&
        & 9.984729e-01 ,9.984319e-01 ,9.983910e-01 ,9.983502e-01 ,9.983094e-01 ,&
        & 9.982686e-01 ,9.982279e-01 ,9.981872e-01 ,9.981465e-01 ,9.981059e-01 ,&
        & 9.980653e-01  /)
      ssaice3(:, 24) = (/ &
! band 24
        & 9.999978e-01 ,9.999965e-01 ,9.999952e-01 ,9.999939e-01 ,9.999926e-01 ,&
        & 9.999913e-01 ,9.999900e-01 ,9.999887e-01 ,9.999873e-01 ,9.999860e-01 ,&
        & 9.999847e-01 ,9.999834e-01 ,9.999821e-01 ,9.999808e-01 ,9.999795e-01 ,&
        & 9.999782e-01 ,9.999769e-01 ,9.999756e-01 ,9.999743e-01 ,9.999730e-01 ,&
        & 9.999717e-01 ,9.999704e-01 ,9.999691e-01 ,9.999678e-01 ,9.999665e-01 ,&
        & 9.999652e-01 ,9.999639e-01 ,9.999626e-01 ,9.999613e-01 ,9.999600e-01 ,&
        & 9.999587e-01 ,9.999574e-01 ,9.999561e-01 ,9.999548e-01 ,9.999535e-01 ,&
        & 9.999522e-01 ,9.999509e-01 ,9.999496e-01 ,9.999483e-01 ,9.999470e-01 ,&
        & 9.999457e-01 ,9.999444e-01 ,9.999431e-01 ,9.999418e-01 ,9.999405e-01 ,&
        & 9.999392e-01  /)
      ssaice3(:, 25) = (/ &
! band 25
        & 9.999994e-01 ,9.999993e-01 ,9.999991e-01 ,9.999990e-01 ,9.999989e-01 ,&
        & 9.999987e-01 ,9.999986e-01 ,9.999984e-01 ,9.999983e-01 ,9.999982e-01 ,&
        & 9.999980e-01 ,9.999979e-01 ,9.999977e-01 ,9.999976e-01 ,9.999975e-01 ,&
        & 9.999973e-01 ,9.999972e-01 ,9.999970e-01 ,9.999969e-01 ,9.999967e-01 ,&
        & 9.999966e-01 ,9.999965e-01 ,9.999963e-01 ,9.999962e-01 ,9.999960e-01 ,&
        & 9.999959e-01 ,9.999957e-01 ,9.999956e-01 ,9.999954e-01 ,9.999953e-01 ,&
        & 9.999952e-01 ,9.999950e-01 ,9.999949e-01 ,9.999947e-01 ,9.999946e-01 ,&
        & 9.999944e-01 ,9.999943e-01 ,9.999941e-01 ,9.999940e-01 ,9.999939e-01 ,&
        & 9.999937e-01 ,9.999936e-01 ,9.999934e-01 ,9.999933e-01 ,9.999931e-01 ,&
        & 9.999930e-01  /)
      ssaice3(:, 26) = (/ &
! band 26
        & 9.999997e-01 ,9.999995e-01 ,9.999992e-01 ,9.999990e-01 ,9.999987e-01 ,&
        & 9.999985e-01 ,9.999983e-01 ,9.999980e-01 ,9.999978e-01 ,9.999976e-01 ,&
        & 9.999973e-01 ,9.999971e-01 ,9.999969e-01 ,9.999967e-01 ,9.999965e-01 ,&
        & 9.999963e-01 ,9.999960e-01 ,9.999958e-01 ,9.999956e-01 ,9.999954e-01 ,&
        & 9.999952e-01 ,9.999950e-01 ,9.999948e-01 ,9.999946e-01 ,9.999944e-01 ,&
        & 9.999942e-01 ,9.999939e-01 ,9.999937e-01 ,9.999935e-01 ,9.999933e-01 ,&
        & 9.999931e-01 ,9.999929e-01 ,9.999927e-01 ,9.999925e-01 ,9.999923e-01 ,&
        & 9.999920e-01 ,9.999918e-01 ,9.999916e-01 ,9.999914e-01 ,9.999911e-01 ,&
        & 9.999909e-01 ,9.999907e-01 ,9.999905e-01 ,9.999902e-01 ,9.999900e-01 ,&
        & 9.999897e-01  /)
      ssaice3(:, 27) = (/ &
! band 27
        & 9.999991e-01 ,9.999985e-01 ,9.999980e-01 ,9.999974e-01 ,9.999968e-01 ,&
        & 9.999963e-01 ,9.999957e-01 ,9.999951e-01 ,9.999946e-01 ,9.999940e-01 ,&
        & 9.999934e-01 ,9.999929e-01 ,9.999923e-01 ,9.999918e-01 ,9.999912e-01 ,&
        & 9.999907e-01 ,9.999901e-01 ,9.999896e-01 ,9.999891e-01 ,9.999885e-01 ,&
        & 9.999880e-01 ,9.999874e-01 ,9.999869e-01 ,9.999863e-01 ,9.999858e-01 ,&
        & 9.999853e-01 ,9.999847e-01 ,9.999842e-01 ,9.999836e-01 ,9.999831e-01 ,&
        & 9.999826e-01 ,9.999820e-01 ,9.999815e-01 ,9.999809e-01 ,9.999804e-01 ,&
        & 9.999798e-01 ,9.999793e-01 ,9.999787e-01 ,9.999782e-01 ,9.999776e-01 ,&
        & 9.999770e-01 ,9.999765e-01 ,9.999759e-01 ,9.999754e-01 ,9.999748e-01 ,&
        & 9.999742e-01  /)
      ssaice3(:, 28) = (/ &
! band 28
        & 9.999975e-01 ,9.999961e-01 ,9.999946e-01 ,9.999931e-01 ,9.999917e-01 ,&
        & 9.999903e-01 ,9.999888e-01 ,9.999874e-01 ,9.999859e-01 ,9.999845e-01 ,&
        & 9.999831e-01 ,9.999816e-01 ,9.999802e-01 ,9.999788e-01 ,9.999774e-01 ,&
        & 9.999759e-01 ,9.999745e-01 ,9.999731e-01 ,9.999717e-01 ,9.999702e-01 ,&
        & 9.999688e-01 ,9.999674e-01 ,9.999660e-01 ,9.999646e-01 ,9.999631e-01 ,&
        & 9.999617e-01 ,9.999603e-01 ,9.999589e-01 ,9.999574e-01 ,9.999560e-01 ,&
        & 9.999546e-01 ,9.999532e-01 ,9.999517e-01 ,9.999503e-01 ,9.999489e-01 ,&
        & 9.999474e-01 ,9.999460e-01 ,9.999446e-01 ,9.999431e-01 ,9.999417e-01 ,&
        & 9.999403e-01 ,9.999388e-01 ,9.999374e-01 ,9.999359e-01 ,9.999345e-01 ,&
        & 9.999330e-01  /)
      ssaice3(:, 29) = (/ &
! band 29
        & 4.526500e-01 ,5.287890e-01 ,5.410487e-01 ,5.459865e-01 ,5.485149e-01 ,&
        & 5.498914e-01 ,5.505895e-01 ,5.508310e-01 ,5.507364e-01 ,5.503793e-01 ,&
        & 5.498090e-01 ,5.490612e-01 ,5.481637e-01 ,5.471395e-01 ,5.460083e-01 ,&
        & 5.447878e-01 ,5.434946e-01 ,5.421442e-01 ,5.407514e-01 ,5.393309e-01 ,&
        & 5.378970e-01 ,5.364641e-01 ,5.350464e-01 ,5.336582e-01 ,5.323140e-01 ,&
        & 5.310283e-01 ,5.298158e-01 ,5.286914e-01 ,5.276704e-01 ,5.267680e-01 ,&
        & 5.260000e-01 ,5.253823e-01 ,5.249311e-01 ,5.246629e-01 ,5.245946e-01 ,&
        & 5.247434e-01 ,5.251268e-01 ,5.257626e-01 ,5.266693e-01 ,5.278653e-01 ,&
        & 5.293698e-01 ,5.312022e-01 ,5.333823e-01 ,5.359305e-01 ,5.388676e-01 ,&
        & 5.422146e-01  /)

! asymmetry factor: unitless
      asyice3(:, 16) = (/ &
! band 16
        & 8.340752e-01 ,8.435170e-01 ,8.517487e-01 ,8.592064e-01 ,8.660387e-01 ,&
        & 8.723204e-01 ,8.780997e-01 ,8.834137e-01 ,8.882934e-01 ,8.927662e-01 ,&
        & 8.968577e-01 ,9.005914e-01 ,9.039899e-01 ,9.070745e-01 ,9.098659e-01 ,&
        & 9.123836e-01 ,9.146466e-01 ,9.166734e-01 ,9.184817e-01 ,9.200886e-01 ,&
        & 9.215109e-01 ,9.227648e-01 ,9.238661e-01 ,9.248304e-01 ,9.256727e-01 ,&
        & 9.264078e-01 ,9.270505e-01 ,9.276150e-01 ,9.281156e-01 ,9.285662e-01 ,&
        & 9.289806e-01 ,9.293726e-01 ,9.297557e-01 ,9.301435e-01 ,9.305491e-01 ,&
        & 9.309859e-01 ,9.314671e-01 ,9.320055e-01 ,9.326140e-01 ,9.333053e-01 ,&
        & 9.340919e-01 ,9.349861e-01 ,9.360000e-01 ,9.371451e-01 ,9.384329e-01 ,&
        & 9.398744e-01  /)
      asyice3(:, 17) = (/ &
! band 17
        & 8.728160e-01 ,8.777333e-01 ,8.823754e-01 ,8.867535e-01 ,8.908785e-01 ,&
        & 8.947611e-01 ,8.984118e-01 ,9.018408e-01 ,9.050582e-01 ,9.080739e-01 ,&
        & 9.108976e-01 ,9.135388e-01 ,9.160068e-01 ,9.183106e-01 ,9.204595e-01 ,&
        & 9.224620e-01 ,9.243271e-01 ,9.260632e-01 ,9.276788e-01 ,9.291822e-01 ,&
        & 9.305817e-01 ,9.318853e-01 ,9.331012e-01 ,9.342372e-01 ,9.353013e-01 ,&
        & 9.363013e-01 ,9.372450e-01 ,9.381400e-01 ,9.389939e-01 ,9.398145e-01 ,&
        & 9.406092e-01 ,9.413856e-01 ,9.421511e-01 ,9.429131e-01 ,9.436790e-01 ,&
        & 9.444561e-01 ,9.452517e-01 ,9.460729e-01 ,9.469270e-01 ,9.478209e-01 ,&
        & 9.487617e-01 ,9.497562e-01 ,9.508112e-01 ,9.519335e-01 ,9.531294e-01 ,&
        & 9.544055e-01  /)
      asyice3(:, 18) = (/ &
! band 18
        & 7.897566e-01 ,7.948704e-01 ,7.998041e-01 ,8.045623e-01 ,8.091495e-01 ,&
        & 8.135702e-01 ,8.178290e-01 ,8.219305e-01 ,8.258790e-01 ,8.296792e-01 ,&
        & 8.333355e-01 ,8.368524e-01 ,8.402343e-01 ,8.434856e-01 ,8.466108e-01 ,&
        & 8.496143e-01 ,8.525004e-01 ,8.552737e-01 ,8.579384e-01 ,8.604990e-01 ,&
        & 8.629597e-01 ,8.653250e-01 ,8.675992e-01 ,8.697867e-01 ,8.718916e-01 ,&
        & 8.739185e-01 ,8.758715e-01 ,8.777551e-01 ,8.795734e-01 ,8.813308e-01 ,&
        & 8.830315e-01 ,8.846799e-01 ,8.862802e-01 ,8.878366e-01 ,8.893534e-01 ,&
        & 8.908350e-01 ,8.922854e-01 ,8.937090e-01 ,8.951099e-01 ,8.964925e-01 ,&
        & 8.978609e-01 ,8.992192e-01 ,9.005718e-01 ,9.019229e-01 ,9.032765e-01 ,&
        & 9.046369e-01  /)
      asyice3(:, 19) = (/ &
! band 19
        & 7.812615e-01 ,7.887764e-01 ,7.959664e-01 ,8.028413e-01 ,8.094109e-01 ,&
        & 8.156849e-01 ,8.216730e-01 ,8.273846e-01 ,8.328294e-01 ,8.380166e-01 ,&
        & 8.429556e-01 ,8.476556e-01 ,8.521258e-01 ,8.563753e-01 ,8.604131e-01 ,&
        & 8.642481e-01 ,8.678893e-01 ,8.713455e-01 ,8.746254e-01 ,8.777378e-01 ,&
        & 8.806914e-01 ,8.834948e-01 ,8.861566e-01 ,8.886854e-01 ,8.910897e-01 ,&
        & 8.933779e-01 ,8.955586e-01 ,8.976402e-01 ,8.996311e-01 ,9.015398e-01 ,&
        & 9.033745e-01 ,9.051436e-01 ,9.068555e-01 ,9.085185e-01 ,9.101410e-01 ,&
        & 9.117311e-01 ,9.132972e-01 ,9.148476e-01 ,9.163905e-01 ,9.179340e-01 ,&
        & 9.194864e-01 ,9.210559e-01 ,9.226505e-01 ,9.242784e-01 ,9.259476e-01 ,&
        & 9.276661e-01  /)
      asyice3(:, 20) = (/ &
! band 20
        & 7.640720e-01 ,7.691119e-01 ,7.739941e-01 ,7.787222e-01 ,7.832998e-01 ,&
        & 7.877304e-01 ,7.920177e-01 ,7.961652e-01 ,8.001765e-01 ,8.040551e-01 ,&
        & 8.078044e-01 ,8.114280e-01 ,8.149294e-01 ,8.183119e-01 ,8.215791e-01 ,&
        & 8.247344e-01 ,8.277812e-01 ,8.307229e-01 ,8.335629e-01 ,8.363046e-01 ,&
        & 8.389514e-01 ,8.415067e-01 ,8.439738e-01 ,8.463560e-01 ,8.486568e-01 ,&
        & 8.508795e-01 ,8.530274e-01 ,8.551039e-01 ,8.571122e-01 ,8.590558e-01 ,&
        & 8.609378e-01 ,8.627618e-01 ,8.645309e-01 ,8.662485e-01 ,8.679178e-01 ,&
        & 8.695423e-01 ,8.711251e-01 ,8.726697e-01 ,8.741792e-01 ,8.756571e-01 ,&
        & 8.771065e-01 ,8.785307e-01 ,8.799331e-01 ,8.813169e-01 ,8.826854e-01 ,&
        & 8.840419e-01  /)
      asyice3(:, 21) = (/ &
! band 21
        & 7.602598e-01 ,7.651572e-01 ,7.699014e-01 ,7.744962e-01 ,7.789452e-01 ,&
        & 7.832522e-01 ,7.874205e-01 ,7.914538e-01 ,7.953555e-01 ,7.991290e-01 ,&
        & 8.027777e-01 ,8.063049e-01 ,8.097140e-01 ,8.130081e-01 ,8.161906e-01 ,&
        & 8.192645e-01 ,8.222331e-01 ,8.250993e-01 ,8.278664e-01 ,8.305374e-01 ,&
        & 8.331153e-01 ,8.356030e-01 ,8.380037e-01 ,8.403201e-01 ,8.425553e-01 ,&
        & 8.447121e-01 ,8.467935e-01 ,8.488022e-01 ,8.507412e-01 ,8.526132e-01 ,&
        & 8.544210e-01 ,8.561675e-01 ,8.578554e-01 ,8.594875e-01 ,8.610665e-01 ,&
        & 8.625951e-01 ,8.640760e-01 ,8.655119e-01 ,8.669055e-01 ,8.682594e-01 ,&
        & 8.695763e-01 ,8.708587e-01 ,8.721094e-01 ,8.733308e-01 ,8.745255e-01 ,&
        & 8.756961e-01  /)
      asyice3(:, 22) = (/ &
! band 22
        & 7.568957e-01 ,7.606995e-01 ,7.644072e-01 ,7.680204e-01 ,7.715402e-01 ,&
        & 7.749682e-01 ,7.783057e-01 ,7.815541e-01 ,7.847148e-01 ,7.877892e-01 ,&
        & 7.907786e-01 ,7.936846e-01 ,7.965084e-01 ,7.992515e-01 ,8.019153e-01 ,&
        & 8.045011e-01 ,8.070103e-01 ,8.094444e-01 ,8.118048e-01 ,8.140927e-01 ,&
        & 8.163097e-01 ,8.184571e-01 ,8.205364e-01 ,8.225488e-01 ,8.244958e-01 ,&
        & 8.263789e-01 ,8.281993e-01 ,8.299586e-01 ,8.316580e-01 ,8.332991e-01 ,&
        & 8.348831e-01 ,8.364115e-01 ,8.378857e-01 ,8.393071e-01 ,8.406770e-01 ,&
        & 8.419969e-01 ,8.432682e-01 ,8.444923e-01 ,8.456706e-01 ,8.468044e-01 ,&
        & 8.478952e-01 ,8.489444e-01 ,8.499533e-01 ,8.509234e-01 ,8.518561e-01 ,&
        & 8.527528e-01  /)
      asyice3(:, 23) = (/ &
! band 23
        & 7.575066e-01 ,7.606912e-01 ,7.638236e-01 ,7.669035e-01 ,7.699306e-01 ,&
        & 7.729046e-01 ,7.758254e-01 ,7.786926e-01 ,7.815060e-01 ,7.842654e-01 ,&
        & 7.869705e-01 ,7.896211e-01 ,7.922168e-01 ,7.947574e-01 ,7.972428e-01 ,&
        & 7.996726e-01 ,8.020466e-01 ,8.043646e-01 ,8.066262e-01 ,8.088313e-01 ,&
        & 8.109796e-01 ,8.130709e-01 ,8.151049e-01 ,8.170814e-01 ,8.190001e-01 ,&
        & 8.208608e-01 ,8.226632e-01 ,8.244071e-01 ,8.260924e-01 ,8.277186e-01 ,&
        & 8.292856e-01 ,8.307932e-01 ,8.322411e-01 ,8.336291e-01 ,8.349570e-01 ,&
        & 8.362244e-01 ,8.374312e-01 ,8.385772e-01 ,8.396621e-01 ,8.406856e-01 ,&
        & 8.416476e-01 ,8.425479e-01 ,8.433861e-01 ,8.441620e-01 ,8.448755e-01 ,&
        & 8.455263e-01  /)
      asyice3(:, 24) = (/ &
! band 24
        & 7.568829e-01 ,7.597947e-01 ,7.626745e-01 ,7.655212e-01 ,7.683337e-01 ,&
        & 7.711111e-01 ,7.738523e-01 ,7.765565e-01 ,7.792225e-01 ,7.818494e-01 ,&
        & 7.844362e-01 ,7.869819e-01 ,7.894854e-01 ,7.919459e-01 ,7.943623e-01 ,&
        & 7.967337e-01 ,7.990590e-01 ,8.013373e-01 ,8.035676e-01 ,8.057488e-01 ,&
        & 8.078802e-01 ,8.099605e-01 ,8.119890e-01 ,8.139645e-01 ,8.158862e-01 ,&
        & 8.177530e-01 ,8.195641e-01 ,8.213183e-01 ,8.230149e-01 ,8.246527e-01 ,&
        & 8.262308e-01 ,8.277483e-01 ,8.292042e-01 ,8.305976e-01 ,8.319275e-01 ,&
        & 8.331929e-01 ,8.343929e-01 ,8.355265e-01 ,8.365928e-01 ,8.375909e-01 ,&
        & 8.385197e-01 ,8.393784e-01 ,8.401659e-01 ,8.408815e-01 ,8.415240e-01 ,&
        & 8.420926e-01  /)
      asyice3(:, 25) = (/ &
! band 25
        & 7.548616e-01 ,7.575454e-01 ,7.602153e-01 ,7.628696e-01 ,7.655067e-01 ,&
        & 7.681249e-01 ,7.707225e-01 ,7.732978e-01 ,7.758492e-01 ,7.783750e-01 ,&
        & 7.808735e-01 ,7.833430e-01 ,7.857819e-01 ,7.881886e-01 ,7.905612e-01 ,&
        & 7.928983e-01 ,7.951980e-01 ,7.974588e-01 ,7.996789e-01 ,8.018567e-01 ,&
        & 8.039905e-01 ,8.060787e-01 ,8.081196e-01 ,8.101115e-01 ,8.120527e-01 ,&
        & 8.139416e-01 ,8.157764e-01 ,8.175557e-01 ,8.192776e-01 ,8.209405e-01 ,&
        & 8.225427e-01 ,8.240826e-01 ,8.255585e-01 ,8.269688e-01 ,8.283117e-01 ,&
        & 8.295856e-01 ,8.307889e-01 ,8.319198e-01 ,8.329767e-01 ,8.339579e-01 ,&
        & 8.348619e-01 ,8.356868e-01 ,8.364311e-01 ,8.370930e-01 ,8.376710e-01 ,&
        & 8.381633e-01  /)
      asyice3(:, 26) = (/ &
! band 26
        & 7.491854e-01 ,7.518523e-01 ,7.545089e-01 ,7.571534e-01 ,7.597839e-01 ,&
        & 7.623987e-01 ,7.649959e-01 ,7.675737e-01 ,7.701303e-01 ,7.726639e-01 ,&
        & 7.751727e-01 ,7.776548e-01 ,7.801084e-01 ,7.825318e-01 ,7.849230e-01 ,&
        & 7.872804e-01 ,7.896020e-01 ,7.918862e-01 ,7.941309e-01 ,7.963345e-01 ,&
        & 7.984951e-01 ,8.006109e-01 ,8.026802e-01 ,8.047009e-01 ,8.066715e-01 ,&
        & 8.085900e-01 ,8.104546e-01 ,8.122636e-01 ,8.140150e-01 ,8.157072e-01 ,&
        & 8.173382e-01 ,8.189063e-01 ,8.204096e-01 ,8.218464e-01 ,8.232148e-01 ,&
        & 8.245130e-01 ,8.257391e-01 ,8.268915e-01 ,8.279682e-01 ,8.289675e-01 ,&
        & 8.298875e-01 ,8.307264e-01 ,8.314824e-01 ,8.321537e-01 ,8.327385e-01 ,&
        & 8.332350e-01  /)
      asyice3(:, 27) = (/ &
! band 27
        & 7.397086e-01 ,7.424069e-01 ,7.450955e-01 ,7.477725e-01 ,7.504362e-01 ,&
        & 7.530846e-01 ,7.557159e-01 ,7.583283e-01 ,7.609199e-01 ,7.634888e-01 ,&
        & 7.660332e-01 ,7.685512e-01 ,7.710411e-01 ,7.735009e-01 ,7.759288e-01 ,&
        & 7.783229e-01 ,7.806814e-01 ,7.830024e-01 ,7.852841e-01 ,7.875246e-01 ,&
        & 7.897221e-01 ,7.918748e-01 ,7.939807e-01 ,7.960380e-01 ,7.980449e-01 ,&
        & 7.999995e-01 ,8.019000e-01 ,8.037445e-01 ,8.055311e-01 ,8.072581e-01 ,&
        & 8.089235e-01 ,8.105255e-01 ,8.120623e-01 ,8.135319e-01 ,8.149326e-01 ,&
        & 8.162626e-01 ,8.175198e-01 ,8.187025e-01 ,8.198089e-01 ,8.208371e-01 ,&
        & 8.217852e-01 ,8.226514e-01 ,8.234338e-01 ,8.241306e-01 ,8.247399e-01 ,&
        & 8.252599e-01  /)
      asyice3(:, 28) = (/ &
! band 28
        & 7.224533e-01 ,7.251681e-01 ,7.278728e-01 ,7.305654e-01 ,7.332444e-01 ,&
        & 7.359078e-01 ,7.385539e-01 ,7.411808e-01 ,7.437869e-01 ,7.463702e-01 ,&
        & 7.489291e-01 ,7.514616e-01 ,7.539661e-01 ,7.564408e-01 ,7.588837e-01 ,&
        & 7.612933e-01 ,7.636676e-01 ,7.660049e-01 ,7.683034e-01 ,7.705612e-01 ,&
        & 7.727767e-01 ,7.749480e-01 ,7.770733e-01 ,7.791509e-01 ,7.811789e-01 ,&
        & 7.831556e-01 ,7.850791e-01 ,7.869478e-01 ,7.887597e-01 ,7.905131e-01 ,&
        & 7.922062e-01 ,7.938372e-01 ,7.954044e-01 ,7.969059e-01 ,7.983399e-01 ,&
        & 7.997047e-01 ,8.009985e-01 ,8.022195e-01 ,8.033658e-01 ,8.044357e-01 ,&
        & 8.054275e-01 ,8.063392e-01 ,8.071692e-01 ,8.079157e-01 ,8.085768e-01 ,&
        & 8.091507e-01  /)
      asyice3(:, 29) = (/ &
! band 29
        & 8.850026e-01 ,9.005489e-01 ,9.069242e-01 ,9.121799e-01 ,9.168987e-01 ,&
        & 9.212259e-01 ,9.252176e-01 ,9.289028e-01 ,9.323000e-01 ,9.354235e-01 ,&
        & 9.382858e-01 ,9.408985e-01 ,9.432734e-01 ,9.454218e-01 ,9.473557e-01 ,&
        & 9.490871e-01 ,9.506282e-01 ,9.519917e-01 ,9.531904e-01 ,9.542374e-01 ,&
        & 9.551461e-01 ,9.559298e-01 ,9.566023e-01 ,9.571775e-01 ,9.576692e-01 ,&
        & 9.580916e-01 ,9.584589e-01 ,9.587853e-01 ,9.590851e-01 ,9.593729e-01 ,&
        & 9.596632e-01 ,9.599705e-01 ,9.603096e-01 ,9.606954e-01 ,9.611427e-01 ,&
        & 9.616667e-01 ,9.622826e-01 ,9.630060e-01 ,9.638524e-01 ,9.648379e-01 ,&
        & 9.659788e-01 ,9.672916e-01 ,9.687933e-01 ,9.705014e-01 ,9.724337e-01 ,&
        & 9.746084e-01  /)

! fdelta: unitless
      fdlice3(:, 16) = (/ &
! band 16
        & 4.959277e-02 ,4.685292e-02 ,4.426104e-02 ,4.181231e-02 ,3.950191e-02 ,&
        & 3.732500e-02 ,3.527675e-02 ,3.335235e-02 ,3.154697e-02 ,2.985578e-02 ,&
        & 2.827395e-02 ,2.679666e-02 ,2.541909e-02 ,2.413640e-02 ,2.294378e-02 ,&
        & 2.183639e-02 ,2.080940e-02 ,1.985801e-02 ,1.897736e-02 ,1.816265e-02 ,&
        & 1.740905e-02 ,1.671172e-02 ,1.606585e-02 ,1.546661e-02 ,1.490917e-02 ,&
        & 1.438870e-02 ,1.390038e-02 ,1.343939e-02 ,1.300089e-02 ,1.258006e-02 ,&
        & 1.217208e-02 ,1.177212e-02 ,1.137536e-02 ,1.097696e-02 ,1.057210e-02 ,&
        & 1.015596e-02 ,9.723704e-03 ,9.270516e-03 ,8.791565e-03 ,8.282026e-03 ,&
        & 7.737072e-03 ,7.151879e-03 ,6.521619e-03 ,5.841467e-03 ,5.106597e-03 ,&
        & 4.312183e-03  /)
      fdlice3(:, 17) = (/ &
! band 17
        & 5.071224e-02 ,5.000217e-02 ,4.933872e-02 ,4.871992e-02 ,4.814380e-02 ,&
        & 4.760839e-02 ,4.711170e-02 ,4.665177e-02 ,4.622662e-02 ,4.583426e-02 ,&
        & 4.547274e-02 ,4.514007e-02 ,4.483428e-02 ,4.455340e-02 ,4.429544e-02 ,&
        & 4.405844e-02 ,4.384041e-02 ,4.363939e-02 ,4.345340e-02 ,4.328047e-02 ,&
        & 4.311861e-02 ,4.296586e-02 ,4.282024e-02 ,4.267977e-02 ,4.254248e-02 ,&
        & 4.240640e-02 ,4.226955e-02 ,4.212995e-02 ,4.198564e-02 ,4.183462e-02 ,&
        & 4.167494e-02 ,4.150462e-02 ,4.132167e-02 ,4.112413e-02 ,4.091003e-02 ,&
        & 4.067737e-02 ,4.042420e-02 ,4.014854e-02 ,3.984840e-02 ,3.952183e-02 ,&
        & 3.916683e-02 ,3.878144e-02 ,3.836368e-02 ,3.791158e-02 ,3.742316e-02 ,&
        & 3.689645e-02  /)
      fdlice3(:, 18) = (/ &
! band 18
        & 1.062938e-01 ,1.065234e-01 ,1.067822e-01 ,1.070682e-01 ,1.073793e-01 ,&
        & 1.077137e-01 ,1.080693e-01 ,1.084442e-01 ,1.088364e-01 ,1.092439e-01 ,&
        & 1.096647e-01 ,1.100970e-01 ,1.105387e-01 ,1.109878e-01 ,1.114423e-01 ,&
        & 1.119004e-01 ,1.123599e-01 ,1.128190e-01 ,1.132757e-01 ,1.137279e-01 ,&
        & 1.141738e-01 ,1.146113e-01 ,1.150385e-01 ,1.154534e-01 ,1.158540e-01 ,&
        & 1.162383e-01 ,1.166045e-01 ,1.169504e-01 ,1.172741e-01 ,1.175738e-01 ,&
        & 1.178472e-01 ,1.180926e-01 ,1.183080e-01 ,1.184913e-01 ,1.186405e-01 ,&
        & 1.187538e-01 ,1.188291e-01 ,1.188645e-01 ,1.188580e-01 ,1.188076e-01 ,&
        & 1.187113e-01 ,1.185672e-01 ,1.183733e-01 ,1.181277e-01 ,1.178282e-01 ,&
        & 1.174731e-01  /)
      fdlice3(:, 19) = (/ &
! band 19
        & 1.076195e-01 ,1.065195e-01 ,1.054696e-01 ,1.044673e-01 ,1.035099e-01 ,&
        & 1.025951e-01 ,1.017203e-01 ,1.008831e-01 ,1.000808e-01 ,9.931116e-02 ,&
        & 9.857151e-02 ,9.785939e-02 ,9.717230e-02 ,9.650774e-02 ,9.586322e-02 ,&
        & 9.523623e-02 ,9.462427e-02 ,9.402484e-02 ,9.343544e-02 ,9.285358e-02 ,&
        & 9.227675e-02 ,9.170245e-02 ,9.112818e-02 ,9.055144e-02 ,8.996974e-02 ,&
        & 8.938056e-02 ,8.878142e-02 ,8.816981e-02 ,8.754323e-02 ,8.689919e-02 ,&
        & 8.623517e-02 ,8.554869e-02 ,8.483724e-02 ,8.409832e-02 ,8.332943e-02 ,&
        & 8.252807e-02 ,8.169175e-02 ,8.081795e-02 ,7.990419e-02 ,7.894796e-02 ,&
        & 7.794676e-02 ,7.689809e-02 ,7.579945e-02 ,7.464834e-02 ,7.344227e-02 ,&
        & 7.217872e-02  /)
      fdlice3(:, 20) = (/ &
! band 20
        & 1.119014e-01 ,1.122706e-01 ,1.126690e-01 ,1.130947e-01 ,1.135456e-01 ,&
        & 1.140199e-01 ,1.145154e-01 ,1.150302e-01 ,1.155623e-01 ,1.161096e-01 ,&
        & 1.166703e-01 ,1.172422e-01 ,1.178233e-01 ,1.184118e-01 ,1.190055e-01 ,&
        & 1.196025e-01 ,1.202008e-01 ,1.207983e-01 ,1.213931e-01 ,1.219832e-01 ,&
        & 1.225665e-01 ,1.231411e-01 ,1.237050e-01 ,1.242561e-01 ,1.247926e-01 ,&
        & 1.253122e-01 ,1.258132e-01 ,1.262934e-01 ,1.267509e-01 ,1.271836e-01 ,&
        & 1.275896e-01 ,1.279669e-01 ,1.283134e-01 ,1.286272e-01 ,1.289063e-01 ,&
        & 1.291486e-01 ,1.293522e-01 ,1.295150e-01 ,1.296351e-01 ,1.297104e-01 ,&
        & 1.297390e-01 ,1.297189e-01 ,1.296480e-01 ,1.295244e-01 ,1.293460e-01 ,&
        & 1.291109e-01  /)
      fdlice3(:, 21) = (/ &
! band 21
        & 1.133298e-01 ,1.136777e-01 ,1.140556e-01 ,1.144615e-01 ,1.148934e-01 ,&
        & 1.153492e-01 ,1.158269e-01 ,1.163243e-01 ,1.168396e-01 ,1.173706e-01 ,&
        & 1.179152e-01 ,1.184715e-01 ,1.190374e-01 ,1.196108e-01 ,1.201897e-01 ,&
        & 1.207720e-01 ,1.213558e-01 ,1.219389e-01 ,1.225194e-01 ,1.230951e-01 ,&
        & 1.236640e-01 ,1.242241e-01 ,1.247733e-01 ,1.253096e-01 ,1.258309e-01 ,&
        & 1.263352e-01 ,1.268205e-01 ,1.272847e-01 ,1.277257e-01 ,1.281415e-01 ,&
        & 1.285300e-01 ,1.288893e-01 ,1.292173e-01 ,1.295118e-01 ,1.297710e-01 ,&
        & 1.299927e-01 ,1.301748e-01 ,1.303154e-01 ,1.304124e-01 ,1.304637e-01 ,&
        & 1.304673e-01 ,1.304212e-01 ,1.303233e-01 ,1.301715e-01 ,1.299638e-01 ,&
        & 1.296983e-01  /)
      fdlice3(:, 22) = (/ &
! band 22
        & 1.145360e-01 ,1.153256e-01 ,1.161453e-01 ,1.169929e-01 ,1.178666e-01 ,&
        & 1.187641e-01 ,1.196835e-01 ,1.206227e-01 ,1.215796e-01 ,1.225522e-01 ,&
        & 1.235383e-01 ,1.245361e-01 ,1.255433e-01 ,1.265579e-01 ,1.275779e-01 ,&
        & 1.286011e-01 ,1.296257e-01 ,1.306494e-01 ,1.316703e-01 ,1.326862e-01 ,&
        & 1.336951e-01 ,1.346950e-01 ,1.356838e-01 ,1.366594e-01 ,1.376198e-01 ,&
        & 1.385629e-01 ,1.394866e-01 ,1.403889e-01 ,1.412678e-01 ,1.421212e-01 ,&
        & 1.429469e-01 ,1.437430e-01 ,1.445074e-01 ,1.452381e-01 ,1.459329e-01 ,&
        & 1.465899e-01 ,1.472069e-01 ,1.477819e-01 ,1.483128e-01 ,1.487976e-01 ,&
        & 1.492343e-01 ,1.496207e-01 ,1.499548e-01 ,1.502346e-01 ,1.504579e-01 ,&
        & 1.506227e-01  /)
      fdlice3(:, 23) = (/ &
! band 23
        & 1.153263e-01 ,1.161445e-01 ,1.169932e-01 ,1.178703e-01 ,1.187738e-01 ,&
        & 1.197016e-01 ,1.206516e-01 ,1.216217e-01 ,1.226099e-01 ,1.236141e-01 ,&
        & 1.246322e-01 ,1.256621e-01 ,1.267017e-01 ,1.277491e-01 ,1.288020e-01 ,&
        & 1.298584e-01 ,1.309163e-01 ,1.319736e-01 ,1.330281e-01 ,1.340778e-01 ,&
        & 1.351207e-01 ,1.361546e-01 ,1.371775e-01 ,1.381873e-01 ,1.391820e-01 ,&
        & 1.401593e-01 ,1.411174e-01 ,1.420540e-01 ,1.429671e-01 ,1.438547e-01 ,&
        & 1.447146e-01 ,1.455449e-01 ,1.463433e-01 ,1.471078e-01 ,1.478364e-01 ,&
        & 1.485270e-01 ,1.491774e-01 ,1.497857e-01 ,1.503497e-01 ,1.508674e-01 ,&
        & 1.513367e-01 ,1.517554e-01 ,1.521216e-01 ,1.524332e-01 ,1.526880e-01 ,&
        & 1.528840e-01  /)
      fdlice3(:, 24) = (/ &
! band 24
        & 1.160842e-01 ,1.169118e-01 ,1.177697e-01 ,1.186556e-01 ,1.195676e-01 ,&
        & 1.205036e-01 ,1.214616e-01 ,1.224394e-01 ,1.234349e-01 ,1.244463e-01 ,&
        & 1.254712e-01 ,1.265078e-01 ,1.275539e-01 ,1.286075e-01 ,1.296664e-01 ,&
        & 1.307287e-01 ,1.317923e-01 ,1.328550e-01 ,1.339149e-01 ,1.349699e-01 ,&
        & 1.360179e-01 ,1.370567e-01 ,1.380845e-01 ,1.390991e-01 ,1.400984e-01 ,&
        & 1.410803e-01 ,1.420429e-01 ,1.429840e-01 ,1.439016e-01 ,1.447936e-01 ,&
        & 1.456579e-01 ,1.464925e-01 ,1.472953e-01 ,1.480642e-01 ,1.487972e-01 ,&
        & 1.494923e-01 ,1.501472e-01 ,1.507601e-01 ,1.513287e-01 ,1.518511e-01 ,&
        & 1.523252e-01 ,1.527489e-01 ,1.531201e-01 ,1.534368e-01 ,1.536969e-01 ,&
        & 1.538984e-01  /)
      fdlice3(:, 25) = (/ &
! band 25
        & 1.168725e-01 ,1.177088e-01 ,1.185747e-01 ,1.194680e-01 ,1.203867e-01 ,&
        & 1.213288e-01 ,1.222923e-01 ,1.232750e-01 ,1.242750e-01 ,1.252903e-01 ,&
        & 1.263187e-01 ,1.273583e-01 ,1.284069e-01 ,1.294626e-01 ,1.305233e-01 ,&
        & 1.315870e-01 ,1.326517e-01 ,1.337152e-01 ,1.347756e-01 ,1.358308e-01 ,&
        & 1.368788e-01 ,1.379175e-01 ,1.389449e-01 ,1.399590e-01 ,1.409577e-01 ,&
        & 1.419389e-01 ,1.429007e-01 ,1.438410e-01 ,1.447577e-01 ,1.456488e-01 ,&
        & 1.465123e-01 ,1.473461e-01 ,1.481483e-01 ,1.489166e-01 ,1.496492e-01 ,&
        & 1.503439e-01 ,1.509988e-01 ,1.516118e-01 ,1.521808e-01 ,1.527038e-01 ,&
        & 1.531788e-01 ,1.536037e-01 ,1.539764e-01 ,1.542951e-01 ,1.545575e-01 ,&
        & 1.547617e-01  /)
      fdlice3(:, 26) = (/ &
!band 26
        & 1.180509e-01 ,1.189025e-01 ,1.197820e-01 ,1.206875e-01 ,1.216171e-01 ,&
        & 1.225687e-01 ,1.235404e-01 ,1.245303e-01 ,1.255363e-01 ,1.265564e-01 ,&
        & 1.275888e-01 ,1.286313e-01 ,1.296821e-01 ,1.307392e-01 ,1.318006e-01 ,&
        & 1.328643e-01 ,1.339284e-01 ,1.349908e-01 ,1.360497e-01 ,1.371029e-01 ,&
        & 1.381486e-01 ,1.391848e-01 ,1.402095e-01 ,1.412208e-01 ,1.422165e-01 ,&
        & 1.431949e-01 ,1.441539e-01 ,1.450915e-01 ,1.460058e-01 ,1.468947e-01 ,&
        & 1.477564e-01 ,1.485888e-01 ,1.493900e-01 ,1.501580e-01 ,1.508907e-01 ,&
        & 1.515864e-01 ,1.522428e-01 ,1.528582e-01 ,1.534305e-01 ,1.539578e-01 ,&
        & 1.544380e-01 ,1.548692e-01 ,1.552494e-01 ,1.555767e-01 ,1.558490e-01 ,&
        & 1.560645e-01  /)
      fdlice3(:, 27) = (/ &
! band 27
        & 1.200480e-01 ,1.209267e-01 ,1.218304e-01 ,1.227575e-01 ,1.237059e-01 ,&
        & 1.246739e-01 ,1.256595e-01 ,1.266610e-01 ,1.276765e-01 ,1.287041e-01 ,&
        & 1.297420e-01 ,1.307883e-01 ,1.318412e-01 ,1.328988e-01 ,1.339593e-01 ,&
        & 1.350207e-01 ,1.360813e-01 ,1.371393e-01 ,1.381926e-01 ,1.392396e-01 ,&
        & 1.402783e-01 ,1.413069e-01 ,1.423235e-01 ,1.433263e-01 ,1.443134e-01 ,&
        & 1.452830e-01 ,1.462332e-01 ,1.471622e-01 ,1.480681e-01 ,1.489490e-01 ,&
        & 1.498032e-01 ,1.506286e-01 ,1.514236e-01 ,1.521863e-01 ,1.529147e-01 ,&
        & 1.536070e-01 ,1.542614e-01 ,1.548761e-01 ,1.554491e-01 ,1.559787e-01 ,&
        & 1.564629e-01 ,1.568999e-01 ,1.572879e-01 ,1.576249e-01 ,1.579093e-01 ,&
        & 1.581390e-01  /)
      fdlice3(:, 28) = (/ &
! band 28
        & 1.247813e-01 ,1.256496e-01 ,1.265417e-01 ,1.274560e-01 ,1.283905e-01 ,&
        & 1.293436e-01 ,1.303135e-01 ,1.312983e-01 ,1.322964e-01 ,1.333060e-01 ,&
        & 1.343252e-01 ,1.353523e-01 ,1.363855e-01 ,1.374231e-01 ,1.384632e-01 ,&
        & 1.395042e-01 ,1.405441e-01 ,1.415813e-01 ,1.426140e-01 ,1.436404e-01 ,&
        & 1.446587e-01 ,1.456672e-01 ,1.466640e-01 ,1.476475e-01 ,1.486157e-01 ,&
        & 1.495671e-01 ,1.504997e-01 ,1.514117e-01 ,1.523016e-01 ,1.531673e-01 ,&
        & 1.540073e-01 ,1.548197e-01 ,1.556026e-01 ,1.563545e-01 ,1.570734e-01 ,&
        & 1.577576e-01 ,1.584054e-01 ,1.590149e-01 ,1.595843e-01 ,1.601120e-01 ,&
        & 1.605962e-01 ,1.610349e-01 ,1.614266e-01 ,1.617693e-01 ,1.620614e-01 ,&
        & 1.623011e-01  /)
      fdlice3(:, 29) = (/ &
! band 29
        & 1.006055e-01 ,9.549582e-02 ,9.063960e-02 ,8.602900e-02 ,8.165612e-02 ,&
        & 7.751308e-02 ,7.359199e-02 ,6.988496e-02 ,6.638412e-02 ,6.308156e-02 ,&
        & 5.996942e-02 ,5.703979e-02 ,5.428481e-02 ,5.169657e-02 ,4.926719e-02 ,&
        & 4.698880e-02 ,4.485349e-02 ,4.285339e-02 ,4.098061e-02 ,3.922727e-02 ,&
        & 3.758547e-02 ,3.604733e-02 ,3.460497e-02 ,3.325051e-02 ,3.197604e-02 ,&
        & 3.077369e-02 ,2.963558e-02 ,2.855381e-02 ,2.752050e-02 ,2.652776e-02 ,&
        & 2.556772e-02 ,2.463247e-02 ,2.371415e-02 ,2.280485e-02 ,2.189670e-02 ,&
        & 2.098180e-02 ,2.005228e-02 ,1.910024e-02 ,1.811781e-02 ,1.709709e-02 ,&
        & 1.603020e-02 ,1.490925e-02 ,1.372635e-02 ,1.247363e-02 ,1.114319e-02 ,&
        & 9.727157e-03  /)


      extice4(:, 16) = (/ &
        &  0.1044555E+01 ,  0.1203676E+01 ,  0.1073405E+01 ,  0.8951550E+00 ,  0.7390689E+00 ,&
        &  0.6153759E+00 ,  0.5198989E+00 ,  0.4464092E+00 ,  0.3894742E+00 ,  0.3448678E+00 ,&
        &  0.3094197E+00 ,  0.2807807E+00 ,  0.2572316E+00 ,  0.2375326E+00 ,  0.2207851E+00 ,&
        &  0.2063371E+00 ,  0.1937186E+00 ,  0.1825884E+00 ,  0.1726631E+00 ,  0.1637662E+00 ,&
        &  0.1557344E+00 ,  0.1484502E+00 ,  0.1418036E+00 ,  0.1357148E+00 ,  0.1301263E+00 ,&
        &  0.1249710E+00 ,  0.1202028E+00 ,  0.1157786E+00 ,  0.1116665E+00 ,  0.1078338E+00 ,&
        &  0.1042548E+00 ,  0.1009008E+00 ,  0.9775314E-01 ,  0.9479627E-01 ,  0.9201247E-01 ,&
        &  0.8938390E-01 ,  0.8690302E-01 ,  0.8455290E-01 ,  0.8232602E-01 ,  0.8021355E-01 ,&
        &  0.7820601E-01 ,  0.7629608E-01 ,  0.7447571E-01 ,  0.7274083E-01 ,  0.7108492E-01 ,&
        &  0.6950055E-01 ,  0.6798557E-01 ,  0.6653521E-01 ,  0.6514487E-01 ,  0.6381106E-01 ,&
        &  0.6253067E-01 ,  0.6130035E-01 ,  0.6011723E-01 ,  0.5897878E-01 ,  0.5788214E-01 ,&
        &  0.5682641E-01 ,  0.5580740E-01 ,  0.5482358E-01 ,  0.5387500E-01 ,  0.5295752E-01 ,&
        &  0.5207116E-01 ,  0.5121392E-01 ,  0.5038402E-01 ,  0.4958022E-01 ,  0.4880159E-01 ,&
        &  0.4804720E-01 ,  0.4731641E-01 ,  0.4660610E-01 ,  0.4591725E-01 ,  0.4524862E-01 ,&
        &  0.4459908E-01 ,  0.4396743E-01 ,  0.4335355E-01 ,  0.4275709E-01 ,  0.4217590E-01 ,&
        &  0.4161095E-01 ,  0.4106024E-01 ,  0.4052462E-01 ,  0.4000197E-01 ,  0.3949309E-01 ,&
        &  0.3899699E-01 ,  0.3851286E-01 ,  0.3804038E-01 ,  0.3757953E-01 ,  0.3712986E-01 ,&
        &  0.3669107E-01 ,  0.3626178E-01 ,  0.3584285E-01 ,  0.3543320E-01 ,  0.3503278E-01 ,&
        &  0.3464155E-01 ,  0.3425872E-01 ,  0.3388441E-01 ,  0.3351796E-01 ,  0.3315932E-01 ,&
        &  0.3280823E-01 ,  0.3246461E-01 ,  0.3212810E-01 ,  0.3179850E-01 ,  0.3147537E-01 ,&
        &  0.3115890E-01 ,  0.3084878E-01 ,  0.3054465E-01 ,  0.3024667E-01 ,  0.2995411E-01 ,&
        &  0.2966714E-01 ,  0.2938586E-01 ,  0.2910958E-01 ,  0.2883859E-01 ,  0.2857251E-01 ,&
        &  0.2831137E-01 ,  0.2805494E-01 ,  0.2780301E-01 ,  0.2755551E-01 ,  0.2731267E-01 ,&
        &  0.2707366E-01 ,  0.2683903E-01 ,  0.2660842E-01 ,  0.2638168E-01 ,  0.2615874E-01 ,&
        &  0.2593950E-01 ,  0.2572401E-01 ,  0.2551194E-01 ,  0.2530332E-01 ,  0.2509815E-01 ,&
        &  0.2489631E-01 ,  0.2469759E-01 ,  0.2450210E-01 ,  0.2430961E-01 ,  0.2412017E-01 ,&
        &  0.2393354E-01 ,  0.2374990E-01 ,  0.2356901E-01 ,  0.2339082E-01 ,  0.2321524E-01 ,&
        &  0.2304241E-01 ,  0.2287203E-01 ,  0.2270414E-01 ,  0.2253871E-01 ,  0.2237578E-01 ,&
        &  0.2221496E-01 ,  0.2205657E-01 ,  0.2190050E-01 ,  0.2174648E-01 ,  0.2159478E-01 ,&
        &  0.2144496E-01 ,  0.2129730E-01 ,  0.2115171E-01 ,  0.2100797E-01 ,  0.2086618E-01 ,&
        &  0.2072638E-01 ,  0.2058838E-01 ,  0.2045222E-01 ,  0.2031766E-01 ,  0.2018519E-01 ,&
        &  0.2005415E-01 ,  0.1992492E-01 ,  0.1979736E-01 ,  0.1967139E-01 ,  0.1954699E-01 ,&
        &  0.1942423E-01 ,  0.1930292E-01 ,  0.1918316E-01 ,  0.1906489E-01 ,  0.1894801E-01 ,&
        &  0.1883259E-01 ,  0.1871851E-01 ,  0.1860590E-01 ,  0.1849451E-01 ,  0.1838453E-01 ,&
        &  0.1827577E-01 ,  0.1816835E-01 ,  0.1806215E-01 ,  0.1795730E-01 ,  0.1785355E-01 ,&
        &  0.1775094E-01 ,  0.1764968E-01 ,  0.1754933E-01 ,  0.1745033E-01 ,  0.1735235E-01 ,&
        &  0.1725539E-01 ,  0.1715956E-01 ,  0.1706481E-01 ,  0.1697120E-01 ,  0.1687851E-01 ,&
        &  0.1678658E-01 ,  0.1669611E-01 ,  0.1660635E-01 ,  0.1651768E-01 ,  0.1642979E-01 ,&
        &  0.1634285E-01 ,  0.1625676E-01 ,  0.1617153E-01 ,  0.1608739E-01 ,  0.1600428E-01 ,&
        &  0.1592169E-01 ,  0.1583990E-01 ,  0.1575910E-01 ,  0.1567927E-01 ,  0.1559990E-01 /)
      extice4(:, 17) = (/ &
        &  0.4763286E+00 ,  0.5141250E+00 ,  0.5273511E+00 ,  0.5132986E+00 ,  0.4830320E+00 ,&
        &  0.4463672E+00 ,  0.4088545E+00 ,  0.3731202E+00 ,  0.3402724E+00 ,  0.3106955E+00 ,&
        &  0.2844235E+00 ,  0.2612867E+00 ,  0.2410232E+00 ,  0.2233216E+00 ,  0.2078603E+00 ,&
        &  0.1943273E+00 ,  0.1824417E+00 ,  0.1719589E+00 ,  0.1626463E+00 ,  0.1543391E+00 ,&
        &  0.1468798E+00 ,  0.1401474E+00 ,  0.1340278E+00 ,  0.1284363E+00 ,  0.1233124E+00 ,&
        &  0.1185873E+00 ,  0.1142152E+00 ,  0.1101545E+00 ,  0.1063732E+00 ,  0.1028420E+00 ,&
        &  0.9953757E-01 ,  0.9643336E-01 ,  0.9351338E-01 ,  0.9076300E-01 ,  0.8816896E-01 ,&
        &  0.8571373E-01 ,  0.8339211E-01 ,  0.8118889E-01 ,  0.7909822E-01 ,  0.7711162E-01 ,&
        &  0.7522193E-01 ,  0.7342164E-01 ,  0.7170478E-01 ,  0.7006659E-01 ,  0.6850200E-01 ,&
        &  0.6700403E-01 ,  0.6557077E-01 ,  0.6419807E-01 ,  0.6288159E-01 ,  0.6161838E-01 ,&
        &  0.6040492E-01 ,  0.5923865E-01 ,  0.5811687E-01 ,  0.5703721E-01 ,  0.5599665E-01 ,&
        &  0.5499471E-01 ,  0.5402735E-01 ,  0.5309343E-01 ,  0.5219220E-01 ,  0.5132055E-01 ,&
        &  0.5047825E-01 ,  0.4966339E-01 ,  0.4887404E-01 ,  0.4810952E-01 ,  0.4736874E-01 ,&
        &  0.4665057E-01 ,  0.4595489E-01 ,  0.4527827E-01 ,  0.4462209E-01 ,  0.4398476E-01 ,&
        &  0.4336542E-01 ,  0.4276314E-01 ,  0.4217739E-01 ,  0.4160809E-01 ,  0.4105314E-01 ,&
        &  0.4051353E-01 ,  0.3998732E-01 ,  0.3947515E-01 ,  0.3897537E-01 ,  0.3848859E-01 ,&
        &  0.3801385E-01 ,  0.3755036E-01 ,  0.3709788E-01 ,  0.3665651E-01 ,  0.3622552E-01 ,&
        &  0.3580478E-01 ,  0.3539314E-01 ,  0.3499128E-01 ,  0.3459814E-01 ,  0.3421370E-01 ,&
        &  0.3383791E-01 ,  0.3347036E-01 ,  0.3311051E-01 ,  0.3275822E-01 ,  0.3241343E-01 ,&
        &  0.3207574E-01 ,  0.3174508E-01 ,  0.3142111E-01 ,  0.3110381E-01 ,  0.3079257E-01 ,&
        &  0.3048776E-01 ,  0.3018891E-01 ,  0.2989568E-01 ,  0.2960840E-01 ,  0.2932633E-01 ,&
        &  0.2904951E-01 ,  0.2877804E-01 ,  0.2851140E-01 ,  0.2824986E-01 ,  0.2799294E-01 ,&
        &  0.2774077E-01 ,  0.2749288E-01 ,  0.2724948E-01 ,  0.2701036E-01 ,  0.2677548E-01 ,&
        &  0.2654443E-01 ,  0.2631749E-01 ,  0.2609444E-01 ,  0.2587501E-01 ,  0.2565926E-01 ,&
        &  0.2544707E-01 ,  0.2523841E-01 ,  0.2503305E-01 ,  0.2483104E-01 ,  0.2463224E-01 ,&
        &  0.2443679E-01 ,  0.2424424E-01 ,  0.2405471E-01 ,  0.2386809E-01 ,  0.2368442E-01 ,&
        &  0.2350347E-01 ,  0.2332531E-01 ,  0.2314983E-01 ,  0.2297696E-01 ,  0.2280661E-01 ,&
        &  0.2263894E-01 ,  0.2247354E-01 ,  0.2231056E-01 ,  0.2214995E-01 ,  0.2199167E-01 ,&
        &  0.2183555E-01 ,  0.2168168E-01 ,  0.2152997E-01 ,  0.2138044E-01 ,  0.2123297E-01 ,&
        &  0.2108743E-01 ,  0.2094389E-01 ,  0.2080225E-01 ,  0.2066262E-01 ,  0.2052478E-01 ,&
        &  0.2038878E-01 ,  0.2025452E-01 ,  0.2012206E-01 ,  0.1999124E-01 ,  0.1986228E-01 ,&
        &  0.1973479E-01 ,  0.1960898E-01 ,  0.1948489E-01 ,  0.1936224E-01 ,  0.1924112E-01 ,&
        &  0.1912151E-01 ,  0.1900350E-01 ,  0.1888681E-01 ,  0.1877157E-01 ,  0.1865777E-01 ,&
        &  0.1854531E-01 ,  0.1843415E-01 ,  0.1832443E-01 ,  0.1821590E-01 ,  0.1810865E-01 ,&
        &  0.1800276E-01 ,  0.1789802E-01 ,  0.1779445E-01 ,  0.1769220E-01 ,  0.1759102E-01 ,&
        &  0.1749104E-01 ,  0.1739221E-01 ,  0.1729435E-01 ,  0.1719781E-01 ,  0.1710218E-01 ,&
        &  0.1700763E-01 ,  0.1691409E-01 ,  0.1682161E-01 ,  0.1673025E-01 ,  0.1663977E-01 ,&
        &  0.1655012E-01 ,  0.1646175E-01 ,  0.1637405E-01 ,  0.1628751E-01 ,  0.1620165E-01 ,&
        &  0.1611679E-01 ,  0.1603268E-01 ,  0.1594950E-01 ,  0.1586730E-01 ,  0.1578603E-01 ,&
        &  0.1570542E-01 ,  0.1562544E-01 ,  0.1554657E-01 ,  0.1546851E-01 ,  0.1539096E-01 /)
      extice4(:, 18) = (/ &
        &  0.4064021E+00 ,  0.7872863E+00 ,  0.8820615E+00 ,  0.8292196E+00 ,  0.7314100E+00 ,&
        &  0.6302420E+00 ,  0.5396259E+00 ,  0.4631953E+00 ,  0.4008095E+00 ,  0.3508492E+00 ,&
        &  0.3111808E+00 ,  0.2796517E+00 ,  0.2543678E+00 ,  0.2337898E+00 ,  0.2167377E+00 ,&
        &  0.2023380E+00 ,  0.1899584E+00 ,  0.1791528E+00 ,  0.1695768E+00 ,  0.1610149E+00 ,&
        &  0.1532899E+00 ,  0.1462755E+00 ,  0.1398632E+00 ,  0.1339731E+00 ,  0.1285543E+00 ,&
        &  0.1235421E+00 ,  0.1188960E+00 ,  0.1145765E+00 ,  0.1105532E+00 ,  0.1067977E+00 ,&
        &  0.1032865E+00 ,  0.9999224E-01 ,  0.9689742E-01 ,  0.9398752E-01 ,  0.9124668E-01 ,&
        &  0.8865657E-01 ,  0.8621120E-01 ,  0.8389350E-01 ,  0.8169663E-01 ,  0.7961147E-01 ,&
        &  0.7762991E-01 ,  0.7574398E-01 ,  0.7394617E-01 ,  0.7223210E-01 ,  0.7059608E-01 ,&
        &  0.6902973E-01 ,  0.6753232E-01 ,  0.6609816E-01 ,  0.6472336E-01 ,  0.6340417E-01 ,&
        &  0.6213750E-01 ,  0.6092009E-01 ,  0.5974939E-01 ,  0.5862262E-01 ,  0.5753724E-01 ,&
        &  0.5649206E-01 ,  0.5548300E-01 ,  0.5450904E-01 ,  0.5356917E-01 ,  0.5266066E-01 ,&
        &  0.5178246E-01 ,  0.5093337E-01 ,  0.5011111E-01 ,  0.4931447E-01 ,  0.4854301E-01 ,&
        &  0.4779536E-01 ,  0.4707108E-01 ,  0.4636711E-01 ,  0.4568442E-01 ,  0.4502154E-01 ,&
        &  0.4437758E-01 ,  0.4375159E-01 ,  0.4314278E-01 ,  0.4255147E-01 ,  0.4197507E-01 ,&
        &  0.4141500E-01 ,  0.4086885E-01 ,  0.4033767E-01 ,  0.3981933E-01 ,  0.3931467E-01 ,&
        &  0.3882248E-01 ,  0.3834236E-01 ,  0.3787363E-01 ,  0.3741677E-01 ,  0.3697066E-01 ,&
        &  0.3653515E-01 ,  0.3610943E-01 ,  0.3569381E-01 ,  0.3528723E-01 ,  0.3489015E-01 ,&
        &  0.3450166E-01 ,  0.3412203E-01 ,  0.3375052E-01 ,  0.3338682E-01 ,  0.3303086E-01 ,&
        &  0.3268255E-01 ,  0.3234151E-01 ,  0.3200735E-01 ,  0.3168022E-01 ,  0.3135935E-01 ,&
        &  0.3104525E-01 ,  0.3073731E-01 ,  0.3043546E-01 ,  0.3013958E-01 ,  0.2984907E-01 ,&
        &  0.2956425E-01 ,  0.2928479E-01 ,  0.2901045E-01 ,  0.2874136E-01 ,  0.2847716E-01 ,&
        &  0.2821771E-01 ,  0.2796281E-01 ,  0.2771265E-01 ,  0.2746689E-01 ,  0.2722548E-01 ,&
        &  0.2698816E-01 ,  0.2675505E-01 ,  0.2652581E-01 ,  0.2630054E-01 ,  0.2607893E-01 ,&
        &  0.2586111E-01 ,  0.2564690E-01 ,  0.2543620E-01 ,  0.2522895E-01 ,  0.2502487E-01 ,&
        &  0.2482434E-01 ,  0.2462679E-01 ,  0.2443234E-01 ,  0.2424099E-01 ,  0.2405267E-01 ,&
        &  0.2386715E-01 ,  0.2368448E-01 ,  0.2350466E-01 ,  0.2332753E-01 ,  0.2315287E-01 ,&
        &  0.2298096E-01 ,  0.2281159E-01 ,  0.2264459E-01 ,  0.2248003E-01 ,  0.2231796E-01 ,&
        &  0.2215810E-01 ,  0.2200054E-01 ,  0.2184520E-01 ,  0.2169198E-01 ,  0.2154108E-01 ,&
        &  0.2139205E-01 ,  0.2124518E-01 ,  0.2110025E-01 ,  0.2095727E-01 ,  0.2081623E-01 ,&
        &  0.2067707E-01 ,  0.2053980E-01 ,  0.2040426E-01 ,  0.2027041E-01 ,  0.2013855E-01 ,&
        &  0.2000820E-01 ,  0.1987956E-01 ,  0.1975258E-01 ,  0.1962719E-01 ,  0.1950344E-01 ,&
        &  0.1938115E-01 ,  0.1926048E-01 ,  0.1914127E-01 ,  0.1902344E-01 ,  0.1890709E-01 ,&
        &  0.1879229E-01 ,  0.1867873E-01 ,  0.1856654E-01 ,  0.1845567E-01 ,  0.1834618E-01 ,&
        &  0.1823801E-01 ,  0.1813099E-01 ,  0.1802527E-01 ,  0.1792081E-01 ,  0.1781754E-01 ,&
        &  0.1771539E-01 ,  0.1761451E-01 ,  0.1751470E-01 ,  0.1741607E-01 ,  0.1731854E-01 ,&
        &  0.1722193E-01 ,  0.1712654E-01 ,  0.1703223E-01 ,  0.1693896E-01 ,  0.1684669E-01 ,&
        &  0.1675518E-01 ,  0.1666505E-01 ,  0.1657562E-01 ,  0.1648727E-01 ,  0.1639979E-01 ,&
        &  0.1631317E-01 ,  0.1622747E-01 ,  0.1614256E-01 ,  0.1605880E-01 ,  0.1597600E-01 ,&
        &  0.1589379E-01 ,  0.1581230E-01 ,  0.1573187E-01 ,  0.1565234E-01 ,  0.1557325E-01 /)
      extice4(:, 19) = (/ &
        &  0.6246532E+00 ,  0.1059293E+01 ,  0.1060130E+01 ,  0.9193889E+00 ,  0.7653428E+00 ,&
        &  0.6319336E+00 ,  0.5252968E+00 ,  0.4434857E+00 ,  0.3819407E+00 ,  0.3356902E+00 ,&
        &  0.3004149E+00 ,  0.2728044E+00 ,  0.2505319E+00 ,  0.2320409E+00 ,  0.2163062E+00 ,&
        &  0.2026566E+00 ,  0.1906463E+00 ,  0.1799716E+00 ,  0.1703882E+00 ,  0.1617487E+00 ,&
        &  0.1539159E+00 ,  0.1467881E+00 ,  0.1402689E+00 ,  0.1342852E+00 ,  0.1287873E+00 ,&
        &  0.1237108E+00 ,  0.1190127E+00 ,  0.1146521E+00 ,  0.1105974E+00 ,  0.1068178E+00 ,&
        &  0.1032880E+00 ,  0.9997968E-01 ,  0.9687447E-01 ,  0.9395663E-01 ,  0.9121004E-01 ,&
        &  0.8861579E-01 ,  0.8616695E-01 ,  0.8384676E-01 ,  0.8164830E-01 ,  0.7956241E-01 ,&
        &  0.7758018E-01 ,  0.7569396E-01 ,  0.7389624E-01 ,  0.7218225E-01 ,  0.7054665E-01 ,&
        &  0.6898104E-01 ,  0.6748401E-01 ,  0.6605086E-01 ,  0.6467671E-01 ,  0.6335874E-01 ,&
        &  0.6209297E-01 ,  0.6087672E-01 ,  0.5970713E-01 ,  0.5858143E-01 ,  0.5749736E-01 ,&
        &  0.5645317E-01 ,  0.5544532E-01 ,  0.5447229E-01 ,  0.5353383E-01 ,  0.5262641E-01 ,&
        &  0.5174929E-01 ,  0.5090123E-01 ,  0.5008022E-01 ,  0.4928453E-01 ,  0.4851402E-01 ,&
        &  0.4776750E-01 ,  0.4704387E-01 ,  0.4634076E-01 ,  0.4565890E-01 ,  0.4499682E-01 ,&
        &  0.4435365E-01 ,  0.4372821E-01 ,  0.4312014E-01 ,  0.4252933E-01 ,  0.4195344E-01 ,&
        &  0.4139386E-01 ,  0.4084839E-01 ,  0.4031747E-01 ,  0.3979958E-01 ,  0.3929536E-01 ,&
        &  0.3880341E-01 ,  0.3832371E-01 ,  0.3785538E-01 ,  0.3739856E-01 ,  0.3695267E-01 ,&
        &  0.3651755E-01 ,  0.3609185E-01 ,  0.3567643E-01 ,  0.3527022E-01 ,  0.3487315E-01 ,&
        &  0.3448502E-01 ,  0.3410541E-01 ,  0.3373408E-01 ,  0.3337055E-01 ,  0.3301460E-01 ,&
        &  0.3266646E-01 ,  0.3232542E-01 ,  0.3199143E-01 ,  0.3166446E-01 ,  0.3134360E-01 ,&
        &  0.3102965E-01 ,  0.3072186E-01 ,  0.3042001E-01 ,  0.3012413E-01 ,  0.2983377E-01 ,&
        &  0.2954895E-01 ,  0.2926964E-01 ,  0.2899543E-01 ,  0.2872634E-01 ,  0.2846227E-01 ,&
        &  0.2820295E-01 ,  0.2794818E-01 ,  0.2769815E-01 ,  0.2745239E-01 ,  0.2721111E-01 ,&
        &  0.2697378E-01 ,  0.2674079E-01 ,  0.2651167E-01 ,  0.2628652E-01 ,  0.2606503E-01 ,&
        &  0.2584732E-01 ,  0.2563322E-01 ,  0.2542264E-01 ,  0.2521549E-01 ,  0.2501152E-01 ,&
        &  0.2481110E-01 ,  0.2461365E-01 ,  0.2441943E-01 ,  0.2422806E-01 ,  0.2403996E-01 ,&
        &  0.2385453E-01 ,  0.2367196E-01 ,  0.2349224E-01 ,  0.2331520E-01 ,  0.2314074E-01 ,&
        &  0.2296892E-01 ,  0.2279964E-01 ,  0.2263273E-01 ,  0.2246836E-01 ,  0.2230637E-01 ,&
        &  0.2214660E-01 ,  0.2198923E-01 ,  0.2183396E-01 ,  0.2168093E-01 ,  0.2153011E-01 ,&
        &  0.2138126E-01 ,  0.2123446E-01 ,  0.2108960E-01 ,  0.2094669E-01 ,  0.2080583E-01 ,&
        &  0.2066684E-01 ,  0.2052953E-01 ,  0.2039417E-01 ,  0.2026048E-01 ,  0.2012869E-01 ,&
        &  0.1999840E-01 ,  0.1986992E-01 ,  0.1974300E-01 ,  0.1961777E-01 ,  0.1949408E-01 ,&
        &  0.1937194E-01 ,  0.1925133E-01 ,  0.1913217E-01 ,  0.1901449E-01 ,  0.1889829E-01 ,&
        &  0.1878345E-01 ,  0.1867004E-01 ,  0.1855799E-01 ,  0.1844726E-01 ,  0.1833782E-01 ,&
        &  0.1822970E-01 ,  0.1812282E-01 ,  0.1801714E-01 ,  0.1791273E-01 ,  0.1780959E-01 ,&
        &  0.1770749E-01 ,  0.1760674E-01 ,  0.1750697E-01 ,  0.1740839E-01 ,  0.1731090E-01 ,&
        &  0.1721442E-01 ,  0.1711907E-01 ,  0.1702480E-01 ,  0.1693166E-01 ,  0.1683943E-01 ,&
        &  0.1674796E-01 ,  0.1665794E-01 ,  0.1656855E-01 ,  0.1648032E-01 ,  0.1639288E-01 ,&
        &  0.1630629E-01 ,  0.1622063E-01 ,  0.1613583E-01 ,  0.1605211E-01 ,  0.1596934E-01 ,&
        &  0.1588717E-01 ,  0.1580579E-01 ,  0.1572539E-01 ,  0.1564589E-01 ,  0.1556692E-01 /)
      extice4(:, 20) = (/ &
        &  0.9085256E+00 ,  0.1319605E+01 ,  0.1184964E+01 ,  0.9591054E+00 ,  0.7636590E+00 ,&
        &  0.6152265E+00 ,  0.5079246E+00 ,  0.4314498E+00 ,  0.3761872E+00 ,  0.3348962E+00 ,&
        &  0.3027546E+00 ,  0.2767475E+00 ,  0.2550463E+00 ,  0.2365285E+00 ,  0.2204751E+00 ,&
        &  0.2063979E+00 ,  0.1939485E+00 ,  0.1828724E+00 ,  0.1729404E+00 ,  0.1640087E+00 ,&
        &  0.1559322E+00 ,  0.1486047E+00 ,  0.1419199E+00 ,  0.1357985E+00 ,  0.1301851E+00 ,&
        &  0.1250102E+00 ,  0.1202274E+00 ,  0.1157924E+00 ,  0.1116724E+00 ,  0.1078338E+00 ,&
        &  0.1042508E+00 ,  0.1008945E+00 ,  0.9774424E-01 ,  0.9478536E-01 ,  0.9200054E-01 ,&
        &  0.8937059E-01 ,  0.8688883E-01 ,  0.8453744E-01 ,  0.8230979E-01 ,  0.8019655E-01 ,&
        &  0.7818793E-01 ,  0.7627731E-01 ,  0.7445667E-01 ,  0.7272081E-01 ,  0.7106467E-01 ,&
        &  0.6947973E-01 ,  0.6796420E-01 ,  0.6651365E-01 ,  0.6512312E-01 ,  0.6378913E-01 ,&
        &  0.6250855E-01 ,  0.6127837E-01 ,  0.6009537E-01 ,  0.5895704E-01 ,  0.5786052E-01 ,&
        &  0.5680489E-01 ,  0.5578626E-01 ,  0.5480282E-01 ,  0.5385457E-01 ,  0.5293744E-01 ,&
        &  0.5205141E-01 ,  0.5119474E-01 ,  0.5036539E-01 ,  0.4956187E-01 ,  0.4878400E-01 ,&
        &  0.4803012E-01 ,  0.4729981E-01 ,  0.4658997E-01 ,  0.4590181E-01 ,  0.4523361E-01 ,&
        &  0.4458472E-01 ,  0.4395370E-01 ,  0.4334043E-01 ,  0.4274435E-01 ,  0.4216374E-01 ,&
        &  0.4159937E-01 ,  0.4104921E-01 ,  0.4051393E-01 ,  0.3999200E-01 ,  0.3948363E-01 ,&
        &  0.3898784E-01 ,  0.3850419E-01 ,  0.3803220E-01 ,  0.3757199E-01 ,  0.3712277E-01 ,&
        &  0.3668424E-01 ,  0.3625556E-01 ,  0.3583706E-01 ,  0.3542765E-01 ,  0.3502781E-01 ,&
        &  0.3463679E-01 ,  0.3425452E-01 ,  0.3388058E-01 ,  0.3351451E-01 ,  0.3315607E-01 ,&
        &  0.3280549E-01 ,  0.3246222E-01 ,  0.3212589E-01 ,  0.3179663E-01 ,  0.3147382E-01 ,&
        &  0.3115767E-01 ,  0.3084787E-01 ,  0.3054404E-01 ,  0.3024638E-01 ,  0.2995411E-01 ,&
        &  0.2966743E-01 ,  0.2938629E-01 ,  0.2911044E-01 ,  0.2883972E-01 ,  0.2857392E-01 ,&
        &  0.2831290E-01 ,  0.2805660E-01 ,  0.2780506E-01 ,  0.2755781E-01 ,  0.2731508E-01 ,&
        &  0.2707632E-01 ,  0.2684193E-01 ,  0.2661156E-01 ,  0.2638492E-01 ,  0.2616222E-01 ,&
        &  0.2594320E-01 ,  0.2572794E-01 ,  0.2551595E-01 ,  0.2530756E-01 ,  0.2510260E-01 ,&
        &  0.2490097E-01 ,  0.2470233E-01 ,  0.2450705E-01 ,  0.2431464E-01 ,  0.2412540E-01 ,&
        &  0.2393896E-01 ,  0.2375540E-01 ,  0.2357459E-01 ,  0.2339659E-01 ,  0.2322119E-01 ,&
        &  0.2304843E-01 ,  0.2287823E-01 ,  0.2271041E-01 ,  0.2254515E-01 ,  0.2238229E-01 ,&
        &  0.2222164E-01 ,  0.2206342E-01 ,  0.2190731E-01 ,  0.2175345E-01 ,  0.2160181E-01 ,&
        &  0.2145215E-01 ,  0.2130466E-01 ,  0.2115901E-01 ,  0.2101543E-01 ,  0.2087381E-01 ,&
        &  0.2073406E-01 ,  0.2059610E-01 ,  0.2046010E-01 ,  0.2032558E-01 ,  0.2019317E-01 ,&
        &  0.2006227E-01 ,  0.1993309E-01 ,  0.1980558E-01 ,  0.1967965E-01 ,  0.1955538E-01 ,&
        &  0.1943258E-01 ,  0.1931141E-01 ,  0.1919169E-01 ,  0.1907346E-01 ,  0.1895662E-01 ,&
        &  0.1884124E-01 ,  0.1872730E-01 ,  0.1861463E-01 ,  0.1850338E-01 ,  0.1839335E-01 ,&
        &  0.1828471E-01 ,  0.1817734E-01 ,  0.1807126E-01 ,  0.1796635E-01 ,  0.1786264E-01 ,&
        &  0.1776015E-01 ,  0.1765884E-01 ,  0.1755861E-01 ,  0.1745965E-01 ,  0.1736170E-01 ,&
        &  0.1726478E-01 ,  0.1716898E-01 ,  0.1707427E-01 ,  0.1698061E-01 ,  0.1688803E-01 ,&
        &  0.1679613E-01 ,  0.1670569E-01 ,  0.1661588E-01 ,  0.1652724E-01 ,  0.1643938E-01 ,&
        &  0.1635247E-01 ,  0.1626641E-01 ,  0.1618122E-01 ,  0.1609718E-01 ,  0.1601402E-01 ,&
        &  0.1593146E-01 ,  0.1584970E-01 ,  0.1576893E-01 ,  0.1568913E-01 ,  0.1560978E-01 /)
      extice4(:, 21) = (/ &
        &  0.1393451E+01 ,  0.1615386E+01 ,  0.1273594E+01 ,  0.9558807E+00 ,  0.7321575E+00 ,&
        &  0.5849279E+00 ,  0.4879658E+00 ,  0.4211306E+00 ,  0.3720420E+00 ,  0.3338566E+00 ,&
        &  0.3029021E+00 ,  0.2771144E+00 ,  0.2552424E+00 ,  0.2364579E+00 ,  0.2201651E+00 ,&
        &  0.2059142E+00 ,  0.1933588E+00 ,  0.1822288E+00 ,  0.1722826E+00 ,  0.1633599E+00 ,&
        &  0.1553092E+00 ,  0.1480135E+00 ,  0.1413637E+00 ,  0.1352778E+00 ,  0.1296975E+00 ,&
        &  0.1245527E+00 ,  0.1197977E+00 ,  0.1153869E+00 ,  0.1112889E+00 ,  0.1074702E+00 ,&
        &  0.1039044E+00 ,  0.1005633E+00 ,  0.9742779E-01 ,  0.9448186E-01 ,  0.9170880E-01 ,&
        &  0.8908996E-01 ,  0.8661828E-01 ,  0.8427727E-01 ,  0.8205866E-01 ,  0.7995403E-01 ,&
        &  0.7795393E-01 ,  0.7605147E-01 ,  0.7423860E-01 ,  0.7251016E-01 ,  0.7086110E-01 ,&
        &  0.6928327E-01 ,  0.6777456E-01 ,  0.6633020E-01 ,  0.6494594E-01 ,  0.6361796E-01 ,&
        &  0.6234317E-01 ,  0.6111854E-01 ,  0.5994089E-01 ,  0.5880770E-01 ,  0.5771614E-01 ,&
        &  0.5666531E-01 ,  0.5565130E-01 ,  0.5467230E-01 ,  0.5372811E-01 ,  0.5281514E-01 ,&
        &  0.5193289E-01 ,  0.5108013E-01 ,  0.5025406E-01 ,  0.4945422E-01 ,  0.4867967E-01 ,&
        &  0.4792877E-01 ,  0.4720158E-01 ,  0.4649479E-01 ,  0.4580934E-01 ,  0.4514379E-01 ,&
        &  0.4449747E-01 ,  0.4386896E-01 ,  0.4325790E-01 ,  0.4266440E-01 ,  0.4208589E-01 ,&
        &  0.4152355E-01 ,  0.4097538E-01 ,  0.4044204E-01 ,  0.3992179E-01 ,  0.3941527E-01 ,&
        &  0.3892128E-01 ,  0.3843920E-01 ,  0.3796873E-01 ,  0.3751001E-01 ,  0.3706225E-01 ,&
        &  0.3662515E-01 ,  0.3619768E-01 ,  0.3578053E-01 ,  0.3537245E-01 ,  0.3497374E-01 ,&
        &  0.3458383E-01 ,  0.3420263E-01 ,  0.3382975E-01 ,  0.3346472E-01 ,  0.3310745E-01 ,&
        &  0.3275770E-01 ,  0.3241524E-01 ,  0.3207986E-01 ,  0.3175153E-01 ,  0.3142933E-01 ,&
        &  0.3111408E-01 ,  0.3080501E-01 ,  0.3050190E-01 ,  0.3020478E-01 ,  0.2991321E-01 ,&
        &  0.2962735E-01 ,  0.2934674E-01 ,  0.2907153E-01 ,  0.2880131E-01 ,  0.2853614E-01 ,&
        &  0.2827574E-01 ,  0.2802005E-01 ,  0.2776883E-01 ,  0.2752204E-01 ,  0.2727989E-01 ,&
        &  0.2704170E-01 ,  0.2680773E-01 ,  0.2657765E-01 ,  0.2635156E-01 ,  0.2612926E-01 ,&
        &  0.2591064E-01 ,  0.2569577E-01 ,  0.2548417E-01 ,  0.2527616E-01 ,  0.2507158E-01 ,&
        &  0.2487019E-01 ,  0.2467204E-01 ,  0.2447700E-01 ,  0.2428494E-01 ,  0.2409605E-01 ,&
        &  0.2390995E-01 ,  0.2372673E-01 ,  0.2354625E-01 ,  0.2336846E-01 ,  0.2319338E-01 ,&
        &  0.2302094E-01 ,  0.2285094E-01 ,  0.2268344E-01 ,  0.2251848E-01 ,  0.2235581E-01 ,&
        &  0.2219546E-01 ,  0.2203742E-01 ,  0.2188160E-01 ,  0.2172802E-01 ,  0.2157656E-01 ,&
        &  0.2142718E-01 ,  0.2127985E-01 ,  0.2113449E-01 ,  0.2099107E-01 ,  0.2084970E-01 ,&
        &  0.2071012E-01 ,  0.2057242E-01 ,  0.2043657E-01 ,  0.2030231E-01 ,  0.2017004E-01 ,&
        &  0.2003939E-01 ,  0.1991036E-01 ,  0.1978309E-01 ,  0.1965741E-01 ,  0.1953328E-01 ,&
        &  0.1941071E-01 ,  0.1928967E-01 ,  0.1917018E-01 ,  0.1905208E-01 ,  0.1893546E-01 ,&
        &  0.1882021E-01 ,  0.1870639E-01 ,  0.1859394E-01 ,  0.1848282E-01 ,  0.1837308E-01 ,&
        &  0.1826457E-01 ,  0.1815740E-01 ,  0.1805134E-01 ,  0.1794665E-01 ,  0.1784313E-01 ,&
        &  0.1774075E-01 ,  0.1763964E-01 ,  0.1753952E-01 ,  0.1744066E-01 ,  0.1734282E-01 ,&
        &  0.1724609E-01 ,  0.1715039E-01 ,  0.1705586E-01 ,  0.1696239E-01 ,  0.1686991E-01 ,&
        &  0.1677819E-01 ,  0.1668785E-01 ,  0.1659821E-01 ,  0.1650967E-01 ,  0.1642190E-01 ,&
        &  0.1633517E-01 ,  0.1624919E-01 ,  0.1616417E-01 ,  0.1608014E-01 ,  0.1599715E-01 ,&
        &  0.1591475E-01 ,  0.1583308E-01 ,  0.1575247E-01 ,  0.1567275E-01 ,  0.1559349E-01 /)
      extice4(:, 22) = (/ &
        &  0.1753817E+01 ,  0.1766698E+01 ,  0.1293728E+01 ,  0.9349526E+00 ,  0.7090753E+00 ,&
        &  0.5709475E+00 ,  0.4819101E+00 ,  0.4192748E+00 ,  0.3717348E+00 ,  0.3338303E+00 ,&
        &  0.3027139E+00 ,  0.2766934E+00 ,  0.2546459E+00 ,  0.2357605E+00 ,  0.2194277E+00 ,&
        &  0.2051804E+00 ,  0.1926509E+00 ,  0.1815602E+00 ,  0.1716574E+00 ,  0.1627775E+00 ,&
        &  0.1547663E+00 ,  0.1475072E+00 ,  0.1408909E+00 ,  0.1348343E+00 ,  0.1292798E+00 ,&
        &  0.1241588E+00 ,  0.1194253E+00 ,  0.1150349E+00 ,  0.1109543E+00 ,  0.1071520E+00 ,&
        &  0.1036015E+00 ,  0.1002747E+00 ,  0.9715207E-01 ,  0.9421833E-01 ,  0.9145632E-01 ,&
        &  0.8884794E-01 ,  0.8638614E-01 ,  0.8405367E-01 ,  0.8184358E-01 ,  0.7974625E-01 ,&
        &  0.7775351E-01 ,  0.7585764E-01 ,  0.7405071E-01 ,  0.7232831E-01 ,  0.7068431E-01 ,&
        &  0.6911133E-01 ,  0.6760726E-01 ,  0.6616735E-01 ,  0.6478736E-01 ,  0.6346317E-01 ,&
        &  0.6219203E-01 ,  0.6097089E-01 ,  0.5979661E-01 ,  0.5866668E-01 ,  0.5757852E-01 ,&
        &  0.5653068E-01 ,  0.5551931E-01 ,  0.5454312E-01 ,  0.5360190E-01 ,  0.5269156E-01 ,&
        &  0.5181209E-01 ,  0.5096152E-01 ,  0.5013832E-01 ,  0.4934077E-01 ,  0.4856845E-01 ,&
        &  0.4782017E-01 ,  0.4709529E-01 ,  0.4639051E-01 ,  0.4570747E-01 ,  0.4504427E-01 ,&
        &  0.4439999E-01 ,  0.4377348E-01 ,  0.4316437E-01 ,  0.4257276E-01 ,  0.4199629E-01 ,&
        &  0.4143594E-01 ,  0.4088971E-01 ,  0.4035826E-01 ,  0.3983967E-01 ,  0.3933495E-01 ,&
        &  0.3884251E-01 ,  0.3836214E-01 ,  0.3789335E-01 ,  0.3743626E-01 ,  0.3699011E-01 ,&
        &  0.3655455E-01 ,  0.3612860E-01 ,  0.3571277E-01 ,  0.3530615E-01 ,  0.3490869E-01 ,&
        &  0.3452016E-01 ,  0.3414033E-01 ,  0.3376862E-01 ,  0.3340490E-01 ,  0.3304875E-01 ,&
        &  0.3270010E-01 ,  0.3235902E-01 ,  0.3202469E-01 ,  0.3169739E-01 ,  0.3137635E-01 ,&
        &  0.3106208E-01 ,  0.3075397E-01 ,  0.3045181E-01 ,  0.3015577E-01 ,  0.2986510E-01 ,&
        &  0.2957999E-01 ,  0.2930039E-01 ,  0.2902590E-01 ,  0.2875653E-01 ,  0.2849219E-01 ,&
        &  0.2823260E-01 ,  0.2797756E-01 ,  0.2772727E-01 ,  0.2748125E-01 ,  0.2723972E-01 ,&
        &  0.2700214E-01 ,  0.2676878E-01 ,  0.2653942E-01 ,  0.2631391E-01 ,  0.2609232E-01 ,&
        &  0.2587426E-01 ,  0.2565994E-01 ,  0.2544901E-01 ,  0.2524153E-01 ,  0.2503735E-01 ,&
        &  0.2483660E-01 ,  0.2463895E-01 ,  0.2444441E-01 ,  0.2425285E-01 ,  0.2406432E-01 ,&
        &  0.2387870E-01 ,  0.2369583E-01 ,  0.2351581E-01 ,  0.2333849E-01 ,  0.2316386E-01 ,&
        &  0.2299175E-01 ,  0.2282219E-01 ,  0.2265500E-01 ,  0.2249036E-01 ,  0.2232811E-01 ,&
        &  0.2216818E-01 ,  0.2201044E-01 ,  0.2185502E-01 ,  0.2170163E-01 ,  0.2155056E-01 ,&
        &  0.2140147E-01 ,  0.2125442E-01 ,  0.2110933E-01 ,  0.2096629E-01 ,  0.2082509E-01 ,&
        &  0.2068587E-01 ,  0.2054844E-01 ,  0.2041284E-01 ,  0.2027884E-01 ,  0.2014682E-01 ,&
        &  0.2001632E-01 ,  0.1988753E-01 ,  0.1976050E-01 ,  0.1963496E-01 ,  0.1951107E-01 ,&
        &  0.1938873E-01 ,  0.1926793E-01 ,  0.1914857E-01 ,  0.1903070E-01 ,  0.1891421E-01 ,&
        &  0.1879927E-01 ,  0.1868558E-01 ,  0.1857334E-01 ,  0.1846234E-01 ,  0.1835273E-01 ,&
        &  0.1824442E-01 ,  0.1813737E-01 ,  0.1803152E-01 ,  0.1792694E-01 ,  0.1782354E-01 ,&
        &  0.1772135E-01 ,  0.1762035E-01 ,  0.1752043E-01 ,  0.1742168E-01 ,  0.1732403E-01 ,&
        &  0.1722740E-01 ,  0.1713189E-01 ,  0.1703738E-01 ,  0.1694400E-01 ,  0.1685170E-01 ,&
        &  0.1676008E-01 ,  0.1666984E-01 ,  0.1658030E-01 ,  0.1649193E-01 ,  0.1640426E-01 ,&
        &  0.1631762E-01 ,  0.1623174E-01 ,  0.1614680E-01 ,  0.1606294E-01 ,  0.1598004E-01 ,&
        &  0.1589765E-01 ,  0.1581606E-01 ,  0.1573553E-01 ,  0.1565598E-01 ,  0.1557680E-01 /)
      extice4(:, 23) = (/ &
        &  0.2539065E+01 ,  0.1905829E+01 ,  0.1242668E+01 ,  0.8773899E+00 ,  0.6785038E+00 ,&
        &  0.5577472E+00 ,  0.4752940E+00 ,  0.4142813E+00 ,  0.3669287E+00 ,  0.3290615E+00 ,&
        &  0.2981315E+00 ,  0.2724272E+00 ,  0.2507530E+00 ,  0.2322428E+00 ,  0.2162578E+00 ,&
        &  0.2023158E+00 ,  0.1900511E+00 ,  0.1791871E+00 ,  0.1694768E+00 ,  0.1607631E+00 ,&
        &  0.1528971E+00 ,  0.1457643E+00 ,  0.1392594E+00 ,  0.1333024E+00 ,  0.1278381E+00 ,&
        &  0.1227988E+00 ,  0.1181396E+00 ,  0.1138173E+00 ,  0.1098000E+00 ,  0.1060552E+00 ,&
        &  0.1025584E+00 ,  0.9928162E-01 ,  0.9620553E-01 ,  0.9331508E-01 ,  0.9059433E-01 ,&
        &  0.8802406E-01 ,  0.8559787E-01 ,  0.8329917E-01 ,  0.8112070E-01 ,  0.7905379E-01 ,&
        &  0.7708962E-01 ,  0.7522023E-01 ,  0.7343891E-01 ,  0.7174058E-01 ,  0.7011959E-01 ,&
        &  0.6856797E-01 ,  0.6708433E-01 ,  0.6566401E-01 ,  0.6430217E-01 ,  0.6299572E-01 ,&
        &  0.6174131E-01 ,  0.6053597E-01 ,  0.5937660E-01 ,  0.5826102E-01 ,  0.5718614E-01 ,&
        &  0.5615138E-01 ,  0.5515235E-01 ,  0.5418782E-01 ,  0.5325733E-01 ,  0.5235788E-01 ,&
        &  0.5148819E-01 ,  0.5064733E-01 ,  0.4983304E-01 ,  0.4904436E-01 ,  0.4828016E-01 ,&
        &  0.4753976E-01 ,  0.4682229E-01 ,  0.4612472E-01 ,  0.4544844E-01 ,  0.4479180E-01 ,&
        &  0.4415370E-01 ,  0.4353318E-01 ,  0.4293011E-01 ,  0.4234375E-01 ,  0.4177260E-01 ,&
        &  0.4121723E-01 ,  0.4067585E-01 ,  0.4014892E-01 ,  0.3963494E-01 ,  0.3913450E-01 ,&
        &  0.3864626E-01 ,  0.3816998E-01 ,  0.3770500E-01 ,  0.3725162E-01 ,  0.3680891E-01 ,&
        &  0.3637690E-01 ,  0.3595424E-01 ,  0.3554161E-01 ,  0.3513813E-01 ,  0.3474391E-01 ,&
        &  0.3435839E-01 ,  0.3398132E-01 ,  0.3361232E-01 ,  0.3325124E-01 ,  0.3289768E-01 ,&
        &  0.3255173E-01 ,  0.3221298E-01 ,  0.3188109E-01 ,  0.3155617E-01 ,  0.3123732E-01 ,&
        &  0.3092534E-01 ,  0.3061933E-01 ,  0.3031938E-01 ,  0.3002535E-01 ,  0.2973666E-01 ,&
        &  0.2945349E-01 ,  0.2917579E-01 ,  0.2890318E-01 ,  0.2863578E-01 ,  0.2837309E-01 ,&
        &  0.2811528E-01 ,  0.2786198E-01 ,  0.2761326E-01 ,  0.2736878E-01 ,  0.2712890E-01 ,&
        &  0.2689294E-01 ,  0.2666117E-01 ,  0.2643325E-01 ,  0.2620928E-01 ,  0.2598894E-01 ,&
        &  0.2577238E-01 ,  0.2555940E-01 ,  0.2534979E-01 ,  0.2514361E-01 ,  0.2494083E-01 ,&
        &  0.2474122E-01 ,  0.2454481E-01 ,  0.2435148E-01 ,  0.2416112E-01 ,  0.2397389E-01 ,&
        &  0.2378932E-01 ,  0.2360759E-01 ,  0.2342870E-01 ,  0.2325248E-01 ,  0.2307884E-01 ,&
        &  0.2290781E-01 ,  0.2273920E-01 ,  0.2257317E-01 ,  0.2240946E-01 ,  0.2224822E-01 ,&
        &  0.2208908E-01 ,  0.2193233E-01 ,  0.2177778E-01 ,  0.2162536E-01 ,  0.2147513E-01 ,&
        &  0.2132687E-01 ,  0.2118076E-01 ,  0.2103647E-01 ,  0.2089423E-01 ,  0.2075382E-01 ,&
        &  0.2061538E-01 ,  0.2047872E-01 ,  0.2034388E-01 ,  0.2021062E-01 ,  0.2007935E-01 ,&
        &  0.1994958E-01 ,  0.1982151E-01 ,  0.1969519E-01 ,  0.1957035E-01 ,  0.1944716E-01 ,&
        &  0.1932541E-01 ,  0.1920528E-01 ,  0.1908659E-01 ,  0.1896938E-01 ,  0.1885354E-01 ,&
        &  0.1873915E-01 ,  0.1862610E-01 ,  0.1851440E-01 ,  0.1840402E-01 ,  0.1829493E-01 ,&
        &  0.1818724E-01 ,  0.1808070E-01 ,  0.1797544E-01 ,  0.1787136E-01 ,  0.1776854E-01 ,&
        &  0.1766685E-01 ,  0.1756633E-01 ,  0.1746688E-01 ,  0.1736869E-01 ,  0.1727151E-01 ,&
        &  0.1717533E-01 ,  0.1708028E-01 ,  0.1698630E-01 ,  0.1689346E-01 ,  0.1680152E-01 ,&
        &  0.1671034E-01 ,  0.1662061E-01 ,  0.1653149E-01 ,  0.1644346E-01 ,  0.1635629E-01 ,&
        &  0.1627006E-01 ,  0.1618459E-01 ,  0.1610006E-01 ,  0.1601660E-01 ,  0.1593409E-01 ,&
        &  0.1585209E-01 ,  0.1577090E-01 ,  0.1569075E-01 ,  0.1561150E-01 ,  0.1553270E-01 /)
      extice4(:, 24) = (/ &
        &  0.3354143E+01 ,  0.1891610E+01 ,  0.1158487E+01 ,  0.8431227E+00 ,  0.6696832E+00 ,&
        &  0.5552858E+00 ,  0.4733763E+00 ,  0.4120240E+00 ,  0.3645240E+00 ,  0.3267351E+00 ,&
        &  0.2959884E+00 ,  0.2704911E+00 ,  0.2490109E+00 ,  0.2306737E+00 ,  0.2148376E+00 ,&
        &  0.2010259E+00 ,  0.1888753E+00 ,  0.1781117E+00 ,  0.1684922E+00 ,  0.1598594E+00 ,&
        &  0.1520659E+00 ,  0.1449996E+00 ,  0.1385541E+00 ,  0.1326510E+00 ,  0.1272357E+00 ,&
        &  0.1222396E+00 ,  0.1176201E+00 ,  0.1133329E+00 ,  0.1093472E+00 ,  0.1056319E+00 ,&
        &  0.1021613E+00 ,  0.9890797E-01 ,  0.9585351E-01 ,  0.9298249E-01 ,  0.9027873E-01 ,&
        &  0.8772452E-01 ,  0.8531312E-01 ,  0.8302801E-01 ,  0.8086165E-01 ,  0.7880586E-01 ,&
        &  0.7685187E-01 ,  0.7499254E-01 ,  0.7322011E-01 ,  0.7152992E-01 ,  0.6991670E-01 ,&
        &  0.6837252E-01 ,  0.6689567E-01 ,  0.6548186E-01 ,  0.6412625E-01 ,  0.6282548E-01 ,&
        &  0.6157654E-01 ,  0.6037615E-01 ,  0.5922183E-01 ,  0.5811084E-01 ,  0.5704065E-01 ,&
        &  0.5600986E-01 ,  0.5501495E-01 ,  0.5405439E-01 ,  0.5312772E-01 ,  0.5223147E-01 ,&
        &  0.5136536E-01 ,  0.5052774E-01 ,  0.4971657E-01 ,  0.4893068E-01 ,  0.4816965E-01 ,&
        &  0.4743186E-01 ,  0.4671693E-01 ,  0.4602226E-01 ,  0.4534837E-01 ,  0.4469404E-01 ,&
        &  0.4405818E-01 ,  0.4344006E-01 ,  0.4283890E-01 ,  0.4225461E-01 ,  0.4168547E-01 ,&
        &  0.4113206E-01 ,  0.4059239E-01 ,  0.4006733E-01 ,  0.3955515E-01 ,  0.3905630E-01 ,&
        &  0.3856979E-01 ,  0.3809500E-01 ,  0.3763148E-01 ,  0.3717953E-01 ,  0.3673821E-01 ,&
        &  0.3630756E-01 ,  0.3588623E-01 ,  0.3547491E-01 ,  0.3507269E-01 ,  0.3467954E-01 ,&
        &  0.3429507E-01 ,  0.3391919E-01 ,  0.3355136E-01 ,  0.3319125E-01 ,  0.3283882E-01 ,&
        &  0.3249364E-01 ,  0.3215581E-01 ,  0.3182481E-01 ,  0.3150078E-01 ,  0.3118294E-01 ,&
        &  0.3087150E-01 ,  0.3056632E-01 ,  0.3026718E-01 ,  0.2997381E-01 ,  0.2968591E-01 ,&
        &  0.2940350E-01 ,  0.2912642E-01 ,  0.2885440E-01 ,  0.2858759E-01 ,  0.2832549E-01 ,&
        &  0.2806824E-01 ,  0.2781550E-01 ,  0.2756732E-01 ,  0.2732338E-01 ,  0.2708404E-01 ,&
        &  0.2684846E-01 ,  0.2661720E-01 ,  0.2638979E-01 ,  0.2616619E-01 ,  0.2594633E-01 ,&
        &  0.2573024E-01 ,  0.2551762E-01 ,  0.2530847E-01 ,  0.2510275E-01 ,  0.2490017E-01 ,&
        &  0.2470112E-01 ,  0.2450503E-01 ,  0.2431202E-01 ,  0.2412208E-01 ,  0.2393503E-01 ,&
        &  0.2375088E-01 ,  0.2356955E-01 ,  0.2339095E-01 ,  0.2321502E-01 ,  0.2304176E-01 ,&
        &  0.2287100E-01 ,  0.2270278E-01 ,  0.2253702E-01 ,  0.2237367E-01 ,  0.2221269E-01 ,&
        &  0.2205391E-01 ,  0.2189741E-01 ,  0.2174311E-01 ,  0.2159103E-01 ,  0.2144115E-01 ,&
        &  0.2129312E-01 ,  0.2114723E-01 ,  0.2100328E-01 ,  0.2086126E-01 ,  0.2072128E-01 ,&
        &  0.2058306E-01 ,  0.2044670E-01 ,  0.2031208E-01 ,  0.2017913E-01 ,  0.2004816E-01 ,&
        &  0.1991868E-01 ,  0.1979091E-01 ,  0.1966478E-01 ,  0.1954024E-01 ,  0.1941722E-01 ,&
        &  0.1929585E-01 ,  0.1917600E-01 ,  0.1905749E-01 ,  0.1894055E-01 ,  0.1882499E-01 ,&
        &  0.1871086E-01 ,  0.1859807E-01 ,  0.1848663E-01 ,  0.1837651E-01 ,  0.1826776E-01 ,&
        &  0.1816022E-01 ,  0.1805402E-01 ,  0.1794893E-01 ,  0.1784517E-01 ,  0.1774259E-01 ,&
        &  0.1764113E-01 ,  0.1754093E-01 ,  0.1744171E-01 ,  0.1734375E-01 ,  0.1724679E-01 ,&
        &  0.1715084E-01 ,  0.1705609E-01 ,  0.1696233E-01 ,  0.1686970E-01 ,  0.1677797E-01 ,&
        &  0.1668708E-01 ,  0.1659756E-01 ,  0.1650865E-01 ,  0.1642090E-01 ,  0.1633393E-01 ,&
        &  0.1624789E-01 ,  0.1616270E-01 ,  0.1607844E-01 ,  0.1599518E-01 ,  0.1591286E-01 ,&
        &  0.1583113E-01 ,  0.1575019E-01 ,  0.1567023E-01 ,  0.1559124E-01 ,  0.1551262E-01 /)
      extice4(:, 25) = (/ &
        &  0.3697936E+01 ,  0.1768606E+01 ,  0.1115521E+01 ,  0.8295092E+00 ,  0.6598829E+00 ,&
        &  0.5465474E+00 ,  0.4659038E+00 ,  0.4058042E+00 ,  0.3593403E+00 ,  0.3223587E+00 ,&
        &  0.2922416E+00 ,  0.2672444E+00 ,  0.2461685E+00 ,  0.2281623E+00 ,  0.2126039E+00 ,&
        &  0.1990254E+00 ,  0.1870715E+00 ,  0.1764750E+00 ,  0.1669994E+00 ,  0.1584899E+00 ,&
        &  0.1508021E+00 ,  0.1438276E+00 ,  0.1374620E+00 ,  0.1316302E+00 ,  0.1262766E+00 ,&
        &  0.1213371E+00 ,  0.1167670E+00 ,  0.1125258E+00 ,  0.1085817E+00 ,  0.1049038E+00 ,&
        &  0.1014686E+00 ,  0.9824804E-01 ,  0.9522388E-01 ,  0.9238140E-01 ,  0.8970495E-01 ,&
        &  0.8717656E-01 ,  0.8478913E-01 ,  0.8252677E-01 ,  0.8038238E-01 ,  0.7834745E-01 ,&
        &  0.7641292E-01 ,  0.7457178E-01 ,  0.7281704E-01 ,  0.7114337E-01 ,  0.6954630E-01 ,&
        &  0.6801687E-01 ,  0.6655483E-01 ,  0.6515423E-01 ,  0.6381130E-01 ,  0.6252270E-01 ,&
        &  0.6128514E-01 ,  0.6009571E-01 ,  0.5895163E-01 ,  0.5785022E-01 ,  0.5678899E-01 ,&
        &  0.5576711E-01 ,  0.5478024E-01 ,  0.5382720E-01 ,  0.5290805E-01 ,  0.5201880E-01 ,&
        &  0.5115897E-01 ,  0.5032765E-01 ,  0.4952236E-01 ,  0.4874193E-01 ,  0.4798617E-01 ,&
        &  0.4725328E-01 ,  0.4654308E-01 ,  0.4585279E-01 ,  0.4518314E-01 ,  0.4453272E-01 ,&
        &  0.4390087E-01 ,  0.4328644E-01 ,  0.4268866E-01 ,  0.4210785E-01 ,  0.4154170E-01 ,&
        &  0.4099140E-01 ,  0.4045457E-01 ,  0.3993245E-01 ,  0.3942278E-01 ,  0.3892654E-01 ,&
        &  0.3844239E-01 ,  0.3796992E-01 ,  0.3750864E-01 ,  0.3705889E-01 ,  0.3661972E-01 ,&
        &  0.3619098E-01 ,  0.3577171E-01 ,  0.3536238E-01 ,  0.3496195E-01 ,  0.3457054E-01 ,&
        &  0.3418778E-01 ,  0.3381358E-01 ,  0.3344737E-01 ,  0.3308887E-01 ,  0.3273784E-01 ,&
        &  0.3239435E-01 ,  0.3205802E-01 ,  0.3172834E-01 ,  0.3140575E-01 ,  0.3108917E-01 ,&
        &  0.3077927E-01 ,  0.3047530E-01 ,  0.3017734E-01 ,  0.2988543E-01 ,  0.2959866E-01 ,&
        &  0.2931752E-01 ,  0.2904153E-01 ,  0.2877072E-01 ,  0.2850497E-01 ,  0.2824403E-01 ,&
        &  0.2798793E-01 ,  0.2773633E-01 ,  0.2748913E-01 ,  0.2724628E-01 ,  0.2700787E-01 ,&
        &  0.2677349E-01 ,  0.2654314E-01 ,  0.2631674E-01 ,  0.2609401E-01 ,  0.2587514E-01 ,&
        &  0.2565990E-01 ,  0.2544823E-01 ,  0.2524002E-01 ,  0.2503510E-01 ,  0.2483356E-01 ,&
        &  0.2463516E-01 ,  0.2443996E-01 ,  0.2424781E-01 ,  0.2405861E-01 ,  0.2387252E-01 ,&
        &  0.2368909E-01 ,  0.2350846E-01 ,  0.2333067E-01 ,  0.2315553E-01 ,  0.2298295E-01 ,&
        &  0.2281285E-01 ,  0.2264538E-01 ,  0.2248026E-01 ,  0.2231755E-01 ,  0.2215719E-01 ,&
        &  0.2199913E-01 ,  0.2184334E-01 ,  0.2168963E-01 ,  0.2153814E-01 ,  0.2138884E-01 ,&
        &  0.2124148E-01 ,  0.2109616E-01 ,  0.2095276E-01 ,  0.2081139E-01 ,  0.2067185E-01 ,&
        &  0.2053426E-01 ,  0.2039833E-01 ,  0.2026432E-01 ,  0.2013188E-01 ,  0.2000131E-01 ,&
        &  0.1987234E-01 ,  0.1974506E-01 ,  0.1961942E-01 ,  0.1949535E-01 ,  0.1937291E-01 ,&
        &  0.1925191E-01 ,  0.1913242E-01 ,  0.1901447E-01 ,  0.1889788E-01 ,  0.1878276E-01 ,&
        &  0.1866908E-01 ,  0.1855663E-01 ,  0.1844562E-01 ,  0.1833592E-01 ,  0.1822751E-01 ,&
        &  0.1812038E-01 ,  0.1801450E-01 ,  0.1790981E-01 ,  0.1780637E-01 ,  0.1770419E-01 ,&
        &  0.1760304E-01 ,  0.1750314E-01 ,  0.1740430E-01 ,  0.1730664E-01 ,  0.1721006E-01 ,&
        &  0.1711440E-01 ,  0.1701993E-01 ,  0.1692654E-01 ,  0.1683410E-01 ,  0.1674273E-01 ,&
        &  0.1665211E-01 ,  0.1656286E-01 ,  0.1647430E-01 ,  0.1638682E-01 ,  0.1630011E-01 ,&
        &  0.1621433E-01 ,  0.1612939E-01 ,  0.1604531E-01 ,  0.1596229E-01 ,  0.1588022E-01 ,&
        &  0.1579881E-01 ,  0.1571804E-01 ,  0.1563832E-01 ,  0.1555957E-01 ,  0.1548118E-01 /)
      extice4(:, 26) = (/ &
        &  0.3713709E+01 ,  0.1690972E+01 ,  0.1108904E+01 ,  0.8237085E+00 ,  0.6531167E+00 ,&
        &  0.5406566E+00 ,  0.4611393E+00 ,  0.4019442E+00 ,  0.3561546E+00 ,  0.3196800E+00 ,&
        &  0.2899511E+00 ,  0.2652555E+00 ,  0.2444193E+00 ,  0.2266076E+00 ,  0.2112074E+00 ,&
        &  0.1977606E+00 ,  0.1859201E+00 ,  0.1754203E+00 ,  0.1660294E+00 ,  0.1575963E+00 ,&
        &  0.1499782E+00 ,  0.1430658E+00 ,  0.1367587E+00 ,  0.1309794E+00 ,  0.1256754E+00 ,&
        &  0.1207804E+00 ,  0.1162521E+00 ,  0.1120480E+00 ,  0.1081386E+00 ,  0.1044924E+00 ,&
        &  0.1010854E+00 ,  0.9789031E-01 ,  0.9488917E-01 ,  0.9206744E-01 ,  0.8940967E-01 ,&
        &  0.8689763E-01 ,  0.8452526E-01 ,  0.8227635E-01 ,  0.8014352E-01 ,  0.7811920E-01 ,&
        &  0.7619439E-01 ,  0.7436213E-01 ,  0.7261478E-01 ,  0.7094852E-01 ,  0.6935748E-01 ,&
        &  0.6783386E-01 ,  0.6637669E-01 ,  0.6498108E-01 ,  0.6364264E-01 ,  0.6235803E-01 ,&
        &  0.6112400E-01 ,  0.5993826E-01 ,  0.5879746E-01 ,  0.5769918E-01 ,  0.5664097E-01 ,&
        &  0.5562172E-01 ,  0.5463769E-01 ,  0.5368736E-01 ,  0.5277033E-01 ,  0.5188362E-01 ,&
        &  0.5102627E-01 ,  0.5019710E-01 ,  0.4939412E-01 ,  0.4861593E-01 ,  0.4786235E-01 ,&
        &  0.4713156E-01 ,  0.4642364E-01 ,  0.4573511E-01 ,  0.4506762E-01 ,  0.4441928E-01 ,&
        &  0.4378926E-01 ,  0.4317658E-01 ,  0.4258095E-01 ,  0.4200201E-01 ,  0.4143769E-01 ,&
        &  0.4088916E-01 ,  0.4035445E-01 ,  0.3983382E-01 ,  0.3932617E-01 ,  0.3883153E-01 ,&
        &  0.3834913E-01 ,  0.3787835E-01 ,  0.3741893E-01 ,  0.3697080E-01 ,  0.3653321E-01 ,&
        &  0.3610620E-01 ,  0.3568843E-01 ,  0.3528057E-01 ,  0.3488176E-01 ,  0.3449193E-01 ,&
        &  0.3411071E-01 ,  0.3373784E-01 ,  0.3337312E-01 ,  0.3301606E-01 ,  0.3266645E-01 ,&
        &  0.3232419E-01 ,  0.3198923E-01 ,  0.3166103E-01 ,  0.3133959E-01 ,  0.3102429E-01 ,&
        &  0.3071549E-01 ,  0.3041290E-01 ,  0.3011600E-01 ,  0.2982512E-01 ,  0.2953952E-01 ,&
        &  0.2925951E-01 ,  0.2898465E-01 ,  0.2871494E-01 ,  0.2845012E-01 ,  0.2819025E-01 ,&
        &  0.2793505E-01 ,  0.2768433E-01 ,  0.2743813E-01 ,  0.2719628E-01 ,  0.2695870E-01 ,&
        &  0.2672515E-01 ,  0.2649561E-01 ,  0.2627000E-01 ,  0.2604818E-01 ,  0.2583009E-01 ,&
        &  0.2561560E-01 ,  0.2540467E-01 ,  0.2519719E-01 ,  0.2499299E-01 ,  0.2479216E-01 ,&
        &  0.2459446E-01 ,  0.2439994E-01 ,  0.2420847E-01 ,  0.2401981E-01 ,  0.2383426E-01 ,&
        &  0.2365147E-01 ,  0.2347148E-01 ,  0.2329420E-01 ,  0.2311968E-01 ,  0.2294759E-01 ,&
        &  0.2277809E-01 ,  0.2261099E-01 ,  0.2244645E-01 ,  0.2228432E-01 ,  0.2212442E-01 ,&
        &  0.2196681E-01 ,  0.2181147E-01 ,  0.2165820E-01 ,  0.2150714E-01 ,  0.2135826E-01 ,&
        &  0.2121133E-01 ,  0.2106642E-01 ,  0.2092343E-01 ,  0.2078247E-01 ,  0.2064332E-01 ,&
        &  0.2050602E-01 ,  0.2037048E-01 ,  0.2023686E-01 ,  0.2010469E-01 ,  0.1997460E-01 ,&
        &  0.1984590E-01 ,  0.1971898E-01 ,  0.1959370E-01 ,  0.1946999E-01 ,  0.1934771E-01 ,&
        &  0.1922706E-01 ,  0.1910791E-01 ,  0.1899021E-01 ,  0.1887396E-01 ,  0.1875907E-01 ,&
        &  0.1864563E-01 ,  0.1853350E-01 ,  0.1842282E-01 ,  0.1831334E-01 ,  0.1820515E-01 ,&
        &  0.1809834E-01 ,  0.1799268E-01 ,  0.1788820E-01 ,  0.1778506E-01 ,  0.1768309E-01 ,&
        &  0.1758215E-01 ,  0.1748254E-01 ,  0.1738383E-01 ,  0.1728644E-01 ,  0.1719006E-01 ,&
        &  0.1709468E-01 ,  0.1700041E-01 ,  0.1690712E-01 ,  0.1681496E-01 ,  0.1672378E-01 ,&
        &  0.1663334E-01 ,  0.1654427E-01 ,  0.1645598E-01 ,  0.1636867E-01 ,  0.1628214E-01 ,&
        &  0.1619654E-01 ,  0.1611177E-01 ,  0.1602794E-01 ,  0.1594509E-01 ,  0.1586319E-01 ,&
        &  0.1578195E-01 ,  0.1570134E-01 ,  0.1562186E-01 ,  0.1554319E-01 ,  0.1546504E-01 /)
      extice4(:, 27) = (/ &
        &  0.3515744E+01 ,  0.1679518E+01 ,  0.1106005E+01 ,  0.8205456E+00 ,  0.6509200E+00 ,&
        &  0.5389830E+00 ,  0.4596938E+00 ,  0.4006369E+00 ,  0.3549737E+00 ,  0.3186234E+00 ,&
        &  0.2890102E+00 ,  0.2644200E+00 ,  0.2436766E+00 ,  0.2259466E+00 ,  0.2106173E+00 ,&
        &  0.1972325E+00 ,  0.1854458E+00 ,  0.1749928E+00 ,  0.1656424E+00 ,  0.1572441E+00 ,&
        &  0.1496553E+00 ,  0.1427687E+00 ,  0.1364827E+00 ,  0.1307226E+00 ,  0.1254338E+00 ,&
        &  0.1205528E+00 ,  0.1160370E+00 ,  0.1118438E+00 ,  0.1079436E+00 ,  0.1043065E+00 ,&
        &  0.1009075E+00 ,  0.9771990E-01 ,  0.9472628E-01 ,  0.9191114E-01 ,  0.8926004E-01 ,&
        &  0.8675431E-01 ,  0.8438747E-01 ,  0.8214422E-01 ,  0.8001716E-01 ,  0.7799793E-01 ,&
        &  0.7607797E-01 ,  0.7425031E-01 ,  0.7250772E-01 ,  0.7084529E-01 ,  0.6925827E-01 ,&
        &  0.6773882E-01 ,  0.6628531E-01 ,  0.6489290E-01 ,  0.6355783E-01 ,  0.6227646E-01 ,&
        &  0.6104585E-01 ,  0.5986280E-01 ,  0.5872458E-01 ,  0.5762908E-01 ,  0.5657328E-01 ,&
        &  0.5555663E-01 ,  0.5457481E-01 ,  0.5362663E-01 ,  0.5271194E-01 ,  0.5182698E-01 ,&
        &  0.5097182E-01 ,  0.5014452E-01 ,  0.4934312E-01 ,  0.4856693E-01 ,  0.4781482E-01 ,&
        &  0.4708568E-01 ,  0.4637915E-01 ,  0.4569240E-01 ,  0.4502620E-01 ,  0.4437934E-01 ,&
        &  0.4375074E-01 ,  0.4313925E-01 ,  0.4254476E-01 ,  0.4196694E-01 ,  0.4140391E-01 ,&
        &  0.4085644E-01 ,  0.4032255E-01 ,  0.3980313E-01 ,  0.3929625E-01 ,  0.3880256E-01 ,&
        &  0.3832109E-01 ,  0.3785123E-01 ,  0.3739251E-01 ,  0.3694505E-01 ,  0.3650813E-01 ,&
        &  0.3608177E-01 ,  0.3566463E-01 ,  0.3525740E-01 ,  0.3485902E-01 ,  0.3446979E-01 ,&
        &  0.3408898E-01 ,  0.3371669E-01 ,  0.3335236E-01 ,  0.3299569E-01 ,  0.3264645E-01 ,&
        &  0.3230456E-01 ,  0.3196996E-01 ,  0.3164196E-01 ,  0.3132086E-01 ,  0.3100575E-01 ,&
        &  0.3069730E-01 ,  0.3039488E-01 ,  0.3009830E-01 ,  0.2980759E-01 ,  0.2952215E-01 ,&
        &  0.2924217E-01 ,  0.2896746E-01 ,  0.2869777E-01 ,  0.2843311E-01 ,  0.2817340E-01 ,&
        &  0.2791821E-01 ,  0.2766764E-01 ,  0.2742146E-01 ,  0.2717961E-01 ,  0.2694218E-01 ,&
        &  0.2670850E-01 ,  0.2647910E-01 ,  0.2625350E-01 ,  0.2603170E-01 ,  0.2581361E-01 ,&
        &  0.2559913E-01 ,  0.2538821E-01 ,  0.2518062E-01 ,  0.2497642E-01 ,  0.2477548E-01 ,&
        &  0.2457791E-01 ,  0.2438327E-01 ,  0.2419169E-01 ,  0.2400317E-01 ,  0.2381751E-01 ,&
        &  0.2363472E-01 ,  0.2345475E-01 ,  0.2327748E-01 ,  0.2310273E-01 ,  0.2293065E-01 ,&
        &  0.2276116E-01 ,  0.2259408E-01 ,  0.2242944E-01 ,  0.2226720E-01 ,  0.2210742E-01 ,&
        &  0.2194972E-01 ,  0.2179428E-01 ,  0.2164113E-01 ,  0.2148997E-01 ,  0.2134111E-01 ,&
        &  0.2119419E-01 ,  0.2104919E-01 ,  0.2090621E-01 ,  0.2076516E-01 ,  0.2062602E-01 ,&
        &  0.2048873E-01 ,  0.2035320E-01 ,  0.2021949E-01 ,  0.2008744E-01 ,  0.1995726E-01 ,&
        &  0.1982857E-01 ,  0.1970166E-01 ,  0.1957640E-01 ,  0.1945260E-01 ,  0.1933043E-01 ,&
        &  0.1920979E-01 ,  0.1909065E-01 ,  0.1897296E-01 ,  0.1885672E-01 ,  0.1874185E-01 ,&
        &  0.1862841E-01 ,  0.1851630E-01 ,  0.1840553E-01 ,  0.1829616E-01 ,  0.1818798E-01 ,&
        &  0.1808118E-01 ,  0.1797552E-01 ,  0.1787115E-01 ,  0.1776802E-01 ,  0.1766606E-01 ,&
        &  0.1756512E-01 ,  0.1746552E-01 ,  0.1736690E-01 ,  0.1726953E-01 ,  0.1717324E-01 ,&
        &  0.1707787E-01 ,  0.1698360E-01 ,  0.1689041E-01 ,  0.1679834E-01 ,  0.1670716E-01 ,&
        &  0.1661682E-01 ,  0.1652775E-01 ,  0.1643946E-01 ,  0.1635216E-01 ,  0.1626571E-01 ,&
        &  0.1618020E-01 ,  0.1609552E-01 ,  0.1601169E-01 ,  0.1592885E-01 ,  0.1584702E-01 ,&
        &  0.1576579E-01 ,  0.1568534E-01 ,  0.1560587E-01 ,  0.1552728E-01 ,  0.1544913E-01 /)
      extice4(:, 28) = (/ &
        &  0.3275436E+01 ,  0.1633360E+01 ,  0.1077385E+01 ,  0.8017917E+00 ,  0.6382249E+00 ,&
        &  0.5298043E+00 ,  0.4526385E+00 ,  0.3949714E+00 ,  0.3502861E+00 ,  0.3146610E+00 ,&
        &  0.2856088E+00 ,  0.2614656E+00 ,  0.2410873E+00 ,  0.2236571E+00 ,  0.2085771E+00 ,&
        &  0.1954028E+00 ,  0.1837920E+00 ,  0.1734900E+00 ,  0.1642676E+00 ,  0.1559796E+00 ,&
        &  0.1484856E+00 ,  0.1416817E+00 ,  0.1354685E+00 ,  0.1297719E+00 ,  0.1245402E+00 ,&
        &  0.1197103E+00 ,  0.1152399E+00 ,  0.1110885E+00 ,  0.1072266E+00 ,  0.1036247E+00 ,&
        &  0.1002581E+00 ,  0.9710101E-01 ,  0.9413552E-01 ,  0.9134683E-01 ,  0.8872025E-01 ,&
        &  0.8623767E-01 ,  0.8389272E-01 ,  0.8166981E-01 ,  0.7956205E-01 ,  0.7756115E-01 ,&
        &  0.7565861E-01 ,  0.7384720E-01 ,  0.7211972E-01 ,  0.7047174E-01 ,  0.6889849E-01 ,&
        &  0.6739190E-01 ,  0.6595072E-01 ,  0.6457011E-01 ,  0.6324603E-01 ,  0.6197523E-01 ,&
        &  0.6075416E-01 ,  0.5958087E-01 ,  0.5845147E-01 ,  0.5736447E-01 ,  0.5631685E-01 ,&
        &  0.5530781E-01 ,  0.5433333E-01 ,  0.5339199E-01 ,  0.5248414E-01 ,  0.5160556E-01 ,&
        &  0.5075631E-01 ,  0.4993473E-01 ,  0.4913910E-01 ,  0.4836803E-01 ,  0.4762113E-01 ,&
        &  0.4689680E-01 ,  0.4619492E-01 ,  0.4551271E-01 ,  0.4485068E-01 ,  0.4420787E-01 ,&
        &  0.4358321E-01 ,  0.4297576E-01 ,  0.4238499E-01 ,  0.4181058E-01 ,  0.4125108E-01 ,&
        &  0.4070684E-01 ,  0.4017610E-01 ,  0.3965973E-01 ,  0.3915585E-01 ,  0.3866508E-01 ,&
        &  0.3818625E-01 ,  0.3771917E-01 ,  0.3726296E-01 ,  0.3681816E-01 ,  0.3638363E-01 ,&
        &  0.3595962E-01 ,  0.3554478E-01 ,  0.3513996E-01 ,  0.3474377E-01 ,  0.3435650E-01 ,&
        &  0.3397796E-01 ,  0.3360755E-01 ,  0.3324522E-01 ,  0.3289051E-01 ,  0.3254320E-01 ,&
        &  0.3220303E-01 ,  0.3187010E-01 ,  0.3154391E-01 ,  0.3122459E-01 ,  0.3091121E-01 ,&
        &  0.3060430E-01 ,  0.3030341E-01 ,  0.3000847E-01 ,  0.2971921E-01 ,  0.2943521E-01 ,&
        &  0.2915677E-01 ,  0.2888344E-01 ,  0.2861525E-01 ,  0.2835190E-01 ,  0.2809349E-01 ,&
        &  0.2783972E-01 ,  0.2759026E-01 ,  0.2734545E-01 ,  0.2710481E-01 ,  0.2686857E-01 ,&
        &  0.2663619E-01 ,  0.2640780E-01 ,  0.2618333E-01 ,  0.2596264E-01 ,  0.2574564E-01 ,&
        &  0.2553223E-01 ,  0.2532236E-01 ,  0.2511581E-01 ,  0.2491263E-01 ,  0.2471270E-01 ,&
        &  0.2451599E-01 ,  0.2432245E-01 ,  0.2413183E-01 ,  0.2394413E-01 ,  0.2375940E-01 ,&
        &  0.2357741E-01 ,  0.2339834E-01 ,  0.2322184E-01 ,  0.2304797E-01 ,  0.2287675E-01 ,&
        &  0.2270800E-01 ,  0.2254176E-01 ,  0.2237784E-01 ,  0.2221641E-01 ,  0.2205722E-01 ,&
        &  0.2190030E-01 ,  0.2174565E-01 ,  0.2159306E-01 ,  0.2144266E-01 ,  0.2129444E-01 ,&
        &  0.2114816E-01 ,  0.2100379E-01 ,  0.2086143E-01 ,  0.2072099E-01 ,  0.2058245E-01 ,&
        &  0.2044576E-01 ,  0.2031082E-01 ,  0.2017769E-01 ,  0.2004611E-01 ,  0.1991650E-01 ,&
        &  0.1978836E-01 ,  0.1966201E-01 ,  0.1953719E-01 ,  0.1941393E-01 ,  0.1929219E-01 ,&
        &  0.1917208E-01 ,  0.1905337E-01 ,  0.1893619E-01 ,  0.1882036E-01 ,  0.1870599E-01 ,&
        &  0.1859296E-01 ,  0.1848133E-01 ,  0.1837096E-01 ,  0.1826197E-01 ,  0.1815427E-01 ,&
        &  0.1804775E-01 ,  0.1794256E-01 ,  0.1783856E-01 ,  0.1773579E-01 ,  0.1763419E-01 ,&
        &  0.1753361E-01 ,  0.1743437E-01 ,  0.1733609E-01 ,  0.1723906E-01 ,  0.1714303E-01 ,&
        &  0.1704799E-01 ,  0.1695415E-01 ,  0.1686129E-01 ,  0.1676946E-01 ,  0.1667860E-01 ,&
        &  0.1658849E-01 ,  0.1649975E-01 ,  0.1641177E-01 ,  0.1632478E-01 ,  0.1623856E-01 ,&
        &  0.1615335E-01 ,  0.1606889E-01 ,  0.1598528E-01 ,  0.1590281E-01 ,  0.1582120E-01 ,&
        &  0.1574025E-01 ,  0.1565994E-01 ,  0.1558074E-01 ,  0.1550236E-01 ,  0.1542449E-01 /)
      extice4(:, 29) = (/ &
        &  0.1908497E+00 ,  0.3805395E+00 ,  0.4949888E+00 ,  0.5340181E+00 ,  0.5274293E+00 ,&
        &  0.4988388E+00 ,  0.4617307E+00 ,  0.4229380E+00 ,  0.3856946E+00 ,  0.3514130E+00 ,&
        &  0.3206082E+00 ,  0.2933168E+00 ,  0.2693392E+00 ,  0.2483671E+00 ,  0.2300456E+00 ,&
        &  0.2140234E+00 ,  0.1999723E+00 ,  0.1876091E+00 ,  0.1766584E+00 ,  0.1669276E+00 ,&
        &  0.1582281E+00 ,  0.1504157E+00 ,  0.1433538E+00 ,  0.1369384E+00 ,  0.1310959E+00 ,&
        &  0.1257422E+00 ,  0.1208190E+00 ,  0.1162741E+00 ,  0.1120671E+00 ,  0.1081608E+00 ,&
        &  0.1045239E+00 ,  0.1011248E+00 ,  0.9794132E-01 ,  0.9495574E-01 ,  0.9215017E-01 ,&
        &  0.8950404E-01 ,  0.8700949E-01 ,  0.8464843E-01 ,  0.8241317E-01 ,  0.8029388E-01 ,&
        &  0.7828099E-01 ,  0.7636706E-01 ,  0.7454397E-01 ,  0.7280648E-01 ,  0.7114913E-01 ,&
        &  0.6956335E-01 ,  0.6804769E-01 ,  0.6659668E-01 ,  0.6520603E-01 ,  0.6387223E-01 ,&
        &  0.6259155E-01 ,  0.6136125E-01 ,  0.6017844E-01 ,  0.5904000E-01 ,  0.5794365E-01 ,&
        &  0.5688820E-01 ,  0.5586947E-01 ,  0.5488591E-01 ,  0.5393731E-01 ,  0.5302008E-01 ,&
        &  0.5213371E-01 ,  0.5127672E-01 ,  0.5044680E-01 ,  0.4964297E-01 ,  0.4886457E-01 ,&
        &  0.4811016E-01 ,  0.4737912E-01 ,  0.4666880E-01 ,  0.4597994E-01 ,  0.4531129E-01 ,&
        &  0.4466152E-01 ,  0.4402987E-01 ,  0.4341577E-01 ,  0.4281909E-01 ,  0.4223768E-01 ,&
        &  0.4167234E-01 ,  0.4112143E-01 ,  0.4058543E-01 ,  0.4006259E-01 ,  0.3955334E-01 ,&
        &  0.3905668E-01 ,  0.3857239E-01 ,  0.3809957E-01 ,  0.3763837E-01 ,  0.3718839E-01 ,&
        &  0.3674909E-01 ,  0.3631949E-01 ,  0.3590008E-01 ,  0.3549014E-01 ,  0.3508926E-01 ,&
        &  0.3469757E-01 ,  0.3431446E-01 ,  0.3393972E-01 ,  0.3357286E-01 ,  0.3321380E-01 ,&
        &  0.3286230E-01 ,  0.3251828E-01 ,  0.3218121E-01 ,  0.3185124E-01 ,  0.3152774E-01 ,&
        &  0.3121090E-01 ,  0.3090027E-01 ,  0.3059579E-01 ,  0.3029733E-01 ,  0.3000443E-01 ,&
        &  0.2971712E-01 ,  0.2943538E-01 ,  0.2915878E-01 ,  0.2888734E-01 ,  0.2862097E-01 ,&
        &  0.2835938E-01 ,  0.2810253E-01 ,  0.2785031E-01 ,  0.2760240E-01 ,  0.2735914E-01 ,&
        &  0.2711987E-01 ,  0.2688484E-01 ,  0.2665384E-01 ,  0.2642673E-01 ,  0.2620341E-01 ,&
        &  0.2598392E-01 ,  0.2576808E-01 ,  0.2555564E-01 ,  0.2534667E-01 ,  0.2514116E-01 ,&
        &  0.2493897E-01 ,  0.2473992E-01 ,  0.2454410E-01 ,  0.2435128E-01 ,  0.2416152E-01 ,&
        &  0.2397469E-01 ,  0.2379063E-01 ,  0.2360943E-01 ,  0.2343094E-01 ,  0.2325517E-01 ,&
        &  0.2308194E-01 ,  0.2291127E-01 ,  0.2274310E-01 ,  0.2257738E-01 ,  0.2241418E-01 ,&
        &  0.2225309E-01 ,  0.2209443E-01 ,  0.2193799E-01 ,  0.2178381E-01 ,  0.2163175E-01 ,&
        &  0.2148178E-01 ,  0.2133387E-01 ,  0.2118793E-01 ,  0.2104395E-01 ,  0.2090202E-01 ,&
        &  0.2076188E-01 ,  0.2062364E-01 ,  0.2048726E-01 ,  0.2035246E-01 ,  0.2021968E-01 ,&
        &  0.2008851E-01 ,  0.1995897E-01 ,  0.1983119E-01 ,  0.1970501E-01 ,  0.1958039E-01 ,&
        &  0.1945734E-01 ,  0.1933591E-01 ,  0.1921586E-01 ,  0.1909739E-01 ,  0.1898031E-01 ,&
        &  0.1886469E-01 ,  0.1875042E-01 ,  0.1863753E-01 ,  0.1852596E-01 ,  0.1841579E-01 ,&
        &  0.1830684E-01 ,  0.1819925E-01 ,  0.1809287E-01 ,  0.1798775E-01 ,  0.1788383E-01 ,&
        &  0.1778104E-01 ,  0.1767953E-01 ,  0.1757909E-01 ,  0.1747984E-01 ,  0.1738170E-01 ,&
        &  0.1728458E-01 ,  0.1718858E-01 ,  0.1709368E-01 ,  0.1699984E-01 ,  0.1690698E-01 ,&
        &  0.1681490E-01 ,  0.1672428E-01 ,  0.1663429E-01 ,  0.1654547E-01 ,  0.1645735E-01 ,&
        &  0.1637027E-01 ,  0.1628403E-01 ,  0.1619866E-01 ,  0.1611438E-01 ,  0.1603106E-01 ,&
        &  0.1594833E-01 ,  0.1586641E-01 ,  0.1578547E-01 ,  0.1570551E-01 ,  0.1562593E-01 /)
      ssaice4(:, 16) = (/ &
        &  0.5427927E+00 ,  0.6971002E+00 ,  0.7333586E+00 ,  0.7383546E+00 ,  0.7319084E+00 ,&
        &  0.7207797E+00 ,  0.7079853E+00 ,  0.6951566E+00 ,  0.6832373E+00 ,  0.6727153E+00 ,&
        &  0.6637468E+00 ,  0.6562530E+00 ,  0.6500300E+00 ,  0.6448258E+00 ,  0.6404006E+00 ,&
        &  0.6365653E+00 ,  0.6331491E+00 ,  0.6300548E+00 ,  0.6271985E+00 ,  0.6245189E+00 ,&
        &  0.6219917E+00 ,  0.6195887E+00 ,  0.6172876E+00 ,  0.6150829E+00 ,  0.6129715E+00 ,&
        &  0.6109371E+00 ,  0.6089858E+00 ,  0.6071011E+00 ,  0.6052884E+00 ,  0.6035461E+00 ,&
        &  0.6018651E+00 ,  0.6002488E+00 ,  0.5986898E+00 ,  0.5971789E+00 ,  0.5957298E+00 ,&
        &  0.5943226E+00 ,  0.5929702E+00 ,  0.5916611E+00 ,  0.5903956E+00 ,  0.5891690E+00 ,&
        &  0.5879845E+00 ,  0.5868421E+00 ,  0.5857351E+00 ,  0.5846582E+00 ,  0.5836151E+00 ,&
        &  0.5826049E+00 ,  0.5816239E+00 ,  0.5806742E+00 ,  0.5797538E+00 ,  0.5788579E+00 ,&
        &  0.5779887E+00 ,  0.5771471E+00 ,  0.5763254E+00 ,  0.5755286E+00 ,  0.5747546E+00 ,&
        &  0.5739959E+00 ,  0.5732650E+00 ,  0.5725522E+00 ,  0.5718549E+00 ,  0.5711786E+00 ,&
        &  0.5705226E+00 ,  0.5698780E+00 ,  0.5692566E+00 ,  0.5686439E+00 ,  0.5680524E+00 ,&
        &  0.5674744E+00 ,  0.5669100E+00 ,  0.5663620E+00 ,  0.5658256E+00 ,  0.5653028E+00 ,&
        &  0.5647966E+00 ,  0.5642970E+00 ,  0.5638112E+00 ,  0.5633370E+00 ,  0.5628745E+00 ,&
        &  0.5624236E+00 ,  0.5619797E+00 ,  0.5615494E+00 ,  0.5611288E+00 ,  0.5607150E+00 ,&
        &  0.5603102E+00 ,  0.5599151E+00 ,  0.5595317E+00 ,  0.5591579E+00 ,  0.5587882E+00 ,&
        &  0.5584304E+00 ,  0.5580773E+00 ,  0.5577312E+00 ,  0.5573969E+00 ,  0.5570673E+00 ,&
        &  0.5567420E+00 ,  0.5564291E+00 ,  0.5561155E+00 ,  0.5558165E+00 ,  0.5555196E+00 ,&
        &  0.5552275E+00 ,  0.5549424E+00 ,  0.5546643E+00 ,  0.5543909E+00 ,  0.5541197E+00 ,&
        &  0.5538582E+00 ,  0.5536038E+00 ,  0.5533493E+00 ,  0.5531017E+00 ,  0.5528640E+00 ,&
        &  0.5526261E+00 ,  0.5523927E+00 ,  0.5521618E+00 ,  0.5519380E+00 ,  0.5517191E+00 ,&
        &  0.5515022E+00 ,  0.5512925E+00 ,  0.5510854E+00 ,  0.5508805E+00 ,  0.5506777E+00 ,&
        &  0.5504875E+00 ,  0.5502945E+00 ,  0.5501035E+00 ,  0.5499176E+00 ,  0.5497364E+00 ,&
        &  0.5495553E+00 ,  0.5493813E+00 ,  0.5492072E+00 ,  0.5490379E+00 ,  0.5488709E+00 ,&
        &  0.5487088E+00 ,  0.5485516E+00 ,  0.5483915E+00 ,  0.5482364E+00 ,  0.5480813E+00 ,&
        &  0.5479360E+00 ,  0.5477880E+00 ,  0.5476399E+00 ,  0.5475017E+00 ,  0.5473635E+00 ,&
        &  0.5472224E+00 ,  0.5470864E+00 ,  0.5469552E+00 ,  0.5468240E+00 ,  0.5466950E+00 ,&
        &  0.5465736E+00 ,  0.5464495E+00 ,  0.5463227E+00 ,  0.5462035E+00 ,  0.5460865E+00 ,&
        &  0.5459722E+00 ,  0.5458551E+00 ,  0.5457403E+00 ,  0.5456331E+00 ,  0.5455259E+00 ,&
        &  0.5454159E+00 ,  0.5453109E+00 ,  0.5452059E+00 ,  0.5451058E+00 ,  0.5450030E+00 ,&
        &  0.5449078E+00 ,  0.5448099E+00 ,  0.5447119E+00 ,  0.5446190E+00 ,  0.5445259E+00 ,&
        &  0.5444351E+00 ,  0.5443470E+00 ,  0.5442563E+00 ,  0.5441654E+00 ,  0.5440795E+00 ,&
        &  0.5439986E+00 ,  0.5439127E+00 ,  0.5438290E+00 ,  0.5437530E+00 ,  0.5436693E+00 ,&
        &  0.5435932E+00 ,  0.5435145E+00 ,  0.5434406E+00 ,  0.5433641E+00 ,  0.5432903E+00 ,&
        &  0.5432164E+00 ,  0.5431448E+00 ,  0.5430759E+00 ,  0.5430042E+00 ,  0.5429376E+00 ,&
        &  0.5428659E+00 ,  0.5428041E+00 ,  0.5427374E+00 ,  0.5426680E+00 ,  0.5426062E+00 ,&
        &  0.5425444E+00 ,  0.5424800E+00 ,  0.5424204E+00 ,  0.5423609E+00 ,  0.5423014E+00 ,&
        &  0.5422418E+00 ,  0.5421822E+00 ,  0.5421276E+00 ,  0.5420730E+00 ,  0.5420157E+00 ,&
        &  0.5419610E+00 ,  0.5419037E+00 ,  0.5418540E+00 ,  0.5418016E+00 ,  0.5417492E+00 /)
      ssaice4(:, 17) = (/ &
        &  0.3538244E+00 ,  0.5912308E+00 ,  0.6984357E+00 ,  0.7494444E+00 ,  0.7754027E+00 ,&
        &  0.7890396E+00 ,  0.7960014E+00 ,  0.7990188E+00 ,  0.7995591E+00 ,  0.7985084E+00 ,&
        &  0.7964439E+00 ,  0.7937754E+00 ,  0.7907903E+00 ,  0.7877069E+00 ,  0.7846487E+00 ,&
        &  0.7817183E+00 ,  0.7789533E+00 ,  0.7763775E+00 ,  0.7739809E+00 ,  0.7717634E+00 ,&
        &  0.7696950E+00 ,  0.7677569E+00 ,  0.7659325E+00 ,  0.7641926E+00 ,  0.7625340E+00 ,&
        &  0.7609313E+00 ,  0.7593802E+00 ,  0.7578622E+00 ,  0.7563848E+00 ,  0.7549369E+00 ,&
        &  0.7535120E+00 ,  0.7521076E+00 ,  0.7507209E+00 ,  0.7493593E+00 ,  0.7480114E+00 ,&
        &  0.7466858E+00 ,  0.7453751E+00 ,  0.7440841E+00 ,  0.7428102E+00 ,  0.7415573E+00 ,&
        &  0.7403191E+00 ,  0.7391030E+00 ,  0.7378979E+00 ,  0.7367126E+00 ,  0.7355521E+00 ,&
        &  0.7343989E+00 ,  0.7332705E+00 ,  0.7321546E+00 ,  0.7310548E+00 ,  0.7299726E+00 ,&
        &  0.7289065E+00 ,  0.7278568E+00 ,  0.7268195E+00 ,  0.7257950E+00 ,  0.7247853E+00 ,&
        &  0.7237934E+00 ,  0.7228078E+00 ,  0.7218348E+00 ,  0.7208819E+00 ,  0.7199354E+00 ,&
        &  0.7190017E+00 ,  0.7180779E+00 ,  0.7171691E+00 ,  0.7162632E+00 ,  0.7153723E+00 ,&
        &  0.7144914E+00 ,  0.7136134E+00 ,  0.7127540E+00 ,  0.7118961E+00 ,  0.7110531E+00 ,&
        &  0.7102153E+00 ,  0.7093838E+00 ,  0.7085624E+00 ,  0.7077512E+00 ,  0.7069448E+00 ,&
        &  0.7061486E+00 ,  0.7053573E+00 ,  0.7045711E+00 ,  0.7037950E+00 ,  0.7030238E+00 ,&
        &  0.7022592E+00 ,  0.7015031E+00 ,  0.7007520E+00 ,  0.7000060E+00 ,  0.6992700E+00 ,&
        &  0.6985341E+00 ,  0.6978081E+00 ,  0.6970872E+00 ,  0.6963713E+00 ,  0.6956639E+00 ,&
        &  0.6949581E+00 ,  0.6942573E+00 ,  0.6935650E+00 ,  0.6928777E+00 ,  0.6921869E+00 ,&
        &  0.6915097E+00 ,  0.6908374E+00 ,  0.6901702E+00 ,  0.6895080E+00 ,  0.6888458E+00 ,&
        &  0.6881936E+00 ,  0.6875414E+00 ,  0.6868977E+00 ,  0.6862555E+00 ,  0.6856184E+00 ,&
        &  0.6849897E+00 ,  0.6843626E+00 ,  0.6837389E+00 ,  0.6831169E+00 ,  0.6825082E+00 ,&
        &  0.6818911E+00 ,  0.6812875E+00 ,  0.6806889E+00 ,  0.6800869E+00 ,  0.6794933E+00 ,&
        &  0.6789047E+00 ,  0.6783161E+00 ,  0.6777341E+00 ,  0.6771556E+00 ,  0.6765770E+00 ,&
        &  0.6760084E+00 ,  0.6754399E+00 ,  0.6748763E+00 ,  0.6743178E+00 ,  0.6737593E+00 ,&
        &  0.6732023E+00 ,  0.6726539E+00 ,  0.6721103E+00 ,  0.6715668E+00 ,  0.6710283E+00 ,&
        &  0.6704898E+00 ,  0.6699563E+00 ,  0.6694279E+00 ,  0.6689044E+00 ,  0.6683809E+00 ,&
        &  0.6678591E+00 ,  0.6673407E+00 ,  0.6668272E+00 ,  0.6663188E+00 ,  0.6658103E+00 ,&
        &  0.6653069E+00 ,  0.6648035E+00 ,  0.6643101E+00 ,  0.6638083E+00 ,  0.6633199E+00 ,&
        &  0.6628265E+00 ,  0.6623431E+00 ,  0.6618598E+00 ,  0.6613730E+00 ,  0.6608997E+00 ,&
        &  0.6604213E+00 ,  0.6599529E+00 ,  0.6594796E+00 ,  0.6590129E+00 ,  0.6585495E+00 ,&
        &  0.6580862E+00 ,  0.6576279E+00 ,  0.6571662E+00 ,  0.6567129E+00 ,  0.6562646E+00 ,&
        &  0.6558163E+00 ,  0.6553697E+00 ,  0.6549264E+00 ,  0.6544830E+00 ,  0.6540415E+00 ,&
        &  0.6536082E+00 ,  0.6531749E+00 ,  0.6527383E+00 ,  0.6523101E+00 ,  0.6518868E+00 ,&
        &  0.6514602E+00 ,  0.6510370E+00 ,  0.6506187E+00 ,  0.6501972E+00 ,  0.6497840E+00 ,&
        &  0.6493675E+00 ,  0.6489592E+00 ,  0.6485510E+00 ,  0.6481395E+00 ,  0.6477363E+00 ,&
        &  0.6473348E+00 ,  0.6469316E+00 ,  0.6465384E+00 ,  0.6461369E+00 ,  0.6457437E+00 ,&
        &  0.6453523E+00 ,  0.6449641E+00 ,  0.6445760E+00 ,  0.6441895E+00 ,  0.6438063E+00 ,&
        &  0.6434199E+00 ,  0.6430417E+00 ,  0.6426604E+00 ,  0.6422872E+00 ,  0.6419140E+00 ,&
        &  0.6415427E+00 ,  0.6411695E+00 ,  0.6408032E+00 ,  0.6404350E+00 ,  0.6400687E+00 /)
      ssaice4(:, 18) = (/ &
        &  0.9930021E+00 ,  0.9959481E+00 ,  0.9961996E+00 ,  0.9958317E+00 ,  0.9951802E+00 ,&
        &  0.9943420E+00 ,  0.9933667E+00 ,  0.9922835E+00 ,  0.9911140E+00 ,  0.9898951E+00 ,&
        &  0.9886724E+00 ,  0.9874651E+00 ,  0.9862889E+00 ,  0.9851565E+00 ,  0.9840630E+00 ,&
        &  0.9830032E+00 ,  0.9819655E+00 ,  0.9809526E+00 ,  0.9799491E+00 ,  0.9789602E+00 ,&
        &  0.9779686E+00 ,  0.9769863E+00 ,  0.9759980E+00 ,  0.9750171E+00 ,  0.9740338E+00 ,&
        &  0.9730529E+00 ,  0.9720745E+00 ,  0.9710941E+00 ,  0.9701164E+00 ,  0.9691421E+00 ,&
        &  0.9681664E+00 ,  0.9671990E+00 ,  0.9662259E+00 ,  0.9652617E+00 ,  0.9642968E+00 ,&
        &  0.9633360E+00 ,  0.9623750E+00 ,  0.9614183E+00 ,  0.9604660E+00 ,  0.9595230E+00 ,&
        &  0.9585704E+00 ,  0.9576270E+00 ,  0.9566931E+00 ,  0.9557542E+00 ,  0.9548200E+00 ,&
        &  0.9538903E+00 ,  0.9529606E+00 ,  0.9520355E+00 ,  0.9511152E+00 ,  0.9501995E+00 ,&
        &  0.9492790E+00 ,  0.9483727E+00 ,  0.9474616E+00 ,  0.9465553E+00 ,  0.9456488E+00 ,&
        &  0.9447472E+00 ,  0.9438550E+00 ,  0.9429582E+00 ,  0.9420707E+00 ,  0.9411785E+00 ,&
        &  0.9402959E+00 ,  0.9394134E+00 ,  0.9385355E+00 ,  0.9376575E+00 ,  0.9367846E+00 ,&
        &  0.9359164E+00 ,  0.9350484E+00 ,  0.9341803E+00 ,  0.9333219E+00 ,  0.9324634E+00 ,&
        &  0.9316098E+00 ,  0.9307565E+00 ,  0.9299077E+00 ,  0.9290592E+00 ,  0.9282202E+00 ,&
        &  0.9273767E+00 ,  0.9265426E+00 ,  0.9257041E+00 ,  0.9248701E+00 ,  0.9240460E+00 ,&
        &  0.9232171E+00 ,  0.9223932E+00 ,  0.9215739E+00 ,  0.9207551E+00 ,  0.9199361E+00 ,&
        &  0.9191266E+00 ,  0.9183127E+00 ,  0.9175085E+00 ,  0.9167089E+00 ,  0.9159001E+00 ,&
        &  0.9151053E+00 ,  0.9143062E+00 ,  0.9135119E+00 ,  0.9127222E+00 ,  0.9119328E+00 ,&
        &  0.9111485E+00 ,  0.9103640E+00 ,  0.9095843E+00 ,  0.9088048E+00 ,  0.9080300E+00 ,&
        &  0.9072555E+00 ,  0.9064857E+00 ,  0.9057162E+00 ,  0.9049515E+00 ,  0.9041916E+00 ,&
        &  0.9034269E+00 ,  0.9026721E+00 ,  0.9019170E+00 ,  0.9011623E+00 ,  0.9004123E+00 ,&
        &  0.8996621E+00 ,  0.8989217E+00 ,  0.8981767E+00 ,  0.8974321E+00 ,  0.8966967E+00 ,&
        &  0.8959567E+00 ,  0.8952215E+00 ,  0.8944911E+00 ,  0.8937611E+00 ,  0.8930354E+00 ,&
        &  0.8923100E+00 ,  0.8915895E+00 ,  0.8908694E+00 ,  0.8901492E+00 ,  0.8894382E+00 ,&
        &  0.8887182E+00 ,  0.8880075E+00 ,  0.8873016E+00 ,  0.8865912E+00 ,  0.8858906E+00 ,&
        &  0.8851849E+00 ,  0.8844890E+00 ,  0.8837886E+00 ,  0.8830881E+00 ,  0.8823968E+00 ,&
        &  0.8817060E+00 ,  0.8810157E+00 ,  0.8803296E+00 ,  0.8796434E+00 ,  0.8789577E+00 ,&
        &  0.8782769E+00 ,  0.8775960E+00 ,  0.8769200E+00 ,  0.8762487E+00 ,  0.8755731E+00 ,&
        &  0.8749067E+00 ,  0.8742357E+00 ,  0.8735648E+00 ,  0.8729036E+00 ,  0.8722375E+00 ,&
        &  0.8715811E+00 ,  0.8709204E+00 ,  0.8702638E+00 ,  0.8696078E+00 ,  0.8689517E+00 ,&
        &  0.8683006E+00 ,  0.8676543E+00 ,  0.8670079E+00 ,  0.8663614E+00 ,  0.8657155E+00 ,&
        &  0.8650788E+00 ,  0.8644378E+00 ,  0.8638015E+00 ,  0.8631696E+00 ,  0.8625332E+00 ,&
        &  0.8618975E+00 ,  0.8612710E+00 ,  0.8606443E+00 ,  0.8600177E+00 ,  0.8593916E+00 ,&
        &  0.8587654E+00 ,  0.8581485E+00 ,  0.8575321E+00 ,  0.8569107E+00 ,  0.8562992E+00 ,&
        &  0.8556827E+00 ,  0.8550760E+00 ,  0.8544601E+00 ,  0.8538532E+00 ,  0.8532471E+00 ,&
        &  0.8526452E+00 ,  0.8520389E+00 ,  0.8514376E+00 ,  0.8508362E+00 ,  0.8502396E+00 ,&
        &  0.8496431E+00 ,  0.8490515E+00 ,  0.8484598E+00 ,  0.8478730E+00 ,  0.8472770E+00 ,&
        &  0.8466950E+00 ,  0.8461039E+00 ,  0.8455219E+00 ,  0.8449405E+00 ,  0.8443592E+00 ,&
        &  0.8437777E+00 ,  0.8432012E+00 ,  0.8426247E+00 ,  0.8420531E+00 ,  0.8414814E+00 /)
      ssaice4(:, 19) = (/ &
        &  0.9862202E+00 ,  0.9909001E+00 ,  0.9905429E+00 ,  0.9888517E+00 ,  0.9863635E+00 ,&
        &  0.9833159E+00 ,  0.9798765E+00 ,  0.9762177E+00 ,  0.9725327E+00 ,  0.9689515E+00 ,&
        &  0.9655472E+00 ,  0.9623340E+00 ,  0.9592851E+00 ,  0.9563512E+00 ,  0.9535028E+00 ,&
        &  0.9507144E+00 ,  0.9479554E+00 ,  0.9452255E+00 ,  0.9425196E+00 ,  0.9398363E+00 ,&
        &  0.9371663E+00 ,  0.9345200E+00 ,  0.9318942E+00 ,  0.9292901E+00 ,  0.9267142E+00 ,&
        &  0.9241583E+00 ,  0.9216276E+00 ,  0.9191236E+00 ,  0.9166418E+00 ,  0.9141877E+00 ,&
        &  0.9117616E+00 ,  0.9093587E+00 ,  0.9069749E+00 ,  0.9046244E+00 ,  0.9022933E+00 ,&
        &  0.8999860E+00 ,  0.8977076E+00 ,  0.8954483E+00 ,  0.8932135E+00 ,  0.8909982E+00 ,&
        &  0.8888074E+00 ,  0.8866358E+00 ,  0.8844886E+00 ,  0.8823609E+00 ,  0.8802579E+00 ,&
        &  0.8781742E+00 ,  0.8761098E+00 ,  0.8740657E+00 ,  0.8720406E+00 ,  0.8700359E+00 ,&
        &  0.8680505E+00 ,  0.8660843E+00 ,  0.8641334E+00 ,  0.8622069E+00 ,  0.8602908E+00 ,&
        &  0.8584036E+00 ,  0.8565311E+00 ,  0.8546736E+00 ,  0.8528311E+00 ,  0.8510085E+00 ,&
        &  0.8492053E+00 ,  0.8474122E+00 ,  0.8456393E+00 ,  0.8438802E+00 ,  0.8421413E+00 ,&
        &  0.8404130E+00 ,  0.8387083E+00 ,  0.8370141E+00 ,  0.8353347E+00 ,  0.8336701E+00 ,&
        &  0.8320203E+00 ,  0.8303853E+00 ,  0.8287652E+00 ,  0.8271600E+00 ,  0.8255738E+00 ,&
        &  0.8239936E+00 ,  0.8224284E+00 ,  0.8208823E+00 ,  0.8193414E+00 ,  0.8178204E+00 ,&
        &  0.8163088E+00 ,  0.8148082E+00 ,  0.8133220E+00 ,  0.8118500E+00 ,  0.8103932E+00 ,&
        &  0.8089467E+00 ,  0.8075095E+00 ,  0.8060827E+00 ,  0.8046712E+00 ,  0.8032691E+00 ,&
        &  0.8018824E+00 ,  0.8005052E+00 ,  0.7991334E+00 ,  0.7977809E+00 ,  0.7964379E+00 ,&
        &  0.7951015E+00 ,  0.7937786E+00 ,  0.7924661E+00 ,  0.7911642E+00 ,  0.7898757E+00 ,&
        &  0.7885891E+00 ,  0.7873168E+00 ,  0.7860541E+00 ,  0.7848060E+00 ,  0.7835635E+00 ,&
        &  0.7823355E+00 ,  0.7811083E+00 ,  0.7798957E+00 ,  0.7786927E+00 ,  0.7774954E+00 ,&
        &  0.7763127E+00 ,  0.7751347E+00 ,  0.7739674E+00 ,  0.7728099E+00 ,  0.7716569E+00 ,&
        &  0.7705186E+00 ,  0.7693861E+00 ,  0.7682583E+00 ,  0.7671414E+00 ,  0.7660379E+00 ,&
        &  0.7649354E+00 ,  0.7638426E+00 ,  0.7627606E+00 ,  0.7616833E+00 ,  0.7606196E+00 ,&
        &  0.7595531E+00 ,  0.7585000E+00 ,  0.7574579E+00 ,  0.7564193E+00 ,  0.7553879E+00 ,&
        &  0.7543651E+00 ,  0.7533519E+00 ,  0.7523448E+00 ,  0.7513425E+00 ,  0.7503449E+00 ,&
        &  0.7493621E+00 ,  0.7483803E+00 ,  0.7474071E+00 ,  0.7464399E+00 ,  0.7454775E+00 ,&
        &  0.7445248E+00 ,  0.7435783E+00 ,  0.7426403E+00 ,  0.7417034E+00 ,  0.7407763E+00 ,&
        &  0.7398540E+00 ,  0.7389415E+00 ,  0.7380338E+00 ,  0.7371310E+00 ,  0.7362343E+00 ,&
        &  0.7353424E+00 ,  0.7344591E+00 ,  0.7335818E+00 ,  0.7327009E+00 ,  0.7318383E+00 ,&
        &  0.7309757E+00 ,  0.7301192E+00 ,  0.7292712E+00 ,  0.7284244E+00 ,  0.7275875E+00 ,&
        &  0.7267554E+00 ,  0.7259232E+00 ,  0.7251008E+00 ,  0.7242882E+00 ,  0.7234721E+00 ,&
        &  0.7226692E+00 ,  0.7218627E+00 ,  0.7210661E+00 ,  0.7202743E+00 ,  0.7194874E+00 ,&
        &  0.7187054E+00 ,  0.7179282E+00 ,  0.7171609E+00 ,  0.7163936E+00 ,  0.7156276E+00 ,&
        &  0.7148749E+00 ,  0.7141186E+00 ,  0.7133722E+00 ,  0.7126292E+00 ,  0.7118925E+00 ,&
        &  0.7111558E+00 ,  0.7104290E+00 ,  0.7097021E+00 ,  0.7089765E+00 ,  0.7082644E+00 ,&
        &  0.7075521E+00 ,  0.7068412E+00 ,  0.7061437E+00 ,  0.7054427E+00 ,  0.7047465E+00 ,&
        &  0.7040588E+00 ,  0.7033725E+00 ,  0.7026861E+00 ,  0.7020096E+00 ,  0.7013330E+00 ,&
        &  0.7006662E+00 ,  0.6999961E+00 ,  0.6993342E+00 ,  0.6986772E+00 ,  0.6980218E+00 /)
      ssaice4(:, 20) = (/ &
        &  0.9975056E+00 ,  0.9980743E+00 ,  0.9977447E+00 ,  0.9971491E+00 ,  0.9963710E+00 ,&
        &  0.9954761E+00 ,  0.9945168E+00 ,  0.9935507E+00 ,  0.9926266E+00 ,  0.9917478E+00 ,&
        &  0.9909037E+00 ,  0.9900827E+00 ,  0.9892793E+00 ,  0.9884819E+00 ,  0.9876776E+00 ,&
        &  0.9868787E+00 ,  0.9860801E+00 ,  0.9852775E+00 ,  0.9844809E+00 ,  0.9836723E+00 ,&
        &  0.9828754E+00 ,  0.9820723E+00 ,  0.9812726E+00 ,  0.9804766E+00 ,  0.9796847E+00 ,&
        &  0.9788876E+00 ,  0.9780993E+00 ,  0.9773107E+00 ,  0.9765217E+00 ,  0.9757418E+00 ,&
        &  0.9749618E+00 ,  0.9741766E+00 ,  0.9734054E+00 ,  0.9726292E+00 ,  0.9718576E+00 ,&
        &  0.9710856E+00 ,  0.9703135E+00 ,  0.9695553E+00 ,  0.9687874E+00 ,  0.9680242E+00 ,&
        &  0.9672701E+00 ,  0.9665161E+00 ,  0.9657618E+00 ,  0.9650121E+00 ,  0.9642576E+00 ,&
        &  0.9635125E+00 ,  0.9627672E+00 ,  0.9620219E+00 ,  0.9612862E+00 ,  0.9605502E+00 ,&
        &  0.9598143E+00 ,  0.9590783E+00 ,  0.9583471E+00 ,  0.9576158E+00 ,  0.9568940E+00 ,&
        &  0.9561723E+00 ,  0.9554458E+00 ,  0.9547288E+00 ,  0.9540071E+00 ,  0.9532949E+00 ,&
        &  0.9525827E+00 ,  0.9518706E+00 ,  0.9511585E+00 ,  0.9504558E+00 ,  0.9497486E+00 ,&
        &  0.9490461E+00 ,  0.9483485E+00 ,  0.9476507E+00 ,  0.9469531E+00 ,  0.9462650E+00 ,&
        &  0.9455723E+00 ,  0.9448843E+00 ,  0.9441916E+00 ,  0.9435084E+00 ,  0.9428253E+00 ,&
        &  0.9421422E+00 ,  0.9414639E+00 ,  0.9407906E+00 ,  0.9401076E+00 ,  0.9394391E+00 ,&
        &  0.9387657E+00 ,  0.9380972E+00 ,  0.9374287E+00 ,  0.9367652E+00 ,  0.9361017E+00 ,&
        &  0.9354428E+00 ,  0.9347794E+00 ,  0.9341207E+00 ,  0.9334666E+00 ,  0.9328129E+00 ,&
        &  0.9321590E+00 ,  0.9315102E+00 ,  0.9308565E+00 ,  0.9302124E+00 ,  0.9295681E+00 ,&
        &  0.9289243E+00 ,  0.9282851E+00 ,  0.9276458E+00 ,  0.9270067E+00 ,  0.9263726E+00 ,&
        &  0.9257383E+00 ,  0.9251042E+00 ,  0.9244748E+00 ,  0.9238408E+00 ,  0.9232163E+00 ,&
        &  0.9225917E+00 ,  0.9219673E+00 ,  0.9213432E+00 ,  0.9207238E+00 ,  0.9201042E+00 ,&
        &  0.9194894E+00 ,  0.9188748E+00 ,  0.9182605E+00 ,  0.9176460E+00 ,  0.9170363E+00 ,&
        &  0.9164265E+00 ,  0.9158219E+00 ,  0.9152126E+00 ,  0.9146076E+00 ,  0.9140079E+00 ,&
        &  0.9134080E+00 ,  0.9128036E+00 ,  0.9122084E+00 ,  0.9116135E+00 ,  0.9110190E+00 ,&
        &  0.9104243E+00 ,  0.9098340E+00 ,  0.9092445E+00 ,  0.9086544E+00 ,  0.9080696E+00 ,&
        &  0.9074796E+00 ,  0.9068995E+00 ,  0.9063193E+00 ,  0.9057345E+00 ,  0.9051594E+00 ,&
        &  0.9045793E+00 ,  0.9039996E+00 ,  0.9034292E+00 ,  0.9028542E+00 ,  0.9022840E+00 ,&
        &  0.9017138E+00 ,  0.9011390E+00 ,  0.9005784E+00 ,  0.9000084E+00 ,  0.8994432E+00 ,&
        &  0.8988828E+00 ,  0.8983179E+00 ,  0.8977624E+00 ,  0.8971973E+00 ,  0.8966421E+00 ,&
        &  0.8960868E+00 ,  0.8955313E+00 ,  0.8949763E+00 ,  0.8944257E+00 ,  0.8938755E+00 ,&
        &  0.8933252E+00 ,  0.8927748E+00 ,  0.8922293E+00 ,  0.8916838E+00 ,  0.8911386E+00 ,&
        &  0.8905978E+00 ,  0.8900575E+00 ,  0.8895171E+00 ,  0.8889767E+00 ,  0.8884411E+00 ,&
        &  0.8879054E+00 ,  0.8873703E+00 ,  0.8868394E+00 ,  0.8863041E+00 ,  0.8857781E+00 ,&
        &  0.8852476E+00 ,  0.8847220E+00 ,  0.8841919E+00 ,  0.8836662E+00 ,  0.8831454E+00 ,&
        &  0.8826201E+00 ,  0.8820992E+00 ,  0.8815787E+00 ,  0.8810582E+00 ,  0.8805376E+00 ,&
        &  0.8800219E+00 ,  0.8795062E+00 ,  0.8789954E+00 ,  0.8784845E+00 ,  0.8779692E+00 ,&
        &  0.8774582E+00 ,  0.8769477E+00 ,  0.8764415E+00 ,  0.8759360E+00 ,  0.8754303E+00 ,&
        &  0.8749246E+00 ,  0.8744237E+00 ,  0.8739179E+00 ,  0.8734177E+00 ,  0.8729167E+00 ,&
        &  0.8724206E+00 ,  0.8719202E+00 ,  0.8714290E+00 ,  0.8709334E+00 ,  0.8704329E+00 /)
      ssaice4(:, 21) = (/ &
        &  0.9981896E+00 ,  0.9982691E+00 ,  0.9977399E+00 ,  0.9968850E+00 ,  0.9956240E+00 ,&
        &  0.9941898E+00 ,  0.9928634E+00 ,  0.9917374E+00 ,  0.9907728E+00 ,  0.9899129E+00 ,&
        &  0.9891046E+00 ,  0.9883166E+00 ,  0.9875370E+00 ,  0.9867612E+00 ,  0.9859810E+00 ,&
        &  0.9852068E+00 ,  0.9844257E+00 ,  0.9836478E+00 ,  0.9828643E+00 ,  0.9820896E+00 ,&
        &  0.9813052E+00 ,  0.9805347E+00 ,  0.9797593E+00 ,  0.9789836E+00 ,  0.9782124E+00 ,&
        &  0.9774457E+00 ,  0.9766834E+00 ,  0.9759208E+00 ,  0.9751627E+00 ,  0.9743996E+00 ,&
        &  0.9736505E+00 ,  0.9729011E+00 ,  0.9721515E+00 ,  0.9714065E+00 ,  0.9706659E+00 ,&
        &  0.9699252E+00 ,  0.9691892E+00 ,  0.9684532E+00 ,  0.9677265E+00 ,  0.9669998E+00 ,&
        &  0.9662826E+00 ,  0.9655606E+00 ,  0.9648435E+00 ,  0.9641358E+00 ,  0.9634282E+00 ,&
        &  0.9627159E+00 ,  0.9620132E+00 ,  0.9613153E+00 ,  0.9606223E+00 ,  0.9599293E+00 ,&
        &  0.9592413E+00 ,  0.9585533E+00 ,  0.9578702E+00 ,  0.9571920E+00 ,  0.9565186E+00 ,&
        &  0.9558501E+00 ,  0.9551770E+00 ,  0.9545087E+00 ,  0.9538502E+00 ,  0.9531867E+00 ,&
        &  0.9525331E+00 ,  0.9518748E+00 ,  0.9512308E+00 ,  0.9505823E+00 ,  0.9499338E+00 ,&
        &  0.9492997E+00 ,  0.9486611E+00 ,  0.9480224E+00 ,  0.9473934E+00 ,  0.9467643E+00 ,&
        &  0.9461403E+00 ,  0.9455163E+00 ,  0.9448972E+00 ,  0.9442784E+00 ,  0.9436641E+00 ,&
        &  0.9430548E+00 ,  0.9424455E+00 ,  0.9418411E+00 ,  0.9412367E+00 ,  0.9406373E+00 ,&
        &  0.9400379E+00 ,  0.9394481E+00 ,  0.9388536E+00 ,  0.9382688E+00 ,  0.9376793E+00 ,&
        &  0.9370993E+00 ,  0.9365146E+00 ,  0.9359349E+00 ,  0.9353600E+00 ,  0.9347900E+00 ,&
        &  0.9342198E+00 ,  0.9336498E+00 ,  0.9330849E+00 ,  0.9325247E+00 ,  0.9319597E+00 ,&
        &  0.9314044E+00 ,  0.9308490E+00 ,  0.9302986E+00 ,  0.9297435E+00 ,  0.9292027E+00 ,&
        &  0.9286525E+00 ,  0.9281121E+00 ,  0.9275715E+00 ,  0.9270357E+00 ,  0.9265001E+00 ,&
        &  0.9259647E+00 ,  0.9254387E+00 ,  0.9249035E+00 ,  0.9243824E+00 ,  0.9238568E+00 ,&
        &  0.9233359E+00 ,  0.9228153E+00 ,  0.9222991E+00 ,  0.9217881E+00 ,  0.9212724E+00 ,&
        &  0.9207616E+00 ,  0.9202506E+00 ,  0.9197494E+00 ,  0.9192435E+00 ,  0.9187425E+00 ,&
        &  0.9182413E+00 ,  0.9177454E+00 ,  0.9172540E+00 ,  0.9167578E+00 ,  0.9162670E+00 ,&
        &  0.9157805E+00 ,  0.9152895E+00 ,  0.9148082E+00 ,  0.9143268E+00 ,  0.9138409E+00 ,&
        &  0.9133647E+00 ,  0.9128884E+00 ,  0.9124120E+00 ,  0.9119406E+00 ,  0.9114694E+00 ,&
        &  0.9109982E+00 ,  0.9105318E+00 ,  0.9100703E+00 ,  0.9096042E+00 ,  0.9091474E+00 ,&
        &  0.9086862E+00 ,  0.9082297E+00 ,  0.9077732E+00 ,  0.9073171E+00 ,  0.9068703E+00 ,&
        &  0.9064190E+00 ,  0.9059725E+00 ,  0.9055260E+00 ,  0.9050844E+00 ,  0.9046382E+00 ,&
        &  0.9042013E+00 ,  0.9037600E+00 ,  0.9033235E+00 ,  0.9028870E+00 ,  0.9024553E+00 ,&
        &  0.9020191E+00 ,  0.9015923E+00 ,  0.9011609E+00 ,  0.9007345E+00 ,  0.9003080E+00 ,&
        &  0.8998863E+00 ,  0.8994646E+00 ,  0.8990434E+00 ,  0.8986265E+00 ,  0.8982052E+00 ,&
        &  0.8977932E+00 ,  0.8973816E+00 ,  0.8969700E+00 ,  0.8965584E+00 ,  0.8961472E+00 ,&
        &  0.8957404E+00 ,  0.8953341E+00 ,  0.8949321E+00 ,  0.8945257E+00 ,  0.8941242E+00 ,&
        &  0.8937275E+00 ,  0.8933259E+00 ,  0.8929342E+00 ,  0.8925375E+00 ,  0.8921456E+00 ,&
        &  0.8917543E+00 ,  0.8913623E+00 ,  0.8909709E+00 ,  0.8905844E+00 ,  0.8901979E+00 ,&
        &  0.8898113E+00 ,  0.8894296E+00 ,  0.8890479E+00 ,  0.8886710E+00 ,  0.8882942E+00 ,&
        &  0.8879129E+00 ,  0.8875360E+00 ,  0.8871596E+00 ,  0.8867876E+00 ,  0.8864161E+00 ,&
        &  0.8860446E+00 ,  0.8856730E+00 ,  0.8853063E+00 ,  0.8849396E+00 ,  0.8845779E+00 /)
      ssaice4(:, 22) = (/ &
        &  0.9999119E+00 ,  0.9999038E+00 ,  0.9998647E+00 ,  0.9998100E+00 ,  0.9997516E+00 ,&
        &  0.9996933E+00 ,  0.9996428E+00 ,  0.9995855E+00 ,  0.9995382E+00 ,  0.9994771E+00 ,&
        &  0.9994248E+00 ,  0.9993719E+00 ,  0.9993140E+00 ,  0.9992558E+00 ,  0.9992021E+00 ,&
        &  0.9991436E+00 ,  0.9990897E+00 ,  0.9990357E+00 ,  0.9989722E+00 ,  0.9989181E+00 ,&
        &  0.9988640E+00 ,  0.9988099E+00 ,  0.9987462E+00 ,  0.9986920E+00 ,  0.9986377E+00 ,&
        &  0.9985835E+00 ,  0.9985292E+00 ,  0.9984701E+00 ,  0.9984157E+00 ,  0.9983565E+00 ,&
        &  0.9983020E+00 ,  0.9982476E+00 ,  0.9981883E+00 ,  0.9981290E+00 ,  0.9980792E+00 ,&
        &  0.9980198E+00 ,  0.9979604E+00 ,  0.9979058E+00 ,  0.9978463E+00 ,  0.9977965E+00 ,&
        &  0.9977370E+00 ,  0.9976823E+00 ,  0.9976276E+00 ,  0.9975681E+00 ,  0.9975134E+00 ,&
        &  0.9974538E+00 ,  0.9973990E+00 ,  0.9973443E+00 ,  0.9972847E+00 ,  0.9972299E+00 ,&
        &  0.9971752E+00 ,  0.9971204E+00 ,  0.9970608E+00 ,  0.9970060E+00 ,  0.9969463E+00 ,&
        &  0.9968916E+00 ,  0.9968368E+00 ,  0.9967820E+00 ,  0.9967223E+00 ,  0.9966676E+00 ,&
        &  0.9966079E+00 ,  0.9965580E+00 ,  0.9964983E+00 ,  0.9964435E+00 ,  0.9963887E+00 ,&
        &  0.9963291E+00 ,  0.9962743E+00 ,  0.9962195E+00 ,  0.9961599E+00 ,  0.9961051E+00 ,&
        &  0.9960503E+00 ,  0.9959955E+00 ,  0.9959407E+00 ,  0.9958859E+00 ,  0.9958262E+00 ,&
        &  0.9957715E+00 ,  0.9957118E+00 ,  0.9956570E+00 ,  0.9956022E+00 ,  0.9955474E+00 ,&
        &  0.9954926E+00 ,  0.9954378E+00 ,  0.9953830E+00 ,  0.9953234E+00 ,  0.9952636E+00 ,&
        &  0.9952089E+00 ,  0.9951540E+00 ,  0.9950992E+00 ,  0.9950445E+00 ,  0.9949896E+00 ,&
        &  0.9949348E+00 ,  0.9948751E+00 ,  0.9948203E+00 ,  0.9947655E+00 ,  0.9947107E+00 ,&
        &  0.9946558E+00 ,  0.9945961E+00 ,  0.9945413E+00 ,  0.9944865E+00 ,  0.9944316E+00 ,&
        &  0.9943719E+00 ,  0.9943171E+00 ,  0.9942622E+00 ,  0.9942074E+00 ,  0.9941525E+00 ,&
        &  0.9940977E+00 ,  0.9940379E+00 ,  0.9939830E+00 ,  0.9939281E+00 ,  0.9938733E+00 ,&
        &  0.9938185E+00 ,  0.9937636E+00 ,  0.9937038E+00 ,  0.9936489E+00 ,  0.9935941E+00 ,&
        &  0.9935392E+00 ,  0.9934843E+00 ,  0.9934294E+00 ,  0.9933745E+00 ,  0.9933147E+00 ,&
        &  0.9932598E+00 ,  0.9932050E+00 ,  0.9931501E+00 ,  0.9930952E+00 ,  0.9930403E+00 ,&
        &  0.9929804E+00 ,  0.9929255E+00 ,  0.9928706E+00 ,  0.9928157E+00 ,  0.9927608E+00 ,&
        &  0.9927010E+00 ,  0.9926510E+00 ,  0.9925961E+00 ,  0.9925411E+00 ,  0.9924813E+00 ,&
        &  0.9924264E+00 ,  0.9923714E+00 ,  0.9923165E+00 ,  0.9922616E+00 ,  0.9922066E+00 ,&
        &  0.9921468E+00 ,  0.9920968E+00 ,  0.9920369E+00 ,  0.9919869E+00 ,  0.9919270E+00 ,&
        &  0.9918721E+00 ,  0.9918171E+00 ,  0.9917622E+00 ,  0.9917073E+00 ,  0.9916523E+00 ,&
        &  0.9915974E+00 ,  0.9915375E+00 ,  0.9914825E+00 ,  0.9914275E+00 ,  0.9913726E+00 ,&
        &  0.9913176E+00 ,  0.9912676E+00 ,  0.9912077E+00 ,  0.9911527E+00 ,  0.9910977E+00 ,&
        &  0.9910427E+00 ,  0.9909878E+00 ,  0.9909328E+00 ,  0.9908778E+00 ,  0.9908229E+00 ,&
        &  0.9907629E+00 ,  0.9907129E+00 ,  0.9906530E+00 ,  0.9906029E+00 ,  0.9905479E+00 ,&
        &  0.9904880E+00 ,  0.9904330E+00 ,  0.9903780E+00 ,  0.9903230E+00 ,  0.9902729E+00 ,&
        &  0.9902130E+00 ,  0.9901580E+00 ,  0.9901030E+00 ,  0.9900480E+00 ,  0.9899930E+00 ,&
        &  0.9899380E+00 ,  0.9898781E+00 ,  0.9898280E+00 ,  0.9897730E+00 ,  0.9897131E+00 ,&
        &  0.9896630E+00 ,  0.9896079E+00 ,  0.9895529E+00 ,  0.9894930E+00 ,  0.9894429E+00 ,&
        &  0.9893829E+00 ,  0.9893329E+00 ,  0.9892778E+00 ,  0.9892179E+00 ,  0.9891629E+00 ,&
        &  0.9891127E+00 ,  0.9890577E+00 ,  0.9890027E+00 ,  0.9889427E+00 ,  0.9888927E+00 /)
      ssaice4(:, 23) = (/ &
        &  0.9999878E+00 ,  0.9999838E+00 ,  0.9999751E+00 ,  0.9999604E+00 ,  0.9999453E+00 ,&
        &  0.9999354E+00 ,  0.9999257E+00 ,  0.9999208E+00 ,  0.9999111E+00 ,  0.9999061E+00 ,&
        &  0.9998963E+00 ,  0.9998866E+00 ,  0.9998720E+00 ,  0.9998622E+00 ,  0.9998524E+00 ,&
        &  0.9998425E+00 ,  0.9998326E+00 ,  0.9998180E+00 ,  0.9998081E+00 ,  0.9998030E+00 ,&
        &  0.9997883E+00 ,  0.9997784E+00 ,  0.9997637E+00 ,  0.9997585E+00 ,  0.9997486E+00 ,&
        &  0.9997387E+00 ,  0.9997288E+00 ,  0.9997188E+00 ,  0.9997041E+00 ,  0.9996990E+00 ,&
        &  0.9996890E+00 ,  0.9996791E+00 ,  0.9996691E+00 ,  0.9996592E+00 ,  0.9996443E+00 ,&
        &  0.9996393E+00 ,  0.9996293E+00 ,  0.9996194E+00 ,  0.9996094E+00 ,  0.9995945E+00 ,&
        &  0.9995846E+00 ,  0.9995746E+00 ,  0.9995646E+00 ,  0.9995497E+00 ,  0.9995398E+00 ,&
        &  0.9995298E+00 ,  0.9995248E+00 ,  0.9995099E+00 ,  0.9994999E+00 ,  0.9994900E+00 ,&
        &  0.9994799E+00 ,  0.9994700E+00 ,  0.9994600E+00 ,  0.9994451E+00 ,  0.9994352E+00 ,&
        &  0.9994252E+00 ,  0.9994152E+00 ,  0.9994053E+00 ,  0.9993953E+00 ,  0.9993804E+00 ,&
        &  0.9993753E+00 ,  0.9993654E+00 ,  0.9993554E+00 ,  0.9993405E+00 ,  0.9993305E+00 ,&
        &  0.9993206E+00 ,  0.9993106E+00 ,  0.9993006E+00 ,  0.9992906E+00 ,  0.9992807E+00 ,&
        &  0.9992707E+00 ,  0.9992607E+00 ,  0.9992458E+00 ,  0.9992408E+00 ,  0.9992259E+00 ,&
        &  0.9992208E+00 ,  0.9992059E+00 ,  0.9992009E+00 ,  0.9991909E+00 ,  0.9991760E+00 ,&
        &  0.9991660E+00 ,  0.9991560E+00 ,  0.9991460E+00 ,  0.9991360E+00 ,  0.9991261E+00 ,&
        &  0.9991161E+00 ,  0.9991061E+00 ,  0.9990962E+00 ,  0.9990861E+00 ,  0.9990762E+00 ,&
        &  0.9990612E+00 ,  0.9990513E+00 ,  0.9990462E+00 ,  0.9990363E+00 ,  0.9990262E+00 ,&
        &  0.9990113E+00 ,  0.9990013E+00 ,  0.9989914E+00 ,  0.9989814E+00 ,  0.9989763E+00 ,&
        &  0.9989614E+00 ,  0.9989514E+00 ,  0.9989414E+00 ,  0.9989314E+00 ,  0.9989215E+00 ,&
        &  0.9989114E+00 ,  0.9989014E+00 ,  0.9988915E+00 ,  0.9988765E+00 ,  0.9988715E+00 ,&
        &  0.9988565E+00 ,  0.9988465E+00 ,  0.9988366E+00 ,  0.9988315E+00 ,  0.9988165E+00 ,&
        &  0.9988066E+00 ,  0.9987966E+00 ,  0.9987866E+00 ,  0.9987766E+00 ,  0.9987666E+00 ,&
        &  0.9987566E+00 ,  0.9987466E+00 ,  0.9987366E+00 ,  0.9987266E+00 ,  0.9987167E+00 ,&
        &  0.9987066E+00 ,  0.9986966E+00 ,  0.9986867E+00 ,  0.9986767E+00 ,  0.9986617E+00 ,&
        &  0.9986517E+00 ,  0.9986467E+00 ,  0.9986317E+00 ,  0.9986217E+00 ,  0.9986117E+00 ,&
        &  0.9986017E+00 ,  0.9985917E+00 ,  0.9985768E+00 ,  0.9985717E+00 ,  0.9985567E+00 ,&
        &  0.9985517E+00 ,  0.9985417E+00 ,  0.9985317E+00 ,  0.9985217E+00 ,  0.9985117E+00 ,&
        &  0.9985017E+00 ,  0.9984868E+00 ,  0.9984817E+00 ,  0.9984667E+00 ,  0.9984617E+00 ,&
        &  0.9984467E+00 ,  0.9984367E+00 ,  0.9984267E+00 ,  0.9984167E+00 ,  0.9984067E+00 ,&
        &  0.9983967E+00 ,  0.9983867E+00 ,  0.9983767E+00 ,  0.9983667E+00 ,  0.9983567E+00 ,&
        &  0.9983467E+00 ,  0.9983367E+00 ,  0.9983267E+00 ,  0.9983117E+00 ,  0.9983017E+00 ,&
        &  0.9982917E+00 ,  0.9982817E+00 ,  0.9982716E+00 ,  0.9982616E+00 ,  0.9982566E+00 ,&
        &  0.9982416E+00 ,  0.9982316E+00 ,  0.9982216E+00 ,  0.9982116E+00 ,  0.9982016E+00 ,&
        &  0.9981916E+00 ,  0.9981816E+00 ,  0.9981716E+00 ,  0.9981616E+00 ,  0.9981515E+00 ,&
        &  0.9981415E+00 ,  0.9981315E+00 ,  0.9981215E+00 ,  0.9981065E+00 ,  0.9980965E+00 ,&
        &  0.9980915E+00 ,  0.9980765E+00 ,  0.9980665E+00 ,  0.9980614E+00 ,  0.9980465E+00 ,&
        &  0.9980364E+00 ,  0.9980264E+00 ,  0.9980164E+00 ,  0.9980063E+00 ,  0.9979964E+00 ,&
        &  0.9979863E+00 ,  0.9979763E+00 ,  0.9979663E+00 ,  0.9979563E+00 ,  0.9979463E+00 /)
      ssaice4(:, 24) = (/ &
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999954E+00 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.9999952E+00 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999952E+00 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999952E+00 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.9999951E+00 ,  0.9999951E+00 ,  0.9999951E+00 ,&
        &  0.9999951E+00 ,  0.9999951E+00 ,  0.9999902E+00 ,  0.9999951E+00 ,  0.9999951E+00 ,&
        &  0.9999951E+00 ,  0.9999902E+00 ,  0.9999902E+00 ,  0.9999902E+00 ,  0.9999902E+00 ,&
        &  0.9999902E+00 ,  0.9999902E+00 ,  0.9999902E+00 ,  0.9999853E+00 ,  0.9999902E+00 ,&
        &  0.9999852E+00 ,  0.9999902E+00 ,  0.9999902E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,&
        &  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,&
        &  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,&
        &  0.9999852E+00 ,  0.9999852E+00 ,  0.9999803E+00 ,  0.9999803E+00 ,  0.9999803E+00 ,&
        &  0.9999852E+00 ,  0.9999803E+00 ,  0.9999802E+00 ,  0.9999852E+00 ,  0.9999802E+00 ,&
        &  0.9999802E+00 ,  0.9999852E+00 ,  0.9999852E+00 ,  0.9999802E+00 ,  0.9999852E+00 ,&
        &  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999852E+00 ,&
        &  0.9999802E+00 ,  0.9999802E+00 ,  0.9999852E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,&
        &  0.9999852E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,&
        &  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999802E+00 ,  0.9999752E+00 ,&
        &  0.9999802E+00 ,  0.9999802E+00 ,  0.9999752E+00 ,  0.9999802E+00 ,  0.9999752E+00 ,&
        &  0.9999752E+00 ,  0.9999752E+00 ,  0.9999752E+00 ,  0.9999752E+00 ,  0.9999752E+00 ,&
        &  0.9999752E+00 ,  0.9999752E+00 ,  0.9999752E+00 ,  0.9999752E+00 ,  0.9999703E+00 ,&
        &  0.9999752E+00 ,  0.9999752E+00 ,  0.9999703E+00 ,  0.9999703E+00 ,  0.9999752E+00 ,&
        &  0.9999703E+00 ,  0.9999702E+00 ,  0.9999702E+00 ,  0.9999702E+00 ,  0.9999702E+00 ,&
        &  0.9999653E+00 ,  0.9999653E+00 ,  0.9999702E+00 ,  0.9999653E+00 ,  0.9999702E+00 ,&
        &  0.9999653E+00 ,  0.9999653E+00 ,  0.9999653E+00 ,  0.9999653E+00 ,  0.9999653E+00 ,&
        &  0.9999653E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,&
        &  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999553E+00 ,&
        &  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999602E+00 ,  0.9999553E+00 ,&
        &  0.9999553E+00 ,  0.9999503E+00 ,  0.9999552E+00 ,  0.9999503E+00 ,  0.9999503E+00 ,&
        &  0.9999503E+00 ,  0.9999503E+00 ,  0.9999503E+00 ,  0.9999503E+00 ,  0.9999503E+00 ,&
        &  0.9999453E+00 ,  0.9999453E+00 ,  0.9999453E+00 ,  0.9999453E+00 ,  0.9999453E+00 ,&
        &  0.9999453E+00 ,  0.9999453E+00 ,  0.9999453E+00 ,  0.9999453E+00 ,  0.9999403E+00 ,&
        &  0.9999403E+00 ,  0.9999403E+00 ,  0.9999403E+00 ,  0.9999403E+00 ,  0.9999403E+00 ,&
        &  0.9999353E+00 ,  0.9999353E+00 ,  0.9999353E+00 ,  0.9999353E+00 ,  0.9999353E+00 ,&
        &  0.9999353E+00 ,  0.9999353E+00 ,  0.9999353E+00 ,  0.9999303E+00 ,  0.9999353E+00 ,&
        &  0.9999303E+00 ,  0.9999303E+00 ,  0.9999353E+00 ,  0.9999303E+00 ,  0.9999303E+00 ,&
        &  0.9999303E+00 ,  0.9999303E+00 ,  0.9999253E+00 ,  0.9999253E+00 ,  0.9999253E+00 ,&
        &  0.9999303E+00 ,  0.9999253E+00 ,  0.9999253E+00 ,  0.9999253E+00 ,  0.9999253E+00 /)
      ssaice4(:, 25) = (/ &
        &  0.1000000E+01 ,  0.9999956E+00 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.9999951E+00 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999950E+00 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.9999950E+00 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999950E+00 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.9999950E+00 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.9999950E+00 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 /)
      ssaice4(:, 26) = (/ &
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 /)
      ssaice4(:, 27) = (/ &
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 /)
      ssaice4(:, 28) = (/ &
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,&
        &  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 ,  0.1000000E+01 /)
      ssaice4(:, 29) = (/ &
        &  0.6333784E+00 ,  0.8000548E+00 ,  0.8410283E+00 ,  0.8518977E+00 ,  0.8518206E+00 ,&
        &  0.8466160E+00 ,  0.8386434E+00 ,  0.8290431E+00 ,  0.8184650E+00 ,  0.8073614E+00 ,&
        &  0.7960402E+00 ,  0.7847390E+00 ,  0.7736600E+00 ,  0.7629327E+00 ,  0.7526625E+00 ,&
        &  0.7429039E+00 ,  0.7336956E+00 ,  0.7250440E+00 ,  0.7169170E+00 ,  0.7093102E+00 ,&
        &  0.7021855E+00 ,  0.6955141E+00 ,  0.6892471E+00 ,  0.6833546E+00 ,  0.6778174E+00 ,&
        &  0.6725874E+00 ,  0.6676517E+00 ,  0.6629767E+00 ,  0.6585481E+00 ,  0.6543425E+00 ,&
        &  0.6503423E+00 ,  0.6465337E+00 ,  0.6429026E+00 ,  0.6394359E+00 ,  0.6361224E+00 ,&
        &  0.6329532E+00 ,  0.6299179E+00 ,  0.6270074E+00 ,  0.6242178E+00 ,  0.6215364E+00 ,&
        &  0.6189667E+00 ,  0.6164950E+00 ,  0.6141149E+00 ,  0.6118250E+00 ,  0.6096210E+00 ,&
        &  0.6074915E+00 ,  0.6054416E+00 ,  0.6034698E+00 ,  0.6015617E+00 ,  0.5997175E+00 ,&
        &  0.5979404E+00 ,  0.5962256E+00 ,  0.5945608E+00 ,  0.5929567E+00 ,  0.5914009E+00 ,&
        &  0.5898983E+00 ,  0.5884421E+00 ,  0.5870278E+00 ,  0.5856650E+00 ,  0.5843393E+00 ,&
        &  0.5830556E+00 ,  0.5818092E+00 ,  0.5806028E+00 ,  0.5794290E+00 ,  0.5782877E+00 ,&
        &  0.5771819E+00 ,  0.5761115E+00 ,  0.5750690E+00 ,  0.5740572E+00 ,  0.5730685E+00 ,&
        &  0.5721134E+00 ,  0.5711766E+00 ,  0.5702706E+00 ,  0.5693907E+00 ,  0.5685319E+00 ,&
        &  0.5676972E+00 ,  0.5668857E+00 ,  0.5660907E+00 ,  0.5653169E+00 ,  0.5645672E+00 ,&
        &  0.5638389E+00 ,  0.5631194E+00 ,  0.5624240E+00 ,  0.5617478E+00 ,  0.5610855E+00 ,&
        &  0.5604376E+00 ,  0.5598062E+00 ,  0.5591941E+00 ,  0.5585911E+00 ,  0.5580101E+00 ,&
        &  0.5574332E+00 ,  0.5568757E+00 ,  0.5563299E+00 ,  0.5557986E+00 ,  0.5552792E+00 ,&
        &  0.5547693E+00 ,  0.5542713E+00 ,  0.5537878E+00 ,  0.5533112E+00 ,  0.5528466E+00 ,&
        &  0.5523915E+00 ,  0.5519509E+00 ,  0.5515125E+00 ,  0.5510886E+00 ,  0.5506718E+00 ,&
        &  0.5502646E+00 ,  0.5498644E+00 ,  0.5494788E+00 ,  0.5490980E+00 ,  0.5487193E+00 ,&
        &  0.5483553E+00 ,  0.5479982E+00 ,  0.5476460E+00 ,  0.5473036E+00 ,  0.5469632E+00 ,&
        &  0.5466326E+00 ,  0.5463117E+00 ,  0.5459930E+00 ,  0.5456792E+00 ,  0.5453750E+00 ,&
        &  0.5450780E+00 ,  0.5447858E+00 ,  0.5444984E+00 ,  0.5442160E+00 ,  0.5439405E+00 ,&
        &  0.5436651E+00 ,  0.5434043E+00 ,  0.5431408E+00 ,  0.5428821E+00 ,  0.5426333E+00 ,&
        &  0.5423817E+00 ,  0.5421426E+00 ,  0.5419057E+00 ,  0.5416687E+00 ,  0.5414389E+00 ,&
        &  0.5412166E+00 ,  0.5409965E+00 ,  0.5407764E+00 ,  0.5405661E+00 ,  0.5403531E+00 ,&
        &  0.5401476E+00 ,  0.5399444E+00 ,  0.5397460E+00 ,  0.5395499E+00 ,  0.5393564E+00 ,&
        &  0.5391700E+00 ,  0.5389837E+00 ,  0.5387973E+00 ,  0.5386207E+00 ,  0.5384415E+00 ,&
        &  0.5382698E+00 ,  0.5381003E+00 ,  0.5379308E+00 ,  0.5377662E+00 ,  0.5376016E+00 ,&
        &  0.5374442E+00 ,  0.5372844E+00 ,  0.5371319E+00 ,  0.5369793E+00 ,  0.5368267E+00 ,&
        &  0.5366791E+00 ,  0.5365337E+00 ,  0.5363908E+00 ,  0.5362504E+00 ,  0.5361098E+00 ,&
        &  0.5359741E+00 ,  0.5358385E+00 ,  0.5357078E+00 ,  0.5355770E+00 ,  0.5354486E+00 ,&
        &  0.5353227E+00 ,  0.5351991E+00 ,  0.5350755E+00 ,  0.5349519E+00 ,  0.5348332E+00 ,&
        &  0.5347195E+00 ,  0.5346007E+00 ,  0.5344843E+00 ,  0.5343754E+00 ,  0.5342639E+00 ,&
        &  0.5341523E+00 ,  0.5340458E+00 ,  0.5339391E+00 ,  0.5338374E+00 ,  0.5337307E+00 ,&
        &  0.5336291E+00 ,  0.5335296E+00 ,  0.5334328E+00 ,  0.5333334E+00 ,  0.5332365E+00 ,&
        &  0.5331420E+00 ,  0.5330474E+00 ,  0.5329578E+00 ,  0.5328633E+00 ,  0.5327736E+00 ,&
        &  0.5326889E+00 ,  0.5325966E+00 ,  0.5325119E+00 ,  0.5324245E+00 ,  0.5323447E+00 /)
      asyice4(:, 16) = (/ &
        &  0.6308889E+00 ,  0.7788630E+00 ,  0.8173713E+00 ,  0.8292636E+00 ,  0.8328955E+00 ,&
        &  0.8336033E+00 ,  0.8335392E+00 ,  0.8337720E+00 ,  0.8348147E+00 ,  0.8368036E+00 ,&
        &  0.8396288E+00 ,  0.8430514E+00 ,  0.8468503E+00 ,  0.8508188E+00 ,  0.8548120E+00 ,&
        &  0.8587264E+00 ,  0.8625237E+00 ,  0.8661603E+00 ,  0.8696347E+00 ,  0.8729506E+00 ,&
        &  0.8760936E+00 ,  0.8790711E+00 ,  0.8819165E+00 ,  0.8846163E+00 ,  0.8871803E+00 ,&
        &  0.8896178E+00 ,  0.8919415E+00 ,  0.8941618E+00 ,  0.8962691E+00 ,  0.8982760E+00 ,&
        &  0.9002025E+00 ,  0.9020389E+00 ,  0.9037826E+00 ,  0.9054705E+00 ,  0.9070698E+00 ,&
        &  0.9086173E+00 ,  0.9100809E+00 ,  0.9114977E+00 ,  0.9128507E+00 ,  0.9141627E+00 ,&
        &  0.9154083E+00 ,  0.9166112E+00 ,  0.9177613E+00 ,  0.9188743E+00 ,  0.9199491E+00 ,&
        &  0.9209774E+00 ,  0.9219823E+00 ,  0.9229392E+00 ,  0.9238552E+00 ,  0.9247543E+00 ,&
        &  0.9256118E+00 ,  0.9264434E+00 ,  0.9272482E+00 ,  0.9280182E+00 ,  0.9287692E+00 ,&
        &  0.9295005E+00 ,  0.9301961E+00 ,  0.9308800E+00 ,  0.9315350E+00 ,  0.9321780E+00 ,&
        &  0.9327838E+00 ,  0.9333931E+00 ,  0.9339648E+00 ,  0.9345313E+00 ,  0.9350765E+00 ,&
        &  0.9356082E+00 ,  0.9361177E+00 ,  0.9366136E+00 ,  0.9370950E+00 ,  0.9375712E+00 ,&
        &  0.9380161E+00 ,  0.9384632E+00 ,  0.9388960E+00 ,  0.9393141E+00 ,  0.9397174E+00 ,&
        &  0.9401056E+00 ,  0.9404957E+00 ,  0.9408627E+00 ,  0.9412313E+00 ,  0.9415843E+00 ,&
        &  0.9419307E+00 ,  0.9422700E+00 ,  0.9425941E+00 ,  0.9429109E+00 ,  0.9432206E+00 ,&
        &  0.9435146E+00 ,  0.9438097E+00 ,  0.9440976E+00 ,  0.9443697E+00 ,  0.9446516E+00 ,&
        &  0.9449085E+00 ,  0.9451668E+00 ,  0.9454259E+00 ,  0.9456692E+00 ,  0.9459044E+00 ,&
        &  0.9461405E+00 ,  0.9463691E+00 ,  0.9465900E+00 ,  0.9468117E+00 ,  0.9470253E+00 ,&
        &  0.9472313E+00 ,  0.9474295E+00 ,  0.9476370E+00 ,  0.9478278E+00 ,  0.9480109E+00 ,&
        &  0.9481941E+00 ,  0.9483697E+00 ,  0.9485543E+00 ,  0.9487312E+00 ,  0.9488998E+00 ,&
        &  0.9490601E+00 ,  0.9492126E+00 ,  0.9493741E+00 ,  0.9495273E+00 ,  0.9496812E+00 ,&
        &  0.9498271E+00 ,  0.9499732E+00 ,  0.9501109E+00 ,  0.9502492E+00 ,  0.9503881E+00 ,&
        &  0.9505181E+00 ,  0.9506491E+00 ,  0.9507803E+00 ,  0.9509030E+00 ,  0.9510263E+00 ,&
        &  0.9511411E+00 ,  0.9512565E+00 ,  0.9513720E+00 ,  0.9514790E+00 ,  0.9515951E+00 ,&
        &  0.9516941E+00 ,  0.9518023E+00 ,  0.9519104E+00 ,  0.9520106E+00 ,  0.9521108E+00 ,&
        &  0.9522111E+00 ,  0.9523029E+00 ,  0.9523951E+00 ,  0.9524875E+00 ,  0.9525803E+00 ,&
        &  0.9526646E+00 ,  0.9527489E+00 ,  0.9528423E+00 ,  0.9529272E+00 ,  0.9530035E+00 ,&
        &  0.9530798E+00 ,  0.9531652E+00 ,  0.9532421E+00 ,  0.9533195E+00 ,  0.9533969E+00 ,&
        &  0.9534652E+00 ,  0.9535431E+00 ,  0.9536120E+00 ,  0.9536813E+00 ,  0.9537507E+00 ,&
        &  0.9538115E+00 ,  0.9538813E+00 ,  0.9539512E+00 ,  0.9540125E+00 ,  0.9540738E+00 ,&
        &  0.9541355E+00 ,  0.9541882E+00 ,  0.9542500E+00 ,  0.9543210E+00 ,  0.9543741E+00 ,&
        &  0.9544278E+00 ,  0.9544810E+00 ,  0.9545438E+00 ,  0.9545888E+00 ,  0.9546426E+00 ,&
        &  0.9546967E+00 ,  0.9547510E+00 ,  0.9547964E+00 ,  0.9548420E+00 ,  0.9548967E+00 ,&
        &  0.9549422E+00 ,  0.9549882E+00 ,  0.9550343E+00 ,  0.9550803E+00 ,  0.9551268E+00 ,&
        &  0.9551729E+00 ,  0.9552107E+00 ,  0.9552572E+00 ,  0.9553037E+00 ,  0.9553416E+00 ,&
        &  0.9553794E+00 ,  0.9554265E+00 ,  0.9554556E+00 ,  0.9554939E+00 ,  0.9555323E+00 ,&
        &  0.9555706E+00 ,  0.9556180E+00 ,  0.9556476E+00 ,  0.9556865E+00 ,  0.9557161E+00 ,&
        &  0.9557549E+00 ,  0.9557937E+00 ,  0.9558238E+00 ,  0.9558539E+00 ,  0.9558931E+00 /)
      asyice4(:, 17) = (/ &
        &  0.6838497E+00 ,  0.8360057E+00 ,  0.8845630E+00 ,  0.9037390E+00 ,  0.9116541E+00 ,&
        &  0.9128916E+00 ,  0.9094743E+00 ,  0.9036284E+00 ,  0.8973988E+00 ,  0.8920299E+00 ,&
        &  0.8880306E+00 ,  0.8854268E+00 ,  0.8840058E+00 ,  0.8834742E+00 ,  0.8835939E+00 ,&
        &  0.8841411E+00 ,  0.8849558E+00 ,  0.8859246E+00 ,  0.8869671E+00 ,  0.8880250E+00 ,&
        &  0.8890783E+00 ,  0.8901013E+00 ,  0.8910742E+00 ,  0.8920170E+00 ,  0.8929085E+00 ,&
        &  0.8937592E+00 ,  0.8945768E+00 ,  0.8953500E+00 ,  0.8960899E+00 ,  0.8968006E+00 ,&
        &  0.8974739E+00 ,  0.8981349E+00 ,  0.8987710E+00 ,  0.8993821E+00 ,  0.8999813E+00 ,&
        &  0.9005558E+00 ,  0.9011191E+00 ,  0.9016715E+00 ,  0.9022072E+00 ,  0.9027326E+00 ,&
        &  0.9032483E+00 ,  0.9037542E+00 ,  0.9042504E+00 ,  0.9047441E+00 ,  0.9052223E+00 ,&
        &  0.9056972E+00 ,  0.9061564E+00 ,  0.9066197E+00 ,  0.9070734E+00 ,  0.9075180E+00 ,&
        &  0.9079527E+00 ,  0.9083912E+00 ,  0.9088199E+00 ,  0.9092454E+00 ,  0.9096672E+00 ,&
        &  0.9100725E+00 ,  0.9104871E+00 ,  0.9108915E+00 ,  0.9112856E+00 ,  0.9116819E+00 ,&
        &  0.9120678E+00 ,  0.9124560E+00 ,  0.9128400E+00 ,  0.9132192E+00 ,  0.9135872E+00 ,&
        &  0.9139573E+00 ,  0.9143295E+00 ,  0.9146833E+00 ,  0.9150457E+00 ,  0.9153966E+00 ,&
        &  0.9157418E+00 ,  0.9160891E+00 ,  0.9164312E+00 ,  0.9167612E+00 ,  0.9170996E+00 ,&
        &  0.9174257E+00 ,  0.9177532E+00 ,  0.9180748E+00 ,  0.9183983E+00 ,  0.9187089E+00 ,&
        &  0.9190212E+00 ,  0.9193277E+00 ,  0.9196283E+00 ,  0.9199300E+00 ,  0.9202265E+00 ,&
        &  0.9205234E+00 ,  0.9208078E+00 ,  0.9210933E+00 ,  0.9213800E+00 ,  0.9216534E+00 ,&
        &  0.9219280E+00 ,  0.9222037E+00 ,  0.9224732E+00 ,  0.9227366E+00 ,  0.9230078E+00 ,&
        &  0.9232588E+00 ,  0.9235182E+00 ,  0.9237641E+00 ,  0.9240183E+00 ,  0.9242657E+00 ,&
        &  0.9245001E+00 ,  0.9247422E+00 ,  0.9249854E+00 ,  0.9252222E+00 ,  0.9254528E+00 ,&
        &  0.9256769E+00 ,  0.9259020E+00 ,  0.9261276E+00 ,  0.9263467E+00 ,  0.9265600E+00 ,&
        &  0.9267805E+00 ,  0.9269877E+00 ,  0.9271958E+00 ,  0.9274043E+00 ,  0.9276063E+00 ,&
        &  0.9278092E+00 ,  0.9280125E+00 ,  0.9282023E+00 ,  0.9284000E+00 ,  0.9285979E+00 ,&
        &  0.9287899E+00 ,  0.9289821E+00 ,  0.9291677E+00 ,  0.9293468E+00 ,  0.9295336E+00 ,&
        &  0.9297138E+00 ,  0.9298948E+00 ,  0.9300692E+00 ,  0.9302513E+00 ,  0.9304193E+00 ,&
        &  0.9305950E+00 ,  0.9307716E+00 ,  0.9309415E+00 ,  0.9311046E+00 ,  0.9312755E+00 ,&
        &  0.9314396E+00 ,  0.9316041E+00 ,  0.9317693E+00 ,  0.9319277E+00 ,  0.9320939E+00 ,&
        &  0.9322459E+00 ,  0.9324130E+00 ,  0.9325589E+00 ,  0.9327196E+00 ,  0.9328739E+00 ,&
        &  0.9330356E+00 ,  0.9331833E+00 ,  0.9333313E+00 ,  0.9334871E+00 ,  0.9336365E+00 ,&
        &  0.9337857E+00 ,  0.9339284E+00 ,  0.9340861E+00 ,  0.9342222E+00 ,  0.9343736E+00 ,&
        &  0.9345177E+00 ,  0.9346625E+00 ,  0.9348075E+00 ,  0.9349532E+00 ,  0.9350920E+00 ,&
        &  0.9352309E+00 ,  0.9353705E+00 ,  0.9355103E+00 ,  0.9356580E+00 ,  0.9357910E+00 ,&
        &  0.9359247E+00 ,  0.9360663E+00 ,  0.9362080E+00 ,  0.9363427E+00 ,  0.9364704E+00 ,&
        &  0.9366060E+00 ,  0.9367495E+00 ,  0.9368781E+00 ,  0.9370148E+00 ,  0.9371443E+00 ,&
        &  0.9372817E+00 ,  0.9374120E+00 ,  0.9375426E+00 ,  0.9376732E+00 ,  0.9378046E+00 ,&
        &  0.9379288E+00 ,  0.9380682E+00 ,  0.9381932E+00 ,  0.9383257E+00 ,  0.9384510E+00 ,&
        &  0.9385770E+00 ,  0.9387031E+00 ,  0.9388294E+00 ,  0.9389563E+00 ,  0.9390833E+00 ,&
        &  0.9392105E+00 ,  0.9393384E+00 ,  0.9394664E+00 ,  0.9395871E+00 ,  0.9397081E+00 ,&
        &  0.9398296E+00 ,  0.9399587E+00 ,  0.9400731E+00 ,  0.9402030E+00 ,  0.9403177E+00 /)
      asyice4(:, 18) = (/ &
        &  0.7352185E+00 ,  0.8481801E+00 ,  0.8743246E+00 ,  0.8794214E+00 ,  0.8765073E+00 ,&
        &  0.8689405E+00 ,  0.8582879E+00 ,  0.8462166E+00 ,  0.8343480E+00 ,  0.8238125E+00 ,&
        &  0.8151841E+00 ,  0.8085600E+00 ,  0.8037703E+00 ,  0.8004906E+00 ,  0.7983911E+00 ,&
        &  0.7971622E+00 ,  0.7965646E+00 ,  0.7963969E+00 ,  0.7965330E+00 ,  0.7968698E+00 ,&
        &  0.7973385E+00 ,  0.7978891E+00 ,  0.7984943E+00 ,  0.7991298E+00 ,  0.7997760E+00 ,&
        &  0.8004330E+00 ,  0.8010792E+00 ,  0.8017167E+00 ,  0.8023513E+00 ,  0.8029721E+00 ,&
        &  0.8035800E+00 ,  0.8041720E+00 ,  0.8047569E+00 ,  0.8053237E+00 ,  0.8058801E+00 ,&
        &  0.8064221E+00 ,  0.8069556E+00 ,  0.8074797E+00 ,  0.8079950E+00 ,  0.8084915E+00 ,&
        &  0.8089924E+00 ,  0.8094743E+00 ,  0.8099523E+00 ,  0.8104194E+00 ,  0.8108865E+00 ,&
        &  0.8113426E+00 ,  0.8117936E+00 ,  0.8122385E+00 ,  0.8126783E+00 ,  0.8131067E+00 ,&
        &  0.8135383E+00 ,  0.8139594E+00 ,  0.8143834E+00 ,  0.8147961E+00 ,  0.8152117E+00 ,&
        &  0.8156158E+00 ,  0.8160185E+00 ,  0.8164191E+00 ,  0.8168173E+00 ,  0.8172081E+00 ,&
        &  0.8175974E+00 ,  0.8179844E+00 ,  0.8183690E+00 ,  0.8187556E+00 ,  0.8191301E+00 ,&
        &  0.8195075E+00 ,  0.8198825E+00 ,  0.8202540E+00 ,  0.8206239E+00 ,  0.8209904E+00 ,&
        &  0.8213596E+00 ,  0.8217210E+00 ,  0.8220841E+00 ,  0.8224447E+00 ,  0.8228027E+00 ,&
        &  0.8231571E+00 ,  0.8235089E+00 ,  0.8238623E+00 ,  0.8242175E+00 ,  0.8245656E+00 ,&
        &  0.8249145E+00 ,  0.8252606E+00 ,  0.8256084E+00 ,  0.8259482E+00 ,  0.8262939E+00 ,&
        &  0.8266370E+00 ,  0.8269763E+00 ,  0.8273075E+00 ,  0.8276456E+00 ,  0.8279845E+00 ,&
        &  0.8283150E+00 ,  0.8286473E+00 ,  0.8289810E+00 ,  0.8293110E+00 ,  0.8296372E+00 ,&
        &  0.8299605E+00 ,  0.8302898E+00 ,  0.8306099E+00 ,  0.8309369E+00 ,  0.8312547E+00 ,&
        &  0.8315793E+00 ,  0.8318947E+00 ,  0.8322116E+00 ,  0.8325246E+00 ,  0.8328391E+00 ,&
        &  0.8331542E+00 ,  0.8334662E+00 ,  0.8337734E+00 ,  0.8340821E+00 ,  0.8343922E+00 ,&
        &  0.8347030E+00 ,  0.8350051E+00 ,  0.8353078E+00 ,  0.8356120E+00 ,  0.8359121E+00 ,&
        &  0.8362128E+00 ,  0.8365095E+00 ,  0.8368075E+00 ,  0.8371015E+00 ,  0.8374016E+00 ,&
        &  0.8376920E+00 ,  0.8379839E+00 ,  0.8382716E+00 ,  0.8385655E+00 ,  0.8388495E+00 ,&
        &  0.8391398E+00 ,  0.8394257E+00 ,  0.8397132E+00 ,  0.8399956E+00 ,  0.8402746E+00 ,&
        &  0.8405588E+00 ,  0.8408397E+00 ,  0.8411156E+00 ,  0.8413975E+00 ,  0.8416752E+00 ,&
        &  0.8419486E+00 ,  0.8422234E+00 ,  0.8424987E+00 ,  0.8427687E+00 ,  0.8430458E+00 ,&
        &  0.8433130E+00 ,  0.8435806E+00 ,  0.8438495E+00 ,  0.8441142E+00 ,  0.8443850E+00 ,&
        &  0.8446457E+00 ,  0.8449125E+00 ,  0.8451742E+00 ,  0.8454380E+00 ,  0.8457014E+00 ,&
        &  0.8459556E+00 ,  0.8462159E+00 ,  0.8464766E+00 ,  0.8467330E+00 ,  0.8469898E+00 ,&
        &  0.8472478E+00 ,  0.8474957E+00 ,  0.8477498E+00 ,  0.8480099E+00 ,  0.8482599E+00 ,&
        &  0.8485055E+00 ,  0.8487571E+00 ,  0.8490044E+00 ,  0.8492520E+00 ,  0.8495057E+00 ,&
        &  0.8497492E+00 ,  0.8499939E+00 ,  0.8502391E+00 ,  0.8504846E+00 ,  0.8507257E+00 ,&
        &  0.8509728E+00 ,  0.8512096E+00 ,  0.8514477E+00 ,  0.8516968E+00 ,  0.8519307E+00 ,&
        &  0.8521699E+00 ,  0.8524054E+00 ,  0.8526461E+00 ,  0.8528823E+00 ,  0.8531139E+00 ,&
        &  0.8533516E+00 ,  0.8535897E+00 ,  0.8538174E+00 ,  0.8540512E+00 ,  0.8542863E+00 ,&
        &  0.8545159E+00 ,  0.8547466E+00 ,  0.8549777E+00 ,  0.8552042E+00 ,  0.8554359E+00 ,&
        &  0.8556581E+00 ,  0.8558913E+00 ,  0.8561141E+00 ,  0.8563380E+00 ,  0.8565623E+00 ,&
        &  0.8567927E+00 ,  0.8570126E+00 ,  0.8572387E+00 ,  0.8574601E+00 ,  0.8576759E+00 /)
      asyice4(:, 19) = (/ &
        &  0.7611694E+00 ,  0.8532702E+00 ,  0.8687301E+00 ,  0.8666995E+00 ,  0.8576527E+00 ,&
        &  0.8447345E+00 ,  0.8305908E+00 ,  0.8175464E+00 ,  0.8069980E+00 ,  0.7993522E+00 ,&
        &  0.7943702E+00 ,  0.7915002E+00 ,  0.7901685E+00 ,  0.7898793E+00 ,  0.7902810E+00 ,&
        &  0.7911142E+00 ,  0.7922185E+00 ,  0.7934816E+00 ,  0.7948319E+00 ,  0.7962279E+00 ,&
        &  0.7976367E+00 ,  0.7990458E+00 ,  0.8004408E+00 ,  0.8018208E+00 ,  0.8031769E+00 ,&
        &  0.8045048E+00 ,  0.8058162E+00 ,  0.8070983E+00 ,  0.8083598E+00 ,  0.8095918E+00 ,&
        &  0.8108044E+00 ,  0.8119949E+00 ,  0.8131683E+00 ,  0.8143155E+00 ,  0.8154491E+00 ,&
        &  0.8165632E+00 ,  0.8176534E+00 ,  0.8187321E+00 ,  0.8197905E+00 ,  0.8208369E+00 ,&
        &  0.8218667E+00 ,  0.8228831E+00 ,  0.8238812E+00 ,  0.8248709E+00 ,  0.8258362E+00 ,&
        &  0.8267973E+00 ,  0.8277482E+00 ,  0.8286786E+00 ,  0.8296087E+00 ,  0.8305179E+00 ,&
        &  0.8314208E+00 ,  0.8323116E+00 ,  0.8331959E+00 ,  0.8340687E+00 ,  0.8349336E+00 ,&
        &  0.8357812E+00 ,  0.8366262E+00 ,  0.8374629E+00 ,  0.8382912E+00 ,  0.8391061E+00 ,&
        &  0.8399124E+00 ,  0.8407204E+00 ,  0.8415089E+00 ,  0.8423038E+00 ,  0.8430789E+00 ,&
        &  0.8438553E+00 ,  0.8446165E+00 ,  0.8453731E+00 ,  0.8461249E+00 ,  0.8468720E+00 ,&
        &  0.8476083E+00 ,  0.8483396E+00 ,  0.8490659E+00 ,  0.8497869E+00 ,  0.8504909E+00 ,&
        &  0.8512005E+00 ,  0.8518987E+00 ,  0.8525915E+00 ,  0.8532828E+00 ,  0.8539634E+00 ,&
        &  0.8546425E+00 ,  0.8553097E+00 ,  0.8559762E+00 ,  0.8566358E+00 ,  0.8572894E+00 ,&
        &  0.8579360E+00 ,  0.8585807E+00 ,  0.8592244E+00 ,  0.8598496E+00 ,  0.8604788E+00 ,&
        &  0.8610954E+00 ,  0.8617161E+00 ,  0.8623284E+00 ,  0.8629341E+00 ,  0.8635374E+00 ,&
        &  0.8641331E+00 ,  0.8647265E+00 ,  0.8653182E+00 ,  0.8658960E+00 ,  0.8664712E+00 ,&
        &  0.8670501E+00 ,  0.8676209E+00 ,  0.8681829E+00 ,  0.8687429E+00 ,  0.8693004E+00 ,&
        &  0.8698431E+00 ,  0.8703949E+00 ,  0.8709320E+00 ,  0.8714725E+00 ,  0.8720102E+00 ,&
        &  0.8725329E+00 ,  0.8730645E+00 ,  0.8735811E+00 ,  0.8740946E+00 ,  0.8746105E+00 ,&
        &  0.8751177E+00 ,  0.8756217E+00 ,  0.8761280E+00 ,  0.8766255E+00 ,  0.8771131E+00 ,&
        &  0.8776096E+00 ,  0.8780961E+00 ,  0.8785735E+00 ,  0.8790532E+00 ,  0.8795294E+00 ,&
        &  0.8800013E+00 ,  0.8804696E+00 ,  0.8809351E+00 ,  0.8813955E+00 ,  0.8818529E+00 ,&
        &  0.8823125E+00 ,  0.8827617E+00 ,  0.8832073E+00 ,  0.8836482E+00 ,  0.8840978E+00 ,&
        &  0.8845311E+00 ,  0.8849664E+00 ,  0.8853970E+00 ,  0.8858303E+00 ,  0.8862590E+00 ,&
        &  0.8866770E+00 ,  0.8870977E+00 ,  0.8875135E+00 ,  0.8879312E+00 ,  0.8883381E+00 ,&
        &  0.8887469E+00 ,  0.8891516E+00 ,  0.8895513E+00 ,  0.8899595E+00 ,  0.8903501E+00 ,&
        &  0.8907491E+00 ,  0.8911431E+00 ,  0.8915262E+00 ,  0.8919169E+00 ,  0.8923041E+00 ,&
        &  0.8926854E+00 ,  0.8930623E+00 ,  0.8934409E+00 ,  0.8938143E+00 ,  0.8941833E+00 ,&
        &  0.8945539E+00 ,  0.8949254E+00 ,  0.8952855E+00 ,  0.8956479E+00 ,  0.8960044E+00 ,&
        &  0.8963631E+00 ,  0.8967226E+00 ,  0.8970706E+00 ,  0.8974202E+00 ,  0.8977712E+00 ,&
        &  0.8981169E+00 ,  0.8984641E+00 ,  0.8987996E+00 ,  0.8991428E+00 ,  0.8994805E+00 ,&
        &  0.8998135E+00 ,  0.9001541E+00 ,  0.9004830E+00 ,  0.9008125E+00 ,  0.9011372E+00 ,&
        &  0.9014627E+00 ,  0.9017832E+00 ,  0.9021114E+00 ,  0.9024339E+00 ,  0.9027445E+00 ,&
        &  0.9030628E+00 ,  0.9033824E+00 ,  0.9036830E+00 ,  0.9039983E+00 ,  0.9043078E+00 ,&
        &  0.9046116E+00 ,  0.9049167E+00 ,  0.9052224E+00 ,  0.9055231E+00 ,  0.9058242E+00 ,&
        &  0.9061204E+00 ,  0.9064170E+00 ,  0.9067078E+00 ,  0.9069999E+00 ,  0.9072860E+00 /)
      asyice4(:, 20) = (/ &
        &  0.7878429E+00 ,  0.8563017E+00 ,  0.8607460E+00 ,  0.8512708E+00 ,  0.8366460E+00 ,&
        &  0.8201939E+00 ,  0.8045444E+00 ,  0.7915565E+00 ,  0.7819592E+00 ,  0.7755091E+00 ,&
        &  0.7715159E+00 ,  0.7692651E+00 ,  0.7681581E+00 ,  0.7677852E+00 ,  0.7678676E+00 ,&
        &  0.7682171E+00 ,  0.7687253E+00 ,  0.7693235E+00 ,  0.7699625E+00 ,  0.7706240E+00 ,&
        &  0.7712795E+00 ,  0.7719285E+00 ,  0.7725646E+00 ,  0.7731836E+00 ,  0.7737828E+00 ,&
        &  0.7743682E+00 ,  0.7749358E+00 ,  0.7754831E+00 ,  0.7760236E+00 ,  0.7765423E+00 ,&
        &  0.7770467E+00 ,  0.7775442E+00 ,  0.7780260E+00 ,  0.7784946E+00 ,  0.7789598E+00 ,&
        &  0.7794144E+00 ,  0.7798594E+00 ,  0.7802946E+00 ,  0.7807267E+00 ,  0.7811478E+00 ,&
        &  0.7815619E+00 ,  0.7819735E+00 ,  0.7823769E+00 ,  0.7827768E+00 ,  0.7831722E+00 ,&
        &  0.7835601E+00 ,  0.7839472E+00 ,  0.7843308E+00 ,  0.7847056E+00 ,  0.7850797E+00 ,&
        &  0.7854488E+00 ,  0.7858222E+00 ,  0.7861857E+00 ,  0.7865481E+00 ,  0.7869108E+00 ,&
        &  0.7872673E+00 ,  0.7876218E+00 ,  0.7879763E+00 ,  0.7883286E+00 ,  0.7886800E+00 ,&
        &  0.7890250E+00 ,  0.7893730E+00 ,  0.7897189E+00 ,  0.7900584E+00 ,  0.7904007E+00 ,&
        &  0.7907409E+00 ,  0.7910747E+00 ,  0.7914154E+00 ,  0.7917486E+00 ,  0.7920806E+00 ,&
        &  0.7924144E+00 ,  0.7927406E+00 ,  0.7930737E+00 ,  0.7933992E+00 ,  0.7937223E+00 ,&
        &  0.7940524E+00 ,  0.7943748E+00 ,  0.7946895E+00 ,  0.7950153E+00 ,  0.7953293E+00 ,&
        &  0.7956490E+00 ,  0.7959610E+00 ,  0.7962800E+00 ,  0.7965911E+00 ,  0.7968987E+00 ,&
        &  0.7972079E+00 ,  0.7975134E+00 ,  0.7978259E+00 ,  0.7981294E+00 ,  0.7984303E+00 ,&
        &  0.7987371E+00 ,  0.7990360E+00 ,  0.7993353E+00 ,  0.7996321E+00 ,  0.7999346E+00 ,&
        &  0.8002292E+00 ,  0.8005201E+00 ,  0.8008114E+00 ,  0.8011043E+00 ,  0.8013945E+00 ,&
        &  0.8016799E+00 ,  0.8019721E+00 ,  0.8022552E+00 ,  0.8025442E+00 ,  0.8028250E+00 ,&
        &  0.8031063E+00 ,  0.8033891E+00 ,  0.8036681E+00 ,  0.8039486E+00 ,  0.8042241E+00 ,&
        &  0.8045012E+00 ,  0.8047798E+00 ,  0.8050491E+00 ,  0.8053243E+00 ,  0.8055955E+00 ,&
        &  0.8058725E+00 ,  0.8061360E+00 ,  0.8064051E+00 ,  0.8066748E+00 ,  0.8069415E+00 ,&
        &  0.8072087E+00 ,  0.8074762E+00 ,  0.8077399E+00 ,  0.8079995E+00 ,  0.8082606E+00 ,&
        &  0.8085220E+00 ,  0.8087839E+00 ,  0.8090374E+00 ,  0.8093011E+00 ,  0.8095564E+00 ,&
        &  0.8098164E+00 ,  0.8100681E+00 ,  0.8103255E+00 ,  0.8105779E+00 ,  0.8108273E+00 ,&
        &  0.8110815E+00 ,  0.8113370E+00 ,  0.8115830E+00 ,  0.8118349E+00 ,  0.8120827E+00 ,&
        &  0.8123308E+00 ,  0.8125848E+00 ,  0.8128248E+00 ,  0.8130750E+00 ,  0.8133211E+00 ,&
        &  0.8135631E+00 ,  0.8138055E+00 ,  0.8140492E+00 ,  0.8142977E+00 ,  0.8145376E+00 ,&
        &  0.8147779E+00 ,  0.8150185E+00 ,  0.8152549E+00 ,  0.8154972E+00 ,  0.8157353E+00 ,&
        &  0.8159737E+00 ,  0.8162125E+00 ,  0.8164470E+00 ,  0.8166874E+00 ,  0.8169236E+00 ,&
        &  0.8171546E+00 ,  0.8173870E+00 ,  0.8176252E+00 ,  0.8178581E+00 ,  0.8180868E+00 ,&
        &  0.8183214E+00 ,  0.8185517E+00 ,  0.8187824E+00 ,  0.8190133E+00 ,  0.8192456E+00 ,&
        &  0.8194726E+00 ,  0.8197009E+00 ,  0.8199295E+00 ,  0.8201584E+00 ,  0.8203830E+00 ,&
        &  0.8206078E+00 ,  0.8208386E+00 ,  0.8210595E+00 ,  0.8212863E+00 ,  0.8215134E+00 ,&
        &  0.8217361E+00 ,  0.8219591E+00 ,  0.8221778E+00 ,  0.8224025E+00 ,  0.8226273E+00 ,&
        &  0.8228469E+00 ,  0.8230677E+00 ,  0.8232888E+00 ,  0.8235111E+00 ,  0.8237281E+00 ,&
        &  0.8239454E+00 ,  0.8241640E+00 ,  0.8243874E+00 ,  0.8246018E+00 ,  0.8248212E+00 ,&
        &  0.8250362E+00 ,  0.8252513E+00 ,  0.8254678E+00 ,  0.8256789E+00 ,  0.8259006E+00 /)
      asyice4(:, 21) = (/ &
        &  0.8164403E+00 ,  0.8563977E+00 ,  0.8482134E+00 ,  0.8296506E+00 ,  0.8088785E+00 ,&
        &  0.7905661E+00 ,  0.7771026E+00 ,  0.7685401E+00 ,  0.7636855E+00 ,  0.7612579E+00 ,&
        &  0.7602760E+00 ,  0.7601247E+00 ,  0.7604364E+00 ,  0.7609833E+00 ,  0.7616518E+00 ,&
        &  0.7623716E+00 ,  0.7631044E+00 ,  0.7638302E+00 ,  0.7645367E+00 ,  0.7652208E+00 ,&
        &  0.7658822E+00 ,  0.7665120E+00 ,  0.7671223E+00 ,  0.7677106E+00 ,  0.7682706E+00 ,&
        &  0.7688178E+00 ,  0.7693365E+00 ,  0.7698511E+00 ,  0.7703393E+00 ,  0.7708209E+00 ,&
        &  0.7712833E+00 ,  0.7717378E+00 ,  0.7721841E+00 ,  0.7726172E+00 ,  0.7730410E+00 ,&
        &  0.7734591E+00 ,  0.7738725E+00 ,  0.7742752E+00 ,  0.7746719E+00 ,  0.7750615E+00 ,&
        &  0.7754452E+00 ,  0.7758254E+00 ,  0.7762035E+00 ,  0.7765691E+00 ,  0.7769364E+00 ,&
        &  0.7773039E+00 ,  0.7776629E+00 ,  0.7780184E+00 ,  0.7783701E+00 ,  0.7787210E+00 ,&
        &  0.7790632E+00 ,  0.7794095E+00 ,  0.7797458E+00 ,  0.7800822E+00 ,  0.7804177E+00 ,&
        &  0.7807431E+00 ,  0.7810714E+00 ,  0.7813987E+00 ,  0.7817208E+00 ,  0.7820397E+00 ,&
        &  0.7823534E+00 ,  0.7826700E+00 ,  0.7829803E+00 ,  0.7832882E+00 ,  0.7835939E+00 ,&
        &  0.7838932E+00 ,  0.7841901E+00 ,  0.7844887E+00 ,  0.7847809E+00 ,  0.7850748E+00 ,&
        &  0.7853621E+00 ,  0.7856511E+00 ,  0.7859325E+00 ,  0.7862114E+00 ,  0.7864919E+00 ,&
        &  0.7867647E+00 ,  0.7870390E+00 ,  0.7873110E+00 ,  0.7875792E+00 ,  0.7878448E+00 ,&
        &  0.7881121E+00 ,  0.7883716E+00 ,  0.7886366E+00 ,  0.7888898E+00 ,  0.7891434E+00 ,&
        &  0.7893944E+00 ,  0.7896509E+00 ,  0.7898996E+00 ,  0.7901446E+00 ,  0.7903869E+00 ,&
        &  0.7906349E+00 ,  0.7908790E+00 ,  0.7911152E+00 ,  0.7913476E+00 ,  0.7915856E+00 ,&
        &  0.7918209E+00 ,  0.7920566E+00 ,  0.7922842E+00 ,  0.7925175E+00 ,  0.7927427E+00 ,&
        &  0.7929682E+00 ,  0.7931910E+00 ,  0.7934140E+00 ,  0.7936385E+00 ,  0.7938538E+00 ,&
        &  0.7940758E+00 ,  0.7942885E+00 ,  0.7945069E+00 ,  0.7947171E+00 ,  0.7949330E+00 ,&
        &  0.7951449E+00 ,  0.7953528E+00 ,  0.7955664E+00 ,  0.7957718E+00 ,  0.7959774E+00 ,&
        &  0.7961844E+00 ,  0.7963917E+00 ,  0.7965906E+00 ,  0.7967899E+00 ,  0.7969906E+00 ,&
        &  0.7971914E+00 ,  0.7973894E+00 ,  0.7975822E+00 ,  0.7977806E+00 ,  0.7979761E+00 ,&
        &  0.7981665E+00 ,  0.7983625E+00 ,  0.7985501E+00 ,  0.7987434E+00 ,  0.7989369E+00 ,&
        &  0.7991220E+00 ,  0.7993073E+00 ,  0.7994929E+00 ,  0.7996798E+00 ,  0.7998625E+00 ,&
        &  0.8000509E+00 ,  0.8002298E+00 ,  0.8004100E+00 ,  0.8005903E+00 ,  0.8007720E+00 ,&
        &  0.8009484E+00 ,  0.8011262E+00 ,  0.8013042E+00 ,  0.8014834E+00 ,  0.8016530E+00 ,&
        &  0.8018283E+00 ,  0.8019994E+00 ,  0.8021707E+00 ,  0.8023433E+00 ,  0.8025160E+00 ,&
        &  0.8026846E+00 ,  0.8028534E+00 ,  0.8030180E+00 ,  0.8031881E+00 ,  0.8033542E+00 ,&
        &  0.8035204E+00 ,  0.8036823E+00 ,  0.8038499E+00 ,  0.8040078E+00 ,  0.8041714E+00 ,&
        &  0.8043308E+00 ,  0.8044958E+00 ,  0.8046510E+00 ,  0.8048119E+00 ,  0.8049729E+00 ,&
        &  0.8051298E+00 ,  0.8052878E+00 ,  0.8054405E+00 ,  0.8055989E+00 ,  0.8057530E+00 ,&
        &  0.8059072E+00 ,  0.8060572E+00 ,  0.8062128E+00 ,  0.8063631E+00 ,  0.8065146E+00 ,&
        &  0.8066618E+00 ,  0.8068136E+00 ,  0.8069621E+00 ,  0.8071098E+00 ,  0.8072586E+00 ,&
        &  0.8074031E+00 ,  0.8075522E+00 ,  0.8077025E+00 ,  0.8078430E+00 ,  0.8079891E+00 ,&
        &  0.8081298E+00 ,  0.8082718E+00 ,  0.8084194E+00 ,  0.8085571E+00 ,  0.8086949E+00 ,&
        &  0.8088384E+00 ,  0.8089821E+00 ,  0.8091214E+00 ,  0.8092608E+00 ,  0.8093958E+00 ,&
        &  0.8095310E+00 ,  0.8096719E+00 ,  0.8098083E+00 ,  0.8099449E+00 ,  0.8100771E+00 /)
      asyice4(:, 22) = (/ &
        &  0.8295329E+00 ,  0.8538179E+00 ,  0.8374700E+00 ,  0.8130934E+00 ,  0.7899350E+00 ,&
        &  0.7728249E+00 ,  0.7623641E+00 ,  0.7567856E+00 ,  0.7541356E+00 ,  0.7530928E+00 ,&
        &  0.7528698E+00 ,  0.7530598E+00 ,  0.7534404E+00 ,  0.7539015E+00 ,  0.7543766E+00 ,&
        &  0.7548403E+00 ,  0.7552758E+00 ,  0.7556863E+00 ,  0.7560630E+00 ,  0.7564128E+00 ,&
        &  0.7567332E+00 ,  0.7570240E+00 ,  0.7572984E+00 ,  0.7575452E+00 ,  0.7577789E+00 ,&
        &  0.7579936E+00 ,  0.7581913E+00 ,  0.7583795E+00 ,  0.7585568E+00 ,  0.7587221E+00 ,&
        &  0.7588753E+00 ,  0.7590198E+00 ,  0.7591583E+00 ,  0.7592919E+00 ,  0.7594157E+00 ,&
        &  0.7595358E+00 ,  0.7596546E+00 ,  0.7597660E+00 ,  0.7598786E+00 ,  0.7599788E+00 ,&
        &  0.7600888E+00 ,  0.7601864E+00 ,  0.7602840E+00 ,  0.7603815E+00 ,  0.7604789E+00 ,&
        &  0.7605714E+00 ,  0.7606637E+00 ,  0.7607559E+00 ,  0.7608420E+00 ,  0.7609317E+00 ,&
        &  0.7610163E+00 ,  0.7610996E+00 ,  0.7611854E+00 ,  0.7612662E+00 ,  0.7613493E+00 ,&
        &  0.7614262E+00 ,  0.7615055E+00 ,  0.7615786E+00 ,  0.7616541E+00 ,  0.7617271E+00 ,&
        &  0.7618025E+00 ,  0.7618716E+00 ,  0.7619370E+00 ,  0.7620048E+00 ,  0.7620701E+00 ,&
        &  0.7621366E+00 ,  0.7621968E+00 ,  0.7622620E+00 ,  0.7623246E+00 ,  0.7623847E+00 ,&
        &  0.7624410E+00 ,  0.7624986E+00 ,  0.7625573E+00 ,  0.7626086E+00 ,  0.7626649E+00 ,&
        &  0.7627186E+00 ,  0.7627723E+00 ,  0.7628185E+00 ,  0.7628745E+00 ,  0.7629182E+00 ,&
        &  0.7629668E+00 ,  0.7630166E+00 ,  0.7630627E+00 ,  0.7631088E+00 ,  0.7631561E+00 ,&
        &  0.7631996E+00 ,  0.7632443E+00 ,  0.7632890E+00 ,  0.7633300E+00 ,  0.7633760E+00 ,&
        &  0.7634133E+00 ,  0.7634555E+00 ,  0.7634988E+00 ,  0.7635384E+00 ,  0.7635731E+00 ,&
        &  0.7636178E+00 ,  0.7636536E+00 ,  0.7636945E+00 ,  0.7637316E+00 ,  0.7637687E+00 ,&
        &  0.7638058E+00 ,  0.7638441E+00 ,  0.7638775E+00 ,  0.7639120E+00 ,  0.7639465E+00 ,&
        &  0.7639861E+00 ,  0.7640207E+00 ,  0.7640564E+00 ,  0.7640922E+00 ,  0.7641242E+00 ,&
        &  0.7641562E+00 ,  0.7641882E+00 ,  0.7642252E+00 ,  0.7642534E+00 ,  0.7642866E+00 ,&
        &  0.7643198E+00 ,  0.7643531E+00 ,  0.7643825E+00 ,  0.7644170E+00 ,  0.7644464E+00 ,&
        &  0.7644809E+00 ,  0.7645066E+00 ,  0.7645372E+00 ,  0.7645679E+00 ,  0.7645985E+00 ,&
        &  0.7646343E+00 ,  0.7646611E+00 ,  0.7646880E+00 ,  0.7647199E+00 ,  0.7647518E+00 ,&
        &  0.7647787E+00 ,  0.7648069E+00 ,  0.7648349E+00 ,  0.7648631E+00 ,  0.7648912E+00 ,&
        &  0.7649243E+00 ,  0.7649487E+00 ,  0.7649818E+00 ,  0.7650061E+00 ,  0.7650355E+00 ,&
        &  0.7650598E+00 ,  0.7650854E+00 ,  0.7651147E+00 ,  0.7651403E+00 ,  0.7651697E+00 ,&
        &  0.7652002E+00 ,  0.7652258E+00 ,  0.7652514E+00 ,  0.7652782E+00 ,  0.7653037E+00 ,&
        &  0.7653305E+00 ,  0.7653561E+00 ,  0.7653829E+00 ,  0.7654096E+00 ,  0.7654365E+00 ,&
        &  0.7654633E+00 ,  0.7654862E+00 ,  0.7655131E+00 ,  0.7655399E+00 ,  0.7655629E+00 ,&
        &  0.7655909E+00 ,  0.7656139E+00 ,  0.7656419E+00 ,  0.7656649E+00 ,  0.7656930E+00 ,&
        &  0.7657160E+00 ,  0.7657402E+00 ,  0.7657683E+00 ,  0.7657925E+00 ,  0.7658167E+00 ,&
        &  0.7658448E+00 ,  0.7658690E+00 ,  0.7658933E+00 ,  0.7659175E+00 ,  0.7659380E+00 ,&
        &  0.7659672E+00 ,  0.7659915E+00 ,  0.7660120E+00 ,  0.7660412E+00 ,  0.7660617E+00 ,&
        &  0.7660871E+00 ,  0.7661164E+00 ,  0.7661369E+00 ,  0.7661624E+00 ,  0.7661878E+00 ,&
        &  0.7662095E+00 ,  0.7662350E+00 ,  0.7662605E+00 ,  0.7662860E+00 ,  0.7663077E+00 ,&
        &  0.7663332E+00 ,  0.7663549E+00 ,  0.7663816E+00 ,  0.7664071E+00 ,  0.7664288E+00 ,&
        &  0.7664555E+00 ,  0.7664772E+00 ,  0.7665039E+00 ,  0.7665256E+00 ,  0.7665485E+00 /)
      asyice4(:, 23) = (/ &
        &  0.8449632E+00 ,  0.8394781E+00 ,  0.8089847E+00 ,  0.7806522E+00 ,  0.7627347E+00 ,&
        &  0.7538859E+00 ,  0.7502508E+00 ,  0.7491167E+00 ,  0.7490667E+00 ,  0.7494577E+00 ,&
        &  0.7499953E+00 ,  0.7505553E+00 ,  0.7510936E+00 ,  0.7515800E+00 ,  0.7520195E+00 ,&
        &  0.7524163E+00 ,  0.7527677E+00 ,  0.7530851E+00 ,  0.7533708E+00 ,  0.7536269E+00 ,&
        &  0.7538666E+00 ,  0.7540824E+00 ,  0.7542852E+00 ,  0.7544651E+00 ,  0.7546427E+00 ,&
        &  0.7548023E+00 ,  0.7549570E+00 ,  0.7551008E+00 ,  0.7552459E+00 ,  0.7553740E+00 ,&
        &  0.7555069E+00 ,  0.7556252E+00 ,  0.7557472E+00 ,  0.7558582E+00 ,  0.7559716E+00 ,&
        &  0.7560739E+00 ,  0.7561774E+00 ,  0.7562773E+00 ,  0.7563685E+00 ,  0.7564646E+00 ,&
        &  0.7565483E+00 ,  0.7566358E+00 ,  0.7567146E+00 ,  0.7567921E+00 ,  0.7568696E+00 ,&
        &  0.7569408E+00 ,  0.7570071E+00 ,  0.7570710E+00 ,  0.7571335E+00 ,  0.7571948E+00 ,&
        &  0.7572499E+00 ,  0.7573024E+00 ,  0.7573574E+00 ,  0.7574050E+00 ,  0.7574550E+00 ,&
        &  0.7574939E+00 ,  0.7575388E+00 ,  0.7575763E+00 ,  0.7576151E+00 ,  0.7576513E+00 ,&
        &  0.7576851E+00 ,  0.7577151E+00 ,  0.7577463E+00 ,  0.7577787E+00 ,  0.7578074E+00 ,&
        &  0.7578336E+00 ,  0.7578560E+00 ,  0.7578834E+00 ,  0.7579083E+00 ,  0.7579245E+00 ,&
        &  0.7579456E+00 ,  0.7579679E+00 ,  0.7579865E+00 ,  0.7580063E+00 ,  0.7580212E+00 ,&
        &  0.7580385E+00 ,  0.7580546E+00 ,  0.7580681E+00 ,  0.7580816E+00 ,  0.7581001E+00 ,&
        &  0.7581099E+00 ,  0.7581258E+00 ,  0.7581369E+00 ,  0.7581491E+00 ,  0.7581613E+00 ,&
        &  0.7581698E+00 ,  0.7581831E+00 ,  0.7581965E+00 ,  0.7582062E+00 ,  0.7582121E+00 ,&
        &  0.7582268E+00 ,  0.7582327E+00 ,  0.7582398E+00 ,  0.7582518E+00 ,  0.7582590E+00 ,&
        &  0.7582710E+00 ,  0.7582794E+00 ,  0.7582877E+00 ,  0.7582973E+00 ,  0.7583018E+00 ,&
        &  0.7583113E+00 ,  0.7583209E+00 ,  0.7583266E+00 ,  0.7583374E+00 ,  0.7583432E+00 ,&
        &  0.7583539E+00 ,  0.7583609E+00 ,  0.7583678E+00 ,  0.7583748E+00 ,  0.7583830E+00 ,&
        &  0.7583900E+00 ,  0.7583982E+00 ,  0.7584063E+00 ,  0.7584108E+00 ,  0.7584190E+00 ,&
        &  0.7584283E+00 ,  0.7584328E+00 ,  0.7584422E+00 ,  0.7584478E+00 ,  0.7584572E+00 ,&
        &  0.7584628E+00 ,  0.7584684E+00 ,  0.7584741E+00 ,  0.7584847E+00 ,  0.7584866E+00 ,&
        &  0.7584972E+00 ,  0.7585040E+00 ,  0.7585108E+00 ,  0.7585177E+00 ,  0.7585245E+00 ,&
        &  0.7585313E+00 ,  0.7585344E+00 ,  0.7585462E+00 ,  0.7585493E+00 ,  0.7585573E+00 ,&
        &  0.7585604E+00 ,  0.7585685E+00 ,  0.7585765E+00 ,  0.7585808E+00 ,  0.7585889E+00 ,&
        &  0.7585931E+00 ,  0.7585974E+00 ,  0.7586067E+00 ,  0.7586109E+00 ,  0.7586152E+00 ,&
        &  0.7586244E+00 ,  0.7586287E+00 ,  0.7586342E+00 ,  0.7586435E+00 ,  0.7586490E+00 ,&
        &  0.7586582E+00 ,  0.7586637E+00 ,  0.7586692E+00 ,  0.7586747E+00 ,  0.7586802E+00 ,&
        &  0.7586856E+00 ,  0.7586911E+00 ,  0.7586979E+00 ,  0.7587034E+00 ,  0.7587051E+00 ,&
        &  0.7587155E+00 ,  0.7587172E+00 ,  0.7587239E+00 ,  0.7587344E+00 ,  0.7587361E+00 ,&
        &  0.7587428E+00 ,  0.7587495E+00 ,  0.7587562E+00 ,  0.7587629E+00 ,  0.7587658E+00 ,&
        &  0.7587726E+00 ,  0.7587792E+00 ,  0.7587821E+00 ,  0.7587889E+00 ,  0.7587968E+00 ,&
        &  0.7587997E+00 ,  0.7588064E+00 ,  0.7588143E+00 ,  0.7588173E+00 ,  0.7588251E+00 ,&
        &  0.7588280E+00 ,  0.7588360E+00 ,  0.7588389E+00 ,  0.7588468E+00 ,  0.7588547E+00 ,&
        &  0.7588539E+00 ,  0.7588618E+00 ,  0.7588697E+00 ,  0.7588738E+00 ,  0.7588817E+00 ,&
        &  0.7588859E+00 ,  0.7588938E+00 ,  0.7588979E+00 ,  0.7589021E+00 ,  0.7589062E+00 ,&
        &  0.7589141E+00 ,  0.7589182E+00 ,  0.7589224E+00 ,  0.7589315E+00 ,  0.7589356E+00 /)
      asyice4(:, 24) = (/ &
        &  0.8484567E+00 ,  0.8114185E+00 ,  0.7707964E+00 ,  0.7515219E+00 ,  0.7458144E+00 ,&
        &  0.7449797E+00 ,  0.7455234E+00 ,  0.7463468E+00 ,  0.7471324E+00 ,  0.7478110E+00 ,&
        &  0.7483951E+00 ,  0.7488887E+00 ,  0.7493176E+00 ,  0.7496843E+00 ,  0.7500048E+00 ,&
        &  0.7502906E+00 ,  0.7505437E+00 ,  0.7507722E+00 ,  0.7509784E+00 ,  0.7511717E+00 ,&
        &  0.7513496E+00 ,  0.7515085E+00 ,  0.7516627E+00 ,  0.7518099E+00 ,  0.7519535E+00 ,&
        &  0.7520828E+00 ,  0.7522048E+00 ,  0.7523221E+00 ,  0.7524346E+00 ,  0.7525373E+00 ,&
        &  0.7526377E+00 ,  0.7527308E+00 ,  0.7528204E+00 ,  0.7529026E+00 ,  0.7529799E+00 ,&
        &  0.7530535E+00 ,  0.7531187E+00 ,  0.7531875E+00 ,  0.7532428E+00 ,  0.7533018E+00 ,&
        &  0.7533510E+00 ,  0.7534038E+00 ,  0.7534468E+00 ,  0.7534886E+00 ,  0.7535291E+00 ,&
        &  0.7535635E+00 ,  0.7536002E+00 ,  0.7536321E+00 ,  0.7536603E+00 ,  0.7536883E+00 ,&
        &  0.7537127E+00 ,  0.7537358E+00 ,  0.7537565E+00 ,  0.7537833E+00 ,  0.7537990E+00 ,&
        &  0.7538208E+00 ,  0.7538351E+00 ,  0.7538520E+00 ,  0.7538651E+00 ,  0.7538794E+00 ,&
        &  0.7538913E+00 ,  0.7539043E+00 ,  0.7539186E+00 ,  0.7539292E+00 ,  0.7539372E+00 ,&
        &  0.7539502E+00 ,  0.7539595E+00 ,  0.7539700E+00 ,  0.7539768E+00 ,  0.7539848E+00 ,&
        &  0.7539940E+00 ,  0.7539983E+00 ,  0.7540087E+00 ,  0.7540154E+00 ,  0.7540221E+00 ,&
        &  0.7540251E+00 ,  0.7540293E+00 ,  0.7540385E+00 ,  0.7540476E+00 ,  0.7540493E+00 ,&
        &  0.7540547E+00 ,  0.7540614E+00 ,  0.7540680E+00 ,  0.7540709E+00 ,  0.7540751E+00 ,&
        &  0.7540830E+00 ,  0.7540871E+00 ,  0.7540875E+00 ,  0.7540966E+00 ,  0.7540970E+00 ,&
        &  0.7540986E+00 ,  0.7541089E+00 ,  0.7541105E+00 ,  0.7541171E+00 ,  0.7541150E+00 ,&
        &  0.7541216E+00 ,  0.7541294E+00 ,  0.7541322E+00 ,  0.7541363E+00 ,  0.7541391E+00 ,&
        &  0.7541432E+00 ,  0.7541472E+00 ,  0.7541513E+00 ,  0.7541516E+00 ,  0.7541607E+00 ,&
        &  0.7541610E+00 ,  0.7541663E+00 ,  0.7541716E+00 ,  0.7541732E+00 ,  0.7541784E+00 ,&
        &  0.7541800E+00 ,  0.7541815E+00 ,  0.7541881E+00 ,  0.7541896E+00 ,  0.7541961E+00 ,&
        &  0.7541939E+00 ,  0.7541967E+00 ,  0.7542032E+00 ,  0.7542059E+00 ,  0.7542099E+00 ,&
        &  0.7542127E+00 ,  0.7542155E+00 ,  0.7542195E+00 ,  0.7542185E+00 ,  0.7542263E+00 ,&
        &  0.7542303E+00 ,  0.7542343E+00 ,  0.7542345E+00 ,  0.7542385E+00 ,  0.7542388E+00 ,&
        &  0.7542428E+00 ,  0.7542480E+00 ,  0.7542482E+00 ,  0.7542534E+00 ,  0.7542499E+00 ,&
        &  0.7542552E+00 ,  0.7542604E+00 ,  0.7542619E+00 ,  0.7542633E+00 ,  0.7542648E+00 ,&
        &  0.7542663E+00 ,  0.7542678E+00 ,  0.7542742E+00 ,  0.7542719E+00 ,  0.7542784E+00 ,&
        &  0.7542761E+00 ,  0.7542788E+00 ,  0.7542815E+00 ,  0.7542842E+00 ,  0.7542869E+00 ,&
        &  0.7542896E+00 ,  0.7542923E+00 ,  0.7542912E+00 ,  0.7542940E+00 ,  0.7542979E+00 ,&
        &  0.7542968E+00 ,  0.7543008E+00 ,  0.7542997E+00 ,  0.7543036E+00 ,  0.7543075E+00 ,&
        &  0.7543065E+00 ,  0.7543067E+00 ,  0.7543106E+00 ,  0.7543107E+00 ,  0.7543109E+00 ,&
        &  0.7543111E+00 ,  0.7543163E+00 ,  0.7543164E+00 ,  0.7543166E+00 ,  0.7543218E+00 ,&
        &  0.7543219E+00 ,  0.7543233E+00 ,  0.7543235E+00 ,  0.7543249E+00 ,  0.7543263E+00 ,&
        &  0.7543264E+00 ,  0.7543278E+00 ,  0.7543292E+00 ,  0.7543306E+00 ,  0.7543320E+00 ,&
        &  0.7543334E+00 ,  0.7543311E+00 ,  0.7543324E+00 ,  0.7543388E+00 ,  0.7543365E+00 ,&
        &  0.7543378E+00 ,  0.7543355E+00 ,  0.7543381E+00 ,  0.7543395E+00 ,  0.7543421E+00 ,&
        &  0.7543398E+00 ,  0.7543424E+00 ,  0.7543450E+00 ,  0.7543426E+00 ,  0.7543452E+00 ,&
        &  0.7543441E+00 ,  0.7543467E+00 ,  0.7543494E+00 ,  0.7543482E+00 ,  0.7543508E+00 /)
      asyice4(:, 25) = (/ &
        &  0.8357430E+00 ,  0.7773391E+00 ,  0.7472338E+00 ,  0.7411460E+00 ,  0.7412388E+00 ,&
        &  0.7423721E+00 ,  0.7434930E+00 ,  0.7444100E+00 ,  0.7451326E+00 ,  0.7457146E+00 ,&
        &  0.7461796E+00 ,  0.7465703E+00 ,  0.7469010E+00 ,  0.7471963E+00 ,  0.7474557E+00 ,&
        &  0.7476933E+00 ,  0.7479149E+00 ,  0.7481155E+00 ,  0.7483045E+00 ,  0.7484807E+00 ,&
        &  0.7486464E+00 ,  0.7488002E+00 ,  0.7489470E+00 ,  0.7490783E+00 ,  0.7492048E+00 ,&
        &  0.7493181E+00 ,  0.7494292E+00 ,  0.7495256E+00 ,  0.7496149E+00 ,  0.7497005E+00 ,&
        &  0.7497753E+00 ,  0.7498465E+00 ,  0.7499090E+00 ,  0.7499631E+00 ,  0.7500185E+00 ,&
        &  0.7500652E+00 ,  0.7501071E+00 ,  0.7501441E+00 ,  0.7501811E+00 ,  0.7502083E+00 ,&
        &  0.7502391E+00 ,  0.7502688E+00 ,  0.7502886E+00 ,  0.7503109E+00 ,  0.7503282E+00 ,&
        &  0.7503468E+00 ,  0.7503605E+00 ,  0.7503754E+00 ,  0.7503878E+00 ,  0.7504027E+00 ,&
        &  0.7504102E+00 ,  0.7504239E+00 ,  0.7504302E+00 ,  0.7504376E+00 ,  0.7504513E+00 ,&
        &  0.7504526E+00 ,  0.7504638E+00 ,  0.7504713E+00 ,  0.7504714E+00 ,  0.7504764E+00 ,&
        &  0.7504864E+00 ,  0.7504889E+00 ,  0.7504964E+00 ,  0.7505039E+00 ,  0.7505040E+00 ,&
        &  0.7505127E+00 ,  0.7505177E+00 ,  0.7505190E+00 ,  0.7505252E+00 ,  0.7505315E+00 ,&
        &  0.7505340E+00 ,  0.7505378E+00 ,  0.7505403E+00 ,  0.7505491E+00 ,  0.7505528E+00 ,&
        &  0.7505529E+00 ,  0.7505617E+00 ,  0.7505629E+00 ,  0.7505679E+00 ,  0.7505692E+00 ,&
        &  0.7505755E+00 ,  0.7505817E+00 ,  0.7505880E+00 ,  0.7505906E+00 ,  0.7505931E+00 ,&
        &  0.7506006E+00 ,  0.7505994E+00 ,  0.7506031E+00 ,  0.7506070E+00 ,  0.7506107E+00 ,&
        &  0.7506145E+00 ,  0.7506195E+00 ,  0.7506195E+00 ,  0.7506245E+00 ,  0.7506295E+00 ,&
        &  0.7506309E+00 ,  0.7506322E+00 ,  0.7506421E+00 ,  0.7506397E+00 ,  0.7506459E+00 ,&
        &  0.7506435E+00 ,  0.7506498E+00 ,  0.7506523E+00 ,  0.7506511E+00 ,  0.7506536E+00 ,&
        &  0.7506611E+00 ,  0.7506599E+00 ,  0.7506588E+00 ,  0.7506625E+00 ,  0.7506663E+00 ,&
        &  0.7506663E+00 ,  0.7506664E+00 ,  0.7506701E+00 ,  0.7506702E+00 ,  0.7506702E+00 ,&
        &  0.7506665E+00 ,  0.7506715E+00 ,  0.7506679E+00 ,  0.7506729E+00 ,  0.7506741E+00 ,&
        &  0.7506754E+00 ,  0.7506717E+00 ,  0.7506692E+00 ,  0.7506755E+00 ,  0.7506731E+00 ,&
        &  0.7506744E+00 ,  0.7506719E+00 ,  0.7506744E+00 ,  0.7506720E+00 ,  0.7506745E+00 ,&
        &  0.7506733E+00 ,  0.7506708E+00 ,  0.7506696E+00 ,  0.7506684E+00 ,  0.7506672E+00 ,&
        &  0.7506697E+00 ,  0.7506648E+00 ,  0.7506635E+00 ,  0.7506673E+00 ,  0.7506661E+00 ,&
        &  0.7506661E+00 ,  0.7506611E+00 ,  0.7506649E+00 ,  0.7506599E+00 ,  0.7506600E+00 ,&
        &  0.7506600E+00 ,  0.7506601E+00 ,  0.7506601E+00 ,  0.7506564E+00 ,  0.7506564E+00 ,&
        &  0.7506564E+00 ,  0.7506527E+00 ,  0.7506540E+00 ,  0.7506540E+00 ,  0.7506503E+00 ,&
        &  0.7506515E+00 ,  0.7506478E+00 ,  0.7506491E+00 ,  0.7506454E+00 ,  0.7506429E+00 ,&
        &  0.7506441E+00 ,  0.7506455E+00 ,  0.7506430E+00 ,  0.7506442E+00 ,  0.7506418E+00 ,&
        &  0.7506393E+00 ,  0.7506406E+00 ,  0.7506381E+00 ,  0.7506356E+00 ,  0.7506331E+00 ,&
        &  0.7506356E+00 ,  0.7506332E+00 ,  0.7506307E+00 ,  0.7506332E+00 ,  0.7506307E+00 ,&
        &  0.7506295E+00 ,  0.7506320E+00 ,  0.7506258E+00 ,  0.7506283E+00 ,  0.7506271E+00 ,&
        &  0.7506246E+00 ,  0.7506234E+00 ,  0.7506259E+00 ,  0.7506247E+00 ,  0.7506234E+00 ,&
        &  0.7506222E+00 ,  0.7506210E+00 ,  0.7506197E+00 ,  0.7506185E+00 ,  0.7506173E+00 ,&
        &  0.7506210E+00 ,  0.7506198E+00 ,  0.7506186E+00 ,  0.7506173E+00 ,  0.7506211E+00 ,&
        &  0.7506161E+00 ,  0.7506199E+00 ,  0.7506186E+00 ,  0.7506186E+00 ,  0.7506174E+00 /)
      asyice4(:, 26) = (/ &
        &  0.8092321E+00 ,  0.7460147E+00 ,  0.7352403E+00 ,  0.7359520E+00 ,  0.7374227E+00 ,&
        &  0.7386078E+00 ,  0.7395314E+00 ,  0.7402644E+00 ,  0.7408577E+00 ,  0.7413321E+00 ,&
        &  0.7417141E+00 ,  0.7420347E+00 ,  0.7423136E+00 ,  0.7425548E+00 ,  0.7427773E+00 ,&
        &  0.7429795E+00 ,  0.7431623E+00 ,  0.7433376E+00 ,  0.7434894E+00 ,  0.7436334E+00 ,&
        &  0.7437586E+00 ,  0.7438697E+00 ,  0.7439677E+00 ,  0.7440575E+00 ,  0.7441354E+00 ,&
        &  0.7442025E+00 ,  0.7442588E+00 ,  0.7443117E+00 ,  0.7443573E+00 ,  0.7443982E+00 ,&
        &  0.7444293E+00 ,  0.7444593E+00 ,  0.7444882E+00 ,  0.7445111E+00 ,  0.7445328E+00 ,&
        &  0.7445520E+00 ,  0.7445702E+00 ,  0.7445810E+00 ,  0.7445980E+00 ,  0.7446138E+00 ,&
        &  0.7446223E+00 ,  0.7446334E+00 ,  0.7446457E+00 ,  0.7446519E+00 ,  0.7446594E+00 ,&
        &  0.7446681E+00 ,  0.7446744E+00 ,  0.7446783E+00 ,  0.7446885E+00 ,  0.7446949E+00 ,&
        &  0.7447026E+00 ,  0.7447029E+00 ,  0.7447094E+00 ,  0.7447123E+00 ,  0.7447214E+00 ,&
        &  0.7447218E+00 ,  0.7447284E+00 ,  0.7447314E+00 ,  0.7447394E+00 ,  0.7447399E+00 ,&
        &  0.7447454E+00 ,  0.7447472E+00 ,  0.7447503E+00 ,  0.7447533E+00 ,  0.7447540E+00 ,&
        &  0.7447583E+00 ,  0.7447602E+00 ,  0.7447658E+00 ,  0.7447690E+00 ,  0.7447722E+00 ,&
        &  0.7447754E+00 ,  0.7447798E+00 ,  0.7447805E+00 ,  0.7447813E+00 ,  0.7447870E+00 ,&
        &  0.7447891E+00 ,  0.7447874E+00 ,  0.7447944E+00 ,  0.7447940E+00 ,  0.7447972E+00 ,&
        &  0.7448018E+00 ,  0.7448063E+00 ,  0.7448072E+00 ,  0.7448081E+00 ,  0.7448139E+00 ,&
        &  0.7448111E+00 ,  0.7448132E+00 ,  0.7448203E+00 ,  0.7448187E+00 ,  0.7448221E+00 ,&
        &  0.7448255E+00 ,  0.7448289E+00 ,  0.7448286E+00 ,  0.7448283E+00 ,  0.7448329E+00 ,&
        &  0.7448376E+00 ,  0.7448386E+00 ,  0.7448395E+00 ,  0.7448405E+00 ,  0.7448415E+00 ,&
        &  0.7448474E+00 ,  0.7448447E+00 ,  0.7448506E+00 ,  0.7448528E+00 ,  0.7448550E+00 ,&
        &  0.7448536E+00 ,  0.7448558E+00 ,  0.7448543E+00 ,  0.7448615E+00 ,  0.7448601E+00 ,&
        &  0.7448636E+00 ,  0.7448621E+00 ,  0.7448656E+00 ,  0.7448654E+00 ,  0.7448689E+00 ,&
        &  0.7448687E+00 ,  0.7448722E+00 ,  0.7448720E+00 ,  0.7448718E+00 ,  0.7448766E+00 ,&
        &  0.7448764E+00 ,  0.7448762E+00 ,  0.7448773E+00 ,  0.7448770E+00 ,  0.7448781E+00 ,&
        &  0.7448779E+00 ,  0.7448790E+00 ,  0.7448801E+00 ,  0.7448848E+00 ,  0.7448809E+00 ,&
        &  0.7448820E+00 ,  0.7448831E+00 ,  0.7448841E+00 ,  0.7448815E+00 ,  0.7448825E+00 ,&
        &  0.7448836E+00 ,  0.7448897E+00 ,  0.7448870E+00 ,  0.7448844E+00 ,  0.7448854E+00 ,&
        &  0.7448878E+00 ,  0.7448851E+00 ,  0.7448862E+00 ,  0.7448885E+00 ,  0.7448859E+00 ,&
        &  0.7448882E+00 ,  0.7448906E+00 ,  0.7448879E+00 ,  0.7448866E+00 ,  0.7448889E+00 ,&
        &  0.7448863E+00 ,  0.7448886E+00 ,  0.7448872E+00 ,  0.7448896E+00 ,  0.7448882E+00 ,&
        &  0.7448905E+00 ,  0.7448891E+00 ,  0.7448878E+00 ,  0.7448864E+00 ,  0.7448887E+00 ,&
        &  0.7448874E+00 ,  0.7448860E+00 ,  0.7448846E+00 ,  0.7448832E+00 ,  0.7448869E+00 ,&
        &  0.7448855E+00 ,  0.7448841E+00 ,  0.7448840E+00 ,  0.7448826E+00 ,  0.7448862E+00 ,&
        &  0.7448812E+00 ,  0.7448848E+00 ,  0.7448834E+00 ,  0.7448833E+00 ,  0.7448782E+00 ,&
        &  0.7448818E+00 ,  0.7448817E+00 ,  0.7448853E+00 ,  0.7448803E+00 ,  0.7448801E+00 ,&
        &  0.7448800E+00 ,  0.7448799E+00 ,  0.7448835E+00 ,  0.7448834E+00 ,  0.7448834E+00 ,&
        &  0.7448832E+00 ,  0.7448831E+00 ,  0.7448793E+00 ,  0.7448792E+00 ,  0.7448791E+00 ,&
        &  0.7448790E+00 ,  0.7448789E+00 ,  0.7448751E+00 ,  0.7448800E+00 ,  0.7448798E+00 ,&
        &  0.7448760E+00 ,  0.7448809E+00 ,  0.7448771E+00 ,  0.7448770E+00 ,  0.7448782E+00 /)
      asyice4(:, 27) = (/ &
        &  0.7731197E+00 ,  0.7285863E+00 ,  0.7274370E+00 ,  0.7293804E+00 ,  0.7308171E+00 ,&
        &  0.7317773E+00 ,  0.7324523E+00 ,  0.7329873E+00 ,  0.7334291E+00 ,  0.7338030E+00 ,&
        &  0.7341189E+00 ,  0.7343940E+00 ,  0.7346279E+00 ,  0.7348329E+00 ,  0.7350105E+00 ,&
        &  0.7351638E+00 ,  0.7352973E+00 ,  0.7354177E+00 ,  0.7355176E+00 ,  0.7356076E+00 ,&
        &  0.7356852E+00 ,  0.7357525E+00 ,  0.7358170E+00 ,  0.7358685E+00 ,  0.7359180E+00 ,&
        &  0.7359570E+00 ,  0.7359940E+00 ,  0.7360313E+00 ,  0.7360689E+00 ,  0.7360982E+00 ,&
        &  0.7361315E+00 ,  0.7361588E+00 ,  0.7361814E+00 ,  0.7362117E+00 ,  0.7362323E+00 ,&
        &  0.7362506E+00 ,  0.7362716E+00 ,  0.7362865E+00 ,  0.7363041E+00 ,  0.7363194E+00 ,&
        &  0.7363322E+00 ,  0.7363428E+00 ,  0.7363511E+00 ,  0.7363606E+00 ,  0.7363678E+00 ,&
        &  0.7363727E+00 ,  0.7363739E+00 ,  0.7363814E+00 ,  0.7363816E+00 ,  0.7363831E+00 ,&
        &  0.7363772E+00 ,  0.7363814E+00 ,  0.7363818E+00 ,  0.7363750E+00 ,  0.7363780E+00 ,&
        &  0.7363688E+00 ,  0.7363696E+00 ,  0.7363666E+00 ,  0.7363600E+00 ,  0.7363584E+00 ,&
        &  0.7363544E+00 ,  0.7363504E+00 ,  0.7363465E+00 ,  0.7363402E+00 ,  0.7363388E+00 ,&
        &  0.7363374E+00 ,  0.7363324E+00 ,  0.7363287E+00 ,  0.7363250E+00 ,  0.7363226E+00 ,&
        &  0.7363202E+00 ,  0.7363178E+00 ,  0.7363167E+00 ,  0.7363156E+00 ,  0.7363108E+00 ,&
        &  0.7363111E+00 ,  0.7363113E+00 ,  0.7363079E+00 ,  0.7363044E+00 ,  0.7363060E+00 ,&
        &  0.7363038E+00 ,  0.7363017E+00 ,  0.7362996E+00 ,  0.7363024E+00 ,  0.7363003E+00 ,&
        &  0.7362995E+00 ,  0.7362986E+00 ,  0.7363028E+00 ,  0.7363020E+00 ,  0.7362975E+00 ,&
        &  0.7363017E+00 ,  0.7363021E+00 ,  0.7363027E+00 ,  0.7363032E+00 ,  0.7363036E+00 ,&
        &  0.7363041E+00 ,  0.7363009E+00 ,  0.7363015E+00 ,  0.7363033E+00 ,  0.7363037E+00 ,&
        &  0.7363056E+00 ,  0.7363024E+00 ,  0.7363042E+00 ,  0.7363060E+00 ,  0.7363078E+00 ,&
        &  0.7363046E+00 ,  0.7363064E+00 ,  0.7363083E+00 ,  0.7363101E+00 ,  0.7363082E+00 ,&
        &  0.7363100E+00 ,  0.7363132E+00 ,  0.7363150E+00 ,  0.7363131E+00 ,  0.7363113E+00 ,&
        &  0.7363181E+00 ,  0.7363162E+00 ,  0.7363143E+00 ,  0.7363175E+00 ,  0.7363156E+00 ,&
        &  0.7363187E+00 ,  0.7363169E+00 ,  0.7363200E+00 ,  0.7363231E+00 ,  0.7363212E+00 ,&
        &  0.7363207E+00 ,  0.7363238E+00 ,  0.7363269E+00 ,  0.7363264E+00 ,  0.7363296E+00 ,&
        &  0.7363290E+00 ,  0.7363284E+00 ,  0.7363279E+00 ,  0.7363310E+00 ,  0.7363304E+00 ,&
        &  0.7363349E+00 ,  0.7363344E+00 ,  0.7363338E+00 ,  0.7363383E+00 ,  0.7363340E+00 ,&
        &  0.7363385E+00 ,  0.7363380E+00 ,  0.7363387E+00 ,  0.7363382E+00 ,  0.7363390E+00 ,&
        &  0.7363397E+00 ,  0.7363442E+00 ,  0.7363449E+00 ,  0.7363457E+00 ,  0.7363465E+00 ,&
        &  0.7363473E+00 ,  0.7363480E+00 ,  0.7363488E+00 ,  0.7363459E+00 ,  0.7363517E+00 ,&
        &  0.7363524E+00 ,  0.7363545E+00 ,  0.7363516E+00 ,  0.7363574E+00 ,  0.7363545E+00 ,&
        &  0.7363566E+00 ,  0.7363536E+00 ,  0.7363557E+00 ,  0.7363578E+00 ,  0.7363599E+00 ,&
        &  0.7363620E+00 ,  0.7363641E+00 ,  0.7363662E+00 ,  0.7363646E+00 ,  0.7363667E+00 ,&
        &  0.7363651E+00 ,  0.7363672E+00 ,  0.7363706E+00 ,  0.7363690E+00 ,  0.7363674E+00 ,&
        &  0.7363745E+00 ,  0.7363729E+00 ,  0.7363763E+00 ,  0.7363747E+00 ,  0.7363744E+00 ,&
        &  0.7363778E+00 ,  0.7363763E+00 ,  0.7363797E+00 ,  0.7363794E+00 ,  0.7363828E+00 ,&
        &  0.7363825E+00 ,  0.7363859E+00 ,  0.7363857E+00 ,  0.7363891E+00 ,  0.7363888E+00 ,&
        &  0.7363886E+00 ,  0.7363883E+00 ,  0.7363880E+00 ,  0.7363914E+00 ,  0.7363961E+00 ,&
        &  0.7363958E+00 ,  0.7363919E+00 ,  0.7363966E+00 ,  0.7363964E+00 ,  0.7363961E+00 /)
      asyice4(:, 28) = (/ &
        &  0.7215502E+00 ,  0.7071316E+00 ,  0.7091225E+00 ,  0.7107826E+00 ,  0.7118016E+00 ,&
        &  0.7124543E+00 ,  0.7129387E+00 ,  0.7133317E+00 ,  0.7136767E+00 ,  0.7139750E+00 ,&
        &  0.7142295E+00 ,  0.7144461E+00 ,  0.7146251E+00 ,  0.7147671E+00 ,  0.7148951E+00 ,&
        &  0.7149901E+00 ,  0.7150719E+00 ,  0.7151338E+00 ,  0.7151886E+00 ,  0.7152315E+00 ,&
        &  0.7152680E+00 ,  0.7152932E+00 ,  0.7153156E+00 ,  0.7153337E+00 ,  0.7153475E+00 ,&
        &  0.7153571E+00 ,  0.7153659E+00 ,  0.7153791E+00 ,  0.7153865E+00 ,  0.7153896E+00 ,&
        &  0.7154005E+00 ,  0.7154058E+00 ,  0.7154103E+00 ,  0.7154176E+00 ,  0.7154242E+00 ,&
        &  0.7154288E+00 ,  0.7154360E+00 ,  0.7154412E+00 ,  0.7154493E+00 ,  0.7154502E+00 ,&
        &  0.7154539E+00 ,  0.7154590E+00 ,  0.7154656E+00 ,  0.7154686E+00 ,  0.7154744E+00 ,&
        &  0.7154767E+00 ,  0.7154804E+00 ,  0.7154805E+00 ,  0.7154821E+00 ,  0.7154850E+00 ,&
        &  0.7154880E+00 ,  0.7154838E+00 ,  0.7154882E+00 ,  0.7154855E+00 ,  0.7154877E+00 ,&
        &  0.7154863E+00 ,  0.7154850E+00 ,  0.7154887E+00 ,  0.7154852E+00 ,  0.7154852E+00 ,&
        &  0.7154818E+00 ,  0.7154832E+00 ,  0.7154812E+00 ,  0.7154791E+00 ,  0.7154785E+00 ,&
        &  0.7154814E+00 ,  0.7154807E+00 ,  0.7154765E+00 ,  0.7154759E+00 ,  0.7154766E+00 ,&
        &  0.7154774E+00 ,  0.7154746E+00 ,  0.7154718E+00 ,  0.7154726E+00 ,  0.7154712E+00 ,&
        &  0.7154734E+00 ,  0.7154756E+00 ,  0.7154742E+00 ,  0.7154729E+00 ,  0.7154714E+00 ,&
        &  0.7154751E+00 ,  0.7154701E+00 ,  0.7154738E+00 ,  0.7154738E+00 ,  0.7154774E+00 ,&
        &  0.7154775E+00 ,  0.7154775E+00 ,  0.7154790E+00 ,  0.7154790E+00 ,  0.7154840E+00 ,&
        &  0.7154805E+00 ,  0.7154855E+00 ,  0.7154870E+00 ,  0.7154835E+00 ,  0.7154849E+00 ,&
        &  0.7154900E+00 ,  0.7154914E+00 ,  0.7154929E+00 ,  0.7154908E+00 ,  0.7154922E+00 ,&
        &  0.7154987E+00 ,  0.7155002E+00 ,  0.7154981E+00 ,  0.7154995E+00 ,  0.7155060E+00 ,&
        &  0.7155039E+00 ,  0.7155054E+00 ,  0.7155082E+00 ,  0.7155097E+00 ,  0.7155126E+00 ,&
        &  0.7155105E+00 ,  0.7155169E+00 ,  0.7155148E+00 ,  0.7155177E+00 ,  0.7155156E+00 ,&
        &  0.7155185E+00 ,  0.7155200E+00 ,  0.7155228E+00 ,  0.7155257E+00 ,  0.7155236E+00 ,&
        &  0.7155265E+00 ,  0.7155294E+00 ,  0.7155273E+00 ,  0.7155302E+00 ,  0.7155331E+00 ,&
        &  0.7155359E+00 ,  0.7155303E+00 ,  0.7155331E+00 ,  0.7155361E+00 ,  0.7155389E+00 ,&
        &  0.7155418E+00 ,  0.7155361E+00 ,  0.7155390E+00 ,  0.7155419E+00 ,  0.7155412E+00 ,&
        &  0.7155441E+00 ,  0.7155434E+00 ,  0.7155463E+00 ,  0.7155456E+00 ,  0.7155485E+00 ,&
        &  0.7155478E+00 ,  0.7155471E+00 ,  0.7155500E+00 ,  0.7155544E+00 ,  0.7155536E+00 ,&
        &  0.7155529E+00 ,  0.7155558E+00 ,  0.7155551E+00 ,  0.7155545E+00 ,  0.7155588E+00 ,&
        &  0.7155581E+00 ,  0.7155574E+00 ,  0.7155617E+00 ,  0.7155610E+00 ,  0.7155603E+00 ,&
        &  0.7155647E+00 ,  0.7155604E+00 ,  0.7155647E+00 ,  0.7155640E+00 ,  0.7155683E+00 ,&
        &  0.7155641E+00 ,  0.7155684E+00 ,  0.7155691E+00 ,  0.7155684E+00 ,  0.7155691E+00 ,&
        &  0.7155685E+00 ,  0.7155692E+00 ,  0.7155735E+00 ,  0.7155743E+00 ,  0.7155700E+00 ,&
        &  0.7155743E+00 ,  0.7155750E+00 ,  0.7155758E+00 ,  0.7155765E+00 ,  0.7155772E+00 ,&
        &  0.7155815E+00 ,  0.7155772E+00 ,  0.7155780E+00 ,  0.7155787E+00 ,  0.7155795E+00 ,&
        &  0.7155802E+00 ,  0.7155774E+00 ,  0.7155831E+00 ,  0.7155839E+00 ,  0.7155846E+00 ,&
        &  0.7155853E+00 ,  0.7155861E+00 ,  0.7155832E+00 ,  0.7155839E+00 ,  0.7155896E+00 ,&
        &  0.7155868E+00 ,  0.7155876E+00 ,  0.7155883E+00 ,  0.7155905E+00 ,  0.7155912E+00 ,&
        &  0.7155883E+00 ,  0.7155890E+00 ,  0.7155913E+00 ,  0.7155920E+00 ,  0.7155941E+00 /)
      asyice4(:, 29) = (/ &
        &  0.5197451E+00 ,  0.7237440E+00 ,  0.7948388E+00 ,  0.8248520E+00 ,  0.8391855E+00 ,&
        &  0.8465656E+00 ,  0.8502502E+00 ,  0.8516648E+00 ,  0.8516752E+00 ,  0.8510135E+00 ,&
        &  0.8502740E+00 ,  0.8499010E+00 ,  0.8501286E+00 ,  0.8510601E+00 ,  0.8526700E+00 ,&
        &  0.8548831E+00 ,  0.8575808E+00 ,  0.8606356E+00 ,  0.8639477E+00 ,  0.8674116E+00 ,&
        &  0.8709596E+00 ,  0.8745100E+00 ,  0.8780335E+00 ,  0.8815061E+00 ,  0.8848826E+00 ,&
        &  0.8881543E+00 ,  0.8913210E+00 ,  0.8943624E+00 ,  0.8972904E+00 ,  0.9000975E+00 ,&
        &  0.9027990E+00 ,  0.9053769E+00 ,  0.9078507E+00 ,  0.9102195E+00 ,  0.9124908E+00 ,&
        &  0.9146658E+00 ,  0.9167460E+00 ,  0.9187411E+00 ,  0.9206474E+00 ,  0.9224906E+00 ,&
        &  0.9242451E+00 ,  0.9259303E+00 ,  0.9275428E+00 ,  0.9290961E+00 ,  0.9305880E+00 ,&
        &  0.9320311E+00 ,  0.9334089E+00 ,  0.9347273E+00 ,  0.9359996E+00 ,  0.9372329E+00 ,&
        &  0.9384100E+00 ,  0.9395376E+00 ,  0.9406384E+00 ,  0.9416875E+00 ,  0.9427077E+00 ,&
        &  0.9436827E+00 ,  0.9446272E+00 ,  0.9455485E+00 ,  0.9464222E+00 ,  0.9472713E+00 ,&
        &  0.9480959E+00 ,  0.9488867E+00 ,  0.9496517E+00 ,  0.9503902E+00 ,  0.9511102E+00 ,&
        &  0.9518030E+00 ,  0.9524767E+00 ,  0.9531222E+00 ,  0.9537477E+00 ,  0.9543612E+00 ,&
        &  0.9549457E+00 ,  0.9555258E+00 ,  0.9560766E+00 ,  0.9566144E+00 ,  0.9571304E+00 ,&
        &  0.9576413E+00 ,  0.9581218E+00 ,  0.9586052E+00 ,  0.9590662E+00 ,  0.9595130E+00 ,&
        &  0.9599456E+00 ,  0.9603719E+00 ,  0.9607751E+00 ,  0.9611809E+00 ,  0.9615629E+00 ,&
        &  0.9619470E+00 ,  0.9623160E+00 ,  0.9626698E+00 ,  0.9630168E+00 ,  0.9633483E+00 ,&
        &  0.9636813E+00 ,  0.9639989E+00 ,  0.9643093E+00 ,  0.9646125E+00 ,  0.9648998E+00 ,&
        &  0.9651970E+00 ,  0.9654782E+00 ,  0.9657432E+00 ,  0.9660183E+00 ,  0.9662681E+00 ,&
        &  0.9665280E+00 ,  0.9667714E+00 ,  0.9670157E+00 ,  0.9672524E+00 ,  0.9674811E+00 ,&
        &  0.9677020E+00 ,  0.9679238E+00 ,  0.9681290E+00 ,  0.9683348E+00 ,  0.9685413E+00 ,&
        &  0.9687399E+00 ,  0.9689305E+00 ,  0.9691217E+00 ,  0.9693047E+00 ,  0.9694883E+00 ,&
        &  0.9696637E+00 ,  0.9698310E+00 ,  0.9699987E+00 ,  0.9701670E+00 ,  0.9703360E+00 ,&
        &  0.9704878E+00 ,  0.9706399E+00 ,  0.9707927E+00 ,  0.9709367E+00 ,  0.9710816E+00 ,&
        &  0.9712265E+00 ,  0.9713634E+00 ,  0.9715005E+00 ,  0.9716380E+00 ,  0.9717579E+00 ,&
        &  0.9718872E+00 ,  0.9720170E+00 ,  0.9721292E+00 ,  0.9722596E+00 ,  0.9723724E+00 ,&
        &  0.9724857E+00 ,  0.9725993E+00 ,  0.9727129E+00 ,  0.9728091E+00 ,  0.9729235E+00 ,&
        &  0.9730291E+00 ,  0.9731352E+00 ,  0.9732324E+00 ,  0.9733299E+00 ,  0.9734276E+00 ,&
        &  0.9735166E+00 ,  0.9736058E+00 ,  0.9737042E+00 ,  0.9737848E+00 ,  0.9738747E+00 ,&
        &  0.9739649E+00 ,  0.9740462E+00 ,  0.9741276E+00 ,  0.9742092E+00 ,  0.9742910E+00 ,&
        &  0.9743642E+00 ,  0.9744462E+00 ,  0.9745197E+00 ,  0.9745932E+00 ,  0.9746668E+00 ,&
        &  0.9747406E+00 ,  0.9748148E+00 ,  0.9748798E+00 ,  0.9749451E+00 ,  0.9750196E+00 ,&
        &  0.9750760E+00 ,  0.9751508E+00 ,  0.9752074E+00 ,  0.9752734E+00 ,  0.9753304E+00 ,&
        &  0.9753966E+00 ,  0.9754447E+00 ,  0.9755112E+00 ,  0.9755686E+00 ,  0.9756262E+00 ,&
        &  0.9756748E+00 ,  0.9757417E+00 ,  0.9757904E+00 ,  0.9758394E+00 ,  0.9758976E+00 ,&
        &  0.9759466E+00 ,  0.9759958E+00 ,  0.9760451E+00 ,  0.9760854E+00 ,  0.9761440E+00 ,&
        &  0.9761935E+00 ,  0.9762341E+00 ,  0.9762840E+00 ,  0.9763246E+00 ,  0.9763744E+00 ,&
        &  0.9764153E+00 ,  0.9764655E+00 ,  0.9764973E+00 ,  0.9765475E+00 ,  0.9765887E+00 ,&
        &  0.9766208E+00 ,  0.9766712E+00 ,  0.9767034E+00 ,  0.9767448E+00 ,  0.9767772E+00 /)

      end subroutine swcldpr

      end module rrtmg_sw_init
