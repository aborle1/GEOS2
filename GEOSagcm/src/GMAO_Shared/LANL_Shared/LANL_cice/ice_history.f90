












!=======================================================================
!
!BOP
!
! !MODULE: ice_history - ice model history files
!

!
! Output files: netCDF or binary data, Fortran unformatted dumps
!
! !REVISION HISTORY:
!  SVN:$Id$
!
! authors Tony Craig and Bruce Briegleb, NCAR
!         Elizabeth C. Hunke and William H. Lipscomb, LANL
!         C. M. Bitz, UW
!
! 2004 WHL: Block structure added 
! 2006 ECH: Accepted some CCSM code into mainstream CICE
!           Added ice_present, aicen, vicen; removed aice1...10, vice1...1.
!           Added histfreq_n and histfreq='h' options, removed histfreq='w'
!           Converted to free source form (F90)
!           Added option for binary output instead of netCDF
!
! !INTERFACE:
!
      module ice_history
!
! !USES:
!
      use ice_kinds_mod
      use ice_broadcast
      use ice_communicate, only: my_task, master_task
      use ice_domain_size
      use ice_read_write
      use ice_fileunits
!
!EOP
!
      implicit none
      save
      
      logical (kind=log_kind) :: &
         hist_avg  ! if true, write averaged data instead of snapshots

      character (len=char_len) :: &
         history_format, & ! file format ('bin'=binary or 'nc'=netcdf)
         history_file  , & ! output file for history
         incond_file       ! output file for snapshot initial conditions

      character (len=char_len_long) :: &
         history_dir   , & ! directory name for history file
         incond_dir        ! directory for snapshot initial conditions

      character (len=char_len_long) :: &
         pointer_file      ! input pointer file for restarts

      !---------------------------------------------------------------
      ! Instructions for adding a field:
      !     Here:
      ! (1) Increase avgsiz
      ! (2) Add to logical flags
      ! (3) Add to namelist (here and also in ice_in)
      ! (4) Assign an index number
      !     In init_hist:
      ! (5) Add to list of fields, vname
      ! (6) Add to field descriptions, vdesc
      ! (7) Add to field units, vunit
      ! (8) Add to field units, vcomment
      ! (9) Add to broadcast list
      ! (10) Load iout array with the logical flag
      ! (11) Specify unit conversion factor if necessary
      ! (12) Increment the field in ice_write_hist
      !---------------------------------------------------------------

      !---------------------------------------------------------------
      ! primary info for the history file
      !---------------------------------------------------------------

      integer (kind=int_kind), parameter :: &
         nvar = 11                  ! number of grid fields that can be written
                                    !   excluding grid vertices

      integer (kind=int_kind) :: &
         ncat_hist             , &  ! number of ice categories written <= ncat
         avgsiz                     ! number of fields that can be written

      real (kind=real_kind) :: time_beg, time_end ! bounds for averaging

      real (kind=dbl_kind), allocatable, &
         dimension (:,:,:,:) :: &
         aa                ! field accumulations and averages

      real (kind=dbl_kind) :: &
         avgct             ! average sample counter

      real (kind=dbl_kind), allocatable :: &
         cona(:)  , & ! multiplicative conversion factor
         conb(:)      ! additive conversion factor

      logical (kind=log_kind), allocatable :: &
         iout(:)  , & ! true if time-dep field is written to output file
         igrd(:)        ! true if grid field is written to output file

      character (len=16), allocatable :: &
         vname(:) , & ! variable names
         vunit(:) , & ! variable units
         vcoord(:), & ! variable coordinates
         vcellmeas(:) ! variable cell measures

      character (len=16), parameter :: &
         tstr  = 'TLON TLAT time', & ! vcoord for T cell quantities
         ustr  = 'ULON ULAT time', & ! vcoord for U cell quantities
         tcstr = 'area: tarea'   , & ! vcellmeas for T cell quantities
         ucstr = 'area: uarea'       ! vcellmeas for U cell quantities

      character (len=55), allocatable :: & 
         vdesc(:) , & ! variable descriptions
         vcomment(:)  ! variable comments

      !---------------------------------------------------------------
      ! logical flags: write to output file if true
      !---------------------------------------------------------------

      logical (kind=log_kind) :: &
           f_tmask     = .true., &
           f_tarea     = .true., f_uarea      = .true., &
           f_dxt       = .true., f_dyt        = .true., &
           f_dxu       = .true., f_dyu        = .true., &
           f_HTN       = .true., f_HTE        = .true., &
           f_ANGLE     = .true., f_ANGLET     = .true., &
           f_bounds    = .true.

      logical (kind=log_kind) :: &
           f_hi        = .true., f_hs         = .true., &
           f_Tsfc      = .true., f_aice       = .true., &
           f_uvel      = .true., f_vvel       = .true., &
           f_fswdn     = .true., f_flwdn      = .true., &
           f_snow      = .true., f_snow_ai    = .true., &
           f_rain      = .true., f_rain_ai    = .true., &
           f_sst       = .true., f_sss        = .true., &
           f_uocn      = .true., f_vocn       = .true., &
           f_frzmlt    = .true., &
           f_fswfac    = .true., &
           f_fswabs    = .true., f_fswabs_ai  = .true., &
           f_albsni    = .true., &
           f_alvdr     = .true., f_alidr      = .true., &
           f_albice    = .true., f_albsno     = .true., &
           f_albpnd    = .true., f_coszen     = .true., &
           f_flat      = .true., f_flat_ai    = .true., &
           f_fsens     = .true., f_fsens_ai   = .true., &
           f_flwup     = .true., f_flwup_ai   = .true., &
           f_evap      = .true., f_evap_ai    = .true., &
           f_Tair      = .true., &
           f_Tref      = .true., f_Qref       = .true., &
           f_congel    = .true., f_frazil     = .true., &
           f_snoice    = .true., f_meltt      = .true., &
           f_meltb     = .true., f_meltl      = .true., &
           f_fresh     = .true., f_fresh_ai   = .true., &
           f_fsalt     = .true., f_fsalt_ai   = .true., &
           f_fhocn     = .true., f_fhocn_ai   = .true., &
           f_fswthru   = .true., f_fswthru_ai = .true., &
           f_strairx   = .true., f_strairy    = .true., &
           f_strtltx   = .true., f_strtlty    = .true., &
           f_strcorx   = .true., f_strcory    = .true., &
           f_strocnx   = .true., f_strocny    = .true., &
           f_strintx   = .true., f_strinty    = .true., &
           f_strength  = .true., f_opening    = .true., &
           f_divu      = .true., f_shear      = .true., &
           f_sig1      = .true., f_sig2       = .true., &
           f_dvidtt    = .true., f_dvidtd     = .true., &
           f_daidtt    = .true., f_daidtd     = .true., &
           f_mlt_onset = .true., f_frz_onset  = .true., &
           f_dardg1dt  = .true., f_dardg2dt   = .true., &
           f_dvirdgdt  = .true., f_iage       = .false.,&
           f_hisnap    = .true., f_aisnap     = .true., &
           f_aicen     = .true., f_vicen      = .true., &
           f_apondn    = .false., &           
           f_trsig     = .true., f_icepresent = .true., &
           f_fsurf_ai  = .true., f_fcondtop_ai= .true., &
           f_fmeltt_ai = .true.,                        &
           f_fsurfn_ai = .true.,f_fcondtopn_ai= .true., &
           f_fmelttn_ai= .true., f_flatn_ai   = .true.

      !---------------------------------------------------------------
      ! namelist variables (same as logical flags)
      !---------------------------------------------------------------

      namelist / icefields_nml /     &
           f_tmask    , &
           f_tarea    , f_uarea    , &
           f_dxt      , f_dyt      , &
           f_dxu      , f_dyu      , &
           f_HTN      , f_HTE      , &
           f_ANGLE    , f_ANGLET   , &
           f_bounds   , &
           f_hi,        f_hs       , &
           f_Tsfc,      f_aice     , &
           f_uvel,      f_vvel     , &
           f_fswdn,     f_flwdn    , &
           f_snow,      f_snow_ai  , &     
           f_rain,      f_rain_ai  , &
           f_sst,       f_sss      , &
           f_uocn,      f_vocn     , &
           f_frzmlt                , &
           f_fswfac                , &
           f_fswabs,    f_fswabs_ai, &
           f_albsni                , &
           f_alvdr,     f_alidr    , &
           f_albice,    f_albsno   , &
           f_albpnd,    f_coszen   , &
           f_flat,      f_flat_ai  , &
           f_fsens,     f_fsens_ai , &
           f_flwup,     f_flwup_ai , &
           f_evap,      f_evap_ai  , &
           f_Tair                  , &
           f_Tref,      f_Qref     , &
           f_congel,    f_frazil   , &
           f_snoice,    f_meltt    , &
           f_meltb,     f_meltl    , &
           f_fresh,     f_fresh_ai , &  
           f_fsalt,     f_fsalt_ai , &
           f_fhocn,     f_fhocn_ai , &
           f_fswthru,   f_fswthru_ai,&
           f_strairx,   f_strairy  , &
           f_strtltx,   f_strtlty  , &
           f_strcorx,   f_strcory  , &
           f_strocnx,   f_strocny  , &
           f_strintx,   f_strinty  , &
           f_strength,  f_opening  , &
           f_divu,      f_shear    , &
           f_sig1,      f_sig2     , &
           f_dvidtt,    f_dvidtd   , &
           f_daidtt,    f_daidtd   , &
           f_mlt_onset, f_frz_onset, &
           f_dardg1dt,  f_dardg2dt , &
           f_dvirdgdt              , &
           f_hisnap,    f_aisnap   , &
           f_aicen,     f_vicen    , &
           f_iage,      f_apondn   , &
           f_trsig,     f_icepresent,&
           f_fsurf_ai,  f_fcondtop_ai,&
           f_fmeltt_ai,              &
           f_fsurfn_ai,f_fcondtopn_ai,&
           f_fmelttn_ai,f_flatn_ai 

      !---------------------------------------------------------------
      ! field indices
      !---------------------------------------------------------------

      integer (kind=int_kind), parameter :: &
           n_tmask      = 1,  &
           n_tarea      = 2,  &
           n_uarea      = 3,  &
           n_dxt        = 4,  &
           n_dyt        = 5,  &
           n_dxu        = 6,  & 
           n_dyu        = 7,  &
           n_HTN        = 8,  &
           n_HTE        = 9,  &
           n_ANGLE      = 10, &
           n_ANGLET     = 11, &

           n_lont_bnds  = 1, &
           n_latt_bnds  = 2, &
           n_lonu_bnds  = 3, &
           n_latu_bnds  = 4, &

           n_hi         = 1,  &
           n_hs         = 2,  &
           n_Tsfc       = 3,  &
           n_aice       = 4,  &
           n_uvel       = 5,  &
           n_vvel       = 6,  &
           n_fswdn      = 7,  &
           n_flwdn      = 8,  &
           n_snow       = 9,  &
           n_snow_ai    = 10, &
           n_rain       = 11, &
           n_rain_ai    = 12, &
           n_sst        = 13, &
           n_sss        = 14, &
           n_uocn       = 15, &
           n_vocn       = 16, &
           n_frzmlt     = 17, &
           n_fswabs     = 18, &
           n_fswabs_ai  = 19, &
           n_albsni     = 20, &
           n_flat       = 21, &
           n_flat_ai    = 22, &
           n_fsens      = 23, &
           n_fsens_ai   = 24, &
           n_flwup      = 25, &
           n_flwup_ai   = 26, &
           n_evap       = 27, &
           n_evap_ai    = 28, &
           n_Tref       = 29, &
           n_Qref       = 30, &
           n_congel     = 31, &
           n_frazil     = 32, &
           n_snoice     = 33, &
           n_meltt      = 34, &
           n_meltb      = 35, &
           n_meltl      = 36, &
           n_fresh      = 37, &
           n_fresh_ai   = 38, &
           n_fsalt      = 39, &
           n_fsalt_ai   = 40, &
           n_fhocn      = 41, &
           n_fhocn_ai   = 42, &
           n_fswthru    = 43, &
           n_fswthru_ai = 44, &
           n_strairx    = 45, &
           n_strairy    = 46, &
           n_strtltx    = 47, &
           n_strtlty    = 48, &
           n_strcorx    = 49, &
           n_strcory    = 50, &
           n_strocnx    = 51, &
           n_strocny    = 52, &
           n_strintx    = 53, &
           n_strinty    = 54, &
           n_strength   = 55, &
           n_divu       = 56, &
           n_shear      = 57, &
           n_sig1       = 58, &
           n_sig2       = 59, &
           n_dvidtt     = 60, &
           n_dvidtd     = 61, &
           n_daidtt     = 62, &
           n_daidtd     = 63, &
           n_mlt_onset  = 64, &
           n_frz_onset  = 65, &
           n_opening    = 66, &
           n_alvdr      = 67, &
           n_alidr      = 68, &
           n_dardg1dt   = 69, &
           n_dardg2dt   = 70, &
           n_dvirdgdt   = 71, &
           n_hisnap     = 72, &
           n_aisnap     = 73, &
           n_Tair       = 74, &
           n_trsig      = 75, &
           n_icepresent = 76, &
           n_iage       = 77, &
           n_fsurf_ai   = 78, &
           n_fcondtop_ai= 79, &
           n_fmeltt_ai  = 80, &
           n_fswfac     = 81, &
           n_albice     = 82, &
           n_albsno     = 83, &
           n_albpnd     = 84, &
           n_coszen     = 85, &
           n_aicen      = 86    ! n_aicen, n_vicen must be last in this list

      integer (kind=int_kind) :: &
           n_vicen        , &
           n_apondn       , &
           n_fsurfn_ai    , &
           n_fcondtopn_ai , &
           n_fmelttn_ai   , &
           n_flatn_ai     

!=======================================================================

      contains

!=======================================================================
!
!BOP
!
! !IROUTINE: alloc_hist - allocate ice_history arrays
!
! !INTERFACE:
!
      subroutine alloc_hist
!
! !DESCRIPTION:
!
! Allocate history arrays
!
! !REVISION HISTORY:
!
! author  Matthew A. Thompson, NASA/GMAO
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!

         ! ---------------------------------------------------------------
         ! MAT: Initialize module level variables that were macroed before
         !      Must be done here since aa needs avgsiz
         ! ---------------------------------------------------------------

         ncat_hist = ncat           ! number of ice categories written <= ncat
         avgsiz = 85 + 7*ncat_hist  ! number of fields that can be written

         n_vicen        = 87 + 1*ncat_hist - 1
         n_apondn       = 87 + 2*ncat_hist - 1
         n_fsurfn_ai    = 87 + 3*ncat_hist - 1
         n_fcondtopn_ai = 87 + 4*ncat_hist - 1
         n_fmelttn_ai   = 87 + 5*ncat_hist - 1
         n_flatn_ai     = 87 + 6*ncat_hist - 1

         allocate(aa(nx_block,ny_block,avgsiz,max_blocks), source=0.0_dbl_kind)

         allocate(cona(avgsiz), source=0.0_dbl_kind)
         allocate(conb(avgsiz), source=0.0_dbl_kind)

         allocate(iout(avgsiz))
         allocate(igrd(nvar))

         allocate(vname(avgsiz))
         allocate(vunit(avgsiz))
         allocate(vcoord(avgsiz))
         allocate(vcellmeas(avgsiz))

         allocate(vdesc(avgsiz))
         allocate(vcomment(avgsiz))

      end subroutine alloc_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: dealloc_hist - deallocate ice_history arrays
!
! !INTERFACE:
!
      subroutine dealloc_hist
!
! !DESCRIPTION:
!
! Deallocate history arrays
!
! !REVISION HISTORY:
!
! author  Matthew A. Thompson, NASA/GMAO
!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!

         deallocate(aa)

         deallocate(cona)
         deallocate(conb)

         deallocate(iout)
         deallocate(igrd)

         deallocate(vname)
         deallocate(vunit)
         deallocate(vcoord)
         deallocate(vcellmeas)

         deallocate(vdesc)
         deallocate(vcomment)

      end subroutine dealloc_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: init_hist - initialize history files
!
! !INTERFACE:
!
      subroutine init_hist (dt)
!
! !DESCRIPTION:
!
! Initialize history files
!
! !REVISION HISTORY:
!
! authors Tony Craig, NCAR
!         Elizabeth C. Hunke, LANL
!         C.M. Bitz, UW
!         Bruce P. Briegleb, NCAR
!         William H. Lipscomb, LANL
!
! !USES:
!
      use ice_constants
      use ice_calendar, only: yday, days_per_year
      use ice_flux, only: mlt_onset, frz_onset, albcnt
      use ice_restart, only: restart
      use ice_age, only: tr_iage
      use ice_exit
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
!
!EOP
!
      integer (kind=int_kind) :: n, k
      integer (kind=int_kind) :: nml_error ! namelist i/o error flag

      character (len=3) :: nchar
      character (len=40) :: tmp

      !---------------------------------------------------------------
      ! field names
      !---------------------------------------------------------------

      vname(n_hi        ) = 'hi'
      vname(n_hs        ) = 'hs'
      vname(n_Tsfc      ) = 'Tsfc'
      vname(n_aice      ) = 'aice'
      vname(n_uvel      ) = 'uvel'
      vname(n_vvel      ) = 'vvel'
      vname(n_fswdn     ) = 'fswdn'
      vname(n_flwdn     ) = 'flwdn'
      vname(n_snow      ) = 'snow'
      vname(n_snow_ai   ) = 'snow_ai'
      vname(n_rain      ) = 'rain'
      vname(n_rain_ai   ) = 'rain_ai'
      vname(n_sst       ) = 'sst'
      vname(n_sss       ) = 'sss'
      vname(n_uocn      ) = 'uocn'
      vname(n_vocn      ) = 'vocn'
      vname(n_frzmlt    ) = 'frzmlt'
      vname(n_fswfac    ) = 'fswfac'
      vname(n_fswabs    ) = 'fswabs'
      vname(n_fswabs_ai ) = 'fswabs_ai'
      vname(n_albsni    ) = 'albsni'  
      vname(n_alvdr     ) = 'alvdr'
      vname(n_alidr     ) = 'alidr'
      vname(n_albice    ) = 'albice'
      vname(n_albsno    ) = 'albsno'
      vname(n_albpnd    ) = 'albpnd'
      vname(n_coszen    ) = 'coszen'
      vname(n_flat      ) = 'flat'  
      vname(n_flat_ai   ) = 'flat_ai'  
      vname(n_fsens     ) = 'fsens'  
      vname(n_fsens_ai  ) = 'fsens_ai'  
      vname(n_flwup     ) = 'flwup'  
      vname(n_flwup_ai  ) = 'flwup_ai'  
      vname(n_evap      ) = 'evap'  
      vname(n_evap_ai   ) = 'evap_ai'  
      vname(n_Tair      ) = 'Tair'
      vname(n_Tref      ) = 'Tref'
      vname(n_Qref      ) = 'Qref'
      vname(n_congel    ) = 'congel'  
      vname(n_frazil    ) = 'frazil'  
      vname(n_snoice    ) = 'snoice'  
      vname(n_meltt     ) = 'meltt'  
      vname(n_meltb     ) = 'meltb'  
      vname(n_meltl     ) = 'meltl'  
      vname(n_fresh     ) = 'fresh' 
      vname(n_fresh_ai  ) = 'fresh_ai' 
      vname(n_fsalt     ) = 'fsalt'  
      vname(n_fsalt_ai  ) = 'fsalt_ai'  
      vname(n_fhocn     ) = 'fhocn'  
      vname(n_fhocn_ai  ) = 'fhocn_ai'
      vname(n_fswthru   ) = 'fswthru ' 
      vname(n_fswthru_ai) = 'fswthru_ai'
      vname(n_strairx   ) = 'strairx'
      vname(n_strairy   ) = 'strairy'
      vname(n_strtltx   ) = 'strtltx'
      vname(n_strtlty   ) = 'strtlty'
      vname(n_strcorx   ) = 'strcorx'
      vname(n_strcory   ) = 'strcory'
      vname(n_strocnx   ) = 'strocnx'
      vname(n_strocny   ) = 'strocny '
      vname(n_strintx   ) = 'strintx'
      vname(n_strinty   ) = 'strinty'
      vname(n_strength  ) = 'strength'
      vname(n_opening   ) = 'opening'
      vname(n_divu      ) = 'divu'
      vname(n_shear     ) = 'shear'
      vname(n_sig1      ) = 'sig1'
      vname(n_sig2      ) = 'sig2'
      vname(n_dvidtt    ) = 'dvidtt'
      vname(n_dvidtd    ) = 'dvidtd' 
      vname(n_daidtt    ) = 'daidtt'
      vname(n_daidtd    ) = 'daidtd' 
      vname(n_mlt_onset ) = 'mlt_onset'
      vname(n_frz_onset ) = 'frz_onset'
      vname(n_dardg1dt  ) = 'dardg1dt' 
      vname(n_dardg2dt  ) = 'dardg2dt'
      vname(n_dvirdgdt  ) = 'dvirdgdt'
      vname(n_hisnap    ) = 'hisnap'
      vname(n_aisnap    ) = 'aisnap'
      vname(n_trsig     ) = 'trsig'
      vname(n_icepresent) = 'ice_present'
      vname(n_iage      ) = 'iage'
      vname(n_fsurf_ai  ) = 'fsurf_ai'
      vname(n_fcondtop_ai)= 'fcondtop_ai'
      vname(n_fmeltt_ai ) = 'fmeltt_ai'
      do n = 1, ncat_hist
        write(nchar,'(i3.3)') n
        write(vname(n_aicen+n-1),'(a,a)') 'aice', trim(nchar) ! aicen
        write(vname(n_vicen+n-1),'(a,a)') 'vice', trim(nchar) ! vicen
        write(vname(n_apondn+n-1),'(a,a)') 'apond', trim(nchar)! apondn
        write(vname(n_fsurfn_ai+n-1),'(a,a)')    &
                      'fsurfn_ai', trim(nchar) ! fsurfn
        write(vname(n_fcondtopn_ai+n-1),'(a,a)') & 
                      'fcondtopn_ai', trim(nchar)  ! fcondtopn
        write(vname(n_fmelttn_ai+n-1),'(a,a)')   &
                      'fmelttn_ai', trim(nchar) ! fmeltn
        write(vname(n_flatn_ai+n-1),'(a,a)')     &
                      'flatn_ai', trim(nchar)   ! flatn
        vname(n_aicen+n-1)        = trim(vname(n_aicen+n-1))
        vname(n_vicen+n-1)        = trim(vname(n_vicen+n-1))
        vname(n_apondn+n-1)       = trim(vname(n_apondn+n-1))
        vname(n_fsurfn_ai+n-1)    = trim(vname(n_fsurfn_ai+n-1))
        vname(n_fcondtopn_ai+n-1) = trim(vname(n_fcondtopn_ai+n-1))
        vname(n_fmelttn_ai+n-1)   = trim(vname(n_fmelttn_ai+n-1))
        vname(n_flatn_ai+n-1)     = trim(vname(n_flatn_ai+n-1))
      enddo

      !---------------------------------------------------------------
      ! field descriptions
      !---------------------------------------------------------------

      vdesc(n_hi        ) = 'grid cell mean ice thickness'  
      vdesc(n_hs        ) = 'grid cell mean snow thickness'
      vdesc(n_Tsfc      ) = 'snow/ice surface temperature'      
      vdesc(n_aice      ) = 'ice area  (aggregate)'
      vdesc(n_uvel      ) = 'ice velocity (x)'            
      vdesc(n_vvel      ) = 'ice velocity (y)'        
      vdesc(n_fswdn     ) = 'down solar flux'             
      vdesc(n_flwdn     ) = 'down longwave flux'
      vdesc(n_snow      ) = 'snowfall rate (cpl)'         
      vdesc(n_snow_ai   ) = 'snowfall rate'
      vdesc(n_rain      ) = 'rainfall rate (cpl)'         
      vdesc(n_rain_ai   ) = 'rainfall rate'
      vdesc(n_sst       ) = 'sea surface temperature'     
      vdesc(n_sss       ) = 'sea surface salinity'
      vdesc(n_uocn      ) = 'ocean current (x)'           
      vdesc(n_vocn      ) = 'ocean current (y)'         
      vdesc(n_frzmlt    ) = 'freeze/melt potential'    
      vdesc(n_fswfac    ) = 'shortwave scaling factor'   
      vdesc(n_fswabs    ) = 'snow/ice/ocn absorbed solar flux (cpl)'   
      vdesc(n_fswabs_ai ) = 'snow/ice/ocn absorbed solar flux'      
      vdesc(n_albsni    ) = 'snw/ice broad band albedo'
      vdesc(n_alvdr     ) = 'visible direct albedo'
      vdesc(n_alidr     ) = 'near IR direct albedo'
      vdesc(n_albice    ) = 'bare ice albedo'
      vdesc(n_albsno    ) = 'snow albedo'
      vdesc(n_albpnd    ) = 'melt pond albedo'
      vdesc(n_coszen    ) = 'cosine of the zenith angle'
      vdesc(n_flat      ) = 'latent heat flux (cpl)'      
      vdesc(n_flat_ai   ) = 'latent heat flux'
      vdesc(n_fsens     ) = 'sensible heat flux (cpl)'    
      vdesc(n_fsens_ai  ) = 'sensible heat flux'
      vdesc(n_flwup     ) = 'upward longwave flx (cpl)'   
      vdesc(n_flwup_ai  ) = 'upward longwave flux'
      vdesc(n_evap      ) = 'evaporative water flux (cpl)'
      vdesc(n_evap_ai   ) = 'evaporative water flux'   
      vdesc(n_Tair      ) = 'air temperature'
      vdesc(n_Tref      ) = '2m reference temperature'
      vdesc(n_Qref      ) = '2m reference specific humidity'
      vdesc(n_congel    ) = 'congelation ice growth'      
      vdesc(n_frazil    ) = 'frazil ice growth'
      vdesc(n_snoice    ) = 'snow-ice formation'          
      vdesc(n_meltt     ) = 'top ice melt'
      vdesc(n_meltb     ) = 'basal ice melt'              
      vdesc(n_meltl     ) = 'lateral ice melt'            
      vdesc(n_fresh     ) = 'freshwtr flx ice to ocn (cpl)'
      vdesc(n_fresh_ai  ) = 'freshwtr flx ice to ocn'
      vdesc(n_fsalt     ) = 'salt flux ice to ocn (cpl)'  
      vdesc(n_fsalt_ai  ) = 'salt flux ice to ocean'   
      vdesc(n_fhocn     ) = 'heat flux ice to ocn (cpl)'  
      vdesc(n_fhocn_ai  ) = 'heat flux ice to ocean'        
      vdesc(n_fswthru   ) = 'SW thru ice to ocean (cpl)'  
      vdesc(n_fswthru_ai) = 'SW flux thru ice to ocean'
      vdesc(n_strairx   ) = 'atm/ice stress (x)'          
      vdesc(n_strairy   ) = 'atm/ice stress (y)'
      vdesc(n_strtltx   ) = 'sea sfc tilt stress (x)'     
      vdesc(n_strtlty   ) = 'sea sfc tilt stress (y)'
      vdesc(n_strcorx   ) = 'coriolis stress (x)'         
      vdesc(n_strcory   ) = 'coriolis stress (y)'
      vdesc(n_strocnx   ) = 'ocean/ice stress (x)'        
      vdesc(n_strocny   ) = 'ocean/ice stress (y)' 
      vdesc(n_strintx   ) = 'internal ice stress (x)'     
      vdesc(n_strinty   ) = 'internal ice stress (y)'  
      vdesc(n_strength  ) = 'compressive ice strength' 
      vdesc(n_opening   ) = 'lead area opening rate'
      vdesc(n_divu      ) = 'strain rate (divergence)'    
      vdesc(n_shear     ) = 'strain rate (shear)'     
      vdesc(n_sig1      ) = 'norm. principal stress 1'    
      vdesc(n_sig2      ) = 'norm. principal stress 2' 
      vdesc(n_dvidtt    ) = 'volume tendency thermo'      
      vdesc(n_dvidtd    ) = 'volume tendency dynamics' 
      vdesc(n_daidtt    ) = 'area tendency thermo'        
      vdesc(n_daidtd    ) = 'area tendency dynamics' 
      vdesc(n_mlt_onset ) = 'melt onset date'            
      vdesc(n_frz_onset ) = 'freeze onset date'
      vdesc(n_dardg1dt  ) = 'ice area ridging rate'       
      vdesc(n_dardg2dt  ) = 'ridge area formation rate'
      vdesc(n_dvirdgdt  ) = 'ice volume ridging rate'     
      vdesc(n_hisnap    ) = 'ice volume snapshot'         
      vdesc(n_aisnap    ) = 'ice area snapshot' 
      vdesc(n_trsig     ) = 'internal stress tensor trace'
      vdesc(n_icepresent) = &
        'fraction of time-avg interval that any ice is present'
      vdesc(n_iage      ) = 'sea ice age'
      vdesc(n_fsurf_ai  ) = 'net surface heat flux'
      vdesc(n_fcondtop_ai)= 'top surface conductive heat flux'
      vdesc(n_fmeltt_ai ) = 'net surface heat flux causing melt'
      do n = 1, ncat_hist
        write(nchar,'(i3)') n

        tmp = 'ice area, category '   ! aicen
        write(vdesc(n_aicen+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_aicen+n-1) = trim(vdesc(n_aicen+n-1))

        tmp = 'ice volume, category ' ! vicen
        write(vdesc(n_vicen+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_vicen+n-1) = trim(vdesc(n_vicen+n-1))

        tmp = 'meltpond concentration, category ' ! apondn
        write(vdesc(n_apondn+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_apondn+n-1) = trim(vdesc(n_apondn+n-1))

        tmp = 'net surface heat flux, category ' ! fsurfn
        write(vdesc(n_fsurfn_ai+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_fsurfn_ai+n-1) = trim(vdesc(n_fsurfn_ai+n-1))

        tmp = 'top sfc conductive heat flux , cat ' ! fcondtopn
        write(vdesc(n_fcondtopn_ai+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_fcondtopn_ai+n-1) = trim(vdesc(n_fcondtopn_ai+n-1))

        tmp = 'net sfc heat flux causing melt, cat ' ! fmelttn
        write(vdesc(n_fmelttn_ai+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_fmelttn_ai+n-1) = trim(vdesc(n_fmelttn_ai+n-1))

        tmp = 'latent heat flux, category ' ! flatn
        write(vdesc(n_flatn_ai+n-1),'(a,2x,a)') trim(tmp), trim(nchar)
        vdesc(n_flatn_ai+n-1) = trim(vdesc(n_flatn_ai+n-1))
      enddo

      !---------------------------------------------------------------
      ! field units
      !---------------------------------------------------------------

      vunit(n_hi        ) = 'm'
      vunit(n_hs        ) = 'm'
      vunit(n_Tsfc      ) = 'degC'
      vunit(n_aice      ) = '1'
      vunit(n_uvel      ) = 'm/s'
      vunit(n_vvel      ) = 'm/s'
      vunit(n_fswdn     ) = 'W/m^2'
      vunit(n_flwdn     ) = 'W/m^2'
      vunit(n_snow      ) = 'cm/day'
      vunit(n_snow_ai   ) = 'cm/day'
      vunit(n_rain      ) = 'cm/day'
      vunit(n_rain_ai   ) = 'cm/day'
      vunit(n_sst       ) = 'C'
      vunit(n_sss       ) = 'psu'
      vunit(n_uocn      ) = 'm/s'
      vunit(n_vocn      ) = 'm/s'
      vunit(n_frzmlt    ) = 'W/m^2'
      vunit(n_fswfac    ) = '1'
      vunit(n_fswabs    ) = 'W/m^2'
      vunit(n_fswabs_ai ) = 'W/m^2'
      vunit(n_albsni    ) = '%'
      vunit(n_alvdr     ) = '%'
      vunit(n_alidr     ) = '%'
      vunit(n_albice    ) = '%'
      vunit(n_albsno    ) = '%'
      vunit(n_albpnd    ) = '%'
      vunit(n_coszen    ) = 'radian'
      vunit(n_flat      ) = 'W/m^2'
      vunit(n_flat_ai   ) = 'W/m^2'
      vunit(n_fsens     ) = 'W/m^2'
      vunit(n_fsens_ai  ) = 'W/m^2'
      vunit(n_flwup     ) = 'W/m^2'
      vunit(n_flwup_ai  ) = 'W/m^2'
      vunit(n_evap      ) = 'cm/day'
      vunit(n_evap_ai   ) = 'cm/day'
      vunit(n_Tair      ) = 'C'
      vunit(n_Tref      ) = 'C'
      vunit(n_Qref      ) = 'g/kg'
      vunit(n_congel    ) = 'cm/day'
      vunit(n_frazil    ) = 'cm/day'
      vunit(n_snoice    ) = 'cm/day'
      vunit(n_meltt     ) = 'cm/day'
      vunit(n_meltb     ) = 'cm/day'
      vunit(n_meltl     ) = 'cm/day'
      vunit(n_fresh     ) = 'cm/day'
      vunit(n_fresh_ai  ) = 'cm/day'
      vunit(n_fsalt     ) = 'kg/m^2/s'
      vunit(n_fsalt_ai  ) = 'kg/m^2/s'
      vunit(n_fhocn     ) = 'W/m^2'
      vunit(n_fhocn_ai  ) = 'W/m^2'
      vunit(n_fswthru   ) = 'W/m^2'
      vunit(n_fswthru_ai) = 'W/m^2'
      vunit(n_strairx   ) = 'N/m^2'
      vunit(n_strairy   ) = 'N/m^2'
      vunit(n_strtltx   ) = 'N/m^2'
      vunit(n_strtlty   ) = 'N/m^2'
      vunit(n_strcorx   ) = 'N/m^2'
      vunit(n_strcory   ) = 'N/m^2'
      vunit(n_strocnx   ) = 'N/m^2'
      vunit(n_strocny   ) = 'N/m^2'
      vunit(n_strintx   ) = 'N/m^2'
      vunit(n_strinty   ) = 'N/m^2'
      vunit(n_strength  ) = 'N/m'
      vunit(n_opening   ) = '%/day'
      vunit(n_divu      ) = '%/day'
      vunit(n_shear     ) = '%/day'
      vunit(n_sig1      ) = ' '
      vunit(n_sig2      ) = ' '
      vunit(n_dvidtt    ) = 'cm/day'
      vunit(n_dvidtd    ) = 'cm/day'
      vunit(n_daidtt    ) = '%/day'
      vunit(n_daidtd    ) = '%/day'
      vunit(n_mlt_onset ) = 'day of year'
      vunit(n_frz_onset ) = 'day of year'
      vunit(n_dardg1dt  ) = '%/day'
      vunit(n_dardg2dt  ) = '%/day'
      vunit(n_dvirdgdt  ) = 'cm/day'
      vunit(n_hisnap    ) = 'm'
      vunit(n_aisnap    ) = ' ' 
      vunit(n_trsig     ) = 'N/m^2'
      vunit(n_icepresent) = '1'
      vunit(n_iage      ) = 'years'
      vunit(n_fsurf_ai  ) = 'W/m^2'
      vunit(n_fcondtop_ai)= 'W/m^2'
      vunit(n_fmeltt_ai ) = 'W/m^2'
      do n = 1, ncat_hist
        vunit(n_aicen+n-1)        = ' ' ! aicen
        vunit(n_vicen+n-1)        = 'm' ! vicen
        vunit(n_apondn+n-1)       = ' ' ! apondn
        vunit(n_fsurfn_ai+n-1)    = 'W/m^2' ! fsurfn
        vunit(n_fcondtopn_ai+n-1) = 'W/m^2' ! fcondtopn
        vunit(n_fmelttn_ai+n-1)   = 'W/m^2' ! fmelttn
        vunit(n_flatn_ai+n-1)     = 'W/m^2' ! flatn
      enddo


      !---------------------------------------------------------------
      ! field comments
      !---------------------------------------------------------------

      vcomment(n_hi        ) = 'ice volume per unit grid cell area'
      vcomment(n_hs        ) = 'snow volume per unit grid cell area'
      vcomment(n_Tsfc      ) = 'averaged with Tf if no ice is present'
      vcomment(n_aice      ) = 'none'
      vcomment(n_uvel      ) = 'positive is x direction on U grid'
      vcomment(n_vvel      ) = 'positive is y direction on U grid'
      vcomment(n_fswdn     ) = 'positive downward'             
      vcomment(n_flwdn     ) = 'positive downward'
      vcomment(n_snow      ) = 'none'         
      vcomment(n_snow_ai   ) = 'weighted by ice area'
      vcomment(n_rain      ) = 'none'         
      vcomment(n_rain_ai   ) = 'weighted by ice area'
      vcomment(n_sst       ) = 'none'     
      vcomment(n_sss       ) = 'none'
      vcomment(n_uocn      ) = 'positive is x direction on U grid'
      vcomment(n_vocn      ) = 'positive is y direction on U grid'
      vcomment(n_frzmlt    ) ='if >0, new ice forms; if <0, ice melts' 
      vcomment(n_fswfac    ) = 'ratio of netsw new:old'   
      vcomment(n_fswabs    ) = 'positive downward'   
      vcomment(n_fswabs_ai ) = 'weighted by ice area'      
      vcomment(n_albsni    ) = 'scaled (divided) by aice'
      vcomment(n_alvdr     ) = 'scaled (divided) by aice'
      vcomment(n_alidr     ) = 'scaled (divided) by aice'
      vcomment(n_albice    ) = 'averaged for coszen>0, weighted by aice'
      vcomment(n_albsno    ) = 'averaged for coszen>0, weighted by aice'
      vcomment(n_albpnd    ) = 'averaged for coszen>0, weighted by aice'
      vcomment(n_coszen    ) = 'negative below horizon'
      vcomment(n_flat      ) = 'positive downward'
      vcomment(n_flat_ai   ) = 'weighted by ice area'
      vcomment(n_fsens     ) = 'positive downward'
      vcomment(n_fsens_ai  ) = 'weighted by ice area'
      vcomment(n_flwup     ) = 'positive downward'
      vcomment(n_flwup_ai  ) = 'weighted by ice area'
      vcomment(n_evap      ) = 'none'
      vcomment(n_evap_ai   ) = 'weighted by ice area'
      vcomment(n_Tair      ) = 'none'
      vcomment(n_Tref      ) = 'none'
      vcomment(n_Qref      ) = 'none'
      vcomment(n_congel    ) = 'none'
      vcomment(n_frazil    ) = 'none'
      vcomment(n_snoice    ) = 'none'
      vcomment(n_meltt     ) = 'none'
      vcomment(n_meltb     ) = 'none'
      vcomment(n_meltl     ) = 'none'
      vcomment(n_fresh     ) = 'if positive, ocean gains fresh water'
      vcomment(n_fresh_ai  ) = 'weighted by ice area'
      vcomment(n_fsalt     ) = 'if positive, ocean gains salt'
      vcomment(n_fsalt_ai  ) = 'weighted by ice area'
      vcomment(n_fhocn     ) = 'if positive, ocean gains heat'
      vcomment(n_fhocn_ai  ) = 'weighted by ice area'
      vcomment(n_fswthru   ) = 'if positive, ocean gains heat'
      vcomment(n_fswthru_ai) = 'weighted by ice area'
      vcomment(n_strairx   ) = 'positive is x direction on U grid'
      vcomment(n_strairy   ) = 'positive is y direction on U grid'
      vcomment(n_strtltx   ) = 'none'
      vcomment(n_strtlty   ) = 'none'
      vcomment(n_strcorx   ) = 'positive is x direction on U grid'
      vcomment(n_strcory   ) = 'positive is y direction on U grid'
      vcomment(n_strocnx   ) = 'positive is x direction on U grid'
      vcomment(n_strocny   ) = 'positive is y direction on U grid'
      vcomment(n_strintx   ) = 'positive is x direction on U grid'
      vcomment(n_strinty   ) = 'positive is y direction on U grid'
      vcomment(n_strength  ) = 'none'
      vcomment(n_opening   ) = 'none'
      vcomment(n_divu      ) = 'none'
      vcomment(n_shear     ) = 'none'
      vcomment(n_sig1      ) = 'sig1 is instantaneous'
      vcomment(n_sig2      ) = 'sig2 is instantaneous'
      vcomment(n_dvidtt    ) = 'none'
      vcomment(n_dvidtd    ) = 'none'
      vcomment(n_daidtt    ) = 'none'
      vcomment(n_daidtd    ) = 'none'
      vcomment(n_mlt_onset ) = 'midyear restart gives erroneous dates'
      vcomment(n_frz_onset ) = 'midyear restart gives erroneous dates'
      vcomment(n_dardg1dt  ) = 'none'
      vcomment(n_dardg2dt  ) = 'none'
      vcomment(n_dvirdgdt  ) = 'none'
      vcomment(n_hisnap    ) = 'none'
      vcomment(n_aisnap    ) = 'none' 
      vcomment(n_trsig     ) = 'ice strength approximation' 
      vcomment(n_icepresent) = 'ice extent flag'
      vcomment(n_iage      ) = 'none' 
      vcomment(n_fsurf_ai  ) = & 
        'positive downwards, excludes conductive flux, weighted by ice area'
      vcomment(n_fcondtop_ai)= 'positive downwards, weighted by ice area'
      vcomment(n_fmeltt_ai ) = 'always >= 0, weighted by ice area'
      do n = 1, ncat_hist
        vcomment(n_aicen+n-1)        = 'Ice range:'           ! aicen
        vcomment(n_vicen+n-1)        = 'none'                 ! vicen
        vcomment(n_apondn+n-1)       = 'none'                 ! apondn
        vcomment(n_fsurfn_ai+n-1)    = 'weighted by ice area' ! fsurfn
        vcomment(n_fcondtopn_ai+n-1) = 'weighted by ice area' ! fcontopn
        vcomment(n_fmelttn_ai+n-1)   = 'weighted by ice area' ! fmelttn
        vcomment(n_flatn_ai+n-1)     = 'weighted by ice area' ! flatn
      enddo

      !-----------------------------------------------------------------
      ! read namelist
      !-----------------------------------------------------------------

      call get_fileunit(nu_nml)
      if (my_task == master_task) then
         open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
         if (nml_error /= 0) then
            nml_error = -1
         else
            nml_error =  1
         endif
         do while (nml_error > 0)
            read(nu_nml, nml=icefields_nml,iostat=nml_error)
            if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
         end do
         if (nml_error == 0) close(nu_nml)
      endif
      call release_fileunit(nu_nml)

      call broadcast_scalar(nml_error, master_task)
      if (nml_error /= 0) then
         close (nu_nml)
         call abort_ice('ice: error reading icefields_nml')
      endif

      if (.not. tr_iage) f_iage = .false.
      f_bounds = .false.

      call broadcast_scalar (f_tmask, master_task)
      call broadcast_scalar (f_tarea, master_task)
      call broadcast_scalar (f_uarea, master_task)
      call broadcast_scalar (f_dxt, master_task)
      call broadcast_scalar (f_dyt, master_task)
      call broadcast_scalar (f_dxu, master_task)
      call broadcast_scalar (f_dyu, master_task)
      call broadcast_scalar (f_HTN, master_task)
      call broadcast_scalar (f_HTE, master_task)
      call broadcast_scalar (f_ANGLE, master_task)
      call broadcast_scalar (f_ANGLET, master_task)
      call broadcast_scalar (f_bounds, master_task)

      call broadcast_scalar (f_hi, master_task)
      call broadcast_scalar (f_hs, master_task)
      call broadcast_scalar (f_Tsfc, master_task)
      call broadcast_scalar (f_aice, master_task)
      call broadcast_scalar (f_uvel, master_task)
      call broadcast_scalar (f_vvel, master_task)
      call broadcast_scalar (f_fswdn, master_task)
      call broadcast_scalar (f_flwdn, master_task)
      call broadcast_scalar (f_snow, master_task)
      call broadcast_scalar (f_snow_ai, master_task)
      call broadcast_scalar (f_rain, master_task)
      call broadcast_scalar (f_rain_ai, master_task)
      call broadcast_scalar (f_sst, master_task)
      call broadcast_scalar (f_sss, master_task)
      call broadcast_scalar (f_uocn, master_task)
      call broadcast_scalar (f_vocn, master_task)
      call broadcast_scalar (f_frzmlt, master_task)
      call broadcast_scalar (f_fswfac, master_task)
      call broadcast_scalar (f_fswabs, master_task)
      call broadcast_scalar (f_fswabs_ai, master_task)
      call broadcast_scalar (f_albsni, master_task)
      call broadcast_scalar (f_alvdr, master_task)
      call broadcast_scalar (f_alidr, master_task)
      call broadcast_scalar (f_albice, master_task)
      call broadcast_scalar (f_albsno, master_task)
      call broadcast_scalar (f_albpnd, master_task)
      call broadcast_scalar (f_coszen, master_task)
      call broadcast_scalar (f_flat, master_task)
      call broadcast_scalar (f_flat_ai, master_task)
      call broadcast_scalar (f_fsens, master_task)
      call broadcast_scalar (f_fsens_ai, master_task)
      call broadcast_scalar (f_flwup, master_task)
      call broadcast_scalar (f_flwup_ai, master_task)
      call broadcast_scalar (f_evap, master_task)
      call broadcast_scalar (f_evap_ai, master_task)
      call broadcast_scalar (f_Tair, master_task)
      call broadcast_scalar (f_Tref, master_task)
      call broadcast_scalar (f_Qref, master_task)
      call broadcast_scalar (f_congel, master_task)
      call broadcast_scalar (f_frazil, master_task)
      call broadcast_scalar (f_snoice, master_task)
      call broadcast_scalar (f_meltt, master_task)
      call broadcast_scalar (f_meltb, master_task)
      call broadcast_scalar (f_meltl, master_task)
      call broadcast_scalar (f_fresh, master_task)
      call broadcast_scalar (f_fresh_ai, master_task)
      call broadcast_scalar (f_fsalt, master_task)
      call broadcast_scalar (f_fsalt_ai, master_task)
      call broadcast_scalar (f_fhocn, master_task)
      call broadcast_scalar (f_fhocn_ai, master_task)
      call broadcast_scalar (f_fswthru, master_task)
      call broadcast_scalar (f_fswthru_ai, master_task)
      call broadcast_scalar (f_strairx, master_task)
      call broadcast_scalar (f_strairy, master_task)
      call broadcast_scalar (f_strtltx, master_task)
      call broadcast_scalar (f_strtlty, master_task)
      call broadcast_scalar (f_strcorx, master_task)
      call broadcast_scalar (f_strcory, master_task)
      call broadcast_scalar (f_strocnx, master_task)
      call broadcast_scalar (f_strocny, master_task)
      call broadcast_scalar (f_strintx, master_task)
      call broadcast_scalar (f_strinty, master_task)
      call broadcast_scalar (f_strength, master_task)
      call broadcast_scalar (f_opening, master_task)
      call broadcast_scalar (f_divu, master_task)
      call broadcast_scalar (f_shear, master_task)
      call broadcast_scalar (f_sig1, master_task)
      call broadcast_scalar (f_sig2, master_task)
      call broadcast_scalar (f_dvidtt, master_task)
      call broadcast_scalar (f_dvidtd, master_task)
      call broadcast_scalar (f_daidtt, master_task)
      call broadcast_scalar (f_daidtd, master_task)
      call broadcast_scalar (f_mlt_onset, master_task)
      call broadcast_scalar (f_frz_onset, master_task)
      call broadcast_scalar (f_dardg1dt, master_task)
      call broadcast_scalar (f_dardg2dt, master_task)
      call broadcast_scalar (f_dvirdgdt, master_task)
      call broadcast_scalar (f_aisnap, master_task)
      call broadcast_scalar (f_hisnap, master_task)
      call broadcast_scalar (f_aicen, master_task)
      call broadcast_scalar (f_vicen, master_task)
      call broadcast_scalar (f_apondn, master_task)
      call broadcast_scalar (f_trsig, master_task)
      call broadcast_scalar (f_icepresent, master_task)
      call broadcast_scalar (f_iage, master_task)
      call broadcast_scalar (f_fsurf_ai, master_task)
      call broadcast_scalar (f_fcondtop_ai, master_task)
      call broadcast_scalar (f_fmeltt_ai, master_task)
      call broadcast_scalar (f_fsurfn_ai, master_task)
      call broadcast_scalar (f_fcondtopn_ai, master_task)
      call broadcast_scalar (f_fmelttn_ai, master_task)
      call broadcast_scalar (f_flatn_ai, master_task)

      !-----------------------------------------------------------------
      ! fill iout array with namelist values
      !-----------------------------------------------------------------

      iout=.true.  ! all fields are written by default
      igrd=.true.

      igrd(n_tmask     ) = f_tmask
      igrd(n_tarea     ) = f_tarea
      igrd(n_uarea     ) = f_uarea
      igrd(n_dxt       ) = f_dxt
      igrd(n_dyt       ) = f_dyt
      igrd(n_dxu       ) = f_dxu
      igrd(n_dyu       ) = f_dyu
      igrd(n_HTN       ) = f_HTN
      igrd(n_HTE       ) = f_HTE
      igrd(n_ANGLE     ) = f_ANGLE
      igrd(n_ANGLET    ) = f_ANGLET

      iout(n_hi        ) = f_hi
      iout(n_hs        ) = f_hs
      iout(n_Tsfc      ) = f_Tsfc  
      iout(n_aice      ) = f_aice  
      iout(n_uvel      ) = f_uvel  
      iout(n_vvel      ) = f_vvel  
      iout(n_fswdn     ) = f_fswdn  
      iout(n_flwdn     ) = f_flwdn  
      iout(n_snow      ) = f_snow
      iout(n_snow_ai   ) = f_snow_ai
      iout(n_rain      ) = f_rain  
      iout(n_rain_ai   ) = f_rain_ai  
      iout(n_sst       ) = f_sst  
      iout(n_sss       ) = f_sss  
      iout(n_uocn      ) = f_uocn  
      iout(n_vocn      ) = f_vocn  
      iout(n_frzmlt    ) = f_frzmlt  
      iout(n_fswfac    ) = f_fswfac
      iout(n_fswabs    ) = f_fswabs  
      iout(n_fswabs_ai ) = f_fswabs_ai  
      iout(n_albsni    ) = f_albsni  
      iout(n_alvdr     ) = f_alvdr
      iout(n_alidr     ) = f_alidr
      iout(n_albice    ) = f_albice
      iout(n_albsno    ) = f_albsno  
      iout(n_albpnd    ) = f_albpnd  
      iout(n_coszen    ) = f_coszen
      iout(n_flat      ) = f_flat  
      iout(n_flat_ai   ) = f_flat_ai  
      iout(n_fsens     ) = f_fsens  
      iout(n_fsens_ai  ) = f_fsens_ai  
      iout(n_flwup     ) = f_flwup  
      iout(n_flwup_ai  ) = f_flwup_ai  
      iout(n_evap      ) = f_evap  
      iout(n_evap_ai   ) = f_evap_ai  
      iout(n_Tair      ) = f_Tair
      iout(n_Tref      ) = f_Tref
      iout(n_Qref      ) = f_Qref
      iout(n_congel    ) = f_congel  
      iout(n_frazil    ) = f_frazil  
      iout(n_snoice    ) = f_snoice  
      iout(n_meltt     ) = f_meltt  
      iout(n_meltb     ) = f_meltb  
      iout(n_meltl     ) = f_meltl  
      iout(n_fresh     ) = f_fresh 
      iout(n_fresh_ai  ) = f_fresh_ai 
      iout(n_fsalt     ) = f_fsalt  
      iout(n_fsalt_ai  ) = f_fsalt_ai  
      iout(n_fhocn     ) = f_fhocn  
      iout(n_fhocn_ai  ) = f_fhocn_ai  
      iout(n_fswthru   ) = f_fswthru  
      iout(n_fswthru_ai) = f_fswthru_ai  
      iout(n_strairx   ) = f_strairx  
      iout(n_strairy   ) = f_strairy  
      iout(n_strtltx   ) = f_strtltx  
      iout(n_strtlty   ) = f_strtlty  
      iout(n_strcorx   ) = f_strcorx  
      iout(n_strcory   ) = f_strcory  
      iout(n_strocnx   ) = f_strocnx
      iout(n_strocny   ) = f_strocny 
      iout(n_strintx   ) = f_strintx  
      iout(n_strinty   ) = f_strinty  
      iout(n_strength  ) = f_strength
      iout(n_opening   ) = f_opening
      iout(n_divu      ) = f_divu  
      iout(n_shear     ) = f_shear  
      iout(n_sig1      ) = f_sig1  
      iout(n_sig2      ) = f_sig2  
      iout(n_dvidtt    ) = f_dvidtt
      iout(n_dvidtd    ) = f_dvidtd  
      iout(n_daidtt    ) = f_daidtt
      iout(n_daidtd    ) = f_daidtd  
      iout(n_mlt_onset ) = f_mlt_onset  
      iout(n_frz_onset ) = f_frz_onset  
      iout(n_dardg1dt  ) = f_dardg1dt  
      iout(n_dardg2dt  ) = f_dardg2dt  
      iout(n_dvirdgdt  ) = f_dvirdgdt 
      iout(n_hisnap    ) = f_hisnap
      iout(n_aisnap    ) = f_aisnap
      iout(n_trsig     ) = f_trsig
      iout(n_icepresent) = f_icepresent
      iout(n_iage      ) = f_iage
      iout(n_fsurf_ai  ) = f_fsurf_ai
      iout(n_fcondtop_ai)= f_fcondtop_ai
      iout(n_fmeltt_ai ) = f_fmeltt_ai
      do n = 1, ncat_hist
        iout(n_aicen+n-1)        = f_aicen
        iout(n_vicen+n-1)        = f_vicen
        iout(n_apondn+n-1)       = f_apondn
        iout(n_fsurfn_ai+n-1)    = f_fsurfn_ai
        iout(n_fcondtopn_ai+n-1) = f_fcondtopn_ai
        iout(n_fmelttn_ai+n-1)   = f_fmelttn_ai
        iout(n_flatn_ai+n-1)     = f_flatn_ai
      enddo

      if (my_task == master_task) then
        write(nu_diag,*) ' '
        write(nu_diag,*) 'The following variables will be ', &
                         'written to the history tape: '
        write(nu_diag,*) ' description                           units', &
             '     netcdf variable'
        do n=1,avgsiz
           if (iout(n)) write(nu_diag,100) vdesc(n), vunit(n), vname(n)
        enddo
        write(nu_diag,*) ' '
      endif
  100 format (1x,a40,2x,a10,2x,a16)

      !-----------------------------------------------------------------
      ! initialize the history arrays
      !-----------------------------------------------------------------
      aa(:,:,:,:) = c0
      avgct = c0
      albcnt(:,:,:) = c0

      do k=1,avgsiz
         cona(k) = c1   ! multiply by 1.
         conb(k) = c0   ! add 0.
      enddo

      if (restart .and. yday >= c2) then
! restarting midyear gives erroneous onset dates
         mlt_onset = 999._dbl_kind 
         frz_onset = 999._dbl_kind 
      else
         mlt_onset = c0
         frz_onset = c0
      endif

      !---------------------------------------------------------------
      ! set conversion factors
      !---------------------------------------------------------------

      cona(n_snow   ) = mps_to_cmpdy/rhofresh  ! snow kg/m2/s to cm/day
      cona(n_snow_ai) = mps_to_cmpdy/rhofresh  ! snow kg/m2/s to cm/day
      cona(n_rain   ) = mps_to_cmpdy/rhofresh  ! rain kg/m2/s to cm/day
      cona(n_rain_ai) = mps_to_cmpdy/rhofresh  ! rain kg/m2/s to cm/day
      cona(n_albsni ) = c100              ! avg of spectral albedos to %
      cona(n_alvdr  ) = c100              ! avg of visible albedo to %
      cona(n_alidr  ) = c100              ! avg of near IR albedo to %
      cona(n_albice ) = c100              ! avg of albedos to %
      cona(n_albsno ) = c100              ! avg of albedos to %
      cona(n_albpnd ) = c100              ! avg of albedos to %
      cona(n_evap   ) = mps_to_cmpdy/rhofresh   ! evap kg/m2/s to cm/day
      cona(n_evap_ai) = mps_to_cmpdy/rhofresh   ! evap kg/m2/s to cm/day
      conb(n_Tref   ) = -tffresh                ! Tref K to C
      cona(n_Qref   ) = kg_to_g                 ! Qref kg/kg to g/kg

      cona(n_congel ) = mps_to_cmpdy/dt  ! congel m per step to cm/day
      cona(n_frazil ) = mps_to_cmpdy/dt  ! frazil m per step to cm/day
      cona(n_snoice ) = mps_to_cmpdy/dt  ! snoice m per step to cm/day
      cona(n_meltt  ) = mps_to_cmpdy/dt  ! meltt  m per step to cm/day
      cona(n_meltb  ) = mps_to_cmpdy/dt  ! meltb  m per step to cm/day
      cona(n_meltl  ) = mps_to_cmpdy/dt  ! meltl  m per step to cm/day
      cona(n_fresh  ) = mps_to_cmpdy/rhofresh ! frshwtr flx kg/m2/s to cm/day
      cona(n_fresh_ai)= mps_to_cmpdy/rhofresh ! frshwtr flx kg/m2/s to cm/day

      cona(n_divu  ) = secday*c100      ! divu from 1/s to %/day
      cona(n_shear ) = secday*c100      ! shear from 1/s to %/day
      cona(n_opening)  = secday*c100    ! opening  frac/s to %/day

      cona(n_dvidtt) = mps_to_cmpdy     ! dvidtt m/s to cm/day
      cona(n_dvidtd) = mps_to_cmpdy     ! dvidtd m/s to cm/day
      cona(n_daidtt) = secday*c100      ! daidtt frac/s to %/day
      cona(n_daidtd) = secday*c100      ! daidtd frac/s to %/day

      cona(n_dardg1dt) = secday*c100    ! dardg1dt frac/s to %/day
      cona(n_dardg2dt) = secday*c100    ! dardg2dt frac/s to %/day
      cona(n_dvirdgdt) = mps_to_cmpdy   ! dvirdgdt m/s to cm/day

      cona(n_iage)   = c1/(secday*days_per_year) ! seconds to years


!------------------------------------------------------------------------
! Change coordinates and cell measures of variables printed out on u grid
!------------------------------------------------------------------------

      if (my_task == master_task) then
        do k=1,avgsiz
          vcoord(k) = tstr 
          if (TRIM(vname(k)) == 'uvel') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'vvel') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'uocn') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'vocn') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strairx') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strairy') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strtltx') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strtlty') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strcorx') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strcory') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strocnx') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strocny') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strintx') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'strinty') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'sig1') vcoord(k) = ustr
          if (TRIM(vname(k)) == 'sig2') vcoord(k) = ustr

          vcellmeas(k) = tcstr 
          if (TRIM(vname(k)) == 'uvel') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'vvel') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'uocn') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'vocn') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strairx') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strairy') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strtltx') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strtlty') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strcorx') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strcory') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strocnx') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strocny') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strintx') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'strinty') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'sig1') vcellmeas(k) = ucstr
          if (TRIM(vname(k)) == 'sig2') vcellmeas(k) = ucstr
        enddo
      endif

      end subroutine init_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: ice_write_hist - write average ice quantities or snapshots
!
! !INTERFACE:
!
      subroutine ice_write_hist (dt)
!
! !DESCRIPTION:
!
! write average ice quantities or snapshots
!
! !REVISION HISTORY:
!
! author:   Elizabeth C. Hunke, LANL
!
! !USES:
!
      use ice_blocks
      use ice_domain
      use ice_grid, only: tmask, lmask_n, lmask_s
      use ice_calendar, only: new_year, secday, yday, write_history, &
                              write_ic, time
      use ice_state
      use ice_constants
      use ice_flux
      use ice_dyn_evp
      use ice_age, only: tr_iage
      use ice_meltpond, only: tr_pond
      use ice_work, only: worka
      use ice_timers
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
!EOP
!
      integer (kind=int_kind) :: &
           i,j,k,n,nct      , &
           iblk             , & ! block index
           ilo,ihi,jlo,jhi      ! beginning and end of physical domain

      real (kind=dbl_kind) :: &
           ravgct           , & ! 1/avgct
           ravgctz          , & ! 1/avgct
           ai               , & ! aice_init
           ain                  ! aicen_init

      type (block) :: &
         this_block           ! block information for current block

      !---------------------------------------------------------------
      ! increment step counter
      !---------------------------------------------------------------

      if (.not. hist_avg) then  ! write snapshots
        aa(:,:,:,:) = c0
        avgct = c1
      else                      ! write averages over time histfreq
        avgct = avgct + c1
        if (avgct == c1) time_beg = (time-dt)/int(secday)
      endif

      !---------------------------------------------------------------
      ! increment field
      !---------------------------------------------------------------

      do iblk = 1, nblocks
       this_block = get_block(blocks_ice(iblk),iblk)         
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi

       do j = jlo, jhi
       do i = ilo, ihi
       if (tmask(i,j,iblk)) then
        ai = aice_init(i,j,iblk)
        aa(i,j,n_hi,    iblk)= aa(i,j,n_hi,    iblk) + vice (i,j,iblk) 
        aa(i,j,n_hs,    iblk)= aa(i,j,n_hs,    iblk) + vsno (i,j,iblk) 
        aa(i,j,n_Tsfc,  iblk)= aa(i,j,n_Tsfc,  iblk) + trcr (i,j,nt_Tsfc,iblk)
        aa(i,j,n_aice,  iblk)= aa(i,j,n_aice,  iblk) + aice (i,j,iblk)
        aa(i,j,n_uvel,  iblk)= aa(i,j,n_uvel,  iblk) + uvel (i,j,iblk)
        aa(i,j,n_vvel,  iblk)= aa(i,j,n_vvel,  iblk) + vvel (i,j,iblk)

        aa(i,j,n_fswdn, iblk)= aa(i,j,n_fswdn, iblk) + fsw  (i,j,iblk) 
        aa(i,j,n_flwdn, iblk)= aa(i,j,n_flwdn, iblk) + flw  (i,j,iblk) 
        aa(i,j,n_snow,  iblk)= aa(i,j,n_snow,  iblk) + fsnow(i,j,iblk) 
        aa(i,j,n_snow_ai,iblk)  =aa(i,j,n_snow_ai,  iblk) &
                                                  + ai*fsnow(i,j,iblk)
        aa(i,j,n_rain,  iblk)= aa(i,j,n_rain,  iblk) + frain(i,j,iblk) 
        aa(i,j,n_rain_ai,iblk) = aa(i,j,n_rain_ai,  iblk) &
                                                  + ai*frain(i,j,iblk)
        aa(i,j,n_sst,   iblk)= aa(i,j,n_sst,   iblk) + sst  (i,j,iblk) 
        aa(i,j,n_sss,   iblk)= aa(i,j,n_sss,   iblk) + sss  (i,j,iblk) 
        aa(i,j,n_uocn,  iblk)= aa(i,j,n_uocn,  iblk) + uocn (i,j,iblk) 
        aa(i,j,n_vocn,  iblk)= aa(i,j,n_vocn,  iblk) + vocn (i,j,iblk) 
        aa(i,j,n_frzmlt,iblk)= aa(i,j,n_frzmlt,iblk) +frzmlt(i,j,iblk) 

        aa(i,j,n_fswfac,iblk)= aa(i,j,n_fswfac,iblk) +fswfac(i,j,iblk)
        aa(i,j,n_fswabs,iblk)= aa(i,j,n_fswabs,iblk) +fswabs(i,j,iblk)
        aa(i,j,n_fswabs_ai,iblk)=aa(i,j,n_fswabs_ai,iblk) &
                                                  +ai*fswabs(i,j,iblk)

        aa(i,j,n_albsni,iblk)= aa(i,j,n_albsni,iblk)  &
                                              + awtvdr*alvdr(i,j,iblk) &
                                              + awtidr*alidr(i,j,iblk) &
                                              + awtvdf*alvdf(i,j,iblk) &
                                              + awtidf*alidf(i,j,iblk)
        aa(i,j,n_alvdr, iblk)= aa(i,j,n_alvdr, iblk) + alvdr(i,j,iblk)
        aa(i,j,n_alidr, iblk)= aa(i,j,n_alidr, iblk) + alidr(i,j,iblk)
        aa(i,j,n_albice,iblk)= aa(i,j,n_albice,iblk) + albice(i,j,iblk)
        aa(i,j,n_albsno,iblk)= aa(i,j,n_albsno,iblk) + albsno(i,j,iblk)
        aa(i,j,n_albpnd,iblk)= aa(i,j,n_albpnd,iblk) + albpnd(i,j,iblk)
        aa(i,j,n_coszen,iblk)= aa(i,j,n_coszen,iblk) + coszen(i,j,iblk)
        aa(i,j,n_flat,  iblk)= aa(i,j,n_flat,  iblk) + flat (i,j,iblk) 
        aa(i,j,n_flat_ai,iblk)  =aa(i,j,n_flat_ai,  iblk)  &
                                                  +  ai*flat(i,j,iblk)
        aa(i,j,n_fsens, iblk)= aa(i,j,n_fsens, iblk) + fsens(i,j,iblk) 
        aa(i,j,n_fsens_ai,iblk) =aa(i,j,n_fsens_ai, iblk)  &
                                                  + ai*fsens(i,j,iblk)
        aa(i,j,n_flwup, iblk)= aa(i,j,n_flwup, iblk) +flwout(i,j,iblk) 
        aa(i,j,n_flwup_ai,iblk) =aa(i,j,n_flwup_ai, iblk)  &
                                                  +ai*flwout(i,j,iblk)
        aa(i,j,n_evap,  iblk)= aa(i,j,n_evap,  iblk) + evap (i,j,iblk) 
        aa(i,j,n_evap_ai, iblk) = aa(i,j,n_evap_ai, iblk)  &
                                                  +  ai*evap(i,j,iblk)
        aa(i,j,n_Tair,  iblk)= aa(i,j,n_Tair,  iblk) + Tair (i,j,iblk) 
        aa(i,j,n_Tref,  iblk)= aa(i,j,n_Tref,  iblk) + Tref (i,j,iblk) 
        aa(i,j,n_Qref,  iblk)= aa(i,j,n_Qref,  iblk) + Qref (i,j,iblk) 
        aa(i,j,n_congel,iblk)= aa(i,j,n_congel,iblk) +congel(i,j,iblk) 
        aa(i,j,n_frazil,iblk)= aa(i,j,n_frazil,iblk) +frazil(i,j,iblk) 
        aa(i,j,n_snoice,iblk)= aa(i,j,n_snoice,iblk) +snoice(i,j,iblk)
        aa(i,j,n_meltt, iblk)= aa(i,j,n_meltt, iblk) + meltt(i,j,iblk)
        aa(i,j,n_meltb, iblk)= aa(i,j,n_meltb, iblk) + meltb(i,j,iblk) 
        aa(i,j,n_meltl, iblk)= aa(i,j,n_meltl, iblk) + meltl(i,j,iblk)
        aa(i,j,n_fresh, iblk)= aa(i,j,n_fresh, iblk)  &
                                                + fresh(i,j,iblk)
        aa(i,j,n_fresh_ai,iblk) = aa(i,j,n_fresh_ai,iblk) &
                                             + fresh_gbm(i,j,iblk)
        aa(i,j,n_fsalt, iblk)   = aa(i,j,n_fsalt, iblk)   &
                                                + fsalt(i,j,iblk)
        aa(i,j,n_fsalt_ai,iblk) = aa(i,j,n_fsalt_ai,iblk) &
                                             + fsalt_gbm(i,j,iblk)
        aa(i,j,n_fhocn, iblk)   = aa(i,j,n_fhocn, iblk)   &
                                                + fhocn(i,j,iblk)
        aa(i,j,n_fhocn_ai,iblk) = aa(i,j,n_fhocn_ai,iblk) &
                                             + fhocn_gbm(i,j,iblk)
        aa(i,j,n_fswthru,iblk)  = aa(i,j,n_fswthru,iblk)  &
                                              + fswthru(i,j,iblk)
        aa(i,j,n_fswthru_ai,iblk)=aa(i,j,n_fswthru_ai,iblk) &
                                           + fswthru_gbm(i,j,iblk)
               
        aa(i,j,n_strairx,iblk) = aa(i,j,n_strairx,iblk)  &
                                                   + strairx(i,j,iblk)
        aa(i,j,n_strairy,iblk) = aa(i,j,n_strairy,iblk)  &
                                                   + strairy(i,j,iblk)
        aa(i,j,n_strtltx,iblk) = aa(i,j,n_strtltx,iblk)  &
                                                   + strtltx(i,j,iblk)
        aa(i,j,n_strtlty,iblk) = aa(i,j,n_strtlty,iblk)  &
                                                   + strtlty(i,j,iblk)
        aa(i,j,n_strcorx,iblk) = aa(i,j,n_strcorx,iblk)  &  
                                         + fm(i,j,iblk)*vvel(i,j,iblk)
        aa(i,j,n_strcory,iblk) = aa(i,j,n_strcory,iblk)  &
                                         - fm(i,j,iblk)*uvel(i,j,iblk)
        aa(i,j,n_strocnx,iblk) = aa(i,j,n_strocnx,iblk)  &
                                                   + strocnx(i,j,iblk)
        aa(i,j,n_strocny,iblk) = aa(i,j,n_strocny,iblk)  &
                                                   + strocny(i,j,iblk)
        aa(i,j,n_strintx,iblk) = aa(i,j,n_strintx,iblk)  &
                                                   + strintx(i,j,iblk)
        aa(i,j,n_strinty,iblk) = aa(i,j,n_strinty,iblk)  &
                                                   + strinty(i,j,iblk)
        aa(i,j,n_strength,iblk)= aa(i,j,n_strength,iblk) &
                                                  + strength(i,j,iblk)

! The following fields (divu, shear, sig1, and sig2) will be smeared
!  if averaged over more than a few days.
! Snapshots may be more useful (see below).

!         aa(i,j,n_divu    ) = aa(i,j,n_divu    ) + divu (i,j,iblk)
!         aa(i,j,n_shear   ) = aa(i,j,n_shear   ) + shear(i,j,iblk)
!         aa(i,j,n_sig1    ) = aa(i,j,n_sig1    ) + sig1 (i,j,iblk)
!         aa(i,j,n_sig2    ) = aa(i,j,n_sig2    ) + sig2 (i,j,iblk)

        aa(i,j,n_dvidtt ,iblk) = aa(i,j,n_dvidtt ,iblk)  &
                                                   + dvidtt(i,j,iblk)
        aa(i,j,n_dvidtd ,iblk) = aa(i,j,n_dvidtd ,iblk)  &
                                                   + dvidtd(i,j,iblk)
        aa(i,j,n_daidtt ,iblk) = aa(i,j,n_daidtt ,iblk)  &
                                                   + daidtt(i,j,iblk)
        aa(i,j,n_daidtd ,iblk) = aa(i,j,n_daidtd ,iblk)  &
                                                   + daidtd(i,j,iblk)
        aa(i,j,n_opening,iblk) = aa(i,j,n_opening,iblk)  &
                                                  + opening(i,j,iblk)
        aa(i,j,n_dardg1dt,iblk)= aa(i,j,n_dardg1dt,iblk) &
                                                 + dardg1dt(i,j,iblk)
        aa(i,j,n_dardg2dt,iblk)= aa(i,j,n_dardg2dt,iblk) &
                                                 + dardg2dt(i,j,iblk)
        aa(i,j,n_dvirdgdt,iblk)= aa(i,j,n_dvirdgdt,iblk) &
                                                 + dvirdgdt(i,j,iblk)
        if (aice(i,j,iblk).gt.puny)  &
           aa(i,j,n_icepresent,iblk) = aa(i,j,n_icepresent,iblk) + c1
        aa(i,j,n_fsurf_ai,iblk)   = aa(i,j,n_fsurf_ai,iblk) &
                                             + fsurf(i,j,iblk)*ai
        aa(i,j,n_fcondtop_ai,iblk)= aa(i,j,n_fcondtop_ai,iblk) &
                                             + fcondtop(i,j,iblk)*ai
       endif                    ! tmask
       enddo                    ! i
       enddo                    ! j

       nct = min(ncat, ncat_hist)
       do n=1,nct
          do j=jlo,jhi
          do i=ilo,ihi
             if (tmask(i,j,iblk)) then
                ain = aicen_init(i,j,n,iblk)
                ! assume consecutive indices
                aa(i,j,n_aicen+n-1,iblk) = aa(i,j,n_aicen+n-1,iblk)  &
                                                + aicen(i,j,n,iblk)
                aa(i,j,n_vicen+n-1,iblk) = aa(i,j,n_vicen+n-1,iblk)  &
                                                + vicen(i,j,n,iblk)
                if (tr_pond) aa(i,j,n_apondn+n-1,iblk) = &
                                           aa(i,j,n_apondn+n-1,iblk)  &
                                           + apondn(i,j,n,iblk)
                aa(i,j,n_fsurfn_ai+n-1,iblk)    = & 
                                        aa(i,j,n_fsurfn_ai+n-1,iblk)  &
                                           + fsurfn(i,j,n,iblk)*ain
                aa(i,j,n_fcondtopn_ai+n-1,iblk) = & 
                                        aa(i,j,n_fcondtopn_ai+n-1,iblk)  &
                                           + fcondtopn(i,j,n,iblk)*ain
                ! Calculate surface heat flux that causes melt as this
                ! is what is calculated by the atmos in HadGEM3 so 
                ! needed for checking purposes
                aa(i,j,n_fmelttn_ai+n-1,iblk)   = &
                                        aa(i,j,n_fmelttn_ai+n-1,iblk) &
                + max(fsurfn(i,j,n,iblk) - fcondtopn(i,j,n,iblk),c0)*ain
                aa(i,j,n_flatn_ai+n-1,iblk)     = & 
                                        aa(i,j,n_flatn_ai+n-1,iblk)  &
                                           + flatn(i,j,n,iblk)*ain
             endif              ! tmask
          enddo                 ! i
          enddo                 ! j
       enddo                    ! n

       ! Calculate aggregate surface melt flux by summing category values
       worka(:,:) = c0
       do j = jlo, jhi
       do i = ilo, ihi
        if (tmask(i,j,iblk)) then
          do n=1,nct 
             worka(i,j)  = worka(i,j) + aa(i,j,n_fmelttn_ai+n-1,iblk)
          enddo            ! n
        endif              ! tmask
      enddo                ! i
      enddo                ! j
      aa(:,:,n_fmeltt_ai,iblk)  = worka(:,:)

      enddo                     ! iblk

      !---------------------------------------------------------------
      ! Write output files at prescribed intervals
      !---------------------------------------------------------------

      if (write_history .or. write_ic) then

      !---------------------------------------------------------------
      ! Mask out land points and convert units 
      !---------------------------------------------------------------

        ravgct = c1/avgct
        do iblk = 1, nblocks
           this_block = get_block(blocks_ice(iblk),iblk)         
           ilo = this_block%ilo
           ihi = this_block%ihi
           jlo = this_block%jlo
           jhi = this_block%jhi

           do k = 1, avgsiz
              do j = jlo, jhi
              do i = ilo, ihi
                 if (.not. tmask(i,j,iblk)) then ! mask out land points
                    aa(i,j,k,iblk) = spval
                 else                            ! convert units
                    aa(i,j,k,iblk) = cona(k)*aa(i,j,k,iblk)*ravgct  &
                                   + conb(k)
                 endif
              enddo             ! i
              enddo             ! j
           enddo                ! k

           ! back out albedo/zenith angle dependence
              do j = jlo, jhi
              do i = ilo, ihi
                 if (tmask(i,j,iblk)) then 
                    ravgctz = c0
                    if (albcnt(i,j,iblk) > puny) &
                        ravgctz = c1/albcnt(i,j,iblk)
                    aa(i,j,n_albice,iblk) = &
                    aa(i,j,n_albice,iblk)*avgct*ravgctz
                    aa(i,j,n_albsno,iblk) = &
                    aa(i,j,n_albsno,iblk)*avgct*ravgctz
                    aa(i,j,n_albpnd,iblk) = &
                    aa(i,j,n_albpnd,iblk)*avgct*ravgctz
                 endif
              enddo             ! i
              enddo             ! j

      !---------------------------------------------------------------
      ! snapshots
      !---------------------------------------------------------------

          ! compute sig1 and sig2
        
           call principal_stress (nx_block,  ny_block,  &
                                  stressp_1 (:,:,iblk), &
                                  stressm_1 (:,:,iblk), &
                                  stress12_1(:,:,iblk), &
                                  prs_sig   (:,:,iblk), &
                                  sig1      (:,:,iblk), &
                                  sig2      (:,:,iblk))
 
           do j = jlo, jhi
           do i = ilo, ihi
              if (.not. tmask(i,j,iblk)) then ! mask out land points
                 aa(i,j,n_divu,iblk)      = spval
                 aa(i,j,n_shear,iblk)     = spval
                 aa(i,j,n_sig1,iblk )     = spval
                 aa(i,j,n_sig2,iblk )     = spval
                 aa(i,j,n_mlt_onset,iblk) = spval
                 aa(i,j,n_frz_onset,iblk) = spval
                 aa(i,j,n_hisnap,iblk)    = spval
                 aa(i,j,n_aisnap,iblk)    = spval
                 aa(i,j,n_trsig,iblk )    = spval
                 aa(i,j,n_iage,iblk )     = spval
              else
                 aa(i,j,n_divu,iblk)  = divu (i,j,iblk)*cona(n_divu)
                 aa(i,j,n_shear,iblk) = shear(i,j,iblk)*cona(n_shear)
                 aa(i,j,n_sig1,iblk)  = sig1 (i,j,iblk)*cona(n_sig1)
                 aa(i,j,n_sig2,iblk)  = sig2 (i,j,iblk)*cona(n_sig2)
                 aa(i,j,n_mlt_onset,iblk) = mlt_onset(i,j,iblk)
                 aa(i,j,n_frz_onset,iblk) = frz_onset(i,j,iblk)
                 aa(i,j,n_hisnap,iblk)    = vice(i,j,iblk)
                 aa(i,j,n_aisnap,iblk)    = aice(i,j,iblk)
                 aa(i,j,n_trsig,iblk )    = p25*(stressp_1(i,j,iblk) &
                                          + stressp_2(i,j,iblk) &
                                          + stressp_3(i,j,iblk) &
                                          + stressp_4(i,j,iblk))
                 if (tr_iage) aa(i,j,n_iage,iblk)  = & 
                       trcr(i,j,nt_iage,iblk)*cona(n_iage)
            endif
           enddo                ! i
           enddo                ! j

        enddo                   ! iblk

        time_end = time/int(secday)

      !---------------------------------------------------------------
      ! write file
      !---------------------------------------------------------------

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (history_format == 'nc') then
        call icecdf         ! netcdf output
      else
        call icebin         ! binary output
      endif

      call ice_timer_stop(timer_readwrite)  ! reading/writing

      !---------------------------------------------------------------
      ! reset to zero
      !------------------------------------------------------------
        aa(:,:,:,:) = c0
        avgct = c0
        albcnt(:,:,:) = c0

      endif  ! write_history or write_ic

      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

         if (new_year) then

            do j=jlo,jhi
            do i=ilo,ihi
               ! reset NH Jan 1
               if (lmask_n(i,j,iblk)) mlt_onset(i,j,iblk) = c0
               ! reset SH Jan 1 
               if (lmask_s(i,j,iblk)) frz_onset(i,j,iblk) = c0
            enddo
            enddo
         endif                  ! new_year

         if ((yday >= 181._dbl_kind) .and. &
             (yday <  181._dbl_kind+dt/secday)) then

            do j=jlo,jhi
            do i=ilo,ihi

               ! reset SH Jul 1
               if (lmask_s(i,j,iblk)) mlt_onset(i,j,iblk) = c0

               ! reset NH Jul 1
               if (lmask_n(i,j,iblk)) frz_onset(i,j,iblk) = c0
            enddo
            enddo

         endif                  ! yday
      enddo                     ! iblk

      write_ic = .false.        ! write initial condition once at most

      end subroutine ice_write_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: icecdf - write netCDF history file
!
! !INTERFACE:
!
      subroutine icecdf
!
! !DESCRIPTION:
!
! write netCDF history file
!
! !REVISION HISTORY:
!
! authors:   E.C.Hunke, LANL
!            Bruce P. Briegleb, NCAR
!
! !USES:
!

      end subroutine icecdf

!=======================================================================
!
!BOP
!
! !IROUTINE: icebin - write binary history file
! This routine writes fewer grid variables compared with the netcdf
! version, to reduce file size.  Grid variables can be obtained from
! the original grid input files.
!
! !INTERFACE:
!
      subroutine icebin
!
! !DESCRIPTION:
!
! write binary history file
!
! !REVISION HISTORY:
!
! authors:   E.C.Hunke, LANL
!
! !USES:
!
      use ice_gather_scatter
      use ice_domain_size
      use ice_constants
      use ice_grid
      use ice_restart, only: lenstr, runid
      use ice_itd, only: c_hi_range
      use ice_calendar, only: write_ic, dayyr, histfreq
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!
      integer (kind=int_kind) :: i,j,n,nrec,nbits
      character (char_len) :: title
      character (char_len_long) :: ncfile, hdrfile

      integer (kind=int_kind) :: icategory,i_aice

      character (char_len) :: current_date,current_time
      character (len=16) :: c_aice
      logical (kind=log_kind) :: diag

      diag = .false.

      if (my_task == master_task) then

        call construct_filename(ncfile,'da')

        ! add local directory path name to ncfile
        if (write_ic) then
          ncfile = trim(incond_dir)//ncfile
        else
          ncfile = trim(history_dir)//ncfile
        endif
        hdrfile = trim(ncfile)//'.hdr'

        !-----------------------------------------------------------------
        ! create history files
        !-----------------------------------------------------------------
        nbits = 32 ! single precision
        call ice_open(nu_history, ncfile, nbits) ! direct access
        open(nu_hdr,file=hdrfile,form='formatted',status='unknown') ! ascii

!echmod call ice_write(nu_history, nrec, work, rda8 or ida4, diag)

        title  = 'sea ice model: Community Ice Code (CICE)'
        write (nu_hdr, 999) 'source',title,' '

        write (nu_hdr, 999) 'file name contains model date',trim(ncfile),' '
        write (nu_hdr, 999) 'calendar','noleap',' '
        write (title,'(a,i3,a)') 'All years have exactly ',int(dayyr),' days'
        write (nu_hdr, 999) 'comment',title,' '
        write (nu_hdr, 999) 'conventions','CICE',' '
        write (nu_hdr, 997) 'missing_value',spval
        write (nu_hdr, 997) '_FillValue',spval

        call date_and_time(date=current_date, time=current_time)
        write (nu_hdr,1000) current_date(1:4), current_date(5:6), &
                                current_date(7:8), current_time(1:2), &
                                current_time(3:4), current_time(5:8)
        write (nu_hdr, *  ) ' '
        write (nu_hdr, *  ) 'Grid size:'
        write (nu_hdr, 998) '  ni',nx_global
        write (nu_hdr, 998) '  nj',ny_global

        write (nu_hdr, *  ) 'Grid variables: (left column = nrec)'
        nrec = 1
        write (nu_hdr, 996) nrec,'tarea','area of T grid cells','m^2'
        write (nu_hdr, *  ) 'History variables: (left column = nrec)'
      endif  ! my_task = master_task
      call ice_write(nu_history, nrec, tarea, 'rda4', diag)

      do n=1,avgsiz
        if (iout(n)) then
          nrec = nrec + 1
          if (my_task == master_task) then
            write (nu_hdr, 996) nrec,trim(vname(n)),trim(vdesc(n)),trim(vunit(n))

            ! Append ice thickness range to aicen comments
            c_aice = TRIM(vname(n))
            i_aice = lenstr(c_aice)
            if (c_aice(1:4) == 'aice' .and. i_aice > 4 ) then
              if (i_aice == 5) then         ! categories 1-9
                read(c_aice(i_aice:i_aice), '(i1)') icategory
              else                          ! categories > 9
                read(c_aice(i_aice-1:i_aice), '(i2)') icategory
              endif
              vcomment(n) = 'Ice range: '//c_hi_range(icategory)
            endif
            write (nu_hdr, 995) nrec,trim(vname(n)),trim(vcomment(n))

            if (histfreq == '1'     .or. .not. hist_avg &
                .or. n==n_divu      .or. n==n_shear     &  ! snapshots
                .or. n==n_sig1      .or. n==n_sig2 .or. n==n_trsig &
                .or. n==n_mlt_onset .or. n==n_frz_onset &
                .or. n==n_hisnap    .or. n==n_aisnap) then
               write (nu_hdr, 996) nrec,trim(vname(n)),'time_rep','instantaneous'
            else
               write (nu_hdr, 996) nrec,trim(vname(n)),'time_rep','averaged'
            endif
          endif

          call ice_write(nu_history, nrec, aa(:,:,n,:), 'rda4', diag)

        endif
      enddo

995     format(i3,2x,a,' comment: ',a)
996     format(i3,2x,a,': ',a,',',2x,a)
997     format(a,': ',es13.6)
998     format(a,': ',i6)
999     format(a,': ',a,2x,a)
1000    format('This dataset was created on ', &
                a,'-',a,'-',a,' at ',a,':',a,':',a)

      if (my_task == master_task) then
        close (nu_hdr)     ! header file
        close (nu_history) ! data file
        write (nu_diag,*) ' '
        write (nu_diag,*) 'Finished writing ',trim(ncfile)
      endif

      end subroutine icebin

!=======================================================================

      subroutine construct_filename(ncfile,suffix)

      use ice_calendar, only: time, sec, idate, nyr, month, daymo,  &
                              mday, write_ic, histfreq, histfreq_n, &
                              year_init, new_year, new_month, new_day, &
                              dayyr, dt
      use ice_restart, only: lenstr

      character (char_len_long), intent(inout) :: ncfile
      character (len=2), intent(in) :: suffix

      integer (kind=int_kind) :: iyear, imonth, iday, isec

        iyear = nyr + year_init - 1 ! set year_init=1 in ice_in to get iyear=nyr
        imonth = month
        iday = mday
        isec = sec - dt

        ! construct filename
        if (write_ic) then
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
              incond_file(1:lenstr(incond_file)),'.',iyear,'-', &
              imonth,'-',iday,'-',isec,'.',suffix
        else

         if (hist_avg) then
          if (histfreq.eq.'h'.or.histfreq.eq.'H') then
           ! do nothing
          elseif (new_year) then
           iyear = iyear - 1
           imonth = 12
           iday = daymo(imonth)
          elseif (new_month) then
           imonth = month - 1
           iday = daymo(imonth)
          elseif (new_day) then
           iday = iday - 1
          endif
         endif

         if (histfreq == '1') then ! instantaneous, write every dt
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix

         elseif (hist_avg) then    ! write averaged data

          if (histfreq.eq.'d'.or.histfreq.eq.'D') then     ! daily
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file)), &
             '.',iyear,'-',imonth,'-',iday,'.',suffix
          elseif (histfreq.eq.'h'.or.histfreq.eq.'H') then ! hourly
           write(ncfile,'(a,a,i2.2,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_',histfreq_n,'h.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
          elseif (histfreq.eq.'m'.or.histfreq.eq.'M') then ! monthly
           write(ncfile,'(a,a,i4.4,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file)),'.', &
             iyear,'-',imonth,'.',suffix
          elseif (histfreq.eq.'y'.or.histfreq.eq.'Y') then ! yearly
           write(ncfile,'(a,a,i4.4,a,a)') &
            history_file(1:lenstr(history_file)),'.', iyear,'.',suffix
          endif

         else                     ! instantaneous with histfreq > dt
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
         endif
        endif

      end subroutine construct_filename

!=======================================================================

      end module ice_history

!=======================================================================
