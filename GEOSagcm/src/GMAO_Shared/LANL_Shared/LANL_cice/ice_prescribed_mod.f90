












!===================================================================
!BOP 
!
! !MODULE: ice_prescribed_mod - Prescribed Ice Model
!
! !DESCRIPTION:
!
! The prescribed ice model reads in ice concentration data from a netCDF
! file.  Ice thickness, temperature, the ice temperature profile are
! prescribed.  Air/ice fluxes are computed to get surface temperature,
! Ice/ocean fluxes are set to zero, and ice dynamics are not calculated.
! Regridding and data cycling capabilities are included.
!
! !REVISION HISTORY:
!  SVN:$Id$
!
! 2010-May-15 - Tony Craig and Mariana Vertenstein - updated to latest streams
! 2006-Aug-22 - D. Bailey, E. Hunke, modified to fit with CICE
! 2005-May-19 - J. Schramm - first version
! 2005-Apr-19 - B. Kauffman, J. Schramm, M. Vertenstein, NCAR - design
!
! !INTERFACE: ----------------------------------------------------------
 
module ice_prescribed_mod

! !USES:


   !use ice_broadcast
   !use ice_communicate, only : my_task, master_task, MPI_COMM_ICE
   use ice_kinds_mod
   use ice_fileunits
   use ice_exit,        only : abort_ice
   use ice_domain_size, only : ncat, nilyr, nslyr, ntilyr, ntslyr,  ntrcr
   use ice_constants
   use ice_state,       only : nt_Tsfc
   !use ice_blocks,     only : nx_block, ny_block
   !use ice_domain,     only : nblocks, distrb_info, blocks_ice
   !use ice_grid,       only : TLAT,TLON,hm,tmask
   !use ice_calendar,   only : idate, sec, calendar_type
   use ice_itd,        only  : ilyr1, slyr1, hin_max
   !use ice_read_write

   implicit none
   save

   private ! except


! !PUBLIC TYPES:

! !PUBLIC MEMBER FUNCTIONS:

   !public :: ice_prescribed_init      ! initialize input data stream
   public :: ice_prescribed_run1       ! get time slices and time interp
   public :: ice_prescribed_run2       ! get time slices and time interp
   !public :: ice_prescribed_phys      ! set prescribed ice state and fluxes

! !PUBLIC DATA MEMBERS:


!EOP


    real (kind=dbl_kind), parameter :: &
       cp_sno = 0.0_dbl_kind & ! specific heat of snow                (J/kg/K)
    ,  rcpi = cp_ice*rhoi & ! heat capacity of fresh ice              (J/m^3)
    ,  rcps = cp_sno*rhos & ! heat capacity of snow                   (J/m^3)
    ,  rcpidepressT = rcpi*depressT & ! param for finding T(z) from q (J/m^3)
    ,  rLfidepressT = rLfi*depressT ! param for heat capacity   (J deg/m^3)
         ! heat capacity of sea ice, rhoi*C=rcpi+rLfidepressT*salinity/T^2

!=======================================================================
contains
!===============================================================================
!BOP
!
  
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ice_prescribed_run1 -- set prescribed ice concentration
!
! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------
 
subroutine ice_prescribed_run1(nx_block,    ny_block,  &
                                            icells,    &
                               indxi,       indxj,     &
                               aicen,       ice_con,   &
                               tile_lat)
                                

! !USES:
 
!   use ice_flux
!  use ice_grid, only : bound
!   use ice_state
!   use ice_itd, only  : aggregate
!   use ice_dyn_evp

   implicit none
 
! !INPUT/OUTPUT PARAMETERS:

     integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells with ice present

     integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj     ! compressed indices for cells with ice

     real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen  ! concentration of ice

     real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         ice_con  ! concentration of ice

     real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         tile_lat  ! latitude of tiles

 
!EOP

   !----- Local ------
   integer(kind=int_kind) :: layer    ! level index
   integer(kind=int_kind) :: nc       ! ice category index
   integer(kind=int_kind) :: i,j,k    ! longitude, latitude and level indices
   integer(kind=int_kind) :: iblk

   real(kind=dbl_kind) :: hi        ! ice prescribed (hemispheric) ice thickness

   real(kind=dbl_kind) :: ice_cov(nx_block,ny_block) ! ice cover 


   !-----------------------------------------------------------------
   ! Initialize ice state
   !-----------------------------------------------------------------

   ! TODO  - can we now get rid of the following???

     aicen(:,:,:) = c0
   !  vicen(:,:,:,:) = c0
   !  eicen(:,:,:,:) = c0
   
   !  do nc=1,ncat
   !     trcrn(:,:,nt_Tsfc,nc,:) = Tf(:,:,:)
   !  enddo
   
   !-----------------------------------------------------------------
   ! Set ice cover over land to zero, not sure if this should be
   ! be done earier, before time/spatial interp??????
   !-----------------------------------------------------------------
   do j = 1,ny_block
   do i = 1,nx_block
         ice_cov(i,j) = ice_con(i,j)
         if (ice_cov(i,j) .lt. eps04) ice_cov(i,j) = c0
         if (ice_cov(i,j) .gt. c1)    ice_cov(i,j) = c1
   enddo
   enddo

   do j = 1,ny_block
   do i = 1,nx_block

         !--------------------------------------------------------------
         ! Place ice where ice concentration > .0001
         !--------------------------------------------------------------
         if (ice_cov(i,j) >= eps04) then

            hi = 0.0_dbl_kind
            !----------------------------------------------------------
            ! Set ice thickness in each hemisphere
            !----------------------------------------------------------
            if(tile_lat(i,j)*rad_to_deg > 40.0_dbl_kind) then
              hi  = 2.0_dbl_kind
            else if(tile_lat(i,j)*rad_to_deg < -40.0_dbl_kind) then
              hi  = 1.0_dbl_kind
            end if

            !----------------------------------------------------------
            ! All ice in appropriate thickness category
            !----------------------------------------------------------
            do nc = 1,ncat

              if(hin_max(nc-1) < hi .and. hi < hin_max(nc)) then
                  aicen(i,j,nc) = ice_cov(i,j)
               end if    ! hin_max
            enddo        ! ncat
         else
            aicen(i,j,:) = c0
         end if          ! ice_cov >= eps04
   enddo                 ! i
   enddo                 ! j

end subroutine ice_prescribed_run1

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ice_prescribed_run2 -- set prescribed ice state and fluxes
!
! !DESCRIPTION:
!
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------
 
subroutine ice_prescribed_run2(nx_block,    ny_block,  &
                                            icells,    &
                               indxi,       indxj,     &
                               aicen,       trcrn,     &
                               vicen,       vsnon,     &
                               eicen,       esnon,     &
                               aice0,       aice,      &
                               ice_con,     Tf,        &
                               tile_lat,    tnh,       &
                               tsh)
                                

! !USES:
 
!   use ice_flux
!  use ice_grid, only : bound
!   use ice_state
!   use ice_itd, only  : aggregate
!   use ice_dyn_evp

   implicit none
 
! !INPUT/OUTPUT PARAMETERS:

     integer (kind=int_kind), intent(in) :: &
         nx_block, ny_block, & ! block dimensions
         icells                ! number of cells with ice present

     integer (kind=int_kind), dimension (nx_block*ny_block), &
         intent(in) :: &
         indxi, indxj     ! compressed indices for cells with ice

     real (kind=dbl_kind), dimension (nx_block,ny_block,ncat), &
         intent(inout) :: &
         aicen  , & ! concentration of ice
         vicen  , & ! volume per unit area of ice      (m)
         vsnon      ! volume per unit area of snow     (m)


     real (kind=dbl_kind), dimension (nx_block,ny_block,ntrcr,ncat), &
         intent(inout) :: &
         trcrn     ! ice tracers

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntilyr), &
         intent(inout) :: &
         eicen     ! energy of melting for each ice layer (J/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block,ntslyr), &
         intent(inout) :: &
         esnon     ! energy of melting for each snow layer (J/m^2)

      real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(inout) :: &
         aice  , & ! concentration of ice
         aice0     ! concentration of open water

     real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         ice_con,  &  ! concentration of ice
         Tf           ! freezing temp. 
 
     real (kind=dbl_kind), dimension (nx_block,ny_block), &
         intent(in) :: &
         tile_lat  ! latitude of tiles

     real (kind=real_kind),                                &
         intent(in) :: &
         tnh, tsh  ! prescribed ice thick in nh and sh
 
! !INPUT/OUTPUT PARAMETERS:
 
!EOP

   !----- Local ------
   integer(kind=int_kind) :: layer    ! level index
   integer(kind=int_kind) :: nc       ! ice category index
   integer(kind=int_kind) :: i,j,k    ! longitude, latitude and level indices
   integer(kind=int_kind) :: ij

   real(kind=dbl_kind) :: slope     ! diff in underlying ocean tmp and ice surface tmp
   real(kind=dbl_kind) :: Ti        ! ice level temperature
   real(kind=dbl_kind) :: Tmlt      ! ice level melt temperature
   real(kind=dbl_kind) :: qin_save(nilyr) 
   real(kind=dbl_kind) :: qsn_save(nslyr)
   real(kind=dbl_kind) :: hi        ! ice prescribed (hemispheric) ice thickness
   real(kind=dbl_kind) :: hs        ! snow thickness
   real(kind=dbl_kind) :: zn        ! normalized ice thickness
   real(kind=dbl_kind) :: salin(nilyr)  ! salinity (ppt) 
   real(kind=dbl_kind) :: ice_cov(nx_block,ny_block) ! ice cover 

   real(kind=dbl_kind), parameter :: nsal    = 0.407_dbl_kind
   real(kind=dbl_kind), parameter :: msal    = 0.573_dbl_kind
   real(kind=dbl_kind), parameter :: saltmax = 3.2_dbl_kind   ! max salinity at ice base (ppm)

   !-----------------------------------------------------------------
   ! Initialize ice state
   !-----------------------------------------------------------------

   ! TODO  - can we now get rid of the following???

   !  aicen(:,:,:,:) = c0
   !  vicen(:,:,:,:) = c0
   !  eicen(:,:,:,:) = c0
   
   !  do nc=1,ncat
   !     trcrn(:,:,nt_Tsfc,nc,:) = Tf(:,:,:)
   !  enddo
   
   !-----------------------------------------------------------------
   ! Set ice cover over land to zero, not sure if this should be
   ! be done earier, before time/spatial interp??????
   !-----------------------------------------------------------------
   !do iblk = 1,nblocks
   do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)
         ice_cov(i,j) = ice_con(i,j)
         if (ice_cov(i,j) .lt. eps04) ice_cov(i,j) = c0
         if (ice_cov(i,j) .gt. c1)    ice_cov(i,j) = c1
   enddo
   !enddo

   !do iblk = 1,nblocks
   do ij = 1, icells
         i = indxi(ij)
         j = indxj(ij)


         !--------------------------------------------------------------
         ! Place ice where ice concentration > .0001
         !--------------------------------------------------------------
         if (ice_cov(i,j) >= eps04) then

            hi = 0.0_dbl_kind
            !----------------------------------------------------------
            ! Set ice thickness in each hemisphere
            !----------------------------------------------------------
            if(tile_lat(i,j)*rad_to_deg > 30.0_dbl_kind) then
              hi  = tnh
            else if(tile_lat(i,j)*rad_to_deg < -30.0_dbl_kind) then
              hi  = tsh
            end if

            do nc = 1,ncat
              if(hin_max(nc-1) < hi .and. hi < hin_max(nc)) then
              elseif(aicen(i,j,nc) > puny) then
                  trcrn(i,j,nt_Tsfc,nc) = Tf(i,j)
                  aicen(i,j,nc) = c0
                  vicen(i,j,nc) = c0
                  vsnon(i,j,nc) = c0
                  do k=1,nilyr
                     eicen(i,j,ilyr1(nc)+k-1) = c0
                  enddo
                  do k=1,nslyr
                     esnon(i,j,slyr1(nc)+k-1) = c0
                  enddo
              endif
            enddo

            !----------------------------------------------------------
            ! All ice in appropriate thickness category
            !----------------------------------------------------------
            do nc = 1,ncat

              if(hin_max(nc-1) < hi .and. hi < hin_max(nc)) then

                  if (aicen(i,j,nc) > c0) then
                     hs = vsnon(i,j,nc) / aicen(i,j,nc)
                  else
                     hs = c0
                  endif

                  qin_save(:) = c0
                  qsn_save(:) = c0

                  if (vicen(i,j,nc) > c0) then
                     do k=1,nilyr
                        qin_save(k) = eicen(i,j,ilyr1(nc)+k-1)         &
                                    * real(nilyr,kind=dbl_kind)             &
                                    / Vicen(i,j,nc)
                     enddo
                  endif

                  if (vsnon(i,j,nc) > c0) then
                     do k=1,nslyr
                        qsn_save(k) = esnon(i,j,slyr1(nc)+k-1)         &
                                    * real(nslyr,kind=dbl_kind)             &
                                    / vsnon(i,j,nc)
                     enddo
                  endif

                  aicen(i,j,nc) = ice_cov(i,j)
                  vicen(i,j,nc) = hi*aicen(i,j,nc) 
                  vsnon(i,j,nc) = hs*aicen(i,j,nc) 

                  ! remember enthalpy profile to compute energy
                  do k=1,nilyr
                     eicen(i,j,ilyr1(nc)+k-1)                          &
                        = qin_save(k) * vicen(i,j,nc)                  &
                        / real(nilyr,kind=dbl_kind)
                  enddo

                  do k=1,nslyr
                     esnon(i,j,slyr1(nc)+k-1)                          &
                        = qsn_save(k) * vsnon(i,j,nc)                  &
                        / real(nslyr,kind=dbl_kind)
                  enddo

                  !---------------------------------------------------------
                  ! if this is a tile without ice at prev time step,
                  ! the enthalpy will be zero hence it needs to be
                  ! somehow prescribed based on a linear temp profile 

                  ! make linear temp profile and compute enthalpy
                  !---------------------------------------------------------

                  if (abs(eicen(i,j,ilyr1(nc))) < puny) then

                  if (aice(i,j) < puny) &
                     trcrn(i,j,nt_Tsfc,nc) = Tf(i,j)

                  slope = Tf(i,j) - trcrn(i,j,nt_Tsfc,nc)
                  do k = 1, nilyr
                     zn = (real(k,kind=dbl_kind)-p5) / real(nilyr,kind=dbl_kind)
                     Ti = trcrn(i,j,nt_Tsfc,nc) + slope*zn
                     salin(k) = (saltmax/c2)*(c1-cos(pi*zn**(nsal/(msal+zn))))
                     Tmlt = -salin(k)*depressT
                     eicen(i,j,ilyr1(nc)+k-1) =                        &
                       -(rhoi * (cp_ice*(Tmlt-Ti) &
                       + Lfresh*(c1-Tmlt/Ti) - cp_ocn*Tmlt)) &
                       * vicen(i,j,nc)/real(nilyr,kind=dbl_kind)
                  enddo

                  do k=1,nslyr
                     esnon(i,j,slyr1(nc)+k-1) =                       &
                        -rhos*(Lfresh - cp_ice*trcrn(i,j,nt_Tsfc,nc)) &
                         *vsnon(i,j,nc)
                  enddo

                  endif  ! aice < puny
               end if    ! hin_max
            enddo        ! ncat
         else
            trcrn(i,j,nt_Tsfc,:) = Tf(i,j)
            aicen(i,j,:) = c0
            vicen(i,j,:) = c0
            vsnon(i,j,:) = c0
            esnon(i,j,:) = c0
            eicen(i,j,:) = c0
         end if          ! ice_cov >= eps04
         aice0(i,j) = max(c1 - sum(aicen(i,j,:)), c0)
   enddo                 ! ij

   !--------------------------------------------------------------------
   ! compute aggregate ice state and open water area
   !--------------------------------------------------------------------
   !call aggregate (nx_block, ny_block,                      &
   !                aicen(:,:,:,iblk),  trcrn(:,:,:,:,iblk), &
   !                vicen(:,:,:,iblk),  vsnon(:,:,:,iblk),   &
   !                eicen(:,:,:,iblk),  esnon(:,:,:,iblk),   &
   !                aice(:,:,iblk),     trcr(:,:,:,iblk),    &
   !                vice(:,:,iblk),     vsno(:,:,iblk),      &
   !                eice(:,:,iblk),     esno(:,:,iblk),      &
   !                aice0(:,:,iblk),    tmask(:,:,iblk),     &
   !                ntrcr,              trcr_depend) 

   !enddo                 ! iblk

   !do iblk = 1, nblocks
   !do j = 1, ny_block
   !  do i = 1, nx_block
   !    aice_init(i,j,iblk) = aice(i,j,iblk)
   !  enddo
   !enddo
   !enddo

   !--------------------------------------------------------------------
   ! set non-computed fluxes, ice velocities, ice-ocn stresses to zero
   !--------------------------------------------------------------------

   !frzmlt    (:,:,:) = c0
   !uvel      (:,:,:) = c0
   !vvel      (:,:,:) = c0
   !strocnxT  (:,:,:) = c0
   !strocnyT  (:,:,:) = c0

   !-----------------------------------------------------------------
   ! other atm and ocn fluxes
   !-----------------------------------------------------------------
   !call init_flux_atm
   !call init_flux_ocn

end subroutine ice_prescribed_run2

!==============================================================================

end module ice_prescribed_mod

!==============================================================================
