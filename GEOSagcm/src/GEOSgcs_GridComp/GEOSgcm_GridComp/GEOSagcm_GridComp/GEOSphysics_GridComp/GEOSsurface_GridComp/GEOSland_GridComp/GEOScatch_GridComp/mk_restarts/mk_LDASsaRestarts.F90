PROGRAM mk_LDASsaRestarts

! This program regrids catch/catchCN restarts from archieved M09 and M36 global simulations
! The start day is restrcited to January 1 of any year
! Follow these steps:
! (1) /home/smahanam/bin/interactive.py -A sp3 -n 36 -a YOUR_SPONSOR_CODE -X --debug (get an interactive node)
! (2) setenv ESMADIR .....; source $ESMADIR/src/g5_modules; limit stacksize unlimited
! (3) goto your EXPDIR
! (4) ln -s $ESMADIR/Linux/bin
! (5) mkdir -p OutData1/  OutData2/
! (6) ln -s BCSDIR/TilFile OutData1/OutTileFile
! (7) ln -s BCSDIR/TilFile OutData2/OutTileFile
! (8) ln -s BCSDIR/clsm OutData2/clsm
! (9) mpirun -np 36 bin/mk_LDASsaRestarts Mxx 50 
!     where Mxx is the choice of input resolution (M09 or M36) depnding on output resolution depending on the output resolution 
!     and 50 is SURFLAY surface layer thickness (5cm), 2cm is the other available option
! (10) bin/Scale_Catch OutData1/catch_internal_rst OutData2/catch_internal_rst catch_internal_rst 50
! (11) bin/Scale_CatchCN OutData1/catchcn_internal_rst OutData2/catchcn_internal_rst catchcn_internal_rst 50

  use MAPL_ConstantsMod,only: MAPL_PI,  MAPL_radius
  use MAPL_HashMod
  use MAPL_IOMod
  use ieee_arithmetic, only: isnan => ieee_is_nan
  
  implicit none
  include 'mpif.h'
  INCLUDE 'netcdf.inc'
  
  ! initialize to non-MPI values
  
  integer  :: myid=0, numprocs=1, mpierr, mpistatus(MPI_STATUS_SIZE)  
  logical  :: master_proc=.true.
  
  ! Carbon model specifics
  ! ----------------------

  character*256 :: Usage="mk_LDASsaRestarts MGRID SURFLAY" 
  real, parameter :: ECCENTRICITY  = 0.0167
  real, parameter :: PERIHELION    = 102.0
  real, parameter :: OBLIQUITY     = 23.45
  integer, parameter :: EQUINOX    = 80
  
  integer, parameter :: nveg    = 4
  integer, parameter :: nzone   = 3
  integer, parameter :: VAR_COL = 40 ! number of CN column restart variables
  integer, parameter :: VAR_PFT = 74 ! number of CN PFT variables per column
  integer, parameter :: npft    = 19  

  real, parameter :: nan = O'17760000000'
  real, parameter :: fmin= 1.e-4 ! ignore vegetation fractions at or below this value
  integer, parameter :: OutUnit = 40, InUnit = 50
  character*256 :: arg(2)

  integer :: ntiles_in, ntiles, nVars, rc
  character(len=300) :: OutFileName
  character*3 :: MGRID 
  real        :: SURFLAY 
  integer     :: STATUS, i, n, iargc

  character(len=256), parameter :: CatNames   (57) = &
       (/'BF1    ',  'BF2    ',  'BF3    ',  'VGWMAX ',  'CDCR1  ', &
         'CDCR2  ',  'PSIS   ',  'BEE    ',  'POROS  ',  'WPWET  ', &
         'COND   ',  'GNU    ',  'ARS1   ',  'ARS2   ',  'ARS3   ', &
         'ARA1   ',  'ARA2   ',  'ARA3   ',  'ARA4   ',  'ARW1   ', &
         'ARW2   ',  'ARW3   ',  'ARW4   ',  'TSA1   ',  'TSA2   ', &
         'TSB1   ',  'TSB2   ',  'ATAU   ',  'BTAU   ',  'OLD_ITY', &
         'TC     ',  'QC     ',  'CAPAC  ',  'CATDEF ',  'RZEXC  ', &
         'SRFEXC ',  'GHTCNT1',  'GHTCNT2',  'GHTCNT3',  'GHTCNT4', &
         'GHTCNT5',  'GHTCNT6',  'TSURF  ',  'WESNN1 ',  'WESNN2 ', &
         'WESNN3 ',  'HTSNNN1',  'HTSNNN2',  'HTSNNN3',  'SNDZN1 ', &
         'SNDZN2 ',  'SNDZN3 ',  'CH     ',  'CM     ',  'CQ     ', &
         'FR     ',  'WW     '/)

  character(len=256), parameter :: CarbNames (68) =  &
       (/'BF1    ',  'BF2    ',  'BF3    ',  'VGWMAX ',  'CDCR1  ', &
         'CDCR2  ',  'PSIS   ',  'BEE    ',  'POROS  ',  'WPWET  ', &
         'COND   ',  'GNU    ',  'ARS1   ',  'ARS2   ',  'ARS3   ', &
         'ARA1   ',  'ARA2   ',  'ARA3   ',  'ARA4   ',  'ARW1   ', &
         'ARW2   ',  'ARW3   ',  'ARW4   ',  'TSA1   ',  'TSA2   ', &
         'TSB1   ',  'TSB2   ',  'ATAU   ',  'BTAU   ',  'ITY    ', &
         'FVG    ',  'TC     ',  'QC     ',  'TG     ',  'CAPAC  ', &
         'CATDEF ',  'RZEXC  ',  'SRFEXC ',  'GHTCNT1',  'GHTCNT2', &
         'GHTCNT3',  'GHTCNT4',  'GHTCNT5',  'GHTCNT6',  'TSURF  ', &
         'WESNN1 ',  'WESNN2 ',  'WESNN3 ',  'HTSNNN1',  'HTSNNN2', &
         'HTSNNN3',  'SNDZN1 ',  'SNDZN2 ',  'SNDZN3 ',  'CH     ', &
         'CM     ',  'CQ     ',  'FR     ',  'WW     ',  'TILE_ID', &
         'NDEP   ',  'CLI_T2M',  'BGALBVR',  'BGALBVF',  'BGALBNR', &
         'BGALBNF',  'CNCOL  ',  'CNPFT  '   /)

  call init_MPI()

  ! Step 1 (1) regrid Qings to Heracles-NL use only til files
  ! Step 2 (2) regrid Qings to Heracles-NL use BCs
  ! Step 3 (3) run scale_catch{CN} 
  I = iargc()
  
  if( I /=2 ) then
     print *, "Wrong Number of arguments: ", i
     print *, trim(Usage)
     stop
  end if
  
  do n=1,I
     call getarg(n,arg(n))
  enddo

  read(arg(1),'(a)') MGRID
  read(arg(2),*)  SURFLAY
 
  if (master_proc) then
     
     ! read in ntiles_in and ntiles 
     ! ----------------------------
     
     open  (10,file = 'OutData2/clsm/catchment.def', form = 'formatted', status ='old', action = 'read')
     read  (10,*) ntiles
     close (10, status ='keep')

  endif

  call MPI_BCAST(NTILES     ,     1, MPI_INTEGER  ,  0,MPI_COMM_WORLD,mpierr)
  
  ! Regridding

  call regrid_hyd_vars (NTILES, MGRID) 

  if (master_proc) call read_bcs_data (NTILES, SURFLAY)

  call MPI_Barrier(MPI_COMM_WORLD, STATUS)

  call  regrid_carbon_vars (NTILES)


  call MPI_FINALIZE(mpierr)
     
contains

  
  ! *****************************************************************************
  
  SUBROUTINE regrid_hyd_vars (NTILES, MGRID) 

    implicit none
    integer, intent (in)           :: NTILES
    character*3, intent (in)       :: MGRID

    character(len=300) :: ldas_rst,  cat_para, tile_cood, BCSDIR, cat_til, path 

    ! ===============================================================================================

    integer, allocatable, dimension(:)   :: Id_glb, Id_loc
    integer, allocatable, dimension(:)   :: tid_offl,ld_reorder
    logical, allocatable, dimension(:)   :: mask
    real    :: dw, min_lon, max_lon, min_lat, max_lat, sub_dist
    integer :: n,i,j, nplus, nv, nx, offl_cell, STATUS,NCFID, VID1, VID2
    integer :: outid, local_id
    integer, allocatable, dimension (:) :: sub_tid
    real   , allocatable, dimension (:) :: sub_lon, sub_lat, rev_dist, lonc, latc, LATT, LONN, long, latg
    integer, allocatable :: low_ind(:), upp_ind(:), nt_local (:)

    logical :: all_found

    ! ===============================================================================================
    ! Below hard-wired ldas restart file(s) are from Qings SMAP_M09/M36
  
    if (MGRID == 'M09') then

       path = '/gpfsm/dnb42/projects/p16/ssd/land/l_data/LandRestarts_for_Regridding/Catch/M09/20160101/'

       ldas_rst  = trim(path)//'SMAP_Nature_v05.ens0000.catch_ldas_rst.20160101_0000z.bin'

       cat_para  = trim(path)//'SMAP_Nature_v05.ldas_catparam.20150101_0000z.bin'

       tile_cood = trim(path)//'SMAP_Nature_v05.ldas_tilecoord.bin'

       BCSDIR    = '/discover/nobackup/projects/gmao/ssd/land/l_data/geos5/bcs/CLSM_params/'// &
            'mkCatchParam_SMAP_L4SM_v002//SMAP_EASEv2_M09/'

       cat_til   = 'SMAP_EASEv2_M09_3856x1624.til'     

    end if
    
    if (MGRID == 'M36') then

       path = '/gpfsm/dnb42/projects/p16/ssd/land/l_data/LandRestarts_for_Regridding/Catch/M36/20150101/'
       
       ldas_rst  = trim(path)//'m3-16_0_M36_spinA.ens0000.catch_ldas_rst.20150101_0000z.bin' 

       cat_para  = trim(path)//'m3-16_0_M36_spinA.ldas_catparam.20140101_0000z.bin' 

       tile_cood = trim(path)//'m3-16_0_M36_spinA.ldas_tilecoord.bin' 

       BCSDIR    = '/discover/nobackup/projects/gmao/ssd/land/l_data/geos5/bcs/CLSM_params/'// &
            'mkCatchParam_SMAP_L4SM_v002//SMAP_EASEv2_M36/'                                                                        

       cat_til   = 'SMAP_EASEv2_M36_964x406.til'       

    end if

    if (master_proc) then
       
       ! --------------------------------------------
       ! Read exact lonn, latt from output .til file 
       ! --------------------------------------------

       open  (10,file = trim(BCSDIR)//'clsm/catchment.def', form = 'formatted', status ='old', action = 'read')
       read  (10,*) ntiles_in
       close (10, status ='keep')

    endif

    call MPI_Barrier(MPI_COMM_WORLD, STATUS)
    call MPI_BCAST(NTILES_IN  ,     1, MPI_INTEGER  ,  0,MPI_COMM_WORLD,mpierr)
    call MPI_Barrier(MPI_COMM_WORLD, STATUS)

    allocate (tid_offl  (ntiles_in))
    allocate (mask      (ntiles_in))

    allocate(low_ind (   numprocs))
    allocate(upp_ind (   numprocs))
    allocate(nt_local(   numprocs))

    low_ind (:)    = 1
    upp_ind (:)    = NTILES       
    nt_local(:)    = NTILES 

    ! Domain decomposition
    ! --------------------

    if (numprocs > 1) then      
       do i = 1, numprocs - 1
          upp_ind(i)   = low_ind(i) + (ntiles/numprocs) - 1 
          low_ind(i+1) = upp_ind(i) + 1
          nt_local(i)  = upp_ind(i) - low_ind(i) + 1
       end do
       nt_local(numprocs) = upp_ind(numprocs) - low_ind(numprocs) + 1
    endif

    allocate (id_loc (nt_local (myid + 1)))
    allocate (lonn   (nt_local (myid + 1)))
    allocate (latt   (nt_local (myid + 1)))
    allocate (lonc   (1:ntiles_in))
    allocate (latc   (1:ntiles_in))

    if (master_proc) then

       allocate (long   (ntiles))
       allocate (latg   (ntiles))
       allocate (ld_reorder(ntiles_in)) 

       call ReadCNTilFile ('OutData1/OutTileFile', ntiles, long, latg)

       ! ---------------------------------------------
       ! Read exact lonc, latc from offline .til File 
       ! ---------------------------------------------

       call ReadCNTilFile(trim(BCSDIR)//trim(cat_til),ntiles_in,lonc,latc)

       open (10, file = trim(tile_cood), form= 'unformatted',convert= 'big_endian', status = 'old', action = 'read')
       read (10) n

       if (n /= ntiles_in) then
          print *,'ldas_tilecoord.bin and TilFile mis-match'
          stop
       endif
       
       read (10) tid_offl
       close (10, status = 'keep')

       do n = 1, ntiles_in
          ld_reorder ( tid_offl(n)) = n
          tid_offl(n)    = n
       end do
       
    endif

    call MPI_Barrier(MPI_COMM_WORLD, STATUS)

    call MPI_SCATTERV (                    &
         long,nt_local,low_ind-1,MPI_real, &
         lonn,size(lonn),MPI_real  , &
         0,MPI_COMM_WORLD, mpierr )

    call MPI_SCATTERV (                    &
         latg,nt_local,low_ind-1,MPI_real, &
         latt,nt_local(myid+1),MPI_real  , &
         0,MPI_COMM_WORLD, mpierr )

    if(master_proc) deallocate (long, latg)
     
    call MPI_BCAST(lonc,ntiles_in,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
    call MPI_BCAST(latc,ntiles_in,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
    call MPI_BCAST(tid_offl  ,size(tid_offl  ),MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

    ! --------------------------------------------------------------------------------
    ! Here we create transfer index array to map offline restarts to output tile space
    ! --------------------------------------------------------------------------------   
      
    ! Loop through NTILES (# of tiles in output array) find the nearest neighbor from Qing.  

    Id_loc = -9999
    
    TILES : do n = low_ind (myid + 1), upp_ind (myid + 1)

       local_id = n - low_ind (myid + 1) + 1
                      
       dw = 1.0 ! Start with a 10x10 window, then zoom out by increasing the size by 2-deg until 4 similar tiles are found for 4 PFT types        

       ZOOMOUT : do  

          all_found = .false.   

          ! Min/Max lon/lat of the working window
          ! -------------------------------------
          
          min_lon = MAX(lonn (local_id) - dw, -180.)
          max_lon = MIN(lonn (local_id) + dw,  180.)
          min_lat = MAX(latt (local_id) - dw,  -90.)
          max_lat = MIN(latt (local_id) + dw,   90.) 
          mask = .false.
          mask =  ((latc >= min_lat .and. latc <= max_lat).and.(lonc >= min_lon .and. lonc <= max_lon))
          nplus =  count(mask = mask)
          
          if(nplus < 0) then
             dw = dw + 0.5
             CYCLE
          endif
          
          allocate (sub_tid (1:nplus))
          allocate (sub_lon (1:nplus))
          allocate (sub_lat (1:nplus))
          allocate (rev_dist  (1:nplus))
          
          sub_tid = PACK (tid_offl, mask= mask) 
          sub_lon = PACK (lonc    , mask= mask)
          sub_lat = PACK (latc    , mask= mask)
          
          ! compute distance from the tile
          
          sub_lat = sub_lat * MAPL_PI/180.
          sub_lon = sub_lon * MAPL_PI/180.
          
!          do i = 1,nplus
!             sub_dist(i) = haversine(to_radian(latt(local_id)), to_radian(lonn(local_id)), &
!                  sub_lat(i), sub_lon(i)) 
!             !                   sub_dist(i) = dist
!          end do
             
          SEEK : if(Id_loc(local_id) < 0) then
                             
                rev_dist  = 1.e20
                
                do i = 1,nplus
                      
                   rev_dist(i) = haversine(to_radian(latt(local_id)), to_radian(lonn(local_id)), &
                        sub_lat(i), sub_lon(i))
                                      
                end do

                FOUND : if(minval (rev_dist) < 1.e19) then 
                   
                   Id_loc(local_id) = sub_tid(minloc(rev_dist,1)) ! Cell ID of the nearest neightbor in the offline 
                                                                     ! restart file that has the same veg type
                    all_found = .true.                  
                endif FOUND
                
             endif SEEK
          
          deallocate (sub_tid, sub_lon, sub_lat, rev_dist)
          
          if(all_found) GO TO 100
           
           ! if not increase the window size
           dw = dw + 0.5
           
        end do ZOOMOUT
        
100   continue !  if(mod (n,1000) == 0) print *, myid +1, n, Id_loc(local_id,:)

     END DO TILES
 
     ! update id_glb in root

     if(master_proc)  allocate (id_glb  (ntiles))

     call MPI_Barrier(MPI_COMM_WORLD, STATUS)
     call MPI_GATHERV( &
                   id_loc, nt_local(myid+1)  , MPI_real, &
                   id_glb, nt_local,low_ind-1, MPI_real, &
                   0, MPI_COMM_WORLD, mpierr )
        


    if (master_proc) call put_catch   (NTILES, NTILES_IN, id_glb, ld_reorder, ldas_rst, cat_para)

    call MPI_Barrier(MPI_COMM_WORLD, STATUS)

   END SUBROUTINE regrid_hyd_vars


  ! *****************************************************************************
  
  SUBROUTINE read_bcs_data (ntiles, SURFLAY)

    ! This subroutine :
    !  1) reads BCs from BCSDIR and hydrological varables from InRestart.
    !     InRestart is a catchcn_internal_rst nc4 file.
    !
    !  2) writes out BCs and hydrological variables in catchcn_internal_rst (1:72). 
    !     output catchcn_internal_rst is nc4.

    implicit none
    real, intent (in)                         :: SURFLAY
    integer, intent (in)                      :: ntiles
    character*300           :: InRestart = 'OutData2/catchcn_internal_rst'
    character*300           :: InRestart2= 'OutData2/catch_internal_rst'

    real, allocatable :: CLMC_pf1(:), CLMC_pf2(:), CLMC_sf1(:), CLMC_sf2(:)
    real, allocatable :: CLMC_pt1(:), CLMC_pt2(:), CLMC_st1(:), CLMC_st2(:)    
    real, allocatable :: BF1(:),   BF2(:),   BF3(:),  VGWMAX(:)
    real, allocatable :: CDCR1(:), CDCR2(:), PSIS(:), BEE(:) 
    real, allocatable :: POROS(:), WPWET(:), COND(:), GNU(:)
    real, allocatable :: ARS1(:),  ARS2(:),  ARS3(:)
    real, allocatable :: ARA1(:),  ARA2(:),  ARA3(:), ARA4(:)
    real, allocatable :: ARW1(:),  ARW2(:),  ARW3(:), ARW4(:)
    real, allocatable :: TSA1(:),  TSA2(:),  TSB1(:), TSB2(:)
    real, allocatable :: ATAU2(:), BTAU2(:), DP2BR(:), rity(:), CanopH(:)
    real, allocatable :: NDEP(:), BVISDR(:), BVISDF(:), BNIRDR(:), BNIRDF(:) 
    real, allocatable :: T2(:), var1(:)
    integer, allocatable :: ity(:)

    integer                                  :: rc, NCFID, STATUS, VID, NCFID2
    character*256                            :: vname
    character*256 :: DataDir="OutData2/clsm/"
    integer       :: idum, i,j,n, ib, nv
    real          :: rdum, zdep1, zdep2, zdep3, zmet, term1, term2, bare,fvg(4)
   
    allocate (   BF1(ntiles),    BF2 (ntiles),     BF3(ntiles)  )
    allocate (VGWMAX(ntiles),   CDCR1(ntiles),   CDCR2(ntiles)  ) 
    allocate (  PSIS(ntiles),     BEE(ntiles),   POROS(ntiles)  ) 
    allocate ( WPWET(ntiles),    COND(ntiles),     GNU(ntiles)  )
    allocate (  ARS1(ntiles),    ARS2(ntiles),    ARS3(ntiles)  )
    allocate (  ARA1(ntiles),    ARA2(ntiles),    ARA3(ntiles)  )
    allocate (  ARA4(ntiles),    ARW1(ntiles),    ARW2(ntiles)  )
    allocate (  ARW3(ntiles),    ARW4(ntiles),    TSA1(ntiles)  )
    allocate (  TSA2(ntiles),    TSB1(ntiles),    TSB2(ntiles)  )
    allocate ( ATAU2(ntiles),   BTAU2(ntiles),   DP2BR(ntiles)  )
    allocate (BVISDR(ntiles),  BVISDF(ntiles),  BNIRDR(ntiles)  )
    allocate (BNIRDF(ntiles),      T2(ntiles),    NDEP(ntiles)  )    
    allocate (   ity(ntiles),      rity(ntiles),    CanopH(ntiles))
    allocate (CLMC_pf1(ntiles), CLMC_pf2(ntiles), CLMC_sf1(ntiles))
    allocate (CLMC_sf2(ntiles), CLMC_pt1(ntiles), CLMC_pt2(ntiles))
    allocate (CLMC_st1(ntiles), CLMC_st2(ntiles), var1(ntiles))
     
     open(unit=22, file=trim(DataDir)//'bf.dat'               ,form='formatted')
     open(unit=23, file=trim(DataDir)//'soil_param.dat'       ,form='formatted')
     open(unit=24, file=trim(DataDir)//'ar.new'               ,form='formatted')
     open(unit=25, file=trim(DataDir)//'ts.dat'               ,form='formatted')
     open(unit=26, file=trim(DataDir)//'tau_param.dat'        ,form='formatted')
     open(unit=27, file=trim(DataDir)//'CLM_veg_typs_fracs'   ,form='formatted')
     open(unit=28, file=trim(DataDir)//'CLM_NDep_SoilAlb_T2m' ,form='formatted')
     
     do n=1,ntiles
        var1 (n) = real (n)
        read (22, *) i,j, GNU(n), BF1(n), BF2(n), BF3(n)
        
        read (23, *) i,j, idum, idum, BEE(n), PSIS(n),&
             POROS(n), COND(n), WPWET(n), DP2BR(n)
        
        read (24, *) i,j, rdum, ARS1(n), ARS2(n), ARS3(n),          &
             ARA1(n), ARA2(n), ARA3(n), ARA4(n), &
             ARW1(n), ARW2(n), ARW3(n), ARW4(n)
        
        read (25, *) i,j, rdum, TSA1(n), TSA2(n), TSB1(n), TSB2(n)
        
        if( SURFLAY.eq.20.0 ) read (26, *) i,j, ATAU2(n), BTAU2(n), rdum, rdum   ! for old soil params
        if( SURFLAY.eq.50.0 ) read (26, *) i,j, rdum , rdum, ATAU2(n), BTAU2(n)  ! for new soil params
        
        read (27, *) i,j, CLMC_pt1(n), CLMC_pt2(n), CLMC_st1(n), CLMC_st2(n), &
             CLMC_pf1(n), CLMC_pf2(n), CLMC_sf1(n), CLMC_sf2(n)
        
        read (28, *) NDEP(n), BVISDR(n), BVISDF(n), BNIRDR(n), BNIRDF(n), T2(n) ! MERRA-2 Annual Mean Temp is default.
                
        BVISDR(n) = amax1(1.e-6, BVISDR(n))
        BVISDF(n) = amax1(1.e-6, BVISDF(n))
        BNIRDR(n) = amax1(1.e-6, BNIRDR(n))
        BNIRDF(n) = amax1(1.e-6, BNIRDF(n))

        zdep2=1000.
        zdep3=amax1(1000.,DP2BR(n))
        
        if (zdep2 .gt.0.75*zdep3) then
           zdep2  =  0.75*zdep3              
        end if
        
        zdep1=20.
        zmet=zdep3/1000.
        
        term1=-1.+((PSIS(n)-zmet)/PSIS(n))**((BEE(n)-1.)/BEE(n))
        term2=PSIS(n)*BEE(n)/(BEE(n)-1)
        
        VGWMAX(n) = POROS(n)*zdep2   
        CDCR1(n)  = 1000.*POROS(n)*(zmet-(-term2*term1))   
        CDCR2(n)  = (1.-WPWET(n))*POROS(n)*zdep3

        ! convert % to fractions
     
        CLMC_pf1(n) = CLMC_pf1(n) / 100.
        CLMC_pf2(n) = CLMC_pf2(n) / 100.
        CLMC_sf1(n) = CLMC_sf1(n) / 100.
        CLMC_sf2(n) = CLMC_sf2(n) / 100.

        fvg(1) = CLMC_pf1(n)
        fvg(2) = CLMC_pf2(n)
        fvg(3) = CLMC_sf1(n)
        fvg(4) = CLMC_sf2(n)

        BARE = 1.      

        DO NV = 1, NVEG
           BARE = BARE - FVG(NV)! subtract vegetated fractions 
        END DO
        
        if (BARE /= 0.) THEN
           IB = MAXLOC(FVG(:),1)
           FVG (IB) = FVG(IB) + BARE ! This also corrects all cases sum ne 0.
        ENDIF

        CLMC_pf1(n) = fvg(1)
        CLMC_pf2(n) = fvg(2)
        CLMC_sf1(n) = fvg(3)
        CLMC_sf2(n) = fvg(4)
        
     enddo
     
     NDEP = NDEP * 1.e-9
     
! prevent trivial fractions
! -------------------------
     do n = 1,ntiles
        if(CLMC_pf1(n) <= 1.e-4) then
           CLMC_pf2(n) = CLMC_pf2(n) + CLMC_pf1(n)
           CLMC_pf1(n) = 0.
        endif

        if(CLMC_pf2(n) <= 1.e-4) then
           CLMC_pf1(n) = CLMC_pf1(n) + CLMC_pf2(n)
           CLMC_pf2(n) = 0.
        endif
        
        if(CLMC_sf1(n) <= 1.e-4) then
           if(CLMC_sf2(n) > 1.e-4) then
              CLMC_sf2(n) = CLMC_sf2(n) + CLMC_sf1(n)
           else if(CLMC_pf2(n) > 1.e-4) then
              CLMC_pf2(n) = CLMC_pf2(n) + CLMC_sf1(n)
           else if(CLMC_pf1(n) > 1.e-4) then
              CLMC_pf1(n) = CLMC_pf1(n) + CLMC_sf1(n)
           else
              stop 'fveg3'
           endif
           CLMC_sf1(n) = 0.
        endif

        if(CLMC_sf2(n) <= 1.e-4) then
           if(CLMC_sf1(n) > 1.e-4) then
              CLMC_sf1(n) = CLMC_sf1(n) + CLMC_sf2(n)
           else if(CLMC_pf2(n) > 1.e-4) then
              CLMC_pf2(n) = CLMC_pf2(n) + CLMC_sf2(n)
           else if(CLMC_pf1(n) > 1.e-4) then
              CLMC_pf1(n) = CLMC_pf1(n) + CLMC_sf2(n)
           else
              stop 'fveg4'
           endif
           CLMC_sf2(n) = 0.
        endif
     end do
     
     CLOSE (22, STATUS = 'KEEP')
     CLOSE (23, STATUS = 'KEEP')
     CLOSE (24, STATUS = 'KEEP')
     CLOSE (25, STATUS = 'KEEP')
     CLOSE (26, STATUS = 'KEEP')
     CLOSE (27, STATUS = 'KEEP')
     CLOSE (28, STATUS = 'KEEP')

     ! Now writing BCs (from BCSDIR) and regridded hydrological variables 1-72
     ! -----------------------------------------------------------------------

     STATUS = NF_OPEN (trim(InRestart),NF_WRITE,NCFID)
     STATUS = NF_OPEN (trim(InRestart2),NF_WRITE,NCFID2)

     STATUS = NF_INQ_VARID (NCFID,'BF1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BF1)
     STATUS = NF_INQ_VARID (NCFID2,'BF1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),BF1)

     STATUS = NF_INQ_VARID (NCFID,'BF2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BF2)
     STATUS = NF_INQ_VARID (NCFID2,'BF2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),BF2)

     STATUS = NF_INQ_VARID (NCFID,'BF3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BF3)
     STATUS = NF_INQ_VARID (NCFID2,'BF3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),BF3)

     STATUS = NF_INQ_VARID (NCFID,'VGWMAX' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),VGWMAX)
     STATUS = NF_INQ_VARID (NCFID2,'VGWMAX' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),VGWMAX)

     STATUS = NF_INQ_VARID (NCFID,'CDCR1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),CDCR1)
     STATUS = NF_INQ_VARID (NCFID2,'CDCR1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),CDCR1)

     STATUS = NF_INQ_VARID (NCFID,'CDCR2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),CDCR2)
     STATUS = NF_INQ_VARID (NCFID2,'CDCR2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),CDCR2)

     STATUS = NF_INQ_VARID (NCFID,'PSIS' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),PSIS)
     STATUS = NF_INQ_VARID (NCFID2,'PSIS' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),PSIS)

     STATUS = NF_INQ_VARID (NCFID,'BEE' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BEE)
     STATUS = NF_INQ_VARID (NCFID2,'BEE' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),BEE)

     STATUS = NF_INQ_VARID (NCFID,'POROS' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),POROS)
     STATUS = NF_INQ_VARID (NCFID2,'POROS' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),POROS)

     STATUS = NF_INQ_VARID (NCFID,'WPWET' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),WPWET)
     STATUS = NF_INQ_VARID (NCFID2,'WPWET' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),WPWET)

     STATUS = NF_INQ_VARID (NCFID,'COND' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),COND)
     STATUS = NF_INQ_VARID (NCFID2,'COND' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),COND)

     STATUS = NF_INQ_VARID (NCFID,'GNU'  ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),GNU)
     STATUS = NF_INQ_VARID (NCFID2,'GNU'  ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),GNU)

     STATUS = NF_INQ_VARID (NCFID,'ARS1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARS1)
     STATUS = NF_INQ_VARID (NCFID2,'ARS1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARS1)

     STATUS = NF_INQ_VARID (NCFID,'ARS2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARS2)
     STATUS = NF_INQ_VARID (NCFID2,'ARS2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARS2)

     STATUS = NF_INQ_VARID (NCFID,'ARS3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARS3)
     STATUS = NF_INQ_VARID (NCFID2,'ARS3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARS3)

     STATUS = NF_INQ_VARID (NCFID,'ARA1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARA1)
     STATUS = NF_INQ_VARID (NCFID2,'ARA1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARA1)

     STATUS = NF_INQ_VARID (NCFID,'ARA2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARA2)
     STATUS = NF_INQ_VARID (NCFID2,'ARA2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARA2)

     STATUS = NF_INQ_VARID (NCFID,'ARA3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARA3)
     STATUS = NF_INQ_VARID (NCFID2,'ARA3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARA3)

     STATUS = NF_INQ_VARID (NCFID,'ARA4' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARA4)
     STATUS = NF_INQ_VARID (NCFID2,'ARA4' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARA4)

     STATUS = NF_INQ_VARID (NCFID,'ARW1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARW1)
     STATUS = NF_INQ_VARID (NCFID2,'ARW1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARW1)

     STATUS = NF_INQ_VARID (NCFID,'ARW2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARW2)
     STATUS = NF_INQ_VARID (NCFID2,'ARW2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARW2)

     STATUS = NF_INQ_VARID (NCFID,'ARW3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARW3)
     STATUS = NF_INQ_VARID (NCFID2,'ARW3' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARW3)

     STATUS = NF_INQ_VARID (NCFID,'ARW4' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ARW4)
     STATUS = NF_INQ_VARID (NCFID2,'ARW4' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ARW4)

     STATUS = NF_INQ_VARID (NCFID,'TSA1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),TSA1)
     STATUS = NF_INQ_VARID (NCFID2,'TSA1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),TSA1)

     STATUS = NF_INQ_VARID (NCFID,'TSA2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),TSA2)
     STATUS = NF_INQ_VARID (NCFID2,'TSA2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),TSA2)

     STATUS = NF_INQ_VARID (NCFID,'TSB1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),TSB1)
     STATUS = NF_INQ_VARID (NCFID2,'TSB1' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),TSB1)

     STATUS = NF_INQ_VARID (NCFID,'TSB2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),TSB2)
     STATUS = NF_INQ_VARID (NCFID2,'TSB2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),TSB2)

     STATUS = NF_INQ_VARID (NCFID,'ATAU2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),ATAU2)
     STATUS = NF_INQ_VARID (NCFID2,'ATAU2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),ATAU2)

     STATUS = NF_INQ_VARID (NCFID,'BTAU2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BTAU2)
     STATUS = NF_INQ_VARID (NCFID2,'BTAU2' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID2,VID, (/1/), (/NTILES/),BTAU2)

     STATUS = NF_INQ_VARID (NCFID,'ITY' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,1/), (/NTILES,1/),CLMC_pt1)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,2/), (/NTILES,1/),CLMC_pt2)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,3/), (/NTILES,1/),CLMC_st1)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,4/), (/NTILES,1/),CLMC_st2)

     STATUS = NF_INQ_VARID (NCFID,'FVG' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,1/), (/NTILES,1/),CLMC_pf1)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,2/), (/NTILES,1/),CLMC_pf2)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,3/), (/NTILES,1/),CLMC_sf1)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1,4/), (/NTILES,1/),CLMC_sf2)

     STATUS = NF_INQ_VARID (NCFID,'TILE_ID' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),VAR1)

     STATUS = NF_INQ_VARID (NCFID,'NDEP' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),NDEP)

     STATUS = NF_INQ_VARID (NCFID,'CLI_T2M' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),T2)

     STATUS = NF_INQ_VARID (NCFID,'BGALBVR' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BVISDR)

     STATUS = NF_INQ_VARID (NCFID,'BGALBVF' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BVISDF)

     STATUS = NF_INQ_VARID (NCFID,'BGALBNR' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BNIRDR)

     STATUS = NF_INQ_VARID (NCFID,'BGALBNF' ,VID)
     STATUS = NF_PUT_VARA_REAL(NCFID,VID, (/1/), (/NTILES/),BNIRDF)

     deallocate (   BF1,     BF2,     BF3  )
     deallocate (VGWMAX,   CDCR1,   CDCR2  ) 
     deallocate (  PSIS,     BEE,   POROS  ) 
     deallocate ( WPWET,    COND,     GNU  )
     deallocate (  ARS1,    ARS2,    ARS3  )
     deallocate (  ARA1,    ARA2,    ARA3  )
     deallocate (  ARA4,    ARW1,    ARW2  )
     deallocate (  ARW3,    ARW4,    TSA1  )
     deallocate (  TSA2,    TSB1,    TSB2  )
     deallocate ( ATAU2,   BTAU2,   DP2BR  )
     deallocate (BVISDR,  BVISDF,  BNIRDR  )
     deallocate (BNIRDF,      T2,    NDEP  )    
     deallocate (   ity,    rity,    CanopH)
     deallocate (CLMC_pf1, CLMC_pf2, CLMC_sf1)
     deallocate (CLMC_sf2, CLMC_pt1, CLMC_pt2)
     deallocate (CLMC_st1,CLMC_st2)

     STATUS = NF_CLOSE ( NCFID)
     STATUS = NF_CLOSE (NCFID2)

  END SUBROUTINE read_bcs_data

  ! *****************************************************************************
  
  SUBROUTINE regrid_carbon_vars (NTILES)

    implicit none

    integer, intent (in)                 :: NTILES
    character*300          :: OutTileFile = 'OutData1/OutTileFile', OutFileName='OutData2/catchcn_internal_rst'
    integer                :: AGCM_YY=2015,AGCM_MM=1,AGCM_DD=1,AGCM_HR=0 
    real, allocatable, dimension (:)     :: CLMC_pf1, CLMC_pf2, CLMC_sf1, CLMC_sf2, &
         CLMC_pt1, CLMC_pt2,CLMC_st1,CLMC_st2

    ! ===============================================================================================
    ! Below hard-wired restart file(s) are from Fanweis SMAP_M09 run for which she had used restarts 
    !    from Gregs some 3000-year spin up.

    integer, parameter :: ntiles_cn = 1653157   

    character*300 :: InCNTileFile="/gpfsm/dnb42/projects/p16/ssd/land/l_data/geos5/bcs/CLSM_params/" &
         //"mkCatchParam_SMAP_L4SM_v001/SMAP_EASEv2_M09/" &
         //"SMAP_EASEv2_M09_3856x1624.til"
    character*256 :: InCNRestart="/gpfsm/dnb42/projects/p16/ssd/land/l_data/LandRestarts_for_Regridding/CatchCN/" &
         //"M09/20151231/catchcn_internal_rst"

    ! The above nc4 file was created using below 2 files. 
    ! character*256 :: InCNRestart="/gpfsm/dnb31/fzeng/Catchment/SMAP_EASEv2_M09/CN_restart.20151231_00z.e0004s"
    ! character*256 :: InPFT = "/discover/nobackup/fzeng/Catchment/pft_SMAP_EASEv2_M09.dat" 
    ! ===============================================================================================

    integer :: iclass(npft) = (/1,1,2,3,3,4,5,5,6,7,8,9,10,11,12,11,12,11,12/)
    integer, allocatable, dimension(:,:) :: Id_glb, Id_loc
    integer, allocatable, dimension(:)   :: tid_offl, id_vec
    logical, allocatable, dimension(:)   :: mask
    real,    allocatable, dimension(:,:) :: fveg_offl,  ityp_offl
    real    :: dw, min_lon, max_lon, min_lat, max_lat, fveg_new, sub_dist
    integer :: n,i,j, nplus, nv, nx, nz, iv, offl_cell, ityp_new, STATUS,NCFID, VID1, VID2
    integer :: outid, local_id
    integer, allocatable, dimension (:) :: sub_tid, sub_ityp1, sub_ityp2,icl_ityp1
    real   , allocatable, dimension (:) :: sub_lon, sub_lat, rev_dist, sub_fevg1, sub_fevg2,&
         lonc, latc, LATT, LONN, DAYX, long, latg, var_dum
    real, allocatable :: var_off_col (:,:,:), var_off_pft (:,:,:,:) 
    real, allocatable :: var_col_out (:,:,:), var_pft_out (:,:,:,:) 
    integer, allocatable :: low_ind(:), upp_ind(:), nt_local (:)

    logical :: all_found
  
    allocate (tid_offl  (ntiles_cn))
    allocate (mask      (ntiles_cn))
    allocate (ityp_offl (ntiles_cn,nveg))
    allocate (fveg_offl (ntiles_cn,nveg))

    allocate(low_ind (   numprocs))
    allocate(upp_ind (   numprocs))
    allocate(nt_local(   numprocs))

    low_ind (:)    = 1
    upp_ind (:)    = NTILES       
    nt_local(:)    = NTILES 

    ! Domain decomposition
    ! --------------------

    if (numprocs > 1) then      
       do i = 1, numprocs - 1
          upp_ind(i)   = low_ind(i) + (ntiles/numprocs) - 1 
          low_ind(i+1) = upp_ind(i) + 1
          nt_local(i)  = upp_ind(i) - low_ind(i) + 1
       end do
       nt_local(numprocs) = upp_ind(numprocs) - low_ind(numprocs) + 1
    endif

    allocate (id_loc  (nt_local (myid + 1),4))
    allocate (lonn    (nt_local (myid + 1)))
    allocate (latt    (nt_local (myid + 1)))
    allocate (CLMC_pf1(nt_local (myid + 1)))
    allocate (CLMC_pf2(nt_local (myid + 1)))
    allocate (CLMC_sf1(nt_local (myid + 1)))
    allocate (CLMC_sf2(nt_local (myid + 1)))
    allocate (CLMC_pt1(nt_local (myid + 1)))
    allocate (CLMC_pt2(nt_local (myid + 1)))
    allocate (CLMC_st1(nt_local (myid + 1)))
    allocate (CLMC_st2(nt_local (myid + 1)))
    allocate (lonc   (1:ntiles_cn))
    allocate (latc   (1:ntiles_cn))

    if (master_proc) then
       
       ! --------------------------------------------
       ! Read exact lonn, latt from output .til file 
       ! --------------------------------------------

       allocate (long   (ntiles))
       allocate (latg   (ntiles))
       allocate (DAYX   (NTILES))

       call ReadCNTilFile (OutTileFile, ntiles, long, latg)

       ! Compute DAYX
       ! ------------

       call compute_dayx (                                     &
            NTILES, AGCM_YY, AGCM_MM, AGCM_DD, AGCM_HR,        &
            LATG, DAYX)   

       ! ---------------------------------------------
       ! Read exact lonc, latc from offline .til File 
       ! ---------------------------------------------

       call ReadCNTilFile(InCNTileFile,ntiles_cn,lonc,latc)

    endif

    call MPI_SCATTERV (                    &
         long,nt_local,low_ind-1,MPI_real, &
         lonn,size(lonn),MPI_real  , &
         0,MPI_COMM_WORLD, mpierr )

    call MPI_SCATTERV (                    &
         latg,nt_local,low_ind-1,MPI_real, &
         latt,nt_local(myid+1),MPI_real  , &
         0,MPI_COMM_WORLD, mpierr )

    if(master_proc) deallocate (long, latg)
 
    call MPI_BCAST(lonc,ntiles_cn,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
    call MPI_BCAST(latc,ntiles_cn,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

    ! Open GKW/Fzeng SMAP M09 catchcn_internal_rst and output catchcn_internal_rst
    ! ----------------------------------------------------------------------------
    
    ! NF_OPEN_PAR is no longer needed since IO is done by the root processor.
    !    call MPI_Info_create(info, STATUS)
    !    call MPI_Info_set(info, "romio_cb_read", "automatic", STATUS)   
    !    STATUS = NF_OPEN_PAR   (trim(InCNRestart),IOR(NF_NOWRITE,NF_MPIIO),MPI_COMM_WORLD, info,NCFID)
    !    STATUS = NF_OPEN_PAR   (trim(OutFileName),IOR(NF_WRITE  ,NF_MPIIO),MPI_COMM_WORLD, info,OUTID)
    
    STATUS = NF_OPEN (trim(InCNRestart),NF_NOWRITE,NCFID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'OFFLINE RESTART FAILED')
    STATUS = NF_OPEN (trim(OutFileName),NF_WRITE,OUTID)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'OUTPUT RESTART FAILED')
    STATUS = NF_INQ_VARID (NCFID,'ITY' ,VID1)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'ITY INQ')
    STATUS = NF_INQ_VARID (NCFID,'FVG' ,VID2)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'FVG INQ')
    
    STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/low_ind(myid+1),1/), (/nt_local(myid+1),1/),CLMC_pt1)
    STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/low_ind(myid+1),2/), (/nt_local(myid+1),1/),CLMC_pt2)
    STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/low_ind(myid+1),3/), (/nt_local(myid+1),1/),CLMC_st1)
    STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/low_ind(myid+1),4/), (/nt_local(myid+1),1/),CLMC_st2)
    STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/low_ind(myid+1),1/), (/nt_local(myid+1),1/),CLMC_pf1)
    STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/low_ind(myid+1),2/), (/nt_local(myid+1),1/),CLMC_pf2)
    STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/low_ind(myid+1),3/), (/nt_local(myid+1),1/),CLMC_sf1)
    STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/low_ind(myid+1),4/), (/nt_local(myid+1),1/),CLMC_sf2)

    if (master_proc) then
       
       do n = 1,ntiles_cn
 
          STATUS = NF_GET_VARA_REAL(NCFID,VID1, (/n,1/), (/1,4/),ityp_offl(n,:))
          STATUS = NF_GET_VARA_REAL(NCFID,VID2, (/n,1/), (/1,4/),fveg_offl(n,:))
          
          tid_offl (n) = n
          
          do nv = 1,nveg
             if(ityp_offl(n,nv)<0 .or. ityp_offl(n,nv)>npft)    stop 'ityp'
             if(fveg_offl(n,nv)<0..or. fveg_offl(n,nv)>1.00001) stop 'fveg'             
          end do

          if((ityp_offl(n,3) == 0).and.(ityp_offl(n,4) == 0)) then
             if(ityp_offl(n,1) /= 0) then
                ityp_offl(n,3) = ityp_offl(n,1)
             else
                ityp_offl(n,3) = ityp_offl(n,2)
             endif
          endif
          
          if((ityp_offl(n,1) == 0).and.(ityp_offl(n,2) /= 0)) ityp_offl(n,1) = ityp_offl(n,2)
          if((ityp_offl(n,2) == 0).and.(ityp_offl(n,1) /= 0)) ityp_offl(n,2) = ityp_offl(n,1)
          if((ityp_offl(n,3) == 0).and.(ityp_offl(n,4) /= 0)) ityp_offl(n,3) = ityp_offl(n,4)
          if((ityp_offl(n,4) == 0).and.(ityp_offl(n,3) /= 0)) ityp_offl(n,4) = ityp_offl(n,3)
          
       end do
       
    endif
    
    call MPI_BCAST(tid_offl ,size(tid_offl ),MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
    call MPI_BCAST(ityp_offl,size(ityp_offl),MPI_REAL   ,0,MPI_COMM_WORLD,mpierr)
    call MPI_BCAST(fveg_offl,size(fveg_offl),MPI_REAL   ,0,MPI_COMM_WORLD,mpierr)    

    ! --------------------------------------------------------------------------------
    ! Here we create transfer index array to map offline restarts to output tile space
    ! --------------------------------------------------------------------------------   
      
    ! Loop through NTILES (# of tiles in output array) and find, for each PFT type seperately, 
    !      a tile with the same PFT type in the neighborhood from the SMAP_M09 offline array.  

    Id_loc = -9999
    
    TILES : do n = low_ind (myid + 1), upp_ind (myid + 1)

       local_id = n - low_ind (myid + 1) + 1
        
       ! Below block was commented out to ensure zero-diff  irrespective to the number of processors used.
      
       !       if (n > (low_ind (myid + 1))) then
       !          
       !          
       !          ! If the previous tile  (n-1) is in the vicinity (within about 1-degree) and has any similar vegetation types 
       !          !  with fraction > 0.
       !          ! we simply take Id_loc(n-1,nv) values and skip the vegetation type.
       !          
       !          dist = haversine(to_radian(latt(local_id)), to_radian(lonn(local_id)), &
       !               to_radian(latt(local_id-1)), to_radian(lonn(local_id-1))) 
       !          
       !          NEIGHBOR : if(dist < 110.) then
       !             
       !             if((CLMC_pt1(local_id) == CLMC_pt1(local_id-1)).and.(CLMC_pf1(local_id-1) >= fmin)) then
       !                Id_loc (local_id,1) = Id_loc (local_id-1,1)
       !             endif
       !             if((CLMC_pt2(local_id) == CLMC_pt2(local_id-1)).and.(CLMC_pf2(local_id-1) >= fmin)) then 
       !                Id_loc (local_id,2) = Id_loc (local_id-1,2)
       !             endif
       !             if((CLMC_st1(local_id) == CLMC_st1(local_id-1)).and.(CLMC_sf1(local_id-1) >= fmin)) then
       !                Id_loc (local_id,3) = Id_loc (local_id-1,3)
       !             endif
       !             if((CLMC_st2(local_id) ==CLMC_st2(local_id-1)).and.(CLMC_sf2(local_id-1) >= fmin)) then
       !                Id_loc (local_id,4) = Id_loc (local_id-1,4)
       !             endif             
       !          endif NEIGHBOR          
       !       endif
              
       dw = 5.0 ! Start with a 10x10 window, then zoom out by increasing the size by 2-deg until 4 similar tiles are found for 4 PFT types        
       ZOOMOUT : do  
           
          ! Min/Max lon/lat of the working window
          ! -------------------------------------
          
          min_lon = MAX(lonn (local_id) - dw, -180.)
          max_lon = MIN(lonn (local_id) + dw,  180.)
          min_lat = MAX(latt (local_id) - dw,  -90.)
          max_lat = MIN(latt (local_id) + dw,   90.) 
          mask = .false.
          mask =  ((latc >= min_lat .and. latc <= max_lat).and.(lonc >= min_lon .and. lonc <= max_lon))
          nplus =  count(mask = mask)
          
          if(nplus < 0) then
             dw = dw + 1.0
             CYCLE
          endif
          
          allocate (sub_tid (1:nplus))
          allocate (sub_lon (1:nplus))
          allocate (sub_lat (1:nplus))
          allocate (rev_dist  (1:nplus))
          allocate (sub_ityp1 (1:nplus))
          allocate (sub_fevg1 (1:nplus))
          allocate (sub_ityp2 (1:nplus))
          allocate (sub_fevg2 (1:nplus))
          allocate (icl_ityp1 (1:nplus))
          
          sub_tid = PACK (tid_offl, mask= mask) 
          sub_lon = PACK (lonc    , mask= mask)
          sub_lat = PACK (latc    , mask= mask)
          
          ! compute distance from the tile
          
          sub_lat = sub_lat * MAPL_PI/180.
          sub_lon = sub_lon * MAPL_PI/180.
          
!          do i = 1,nplus
!             sub_dist(i) = haversine(to_radian(latt(local_id)), to_radian(lonn(local_id)), &
!                  sub_lat(i), sub_lon(i)) 
!             !                   sub_dist(i) = dist
!          end do
          
          ! loop through 4 vegetation types
           
          NVLOOP : do nv = 1, nveg
             
             if (nv == 1) ityp_new = CLMC_pt1(local_id)
             if (nv == 1) fveg_new = CLMC_pf1(local_id)
             if (nv == 2) ityp_new = CLMC_pt2(local_id)
             if (nv == 2) fveg_new = CLMC_pf2(local_id)
             if (nv == 3) ityp_new = CLMC_st1(local_id)
             if (nv == 3) fveg_new = CLMC_sf1(local_id)
             if (nv == 4) ityp_new = CLMC_st2(local_id) 
             if (nv == 4) fveg_new = CLMC_sf2(local_id)
             
             SEEK : if((Id_loc(local_id,nv) < 0).and.(fveg_new > fmin)) then
                
                if(nv <= 2) then ! index for secondary PFT index if primary or primary if secondary
                   nx = nv + 2
                else
                   nx = nv - 2
                endif
                
                sub_ityp1 = ityp_offl (sub_tid,nv)
                sub_fevg1 = fveg_offl (sub_tid,nv)
                sub_ityp2 = ityp_offl (sub_tid,nx)
                sub_fevg2 = fveg_offl (sub_tid,nx)
                
                rev_dist  = 1.e20
                icl_ityp1 = iclass(sub_ityp1)
                
                do i = 1,nplus
                   if((sub_ityp1(i)>fmin .and. (ityp_new ==sub_ityp1(i) .or.   &
                        iclass(ityp_new) ==iclass(sub_ityp1(i)))) .or.             &
                        (sub_fevg2(i)>fmin .and. (ityp_new ==sub_ityp2(i) .or. &
                        iclass(ityp_new)==iclass(sub_ityp2(i))))) then

                      sub_dist = haversine(to_radian(latt(local_id)), to_radian(lonn(local_id)), &
                           sub_lat(i), sub_lon(i))
                      
                      if(ityp_new == sub_ityp1(i) .and. sub_fevg1(i) >fmin) then
                         rev_dist(i) = 1.*sub_dist     ! give priority to same (primary if primary, secondary if secondary)   
                         ! gkw: these weights are tunable
                      else if(ityp_new ==sub_ityp2(i) .and. sub_fevg2(i)>fmin) then
                         rev_dist(i) = 2.*sub_dist     ! lower priority if not same (secondary if primary, primary if secondary)
                      else if(iclass(ityp_new)==iclass(sub_ityp1(i)) .and. sub_fevg1(i)>fmin) then
                         rev_dist(i) = 3.*sub_dist     ! even lower priority if same of some other PFT in same class
                      else if(sub_fevg2(i)>fmin) then
                         rev_dist(i) = 4.*sub_dist     ! even lower priority if not same of some other PFT in same class
                      else
                         rev_dist(i) = 1.e20
                      endif
                   endif                   
                end do

!                where ((sub_ityp1 == ityp_new) .and. (sub_fevg1 > fmin))  
!                   rev_dist = 1. * rev_dist    ! give priority to same (primary if primary, secondary if secondary)  
!                elsewhere ((sub_ityp2 == ityp_new) .and. (sub_fevg2 > fmin))  
!                   rev_dist = 2. * rev_dist    ! lower priority if not same (secondary if primary, primary if secondary)       
!                elsewhere ((icl_ityp1 == iclass(ityp_new)) .and. (sub_fevg1 > fmin))  
!                   rev_dist = 3. * rev_dist    ! even lower priority if same of some other PFT in same class
!                elsewhere (sub_fevg2 > fmin)
!                   rev_dist = 4. * rev_dist    ! even lower priority if not same of some other PFT in same class
!                elsewhere
!                   rev_dist = 1.e20
!                endwhere

                FOUND : if(minval (rev_dist) < 1.e19) then 
                   
                   Id_loc(local_id,nv) = sub_tid(minloc(rev_dist,1)) ! Cell ID of the nearest neightbor in the offline 
                                                                     ! restart file that has the same veg type
                                      
                endif FOUND
                
             endif SEEK
          end do NVLOOP
          
          deallocate (sub_tid, sub_lon, sub_lat, icl_ityp1)
          deallocate (sub_ityp1, sub_fevg1, sub_ityp2, sub_fevg2, rev_dist)  
          
          ! if similar types have been found for ITYPs with fveg > fmin within the window, exit
           
          all_found = .true.
          
          if((all_found).and.((CLMC_pf1(local_id) > fmin).and.(Id_loc(local_id,1) < 0))) all_found = .false.
          if((all_found).and.((CLMC_pf2(local_id) > fmin).and.(Id_loc(local_id,2) < 0))) all_found = .false.
          if((all_found).and.((CLMC_sf1(local_id) > fmin).and.(Id_loc(local_id,3) < 0))) all_found = .false.
          if((all_found).and.((CLMC_sf2(local_id) > fmin).and.(Id_loc(local_id,4) < 0))) all_found = .false.
          
          if(all_found) GO TO 100
           
           ! if not increase the window size
           dw = dw + 2.
           
        end do ZOOMOUT
        
100   continue !  if(mod (n,1000) == 0) print *, myid +1, n, Id_loc(local_id,:)

     END DO TILES

     deallocate (CLMC_pf1, CLMC_pf2, CLMC_sf1, CLMC_sf2)
     deallocate (CLMC_pt1, CLMC_pt2, CLMC_st1, CLMC_st2)
 
     ! update id_glb in root

     if(master_proc)  then
        allocate (id_glb  (ntiles,4))
        allocate (id_vec  (ntiles))
     endif
           
     do nv = 1, nveg
        call MPI_Barrier(MPI_COMM_WORLD, STATUS)
        call MPI_GATHERV( &
                   id_loc (:,nv), nt_local(myid+1)  , MPI_real, &
                   id_vec, nt_local,low_ind-1, MPI_real, &
                   0, MPI_COMM_WORLD, mpierr )
        
        if(master_proc) id_glb (:,nv) = id_vec
        
     end do

! write out regridded carbon variables

     if(master_proc) then

        allocate (CLMC_pf1(NTILES))
        allocate (CLMC_pf2(NTILES))
        allocate (CLMC_sf1(NTILES))
        allocate (CLMC_sf2(NTILES))
        allocate (CLMC_pt1(NTILES))
        allocate (CLMC_pt2(NTILES))
        allocate (CLMC_st1(NTILES))
        allocate (CLMC_st2(NTILES))
        allocate (VAR_DUM (NTILES))

        STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/1,1/), (/NTILES,1/),CLMC_pt1)
        STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/1,2/), (/NTILES,1/),CLMC_pt2)
        STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/1,3/), (/NTILES,1/),CLMC_st1)
        STATUS = NF_GET_VARA_REAL(OUTID,VID1, (/1,4/), (/NTILES,1/),CLMC_st2)
        STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/1,1/), (/NTILES,1/),CLMC_pf1)
        STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/1,2/), (/NTILES,1/),CLMC_pf2)
        STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/1,3/), (/NTILES,1/),CLMC_sf1)
        STATUS = NF_GET_VARA_REAL(OUTID,VID2, (/1,4/), (/NTILES,1/),CLMC_sf2)

        allocate (var_off_col (1: NTILES_CN, 1 : nzone,1 : var_col))
        allocate (var_off_pft (1: NTILES_CN, 1 : nzone,1 : nveg, 1 : var_pft))
        
        allocate (var_col_out (1: NTILES, 1 : nzone,1 : var_col))
        allocate (var_pft_out (1: NTILES, 1 : nzone,1 : nveg, 1 : var_pft)) 

        STATUS = NF_INQ_VARID (OutID,'CNCOL' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'CNCOL INQ')
        STATUS = NF_INQ_VARID (OutID,'CNPFT' ,VID2)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'CNPFT INQ') 
        
        i = 1
        do nv = 1,VAR_COL
           do nz = 1,nzone
              STATUS = NF_GET_VARA_REAL(NCFID,VID1, (/1,i/), (/NTILES_CN,1 /),var_off_col(:, nz,nv))
              i = i + 1
           end do
        end do
        
        i = 1
        do iv = 1,VAR_PFT
           do nv = 1,nveg
              do nz = 1,nzone
                 STATUS = NF_GET_VARA_REAL(NCFID,VID2, (/1,i/), (/NTILES_CN,1 /),var_off_pft(:, nz,nv,iv))
                 i = i + 1
              end do
           end do
        end do

        var_col_out = 0.
        var_pft_out = NaN
        
        where(isnan(var_off_pft))  var_off_pft = 0.   
        where(var_off_pft /= var_off_pft)  var_off_pft = 0.  

        OUT_TILE : DO N = 1, NTILES

           ! if(mod (n,1000) == 0) print *, myid +1, n, Id_glb(n,:)

           NVLOOP2 : do nv = 1, nveg

              if(nv <= 2) then ! index for secondary PFT index if primary or primary if secondary
                 nx = nv + 2
              else
                 nx = nv - 2
              endif

              if (nv == 1) ityp_new = CLMC_pt1(n)
              if (nv == 1) fveg_new = CLMC_pf1(n)
              if (nv == 2) ityp_new = CLMC_pt2(n)
              if (nv == 2) fveg_new = CLMC_pf2(n)
              if (nv == 3) ityp_new = CLMC_st1(n)
              if (nv == 3) fveg_new = CLMC_sf1(n)
              if (nv == 4) ityp_new = CLMC_st2(n) 
              if (nv == 4) fveg_new = CLMC_sf2(n)
              
              if (fveg_new > fmin) then

                 offl_cell    = Id_glb(n,nv)
                 
                 if(ityp_new      == ityp_offl (offl_cell,nv) .and. fveg_offl (offl_cell,nv)> fmin) then
                    iv = nv                                     ! same type fraction (primary of secondary)                          
                 else if(ityp_new == ityp_offl (offl_cell,nx) .and. fveg_offl (offl_cell,nx)> fmin) then
                    iv = nx                                     ! not same fraction
                 else if(iclass(ityp_new)==iclass(ityp_offl(offl_cell,nv)) .and. fveg_offl (offl_cell,nv)> fmin) then
                    iv = nv                                     ! primary, other type (same class)
                 else if(fveg_offl (offl_cell,nx)> fmin) then
                    iv = nx                                     ! secondary, other type (same class)
                 endif
                 
                 ! Get col and pft variables for the Id_glb(nv) grid cell from offline catchcn_internal_rst
                 ! ----------------------------------------------------------------------------------------
                 
                 ! call NCDF_reshape_getOput (NCFID,Id_glb(n,nv),var_off_col,var_off_pft,.true.)  
                 
                 var_pft_out (n,:,nv,:) = var_off_pft(Id_glb(n,nv), :,iv,:)                      
                 var_col_out (n,:,:)    = var_col_out(n,:,:) + fveg_new * var_off_col(Id_glb(n,nv), :,:) ! gkw: column state simple weighted mean; ! could use "woody" fraction?
       
              ! Check whether var_pft_out is realistic
                 do nz = 1, nzone
                    do j = 1, VAR_PFT
                       if (isnan(var_pft_out (n, nz,nv,j))) print *,j,nv,nz,n,var_pft_out (n, nz,nv,j),fveg_new                       
                       !if(isnan(var_pft_out (n, nz,nv,69))) var_pft_out (n, nz,nv,69) = 1.e-6
                       !if(isnan(var_pft_out (n, nz,nv,70))) var_pft_out (n, nz,nv,70) = 1.e-6   
                       !if(isnan(var_pft_out (n, nz,nv,73))) var_pft_out (n, nz,nv,73) = 1.e-6
                       !if(isnan(var_pft_out (n, nz,nv,74))) var_pft_out (n, nz,nv,74) = 1.e-6                 
                    end do
                 end do
              endif

           end do NVLOOP2

           ! reset carbon if negative < 10g
           ! ------------------------
           
           NZLOOP : do nz = 1, nzone

              if(var_col_out (n, nz,14) < 10.) then

                 var_col_out(n, nz, 1) = max(var_col_out(n, nz, 1), 0.)   
                 var_col_out(n, nz, 2) = max(var_col_out(n, nz, 2), 0.)   
                 var_col_out(n, nz, 3) = max(var_col_out(n, nz, 3), 0.)   
                 var_col_out(n, nz, 4) = max(var_col_out(n, nz, 4), 0.)   
                 var_col_out(n, nz, 5) = max(var_col_out(n, nz, 5), 0.)   
                 var_col_out(n, nz,10) = max(var_col_out(n, nz,10), 0.)   
                 var_col_out(n, nz,11) = max(var_col_out(n, nz,11), 0.)   
                 var_col_out(n, nz,12) = max(var_col_out(n, nz,12), 0.)   
                 var_col_out(n, nz,13) = max(var_col_out(n, nz,13),10.)   ! soil4c       
                 var_col_out(n, nz,14) = max(var_col_out(n, nz,14), 0.) 
                 var_col_out(n, nz,15) = max(var_col_out(n, nz,15), 0.)     
                 var_col_out(n, nz,16) = max(var_col_out(n, nz,16), 0.)     
                 var_col_out(n, nz,17) = max(var_col_out(n, nz,17), 0.)     
                 var_col_out(n, nz,18) = max(var_col_out(n, nz,18), 0.)     
                 var_col_out(n, nz,19) = max(var_col_out(n, nz,19), 0.)     
                 var_col_out(n, nz,20) = max(var_col_out(n, nz,20), 0.)     
                 var_col_out(n, nz,24) = max(var_col_out(n, nz,24), 0.)     
                 var_col_out(n, nz,25) = max(var_col_out(n, nz,25), 0.)     
                 var_col_out(n, nz,26) = max(var_col_out(n, nz,26), 0.)     
                 var_col_out(n, nz,27) = max(var_col_out(n, nz,27), 0.)     
                 var_col_out(n, nz,28) = max(var_col_out(n, nz,28), 1.)     
                 var_col_out(n, nz,29) = max(var_col_out(n, nz,29), 0.)     
                 
                 NVLOOP3 : do nv = 1,nveg

                    if (nv == 1) ityp_new = CLMC_pt1(n)
                    if (nv == 1) fveg_new = CLMC_pf1(n)
                    if (nv == 2) ityp_new = CLMC_pt2(n)
                    if (nv == 2) fveg_new = CLMC_pf2(n)
                    if (nv == 3) ityp_new = CLMC_st1(n)
                    if (nv == 3) fveg_new = CLMC_sf1(n)
                    if (nv == 4) ityp_new = CLMC_st2(n) 
                    if (nv == 4) fveg_new = CLMC_sf2(n)
                    
                    if(fveg_new > fmin) then
                       var_pft_out(n, nz,nv, 1) = max(var_pft_out(n, nz,nv, 1),0.)      
                       var_pft_out(n, nz,nv, 2) = max(var_pft_out(n, nz,nv, 2),0.)      
                       var_pft_out(n, nz,nv, 3) = max(var_pft_out(n, nz,nv, 3),0.)  
                       var_pft_out(n, nz,nv, 4) = max(var_pft_out(n, nz,nv, 4),0.)      
                       
                       if(ityp_new <= 12) then ! tree or shrub deadstemc
                          var_pft_out(n, nz,nv, 5) = max(var_pft_out(n, nz,nv, 5),0.1)
                       else            
                          var_pft_out(n, nz,nv, 5) = max(var_pft_out(n, nz,nv, 5),0.0)
                       endif
                       
                       var_pft_out(n, nz,nv, 6) = max(var_pft_out(n, nz,nv, 6),0.)
                       var_pft_out(n, nz,nv, 7) = max(var_pft_out(n, nz,nv, 7),0.)
                       var_pft_out(n, nz,nv, 8) = max(var_pft_out(n, nz,nv, 8),0.)
                       var_pft_out(n, nz,nv, 9) = max(var_pft_out(n, nz,nv, 9),0.)
                       var_pft_out(n, nz,nv,10) = max(var_pft_out(n, nz,nv,10),0.)
                       var_pft_out(n, nz,nv,11) = max(var_pft_out(n, nz,nv,11),0.)
                       var_pft_out(n, nz,nv,12) = max(var_pft_out(n, nz,nv,12),0.)
                       
                       if(ityp_new <=2 .or. ityp_new ==4 .or. ityp_new ==5 .or. ityp_new == 9) then
                          var_pft_out(n, nz,nv,13) = max(var_pft_out(n, nz,nv,13),1.)  ! leaf carbon display for evergreen
                          var_pft_out(n, nz,nv,14) = max(var_pft_out(n, nz,nv,14),0.)
                       else
                          var_pft_out(n, nz,nv,13) = max(var_pft_out(n, nz,nv,13),0.)               
                          var_pft_out(n, nz,nv,14) = max(var_pft_out(n, nz,nv,14),1.)  ! leaf carbon storage for deciduous
                       endif
                       
                       var_pft_out(n, nz,nv,15) = max(var_pft_out(n, nz,nv,15),0.)
                       var_pft_out(n, nz,nv,16) = max(var_pft_out(n, nz,nv,16),0.)
                       var_pft_out(n, nz,nv,17) = max(var_pft_out(n, nz,nv,17),0.)
                       var_pft_out(n, nz,nv,18) = max(var_pft_out(n, nz,nv,18),0.)
                       var_pft_out(n, nz,nv,19) = max(var_pft_out(n, nz,nv,19),0.)
                       var_pft_out(n, nz,nv,20) = max(var_pft_out(n, nz,nv,20),0.)
                       var_pft_out(n, nz,nv,21) = max(var_pft_out(n, nz,nv,21),0.)
                       var_pft_out(n, nz,nv,22) = max(var_pft_out(n, nz,nv,22),0.)
                       var_pft_out(n, nz,nv,23) = max(var_pft_out(n, nz,nv,23),0.)
                       var_pft_out(n, nz,nv,25) = max(var_pft_out(n, nz,nv,25),0.)
                       var_pft_out(n, nz,nv,26) = max(var_pft_out(n, nz,nv,26),0.)
                       var_pft_out(n, nz,nv,27) = max(var_pft_out(n, nz,nv,27),0.)
                       var_pft_out(n, nz,nv,41) = max(var_pft_out(n, nz,nv,41),0.)
                       var_pft_out(n, nz,nv,42) = max(var_pft_out(n, nz,nv,42),0.)
                       var_pft_out(n, nz,nv,44) = max(var_pft_out(n, nz,nv,44),0.)
                       var_pft_out(n, nz,nv,45) = max(var_pft_out(n, nz,nv,45),0.)
                       var_pft_out(n, nz,nv,46) = max(var_pft_out(n, nz,nv,46),0.)
                       var_pft_out(n, nz,nv,47) = max(var_pft_out(n, nz,nv,47),0.)
                       var_pft_out(n, nz,nv,48) = max(var_pft_out(n, nz,nv,48),0.)
                       var_pft_out(n, nz,nv,49) = max(var_pft_out(n, nz,nv,49),0.)
                       var_pft_out(n, nz,nv,50) = max(var_pft_out(n, nz,nv,50),0.)
                       var_pft_out(n, nz,nv,51) = max(var_pft_out(n, nz,nv, 5)/500.,0.)            
                       var_pft_out(n, nz,nv,52) = max(var_pft_out(n, nz,nv,52),0.)
                       var_pft_out(n, nz,nv,53) = max(var_pft_out(n, nz,nv,53),0.)
                       var_pft_out(n, nz,nv,54) = max(var_pft_out(n, nz,nv,54),0.)
                       var_pft_out(n, nz,nv,55) = max(var_pft_out(n, nz,nv,55),0.)
                       var_pft_out(n, nz,nv,56) = max(var_pft_out(n, nz,nv,56),0.)
                       var_pft_out(n, nz,nv,57) = max(var_pft_out(n, nz,nv,13)/25.,0.)        
                       var_pft_out(n, nz,nv,58) = max(var_pft_out(n, nz,nv,14)/25.,0.)        
                       var_pft_out(n, nz,nv,59) = max(var_pft_out(n, nz,nv,59),0.)
                       var_pft_out(n, nz,nv,60) = max(var_pft_out(n, nz,nv,60),0.)  
                       var_pft_out(n, nz,nv,61) = max(var_pft_out(n, nz,nv,61),0.)  
                       var_pft_out(n, nz,nv,62) = max(var_pft_out(n, nz,nv,62),0.)  
                       var_pft_out(n, nz,nv,63) = max(var_pft_out(n, nz,nv,63),0.)  
                       var_pft_out(n, nz,nv,64) = max(var_pft_out(n, nz,nv,64),0.)  
                       var_pft_out(n, nz,nv,65) = max(var_pft_out(n, nz,nv,65),0.)  
                       var_pft_out(n, nz,nv,66) = max(var_pft_out(n, nz,nv,66),0.)  
                       var_pft_out(n, nz,nv,67) = max(var_pft_out(n, nz,nv,67),0.)  
                       var_pft_out(n, nz,nv,68) = max(var_pft_out(n, nz,nv,68),0.)  
                       var_pft_out(n, nz,nv,69) = max(var_pft_out(n, nz,nv,69),0.)  
                       var_pft_out(n, nz,nv,70) = max(var_pft_out(n, nz,nv,70),0.)  
                       var_pft_out(n, nz,nv,73) = max(var_pft_out(n, nz,nv,73),0.)  
                       var_pft_out(n, nz,nv,74) = max(var_pft_out(n, nz,nv,74),0.)  
                    endif
                 end do NVLOOP3  ! end veg loop                 
              endif    ! end carbon check         
           end do NZLOOP ! end zone loop
           
           ! Update dayx variable var_pft_out (:,:,28)
           
           do j = 28, 28  !  1,VAR_PFT var_pft_out (:,:,:,28)
              do nv = 1,nveg
                 do nz = 1,nzone
                    var_pft_out (n, nz,nv,j) = dayx(n)
                 end do
              end do
           end do           

           ! call NCDF_reshape_getOput (OutID,N,var_col_out,var_pft_out,.false.)  
           
           ! column vars
           ! -----------
           !  1 clm3%g%l%c%ccs%col_ctrunc	    
           !  2 clm3%g%l%c%ccs%cwdc			    
           !  3 clm3%g%l%c%ccs%litr1c 	    
           !  4 clm3%g%l%c%ccs%litr2c 	    
           !  5 clm3%g%l%c%ccs%litr3c 	    
           !  6 clm3%g%l%c%ccs%pcs_a%totvegc      
           !  7 clm3%g%l%c%ccs%prod100c	    
           !  8 clm3%g%l%c%ccs%prod10c	    
           !  9 clm3%g%l%c%ccs%seedc  	    
           ! 10 clm3%g%l%c%ccs%soil1c 	    
           ! 11 clm3%g%l%c%ccs%soil2c 	    
           ! 12 clm3%g%l%c%ccs%soil3c 	    
           ! 13 clm3%g%l%c%ccs%soil4c 	    
           ! 14 clm3%g%l%c%ccs%totcolc	    
           ! 15 clm3%g%l%c%ccs%totlitc	    
           ! 16 clm3%g%l%c%cns%col_ntrunc	    
           ! 17 clm3%g%l%c%cns%cwdn			    
           ! 18 clm3%g%l%c%cns%litr1n 	    
           ! 19 clm3%g%l%c%cns%litr2n 	    
           ! 20 clm3%g%l%c%cns%litr3n 	    
           ! 21 clm3%g%l%c%cns%prod100n	    
           ! 22 clm3%g%l%c%cns%prod10n	    
           ! 23 clm3%g%l%c%cns%seedn  	    
           ! 24 clm3%g%l%c%cns%sminn  	    
           ! 25 clm3%g%l%c%cns%soil1n 	    
           ! 26 clm3%g%l%c%cns%soil2n 	    
           ! 27 clm3%g%l%c%cns%soil3n 	    
           ! 28 clm3%g%l%c%cns%soil4n 	    
           ! 29 clm3%g%l%c%cns%totcoln	    
           ! 30 clm3%g%l%c%cps%ann_farea_burned   
           ! 31 clm3%g%l%c%cps%annsum_counter     
           ! 32 clm3%g%l%c%cps%cannavg_t2m		    
           ! 33 clm3%g%l%c%cps%cannsum_npp		    
           ! 34 clm3%g%l%c%cps%farea_burned		    
           ! 35 clm3%g%l%c%cps%fire_prob	    
           ! 36 clm3%g%l%c%cps%fireseasonl		    
           ! 37 clm3%g%l%c%cps%fpg			    
           ! 38 clm3%g%l%c%cps%fpi			    
           ! 39 clm3%g%l%c%cps%me		    
           ! 40 clm3%g%l%c%cps%mean_fire_prob     
           
           ! PFT vars
           ! --------
           !  1 clm3%g%l%c%p%pcs%cpool	       
           !  2 clm3%g%l%c%p%pcs%deadcrootc	       
           !  3 clm3%g%l%c%p%pcs%deadcrootc_storage	
           !  4 clm3%g%l%c%p%pcs%deadcrootc_xfer  
           !  5 clm3%g%l%c%p%pcs%deadstemc 	       
           !  6 clm3%g%l%c%p%pcs%deadstemc_storage 	
           !  7 clm3%g%l%c%p%pcs%deadstemc_xfer   
           !  8 clm3%g%l%c%p%pcs%frootc	       
           !  9 clm3%g%l%c%p%pcs%frootc_storage   
           ! 10 clm3%g%l%c%p%pcs%frootc_xfer      
           ! 11 clm3%g%l%c%p%pcs%gresp_storage    
           ! 12 clm3%g%l%c%p%pcs%gresp_xfer	       
           ! 13 clm3%g%l%c%p%pcs%leafc	       
           ! 14 clm3%g%l%c%p%pcs%leafc_storage	
           ! 15 clm3%g%l%c%p%pcs%leafc_xfer	       
           ! 16 clm3%g%l%c%p%pcs%livecrootc		
           ! 17 clm3%g%l%c%p%pcs%livecrootc_storage	
           ! 18 clm3%g%l%c%p%pcs%livecrootc_xfer  
           ! 19 clm3%g%l%c%p%pcs%livestemc 	       
           ! 20 clm3%g%l%c%p%pcs%livestemc_storage 	
           ! 21 clm3%g%l%c%p%pcs%livestemc_xfer   
           ! 22 clm3%g%l%c%p%pcs%pft_ctrunc	       
           ! 23 clm3%g%l%c%p%pcs%xsmrpool         
           ! 24 clm3%g%l%c%p%pepv%annavg_t2m      
           ! 25 clm3%g%l%c%p%pepv%annmax_retransn 
           ! 26 clm3%g%l%c%p%pepv%annsum_npp      
           ! 27 clm3%g%l%c%p%pepv%annsum_potential_gpp 
           ! 28 clm3%g%l%c%p%pepv%dayl	       
           ! 29 clm3%g%l%c%p%pepv%days_active     
           ! 30 clm3%g%l%c%p%pepv%dormant_flag    
           ! 31 clm3%g%l%c%p%pepv%offset_counter  
           ! 32 clm3%g%l%c%p%pepv%offset_fdd      
           ! 33 clm3%g%l%c%p%pepv%offset_flag     
           ! 34 clm3%g%l%c%p%pepv%offset_swi      
           ! 35 clm3%g%l%c%p%pepv%onset_counter   
           ! 36 clm3%g%l%c%p%pepv%onset_fdd	       
           ! 37 clm3%g%l%c%p%pepv%onset_flag      
           ! 38 clm3%g%l%c%p%pepv%onset_gdd	       
           ! 39 clm3%g%l%c%p%pepv%onset_gddflag   
           ! 40 clm3%g%l%c%p%pepv%onset_swi	       
           ! 41 clm3%g%l%c%p%pepv%prev_frootc_to_litter
           ! 42 clm3%g%l%c%p%pepv%prev_leafc_to_litter 
           ! 43 clm3%g%l%c%p%pepv%tempavg_t2m     
           ! 44 clm3%g%l%c%p%pepv%tempmax_retransn 	
           ! 45 clm3%g%l%c%p%pepv%tempsum_npp     
           ! 46 clm3%g%l%c%p%pepv%tempsum_potential_gpp
           ! 47 clm3%g%l%c%p%pepv%xsmrpool_recover 	
           ! 48 clm3%g%l%c%p%pns%deadcrootn	       
           ! 49 clm3%g%l%c%p%pns%deadcrootn_storage	
           ! 50 clm3%g%l%c%p%pns%deadcrootn_xfer  
           ! 51 clm3%g%l%c%p%pns%deadstemn 	       
           ! 52 clm3%g%l%c%p%pns%deadstemn_storage 	
           ! 53 clm3%g%l%c%p%pns%deadstemn_xfer   
           ! 54 clm3%g%l%c%p%pns%frootn	       
           ! 55 clm3%g%l%c%p%pns%frootn_storage   
           ! 56 clm3%g%l%c%p%pns%frootn_xfer      
           ! 57 clm3%g%l%c%p%pns%leafn	       
           ! 58 clm3%g%l%c%p%pns%leafn_storage    
           ! 59 clm3%g%l%c%p%pns%leafn_xfer	       
           ! 60 clm3%g%l%c%p%pns%livecrootn	       
           ! 61 clm3%g%l%c%p%pns%livecrootn_storage	
           ! 62 clm3%g%l%c%p%pns%livecrootn_xfer  
           ! 63 clm3%g%l%c%p%pns%livestemn 	       
           ! 64 clm3%g%l%c%p%pns%livestemn_storage 	
           ! 65 clm3%g%l%c%p%pns%livestemn_xfer   
           ! 66 clm3%g%l%c%p%pns%npool	    
           ! 67 clm3%g%l%c%p%pns%pft_ntrunc		    
           ! 68 clm3%g%l%c%p%pns%retransn	    
           ! 69 clm3%g%l%c%p%pps%elai 	    
           ! 70 clm3%g%l%c%p%pps%esai 	    
           ! 71 clm3%g%l%c%p%pps%hbot 	      
           ! 72 clm3%g%l%c%p%pps%htop 	      
           ! 73 clm3%g%l%c%p%pps%tlai 	    
           ! 74 clm3%g%l%c%p%pps%tsai 	    
           
        end do OUT_TILE
        
        i = 1
        do nv = 1,VAR_COL
           do nz = 1,nzone
              STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1,i/), (/NTILES,1 /),var_col_out(:, nz,nv))
              i = i + 1
           end do
        end do
        
        i = 1
        do iv = 1,VAR_PFT
           do nv = 1,nveg
              do nz = 1,nzone
                 STATUS = NF_PUT_VARA_REAL(OutID,VID2, (/1,i/), (/NTILES,1 /),var_pft_out(:, nz,nv,iv))
                 i = i + 1
              end do
           end do
        end do

        VAR_DUM = 0.

        STATUS = NF_INQ_VARID (OutID,'TGWM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'TGWM INQ')
        do nz = 1,nzone
           STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1,nz/), (/NTILES,1 /),VAR_DUM(:))
        end do

        STATUS = NF_INQ_VARID (OutID,'RZMM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'RZMM INQ')

        do nz = 1,nzone
           STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1,nz/), (/NTILES,1 /),VAR_DUM(:))
        end do

        STATUS = NF_INQ_VARID (OutID,'SFMCM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'SFMCM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'BFLOWM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'BFLOWM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'TOTWATM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'TOTWATM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'TAIRM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'TAIRM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'TPM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'TPM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'CNSUM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'CNSUM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_INQ_VARID (OutID,'PSNSUNM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'PSNSUNM INQ')
        do nv = 1,nzone
           do nz = 1,nveg
              STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1,nz,nv/), (/NTILES,1,1/),VAR_DUM(:))
           end do
        end do

        STATUS = NF_INQ_VARID (OutID,'PSNSHAM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'PSNSHAM INQ')
        do nv = 1,nzone
           do nz = 1,nveg
              STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1,nz,nv/), (/NTILES,1,1/),VAR_DUM(:))
           end do
        end do

        STATUS = NF_INQ_VARID (OutID,'SNDZM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'SNDZM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))
        
        STATUS = NF_INQ_VARID (OutID,'ASNOWM' ,VID1)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS, 'ASNOWM INQ')
        STATUS = NF_PUT_VARA_REAL(OutID,VID1, (/1/), (/NTILES/),VAR_DUM(:))

        STATUS = NF_CLOSE (NCFID)
        STATUS = NF_CLOSE (OutID)

        deallocate (var_off_col,var_off_pft,var_col_out,var_pft_out)  
        deallocate (CLMC_pf1, CLMC_pf2, CLMC_sf1, CLMC_sf2)
        deallocate (CLMC_pt1, CLMC_pt2, CLMC_st1, CLMC_st2)
 
     endif

     call MPI_Barrier(MPI_COMM_WORLD, STATUS)

  END SUBROUTINE regrid_carbon_vars

  ! *****************************************************************************

   SUBROUTINE put_catch (NTILES, NTILES_IN, id_glb, ld_reorder, ldas_rst, cat_para)

     implicit none
     
     integer, intent (in)       :: NTILES, NTILES_IN
     integer, intent (in)       :: id_glb(NTILES), ld_reorder (NTILES_IN)
     character (*), intent (in) :: ldas_rst, cat_para
     integer                    :: i,k,n
     real   , dimension (:), allocatable :: var_get, var_put
     integer, dimension (:), allocatable :: int_get
     type(MAPL_NCIO) :: InNCIO1,InNCIO2, InNCIO 
     integer         :: nVars

     allocate (var_get (NTILES_IN))
     allocate (int_get (NTILES_IN))
     allocate (var_put (NTILES))

     InNCIO = MAPL_NCIOOpen('/gpfsm/dnb42/projects/p16/ssd/land/l_data/LandRestarts_for_Regridding/Catch/GEOS5/catch_internal_rst' ,rc=rc)
     if(rc /= 0) then
        print *,' Here '
        stop
     endif
     call MAPL_NCIOGetDimSizes(InNCIO,nVars=nVars)
     call MAPL_NCIOChangeRes(InNCIO,InNCIO1,tileSize=ntiles,rc=rc)

     OutFileName = "OutData1/catch_internal_rst"
     call MAPL_NCIOSet( InNCIO1,filename=OutFileName )
     call MAPL_NCIOCreateFile(InNCIO1) 
     call MAPL_NCIOClose ( InNCIO)
   
     ! create output catchcn_internal_rst
     InNCIO = MAPL_NCIOOpen('/gpfsm/dnb42/projects/p16/ssd/land/l_data/LandRestarts_for_Regridding/CatchCN/catchcn_internal_dummy' ,rc=rc) 
     call MAPL_NCIOGetDimSizes(InNCIO,nVars=nVars)
     call MAPL_NCIOChangeRes(InNCIO,InNCIO2,tileSize=ntiles,rc=rc)

     OutFileName = "OutData1/catchcn_internal_rst"
     call MAPL_NCIOSet( InNCIO2,filename=OutFileName )
     call MAPL_NCIOCreateFile(InNCIO2) 
     call MAPL_NCIOClose ( InNCIO)

     ! Read catparam
     ! -------------

     open (10,file = trim(cat_para), form= 'unformatted',convert= 'big_endian', &
          status = 'old', action = 'read')

     read (10) (var_get(n), n = 1,NTILES_IN)
     read (10) (var_get(n), n = 1,NTILES_IN)
     read (10) (var_get(n), n = 1,NTILES_IN)
     read (10) (var_get(n), n = 1,NTILES_IN)
     
     do k=1,6
        read (10) (var_get(n), n = 1,NTILES_IN)
     end do

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'POROS',var_put) 
     call MAPL_VarWrite(InNCIO2,'POROS',var_put)
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'COND',var_put) 
     call MAPL_VarWrite(InNCIO2,'COND',var_put)
 
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'PSIS',var_put)
     call MAPL_VarWrite(InNCIO2,'PSIS',var_put)

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'BEE',var_put) 
     call MAPL_VarWrite(InNCIO2,'BEE',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'WPWET',var_put) 
     call MAPL_VarWrite(InNCIO2,'WPWET',var_put) 
    
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GNU',var_put) 
     call MAPL_VarWrite(InNCIO2,'GNU',var_put)
 
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'VGWMAX',var_put)  
     call MAPL_VarWrite(InNCIO2,'VGWMAX',var_put) 
 
     read (10) (int_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = real(var_get(ld_reorder(id_glb(k))))
     end do
     call MAPL_VarWrite(InNCIO1,'OLD_ITY',var_put) 
     
     read (10) (int_get(n), n = 1,NTILES_IN)
     read (10) (int_get(n), n = 1,NTILES_IN)
         
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'BF1',var_put) 
     call MAPL_VarWrite(InNCIO2,'BF1',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'BF2',var_put) 
     call MAPL_VarWrite(InNCIO2,'BF2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'BF3',var_put) 
     call MAPL_VarWrite(InNCIO2,'BF3',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'CDCR1',var_put) 
     call MAPL_VarWrite(InNCIO2,'CDCR1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'CDCR2',var_put) 
     call MAPL_VarWrite(InNCIO2,'CDCR2',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARS1',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARS1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARS2',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARS2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARS3',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARS3',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARA1',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARA1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARA2',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARA2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARA3',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARA3',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARA4',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARA4',var_put)

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARW1',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARW1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARW2',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARW2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARW3',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARW3',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ARW4',var_put) 
     call MAPL_VarWrite(InNCIO2,'ARW4',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'TSA1',var_put) 
     call MAPL_VarWrite(InNCIO2,'TSA1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'TSA2',var_put) 
     call MAPL_VarWrite(InNCIO2,'TSA2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'TSB1',var_put) 
     call MAPL_VarWrite(InNCIO2,'TSB1',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'TSB2',var_put) 
     call MAPL_VarWrite(InNCIO2,'TSB2',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'ATAU',var_put) 
     call MAPL_VarWrite(InNCIO2,'ATAU',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'BTAU',var_put) 
     call MAPL_VarWrite(InNCIO2,'BTAU',var_put) 
     
     close (10, status = 'keep')

     ! read restart and regrid
     ! -----------------------
    
     open (10,file = trim(ldas_rst), form= 'unformatted',convert= 'big_endian', &
          status = 'old', action = 'read')
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'TC',var_put, offset1=1) 
     call MAPL_VarWrite(InNCIO2,'TC',var_put, offset1=1) 
     call MAPL_VarWrite(InNCIO2,'TG',var_put, offset1=1) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'TC',var_put, offset1=2) 
     call MAPL_VarWrite(InNCIO2,'TC',var_put, offset1=2) 
     call MAPL_VarWrite(InNCIO2,'TG',var_put, offset1=2) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'TC',var_put, offset1=3) 
     call MAPL_VarWrite(InNCIO2,'TC',var_put, offset1=3) 
     call MAPL_VarWrite(InNCIO2,'TG',var_put, offset1=3) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'QC',var_put, offset1=1) 
     call MAPL_VarWrite(InNCIO2,'QC',var_put, offset1=1) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'QC',var_put, offset1=2) 
     call MAPL_VarWrite(InNCIO2,'QC',var_put, offset1=2)

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'QC',var_put, offset1=3) 
     call MAPL_VarWrite(InNCIO2,'QC',var_put, offset1=3)

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'CAPAC',var_put) 
     call MAPL_VarWrite(InNCIO2,'CAPAC',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'CATDEF',var_put) 
     call MAPL_VarWrite(InNCIO2,'CATDEF',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'RZEXC',var_put) 
     call MAPL_VarWrite(InNCIO2,'RZEXC',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'SRFEXC',var_put) 
     call MAPL_VarWrite(InNCIO2,'SRFEXC',var_put) 
     
     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do

     call MAPL_VarWrite(InNCIO1,'GHTCNT1',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT1',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GHTCNT2',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GHTCNT3',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT3',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GHTCNT4',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT4',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GHTCNT5',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT5',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'GHTCNT6',var_put) 
     call MAPL_VarWrite(InNCIO2,'GHTCNT6',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'WESNN1',var_put) 
     call MAPL_VarWrite(InNCIO2,'WESNN1',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'WESNN2',var_put) 
     call MAPL_VarWrite(InNCIO2,'WESNN2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'WESNN3',var_put) 
     call MAPL_VarWrite(InNCIO2,'WESNN3',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'HTSNNN1',var_put) 
     call MAPL_VarWrite(InNCIO2,'HTSNNN1',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'HTSNNN2',var_put) 
     call MAPL_VarWrite(InNCIO2,'HTSNNN2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'HTSNNN3',var_put) 
     call MAPL_VarWrite(InNCIO2,'HTSNNN3',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'SNDZN1',var_put) 
     call MAPL_VarWrite(InNCIO2,'SNDZN1',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'SNDZN2',var_put) 
     call MAPL_VarWrite(InNCIO2,'SNDZN2',var_put) 

     read (10) (var_get(n), n = 1,NTILES_IN)
     do k = 1, NTILES
        VAR_PUT(k) = var_get(ld_reorder(id_glb(k)))
     end do
     call MAPL_VarWrite(InNCIO1,'SNDZN3',var_put) 
     call MAPL_VarWrite(InNCIO2,'SNDZN3',var_put) 
     
     close (10, status = 'keep')

     call MAPL_NCIOClose ( InNCIO1)
     call MAPL_NCIOClose ( InNCIO2)

     deallocate (var_get, int_get, var_put)
  
     call system('/bin/cp OutData1/catchcn_internal_rst OutData2/catchcn_internal_rst')
     call system('/bin/cp OutData1/catch_internal_rst OutData2/catch_internal_rst')

   END SUBROUTINE put_catch


  ! *****************************************************************************
  
  subroutine ReadCNTilFile (InCNTileFile, nt, xlon, xlat)
    
     implicit none
     character(*), intent (in) ::  InCNTileFile
     integer , intent (in) :: nt
     real, dimension (nt), intent(inout) :: xlon, xlat
     integer :: n,icnt,ityp,status
     real    :: xval,yval, pf
     
   open(11,file=InCNTileFile, &
        form='formatted',action='read',status='old')

   do n = 1,8 ! skip header
      read(11,*)
   end do
   
   icnt = 0
   ityp = 100

   do while (ityp == 100) ! loop over land tiles
      read(11,*, iostat = status) ityp,pf,xval,yval
      if (status /= 0) exit
      if((ityp == 100).and.(status == 0)) then
         icnt = icnt + 1
         xlon(icnt) = xval
         xlat(icnt) = yval
      endif
   end do

   close(11)
    

   end subroutine ReadCNTilFile

  ! *****************************************************************************
  
  subroutine init_MPI()
    
    ! initialize MPI
    
    call MPI_INIT(mpierr)
    
    call MPI_COMM_RANK( MPI_COMM_WORLD, myid, mpierr )
    call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, mpierr )

    if (myid .ne. 0)  master_proc = .false.
    
!    call init_MPI_types()
    
    write (*,*) "MPI process ", myid, " of ", numprocs, " is alive"    
    write (*,*) "MPI process ", myid, ": master_proc=", master_proc

  end subroutine init_MPI

  ! *****************************************************************************

     SUBROUTINE HANDLE_ERR(STATUS, LNo)
     INTEGER, INTENT (IN) :: STATUS
     CHARACTER(*), INTENT (IN) :: LNo
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, trim(LNo),': ',NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
   END SUBROUTINE HANDLE_ERR

  ! *****************************************************************************

  subroutine compute_dayx (                               &
       NTILES, AGCM_YY, AGCM_MM, AGCM_DD, AGCM_HR,        &
       LATT, DAYX)
 
    implicit none

    integer, intent (in) :: NTILES,AGCM_YY,AGCM_MM,AGCM_DD,AGCM_HR 
    real, dimension (NTILES), intent (in)  :: LATT
    real, dimension (NTILES), intent (out) :: DAYX
    integer, parameter :: DT = 900
    integer, parameter :: ncycle = 1461 ! number of days in a 4-year leap cycle (365*4 + 1)   
    real, dimension(ncycle) :: zc, zs
    integer :: dofyr, sec,YEARS_PER_CYCLE, DAYS_PER_CYCLE, year, iday, idayp1, nn, n 
    real    :: fac, YEARLEN, zsin, zcos, declin
    
    dofyr = AGCM_DD
    if(AGCM_MM >  1) dofyr = dofyr + 31
    if(AGCM_MM >  2) then
       dofyr = dofyr + 28
       if(mod(AGCM_YY,4) == 0) dofyr = dofyr + 1
    endif
    if(AGCM_MM >  3) dofyr = dofyr + 31
    if(AGCM_MM >  4) dofyr = dofyr + 30
    if(AGCM_MM >  5) dofyr = dofyr + 31
    if(AGCM_MM >  6) dofyr = dofyr + 30
    if(AGCM_MM >  7) dofyr = dofyr + 31
    if(AGCM_MM >  8) dofyr = dofyr + 31
    if(AGCM_MM >  9) dofyr = dofyr + 30
    if(AGCM_MM > 10) dofyr = dofyr + 31
    if(AGCM_MM > 11) dofyr = dofyr + 30
      
    sec = AGCM_HR * 3600 - DT ! subtract DT to get time of previous physics step
    fac = real(sec) / 86400.

    call orbit_create(zs,zc,ncycle) ! GEOS5 leap cycle routine
    
    YEARLEN = 365.25
  
    !  Compute length of leap cycle
    !------------------------------

    if(YEARLEN-int(YEARLEN) > 0.) then
       YEARS_PER_CYCLE = nint(1./(YEARLEN-int(YEARLEN)))
    else
       YEARS_PER_CYCLE = 1
    endif
  
    DAYS_PER_CYCLE=nint(YEARLEN*YEARS_PER_CYCLE)
    
    ! declination & daylength
    ! -----------------------

    YEAR = mod(AGCM_YY-1,YEARS_PER_CYCLE)
  
    IDAY = YEAR*int(YEARLEN)+dofyr
    IDAYP1 = mod(IDAY,DAYS_PER_CYCLE) + 1

    ZSin = ZS(IDAYP1)*FAC + ZS(IDAY)*(1.-FAC) !   sine of solar declination
    ZCos = ZC(IDAYP1)*FAC + ZC(IDAY)*(1.-FAC) ! cosine of solar declination
    
    nn = 0
    do n = 1,days_per_cycle
       nn = nn + 1
       if(nn > 365) nn = nn - 365
       !     print *, 'cycle:',n,nn,asin(ZS(n))
    end do
    
    declin = asin(ZSin)
  
    ! compute daylength on input tile space (accounts for any change in physics time step)  
    !  do n = 1,ntiles_cn
    !     fac = -(sin((latc(n)/zoom)*(MAPL_PI/180.))*zsin)/(cos((latc(n)/zoom)*(MAPL_PI/180.))*zcos)
    !     fac = min(1.,max(-1.,fac))
    !     dayl(n) = (86400./MAPL_PI) * acos(fac)   ! daylength (seconds)
    !  end do
  
    ! compute daylength on output tile space (accounts for lat shift due to split & change in time step)
    
    do n = 1,ntiles
       fac = -(sin(latt(n)*(MAPL_PI/180.))*zsin)/(cos(latt(n)*(MAPL_PI/180.))*zcos)
       fac = min(1.,max(-1.,fac))
       dayx(n) = (86400./MAPL_PI) * acos(fac)   ! daylength (seconds)
    end do
    
    ! print *,'DAYX : ', minval(dayx),maxval(dayx), minval(latt), maxval(latt), zsin, zcos, dofyr, iday, idayp1, declin
 
  end subroutine compute_dayx

  ! *****************************************************************************

   subroutine orbit_create(zs,zc,ncycle)
     
     implicit none
     
     integer, intent(in) :: ncycle
     real, intent(out), dimension(ncycle) :: zs, zc
     
     integer :: YEARS_PER_CYCLE, DAYS_PER_CYCLE
     integer :: K, KP !, KM
     real*8  :: T1, T2, T3, T4, FUN, Y, SOB, OMG, PRH, TT
     real*8  :: YEARLEN
     
     !  STATEMENT FUNCTION
     
     FUN(Y) = OMG*(1.0-ECCENTRICITY*cos(Y-PRH))**2
     
     YEARLEN = 365.25
     
     !  Factors involving the orbital parameters
     !------------------------------------------
     
     OMG  = (2.0*MAPL_PI/YEARLEN) / (sqrt(1.-ECCENTRICITY**2)**3)
     PRH  = PERIHELION*(MAPL_PI/180.)
     SOB  = sin(OBLIQUITY*(MAPL_PI/180.))
     
     !  Compute length of leap cycle
     !------------------------------
     
     if(YEARLEN-int(YEARLEN) > 0.) then
        YEARS_PER_CYCLE = nint(1./(YEARLEN-int(YEARLEN)))
     else
        YEARS_PER_CYCLE = 1
     endif
     
     DAYS_PER_CYCLE=nint(YEARLEN*YEARS_PER_CYCLE)
     
     if(days_per_cycle /= ncycle) stop 'bad cycle'
     
     !   ZS:   Sine of declination
     !   ZC:   Cosine of declination
     
     !  Begin integration at vernal equinox
     
     KP           = EQUINOX
     TT           = 0.0
     ZS(KP) = sin(TT)*SOB
     ZC(KP) = sqrt(1.0-ZS(KP)**2)
     
     !  Integrate orbit for entire leap cycle using Runge-Kutta
     
     do K=2,DAYS_PER_CYCLE
        T1 = FUN(TT       )
        T2 = FUN(TT+T1*0.5)
        T3 = FUN(TT+T2*0.5)
        T4 = FUN(TT+T3    )
        KP  = mod(KP,DAYS_PER_CYCLE) + 1
        TT  = TT + (T1 + 2.0*(T2 + T3) + T4) / 6.0
        ZS(KP) = sin(TT)*SOB
        ZC(KP) = sqrt(1.0-ZS(KP)**2)
     end do
     
   end subroutine orbit_create

  ! *****************************************************************************

   function to_radian(degree) result(rad)

     ! degrees to radians
     real,intent(in) :: degree
     real :: rad

     rad = degree*MAPL_PI/180.

   end function to_radian

   ! *****************************************************************************
   
   real function haversine(deglat1,deglon1,deglat2,deglon2)
     ! great circle distance -- adapted from Matlab 
     real,intent(in) :: deglat1,deglon1,deglat2,deglon2
     real :: a,c, dlat,dlon,lat1,lat2
     real,parameter :: radius = MAPL_radius
     
!     dlat = to_radian(deglat2-deglat1)
!     dlon = to_radian(deglon2-deglon1)
     !     lat1 = to_radian(deglat1)
!     lat2 = to_radian(deglat2)
     dlat = deglat2-deglat1
     dlon = deglon2-deglon1
     lat1 = deglat1
     lat2 = deglat2     
     a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
     if(a>=0. .and. a<=1.) then
        c = 2*atan2(sqrt(a),sqrt(1-a))
        haversine = radius*c / 1000.
     else
        haversine = 1.e20
     endif
   end function

  ! *****************************************************************************

 END PROGRAM mk_LDASsaRestarts
