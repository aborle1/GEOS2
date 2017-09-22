

module forcepert_types
  
  implicit none

  save
  
  ! everything is private by default unless made public
  
  private
  
  public :: forcepert_param_type
  public :: allocate_forcepert_param
  
  ! --------------------------------------------------------------------
  !
  ! parameters for each kind of forcing perturbation (precip, radiation, etc)
  
  type :: forcepert_param_type
     
     character(40)    :: descr    ! 'precip', 'shortwave', etc
     integer          :: typ      ! add or multiply model error?
     
     ! max allowed normalized perturbation (relative to N(0,1))
     
     real             :: std_normal_max  
     
     ! if .true. enforce zeromean across ensemble 
     ! (implies mean=1 for multiplicative perturbations)
     ! (not applicable if only one ensemble member is done at a time)
     
     logical          :: zeromean        ! enforce zero mean across ensemble
     
     ! Mean and std are allowed to vary in space (dimension(N_x,N_y)).
     
     real, dimension(:,:), pointer :: mean      ! mean
     real, dimension(:,:), pointer :: std       ! standard deviation
     
     ! Cross-correlations between different kinds of forcing perturbations
     ! (eg. between precip and shortwave perturbations) are allowed to vary
     ! in space (dimension(N_forcepert_kind,N_x,N_y)).
     
     real, dimension(:,:,:), pointer :: ccorr
     
     ! Spatial and temporal correlation scales must be constant in space.
     ! For non-zero cross-correlations they must also be the same for
     ! all kinds for forcing perturbations (eg. if precip and radiation
     ! perturbations are cross-correlated, their xcorr, ycorr and tcorr
     ! must be the same).
     
     real             :: xcorr  ! correlation length along latitudes   [deg]
     real             :: ycorr  ! correlation length along longitudes  [deg]
     real             :: tcorr  ! temporal correlation length          [s]
     
  end type forcepert_param_type
  
  ! --------------------------------------------------------------------
  
contains  
  
  subroutine allocate_forcepert_param(N_forcepert, N_x, N_y, fpp)
    
    implicit none
    
    integer, intent(in) :: N_forcepert, N_x, N_y
    
    type(forcepert_param_type), dimension(:), pointer :: fpp
    
    ! local variables
    
    integer :: k
    
    ! --------------------------------------------------------
    
    nullify(fpp)
    !nullify(fpp%mean)
    !nullify(fpp%std)
    !nullify(fpp%ccorr)
    
    allocate(fpp(N_forcepert))
    
    do k=1,N_forcepert
       allocate(fpp(k)%mean(N_x,N_y))
       allocate(fpp(k)%std(N_x,N_y))
       allocate(fpp(k)%ccorr(N_forcepert,N_x,N_y))
    end do
    
  end subroutine allocate_forcepert_param
  
  ! ------------------------------------------------------------------
  
end module forcepert_types

! **********************************************************************
! **********************************************************************

#if 0

program test
  
  use forcepert_types
  
  implicit none
  
  integer :: i, j, k, l, Nx=4, Ny=2, Nf=3
  
  type(forcepert_param_type), dimension(:), pointer :: fpp
  
  character(40) :: tmpstr

  ! --------------------------------------------------------------------
  
  nullify(fpp)

  call allocate_forcepert_param(Nf, Nx, Ny, fpp)
  
  do k=1,Nf
     
     write (tmpstr,'(i4.4)') k
     
     tmpstr = 'descr' // trim(tmpstr)
     
     fpp(k)%descr = tmpstr
     
     do i=1,Nx
        do j=1,Ny
           
           fpp(k)%mean(i,j) = k**2
           
           do l=1,Nf
              fpp(k)%ccorr(l,i,j) = real(i*j)/real(k*l)
           end do
           
        end do
     end do
     
  end do
  
  do k=1,Nf
     
     write (*,*) fpp(k)%descr
     do i=1,Nx
        write (*,*) fpp(k)%mean(i,:)
     end do
     write (*,*) 'ccorr', k
     do i=1,Nx
        do j=1,Ny
           write (*,*) (fpp(k)%ccorr(l,i,j), l=1,Nf)
        end do
     end do
  
  end do
     
  
end program test

#endif

! ================== EOF ====================================================
