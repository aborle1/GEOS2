PROGRAM mk_RouteRestarts

  use routing_model,                    ONLY:     &
       SEARCH_DNST

  USE catch_constants, ONLY:          &
       N_All => N_Pfaf_Catchs, N_LND => N_Pfaf_LandCatchs

  use MAPL_IOMod

  implicit none

  character*256 :: Usage="mk_RouteRestarts RestartTime"
  character*256 :: arg, inFile
  character*10  :: RestartTime
  CHARACTER*2   :: MM
  integer       :: I,N, STATUS, NCFID,Pfaf_land,K, RC, nVars
  real          :: VDUM
  real, dimension (:), allocatable    :: AREACAT,LENGSC, WSTREAM,WRIVER, tmp_var
  integer, dimension (:), allocatable :: DNST,  DNST2, Pfaf_all
  type(MAPL_NCIO)                     :: InNCIO, OutNCIO

  INCLUDE 'netcdf.inc'

  I = iargc()
  
  if( I /=1 ) then
     print *, "Wrong Number of arguments: ", i
     print *, trim(Usage)
     stop
  end if

  call getarg(1,arg)

  read(arg,'(a)') RestartTime
  MM = RestartTime (5: 6)

  inFile = '/discover/nobackup/rreichle/l_data/LandRestarts_for_Regridding/route/' &
               //'route_internal_rst.YYYY'//MM//'01'

  print *,'Copying restarts from ',trim(inFile)

  allocate (AREACAT (1:N_All))
  allocate (LENGSC  (1:N_All))
  allocate (DNST    (1:N_All))
  allocate (WSTREAM (1:N_All))
  allocate (WRIVER  (1:N_All))
  allocate (tmp_var (1:N_All))

  STATUS = NF_OPEN   (trim(inFile),NF_NOWRITE,NCFID)
  STATUS = NF_GET_VARA_REAL(NCFID,VarID(NCFID,'AREACAT'   ), (/1/), (/N_All/),AREACAT )
  STATUS = NF_GET_VARA_REAL(NCFID,VarID(NCFID,'LENGSC'    ), (/1/), (/N_All/),LENGSC  )
  STATUS = NF_GET_VARA_REAL(NCFID,VarID(NCFID,'DNSTR'     ), (/1/), (/N_All/),tmp_var )
  STATUS = NF_GET_VARA_REAL(NCFID,VarID(NCFID,'WSTREAM'   ), (/1/), (/N_All/),WSTREAM )
  STATUS = NF_GET_VARA_REAL(NCFID,VarID(NCFID,'WRIVER'    ), (/1/), (/N_All/),WRIVER  )
  
  STATUS   = NF_CLOSE (NCFID) 

  DNST    = NINT (tmp_var(:))
  
  ! Reset DNST to by pass submerged catchments: 
  ! Out of 291284 (N_Pfaf_Catchs in catch_constants.f90) only 290188 (N_Pfaf_LandCatchs in catch_constants.f90)
  ! are overland Pfafstetetter catchments. The rest is submerged under lakes or large river segments.
  ! Here, we check if the assigned downstream catchment is submerged and if so we bypass the submerged catchments(s)
  ! and link to the first available over land catchment downstream of the catchment in question.

  allocate   (DNST2 (1:N_All), Pfaf_all (1:N_All))

  Pfaf_all(:) = -9999
  DNST2       = -1 

  ! Read land only catchment information
  
  OPEN (10, FILE = '/discover/nobackup/rreichle/l_data/LandRestarts_for_Regridding/route/' &
       //'Pfafstetter.til', FORM='FORMATTED', STATUS = 'OLD', ACTION = 'READ')
  
  DO N = 1,5            
     READ (10,'(A)')            
  END DO
  
  DO N = 1, N_LND
     READ (10,*)K,VDUM,VDUM, VDUM, Pfaf_land, K
     Pfaf_all (Pfaf_land) = Pfaf_land
  END DO
  
  close (10, status = 'keep')

  DO N = 1, N_All
          
     if(Pfaf_all(N) >= 1) call SEARCH_DNST (N, N_All, DNST, Pfaf_all, DNST2(N))

  END DO

  ! Now write route_internal_rst

  InNCIO = MAPL_NCIOOpen('/discover/nobackup/rreichle/l_data/LandRestarts_for_Regridding/route/' &
       //'route_internal_rst.dummy', rc=rc) 
  
  call MAPL_NCIOGetDimSizes( InNCIO,nVars=nVars)
  call MAPL_NCIOChangeRes  ( InNCIO,OutNCIO,tileSize=N_All,rc=rc)    
          
  call MAPL_NCIOSet        (OutNCIO,filename='route_internal_rst' )
  call MAPL_NCIOCreateFile (OutNCIO)   
          
  call MAPL_VarWrite (OutNCIO,'AREACAT',AREACAT )    
  call MAPL_VarWrite (OutNCIO,'LENGSC', LENGSC  )    
  call MAPL_VarWrite (OutNCIO,'DNSTR',  REAL(DNST2))
  call MAPL_VarWrite (OutNCIO,'WSTREAM',WSTREAM )    
  call MAPL_VarWrite (OutNCIO,'WRIVER', WRIVER  )
    
  call MAPL_NCIOClose ( InNCIO)
  call MAPL_NCIOClose (OutNCIO)    

  deallocate (AREACAT,LENGSC, WSTREAM,WRIVER, tmp_var)
  deallocate (DNST, DNST2, Pfaf_all)

contains
  
  ! ----------------------------------------------------------------------
  
  integer function VarID (NCFID, VNAME) 
    
    integer, intent (in)      :: NCFID
    character(*), intent (in) :: VNAME
    integer                   :: status
    
    STATUS = NF_INQ_VARID (NCFID, trim(VNAME) ,VarID)
    IF (STATUS .NE. NF_NOERR) &
         CALL HANDLE_ERR(STATUS, trim(VNAME))  
    
  end function VarID
  
  ! -----------------------------------------------------------------------
  
  SUBROUTINE HANDLE_ERR(STATUS, Line)
    
    INTEGER,      INTENT (IN) :: STATUS
    CHARACTER(*), INTENT (IN) :: Line
    
    IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, trim(Line),': ',NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF
    
  END SUBROUTINE HANDLE_ERR
  
END PROGRAM mk_RouteRestarts
