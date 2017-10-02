! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_LocalArrayGet.F90"
!==============================================================================
!
! ESMF LocalArrayCreate module
module ESMF_LocalArrayGetMod
!
!==============================================================================
!
! This file contains the LocalArray class definition and all LocalArray
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_LocalArrayGetMod - Manage data uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LocalArray} class and
! associated functions and subroutines.
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed. To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!EOPI
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod ! ESMF utility types
  use ESMF_InitMacrosMod ! ESMF initializer macros
  use ESMF_BaseMod ! ESMF base class
  use ESMF_LogErrMod ! ESMF error handling
  use ESMF_ArraySpecMod
  ! class sub modules
  use ESMF_LocalArrayWrapperTypeMod ! contains the LAWrapper derived type
  use ESMF_LocalArrayCreateMod ! contains the ESMF_LocalArray derived type
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
character(*), parameter, private :: version = &
  '$Id$'
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_LocalArrayGet
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrayGet -- Get LocalArray internal information
! !INTERFACE:
  interface ESMF_LocalArrayGet
! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_LocalArrayGetDefault
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_LocalArrayGetData1DI1 
 module procedure ESMF_LocalArrayGetData2DI1 
 module procedure ESMF_LocalArrayGetData3DI1 
 module procedure ESMF_LocalArrayGetData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI1 
 module procedure ESMF_LocalArrayGetData6DI1 
 module procedure ESMF_LocalArrayGetData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_LocalArrayGetData1DI2 
 module procedure ESMF_LocalArrayGetData2DI2 
 module procedure ESMF_LocalArrayGetData3DI2 
 module procedure ESMF_LocalArrayGetData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI2 
 module procedure ESMF_LocalArrayGetData6DI2 
 module procedure ESMF_LocalArrayGetData7DI2 
#endif 
#endif 
#endif 
 module procedure ESMF_LocalArrayGetData1DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData1DI8 
#endif 
 module procedure ESMF_LocalArrayGetData1DR4 
 module procedure ESMF_LocalArrayGetData1DR8 
 module procedure ESMF_LocalArrayGetData2DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData2DI8 
#endif 
 module procedure ESMF_LocalArrayGetData2DR4 
 module procedure ESMF_LocalArrayGetData2DR8 
 module procedure ESMF_LocalArrayGetData3DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData3DI8 
#endif 
 module procedure ESMF_LocalArrayGetData3DR4 
 module procedure ESMF_LocalArrayGetData3DR8 
 module procedure ESMF_LocalArrayGetData4DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData4DI8 
#endif 
 module procedure ESMF_LocalArrayGetData4DR4 
 module procedure ESMF_LocalArrayGetData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData5DI8 
#endif 
 module procedure ESMF_LocalArrayGetData5DR4 
 module procedure ESMF_LocalArrayGetData5DR8 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrayGetData6DI4 
 module procedure ESMF_LocalArrayGetData6DI8 
 module procedure ESMF_LocalArrayGetData6DR4 
 module procedure ESMF_LocalArrayGetData6DR8 
 module procedure ESMF_LocalArrayGetData7DI4 
 module procedure ESMF_LocalArrayGetData7DI8 
 module procedure ESMF_LocalArrayGetData7DR4 
 module procedure ESMF_LocalArrayGetData7DR8 
#endif 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_LocalArrayGet} functions.
!
!EOPI
end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!==============================================================================
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Query for information from the array.
!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayGetDefault"
!BOP
! !IROUTINE: ESMF_LocalArrayGet - Get object-wide LocalArray information
!
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayGet()
  subroutine ESMF_LocalArrayGetDefault(localarray, keywordEnforcer, &
    typekind, rank, totalCount, totalLBound, totalUBound, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: localarray
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_TypeKind_Flag), intent(out), optional :: typekind
    integer, intent(out), optional :: rank
    integer, intent(out), optional :: totalCount(:)
    integer, intent(out), optional :: totalLBound(:)
    integer, intent(out), optional :: totalUBound(:)
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Returns information about the {\tt ESMF\_LocalArray}.
!
! The arguments are:
! \begin{description}
! \item[localarray]
! Queried {\tt ESMF\_LocalArray} object.
! \item[{[typekind]}]
! TypeKind of the LocalArray object.
! \item[{[rank]}]
! Rank of the LocalArray object.
! \item[{[totalCount]}]
! Count per dimension.
! \item[{[totalLBound]}]
! Lower bound per dimension.
! \item[{[totalUBound]}]
! Upper bound per dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    integer :: lrank ! Local use to get rank
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, localarray, rc)
    call c_ESMC_LocalArrayGetRank(localarray, lrank, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(rank)) then
      rank = lrank
    endif
    if (present(typekind)) then
      call c_ESMC_LocalArrayGetTypeKind(localarray, typekind, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(totalCount)) then
      if (size(totalCount) < lrank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg="- size of totalCount argument is smaller than rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      call c_esmc_localarraygetcounts(localarray, totalCount, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(totalLBound)) then
      if (size(totalLBound) < lrank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg="- size of totalLBound argument is smaller than rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      call c_ESMC_LocalArrayGetLbounds(localarray, totalLBound, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(totalUBound)) then
      if (size(totalUBound) < lrank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
          msg="- size of totalUBound argument is smaller than rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      call c_ESMC_LocalArrayGetUbounds(localarray, totalUBound, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayGetDefault
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
!BOP 
! !IROUTINE: ESMF_LocalArrayGet - Get a Fortran array pointer from a LocalArray 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_LocalArrayGet() 
! subroutine ESMF_LocalArrayGetData<rank><type><kind>(localarray, farrayPtr, & 
! keywordEnforcer, datacopyflag, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray) :: localarray 
! <type> (ESMF_KIND_<kind>), pointer :: farrayPtr 
! type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
! type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
! integer, intent(out), optional :: rc 
! 
! !STATUS: 
! \begin{itemize} 
! \item\apiStatusCompatibleVersion{5.2.0r} 
! \end{itemize} 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the data buffer, or return a Fortran pointer 
! to a new copy of the data. 
! 
! The arguments are: 
! \begin{description} 
! \item[localarray] 
! The {\tt ESMF\_LocalArray} to get the value from. 
! \item[farrayPtr] 
! An unassociated or associated Fortran pointer correctly allocated.
! \item[{[datacopyflag]}] 
! An optional copy flag which can be specified. 
! Can either make a new copy of the data or reference existing data. 
! See section \ref{const:datacopyflag} for a list of possible values. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Di1(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Di1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Di1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Di1 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Di1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Di1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Di2(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Di2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Di2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Di2 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Di2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Di2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap1Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr1Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr1Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap2Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr2Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr2Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap3Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr3Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr3Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap4Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr4Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr4Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap5Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr5Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr5Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap6Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr6Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr6Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Di4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Di4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Di4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Di4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Di4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Di4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Di8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Di8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_i8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Di8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Di8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Di8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Di8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Dr4(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Dr4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Dr4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Dr4 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Dr4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Dr4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7Dr8(localarray, & 
 farrayPtr, keywordEnforcer, datacopyflag, rc) 
 
 type(ESMF_LocalArray) :: localarray 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind_Flag) :: typekind 
 
 type (ESMF_LAWrap7Dr8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check datacopyflag argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATACOPY_VALUE) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between localarray and farrayPtr 
 call ESMF_LocalArrayGet(localarray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_r8) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in localarray 
 call c_ESMC_LocalArrayGetFPtr(localarray, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLbounds(localarray, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(localarray, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! test if incoming pointer was associated 
 if (associated(farrayPtr)) then 
 if (size(farrayPtr) .ne. size(lp)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- farrayPtr was associated but of incorrect size", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 farrayPtr = wrap%ptr7Dr8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7Dr8 
 farrayPtr => lp 
 endif 
 else 
 farrayPtr => wrap%ptr7Dr8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7Dr8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_LocalArrayGetMod
