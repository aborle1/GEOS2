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
#define ESMF_FILENAME "ESMF_LocalArrayCreate.F90"
!==============================================================================
!
! ESMF LocalArrayCreate module
module ESMF_LocalArrayCreateMod
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
! !MODULE: ESMF_LocalArrayCreateMod - Manage data uniformly between Fortran and C++
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
  use ESMF_IOUtilMod
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayWrapperTypeMod ! contains the LAWrapper derived type
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
  ! ESMF_DataCopy_Flag
  ! Indicates whether a data array should be copied or referenced.
  ! This matches an enum on the C++ side and the values must match.
  ! Update ../include/ESMCI_LocalArray.h if you change these values.
  type ESMF_DataCopy_Flag
  sequence
  private
    integer :: datacopyflag
  end type
  type(ESMF_DataCopy_Flag), parameter :: &
    ESMF_DATACOPY_VALUE = ESMF_DataCopy_Flag(1), &
    ESMF_DATACOPY_REFERENCE = ESMF_DataCopy_Flag(2), &
    ESMF_DATA_DEFER = ESMF_DataCopy_Flag(3), &
    ESMF_DATA_SPACE = ESMF_DataCopy_Flag(4)
!------------------------------------------------------------------------------
  ! ESMF_LocalArrayOrigin
  ! Private flag which indicates the create was initiated on the Fortran side.
  ! This matches an enum on the C++ side and the values must match.
  ! Update ../include/ESMCI_LocalArray.h if you change these values.
  type, public :: ESMF_LocalArrayOrigin
  sequence
  private
    integer :: origin
  end type
  type(ESMF_LocalArrayOrigin), parameter :: &
    ESMF_FROM_FORTRAN = ESMF_LocalArrayOrigin(1), &
    ESMF_FROM_CPLUSPLUS = ESMF_LocalArrayOrigin(2)
!------------------------------------------------------------------------------
  ! ESMF_LocalArray
  ! LocalArray data type. All information is kept on the C++ side inside
  ! the class structure.
  type ESMF_LocalArray
  sequence
  !private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_DataCopy_Flag, ESMF_DATACOPY_VALUE, ESMF_DATACOPY_REFERENCE, ESMF_DATA_SPACE
  public ESMF_LocalArray
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public operator(==)
  public operator(/=)
  public ESMF_LocalArrayCreate
  public ESMF_LocalArrayDestroy
  public ESMF_LocalArrConstrF90Ptr
  public ESMF_LocalArrayF90Deallocate
  public ESMF_LocalArrayCopyF90Ptr
  public ESMF_LocalArrayAdjust
  public ESMF_LocalArrayValidate
  public ESMF_LocalArrayPrint
  public ESMF_LocalArrayGetInit
  public ESMF_LocalArraySetInitCreated
  public ESMF_LocalArrayGetThis
  public ESMF_LocalArraySetThis
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_LocalArrayCreate -- Generic interface to create an LocalArray
! !INTERFACE:
  interface ESMF_LocalArrayCreate
! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_LocalArrayCreateByTKR ! specify explicit TKR
    module procedure ESMF_LocalArrayCreateBySpec ! specify ArraySpec
    module procedure ESMF_LocalArrayCreateCopy ! create a copy
    ! Plus interfaces for each T/K/R expanded by macro.
!EOPI
    ! < interfaces for each T/K/R >
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_LocalArrCreateByPtr1DI1 
 module procedure ESMF_LocalArrCreateByPtr2DI1 
 module procedure ESMF_LocalArrCreateByPtr3DI1 
 module procedure ESMF_LocalArrCreateByPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI1 
 module procedure ESMF_LocalArrCreateByPtr6DI1 
 module procedure ESMF_LocalArrCreateByPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_LocalArrCreateByPtr1DI2 
 module procedure ESMF_LocalArrCreateByPtr2DI2 
 module procedure ESMF_LocalArrCreateByPtr3DI2 
 module procedure ESMF_LocalArrCreateByPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI2 
 module procedure ESMF_LocalArrCreateByPtr6DI2 
 module procedure ESMF_LocalArrCreateByPtr7DI2 
#endif 
#endif 
#endif 
 module procedure ESMF_LocalArrCreateByPtr1DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr1DI8 
#endif 
 module procedure ESMF_LocalArrCreateByPtr1DR4 
 module procedure ESMF_LocalArrCreateByPtr1DR8 
 module procedure ESMF_LocalArrCreateByPtr2DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr2DI8 
#endif 
 module procedure ESMF_LocalArrCreateByPtr2DR4 
 module procedure ESMF_LocalArrCreateByPtr2DR8 
 module procedure ESMF_LocalArrCreateByPtr3DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr3DI8 
#endif 
 module procedure ESMF_LocalArrCreateByPtr3DR4 
 module procedure ESMF_LocalArrCreateByPtr3DR8 
 module procedure ESMF_LocalArrCreateByPtr4DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr4DI8 
#endif 
 module procedure ESMF_LocalArrCreateByPtr4DR4 
 module procedure ESMF_LocalArrCreateByPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI4 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr5DI8 
#endif 
 module procedure ESMF_LocalArrCreateByPtr5DR4 
 module procedure ESMF_LocalArrCreateByPtr5DR8 
#ifndef PIO_TKR 
 module procedure ESMF_LocalArrCreateByPtr6DI4 
 module procedure ESMF_LocalArrCreateByPtr6DI8 
 module procedure ESMF_LocalArrCreateByPtr6DR4 
 module procedure ESMF_LocalArrCreateByPtr6DR8 
 module procedure ESMF_LocalArrCreateByPtr7DI4 
 module procedure ESMF_LocalArrCreateByPtr7DI8 
 module procedure ESMF_LocalArrCreateByPtr7DR4 
 module procedure ESMF_LocalArrCreateByPtr7DR8 
#endif 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!BOPI
! !DESCRIPTION:
! This interface provides a single (heavily overloaded) entry point for
! the various types of {\tt ESMF\_LocalArrayCreate} functions.
!
! There are 3 options for setting the contents of the {\tt ESMF\_LocalArray}
! at creation time:
! \begin{description}
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_LocalArray}.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller is responsible for deallocating the space;
! when the {\tt ESMF\_LocalArray} is destroyed it will not release the space.
! \end{description}
!
! There are 3 options for
! specifying the type/kind/rank of the {\tt ESMF\_LocalArray} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_LocalArray} are given explicitly
! by individual arguments to the create function.
! \item[ArraySpec]
! A previously created {\tt ESMF\_ArraySpec} object is given which
! describes the characteristics.
! \item[Fortran 90 Pointer]
! An associated or unassociated Fortran 90 array pointer is used to
! describe the array.
! (Only available from the Fortran interface.)
! \end{description}
!
! The concept of an ``empty'' {\tt ESMF\_LocalArray} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt ESMF\_LocalArray} Create calls.
!
!EOPI
end interface
!------------------------------------------------------------------------------
!===============================================================================
! LocalArrayOperator() interfaces
!===============================================================================
! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayAssignment(=) - LocalArray assignment
!
! !INTERFACE:
! interface assignment(=)
! localarray1 = localarray2
!
! !ARGUMENTS:
! type(ESMF_LocalArray) :: localarray1
! type(ESMF_LocalArray) :: localarray2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Assign localarray1 as an alias to the same ESMF LocalArray object in memory
! as localarray2. If localarray2 is invalid, then localarray1 will be equally invalid after
! the assignment.
!
! The arguments are:
! \begin{description}
! \item[localarray1]
! The {\tt ESMF\_LocalArray} object on the left hand side of the assignment.
! \item[localarray2]
! The {\tt ESMF\_LocalArray} object on the right hand side of the assignment.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayOperator(==) - LocalArray equality operator
!
! !INTERFACE:
  interface operator(==)
! if (localarray1 == localarray2) then ... endif
! OR
! result = (localarray1 == localarray2)
! !RETURN VALUE:
! logical :: result
!
! !ARGUMENTS:
! type(ESMF_LocalArray), intent(in) :: localarray1
! type(ESMF_LocalArray), intent(in) :: localarray2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Test whether localarray1 and localarray2 are valid aliases to the same ESMF
! LocalArray object in memory. For a more general comparison of two ESMF LocalArrays,
! going beyond the simple alias test, the ESMF\_LocalArrayMatch() function (not yet
! implemented) must be used.
!
! The arguments are:
! \begin{description}
! \item[localarray1]
! The {\tt ESMF\_LocalArray} object on the left hand side of the equality
! operation.
! \item[localarray2]
! The {\tt ESMF\_LocalArray} object on the right hand side of the equality
! operation.
! \end{description}
!
!EOP
    module procedure ESMF_LocalArrayEQ
    module procedure ESMF_cfeq
  end interface
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayOperator(/=) - LocalArray not equal operator
!
! !INTERFACE:
  interface operator(/=)
! if (localarray1 /= localarray2) then ... endif
! OR
! result = (localarray1 /= localarray2)
! !RETURN VALUE:
! logical :: result
!
! !ARGUMENTS:
! type(ESMF_LocalArray), intent(in) :: localarray1
! type(ESMF_LocalArray), intent(in) :: localarray2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Test whether localarray1 and localarray2 are {\it not} valid aliases to the
! same ESMF LocalArray object in memory. For a more general comparison of two ESMF
! LocalArrays, going beyond the simple alias test, the ESMF\_LocalArrayMatch() function
! (not yet implemented) must be used.
!
! The arguments are:
! \begin{description}
! \item[localarray1]
! The {\tt ESMF\_LocalArray} object on the left hand side of the non-equality
! operation.
! \item[localarray2]
! The {\tt ESMF\_LocalArray} object on the right hand side of the non-equality
! operation.
! \end{description}
!
!EOP
    module procedure ESMF_LocalArrayNE
    module procedure ESMF_cfne
  end interface
!------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayEQ()"
!BOPI
! !IROUTINE: ESMF_LocalArrayEQ - Compare two LocalArrays for equality
!
! !INTERFACE:
  function ESMF_LocalArrayEQ(localarray1, localarray2)
!
! !RETURN VALUE:
    logical :: ESMF_LocalArrayEQ
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: localarray1
    type(ESMF_LocalArray), intent(in) :: localarray2
! !DESCRIPTION:
! Test if both {\tt localarray1} and {\tt localarray2} alias the same ESMF LocalArray
! object.
!
!EOPI
!-------------------------------------------------------------------------------
    ESMF_INIT_TYPE lainit1, lainit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2
    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    ! TODO: Consider moving this logic to C++: use Base class? status?
    ! Or replicate logic for C interface also.
    ! check inputs
    lainit1 = ESMF_LocalArrayGetInit(localarray1)
    lainit2 = ESMF_LocalArrayGetInit(localarray2)
    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (lainit1 .eq. ESMF_INIT_CREATED .and. &
      lainit2 .eq. ESMF_INIT_CREATED) then
      ESMF_LocalArrayEQ = localarray1%this .eq. localarray2%this
    else
      ESMF_LocalArrayEQ = ESMF_FALSE
    endif
  end function ESMF_LocalArrayEQ
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayNE()"
!BOPI
! !IROUTINE: ESMF_LocalArrayNE - Compare two LocalArrays for non-equality
!
! !INTERFACE:
  function ESMF_LocalArrayNE(localarray1, localarray2)
!
! !RETURN VALUE:
    logical :: ESMF_LocalArrayNE
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: localarray1
    type(ESMF_LocalArray), intent(in) :: localarray2
! !DESCRIPTION:
! Test if both {\tt localarray1} and {\tt localarray2} alias the same ESMF LocalArray
! object.
!
!EOPI
!-------------------------------------------------------------------------------
    ESMF_INIT_TYPE lainit1, lainit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2
    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    ESMF_LocalArrayNE = .not.ESMF_LocalArrayEQ(localarray1, localarray2)
  end function ESMF_LocalArrayNE
!-------------------------------------------------------------------------------
! functions to compare two ESMF_DataCopy_Flags to see if they are the same or not
function ESMF_cfeq(cf1, cf2)
 logical ESMF_cfeq
 type(ESMF_DataCopy_Flag), intent(in) :: cf1, cf2
 ESMF_cfeq = (cf1%datacopyflag .eq. cf2%datacopyflag)
end function
function ESMF_cfne(cf1, cf2)
 logical ESMF_cfne
 type(ESMF_DataCopy_Flag), intent(in) :: cf1, cf2
 ESMF_cfne = (cf1%datacopyflag .ne. cf2%datacopyflag)
end function
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the LocalArray Create and Destroy methods.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateByTKR"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray by explicitly specifying typekind and rank arguments
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateByTKR(typekind, rank, keywordEnforcer, totalCount, &
    totalLBound, totalUBound, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByTKR
!
! !ARGUMENTS:
    type(ESMF_TypeKind_Flag), intent(in) :: typekind
    integer, intent(in) :: rank
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(in), optional :: totalCount(:)
    integer, intent(in), optional :: totalLBound(:)
    integer, intent(in), optional :: totalUBound(:)
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[typekind]
! Array typekind. See section \ref{const:typekind} for valid values.
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 7D.
! \item[{[totalCount]}]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank. The {\tt count} argument may
! be omitted if both {\tt totalLBound} and {\tt totalUBound} arguments are present.
! \item[{[totalLBound]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[totalUBound]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    type (ESMF_LocalArray) :: array ! new C++ LocalArray
    integer, dimension(ESMF_MAXDIM) :: cnts ! local totalCount
    integer, dimension(ESMF_MAXDIM) :: lb, ub ! local bounds
    integer:: i
    array%this = ESMF_NULL_POINTER
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check rank argument
    if (rank<1 .or. rank>ESMF_MAXDIM) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
        msg="- Unsupported rank", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif
    ! Check that enough info from totalCount, totalLBound and totalUBound is present
    if (.not.present(totalCount)) then
      if (.not.present(totalLBound).or..not.present(totalUBound)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, &
          msg="- totalLBound and totalUBound must be present when totalCount argument is not present", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif
    ! Check size of optional totalCount and bounds and fill the local variables
    if (present(totalLBound)) then
      if (size(totalLBound)<rank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="- totalLBound argument must be of size rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      lb(1:rank) = totalLBound(1:rank)
    endif
    if (present(totalUBound)) then
      if (size(totalUBound)<rank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="- totalUBound argument must be of size rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      ub(1:rank) = totalUBound(1:rank)
    endif
    if (present(totalCount)) then
      if (size(totalCount)<rank) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, &
          msg="- totalCount argument must be of size rank", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
      cnts(1:rank) = totalCount(1:rank)
    else
      cnts(1:rank) = ub(1:rank) - lb(1:rank) + 1
    endif
    if (.not.present(totalLBound).and..not.present(totalUBound)) then
      lb(1:rank) = 1
      ub(1:rank) = cnts(1:rank)
    else if (.not.present(totalLBound)) then
      lb(1:rank) = ub(1:rank) - cnts(1:rank) + 1
    else if (.not.present(totalUBound)) then
      ub(1:rank) = lb(1:rank) + cnts(1:rank) - 1
    endif
    ! Check that the local bounds and totalCount variables match
    do i=1, rank
      if (cnts(i).ne.(ub(i)-lb(i)+1)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, &
          msg="- totalCount and bounds mismatch detected", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    enddo
    ! Create an initial LocalArray object that must be completed below
    call c_ESMC_LocalArrayCreateNoData(array, rank, typekind, &
      ESMF_FROM_FORTRAN, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Complete the initial LocalArray object
    call ESMF_LocalArrConstrF90Ptr(array, cnts, typekind, rank, lb, ub, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set return value
    ESMF_LocalArrayCreateByTKR = array
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateByTKR)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateByTKR
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateBySpec"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray by specifying an ArraySpec
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateBySpec(arrayspec, keywordEnforcer, totalCount, &
    totalLBound, totalUBound, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateBySpec
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(in) :: arrayspec
    type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(in), optional :: totalCount(:)
    integer, intent(in), optional :: totalLBound(:)
    integer, intent(in), optional :: totalUBound(:)
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! ArraySpec object specifying typekind and rank.
! \item[{[totalCount]}]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank. The {\tt count} argument may
! be omitted if both {\tt totalLBound} and {\tt totalUBound} arguments are present.
! \item[{[totalLBound]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[totalUBound]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    type (ESMF_LocalArray) :: array ! new C++ LocalArray
    integer :: localrc ! local return code
    integer :: rank
    type(ESMF_TypeKind_Flag) :: typekind
    array%this = ESMF_NULL_POINTER
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, arrayspec, rc)
    call ESMF_ArraySpecGet(arrayspec, typekind=typekind, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Call the CreateByTKR function to make the array
    ESMF_LocalArrayCreateBySpec = ESMF_LocalArrayCreateByTKR(typekind, rank, &
      totalCount=totalCount, totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateBySpec)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateBySpec
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateCopy"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray from pre-existing LocalArray
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateCopy(localarray, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateCopy
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: localarray
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Perform a deep copy of an existing {\tt ESMF\_LocalArray} object. The return
! value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[localarray]
! Existing LocalArray to be copied.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    type(ESMF_LocalArray) :: localarrayOut ! opaque pointer to new C++ LocalArray
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Mark this LocalArray object as invalid
    localarrayOut%this = ESMF_NULL_POINTER
    ! Call into the C++ interface
    call c_ESMC_LocalArrayCreateCopy(localarray, localarrayOut, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set return value
    ESMF_LocalArrayCreateCopy = localarrayOut
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateCopy)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateCopy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! !IROUTINE: ESMF_LocalArrayCreate - Create a LocalArray from a Fortran pointer (associated or unassociated) 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_LocalArrayCreate() 
! function ESMF_LocalArrCreateByPtr<rank><type><kind>(farrayPtr, & 
! keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! <type> (ESMF_KIND_<kind>), pointer :: farrayPtr(<rank>) 
! type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
! type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
! integer, intent(in), optional :: totalCount(:) 
! integer, intent(in), optional :: totalLBound(:) 
! integer, intent(in), optional :: totalUBound(:) 
! integer, intent(out), optional :: rc 
! 
! !STATUS: 
! \begin{itemize} 
! \item\apiStatusCompatibleVersion{5.2.0r} 
! \end{itemize} 
! 
! !DESCRIPTION: 
! Creates an {\tt ESMF\_LocalArray} based on a Fortran array pointer. 
! Two cases must be distinguished. 
! 
! First, if {\tt farrayPtr} is associated 
! the optional {\tt datacopyflag} argument may be used to indicate whether the 
! associated data is to be copied or referenced. For associated {\tt farrayPtr} 
! the optional {\tt totalCount}, {\tt totalLBound} and {\tt totalUBound} arguments need 
! not be specified. However, all present arguments will be checked against 
! {\tt farrayPtr} for consistency. 
! 
! Second, if {\tt farrayPtr} is unassociated the optional argument {\tt datacopyflag} 
! must not be specified. However, in this case a complete set of totalCount and 
! bounds information must be provided. Any combination of present {\tt totalCount} 
! {\tt totalLBound} and {\tt totalUBound} arguments that provides a complete 
! specification is valid. All input information will be checked for 
! consistency. 
! 
! The arguments are: 
! \begin{description} 
! \item[farrayPtr] 
! A Fortran array pointer (associated or unassociated). 
! \item[{[datacopyflag]}] 
! Indicate copy vs. reference behavior in case of associated {\tt farrayPtr}. 
! This argument must {\em not} be present for unassociated {\tt farrayPtr}. 
! Default to {\tt ESMF\_DATACOPY\_REFERENCE}, makes the {\tt ESMF\_LocalArray} 
! reference the associated data array. If set to {\tt ESMF\_DATACOPY\_VALUE} this 
! routine allocates new memory and copies the data from the pointer into 
! the new LocalArray allocation. 
! \item[{[totalCount]}] 
! The number of items in each dimension of the array. This is a 1D 
! integer array the same length as the rank. The {\tt count} argument may 
! be omitted if both {\tt totalLBound} and {\tt totalUBound} arguments are present. 
! \item[{[totalLBound]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[totalUBound]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Di1 
 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Di1 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Di1(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Di1 
 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_i1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Di1(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Di1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Di1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Di1 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Di2 
 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Di2 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Di2(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Di2 
 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_i2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Di2(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Di2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Di2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Di2 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Di4 
 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Di8 
 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Dr4 
 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1Dr8 
 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local totalCount 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = totalLBound(1:1) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = totalUBound(1:1) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<1) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 1 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = totalCount(1:1) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(totalLBound)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local totalCount 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = totalLBound(1:2) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = totalUBound(1:2) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<2) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 2 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = totalCount(1:2) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(totalLBound)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local totalCount 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = totalLBound(1:3) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = totalUBound(1:3) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<3) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 3 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = totalCount(1:3) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(totalLBound)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local totalCount 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = totalLBound(1:4) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = totalUBound(1:4) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<4) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 4 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = totalCount(1:4) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(totalLBound)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local totalCount 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = totalLBound(1:5) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = totalUBound(1:5) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<5) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 5 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = totalCount(1:5) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(totalLBound)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local totalCount 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = totalLBound(1:6) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = totalUBound(1:6) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<6) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 6 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = totalCount(1:6) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(totalLBound)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Di4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Di4 
 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_i4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Di4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Di4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Di4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Di8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Di8 
 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_i8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Di8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Di8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Di8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Dr4(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Dr4 
 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_r4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Dr4(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Dr4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Dr4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7Dr8(farrayPtr, & 
 keywordEnforcer, datacopyflag, totalCount, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7Dr8 
 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalCount 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_DataCopy_Flag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local totalCount 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if farrayPtr is associated and check consistency of arguments 
 if (associated(farrayPtr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(farrayPtr) 
 lb = lbound(farrayPtr) 
 ub = ubound(farrayPtr) 
 ! Set default for datacopyflag 
 if (present(datacopyflag)) then 
 copy = datacopyflag 
 else 
 copy = ESMF_DATACOPY_REFERENCE 
 endif 
 else 
 if (present(datacopyflag)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- datacopyflag argument is only allowed with associated farrayPtr argument", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from totalCount, totalLBound and totalUBound is present 
 if (.not.associated(farrayPtr) .and. .not.present(totalCount)) then 
 if (.not.present(totalLBound) .or. .not.present(totalUBound)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_OPT, & 
 msg="- totalLBound and totalUBound must be present when totalCount argument is not present", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional totalCount and bounds and fill the local variables 
 if (present(totalLBound)) then 
 if (size(totalLBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalLBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (lb(i) .ne. totalLBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalLBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = totalLBound(1:7) 
 endif 
 endif 
 if (present(totalUBound)) then 
 if (size(totalUBound)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalUBound argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (ub(i) .ne. totalUBound(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalUBound are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = totalUBound(1:7) 
 endif 
 endif 
 if (present(totalCount)) then 
 if (size(totalCount)<7) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
 msg="- totalCount argument must be of size rank", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 if (associated(farrayPtr)) then 
 do i=1, 7 
 if (cnts(i) .ne. totalCount(i)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, & 
 msg="- provided totalCount are incompatible with associated farrayPtr", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = totalCount(1:7) 
 endif 
 else if (.not.associated(farrayPtr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(farrayPtr)) then 
 if (.not.present(totalLBound) .and. .not.present(totalUBound)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(totalLBound)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(totalUBound)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and totalCount variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_VALUE, & 
 msg="- totalCount and bounds mismatch detected", & 
 ESMF_CONTEXT, rcToReturn=rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_r8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7Dr8(array, cnts, farrayPtr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7Dr8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7Dr8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7Dr8 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayDestroy"
!BOP
! !IROUTINE: ESMF_LocalArrayDestroy - Release resources associated with a LocalArray
!
! !INTERFACE:
  subroutine ESMF_LocalArrayDestroy(localarray, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: localarray
 type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Destroys an {\tt ESMF\_LocalArray}, releasing all resources associated
! with the object.
!
! The arguments are:
! \begin{description}
! \item[localarray]
! Destroy contents of this {\tt ESMF\_LocalArray}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    type(ESMF_Logical) :: dealloc ! do we need to free space?
    type(ESMF_TypeKind_Flag) :: typekind
    integer :: rank
! To reduce the depth of crossings of the F90/C++ boundary we first
! query to see if we are responsible for deleting the data space. If so,
! first deallocate the space and then call the C++ code to release
! the object space. When it returns we are done and can return to the user.
! Otherwise we would need to make a nested call back into F90 from C++ to do
! the deallocate() during the object delete.
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, localarray, rc)
    ! TODO: document the current rule - if we do the allocate in
    ! the case of ESMF_DATACOPY_VALUE at create time then we delete the
    ! space. otherwise, the user needs to destroy the array
    ! (we will ignore the data) and call deallocate themselves.
    call c_ESMC_LocalArrayGetDealloc(localarray, dealloc, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (dealloc.eq.ESMF_TRUE) then
      call c_ESMC_LocalArrayGetRank(localarray, rank, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetTypeKind(localarray, typekind, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_LocalArrayF90Deallocate(localarray, typekind, rank, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArraySetDealloc(localarray, ESMF_FALSE, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Calling deallocate first means this will not return back to F90
    ! before returning for good.
    call c_ESMC_LocalArrayDestroy(localarray, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set init code
    ESMF_INIT_SET_DELETED(localarray)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayDestroy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr"
!BOPI
! !IROUTINE: ESMF_LocalArrConstrF90Ptr - Create and add F90 ptr to array
! !INTERFACE:
  subroutine ESMF_LocalArrConstrF90Ptr(array, totalCount, typekind, rank, totalLBound, &
    totalUBound, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    integer, dimension(:), intent(in) :: totalCount
    type(ESMF_TypeKind_Flag), intent(in) :: typekind
    integer, intent(in) :: rank
    integer, dimension(:), intent(in) :: totalLBound
    integer, dimension(:), intent(in) :: totalUBound
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and
! finish constructiong the LocalArray object.
!
! The arguments are:
! \begin{description}
! \item[array]
! Partially created {\tt ESMF\_LocalArray} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
! \item[totalCount]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[typekind]
! Array typetypekind.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[totalLBound]
! The lower index values per rank.
! \item[totalUBound]
! The upper index values per rank.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    integer :: localtk
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Cannot check init status of array argument here because
    ! the array object is only partially created at this point
    localtk = typekind%dkind
    ! Call a T/K/R specific interface in order to create the proper
    ! type of F90 pointer, allocate the space, set the values in the
    ! Local Array object, and return. (The routine this code is calling is
    ! generated by macro.)
    !! calling routines generated from macros by the preprocessor
    select case (localtk)
#ifndef ESMF_NO_INTEGER_1_BYTE
      case (ESMF_TYPEKIND_I1%dkind)
        select case (rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DI1(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
      case (ESMF_TYPEKIND_I2%dkind)
        select case(rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DI2(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
#endif
      case (ESMF_TYPEKIND_I4%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DI4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_I8%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DI8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_R4%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DR4(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_R8%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrConstrF90Ptr1DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (2)
            call ESMF_LocalArrConstrF90Ptr2DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (3)
            call ESMF_LocalArrConstrF90Ptr3DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (4)
            call ESMF_LocalArrConstrF90Ptr4DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrConstrF90Ptr5DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
   case (6)
            call ESMF_LocalArrConstrF90Ptr6DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
          case (7)
            call ESMF_LocalArrConstrF90Ptr7DR8(array, totalCount, &
              totalLBound=totalLBound, totalUBound=totalUBound, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case default
        if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
          msg="Unsupported typekind", &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end select
    ! Set return code if caller specified it
    if (present(rc)) rc = localrc
  end subroutine ESMF_LocalArrConstrF90Ptr
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr<rank><type><kind> - Create a Fortran Ptr of the proper T/K/R 
! 
! !INTERFACE: 
! subroutine ESMF_LocalArrConstrF90Ptr<rank><type><kind>(array, totalCount, farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(inout) :: array 
! integer, dimension(:), intent(in) :: totalCount 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer, optional :: farrayPtr 
! type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
! integer, dimension(:), intent(in), optional :: totalLBound 
! integer, dimension(:), intent(in), optional :: totalUBound 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on totalCount, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a datacopyflag flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! \item[totalCount] 
! An integer array of totalCount. Must be the same length as the rank. 
! \item[{[farrayPtr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt datacopyflag} is specified. 
! \item[{[datacopyflag]}] 
! An optional copy flag which can be specified if a Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[totalLBound]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[totalUBound]}] 
! An integer array of upper index values. Must be same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
!---------------------------------------------------------------------------- 
 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Di1 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Di1(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Di1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Di1 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Di2 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Di2(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Di2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Di2 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap1Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap2Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap3Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap4Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap5Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap6Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Di4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Di4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Di8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Di8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Dr4(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Dr4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7Dr8(array, totalCount, & 
 farrayPtr, datacopyflag, totalLBound, totalUBound, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer, optional :: farrayPtr 
 type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
 integer, dimension(:), intent(in), optional :: totalLBound 
 integer, dimension(:), intent(in), optional :: totalUBound 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_LAWrap7Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(totalCount)) = totalCount 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(farrayPtr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (present(datacopyflag)) then 
 if (datacopyflag .eq. ESMF_DATA_SPACE) then 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (datacopyflag .eq. ESMF_DATACOPY_VALUE) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATACOPY_REFERENCE 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 else 
 newp => farrayPtr ! ptr alias, important this be => 
 lb(1:size(totalCount)) = lbound(farrayPtr) 
 ub(1:size(totalCount)) = ubound(farrayPtr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! totalLBound, if given, should be used 
 if (present(totalLBound)) then 
 lb(1:size(totalLBound)) = totalLBound 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(totalUBound)) then 
 ub(1:size(totalUBound)) = totalUBound 
 endif 
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="LocalArray data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (willcopy) then 
 newp = farrayPtr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the totalCount, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7Dr8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, totalCount, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7Dr8 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayF90Deallocate"
!BOPI
! !IROUTINE: ESMF_LocalArrayF90Deallocate - Deallocate an F90 pointer
!
! !INTERFACE:
  subroutine ESMF_LocalArrayF90Deallocate(array, typekind, rank, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    type(ESMF_TypeKind_Flag) :: typekind
    integer :: rank
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents for an {\tt ESMF\_LocalArray} created from
! the C++ interface. The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_LocalArray} object.
! \item[typekind]
! The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).
! \item[rank]
! The {\tt ESMF\_LocalArray} rank.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    integer :: localtk
    !! local variables, expanded by macro

#ifndef ESMF_NO_INTEGER_1_BYTE 
#ifndef PIO_TKR 
 type(ESMF_LAWrap1DI1) :: l1DI1 
 type(ESMF_LAWrap2DI1) :: l2DI1 
 type(ESMF_LAWrap3DI1) :: l3DI1 
 type(ESMF_LAWrap4DI1) :: l4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_LAWrap5DI1) :: l5DI1 
 type(ESMF_LAWrap6DI1) :: l6DI1 
 type(ESMF_LAWrap7DI1) :: l7DI1 
#endif 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
#ifndef PIO_TKR 
 type(ESMF_LAWrap1DI2) :: l1DI2 
 type(ESMF_LAWrap2DI2) :: l2DI2 
 type(ESMF_LAWrap3DI2) :: l3DI2 
 type(ESMF_LAWrap4DI2) :: l4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_LAWrap5DI2) :: l5DI2 
 type(ESMF_LAWrap6DI2) :: l6DI2 
 type(ESMF_LAWrap7DI2) :: l7DI2 
#endif 
#endif 
#endif 
 type(ESMF_LAWrap1DI4) :: l1DI4 
#ifndef PIO_TKR 
 type(ESMF_LAWrap1DI8) :: l1DI8 
#endif 
 type(ESMF_LAWrap1DR4) :: l1DR4 
 type(ESMF_LAWrap1DR8) :: l1DR8 
 
 type(ESMF_LAWrap2DI4) :: l2DI4 
#ifndef PIO_TKR 
 type(ESMF_LAWrap2DI8) :: l2DI8 
#endif 
 type(ESMF_LAWrap2DR4) :: l2DR4 
 type(ESMF_LAWrap2DR8) :: l2DR8 
 
 type(ESMF_LAWrap3DI4) :: l3DI4 
#ifndef PIO_TKR 
 type(ESMF_LAWrap3DI8) :: l3DI8 
#endif 
 type(ESMF_LAWrap3DR4) :: l3DR4 
 type(ESMF_LAWrap3DR8) :: l3DR8 
 
 type(ESMF_LAWrap4DI4) :: l4DI4 
#ifndef PIO_TKR 
 type(ESMF_LAWrap4DI8) :: l4DI8 
#endif 
 type(ESMF_LAWrap4DR4) :: l4DR4 
 type(ESMF_LAWrap4DR8) :: l4DR8 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 
 type(ESMF_LAWrap5DI4) :: l5DI4 
#ifndef ESMF_NO_INTEGER_I8 
 type(ESMF_LAWrap5DI8) :: l5DI8 
#endif 
 type(ESMF_LAWrap5DR4) :: l5DR4 
 type(ESMF_LAWrap5DR8) :: l5DR8 
 
#ifndef PIO_TKR 
 
 type(ESMF_LAWrap6DI4) :: l6DI4 
 type(ESMF_LAWrap6DI8) :: l6DI8 
 type(ESMF_LAWrap6DR4) :: l6DR4 
 type(ESMF_LAWrap6DR8) :: l6DR8 
 
 type(ESMF_LAWrap7DI4) :: l7DI4 
 type(ESMF_LAWrap7DI8) :: l7DI8 
 type(ESMF_LAWrap7DR4) :: l7DR4 
 type(ESMF_LAWrap7DR8) :: l7DR8 
 
#endif 
 
#endif 
 
! < end macro - do not edit directly > 
 

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    localtk = typekind
    !! calling routines generated from macros by the preprocessor
        select case (localtk)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DI1%ptr1DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DI1%ptr1DI1) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DI1%ptr2DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DI1%ptr2DI1) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DI1%ptr3DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DI1%ptr3DI1) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DI1%ptr4DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DI1%ptr4DI1) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DI1%ptr5DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DI1%ptr5DI1) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DI1%ptr6DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DI1%ptr6DI1) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DI1, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DI1%ptr7DI1, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DI1%ptr7DI1) 
 

#endif
              case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                  msg="Unsupported rank", &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
          case (ESMF_TYPEKIND_I2%dkind)
     select case(rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DI2%ptr1DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DI2%ptr1DI2) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DI2%ptr2DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DI2%ptr2DI2) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DI2%ptr3DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DI2%ptr3DI2) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DI2%ptr4DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DI2%ptr4DI2) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DI2%ptr5DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DI2%ptr5DI2) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DI2%ptr6DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DI2%ptr6DI2) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DI2, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DI2%ptr7DI2, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DI2%ptr7DI2) 
 

#endif
              case default
               if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                 msg="Unsupported rank", &
                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
#endif
          case (ESMF_TYPEKIND_I4%dkind)
     select case(rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DI4%ptr1DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DI4%ptr1DI4) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DI4%ptr2DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DI4%ptr2DI4) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DI4%ptr3DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DI4%ptr3DI4) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DI4%ptr4DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DI4%ptr4DI4) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DI4%ptr5DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DI4%ptr5DI4) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DI4%ptr6DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DI4%ptr6DI4) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DI4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DI4%ptr7DI4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DI4%ptr7DI4) 
 

#endif
              case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                  msg="Unsupported rank", &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DI8%ptr1DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DI8%ptr1DI8) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DI8%ptr2DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DI8%ptr2DI8) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DI8%ptr3DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DI8%ptr3DI8) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DI8%ptr4DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DI8%ptr4DI8) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DI8%ptr5DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DI8%ptr5DI8) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DI8%ptr6DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DI8%ptr6DI8) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DI8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DI8%ptr7DI8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DI8%ptr7DI8) 
 

#endif
              case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                  msg="Unsupported rank", &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DR4%ptr1DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DR4%ptr1DR4) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DR4%ptr2DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DR4%ptr2DR4) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DR4%ptr3DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DR4%ptr3DR4) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DR4%ptr4DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DR4%ptr4DR4) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DR4%ptr5DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DR4%ptr5DR4) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DR4%ptr6DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DR4%ptr6DR4) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DR4, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DR4%ptr7DR4, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DR4%ptr7DR4) 
 

#endif
              case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                  msg="Unsupported rank", &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
call c_ESMC_LocalArrayGetFPtr(array, l1DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l1DR8%ptr1DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l1DR8%ptr1DR8) 
 

       case (2)
call c_ESMC_LocalArrayGetFPtr(array, l2DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l2DR8%ptr2DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l2DR8%ptr2DR8) 
 

       case (3)
call c_ESMC_LocalArrayGetFPtr(array, l3DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l3DR8%ptr3DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l3DR8%ptr3DR8) 
 

       case (4)
call c_ESMC_LocalArrayGetFPtr(array, l4DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l4DR8%ptr4DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l4DR8%ptr4DR8) 
 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
call c_ESMC_LocalArrayGetFPtr(array, l5DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l5DR8%ptr5DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l5DR8%ptr5DR8) 
 

       case (6)
call c_ESMC_LocalArrayGetFPtr(array, l6DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l6DR8%ptr6DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l6DR8%ptr6DR8) 
 

              case (7)
call c_ESMC_LocalArrayGetFPtr(array, l7DR8, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 deallocate(l7DR8%ptr7DR8, stat=localrc) 
 if (ESMF_LogFoundDeallocError(localrc, msg="LocalArray deallocation", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 nullify(l7DR8%ptr7DR8) 
 

#endif
              case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                  msg="Unsupported rank", &
                  ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported kind", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayF90Deallocate
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCopyF90Ptr"
!BOPI
! !IROUTINE: ESMF_LocalArrayCopyF90Ptr - Copy F90 pointer
! !INTERFACE:
  subroutine ESMF_LocalArrayCopyF90Ptr(localarrayIn, localarrayOut, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: localarrayIn
    type(ESMF_LocalArray), intent(inout) :: localarrayOut
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Copy F90 pointer contents from {\tt arrayIn} to (\tt arrayOut}.
!
! The arguments are:
! \begin{description}
! \item[arrayIn]
! Existing {\tt ESMF\_LocalArray} object.
! \item[arrayOut]
! Existing {\tt ESMF\_LocalArray} object without alloc for data
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    integer :: localtk
    integer :: rank
    type(ESMF_TypeKind_Flag) :: typekind
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP_SHORT(ESMF_LocalArrayGetInit, localarrayIn, rc)
    ! Identify localarrayIn TKR
    call c_ESMC_LocalArrayGetRank(localarrayIn, rank, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call c_ESMC_LocalArrayGetTypeKind(localarrayIn, typekind, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    !TODO: check TKR consistency against localarrayOut
    ! Call a T/K/R specific interface in order to create the proper
    ! type of F90 pointer, allocate the space, set the values in the
    ! LocalArray object, and return. (The routine this code is calling is
    ! generated by macro.)
    localtk = typekind%dkind
    !! calling routines generated from macros by the preprocessor
    select case (localtk)
#ifndef ESMF_NO_INTEGER_1_BYTE
      case (ESMF_TYPEKIND_I1%dkind)
        select case (rank)
          case (1)
            call ESMF_LocalArrayCopy1DI1(localarrayIn, localarrayOut, rc=localrc)
          case (2)
            call ESMF_LocalArrayCopy2DI1(localarrayIn, localarrayOut, rc=localrc)
          case (3)
            call ESMF_LocalArrayCopy3DI1(localarrayIn, localarrayOut, rc=localrc)
          case (4)
            call ESMF_LocalArrayCopy4DI1(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DI1(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DI1(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DI1(localarrayIn, localarrayOut, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
      case (ESMF_TYPEKIND_I2%dkind)
        select case(rank)
   case (1)
            call ESMF_LocalArrayCopy1DI2(localarrayIn, localarrayOut, rc=localrc)
   case (2)
            call ESMF_LocalArrayCopy2DI2(localarrayIn, localarrayOut, rc=localrc)
   case (3)
            call ESMF_LocalArrayCopy3DI2(localarrayIn, localarrayOut, rc=localrc)
   case (4)
            call ESMF_LocalArrayCopy4DI2(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DI2(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DI2(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DI2(localarrayIn, localarrayOut, rc=localrc)
#endif
           case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
#endif
      case (ESMF_TYPEKIND_I4%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrayCopy1DI4(localarrayIn, localarrayOut, rc=localrc)
   case (2)
            call ESMF_LocalArrayCopy2DI4(localarrayIn, localarrayOut, rc=localrc)
   case (3)
            call ESMF_LocalArrayCopy3DI4(localarrayIn, localarrayOut, rc=localrc)
   case (4)
            call ESMF_LocalArrayCopy4DI4(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DI4(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DI4(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DI4(localarrayIn, localarrayOut, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_I8%dkind)
        select case(rank)
   case (1)
            call ESMF_LocalArrayCopy1DI8(localarrayIn, localarrayOut, rc=localrc)
   case (2)
            call ESMF_LocalArrayCopy2DI8(localarrayIn, localarrayOut, rc=localrc)
   case (3)
            call ESMF_LocalArrayCopy3DI8(localarrayIn, localarrayOut, rc=localrc)
   case (4)
            call ESMF_LocalArrayCopy4DI8(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DI8(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DI8(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DI8(localarrayIn, localarrayOut, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_R4%dkind)
 select case(rank)
   case (1)
            call ESMF_LocalArrayCopy1DR4(localarrayIn, localarrayOut, rc=localrc)
   case (2)
            call ESMF_LocalArrayCopy2DR4(localarrayIn, localarrayOut, rc=localrc)
   case (3)
            call ESMF_LocalArrayCopy3DR4(localarrayIn, localarrayOut, rc=localrc)
   case (4)
            call ESMF_LocalArrayCopy4DR4(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DR4(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DR4(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DR4(localarrayIn, localarrayOut, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case (ESMF_TYPEKIND_R8%dkind)
        select case(rank)
   case (1)
            call ESMF_LocalArrayCopy1DR8(localarrayIn, localarrayOut, rc=localrc)
   case (2)
            call ESMF_LocalArrayCopy2DR8(localarrayIn, localarrayOut, rc=localrc)
   case (3)
            call ESMF_LocalArrayCopy3DR8(localarrayIn, localarrayOut, rc=localrc)
   case (4)
            call ESMF_LocalArrayCopy4DR8(localarrayIn, localarrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
   case (5)
            call ESMF_LocalArrayCopy5DR8(localarrayIn, localarrayOut, rc=localrc)
   case (6)
            call ESMF_LocalArrayCopy6DR8(localarrayIn, localarrayOut, rc=localrc)
          case (7)
            call ESMF_LocalArrayCopy7DR8(localarrayIn, localarrayOut, rc=localrc)
#endif
          case default
            if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
              msg="Unsupported rank", &
              ESMF_CONTEXT, rcToReturn=rc)) return
        end select
      case default
        if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
          msg="Unsupported typekind", &
          ESMF_CONTEXT, rcToReturn=rc)) return
    end select
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set init code
    ESMF_INIT_SET_CREATED(localarrayOut)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayCopyF90Ptr
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOPI 
! !INTERFACE: 
! subroutine ESMF_LocalArrayCopy<rank><type><kind>(arrayIn, arrayOut, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(in) :: arrayIn 
! type(ESMF_LocalArray), intent(inout) :: arrayOut 
! type(ESMF_DataCopy_Flag), intent(in), optional :: datacopyflag 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the data buffer, or return a Fortran pointer 
! to a new copy of the data. 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! The arguments are: 
! \begin{description} 
! \item[arrayIn] 
! The {\tt ESMF\_LocalArray} to copy. 
! \item[arrayOut] 
! The copied array. 
! \item[{[datacopyflag]}] 
! An optional copy flag which can be specified. 
! Can either make a new copy of the data or reference existing data. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
!---------------------------------------------------------------------------- 
 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Di1 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Di1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Di1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Di1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Di1 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Di1 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Di2 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Di2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Di2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Di2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Di2 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Di2 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap1Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap2Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap3Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap4Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap5Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap6Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Di4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Di4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Di4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Di4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Di8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Di8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Di8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Di8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Dr4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Dr4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Dr4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Dr4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Dr4 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7Dr8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Dr8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_LAWrap7Dr8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetFPtr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_LocalArrayGetLbounds(arrayOut, lb, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 call c_ESMC_LocalArrayGetUbounds(arrayOut, ub, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! Macro lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) 
 ! is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogFoundAllocError(localrc, & 
 msg="local data space", & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7Dr8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7Dr8 => lp 
 call c_ESMC_LocalArraySetFPtr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 if (size(lp) .ne. 0) then 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_NULL_POINTER, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7Dr8 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayAdjust"
!BOPI
! !IROUTINE: ESMF_LocalArrayAdjust - Adjust bounds of Fortran array member
!
! !INTERFACE:
  subroutine ESMF_LocalArrayAdjust(array, totalCount, typekind, rank, totalLBound, &
    totalUBound, rc)
!
! !ARGUMENTS:
   type(ESMF_LocalArray), intent(inout) :: array
   integer, dimension(:), intent(in) :: totalCount
   type(ESMF_TypeKind_Flag), intent(in) :: typekind
   integer, intent(in) :: rank
   integer, dimension(:), intent(in) :: totalLBound
   integer, dimension(:), intent(in) :: totalUBound
   integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Adjust bounds of Fortran array member in {\tt ESMF\_LocalArray} object.
!
!EOPI
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    integer :: localtk
    localrc = ESMF_RC_NOT_IMPL
    ! Cannot check init status of array argument here because
    ! the array object is only partially created at this point
    localtk = typekind%dkind
    ! Call a T/K/R specific interface
        select case (localtk)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI1(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
          case (ESMF_TYPEKIND_I2%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI2(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
#endif
          case (ESMF_TYPEKIND_I4%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DR4(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DR8(array, totalCount, &
                                    lb=totalLBound, ub=totalUBound, rc=localrc)
#endif
              case default
                    if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported rank", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
            end select
          case default
                if (ESMF_LogFoundError(ESMF_RC_ARG_BAD, &
                                msg="Unsupported kind", &
                                 ESMF_CONTEXT, rcToReturn=rc)) return
        end select
    ! check localrc for errors
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayAdjust
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOPI 
! !IROUTINE: ESMF_LocalArrayAdjust<rank><type><kind> - Adjust the bounds of the Fortran pointer member according to the proper T/K/R 
! 
! !INTERFACE: 
! recursive subroutine ESMF_LocalArrayAdjust<rank><type><kind>(array,&
! totalCount, lb, ub, fshape, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(inout) :: array 
! integer, dimension(:), intent(in) :: totalCount 
! integer, dimension(:), intent(in), optional :: lb 
! integer, dimension(:), intent(in), optional :: ub 
! mname (ESMF_KIND_mtypekind), dimension(mdim), target, optional ::&
! fshape(mrng) 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Each LocalArray object internally keeps a reference to an F90 array pointer. 
! This call modifies the meta-data associated with this F90 array pointer 
! by passing the F90 array pointer into a F90 subroutine with an explicit shape 
! dummy argument. On this interface the bounds meta data for the dummy argument 
! is not those of the actual argument but is reset to the bounds specified 
! on the subroutine interface. Using macros the bounds on the callee side are 
! set to match those of the LocalArray object meta data. Finally the internal 
! F90 array pointer is reset to reflect the desired bounds in the F90 dope 
! vector. The risk of data copy on this interface should be minimal because 
! the shape is not changed and the dummy argument has the target attribute. 
!EOPI 
!---------------------------------------------------------------------------- 
 
 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Di1 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Di1 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Di1(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i1), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Di1 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Di1 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Di1(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Di1 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Di2 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Di2 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Di2(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i2), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Di2 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Di2 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Di2(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Di2 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap1Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr1Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust1Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap2Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr2Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust2Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap3Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr3Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust3Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap4Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr4Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust4Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Di4 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Di8 
 
!---------------------------------------------------------------------------- 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap5Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr5Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust5Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5Dr8 
 
!---------------------------------------------------------------------------- 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap6Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr6Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust6Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6Dr8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Di4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Di4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Di4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Di4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Di4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Di8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Di8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_i8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Di8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Di8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Di8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Di8 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Dr4(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Dr4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r4), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Dr4 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Dr4 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Dr4(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Dr4 
 
!---------------------------------------------------------------------------- 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7Dr8(array, &
 totalCount, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: totalCount 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_LAWrap7Dr8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_r8), dimension(:,:,:,:,:,:,:), pointer :: farrayPtr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7Dr8 => fshape 
 call c_ESMC_LocalArraySetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceFPtr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetFPtr(array, wrap, localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 farrayPtr => wrap%ptr7Dr8 
!print *, "First recursion: ", lbound(farrayPtr), ubound(farrayPtr) 
!call c_esmc_vmpointerprint(farrayPtr) 
 call ESMF_LocalArrayAdjust7Dr8(array, totalCount, lb, ub, farrayPtr, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7Dr8 
 
!---------------------------------------------------------------------------- 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayValidate"
!BOPI
! !IROUTINE: ESMF_LocalArrayValidate - Check validity of LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array
    character(len = *), intent(in), optional :: options
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Validate a {\tt ESMF\_LocalArray} object.
!
!EOPI
!------------------------------------------------------------------------------
    character (len=6) :: defaultopts ! default print options
    integer :: localrc ! local return code
    logical :: rcpresent
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    defaultopts = "brief"
    if(present(options)) then
      !call c_ESMC_LocalArrayValidate(array, options, localrc)
    else
      !call c_ESMC_LocalArrayValidate(array, defaultopts, localrc)
    endif
    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayValidate
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayPrint"
!BOPI
! !IROUTINE: ESMF_LocalArrayPrint - Print contents of an LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayPrint(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array
    character(len = *), intent(in), optional :: options
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Print information about a {\tt ESMF\_LocalArray}.
!
!EOPI
!------------------------------------------------------------------------------
    character (len=6) :: defaultopts ! default print options
    integer :: localrc ! local return code
    logical :: rcpresent
    !character(len=ESMF_MAXSTR) :: msgbuf
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    if (array%this .eq. ESMF_NULL_POINTER) then
      !write(msgbuf,*) "LocalArray Print:"
      !call ESMF_LogWrite(msgbuf, ESMF_LOGMSG_INFO)
      write(*,*) "LocalArray Print:"
      !write(msgbuf,*) " Empty or Uninitialized LocalArray"
      !call ESMF_LogWrite(msgbuf, ESMF_LOGMSG_INFO)
      write(*,*) " Empty or Uninitialized LocalArray"
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif
    defaultopts = "brief"
    ! Flush before crossing language interface to ensure correct output order
    call ESMF_UtilIOUnitFlush(ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if(present(options)) then
        call c_ESMC_LocalArrayPrint(array, options, localrc)
    else
        call c_ESMC_LocalArrayPrint(array, defaultopts, localrc)
    endif
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayPrint
!------------------------------------------------------------------------------
! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayGetInit"
!BOPI
! !IROUTINE: ESMF_LocalArrayGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LocalArrayGetInit(array)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_LocalArrayGetInit
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in), optional :: array
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [array]
! LocalArray object.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(array)) then
      ESMF_LocalArrayGetInit = ESMF_INIT_GET(array)
    else
      ESMF_LocalArrayGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_LocalArrayGetInit
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArraySetInitCreated - Set LocalArray init code to "CREATED"
! !INTERFACE:
  subroutine ESMF_LocalArraySetInitCreated(array, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set init code in LocalArray object to "CREATED".
!
! The arguments are:
! \begin{description}
! \item[array]
! Specified {\tt ESMF\_LocalArray} object.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Set init code
    ESMF_INIT_SET_CREATED(array)
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArraySetInitCreated
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrayGetThis - Internal access routine for C++ pointer
! !INTERFACE:
  subroutine ESMF_LocalArrayGetThis(array, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in), optional :: array
    type(ESMF_Pointer), intent(out) :: this
    integer, intent(out),optional :: rc
!
!
! !DESCRIPTION:
! Internal access routine for C++ pointer.
!
! The arguments are:
! \begin{description}
! \item[array]
! Specified {\tt ESMF\_LocalArray} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Copy C++ pointer
    this = array%this
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayGetThis
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArraySetThis - Set C++ pointer in LocalArray
! !INTERFACE:
  subroutine ESMF_LocalArraySetThis(localarray, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: localarray
    type(ESMF_Pointer), intent(in) :: this
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set C++ pointer in LocalArray.
!
! The arguments are:
! \begin{description}
! \item[localarray]
! Specified {\tt ESMF\_LocalArray} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Copy C++ pointer
    localarray%this = this
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArraySetThis
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_LocalArrayCreateMod
