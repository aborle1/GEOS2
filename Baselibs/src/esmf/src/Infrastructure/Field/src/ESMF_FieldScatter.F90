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
!
#define ESMF_FILENAME "ESMF_FieldScatter.F90"
!
! ESMF Field Communications Scatter module
module ESMF_FieldScatterMod
!
!==============================================================================
!
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldScatterMod - FieldScatter routines for Field objects
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldScatter} subroutine.
!
!EOPI
!------------------------------------------------------------------------------
! !USES:
    use ESMF_UtilTypesMod
    use ESMF_InitMacrosMod
    use ESMF_LogErrMod
    use ESMF_VMMod
    use ESMF_FieldMod
    use ESMF_FieldGetMod
    use ESMF_ArrayMod
    implicit none
    private
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
! <none>
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
! <none>
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_FieldScatter
!
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter, private :: version = &
      '$Id$'
!------------------------------------------------------------------------------
    interface ESMF_FieldScatter
        !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef PIO_TKR 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FieldScatter1DI1 
 module procedure ESMF_FieldScatter2DI1 
 module procedure ESMF_FieldScatter3DI1 
 module procedure ESMF_FieldScatter4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldScatter5DI1 
 module procedure ESMF_FieldScatter6DI1 
 module procedure ESMF_FieldScatter7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FieldScatter1DI2 
 module procedure ESMF_FieldScatter2DI2 
 module procedure ESMF_FieldScatter3DI2 
 module procedure ESMF_FieldScatter4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldScatter5DI2 
 module procedure ESMF_FieldScatter6DI2 
 module procedure ESMF_FieldScatter7DI2 
#endif 
#endif 
#endif 
 module procedure ESMF_FieldScatter1DI4 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter1DI8 
#endif 
 module procedure ESMF_FieldScatter1DR4 
 module procedure ESMF_FieldScatter1DR8 
 module procedure ESMF_FieldScatter2DI4 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter2DI8 
#endif 
 module procedure ESMF_FieldScatter2DR4 
 module procedure ESMF_FieldScatter2DR8 
 module procedure ESMF_FieldScatter3DI4 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter3DI8 
#endif 
 module procedure ESMF_FieldScatter3DR4 
 module procedure ESMF_FieldScatter3DR8 
 module procedure ESMF_FieldScatter4DI4 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter4DI8 
#endif 
 module procedure ESMF_FieldScatter4DR4 
 module procedure ESMF_FieldScatter4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_FieldScatter5DI4 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter5DI8 
#endif 
 module procedure ESMF_FieldScatter5DR4 
 module procedure ESMF_FieldScatter5DR8 
#ifndef PIO_TKR 
 module procedure ESMF_FieldScatter6DI4 
 module procedure ESMF_FieldScatter6DI8 
 module procedure ESMF_FieldScatter6DR4 
 module procedure ESMF_FieldScatter6DR8 
 module procedure ESMF_FieldScatter7DI4 
 module procedure ESMF_FieldScatter7DI8 
 module procedure ESMF_FieldScatter7DR4 
 module procedure ESMF_FieldScatter7DR8 
#endif 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

        module procedure ESMF_FieldScatterNotRoot
    end interface
!------------------------------------------------------------------------------
contains
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!---------------------------------------------------------------------------- 
!BOP 
! 
! !IROUTINE: ESMF_FieldScatter - Scatter a Fortran array across the ESMF_Field 
! 
! !INTERFACE: 
! subroutine ESMF_FieldScatter<rank><type><kind>(field, farray, & 
! rootPet, keywordEnforcer, tile, vm, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_Field), intent(inout) :: field 
! mtype (ESMF_KIND_mtypekind),intent(in), target :: farray(mdim) 
! integer, intent(in) :: rootPet 
! type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
! integer, intent(in), optional :: tile 
! type(ESMF_VM), intent(in), optional :: vm 
! integer, intent(out), optional :: rc 
! 
! 
! 
! !STATUS: 
! \begin{itemize} 
! \item\apiStatusCompatibleVersion{5.2.0r} 
! \end{itemize} 
! 
! !DESCRIPTION: 
! Scatter the data of {\tt farray} located on {\tt rootPET} 
! across an {ESMF\_Field} object. A single {\tt farray} must be 
! scattered across a single DistGrid tile in Field. The optional {\tt tile} 
! argument allows selection of the tile. For Fields defined on a single 
! tile DistGrid the default selection (tile 1) will be correct. The 
! shape of {\tt farray} must match the shape of the tile in Field. 
! 
! If the Field contains replicating DistGrid dimensions data will be 
! scattered across all of the replicated pieces. 
! 
! The implementation of Scatter and Gather is not seqence index based. 
! If the Field is built on arbitrarily distributed Grid, Mesh, LocStream or XGrid, 
! Scatter will not scatter data from rootPet 
! to the destination data points corresponding to the sequence index on the rootPet. 
! Instead Scatter will scatter a contiguous memory range from rootPet to 
! destination PET. The size of the memory range is equal to the number of 
! data elements on the destination PET. Vice versa for the Gather operation. 
! In this case, the user should use {\tt ESMF\_FieldRedist} to achieve 
! the same data operation result. For examples how to use {\tt ESMF\_FieldRedist} 
! to perform Gather and Scatter, please refer to 
! \ref{sec:field:usage:redist_gathering} and 
! \ref{sec:field:usage:redist_scattering}. 
! 
! This version of the interface implements the PET-based blocking paradigm: 
! Each PET of the VM must issue this call exactly once for {\em all} of its 
! DEs. The call will block until all PET-local data objects are accessible. 
! 
! For examples and associated documentations using this method see Section 
! \ref{sec:field:usage:scatter_2dptr}. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} object across which data will be scattered. 
! \item[\{farray\}] 
! The Fortran array that is to be scattered. Only root 
! must provide a valid {\tt farray}, the other PETs may treat 
! {\tt farray} as an optional argument. 
! \item[rootPet] 
! PET that holds the valid data in {\tt farray}. 
! \item[{[tile]}] 
! The DistGrid tile in {\tt field} into which to scatter {\tt farray}. 
! By default {\tt farray} will be scattered into tile 1. 
! \item[{[vm]}] 
! Optional {\tt ESMF\_VM} object of the current context. Providing the 
! VM of the current context will lower the method's overhead. 
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
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Di1 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Di1 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Di1(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i1), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Di1 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Di2 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Di2 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Di2(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i2), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Di2 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Di4 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Di8 
!------------------------------------------------------------------------------ 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter1Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter1Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Di4 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Di8 
!------------------------------------------------------------------------------ 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter2Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter2Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Di4 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Di8 
!------------------------------------------------------------------------------ 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter3Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter3Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Di4 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Di8 
!------------------------------------------------------------------------------ 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter4Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter4Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Di4 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Di8 
!------------------------------------------------------------------------------ 
 
#endif 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter5Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter5Dr8 
!------------------------------------------------------------------------------ 
 
#ifndef PIO_TKR 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter6Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter6Dr8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Di4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i4), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Di4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Di8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_i8), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Di8 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Dr4(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r4), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Dr4 
!------------------------------------------------------------------------------ 
 
!---------------------------------------------------------------------------- 
#undef ESMF_METHOD 
#define ESMF_METHOD "ESMF_FieldScatter" 
 subroutine ESMF_FieldScatter7Dr8(field, farray, & 
 rootPet, keywordEnforcer, tile, vm, rc) 

 ! input arguments 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_r8), intent(in), target :: farray(:,:,:,:,:,:,:) 
 integer, intent(in) :: rootPet 
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below 
 integer, intent(in), optional :: tile 
 type(ESMF_VM), intent(in), optional :: vm 
 integer, intent(out), optional :: rc 

 ! internal local variables 
 integer :: localrc 
 type(ESMF_Array) :: array 

 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if(present(rc)) rc = ESMF_RC_NOT_IMPL 

 ! check variable: focus on field and farray 
 ! rely on ArrayScatter to check the sanity of other variables 
 ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc) 
 ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc) 

 ! Check the rank of the native array. 
 call ESMF_FieldGet(field, array=array, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 ! perform scatter through internal array 
 ! For performance consideration: 
 ! Rely on ArrayScatter to perform sanity checking of the other parameters 
 call ESMF_ArrayScatter(array, farray, rootPet=rootPet, tile=tile, & 
 vm=vm, rc=localrc) 
 if (ESMF_LogFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rcToReturn=rc)) return 

 if (present(rc)) rc = ESMF_SUCCESS 
 end subroutine ESMF_FieldScatter7Dr8 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! -------------------------- ESMF-public method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldScatter"
subroutine ESMF_FieldScatterNotRoot(field, tile, rootPet, vm, rc)
    type(ESMF_Field), intent(inout) :: field
    integer, intent(in), optional :: tile
    integer, intent(in) :: rootPet
    type(ESMF_VM), intent(in), optional :: vm
    integer, intent(out), optional :: rc
    ! Local variables
    integer :: localrc ! local return code
    type(ESMF_Array) :: array
    ! Initialize return code
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit, field, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    call ESMF_FieldGet(field, array=array, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        call ESMF_ArrayScatter(array, rootPet=rootPet, tile=tile, vm=vm, &
          rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
end subroutine ESMF_FieldScatterNotRoot
end module
