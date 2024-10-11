module SWMF_shared

  !-----------------------------------------------------------------------------
  ! SWMF's shared utilities
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
  use ESMF, only: ESMF_Grid, ESMF_Field, ESMF_Array, ESMF_ArraySet
  use ESMF, only: ESMF_ArrayBundle, ESMF_ArrayBundleCreate, ESMF_ArrayBundleDestroy
  use ESMF, only: ESMF_ArrayBundleAdd, ESMF_ArrayBundleWrite
  use ESMF, only: ESMF_GridGetCoord, ESMF_LogWrite
  use ESMF, only: ESMF_STAGGERLOC_CORNER, ESMF_LOGMSG_INFO, ESMF_SUCCESS
  use ESMF, only: ESMF_KIND_R8

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: ChkErr

  !-----------------------------------------------------------------------------
  ! Public module data
  !-----------------------------------------------------------------------------

  ! Data type for model configuration
  type configType
     integer :: debugLevel
     logical :: doTest
  end type configType

  public :: configType

  ! Data type for exchange fields
  type exchType
     type(ESMF_Field)   :: field
     character(len=4)   :: shortName
  end type exchType

  public exchType

  ! Field values and coordinate coefficients for testing
  real(ESMF_KIND_R8), public, parameter :: FieldTest_V(2) = [3.0d0, 5.0d0]
  real(ESMF_KIND_R8), public, parameter :: CoordCoefTest = 0.1d0

  ! Change of Hall field during run
  real(ESMF_KIND_R8), public, parameter:: dHallPerDtTest = 0.4d0  

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(SWMF_shared)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================        

  logical function ChkErr(rc, line, file)

    ! input/output variables
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    ! local variables
    integer :: lrc
    !---------------------------------------------------------------------------

    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
       ChkErr = .true.
    endif

  end function ChkErr

end module SWMF_shared
