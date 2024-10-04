module SWMF_shared

  !-----------------------------------------------------------------------------
  ! SWMF's shared utilities
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
  use ESMF, only: ESMF_Field

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
  end type configType

  public :: configType

  ! Data type for exchange fields
  type exchType
     type(ESMF_Field)   :: field
     character(len=4)   :: shortName
  end type exchType

  public exchType

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
