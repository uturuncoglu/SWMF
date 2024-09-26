module IPE_grid_comp

  !-----------------------------------------------------------------------------
  ! The NUOPC cap for IPE model which is a physics-based model of the 
  ! ionosphere and plasmasphere
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF, only: ESMF_SUCCESS
  use ESMF, only: ESMF_METHOD_INITIALIZE
  use ESMF, only: ESMF_State, ESMF_Clock

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSetEntryPoint

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: model_routine_SS => SetServices

  use SWMF_shared, only: ChkErr

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: SetServices
  public :: SetVM

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  !private :: InitializeAdvertise ! Advertise the fields that can be passed

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(IPE_grid_comp)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================        

  subroutine SetServices(gcomp, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(SetServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! register the generic methods
    !------------------

    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! set entry point for methods that require specific implementation
    !------------------

    !call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
    !     phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    !if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices    

  !=============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    ! Advertise the fields that can be exchanged
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(InitializeAdvertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    ! Query import and export states
    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Export

    ! Import

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise    



end module IPE_grid_comp
