module SWMF_grid_comp

  !-----------------------------------------------------------------------------
  ! The NUOPC cap for SWMF model which is a physics-based model of the ionosphere
  !-----------------------------------------------------------------------------

  use ESMF, only: operator(+)
  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF, only: ESMF_KIND_I4, ESMF_KIND_R8
  use ESMF, only: ESMF_SUCCESS, ESMF_LOGMSG_ERROR, ESMF_END_ABORT
  use ESMF, only: ESMF_Finalize, ESMF_METHOD_INITIALIZE
  use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_State, ESMF_Clock
  use ESMF, only: ESMF_Time, ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_TimeGet, ESMF_ClockGet
  use ESMF, only: ESMF_TimeIntervalPrint, ESMF_TimePrint
  use ESMF, only: ESMF_GridCompSetEntryPoint

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSetEntryPoint
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompFilterPhaseMap

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: modelSS => SetServices
  use NUOPC_Model, only: model_label_Advance => label_Advance  
  use NUOPC_Model, only: model_label_Finalize => label_Finalize

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

  private :: InitializeAdvertise ! Advertise the fields that can be passed
  private :: InitializeRealize   ! Realize the list of fields that will be exchanged
  private :: ModelAdvance        ! Advance the model
  private :: ModelFinalize       ! Finalize the model 

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(SWMF_grid_comp)"
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

    call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! switching to IPD versions
    !------------------

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=InitializeP0, phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! set entry point for methods that require specific implementation
    !------------------

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! attach specializing method(s)
    !------------------

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
         specRoutine=ModelFinalize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices    

  !=============================================================================

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)

    ! Phase zero initialization
    ! input/output variables
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(InitializeP0) '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------    
    ! Switch to IPDv01 by filtering all other phaseMap entries
    !------------------    

    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeP0  

  !=============================================================================

  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    ! Advertise the fields that can be exchanged
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: startTime
    type(ESMF_TimeInterval) :: currSimTime, runDuration
    integer :: ierr
    logical :: isLastSession
    integer :: mpiCommunicator
    integer :: iStartTime(7)
    real(ESMF_KIND_R8) :: s, ms 
    real(ESMF_KIND_R8) :: currTime, stopTime
    character(len=*), parameter :: subname = trim(modName)//':(InitializeAdvertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query component
    !------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, mpiCommunicator=mpiCommunicator, rc=rc)    
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
    !------------------
    ! Query clock 
    !------------------

    call ESMF_ClockGet(clock, startTime=startTime, currSimTime=currSimTime, &
      runDuration=runDuration, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain the simulation start time
    iStartTime(:) = 0
    call ESMF_TimeGet(startTime, yy=iStartTime(1), mm=iStartTime(2), &
      dd=iStartTime(3), h=iStartTime(4), m=iStartTime(5), &
      s=iStartTime(6), ms=iStartTime(7), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Obtain the simulation time from the clock
    call ESMF_TimeIntervalGet(currSimTime, s_r8=s, ms_r8=ms, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    currTime = s+ms/1000.0d0

    ! Obtain the final simulation time from the clock
    call ESMF_TimeIntervalGet(runDuration, s_r8=s, ms_r8=ms, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    stopTime = s+ms/1000.0d0

    !------------------
    ! Call SWMF initialization routine
    !------------------

    call SWMF_initialize(mpiCommunicator, iStartTime, &
      currTime, stopTime, isLastSession, ierr)
    if (ierr /= 0) then
       call ESMF_LogWrite(subname//': error in SWMF_initialize.', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    end if

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeAdvertise    

  !=============================================================================

  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)

    ! Realize the fields that can be exchanged
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(InitializeRealize) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !=============================================================================

  subroutine ModelAdvance(gcomp, rc)

    ! Advance the model component
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: nextTime
    type(ESMF_TimeInterval) :: timeStep
    integer :: ierr
    logical :: DoStop
    real(ESMF_KIND_R8) :: s, ms
    real(ESMF_KIND_R8) :: tCouple, tSimSwmf
    character(len=*), parameter :: subname = trim(modName)//':(ModelAdvance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query component clock
    !------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Calculate simulation time
    !------------------

    nextTime = currTime+timeStep

    call ESMF_TimeGet(nextTime, s_r8=s, ms_r8=ms, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return    

    tCouple = s+0.001*ms

    !------------------
    ! Run SWMF 
    !------------------

    tSimSwmf = tCouple
    !call SWMF_run('IE', tCouple, tSimSwmf, DoStop, ierr)
    call SWMF_run('**', tCouple, tSimSwmf, DoStop, ierr)
    if (ierr /= 0) then
       call ESMF_LogWrite(subname//': error in SWMF_run.', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
       return
    end if

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelAdvance

  !=============================================================================
  
  subroutine ModelFinalize(gcomp, rc)

    ! Finalize the model component
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(ModelFinalize) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelFinalize  

end module SWMF_grid_comp
