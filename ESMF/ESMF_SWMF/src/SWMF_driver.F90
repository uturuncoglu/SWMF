module SWMF_driver

  !-----------------------------------------------------------------------------
  ! SWMF's parent driver
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet, ESMF_GridCompSet
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO
  use ESMF, only: ESMF_SUCCESS
  use ESMF, only: ESMF_State, ESMF_Clock
  use ESMF, only: ESMF_Config, ESMF_ConfigCreate, ESMF_ConfigLoadFile

  use NUOPC, only: NUOPC_CompDerive, NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompAttributeSet
  use NUOPC, only: NUOPC_FreeFormat, NUOPC_FreeFormatDestroy
  use NUOPC, only: NUOPC_FreeFormatCreate

  use NUOPC_Driver, only: SetVM  
  use NUOPC_Driver, only: driverSS => SetServices
  use NUOPC_Driver, only: label_SetModelServices
  use NUOPC_Driver, only: driver_label_SetRunSequence => label_SetRunSequence
  use NUOPC_Driver, only: NUOPC_DriverIngestRunSequence, NUOPC_DriverSetRunSequence

  use SWMF_shared, only: ChkErr

  implicit none
  private

  !-----------------------------------------------------------------------------
  ! Public module routines
  !-----------------------------------------------------------------------------

  public :: SetVM  
  public :: SetServices

  !-----------------------------------------------------------------------------
  ! Private module routines
  !-----------------------------------------------------------------------------

  private :: SetRunSequence

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  character(len=*), parameter :: modName = "(SWMF_driver)"
  character(len=*), parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================        

  subroutine SetServices(driver, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Config) :: config
    character(len=*), parameter :: subname = trim(modName)//':(SetServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! derive from NUOPC_Driver
    !------------------

    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Specialize driver
    !------------------

    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, specRoutine=SetModelServices, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, specRoutine=SetRunSequence, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Set driver verbosity
    !------------------

    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Create, open and set the config
    !------------------

    config = ESMF_ConfigCreate(rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ConfigLoadFile(config, "swmf.configure", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridCompSet(driver, config=config, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetServices

  !=============================================================================

  subroutine SetModelServices(driver, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(SetModelServices) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Create and initialize a clock
    !------------------

    !call ESMF_TimeIntervalSet(TimeStep, s=iCoupleFreq, rc=iError)

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetModelServices

  !=============================================================================

  subroutine SetRunSequence(driver, rc)

    ! input/output variables
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Config) :: config
    type(NUOPC_FreeFormat) :: runSeqFF
    character(len=*), parameter :: subname = trim(modName)//':(SetRunSequence) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Run Sequence and Connectors
    !------------------

    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, autoAddConnectors=.true., rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine SetRunSequence

end module SWMF_driver
