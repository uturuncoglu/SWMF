module ESMF_grid_comp

  ! Code for the ESMFSWMF Gridded Component which creates 4 child Components:
  ! IPE, SWMF, IE and IPE_ie_coupler.

  ! ESMF Framework module
  use ESMF
  use NUOPC

  use NUOPC_Driver, &
      Driver_routine_SS             => SetServices, &
      Driver_label_SetModelServices => label_SetModelServices, &
      Driver_label_SetRunSequence   => label_SetRunSequence, &
      Driver_label_SetRunClock      => label_SetRunClock
  use NUOPC_Driver, only: NUOPC_DriverAddComp    
  !use NUOPC_Connector, only: conSS => SetServices
  !use NUOPC_Model, only: SetVM

  ! Various variables
  use ESMFSWMF_variables, ONLY: &
       iProcRootSwmf, iProcRootEsmf, iProcLastEsmf, iProcLastSwmf, &
       iProc0SwmfComp, iProcLastSwmfComp, nProcSwmfComp, &
       SyncFlag, write_log, write_error, NameParamFile

  ! User Component registration routines
  use SWMF_grid_comp, ONLY: swmf_set_services    => set_services
  use IPE_grid_comp,  ONLY: ipe_set_services     => set_services
  use RIM_grid_comp,  ONLY: rim_set_services     => set_services
  use IPERIM_coupler, ONLY: coupler_set_services => set_services

  implicit none
  private

  public:: ESMF_set_services

  type(ESMF_GridComp), save :: IpeComp, SwmfComp, RimComp
  type(ESMF_CplComp),  save :: CouplerComp
  type(ESMF_State),    save :: IpeExport, RimImport

contains
  !============================================================================
  subroutine ESMF_set_services(gComp, iError)

    type(ESMF_GridComp) :: gComp
    integer, intent(out):: iError

    type(ESMF_Config)   :: config
    !--------------------------------------------------------------------------
    call NUOPC_CompDerive(gComp, Driver_routine_SS, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_CompDerive')
    call NUOPC_CompSpecialize(gComp, specLabel=Driver_label_SetModelServices, &
         specRoutine=set_model_services, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_CompSpecialize - init')
    call NUOPC_CompSpecialize(gComp, specLabel=Driver_label_SetRunSequence, &
         specRoutine=set_run_sequence, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_CompSpecialize - set run seq.')
    config = ESMF_ConfigCreate(rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigCreate')
    call ESMF_ConfigLoadFile(config, trim(NameParamFile), rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigLoadFile')
    call ESMF_GridCompSet(gComp, config=config, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompSet')
    call NUOPC_FieldDictionarySetup('fd_swmf.yaml', rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_FieldDictionarySetup')

  end subroutine ESMF_set_services
  !============================================================================
  subroutine set_run_sequence(gComp, iError)

    type(ESMF_GridComp) :: gComp
    integer, intent(out):: iError

    type(ESMF_Config) :: config
    type(NUOPC_FreeFormat) :: runSeqFF
    !--------------------------------------------------------------------------

    ! Read free format run sequence from config
    call ESMF_GridCompGet(gComp, config=config, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompGet')
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_FreeFormatCreate')

    ! Ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(gComp, runSeqFF, &
         autoAddConnectors=.true., rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_DriverIngestRunSequence')
    call NUOPC_DriverPrint(gComp, orderflag=.true., rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_DriverPrint')

  end subroutine set_run_sequence
  !============================================================================
  subroutine set_model_services(gComp, iError)

    type(ESMF_GridComp) :: gComp
    integer, intent(out):: iError

    type(ESMF_Config) :: config
    type(NUOPC_FreeFormat) :: attrFF
    integer :: i, j, petCount, compCount
    integer :: petListBounds(2)
    integer, allocatable :: petList(:)
    character(len=32) :: model, prefix
    character(len=32), allocatable :: compLabels(:)
    !--------------------------------------------------------------------------    
    call write_log("ESMF_gric_comp set_model_services called")
    iError = ESMF_FAILURE

    ! Query gridded component
    call ESMF_GridCompGet(gComp, petCount=petCount, config=config, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompGet')

    ! Read and ingest free format driver attributes
    attrFF = NUOPC_FreeFormatCreate(config, label='DRV_attributes::', &
             relaxedflag=.true., rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_FreeFormatCreate')
    call NUOPC_CompAttributeIngest(gComp, attrFF, addFlag=.true., rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_CompAttributeIngest')
    call NUOPC_FreeFormatDestroy(attrFF, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('NUOPC_FreeFormatDestroy')

    ! Determine the generic component labels
    compCount = ESMF_ConfigGetLen(config, label='DRV_component_list:', rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigGetLen')

    allocate(compLabels(compCount))
    call ESMF_ConfigGetAttribute(config, valueList=compLabels, &
         label="DRV_component_list:", count=compCount, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigGetAttribute - drv')

    ! Determine information for each component and add to the driver
    do i = 1, compCount
       ! Construct component prefix
       prefix = trim(compLabels(i))
       ! Read in petList bounds
       call ESMF_ConfigGetAttribute(config, petListBounds, &
            label=trim(prefix)//"_petlist_bounds:", default=-1, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigGetAttribute - '//trim(prefix))
       ! Handle the default situation
       if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
          petListBounds(1) = 0
          petListBounds(2) = petCount - 1
       endif
       ! Read in model instance name
       call ESMF_ConfigGetAttribute(config, model, &
            label=trim(prefix)//"_model:", default="none", rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_ConfigGetAttribute - '//trim(model))
       ! Set petList for this component
       allocate(petList(petListBounds(2)-petListBounds(1)+1))
       do j = petListBounds(1), petListBounds(2)
          petList(j-petListBounds(1)+1) = j
       end do
       ! Add component/s
       ! SWMF
       if (trim(model) == 'swmf') then
          call NUOPC_DriverAddComp(gComp, trim(prefix), &
               swmf_set_services, petlist=petList, comp=SwmfComp, rc=iError)
          if(iError /= ESMF_SUCCESS)call my_error('NUOPC_DriverAddComp - SWMF')
       end if
       ! IPE
       if (trim(model) == 'ipe') then
          call NUOPC_DriverAddComp(gComp, trim(prefix), &
               ipe_set_services, petlist=petList, comp=IpeComp, rc=iError)
          if(iError /= ESMF_SUCCESS)call my_error('NUOPC_DriverAddComp - IPE')
       end if
       ! RIM
       if (trim(model) == 'rim') then
          call NUOPC_DriverAddComp(gComp, trim(prefix), &
               rim_set_services, petlist=petList, comp=RimComp, rc=iError)
          if(iError /= ESMF_SUCCESS)call my_error('NUOPC_DriverAddComp - RIM')
       end if
       ! Clear memory
       deallocate(petList)
    end do

    iError = ESMF_SUCCESS
    call write_log("ESMF_grid_comp set_model_services finished")

  end subroutine set_model_services
  !============================================================================
  subroutine my_init(gComp, ImportState, ExportState, ParentClock, iError)

    type(ESMF_GridComp):: gComp
    type(ESMF_State):: ImportState, ExportState
    type(ESMF_Clock):: Parentclock
    integer, intent(out) :: iError

    type(ESMF_VM)      :: ParentVM
    type(ESMF_Grid)    :: SwmfGrid, EsmfGrid
    type(ESMF_DELayout):: Layout

    integer, allocatable:: iProcCouple_I(:)
    integer :: i
    !--------------------------------------------------------------------------
    call write_log("ESMF_gric_comp init called")
    iError = ESMF_FAILURE

    ! Add the SWMF Gridded component
    SwmfComp = ESMF_GridCompCreate(name="SWMF Gridded Component", &
         petlist = [ (i, i=iProcRootSwmf, iProcLastSwmf) ], rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompCreate SWMF')

    ! Create the ESMF Gridded component(s, there could be more than one here)
    IpeComp = ESMF_GridCompCreate(name="IPE Gridded Component", &
         petlist = [ (i, i=iProcRootEsmf, iProcLastEsmf) ], rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompCreate IPE')

    RimComp = ESMF_GridCompCreate(name="RIM Gridded Component", &
         petlist = [ (i, i=iProc0SwmfComp, iProcLastSwmfComp) ], rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompCreate RIM')

    ! Create the Coupler component.
    ! Assume iProcRootEsmf <= iProc0SwmfComp <= iProcLastSwmfComp
    if(iProcRootEsmf == iProc0SwmfComp .and. nProcSwmfComp == 1)then
       ! All share the same PET
       iProcCouple_I = [ iProcRootEsmf ]
    elseif(iProcRootEsmf < iProc0SwmfComp .and. nProcSwmfComp == 2)then
       ! All run on different PETs
       iProcCouple_I = [ iProcRootEsmf, iProc0SwmfComp, iProcLastSwmfComp ]
    else
       ! They use 2 PETs
       iProcCouple_I = [ iProcRootEsmf, iProcLastSwmfComp ]
    end if
    CouplerComp = ESMF_CplCompCreate(name="ESMF-SWMF Coupler Component", &
         petlist = iProcCouple_I, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompCreate Coupler')
    deallocate(iProcCouple_I)

    call write_log("Component Creates finished")

    ! Call the SetServices routine for each so they can register their
    ! subroutines for Init, Run, and Finalize
    call ESMF_GridCompSetServices(IpeComp, &
         userRoutine=ipe_set_services, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompSetServices Esmf')

    call ESMF_GridCompSetServices(SwmfComp, &
         userRoutine=swmf_set_services, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompSetServices Swmf')

    call ESMF_GridCompSetServices(RimComp, &
         userRoutine=rim_set_services, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompSetServices Swmf')

    call ESMF_CplCompSetServices(CouplerComp, &
         userRoutine=coupler_set_services, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompSetServices Coupler')

    ! Create Import and Export State objects in order to pass data
    ! between the Coupler and the Gridded Components
    RimImport = ESMF_StateCreate(name="RIM Import", &
         stateintent=ESMF_STATEINTENT_IMPORT, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_StateCreate RimImport')
    IpeExport = ESMF_StateCreate(name="IPE Export", &
         stateintent=ESMF_STATEINTENT_EXPORT, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_StateCreate IpeExport')

    ! Each of the subcomponents initialize themselves.
    call ESMF_GridCompInitialize(IpeComp, exportState = IpeExport, &
         clock=parentclock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompInitialize IPE')

    call ESMF_GridCompInitialize(SwmfComp, clock=parentclock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompInitialize SWMF')

    call ESMF_GridCompInitialize(RimComp, importState=RimImport, &
         clock=parentclock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompInitialize RIM')

    call ESMF_CplCompInitialize(CouplerComp, &
         importState = IpeExport, exportState=RimImport, &
         clock=ParentClock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_CplCompInitialize')

    iError = ESMF_SUCCESS
    call write_log("ESMF_grid_comp init finished")

  end subroutine my_init
  !============================================================================
  subroutine my_run(gComp, ImportState, ExportState, ParentClock, iError)

    type(ESMF_GridComp):: gComp
    type(ESMF_State)   :: ImportState
    type(ESMF_State)   :: ExportState
    type(ESMF_Clock)   :: Parentclock
    integer, intent(out):: iError

    ! Local variables
    type(ESMF_Clock) :: LocalClock
    !--------------------------------------------------------------------------
    call write_log("ESMF_grid_comp run routine called")

    iError = ESMF_FAILURE

    ! make our own local copy of the clock
    LocalClock = ESMF_ClockCreate(ParentClock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ClockCreate')

    do
       if(ESMF_ClockIsStopTime(LocalClock, rc=iError)) EXIT
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_ClockIsStopTime')

       ! Couple the subcomponents first so that SWMF has the input from ESMF
       call ESMF_CplCompRun(CouplerComp, &
            importstate=IpeExport, exportstate=RimImport, &
            clock=LocalClock, syncflag=SyncFlag, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_CplCompRun')

       ! Run RIM to copy/check the received state
       call ESMF_GridCompRun(RimComp, importState=RimImport, &
            clock=LocalClock, syncflag=SyncFlag, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompRun Swmf')

       ! Run SWMF
       call ESMF_GridCompRun(SwmfComp, &
            clock=LocalClock, syncflag=SyncFlag, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompRun Swmf')

       ! Run IPE
       call ESMF_GridCompRun(IpeComp, exportState=IpeExport, &
            clock=LocalClock, syncflag=SyncFlag, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_GridCompRun Esmf')

       ! Advance the time
       call ESMF_ClockAdvance(localclock, rc=iError)
       if(iError /= ESMF_SUCCESS)call my_error('ESMF_ClockAdvance')

    end do

    call ESMF_ClockDestroy(localclock, rc=iError)
    if(iError /= ESMF_SUCCESS)call my_error('ESMF_ClockDestroy')

    iError = ESMF_SUCCESS
    call write_log("ESMF_grid_comp run finished")

  end subroutine my_run
  !============================================================================
  subroutine my_final(gComp, ImportState, ExportState, Parentclock, iError)

    type(ESMF_GridComp):: gComp
    type(ESMF_State):: ImportState
    type(ESMF_State):: ExportState
    type(ESMF_Clock):: ParentClock
    integer, intent(out) :: iError
    !--------------------------------------------------------------------------
    call write_log("ESMF-SWMF Finalize routine called")

    call ESMF_GridCompFinalize(SwmfComp, clock=Parentclock, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompFinalize SwmfComp')

    ! Give each of the subcomponents and the coupler a chance to finalize
    call ESMF_GridCompFinalize(RimComp, importState=RimImport, &
         clock=Parentclock, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompFinalize RimComp')

    call ESMF_GridCompFinalize(IpeComp, exportState=IpeExport, &
         clock=ParentClock, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompFinalize IpeComp')

    call ESMF_CplCompFinalize(CouplerComp, &
         importState=IpeExport, exportState=RimImport, &
         clock=ParentClock, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_CplCompFinalize')

    ! Now remove the Components to free up their resources
    call ESMF_GridCompDestroy(IpeComp, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompDestroy IpeComp')

    call ESMF_GridCompDestroy(SwmfComp, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompDestroy SwmfComp')

    call ESMF_GridCompDestroy(RimComp, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_GridCompDestroy RimComp')

    call ESMF_CplCompDestroy(CouplerComp, rc=iError)
    if(iError /= ESMF_SUCCESS) call my_error('ESMF_CplCompDestroy')

    iError = ESMF_SUCCESS
    call write_log( "ESMF-SWMF Finalize routine finished")

  end subroutine my_final
  !============================================================================
  subroutine my_error(String)

    character(len=*), intent(in) :: String
    !--------------------------------------------------------------------------
    call write_error("ESMF_grid_comp "//String)

  end subroutine my_error
  !============================================================================
end module ESMF_grid_comp
!==============================================================================
