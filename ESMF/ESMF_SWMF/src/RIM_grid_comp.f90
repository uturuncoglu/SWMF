module RIM_grid_comp

  !-----------------------------------------------------------------------------
  ! The NUOPC cap for RIM model which is an interface for SWMF/IE/RIM
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet
  use ESMF, only: ESMF_Grid, ESMF_GridWriteVTK, ESMF_GridCreateNoPeriDim
  use ESMF, only: ESMF_GridAddCoord, ESMF_GridGetCoord
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_LOGMSG_ERROR
  use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_SUCCESS
  use ESMF, only: ESMF_INDEX_GLOBAL, ESMF_MAXSTR, ESMF_METHOD_INITIALIZE
  use ESMF, only: ESMF_State, ESMF_StateRemove, ESMF_Clock
  use ESMF, only: ESMF_ClockGet, ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_COORDSYS_CART, ESMF_STAGGERLOC_CORNER
  use ESMF, only: ESMF_StateAdd, ESMF_KIND_R8, ESMF_TYPEKIND_R8
  use ESMF, only: ESMF_GridCompSetEntryPoint, ESMF_UtilStringLowerCase
  use ESMF, only: ESMF_ArraySpec, ESMF_ArraySpecSet
  use ESMF, only: ESMF_Field, ESMF_FieldCreate, ESMF_FieldGet

  use NUOPC, only: NUOPC_CompDerive
  use NUOPC, only: NUOPC_CompSetEntryPoint
  use NUOPC, only: NUOPC_CompSpecialize
  use NUOPC, only: NUOPC_CompAttributeGet
  use NUOPC, only: NUOPC_CompFilterPhaseMap
  use NUOPC, only: NUOPC_IsConnected
  use NUOPC, only: NUOPC_Realize, NUOPC_Advertise

  use NUOPC_Model, only: SetVM
  use NUOPC_Model, only: NUOPC_ModelGet
  use NUOPC_Model, only: modelSS => SetServices
  use NUOPC_Model, only: model_label_Advance => label_Advance  
  use NUOPC_Model, only: model_label_Finalize => label_Finalize

  use IE_ModSize, only: nLat => IONO_nTheta
  use IE_ModSize, only: nLon => IONO_nPsi

  use SWMF_shared, only: ChkErr
  use SWMF_shared, only: configType, exchType
  use SWMF_shared, only: FieldTest_V, CoordCoefTest, dHallPerDtTest

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

  type(configType) :: config
  type(exchType) :: importFields(2)
  real(ESMF_KIND_R8), pointer :: Lon_I(:), Lat_I(:)

  character(len=*), parameter :: modName = "(RIM_grid_comp)"
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

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
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
    integer :: n
    character(len=*), parameter :: subname = trim(modName)//':(InitializeAdvertise) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Populate field lists
    !------------------

    importFields(1)%shortName = 'Hall'
    importFields(2)%shortName = 'Ped'

    !------------------
    ! Advertise import fields
    !------------------

    do n = 1, size(importFields)
       call NUOPC_Advertise(importState, standardName=importFields(n)%shortName, &
         TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end do

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
    type(ESMF_VM) :: vm
    type(ESMF_Grid) :: grid
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_Field) :: field
    integer :: i, j, n
    integer :: Istr, Iend, Jstr, Jend
    integer :: petCount, localPet
    integer :: mpiCommunicator
    logical :: isSet, isPresent
    character(len=ESMF_MAXSTR) :: cvalue, msg
    character(len=*), parameter :: subname = trim(modName)//':(InitializeRealize) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query component
    !------------------

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, &
      mpiCommunicator=mpiCommunicator, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Read configuration
    !------------------

    ! Debug level
    call NUOPC_CompAttributeGet(gcomp, name='debugLevel', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(cvalue,*) config%debugLevel
    else
       config%debugLevel = 0
    end if
    write(msg, fmt='(A,I2)') trim(subname)//': debugLevel = ', config%debugLevel
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    ! Test mode 
    call NUOPC_CompAttributeGet(gcomp, name='doTest', value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    config%doTest = .false.
    if (isPresent .and. isSet) then
       if (trim(adjustl(cvalue)) == 'T' .or. trim(cvalue) == 'true' .or. trim(cvalue) == '.true.') then
          config%doTest = .true.
       end if   
    end if
    write(msg, fmt='(A,L1)') trim(subname)//': doTest = ', config%doTest
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    !------------------
    ! Create component grid
    !------------------

    ! RIM grid is node based. Internally it is Colat-Lon grid, but we pretend
    ! here that it is a Lat-Lon grid, so ESMF can use it.
    ! Lon from 0 to 360-dPhi (periodic), Lat from -90 to +90    
    ! Uses 1d decomposition along the latitude
    grid = ESMF_GridCreateNoPeriDim(maxIndex=[nLon-1, nLat-1], &
      regDecomp=[1, petCount], coordDep1=[1], coordDep2=[2], &
      coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
      name="RIM grid", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Fill longitude
    call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
      farrayPtr=Lon_I, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    Istr = lbound(Lon_I, dim=1)
    Iend = ubound(Lon_I, dim=1)

    do i = Istr, Iend
       Lon_I(i) = (i-1)*(360.0d0/(nLon-1))-180.0d0
    end do

    ! Fill latitude
    call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
      farrayPtr=Lat_I, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    Jstr = lbound(Lat_I, dim=1)
    Jend = ubound(Lat_I, dim=1)

    do j = Jstr, Jend
       Lat_I(j) = (j-1)*(180.0d0/(nLat-1))-90.0d0
    end do

    ! Print out coordinate information for debugging
    if (config%debugLevel > 5) then
       write(msg, fmt='(A,2F10.3)') trim(subname)//': Lon min, max = ', &
         minval(Lon_I), maxval(Lon_I)
       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
       write(msg, fmt='(A,2F10.3)') trim(subname)//': Lat min, max = ', &
         minval(Lat_I), maxval(Lat_I)
       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    end if

    !------------------
    ! Realize the actively coupled fields
    !------------------

    call ESMF_ArraySpecSet(arraySpec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add import fields to the state
    do n = 1, size(importFields)
       if (NUOPC_IsConnected(importState, fieldName=trim(importFields(n)%shortName))) then
          ! Create field
          importFields(n)%field = ESMF_FieldCreate(grid, arrayspec=arraySpec, &
            staggerloc=ESMF_STAGGERLOC_CORNER, name=trim(importFields(n)%shortName), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          call ESMF_LogWrite(trim(subname)//": Field = "//trim(importFields(n)%shortName)// &
            " is connected using grid", ESMF_LOGMSG_INFO)

          ! Realize field 
          call NUOPC_Realize(importState, field=importFields(n)%field, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else
          ! Remove field from import state
          call ESMF_StateRemove(importState, (/ trim(importFields(n)%shortName) /), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          call ESMF_LogWrite(trim(subname)//": Field = "// trim(importFields(n)%shortName)// &
            " is not connected and removed from import state.", ESMF_LOGMSG_INFO)
       end if
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !=============================================================================

  subroutine ModelAdvance(gcomp, rc)

    ! Advance the model component
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(len=*), parameter :: subname = trim(modName)//':(ModelAdvance) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------    
    ! Update model state by using its import state
    !------------------

    call ModelImport(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Advance model
    !------------------    

    ! Note: There is no actual advance routine for RIM model since its
    ! called by advance routine of SWMF component - SWMF_Run(). RIM
    ! component is just responsible to fill the internal data structures
    ! for conductance data (Pedersen and high all).

    !------------------    
    ! Update model export state
    !------------------

    ! Note: Currently RIM has no export state

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

  !=============================================================================

  subroutine ModelImport(gcomp, rc)

    ! Imports fields into RIM array structures
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep, currSimTime
    type(ESMF_State) :: importState
    real(ESMF_KIND_R8), pointer :: Ptr_II(:,:)
    integer :: n, i, j
    integer :: Istr, Iend, Jstr, Jend
    real(ESMF_KIND_R8) :: Exact_V(2)
    real(ESMF_KIND_R8) :: couplingFreq
    real(ESMF_KIND_R8) :: tCurrent, s, ms
    character(len=ESMF_MAXSTR) :: msg
    character(len=*), parameter :: subname = trim(modName)//':(ModelImport) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------    
    ! Query component for its import state
    !------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, importState=importState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Query clock 
    !------------------

    call ESMF_ClockGet(clock, currSimTime=currSimTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalGet(timeStep, s_r8=couplingFreq, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalGet(currSimTime, s_r8=s, ms_r8=ms, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    tCurrent = s+0.001d0*ms

    !------------------
    ! Loop over fields and update them
    !------------------

    do n = 1, size(importFields)
       ! Return field pointer
       call ESMF_FieldGet(importFields(n)%field, farrayPtr=Ptr_II, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Check min and max values
       write(msg, fmt='(A,2E12.5)') trim(subname)//': '//trim(importFields(n)%shortName)// &
         ' min, max = ', minval(Ptr_II), maxval(Ptr_II)
       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

       ! Get dimension extents
       Istr = lbound(Ptr_II, dim=1)
       Iend = ubound(Ptr_II, dim=1)
       Jstr = lbound(Ptr_II, dim=2)
       Jend = ubound(Ptr_II, dim=2)

       ! Check if entering test mode or not 
       if (config%doTest) then
          call ESMF_LogWrite(subname//' entering test mode', ESMF_LOGMSG_INFO)

          ! Loop over grid points and check with exact data
          do j = Jstr, Jend 
             do i = Istr, Iend
                ! Calculate exact solution
                Exact_V = FieldTest_V

                ! Add time dependence for Hall field
                Exact_V(1) = Exact_V(1)+tCurrent*dHallPerDtTest

                ! Add spatial dependence
                Exact_V = Exact_V+CoordCoefTest*abs(Lon_I(i))*(90.0d0-abs(Lat_I(j)))

                ! Check with the data coming from import fields
                select case(trim(importFields(n)%shortName))
                case('Hall')
                    if (abs(Ptr_II(i,j)-Exact_V(1)) > 1e-10) then
                       write(msg, fmt='(A,2I4,2F10.3,3E12.5)') trim(subname)//': '// &
                         'issue with incoming Hall at i, j, Lon, Lat, Hall, Exact, Error = ', &
                         i, j, Lon_I(i), Lat_I(j), Ptr_II(i,j), Exact_V(1), Ptr_II(i,j)-Exact_V(1)
                       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
                    end if
                case('Ped')
                    if (abs(Ptr_II(i,j)-Exact_V(2)) > 1e-10) then
                       write(msg, fmt='(A,2I4,2F10.3,3E12.5)') trim(subname)//': '// &
                         'issue with incoming Ped  at i, j, Lon, Lat, Hall, Exact, Error = ', &
                         i, j, Lon_I(i), Lat_I(j), Ptr_II(i,j), Exact_V(2), Ptr_II(i,j)-Exact_V(2)
                       call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
                    end if
                end select
             end do
          end do

          call ESMF_LogWrite(subname//' exiting test mode', ESMF_LOGMSG_INFO)
       else
          call ESMF_LogWrite(subname//' entering active coupling mode', ESMF_LOGMSG_INFO)
          call ESMF_LogWrite(subname//' exiting active coupling mode', ESMF_LOGMSG_INFO)
       end if
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelImport        

end module RIM_grid_comp
