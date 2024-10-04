module IPE_grid_comp

  !-----------------------------------------------------------------------------
  ! The NUOPC cap for data IPE model which is an interface for SWMF
  !-----------------------------------------------------------------------------

  use ESMF, only: ESMF_GridComp, ESMF_GridCompGet
  use ESMF, only: ESMF_Grid, ESMF_GridWriteVTK, ESMF_GridCreateNoPeriDim
  use ESMF, only: ESMF_GridAddCoord, ESMF_GridGetCoord
  use ESMF, only: ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_LOGMSG_ERROR
  use ESMF, only: ESMF_VM, ESMF_VMGet, ESMF_SUCCESS
  use ESMF, only: ESMF_MAXSTR, ESMF_METHOD_INITIALIZE
  use ESMF, only: ESMF_State, ESMF_StateRemove, ESMF_Clock
  use ESMF, only: ESMF_COORDSYS_CART, ESMF_STAGGERLOC_CORNER
  use ESMF, only: ESMF_StateAdd, ESMF_KIND_R8, ESMF_TYPEKIND_R8
  use ESMF, only: ESMF_FAILURE, ESMF_GridCompSetEntryPoint
  use ESMF, only: ESMF_ArraySpec, ESMF_ArraySpecSet
  use ESMF, only: ESMF_Field, ESMF_FieldCreate, ESMF_FieldGet
  use ESMF, only: ESMF_TimeInterval, ESMF_TimeIntervalGet
  use ESMF, only: ESMF_Clock, ESMF_ClockGet

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
  use NUOPC_Model, only: model_label_DataInitialize => label_DataInitialize
  use NUOPC_Model, only: model_label_Advance => label_Advance
  use NUOPC_Model, only: model_label_Finalize => label_Finalize

  use SWMF_shared, only: ChkErr
  use SWMF_shared, only: configType, exchType

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
  private :: ModelExport         ! Updates export state
  private :: ModelAdvance        ! Advance the model
  private :: ModelFinalize       ! Finalize the model 

  !-----------------------------------------------------------------------------
  ! Private module data
  !-----------------------------------------------------------------------------

  type(configType) :: config
  type(exchType) :: exportFields(2)

  ! ESMF "dynamo" grid and domain. The real grid is not uniform in
  ! latitude, which will be implemented in the near future !!!
  ! This is a 2D spherical grid in MAG coordinates (rotates with Earth):
  ! +Z points to north magnetic dipole, +Y is towards rotational Omega x Z

  integer, parameter:: nLon=81, nLat=97 ! Default ESMF grid size

  ! Coordinate variables
  real(ESMF_KIND_R8), pointer:: Lon_I(:), Lat_I(:)

  ! IPE dynamo grid latitudes
  real(ESMF_KIND_R8), parameter:: LatIpe_I(nLat) = [ &
    -90.0000, -88.1238, -86.2386, -84.3344, -82.4013, -80.4296, -78.4095, &
    -76.3318, -74.1877, -71.9690, -69.6682, -67.2793, -64.7977, -62.2208, &
    -59.5484, -56.7835, -53.9323, -51.0045, -48.0138, -44.9776, -41.9167, &
    -38.8546, -35.8165, -32.8285, -29.9165, -27.1046, -24.4146, -21.8655, &
    -19.4724, -17.2473, -15.1984, -13.3307, -11.6462, -10.1443,  -8.8219, &
    -7.6733 , -6.6900 ,  -5.8603,  -5.1688,  -4.5959,  -4.1191,  -3.7133, &
    -3.3532 , -3.0142 ,  -2.6728,  -2.3049,  -1.8786,  -1.3276,   0.0000, &
     1.3276 ,  1.8786 ,   2.3049,   2.6728,   3.0142,   3.3532,   3.7133, &
     4.1191 ,  4.5959 ,   5.1688,   5.8603,   6.6900,   7.6733,   8.8219, &
     10.1443, 11.6462 ,  13.3307,  15.1984,  17.2473,  19.4724,  21.8655, &
     24.4146, 27.1046 ,  29.9165,  32.8285,  35.8165,  38.8546,  41.9167, &
     44.9776, 48.0138 ,  51.0045,  53.9323,  56.7835,  59.5484,  62.2208, &
     64.7977, 67.2793 ,  69.6682,  71.9690,  74.1877,  76.3318,  78.4095, &
     80.4296, 82.4013 ,  84.3344,  86.2386,  88.1238,  90.0000 ]

  ! Field values and coordinate coefficients for testing
  real(ESMF_KIND_R8), parameter :: FieldTest_V(2) = [3.0d0, 5.0d0]
  real(ESMF_KIND_R8), parameter :: CoordCoefTest = 0.1d0

  ! Change of Hall field during run
  real(ESMF_KIND_R8), parameter:: dHallPerDtTest = 0.4d0

  ! Coupling frequency
  real(ESMF_KIND_R8) :: couplingFreq

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

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

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

    exportFields(1)%shortName = 'Hall'
    exportFields(2)%shortName = 'Ped'

    !------------------
    ! Advertise export fields
    !------------------

    do n = 1, size(exportFields)
       call NUOPC_Advertise(exportState, standardName=exportFields(n)%shortName, &
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
    integer :: i, n, petCount, Istr, Iend
    logical :: isPresent, isSet
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

    call ESMF_VMGet(vm, petCount=petCount, rc=rc)
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
    write(msg, fmt='(A,I2)') trim(subname)//' : debugLevel = ', config%debugLevel
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)

    !------------------
    ! Create component grid
    !------------------

    ! Create Lon-Lat grid where -180<=Lon<=180-dLon, -90<=Lat<=90
    grid = ESMF_GridCreateNoPeriDim(maxIndex=[nLon-1, nLat-1], &
      regDecomp=[1, petCount], coordDep1=[1], coordDep2=[2], &
      coordSys=ESMF_COORDSYS_CART, name="IPE grid", rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Fill longitude
    call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CORNER, &
      farrayPtr=Lon_I, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    do i = 1, nLon
       Lon_I(i) = (i-1)*(360.0d0/(nLon-1))-180.0d0
    end do

    ! Fill latitude, nonuniform latitude grid
    call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CORNER, &
      farrayPtr=Lat_I, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    Istr = lbound(Lat_I, dim=1)
    Iend = ubound(Lat_I, dim=1)
    Lat_I(:) = LatIpe_I(Istr:Iend)

    ! Output grid for debug purpose
    if (config%debugLevel > 5) then
       call ESMF_GridWrite(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
         filename="IPE_grid_corner", rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

    !------------------
    ! Realize the actively coupled fields
    !------------------

    call ESMF_ArraySpecSet(arraySpec, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Add export fields to the state
    do n = 1, size(exportFields)
       if (NUOPC_IsConnected(exportState, fieldName=trim(exportFields(n)%shortName))) then
          ! Create field
          exportFields(n)%field = ESMF_FieldCreate(grid, arrayspec=arraySpec, &
            staggerloc=ESMF_STAGGERLOC_CORNER, name=trim(exportFields(n)%shortName), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          call ESMF_LogWrite(trim(subname)//" Field = "//trim(exportFields(n)%shortName)// &
            " is connected using grid", ESMF_LOGMSG_INFO)

          ! Realize field 
          call NUOPC_Realize(exportState, field=exportFields(n)%field, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else
          ! Remove field from export state
          call ESMF_StateRemove(exportState, (/ trim(exportFields(n)%shortName) /), rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return

          call ESMF_LogWrite(trim(subname)// " Field = "// trim(exportFields(n)%shortName)// &
            " is not connected and removed from export state.", ESMF_LOGMSG_INFO)
       end if
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine InitializeRealize

  !=============================================================================

  subroutine DataInitialize(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_State) :: exportState
    character(len=*), parameter :: subname = trim(modName)//':(DataInitialize) '
    !---------------------------------------------------------------------------    

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//' called', ESMF_LOGMSG_INFO)

    !------------------
    ! Query component for its export state
    !------------------

    call NUOPC_ModelGet(gcomp, modelClock=clock, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Query clock 
    !------------------

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalGet(timeStep, s_r8=couplingFreq, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Update fields on export state
    !------------------

    call ModelExport(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine DataInitialize

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
    ! Update export state, based on linear function depends on coupling interval
    !------------------

    call ModelExport(gcomp, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return    

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

  subroutine ModelExport(gcomp, rc)

    ! Updates export state of the model component
    ! input/output variables
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: exportState
    integer :: n, i, j
    integer :: Istr, Iend, Jstr, Jend
    logical, save :: firstTime = .true.
    real(ESMF_KIND_R8), pointer :: Ptr_II(:,:)
    character(len=*), parameter :: subname = trim(modName)//':(ModelExport) '
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    !------------------    
    ! Query component for its export state
    !------------------

    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !------------------
    ! Loop over fields and update them
    !------------------

    do n = 1, size(exportFields)
       ! Return field pointer
       call ESMF_FieldGet(exportFields(n)%field, farrayPtr=Ptr_II, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return

       ! Get dimension extents
       Istr = lbound(Ptr_II, dim=1)
       Iend = ubound(Ptr_II, dim=1)
       Jstr = lbound(Ptr_II, dim=2)
       Jend = ubound(Ptr_II, dim=2)
       print*, "Istr, Iend, Jstr, Jend = ", Istr, Iend, Jstr, Jend

       if (firstTime) then ! Data initialization
          ! Fill pointer with scalar data
          select case(trim(exportFields(n)%shortName))
          case('Hall')
             Ptr_II(Istr:Iend,Jstr:Jend) = FieldTest_V(1)
          case('Ped')
             Ptr_II(Istr:Iend,Jstr:Jend) = FieldTest_V(2)
          case default
             call ESMF_LogWrite(subname//' unknown field '// &
               trim(exportFields(n)%shortName), ESMF_LOGMSG_ERROR)
             rc = ESMF_FAILURE
             return
          end select

          ! Add coordinate dependence
          do j = Jstr, Jend
             do i = Istr, Iend
                Ptr_II(i,j) = Ptr_II(i,j)+CoordCoefTest*abs(Lon_I(i))*(90.0d0-abs(Lat_I(j)))
             end do
          end do

          firstTime = .false.
       else ! Advance
          select case(trim(exportFields(n)%shortName))
          case('Hall')
             Ptr_II = Ptr_II+couplingFreq*dHallPerdtTest
          end select
       end if

       nullify(Ptr_II)
    end do

    call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

  end subroutine ModelExport

end module IPE_grid_comp
