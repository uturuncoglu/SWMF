!  Copyright (C) 2002 Regents of the University of Michigan,
!  portions used with permission For more information, see
!  http://csem.engin.umich.edu/tools/swmf 
!============================================================================
module ModUser

  use ModMain, ONLY: nI, nJ,nK
  use ModCoronalHeating, ONLY: PoyntingFluxPerB
  use ModUserEmpty,                                     &
       IMPLEMENTED1 => user_read_inputs,                &
       IMPLEMENTED2 => user_init_session,               &
       IMPLEMENTED3 => user_set_ics,                    &
       IMPLEMENTED4 => user_get_log_var,                &
       IMPLEMENTED5 => user_set_plot_var,               &
       IMPLEMENTED6 => user_set_cell_boundary,          &
       IMPLEMENTED7 => user_set_face_boundary,          &
       IMPLEMENTED8 => user_set_resistivity,            &
       IMPLEMENTED9 => user_initial_perturbation,       &
       IMPLEMENTED10=> user_update_states,              &
       IMPLEMENTED11=> user_get_b0

  include 'user_module.h' !list of public methods

  real, parameter :: VersionUserModule = 1.0
  character (len=*), parameter :: NameUserModule = &
       'AWSoM and AWSoM-R model'

  ! Input parameters for chromospheric inner BC's
  real    :: nChromoSi = 2e17, tChromoSi = 5e4
  real    :: nChromo, RhoChromo, tChromo
  logical :: UseUparBc = .false.

  ! variables for Parker initial condition
  real    :: nCoronaSi = 1.5e14, tCoronaSi = 1.5e6
  real    :: RhoCorona, tCorona

  ! Input parameters for two-temperature effects
  real    :: TeFraction, TiFraction
  real    :: EtaPerpSi

  ! Input parameters for blocking the near-Sun cells to get bigger timestep
  ! in the CME simulation
  real    :: rSteady = 1.125
  logical :: UseSteady = .false.

  ! variables for polar jet application
  ! Dipole unders surface
  real    :: UserDipoleDepth = 1.0
  real    :: UserDipoleStrengthSi = 0.0, UserDipoleStrength = 0.0

  ! Rotating boundary condition
  real:: PeakFlowSpeedFixer =0.0, PeakFlowSpeedFixerSi =0.0
  real:: tBeginJet = 0.0, tEndJet = 0.0
  real:: BminJet=0.0, BminJetSi = 0.0, BmaxJet = 0.0, BmaxJetSi = 0.0
  real:: kbJet = 0.0
  real:: Bramp = 1.0              ! ramp up of magnetic field?
  integer:: iBcMax = 0            ! Index up to which BC is applied
  logical:: FrampStart = .false.
  logical:: UrZero = .false.
  logical:: UpdateWithParker = .true.

contains 
  !============================================================================
  subroutine user_read_inputs

    use ModMain,      ONLY: UseUserInitSession, lVerbose
    use ModProcMH,    ONLY: iProc
    use ModReadParam, ONLY: read_line, read_command, read_var
    use ModIO,        ONLY: write_prefix, write_myname, iUnitOut

    character (len=100) :: NameCommand

    character(len=*), parameter :: NameSub = 'user_read_inputs'
    !--------------------------------------------------------------------------
    UseUserInitSession = .true.

    if(iProc == 0 .and. lVerbose > 0)then
       call write_prefix;
       write(iUnitOut,*)'User read_input CHROMOSPHERE-CORONA starts'
    endif

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE

       select case(NameCommand)
          ! This command is used when the inner boundary is the chromosphere   
       case("#CHROMOBC")
          call read_var('nChromoSi', nChromoSi)
          call read_var('tChromoSi', tChromoSi)

       case("#LINETIEDBC")
          call read_var('UseUparBc', UseUparBc)

       case("#PARKERIC")
          call read_var('nCoronaSi', nCoronaSi)
          call read_var('tCoronaSi', tCoronaSi)

       case("#LOWCORONASTEADY")
          call read_var('UseSteady', UseSteady)
          if(UseSteady) call read_var('rSteady', rSteady)

       case("#POLARJETDIPOLE")
          call read_var('UserDipoleDepth', UserDipoleDepth)
          call read_var('UserDipoleStrengthSi', UserDipoleStrengthSi)
          
       case('#POLARJETBOUNDARY')
          call read_var('PeakFlowSpeedFixerSi',PeakFlowSpeedFixerSi)
          call read_var('tBeginJet', tBeginJet)
          call read_var('tEndJet',   tEndJet)
          call read_var('BminJetSi', BminJetSi)
          call read_var('BmaxJetSi', BmaxJetSi)
          call read_var('kbJet',     kbJet)
          call read_var('iBcMax',    iBcMax)
          call read_var('FrampStart', FrampStart)
          call read_var('UrZero', UrZero)
          call read_var('UpdateWithParker', UpdateWithParker)
          call read_var('Bramp', Bramp)

       case('#USERINPUTEND')
          if(iProc == 0 .and. lVerbose > 0)then
             call write_prefix;
             write(iUnitOut,*)'User read_input SOLAR CORONA ends'
          endif
          EXIT

       case default
          if(iProc == 0) then
             call write_myname; write(*,*) &
                  'ERROR: Invalid user defined #COMMAND in user_read_inputs. '
             write(*,*) '--Check user_read_inputs for errors'
             write(*,*) '--Check to make sure a #USERINPUTEND command was used'
             write(*,*) '  *Unrecognized command was: '//NameCommand
             call stop_mpi('ERROR: Correct PARAM.in or user_read_inputs!')
          end if
       end select
    end do

  end subroutine user_read_inputs
  !============================================================================
  subroutine user_init_session

    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,   ONLY: EEE_initialize
    use ModProcMH,     ONLY: iProc
    use ModIO,         ONLY: write_prefix, iUnitOut
    use ModWaves,      ONLY: UseWavePressure, UseAlfvenWaves
    use ModAdvance,    ONLY: UseElectronPressure
    use ModVarIndexes, ONLY: WaveFirst_
    use ModMultiFluid, ONLY: MassIon_I
    use ModConst,      ONLY: cElectronCharge, cLightSpeed, cBoltzmann, cEps, &
         cElectronMass
    use ModNumConst,   ONLY: cTwoPi
    use ModPhysics,    ONLY: ElectronTemperatureRatio, AverageIonCharge, &
         Si2No_V, UnitTemperature_, UnitN_, UnitB_, BodyNDim_I, BodyTDim_I, &
         UnitX_, UnitT_, Gamma

    real, parameter :: CoulombLog = 20.0
    character (len=*),parameter :: NameSub = 'user_init_session'
    !--------------------------------------------------------------------------
    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session:'
       call write_prefix; write(iUnitOut,*) ''
    end if

    UseAlfvenWaves  = WaveFirst_ > 1
    UseWavePressure = WaveFirst_ > 1

    ! convert to normalized units
    nChromo = nChromoSi*Si2No_V(UnitN_)
    RhoChromo = nChromo*MassIon_I(1)
    tChromo = tChromoSi*Si2No_V(UnitTemperature_)

    ! Density and temperature in normalized units
    RhoCorona = nCoronaSi*Si2No_V(UnitN_)*MassIon_I(1)
    tCorona   = tCoronaSi*Si2No_V(UnitTemperature_)

    ! polar jet in normalized units
    UserDipoleStrength = UserDipoleStrengthSi*Si2No_V(UnitB_)
    BminJet = BminJetSi*Si2No_V(UnitB_)
    BmaxJet = BmaxJetSi*Si2No_V(UnitB_)
    PeakFlowSpeedFixer = PeakFlowSpeedFixerSi &
         * Si2No_V(UnitX_)**2 / Si2No_V(UnitT_)/Si2No_V(UnitB_)

    ! TeFraction is used for ideal EOS:
    if(UseElectronPressure)then
       ! Pe = ne*Te (dimensionless) and n=rho/ionmass
       ! so that Pe = ne/n *n*Te = (ne/n)*(rho/ionmass)*Te
       ! TeFraction is defined such that Te = Pe/rho * TeFraction
       TiFraction = MassIon_I(1)
       TeFraction = MassIon_I(1)/AverageIonCharge
    else
       ! p = n*T + ne*Te (dimensionless) and n=rho/ionmass
       ! so that p=rho/massion *T*(1+ne/n Te/T)
       ! TeFraction is defined such that Te = p/rho * TeFraction
       TiFraction = MassIon_I(1) &
            /(1 + AverageIonCharge*ElectronTemperatureRatio)
       TeFraction = TiFraction*ElectronTemperatureRatio
    end if

    ! perpendicular resistivity, used for temperature relaxation
    ! Note EtaPerpSi is divided by cMu.
    EtaPerpSi = sqrt(cElectronMass)*CoulombLog &
         *(cElectronCharge*cLightSpeed)**2/(3*(cTwoPi*cBoltzmann)**1.5*cEps)

    if(UseCme) call EEE_initialize(BodyNDim_I(1), BodyTDim_I(1), Gamma)

    if(iProc == 0)then
       call write_prefix; write(iUnitOut,*) ''
       call write_prefix; write(iUnitOut,*) 'user_init_session finished'
       call write_prefix; write(iUnitOut,*) ''
    end if

  end subroutine user_init_session
  !============================================================================
  subroutine user_set_ics(iBlock)

    ! The isothermal parker wind solution is used as initial condition

    use ModAdvance,    ONLY: State_VGB, UseElectronPressure, UseAnisoPressure
    use ModB0,         ONLY: B0_DGB
    use ModCoronalHeating, ONLY: UseTurbulentCascade
    use ModGeometry,   ONLY: Xyz_DGB, r_Blk
    use ModMultiFluid, ONLY: MassIon_I
    use ModPhysics,    ONLY: rBody, GBody, AverageIonCharge
    use ModVarIndexes, ONLY: Rho_, RhoUx_, RhoUy_, RhoUz_, Bx_, Bz_, p_, Pe_, &
         Ppar_, WaveFirst_, WaveLast_
    use ModWaves, ONLY: UseAlfvenWaves

    integer, intent(in) :: iBlock

    integer :: i, j, k
    real :: x, y, z, r, Rho, NumDensIon, NumDensElectron
    real :: uCorona
    real :: r_D(3), Br
    ! variables for iterative Parker solution
    integer :: IterCount
    real :: Ur, Ur0, Ur1, del, rTransonic, Uescape, Usound

    real, parameter :: Epsilon = 1.0e-6
    character (len=*), parameter :: NameSub = 'user_set_ics'
    !--------------------------------------------------------------------------

    ! normalize with isothermal sound speed.
    Usound  = sqrt(tCorona*(1.0 + AverageIonCharge)/MassIon_I(1))
    Uescape = sqrt(-GBody*2.0)/Usound

    !\
    ! Initialize MHD wind with Parker's solution
    ! construct solution which obeys
    !   rho x u_r x r^2 = constant
    !/
    rTransonic = 0.25*Uescape**2
    if(.not.(rTransonic>exp(1.0)))then
       write(*,*) NameSub, 'Gbody=', Gbody
       write(*,*) NameSub,' nCoronaSi, RhoCorona =', NcoronaSi, RhoCorona
       write(*,*) NameSub,' TcoronaSi, Tcorona   =', TcoronaSi, Tcorona
       write(*,*) NameSub,' Usound    =', Usound
       write(*,*) NameSub,' Uescape   =', Uescape
       write(*,*) NameSub,' rTransonic=', rTransonic
       call stop_mpi(NameSub//'sonic point inside Sun')
    end if

    uCorona = rTransonic**2*exp(1.5 - 2.0*rTransonic)

    do k = MinK,MaxK ; do j = MinJ,MaxJ ; do i = MinI,MaxI
       x = Xyz_DGB(x_,i,j,k,iBlock)
       y = Xyz_DGB(y_,i,j,k,iBlock)
       z = Xyz_DGB(z_,i,j,k,iBlock)
       r = r_BLK(i,j,k,iBlock)
       r_D = (/x,y,z/)

       if(r > rTransonic)then
          !\
          ! Inside supersonic region
          !/
          Ur0 = 1.0
          IterCount = 0
          do
             IterCount = IterCount + 1
             Ur1 = sqrt(Uescape**2/r - 3.0 + 2.0*log(16.0*Ur0*r**2/Uescape**4))
             del = abs(Ur1 - Ur0)
             if(del < Epsilon)then
                Ur = Ur1
                EXIT
             elseif(IterCount < 1000)then
                Ur0 = Ur1
                CYCLE
             else
                call stop_mpi('PARKER > 1000 it.')
             end if
          end do
       else
          !\
          ! Inside subsonic region
          !/
          Ur0 = 1.0
          IterCount = 0
          do
             IterCount = IterCount + 1
             Ur1 = (Uescape**2/(4.0*r))**2 &
                  *exp(0.5*(Ur0**2 + 3.0 - Uescape**2/r))
             del = abs(Ur1 - Ur0)
             if(del < Epsilon)then
                Ur = Ur1
                EXIT
             elseif(IterCount < 1000)then
                Ur0 = Ur1
                CYCLE
             else
                call CON_stop('PARKER > 1000 it.')
             end if
          end do
       end if

       Rho = rBody**2*RhoCorona*uCorona/(r**2*Ur)

       NumDensIon = Rho/MassIon_I(1)
       NumDensElectron = NumDensIon*AverageIonCharge

       if(UseElectronPressure)then
          State_VGB(p_,i,j,k,iBlock) = NumDensIon*tCorona
          State_VGB(Pe_,i,j,k,iBlock) = NumDensElectron*tCorona
          if(UseAnisoPressure) &
               State_VGB(Ppar_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock)
       else
          State_VGB(p_,i,j,k,iBlock) = (NumDensIon + NumDensElectron)*tCorona
       end if
       State_VGB(Rho_,i,j,k,iBlock) = Rho
        
       State_VGB(RhoUx_,i,j,k,iBlock) = Rho*Ur*x/r *Usound
       State_VGB(RhoUy_,i,j,k,iBlock) = Rho*Ur*y/r *Usound
       State_VGB(RhoUz_,i,j,k,iBlock) = Rho*Ur*z/r *Usound

       State_VGB(Bx_:Bz_,i,j,k,iBlock) = 0.0

       if(UseAlfvenWaves)then
          Br = sum(B0_DGB(1:3,i,j,k,iBlock)*r_D)
          if (Br >= 0.0) then
             State_VGB(WaveFirst_,i,j,k,iBlock) =  &
                  PoyntingFluxPerB*sqrt(State_VGB(rho_,i,j,k,iBlock))
             if(UseTurbulentCascade)then
                State_VGB(WaveLast_,i,j,k,iBlock) = &
                     1e-4*State_VGB(WaveFirst_,i,j,k,iBlock)
             else
                State_VGB(WaveLast_,i,j,k,iBlock) = 1e-30
             end if
          else
             State_VGB(WaveLast_,i,j,k,iBlock) =  &
                  PoyntingFluxPerB*sqrt(State_VGB(rho_,i,j,k,iBlock))
             if(UseTurbulentCascade)then
                State_VGB(WaveFirst_,i,j,k,iBlock) = &
                     1e-4*State_VGB(WaveLast_,i,j,k,iBlock)
             else
                State_VGB(WaveFirst_,i,j,k,iBlock) = 1e-30
             end if
          end if
       end if

    end do; end do; end do

  end subroutine user_set_ics
  !============================================================================
  subroutine user_get_log_var(VarValue, TypeVar, Radius)

    use ModAdvance,    ONLY: State_VGB, tmp1_BLK, UseElectronPressure
    use ModB0,         ONLY: B0_DGB
    use ModIO,         ONLY: write_myname
    use ModMain,       ONLY: Unused_B, nBlock, x_, y_, z_, UseB0
    use ModPhysics,    ONLY: InvGammaMinus1, No2Io_V, UnitEnergydens_, UnitX_
    use ModVarIndexes, ONLY: Bx_, By_, Bz_, p_, Pe_

    real, intent(out) :: VarValue
    character(len=10), intent(in) :: TypeVar 
    real, optional, intent(in) :: Radius

    integer :: iBlock
    real :: unit_energy
    real, external :: integrate_BLK
    !--------------------------------------------------------------------------
    unit_energy = No2Io_V(UnitEnergydens_)*No2Io_V(UnitX_)**3
    !\
    ! Define log variable to be saved::
    !/
    select case(TypeVar)
    case('eint')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(UseElectronPressure)then
             tmp1_BLK(:,:,:,iBlock) = &
                  State_VGB(p_,:,:,:,iBlock) + State_VGB(Pe_,:,:,:,iBlock)
          else
             tmp1_BLK(:,:,:,iBlock) = State_VGB(p_,:,:,:,iBlock)
          end if
       end do
       VarValue = unit_energy*InvGammaMinus1*integrate_BLK(1,tmp1_BLK)

    case('emag')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE
          if(UseB0)then
             tmp1_BLK(:,:,:,iBlock) = & 
                  ( B0_DGB(x_,:,:,:,iBlock) + State_VGB(Bx_,:,:,:,iBlock))**2 &
                  +(B0_DGB(y_,:,:,:,iBlock) + State_VGB(By_,:,:,:,iBlock))**2 &
                  +(B0_DGB(z_,:,:,:,iBlock) + State_VGB(Bz_,:,:,:,iBlock))**2
          else
             tmp1_BLK(:,:,:,iBlock) = State_VGB(Bx_,:,:,:,iBlock)**2 &
                  + State_VGB(By_,:,:,:,iBlock)**2 &
                  + State_VGB(Bz_,:,:,:,iBlock)**2
          end if
       end do
       VarValue = unit_energy*0.5*integrate_BLK(1,tmp1_BLK)

    case('vol')
       do iBlock = 1, nBlock
          if(Unused_B(iBlock)) CYCLE

          tmp1_BLK(:,:,:,iBlock) = 1.0
       end do
       VarValue = integrate_BLK(1,tmp1_BLK)

    case default
       VarValue = -7777.
       call write_myname;
       write(*,*) 'Warning in set_user_logvar: unknown logvarname = ',TypeVar
    end select

  end subroutine user_get_log_var

  !============================================================================
  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModAdvance,    ONLY: State_VGB, UseElectronPressure, &
         UseAnisoPressure, Source_VC
    use ModChromosphere, ONLY: DoExtendTransitionRegion, extension_factor, &
         get_tesi_c, TeSi_C
    use ModCoronalHeating, ONLY: get_block_heating, CoronalHeating_C, &
         apportion_coronal_heating, IsNewBlockAlfven, get_wave_reflection
    use ModPhysics,    ONLY: No2Si_V, UnitTemperature_, UnitEnergyDens_, UnitT_
    use ModRadiativeCooling, ONLY: RadCooling_C, get_radiative_cooling
    use ModVarIndexes, ONLY: Rho_, p_, Pe_, WaveFirst_, WaveLast_
    use ModFaceValue, ONLY: calc_face_value
    use ModB0, ONLY: set_b0_face
    use ModMultiFluid, ONLY: IonFirst_, IonLast_

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(out)  :: PlotVar_G(MinI:MaxI, MinJ:MaxJ, MinK:MaxK)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    integer :: i, j, k
    real :: QPerQtotal_I(IonFirst_:IonLast_)
    real :: QparPerQtotal_I(IonFirst_:IonLast_)
    real :: QePerQtotal

    character (len=*), parameter :: NameSub = 'user_set_plot_var'
    !--------------------------------------------------------------------------
    IsFound = .true.

    select case(NameVar)
    case('te')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
          if(UseElectronPressure)then
             PlotVar_G(i,j,k) = TeFraction*State_VGB(Pe_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
          else
             PlotVar_G(i,j,k) = TeFraction*State_VGB(p_,i,j,k,iBlock) &
                  /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
          end if
       end do; end do; end do

    case('ti')
       NameIdlUnit = 'K'
       NameTecUnit = '[K]'
       do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
          PlotVar_G(i,j,k) = TiFraction*State_VGB(p_,i,j,k,iBlock) &
               /State_VGB(Rho_,i,j,k,iBlock)*No2Si_V(UnitTemperature_)
       end do; end do; end do

    case('qrad')
       call get_tesi_c(iBlock, TeSi_C)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          call get_radiative_cooling(i, j, k, iBlock, TeSi_C(i,j,k), &
               RadCooling_C(i,j,k))
          PlotVar_G(i,j,k) = RadCooling_C(i,j,k) &
               *No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('refl')
       Source_VC(WaveFirst_:WaveLast_,:,:,:) = 0.0
       call set_b0_face(iBlock)
       call calc_face_value(.false., iBlock)
       IsNewBlockAlfven = .true.
       call get_wave_reflection(iBlock)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = Source_VC(WaveLast_,i,j,k) &
               /sqrt(State_VGB(WaveFirst_,i,j,k,iBlock) &
               *     State_VGB(WaveLast_,i,j,k,iBlock))/No2Si_V(UnitT_)
          Source_VC(WaveFirst_:WaveLast_,i,j,k) = 0.0
       end do; end do; end do
       NameIdlUnit = '1/s'
       NameTecUnit = '1/s'

    case('qheat')
       ! some of the heating terms need face values
       call set_b0_face(iBlock)
       call calc_face_value(.false., iBlock)
       call get_block_heating(iBlock)
       call get_tesi_c(iBlock, TeSi_C)
       do k = 1, nK; do j = 1, nJ; do i = 1, nI
          PlotVar_G(i,j,k) = CoronalHeating_C(i,j,k) &
               /extension_factor(TeSi_C(i,j,k)) &
               *No2Si_V(UnitEnergyDens_)/No2Si_V(UnitT_)
       end do; end do; end do
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case('qebyq', 'qparbyq')
       if(UseElectronPressure)then
          call set_b0_face(iBlock)
          call calc_face_value(.false., iBlock)
          call get_block_heating(iBlock)
          if(DoExtendTransitionRegion) call get_tesi_c(iBlock, TeSi_C)
          do k = 1, nK; do j = 1, nJ; do i = 1, nI
             if(DoExtendTransitionRegion) CoronalHeating_C(i,j,k) = &
                  CoronalHeating_C(i,j,k)/extension_factor(TeSi_C(i,j,k))
             call apportion_coronal_heating(i, j, k, iBlock, &
                  CoronalHeating_C(i,j,k), QPerQtotal_I, QparPerQtotal_I, &
                  QePerQtotal)
             select case(NameVar)
             case('qebyq')
                PlotVar_G(i,j,k) = QePerQtotal
             case('qparbyq')
                if(UseAnisoPressure) &
                     PlotVar_G(i,j,k) = QparPerQtotal_I(IonFirst_)
             end select
          end do; end do; end do
       end if
       NameIdlUnit = 'J/m^3/s'
       NameTecUnit = 'J/m^3/s'

    case default
       IsFound = .false.
    end select

    UsePlotVarBody = .false.
    PlotVarBody    = 0.0

  end subroutine user_set_plot_var
  !============================================================================
  subroutine user_set_cell_boundary(iBlock,iSide, TypeBc, IsFound)

    ! Fill ghost cells inside body for spherical grid - this subroutine only 
    ! modifies ghost cells in the r direction

    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,            ONLY: EEE_get_state_BC
    use ModAdvance,    ONLY: State_VGB, UseElectronPressure
    use ModGeometry,   ONLY: TypeGeometry, Xyz_DGB, r_BLK
    use ModVarIndexes, ONLY: Rho_, p_, Pe_, Bx_, Bz_, &
         RhoUx_,RhoUz_
    use ModMultiFluid, ONLY: MassIon_I
    use ModImplicit,   ONLY: StateSemi_VGB, iTeImpl
    use ModPhysics,    ONLY: AverageIonCharge, UnitRho_, UnitB_, UnitP_, &
         Si2No_V, rBody, GBody
    use ModMain,       ONLY: n_step, iteration_number, time_simulation, &
         time_accurate, ProcTest, BlkTest
    use ModB0,         ONLY: B0_DGB
    use BATL_lib,      ONLY: CellSize_DB, Phi_, Theta_, x_, y_
    use ModCoordTransform, ONLY: rot_xyz_sph
    use ModConst,      ONLY: cPi
    use ModIO,         ONLY : restart
    use ModProcMH,     ONLY: iProc

    integer,          intent(in)  :: iBlock, iSide
    character(len=*), intent(in)  :: TypeBc
    logical,          intent(out) :: IsFound

    integer :: i, j, k
    real    :: Br1_D(3), Bt1_D(3)
    real    :: Runit_D(3)
    real    :: RhoCme, Ucme_D(3), Bcme_D(3), pCme, BrCme, BrCme_D(3)


    real    :: Br_II(MinJ:MaxJ,MinK:MaxK)
    real    :: Uphi, Ulat
    real    :: r, r1, r2Inv

    real    :: NumDensIon, NumDensElectron

    real    :: Framp = 1.0

    real    :: Dlat, Dphi, dBrDphi, dBrDlat, Br, Rho, p, Ur, rCosLat, uCoeff 
    real    :: XyzSph_DD(3,3), u_D(3), Xyz1_D(3), bFace_D(3), b_D(3)

    real    :: Scale, H

    real    :: Usound, Uescape, rTransonic, Ur0, Ur1, del
    real    :: uCorona

    real    :: DiffDelta, Ur2

    real, parameter :: Epsilon = 1.0e-6
    integer :: Itercount

    logical:: DoTest, DoTestMe
    character (len=*), parameter :: NameSub = 'user_set_cell_boundary'
    !--------------------------------------------------------------------------
    if(iSide /= 1 .or. TypeGeometry(1:9) /='spherical') &
         call CON_stop('Wrong iSide in user_set_cell_boundary')

    if(iProc == ProcTest .and. iBlock == BlkTest)then
       call set_oktest(NameSub, DoTest, DoTestMe)
    else
       DoTest = .false.; DoTestMe = .false.
    end if

    if(DoTestMe) write(*,*) NameSub,'!!! starting with UpdateWithParker=',&
         UpdateWithParker

    IsFound = .true.

    ! f(t) = 0                                    if      t < t1
    !      = 1                                    if t2 < t
    !      = 0.5 * ( 1- cos(Pi * (t-t1)/(t2-t1))) if t1 < t < t2 
    if(TypeBc == 'usersurfacerot')then

       ! Check if time accurate is set.
       if(time_accurate)then
          if(time_simulation<tBeginJet)then
             Framp = 0.0
          elseif(time_simulation>tEndJet)then
             Framp = 1.0
          else
             Framp = 0.5 * (1 - cos(cPi*(time_simulation-tBeginJet) / &
                  (tEndJet-tBeginJet)))
          endif
       else
          call stop_mpi(NameSub//'Use time accurate mode with usersurfacerot.')
       endif

       Dphi = CellSize_DB(Phi_,iBlock)
       Dlat = CellSize_DB(Theta_,iBlock)

       !
       ! B_r = X*B/r
       !
       do k = MinK, MaxK; do j = MinJ, MaxJ

          r1 = r_BLK(1,j,k,iBlock)
          r2Inv = r1**(-2)
          bFace_D = 0.5*(B0_DGB(:,0,j,k,iBlock) &
               +         B0_DGB(:,1,j,k,iBlock))

          Br_II(j,k) = sum(Xyz_DGB(:,1,j,k,iBlock)*bFace_D)/r1
       end do; end do

       do k = 0, MaxK-1; do j = 0, MaxJ-1

          ! Latitude = arccos(z/r)  
          ! Longitude = arctg(y/x)
          !
          r1 = r_BLK(1,j,k,iBlock)
          Xyz1_D = Xyz_DGB(:,1,j,k,iBlock)
          rCosLat = sqrt(Xyz1_D(x_)**2 + Xyz1_D(y_)**2)
          ! GradBr = (0 ,
          !           1/(r*cos(Theta)) * dB/dPhi , 
          !           1/r              * dB/dTheta )
          !
          dBrDphi = (Br_II(j+1,k) -  Br_II(j-1,k)) / (2*Dphi*rCosLat)
          dBrDlat = (Br_II(j,k+1) -  Br_II(j,k-1)) / (2*Dlat*r1) 

          Br = Br_II(j,k)

          Ur = sum(Xyz1_D*State_VGB(RhoUx_:RhoUz_,1,j,k,iBlock)) / &
               (r1*State_VGB(Rho_,1,j,k,iBlock))          

          ! u_perpendicular = 0  if Br < B1 or Br > B2
          !                 = v0 * f(t) * kb * (B2-B1)/Br * tanh(kb* (Br-B1)/ 
          !                                            (B2-B1)) * (r x GradB)
          !                 = ( 0 , u_Phi, u_Theta)
          !
          if(Br < BminJet .or. Br > BmaxJet)then
             Uphi = 0
             Ulat = 0
          else
             ! Rotation initiation

             if(FrampStart)then
                Framp = 1.0
             endif

             uCoeff = PeakFlowSpeedFixer * Framp * kbJet * (BmaxJet-BminJet)/ &
                  Br * tanh( kbJet * (Br - BminJet)/(BmaxJet-BminJet) )

             ! r x GradB = (1, 0, 0) x (0, GradBPhi, GradBlat) =
             !           = (0,-GradBLat,GradBPhi )
             Uphi = -dBrDlat*uCoeff
             Ulat =  dBrDphi*uCoeff
          endif

          do i = MinI, iBcMax

             r = r_BLK(i,j,k,iBlock)
             ! Convert to Cartesian components
             XyzSph_DD = rot_xyz_sph(Xyz1_D)

             if(UrZero)then
                u_D = matmul(XyzSph_DD, (/max(Ur,0.0), -Ulat, Uphi/) )
             else
                u_D = matmul(XyzSph_DD, (/Ur, -Ulat, Uphi/) )
             endif

             ! Set velocity in first physical cell (i=1)
             if(i > 0)then
                State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                     State_VGB(Rho_,i,j,k,iBlock) * u_D
                CYCLE
             end if

             ! Set density and pressure in ghost cells once (does not change)
             if(iteration_number < 2)then
                !update with Parker solution
                if(UpdateWithParker)then

                   ! normalize with isothermal sound speed.
                   Usound  = sqrt(tCorona*(1.0 + AverageIonCharge) &
                        /MassIon_I(1))
                   Uescape = sqrt(-GBody*2.0)/ Usound

                   rTransonic = 0.25*Uescape**2
                   uCorona = rTransonic**2*exp(1.5 - 2.0*rTransonic)

                   Ur0 = 1.0
                   IterCount = 0
                   do
                      IterCount = IterCount + 1
                      Ur1 = (Uescape**2/(4.0*r))**2 &
                           *exp(0.5*(Ur0**2 + 3.0 - Uescape**2/r))
                      del = abs(Ur1 - Ur0)
                      if(del < Epsilon)then
                         Ur2 = Ur1
                         EXIT
                      elseif(IterCount < 1000)then
                         Ur0 = Ur1
                         CYCLE
                      else
                         call CON_stop('PARKER > 1000 it.')
                      end if
                   end do

                   Rho = rBody**2*RhoCorona*uCorona/(r**2*Ur2)                

                   NumDensIon = Rho/MassIon_I(1)
                   NumDensElectron = NumDensIon*AverageIonCharge

                   p = (NumDensIon + NumDensElectron)*tCorona

                   DiffDelta = 1e-5

                else
                   ! update with scaleheight solution
                   H = abs(tCorona * (1 + AverageIonCharge) / Gbody  ) 
                   Scale = exp(-(r-1) / H)
                   Rho  = RhoCorona*Scale
                   p = tCorona*Rho/MassIon_I(1) *(1 + AverageIonCharge)

                   DiffDelta = 1e-1
                end if

                ! Check for differences relative to the initial solution.
                if(.not.restart .and. &
                     ( p/State_VGB(p_,i,j,k,iBlock)>(1.0+DiffDelta) & 
                     .or. p/State_VGB(p_,i,j,k,iBlock)<(1.0-DiffDelta) &
                     .or. Rho/State_VGB(Rho_,i,j,k,iBlock)>(1.0+DiffDelta) &
                     .or. Rho/State_VGB(Rho_,i,j,k,iBlock)<(1.0-DiffDelta)))&
                     then
                   write(*,*)'n_step=',n_step
                   write(*,*)'i,j,k,iBlock=',i,j,k,iBlock
                   write(*,*)'x,y,z=',Xyz1_D
                   write(*,*)'Rho,ParkerRho=',&
                        Rho,State_VGB(Rho_,i,j,k,iBlock),&
                        Rho/State_VGB(Rho_,i,j,k,iBlock)
                   write(*,*)'P,ParkerP=',&
                        p,State_VGB(p_,i,j,k,iBlock),&
                        p/State_VGB(p_,i,j,k,iBlock)
                   call stop_mpi(NameSub//' BC too far from Parker solution')
                endif
                State_VGB(Rho_,i,j,k,iBlock)  = Rho
                State_VGB(p_,i,j,k,iBlock)    = p
             end if

             ! Set velocity in ghost cells
             State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock) = &
                  State_VGB(Rho_,i,j,k,iBlock)*u_D

             ! Set magnetic field: azimuthal component floats
             b_D = Bramp**(1-i)*State_VGB(Bx_:Bz_,1,j,k,iBlock)
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = b_D - &
                  sum(b_D*Xyz1_D)*Xyz1_D*r2Inv
          end do

       end do; end do

       if(UpdateWithParker .and. iProc==0 .and. n_step <1)then
          write(*,*)'Update with Parkers solutions'
       endif
       if(.not.UpdateWithParker .and. iProc==0 .and. n_step < 1)then
          write(*,*)'Update with scaleheight calculation solution'
       endif

       RETURN
    endif

    !\
    ! The routine is only used by the solver for semi-implicit heat 
    ! conduction along the magnetic field. To calculate the heat
    ! conduction coefficicients, which are averaged over the true cell
    ! and ghost cell, we need to know the magnetic field and temperature
    ! in the ghost cell. To calculate the fux, we also need to set the 
    ! ghost cell temperature within the implicit solver.  
    !/
    if(TypeBc == 'usersemi')then
       StateSemi_VGB(iTeImpl,0,:,:,iBlock) = tChromo
       RETURN
    elseif(TypeBc == 'usersemilinear')then
       RETURN
    end if

    ! The electron heat conduction requires the electron temperature
    ! in the ghost cells
    NumDensIon = RhoChromo/MassIon_I(1)
    NumDensElectron = NumDensIon*AverageIonCharge
    do k = MinK, MaxK; do j = MinJ, MaxJ; do i = MinI, 0
       State_VGB(Rho_,i,j,k,iBlock) = RhoChromo
       if(UseElectronPressure)then
          State_VGB(Pe_,i,j,k,iBlock) = NumDensElectron*tChromo
       else
          State_VGB(p_,i,j,k,iBlock) = (NumDensIon + NumDensElectron)*tChromo
       end if
    end do; end do; end do

    ! The following is only needed for the semi-implicit heat conduction,
    ! which averages the cell centered heat conduction coefficient towards
    ! the face
    do k = MinK,MaxK; do j = MinJ,MaxJ
       Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_BLK(1,j,k,iBlock)

       Br1_D = sum(State_VGB(Bx_:Bz_,1,j,k,iBlock)*Runit_D)*Runit_D
       Bt1_D = State_VGB(Bx_:Bz_,1,j,k,iBlock) - Br1_D

       do i = MinI, 0
          State_VGB(Bx_:Bz_,i,j,k,iBlock) = Bt1_D
       end do

    end do; end do

    if(UseCme)then
       do k = MinK, MaxK; do j = MinJ, MaxJ
          Runit_D = Xyz_DGB(:,1,j,k,iBlock) / r_BLK(1,j,k,iBlock)

          call EEE_get_state_BC(Runit_D, RhoCme, Ucme_D, Bcme_D, pCme, &
               time_simulation, n_step, iteration_number)

          RhoCme = RhoCme*Si2No_V(UnitRho_)
          Bcme_D = Bcme_D*Si2No_V(UnitB_)
          pCme   = pCme*Si2No_V(UnitP_)

          BrCme   = sum(Runit_D*Bcme_D)
          BrCme_D = BrCme*Runit_D

          do i = MinI, 0
             State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock)+RhoCme
             if(UseElectronPressure)then
                State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock) &
                     + 0.5*pCme
             else
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) + pCme
             end if
             State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
                  State_VGB(Bx_:Bz_,i,j,k,iBlock) + BrCme_D
          end do
       end do; end do
    end if

  end subroutine user_set_cell_boundary
  !============================================================================
  subroutine user_set_face_boundary(VarsGhostFace_V)

    use EEE_ModCommonVariables, ONLY: UseCme
    use EEE_ModMain,            ONLY: EEE_get_state_BC
    use ModAdvance,      ONLY: UseElectronPressure, UseAnisoPressure
    use ModFaceBoundary, ONLY: FaceCoords_D, VarsTrueFace_V, B0Face_D, TimeBc
    use ModMain,         ONLY: x_, y_, UseRotatingFrame, n_step, &
         iteration_number
    use ModMultiFluid,   ONLY: MassIon_I
    use ModPhysics,      ONLY: OmegaBody, AverageIonCharge, UnitRho_, &
         UnitP_, UnitB_, UnitU_, Si2No_V, &
         InvGammaMinus1, InvGammaElectronMinus1
    use ModVarIndexes,   ONLY: nVar, Rho_, Ux_, Uy_, Uz_, Bx_, Bz_, p_, &
         WaveFirst_, WaveLast_, Pe_, Ppar_, Hyp_, Ehot_
    use ModHeatFluxCollisionless, ONLY: UseHeatFluxCollisionless, &
         get_gamma_collisionless

    real, intent(out) :: VarsGhostFace_V(nVar)

    integer :: iP
    real :: NumDensIon, NumDensElectron, FullBr, Ewave, Pressure, Temperature
    real :: GammaHere, InvGammaMinus1Fluid
    real,dimension(3) :: U_D, B1_D, B1t_D, B1r_D, rUnit_D

    ! Line-tied related variables
    real              :: RhoTrue, RhoGhost
    real,dimension(3) :: bUnitGhost_D, bUnitTrue_D
    real,dimension(3) :: FullBGhost_D, FullBTrue_D

    ! CME related variables
    real :: RhoCme, Ucme_D(3), Bcme_D(3), pCme
    real :: BrCme, BrCme_D(3), UrCme, UrCme_D(3), UtCme_D(3)

    character (len=*), parameter :: NameSub = 'user_set_face_boundary'
    !--------------------------------------------------------------------------

    rUnit_D = FaceCoords_D/sqrt(sum(FaceCoords_D**2))
    !\
    ! Magnetic field: radial magnetic field is set to zero, the other are floating
    ! Density is fixed,
    !/
    B1_D  = VarsTrueFace_V(Bx_:Bz_)
    B1r_D = sum(rUnit_D*B1_D)*rUnit_D
    B1t_D = B1_D - B1r_D
    VarsGhostFace_V(Bx_:Bz_) = B1t_D

    ! Fix density
    VarsGhostFace_V(Rho_) = RhoChromo

    !\
    ! The perpendicular to the mf components of velocity are reflected.
    ! The parallel to the field velocity is either floating or reflected
    !/
    if (UseUparBc) then
       ! Use line-tied boundary conditions
       U_D = VarsTrueFace_V(Ux_:Uz_)

       RhoTrue = VarsTrueFace_V(Rho_)
       RhoGhost = VarsGhostFace_V(Rho_)
       FullBGhost_D = B0Face_D + VarsGhostFace_V(Bx_:Bz_)
       FullBTrue_D  = B0Face_D + VarsTrueFace_V(Bx_:Bz_)

       bUnitGhost_D = FullBGhost_D/sqrt(max(1e-30,sum(FullBGhost_D**2)))
       bUnitTrue_D = FullBTrue_D/sqrt(max(1e-30,sum(FullBTrue_D**2)))

       ! Extrapolate field-aligned velocity component to satisfy
       ! the induction equation under steady state conditions.
       ! The density ratio is to satisfy mass conservation along the field
       ! line as well.
       VarsGhostFace_V(Ux_:Uz_) = RhoTrue/RhoGhost* &
            sum(U_D*bUnitTrue_D)*bUnitGhost_D
    else
       ! zero velocity at inner boundary
       VarsGhostFace_V(Ux_:Uz_) = -VarsTrueFace_V(Ux_:Uz_)
    end if

    ! Apply corotation if needed
    if(.not.UseRotatingFrame)then                                            
       VarsGhostFace_V(Ux_) = VarsGhostFace_V(Ux_)-2*OmegaBody*FaceCoords_D(y_)
       VarsGhostFace_V(Uy_) = VarsGhostFace_V(Uy_)+2*OmegaBody*FaceCoords_D(x_)
    end if
    !\
    ! Temperature is fixed 
    !/

    Temperature = tChromo

    !\
    ! If the CME is applied, we modify: density, temperature, magnetic field
    !/
    if(UseCme)then
       call EEE_get_state_BC(Runit_D, RhoCme, Ucme_D, Bcme_D, pCme, TimeBc, &
            n_step, iteration_number)

       RhoCme = RhoCme*Si2No_V(UnitRho_)
       Ucme_D = Ucme_D*Si2No_V(UnitU_)
       Bcme_D = Bcme_D*Si2No_V(UnitB_)
       pCme   = pCme*Si2No_V(UnitP_)

       ! Add CME density
       VarsGhostFace_V(Rho_) = VarsGhostFace_V(Rho_) + RhoCme

       ! Fix the normal component of the CME field to BrCme_D at the Sun
       BrCme   = sum(Runit_D*Bcme_D)
       BrCme_D = BrCme*Runit_D
       VarsGhostFace_V(Bx_:Bz_) = VarsGhostFace_V(Bx_:Bz_) + BrCme_D

       ! Fix the tangential components of the CME velocity at the Sun
       UrCme   = sum(Runit_D*Ucme_D)
       UrCme_D = UrCme*Runit_D
       UtCme_D = UCme_D - UrCme_D
       VarsGhostFace_V(Ux_:Uz_) = VarsGhostFace_V(Ux_:Uz_) + 2*UtCme_D

       Pressure = RhoChromo/MassIon_I(1)*(1 + AverageIonCharge)*tChromo
       Temperature = (Pressure + pCme) &
            / (VarsGhostFace_V(Rho_)/MassIon_I(1)*(1 + AverageIonCharge))
    end if

    FullBr = sum((B0Face_D + VarsGhostFace_V(Bx_:Bz_))*rUnit_D)

    ! Ewave \propto sqrt(rho) for U << Ualfven
    Ewave = PoyntingFluxPerB*sqrt(VarsGhostFace_V(Rho_))
    if (FullBr > 0. ) then
       VarsGhostFace_V(WaveFirst_) = Ewave
       VarsGhostFace_V(WaveLast_) = 0.0
    else
       VarsGhostFace_V(WaveFirst_) = 0.0
       VarsGhostFace_V(WaveLast_) = Ewave
    end if

    ! Fix temperature
    NumDensIon = VarsGhostFace_V(Rho_)/MassIon_I(1)
    NumDensElectron = NumDensIon*AverageIonCharge
    if(UseElectronPressure)then
       VarsGhostFace_V(p_) = NumDensIon*Temperature
       VarsGhostFace_V(Pe_) = NumDensElectron*Temperature
       if(UseAnisoPressure) VarsGhostFace_V(Ppar_) = VarsGhostFace_V(p_)
    else
       VarsGhostFace_V(p_) = (NumDensIon + NumDensElectron)*Temperature
    end if

    if(Hyp_>1) VarsGhostFace_V(Hyp_) = VarsTrueFace_V(Hyp_)

    if(Ehot_ > 1)then
       if(UseHeatFluxCollisionless)then
          call get_gamma_collisionless(FaceCoords_D, GammaHere)
          if(UseElectronPressure) then
             iP = Pe_
             InvGammaMinus1Fluid = InvGammaElectronMinus1
          else
             iP = p_
             InvGammaMinus1Fluid = InvGammaMinus1
          end if
          VarsGhostFace_V(Ehot_) = &
               VarsGhostFace_V(iP)*(1.0/(GammaHere - 1) - InvGammaMinus1Fluid)
       else
          VarsGhostFace_V(Ehot_) = 0.0
       end if
    end if

  end subroutine user_set_face_boundary
  !============================================================================
  subroutine user_set_resistivity(iBlock, Eta_G)

    use ModAdvance,    ONLY: State_VGB
    use ModPhysics,    ONLY: No2Si_V, Si2No_V, UnitTemperature_, UnitX_, UnitT_
    use ModVarIndexes, ONLY: Rho_, Pe_

    integer, intent(in) :: iBlock
    real,    intent(out):: Eta_G(MinI:MaxI,MinJ:MaxJ,MinK:MaxK)

    integer :: i, j, k
    real :: Te, TeSi

    character (len=*), parameter :: NameSub = 'user_set_resistivity'
    !--------------------------------------------------------------------------

    do k = MinK,MaxK; do j = MinJ,MaxJ; do i = MinI,MaxI
       Te = TeFraction*State_VGB(Pe_,i,j,k,iBlock)/State_VGB(Rho_,i,j,k,iBlock)
       TeSi = Te*No2Si_V(UnitTemperature_)

       Eta_G(i,j,k) = EtaPerpSi/TeSi**1.5 *Si2No_V(UnitX_)**2/Si2No_V(UnitT_)
    end do; end do; end do

  end subroutine user_set_resistivity
  !============================================================================
  subroutine user_initial_perturbation

    use EEE_ModMain,  ONLY: EEE_get_state_init
    use ModMain, ONLY: nI, nJ, nK, nBLK, unused_B, n_step, iteration_number
    use ModVarIndexes
    use ModAdvance,   ONLY: State_VGB, UseElectronPressure
    use ModPhysics,   ONLY: Si2No_V, UnitRho_, UnitP_, UnitB_
    use ModGeometry,  ONLY: Xyz_DGB
    use ModEnergy,    ONLY: calc_energy_cell
    use BATL_lib,     ONLY: nDim, MaxDim

    integer :: i, j, k, iBlock
    logical :: oktest, oktest_me
    real :: x_D(nDim), Rho, B_D(MaxDim), p

    character (len=*), parameter :: NameSub = 'user_initial_perturbation'
    ! -------------------------------------------------------------------------
    call set_oktest('user_initial_perturbation',oktest,oktest_me)

    do iBlock = 1, nBLK
       if(unused_B(iBlock))CYCLE

       do k = 1, nK; do j = 1, nJ; do i = 1, nI

          x_D = Xyz_DGB(:,i,j,k,iBlock)

          call EEE_get_state_init(x_D, Rho, B_D, p, n_step, iteration_number)

          Rho = Rho*Si2No_V(UnitRho_)
          B_D = B_D*Si2No_V(UnitB_)
          p = p*Si2No_V(UnitP_)

          !\
          ! Add the eruptive event state to the solar wind
          !/
          if(State_VGB(Rho_,i,j,k,iBlock) + Rho < 0.25*State_VGB(Rho_,i,j,k,iBlock))then
             State_VGB(Rho_,i,j,k,iBlock) = 0.25*State_VGB(Rho_,i,j,k,iBlock)
          else
             State_VGB(Rho_,i,j,k,iBlock) = State_VGB(Rho_,i,j,k,iBlock) + Rho
          endif

          State_VGB(Bx_:Bz_,i,j,k,iBlock) = &
               State_VGB(Bx_:Bz_,i,j,k,iBlock) + B_D

          if(UseElectronPressure)then
             if(State_VGB(Pe_,i,j,k,iBlock) + 0.5*p < 0.25*State_VGB(Pe_,i,j,k,iBlock))then
                State_VGB(Pe_,i,j,k,iBlock) = 0.25*State_VGB(Pe_,i,j,k,iBlock)
             else
                State_VGB(Pe_,i,j,k,iBlock) = State_VGB(Pe_,i,j,k,iBlock) + 0.5*p
             endif

             if(State_VGB(p_,i,j,k,iBlock)  + 0.5*p < 0.25*State_VGB(p_,i,j,k,iBlock))then
                State_VGB(p_,i,j,k,iBlock) = 0.25*State_VGB(p_,i,j,k,iBlock)
             else
                State_VGB(p_,i,j,k,iBlock)  = State_VGB(p_,i,j,k,iBlock)  + 0.5*p
             endif

          else
             if(State_VGB(p_,i,j,k,iBlock) + p < 0.25*State_VGB(p_,i,j,k,iBlock))then
                State_VGB(p_,i,j,k,iBlock) = 0.25*State_VGB(p_,i,j,k,iBlock)
             else
                State_VGB(p_,i,j,k,iBlock) = State_VGB(p_,i,j,k,iBlock) + p
             endif
          end if
       end do; end do; end do

       call calc_energy_cell(iBlock)

    end do

  end subroutine user_initial_perturbation

  !============================================================================

  subroutine user_update_states(iStage, iBlock)

    use ModGeometry, ONLY: true_cell, R_BLK, true_BLK

    integer,intent(in):: iStage,iBlock
    !--------------------------------------------------------------------------

    if(UseSteady)then
       if(minval(R_BLK(1:nI,1:nJ,1:nK,iBlock)) <= rSteady)then
          true_cell(1:nI,1:nJ,1:nK,iBlock) = .false.
          true_BLK(iBlock) = .false.
       end if
    end if
    call update_states_MHD(iStage, iBlock)

  end subroutine user_update_states

  !===================================================================== 
  subroutine user_get_b0(x, y, z, B0_D)

    use ModPhysics, ONLY: MonopoleStrength

    real, intent(in) :: x, y, z
    real, intent(out):: B0_D(3)

    real :: r,Xyz_D(3), Dp, rInv, r2Inv, r3Inv, Dipole_D(3)
    real :: B0_Dm(3)


    character(len=*), parameter :: NameSub = 'user_get_b0'
    !-------------------------------------------------------------------        
    Xyz_D = (/x, y, z/)
    r = sqrt(sum(Xyz_D**2))
    B0_Dm = MonopoleStrength*Xyz_D/r**3
    ! shifted Xyz_D upwards to center of the user-dipole

    Xyz_D = (/x, y-1.0+UserDipoleDepth, z/)
    ! Determine radial distance and powers of it
    rInv  = 1.0/sqrt(sum(Xyz_D**2))
    r2Inv = rInv**2
    r3Inv = rInv*r2Inv
    
    ! Compute dipole moment of the intrinsic magnetic field B0.
    Dipole_D = (/0.0, UserDipoleStrength, 0.0 /)
    Dp = 3*sum(Dipole_D*Xyz_D)*r2Inv

    B0_D = B0_Dm + (Dp*Xyz_D - Dipole_D)*r3Inv

  end subroutine user_get_b0
  !=====================================================================        

end module ModUser
