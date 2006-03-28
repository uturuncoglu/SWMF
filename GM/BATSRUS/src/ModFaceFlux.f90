module ModFaceFlux

  use ModProcMH, ONLY: iProc
  use ModMain,       ONLY: x_, y_, z_
  use ModMain,       ONLY: UseBorisSimple                 !^CFG IF SIMPLEBORIS
  use ModMain,       ONLY: UseBoris => boris_correction   !^CFG IF BORISCORR
  use ModVarIndexes, ONLY: nVar
  use ModGeometry,   ONLY: fAx_BLK, fAy_BLK, fAz_BLK
  
  use ModGeometry,   ONLY: UseCovariant, &                !^CFG IF COVARIANT 
       FaceAreaI_DFB, FaceAreaJ_DFB, FaceAreaK_DFB, &     !^CFG IF COVARIANT 
       FaceArea2MinI_B, FaceArea2MinJ_B, FaceArea2MinK_B  !^CFG IF COVARIANT 

  use ModAdvance, ONLY:&
       B0xFace_x_BLK, B0yFace_x_BLK, B0zFace_x_BLK, & ! input: face X B0
       B0xFace_y_BLK, B0yFace_y_BLK, B0zFace_y_BLK, & ! input: face Y B0
       B0xFace_z_BLK, B0yFace_z_BLK, B0zFace_z_BLK, & ! input: face Z B0
       LeftState_VX,  LeftState_VY,  LeftState_VZ,  & ! input: left  face state
       RightState_VX, RightState_VY, RightState_VZ, & ! input: right face state
       Flux_VX, Flux_VY, Flux_VZ,        & ! output: face flux
       VdtFace_x, VdtFace_y, VdtFace_z,  & ! output: cMax*Area for CFL
       EDotFA_X, EDotFA_Y, EDotFA_Z,     & ! output: E.Area for Boris !^CFG IF BORISCORR
       UDotFA_X, UDotFA_Y, UDotFA_Z        ! output: U.Area for P source

  implicit none

  logical :: UseModFaceFlux = .true.

  logical :: DoLf                !^CFG IF RUSANOVFLUX
  logical :: DoHll               !^CFG IF LINDEFLUX
  logical :: DoAw                !^CFG IF AWFLUX
  logical :: DoRoe               !^CFG IF ROEFLUX

  logical :: DoTestCell

  integer :: iFace, jFace, kFace

  real :: StateLeft_V(nVar), StateRight_V(nVar)
  real :: FluxLeft_V(nVar+1), FluxRight_V(nVar+1)
  real :: StateLeftCons_V(nVar+1), StateRightCons_V(nVar+1)
  real :: B0x, B0y, B0z
  real :: Area, Area2, AreaX, AreaY, AreaZ
  real :: Cmax, Unormal, UnLeft, UnRight
  real :: Enormal                !^CFG IF BORISCORR

contains
  !===========================================================================
  subroutine calc_face_flux(DoResChangeOnly, iBlock)

    use ModAdvance,  ONLY: TypeFlux => FluxType
    use ModParallel, ONLY: &
         neiLtop, neiLbot, neiLeast, neiLwest, neiLnorth, neiLsouth
    use ModMain, ONLY: nI, nJ, nK, nIFace, nJFace, nKFace, &
         jMinFaceX, jMaxFaceX, kMinFaceX, kMaxFaceX, &
         iMinFaceY, iMaxFaceY, kMinFaceY,kMaxFaceY, &
         iMinFaceZ,iMaxFaceZ, jMinFaceZ, jMaxFaceZ, &
         iTest, jTest, kTest, ProcTest, BlkTest
    use ModPhysics,  ONLY: Clight

    implicit none

    logical, intent(in) :: DoResChangeOnly
    integer, intent(in) :: iBlock

    logical :: DoTest, DoTestMe
    !--------------------------------------------------------------------------

    if(iProc==PROCtest .and. iBlock==BLKtest)then
       call set_oktest('calc_facefluxes', DoTest, DoTestMe)
    else
       DoTest=.false.; DoTestMe=.false.
    end if

    DoLf  = TypeFlux == 'Rusanov'     !^CFG IF RUSANOVFLUX
    DoHLL = TypeFlux == 'Linde'       !^CFG IF LINDEFLUX
    DoAw  = TypeFlux == 'Sokolov'     !^CFG IF AWFLUX
    DoRoe = TypeFlux == 'Roe'         !^CFG IF ROEFLUX

    if (DoResChangeOnly) then
       if(neiLeast(iBlock) == 1)call get_flux_x(iBlock,1,1,1,nJ,1,nK)
       if(neiLwest(iBlock) == 1)call get_flux_x(iBlock,nIFace,nIFace,1,nJ,1,nK)
       if(neiLsouth(iBlock)== 1)call get_flux_y(iBlock,1,nI,1,1,1,nK)
       if(neiLnorth(iBlock)== 1)call get_flux_y(iBlock,1,nI,nJFace,nJFace,1,nK)
       if(neiLbot(iBlock)  == 1)call get_flux_z(iBlock,1,nI,1,nJ,1,1)
       if(neiLtop(iBlock)  == 1)call get_flux_z(iBlock,1,nI,1,nJ,nKFace,nKFace)
    else
       call get_flux_x(iBlock,1,nIFace,jMinFaceX,jMaxFaceX,kMinFaceX,kMaxFaceX)
       call get_flux_y(iBlock,iMinFaceY,iMaxFaceY,1,nJFace,kMinFaceY,kMaxFaceY)
       call get_flux_z(iBlock,iMinFaceZ,iMaxFaceZ,jMinFaceZ,jMaxFaceZ,1,nKFace)
    end if

  contains

    !==========================================================================

    subroutine get_flux_x(iBlock,iMin,iMax,jMin,jMax,kMin,kMax)
      integer, intent(in):: iBlock,iMin,iMax,jMin,jMax,kMin,kMax
      !-----------------------------------------------------------------------
      if(.not.UseCovariant)then                 !^CFG IF COVARIANT
         Area  = fAx_BLK(iBlock)
         Area2 = Area**2
         AreaX = Area; AreaY = 0.0; AreaZ = 0.0
      end if                                    !^CFG IF COVARIANT

      do kFace = kMin, kMax; do jFace = jMin, jMax; do iFace = iMin, iMax

         if(UseCovariant)then                   !^CFG IF COVARIANT BEGIN
            AreaX = FaceAreaI_DFB(x_, iFace, jFace, kFace, iBlock)
            AreaY = FaceAreaI_DFB(y_, iFace, jFace, kFace, iBlock)
            AreaZ = FaceAreaI_DFB(z_, iFace, jFace, kFace, iBlock)
            Area2 = max(AreaX**2 + AreaY**2 + AreaZ**2, &
                 FaceArea2MinI_B(iBlock))
         end if                                 !^CFG END COVARIANT

         DoTestCell = DoTestMe &
              .and. (iFace == iTest .or. iFace == iTest+1) &
              .and. jFace == jTest .and. kFace == kTest

         B0x = B0xFace_x_BLK(iFace, jFace, kFace, iBlock)
         B0y = B0yFace_x_BLK(iFace, jFace, kFace, iBlock)
         B0z = B0zFace_x_BLK(iFace, jFace, kFace, iBlock)
         StateLeft_V  = LeftState_VX( :, iFace, jFace, kFace)
         StateRight_V = RightState_VX(:, iFace, jFace, kFace)

         call get_numerical_flux(x_, Flux_VX(:,iFace, jFace, kFace))

         VdtFace_x(iFace, jFace, kFace) = Cmax
         UDotFA_X(iFace, jFace, kFace)  = Unormal
         EDotFA_X(iFace, jFace, kFace)  = Enormal !^CFG IF BORISCORR

      end do; end do; end do
    end subroutine get_flux_x

    !==========================================================================

    subroutine get_flux_y(iBlock,iMin,iMax,jMin,jMax,kMin,kMax)
      integer, intent(in):: iBlock,iMin,iMax,jMin,jMax,kMin,kMax
      !------------------------------------------------------------------------
      if(.not.UseCovariant)then                 !^CFG IF COVARIANT
         Area  = fAy_BLK(iBlock)
         Area2 = Area**2
         AreaX = 0.0; AreaY = Area; AreaZ = 0.0
      end if                                    !^CFG IF COVARIANT

      do kFace = kMin, kMax; do jFace = jMin, jMax; do iFace = iMin, iMax

         if(UseCovariant)then                   !^CFG IF COVARIANT BEGIN
            AreaX = FaceAreaJ_DFB(x_, iFace, jFace, kFace, iBlock)
            AreaY = FaceAreaJ_DFB(y_, iFace, jFace, kFace, iBlock)
            AreaZ = FaceAreaJ_DFB(z_, iFace, jFace, kFace, iBlock)
            Area2 = max(AreaX**2 + AreaY**2 + AreaZ**2, &
                 FaceArea2MinJ_B(iBlock))
         end if                                 !^CFG END COVARIANT

         DoTestCell = DoTestMe .and. iFace == iTest .and. &
              (jFace == jTest .or. jFace == jTest+1) .and. kFace == kTest

         B0x = B0xFace_y_BLK(iFace, jFace, kFace, iBlock)
         B0y = B0yFace_y_BLK(iFace, jFace, kFace, iBlock)
         B0z = B0zFace_y_BLK(iFace, jFace, kFace, iBlock)
         StateLeft_V  = LeftState_VY( :, iFace, jFace, kFace)
         StateRight_V = RightState_VY(:, iFace, jFace, kFace)

         call get_numerical_flux(y_, Flux_VY(:, iFace, jFace, kFace))

         VdtFace_y(iFace, jFace, kFace) = Cmax
         UDotFA_Y( iFace, jFace, kFace) = Unormal
         EDotFA_Y(iFace, jFace, kFace)  = Enormal !^CFG IF BORISCORR

      end do; end do; end do

    end subroutine get_flux_y

    !==========================================================================

    subroutine get_flux_z(iBlock, iMin, iMax, jMin, jMax, kMin, kMax)
      integer, intent(in):: iBlock, iMin, iMax, jMin, jMax, kMin, kMax
      !------------------------------------------------------------------------
      if(.not.UseCovariant)then                 !^CFG IF COVARIANT
         Area  = fAz_BLK(iBlock)
         Area2 = Area**2
         AreaX = 0.0; AreaY = 0.0; AreaZ = Area
      end if                                    !^CFG IF COVARIANT

      do kFace = kMin, kMax; do jFace = jMin, jMax; do iFace = iMin, iMax

         if(UseCovariant)then                   !^CFG IF COVARIANT BEGIN
            AreaX = FaceAreaK_DFB(x_, iFace, jFace, kFace, iBlock)
            AreaY = FaceAreaK_DFB(y_, iFace, jFace, kFace, iBlock)
            AreaZ = FaceAreaK_DFB(z_, iFace, jFace, kFace, iBlock)
            Area2 = max(AreaX**2 + AreaY**2 + AreaZ**2, &
                 FaceArea2MinK_B(iBlock))
         end if                                 !^CFG END COVARIANT

         DoTestCell = DoTestMe .and. iFace == iTest .and. &
              jFace == jTest .and. (kFace == kTest .or. kFace == kTest+1)

         B0x = B0xFace_z_BLK(iFace, jFace, kFace,iBlock)
         B0y = B0yFace_z_BLK(iFace, jFace, kFace,iBlock)
         B0z = B0zFace_z_BLK(iFace, jFace, kFace,iBlock)
         StateLeft_V  = LeftState_VZ( :, iFace, jFace, kFace)
         StateRight_V = RightState_VZ(:, iFace, jFace, kFace)

         call get_numerical_flux(z_, Flux_VZ(:, iFace, jFace, kFace))

         VdtFace_z(iFace, jFace, kFace) = Cmax
         UDotFA_Z( iFace, jFace, kFace) = Unormal
         EDotFA_Z(iFace, jFace, kFace)  = Enormal !^CFG IF BORISCORR

      end do; end do; end do
    end subroutine get_flux_z

  end subroutine calc_face_flux

  !===========================================================================

  subroutine get_numerical_flux(iDir, Flux_V)

    use ModVarIndexes, ONLY: U_, Bx_, By_, Bz_

    integer, intent(in) :: iDir
    real,    intent(out):: Flux_V(nVar+1)

    real :: State_V(nVar)

    real :: DiffBn, DiffBx, DiffBy, DiffBz, DiffE
    real :: EnLeft, EnRight
    !-----------------------------------------------------------------------

    if(.false. &
         .or. DoHll &               !^CFG IF LINDEFLUX
         .or. DoAw  &               !^CFG IF AWFLUX
         )then
       ! Sokolov's algorithm
       ! Calculate and store the jump in the normal magnetic field
       DiffBn    = 0.5* &
            ( (StateRight_V(Bx_) - StateLeft_V(Bx_))*AreaX &
            + (StateRight_V(By_) - StateLeft_V(By_))*AreaY &
            + (StateRight_V(Bz_) - StateLeft_V(Bz_))*AreaZ ) / Area2

       ! Calculate the Cartesian components
       DiffBx    = AreaX*DiffBn
       DiffBy    = AreaY*DiffBn
       DiffBz    = AreaZ*DiffBn

       ! Remove the jump in the normal magnetic field
       StateLeft_V(Bx_)  =  StateLeft_V(Bx_)  + DiffBx
       StateLeft_V(By_)  =  StateLeft_V(By_)  + DiffBy
       StateLeft_V(Bz_)  =  StateLeft_V(Bz_)  + DiffBz
       StateRight_V(Bx_) =  StateRight_V(Bx_) - DiffBx
       StateRight_V(By_) =  StateRight_V(By_) - DiffBy
       StateRight_V(Bz_) =  StateRight_V(Bz_) - DiffBz

       ! The energy jump is also modified by 
       ! 1/2(Br^2 - Bl^2) = 1/2(Br-Bl)*(Br+Bl)
       ! We store half of this in DiffE
       DiffE = 0.5*( &
            (StateRight_V(Bx_) + StateLeft_V(Bx_))*DiffBx + &
            (StateRight_V(By_) + StateLeft_V(By_))*DiffBy + &
            (StateRight_V(Bz_) + StateLeft_V(Bz_))*DiffBz )
    end if

    call get_physical_flux(iDir, StateLeft_V, B0x, B0y, B0z,&
         StateLeftCons_V, FluxLeft_V, UnLeft, EnLeft)

    call get_physical_flux(iDir, StateRight_V, B0x, B0y, B0z,&
         StateRightCons_V, FluxRight_V, UnRight, EnRight)

    ! All the solvers below use the average state
    State_V = 0.5*(StateLeft_V + StateRight_V)

    if(DoLf)  call lax_friedrichs_flux       !^CFG IF RUSANOVFLUX
    if(DoHll) call harten_lax_vanleer_flux   !^CFG IF LINDEFLUX
    if(DoAw)  call artificial_wind           !^CFG IF AWFLUX
    if(DoRoe) call roe_solver(iDir, Flux_V)  !^CFG IF ROEFLUX

    if(DoTestCell)call write_test_info

  contains

    !^CFG IF RUSANOVFLUX BEGIN
    !==========================================================================
    subroutine lax_friedrichs_flux

      call get_speed_max(iDir, State_V, B0x, B0y, B0z, Cmax = Cmax)

      Flux_V = 0.5*(FluxLeft_V + FluxRight_V &
           - Cmax*(StateRightCons_V - StateLeftCons_V))

      Unormal = 0.5*(UnLeft + UnRight)
      Enormal = 0.5*(EnLeft + EnRight)                  !^CFG IF BORISCORR

    end subroutine lax_friedrichs_flux
    !^CFG END RUSANOVFLUX
    !^CFG IF LINDEFLUX BEGIN
    !==========================================================================
    subroutine harten_lax_vanleer_flux

      use ModVarIndexes, ONLY: B_, Energy_

      real :: Cleft,  CleftStateLeft, CleftStateAverage
      real :: Cright, CrightStateRight, CrightStateAverage
      real :: WeightLeft, WeightRight, Diffusion
      !-----------------------------------------------------------------------

      call get_speed_max(iDir, StateLeft_V,  B0x, B0y, B0z, &
           Cleft =CleftStateLeft)
      call get_speed_max(iDir, StateRight_V, B0x, B0y, B0z, &
           Cright=CrightStateRight)
      call get_speed_max(iDir, State_V, B0x, B0y, B0z, &
           Cmax = Cmax, Cleft = CleftStateAverage, Cright = CrightStateAverage)

      Cleft  = min(0.0, CleftStateLeft,   CleftStateAverage)
      Cright = max(0.0, CrightStateRight, CrightStateAverage)

      WeightLeft  = Cright/(Cright - Cleft)
      WeightRight = 1.0 - WeightLeft
      Diffusion   = Cright*WeightRight

      Flux_V = &
           (WeightRight*FluxRight_V + WeightLeft*FluxLeft_V &
           - Diffusion*(StateRightCons_V - StateLeftCons_V))

      ! Linde's idea: use Lax-Friedrichs flux for Bn
      Flux_V(Bx_) = Flux_V(Bx_) - cMax*DiffBx
      Flux_V(By_) = Flux_V(By_) - cMax*DiffBy
      Flux_V(Bz_) = Flux_V(Bz_) - cMax*DiffBz

      ! Fix the energy diffusion
      Flux_V(Energy_) = Flux_V(Energy_) - cMax*DiffE

      ! Average the normal speed
      Unormal = WeightRight*UnRight + WeightLeft*UnLeft
      Enormal = WeightRight*EnRight + WeightLeft*EnLeft !^CFG IF BORISCORR

    end subroutine harten_lax_vanleer_flux
    !^CFG END LINDEFLUX
    !^CFG IF AWFLUX BEGIN
    !==========================================================================
    subroutine artificial_wind

      use ModVarIndexes, ONLY: B_, Energy_

      real :: Cleft, Cright, WeightLeft, WeightRight, Diffusion
      !-----------------------------------------------------------------------

      ! The propagation speeds are modified by the DoAw = .true. !
      call get_speed_max(iDir, State_V, B0x, B0y, B0z,  &
           Cleft = Cleft, Cright = Cright, Cmax = Cmax)

      Cleft  = min(0.0, Cleft)
      Cright = max(0.0, Cright)

      WeightLeft  = Cright/(Cright - Cleft)
      WeightRight = 1.0 - WeightLeft
      Diffusion   = Cright*WeightRight

      Flux_V = &
           (WeightRight*FluxRight_V + WeightLeft*FluxLeft_V &
           - Diffusion*(StateRightCons_V - StateLeftCons_V))

      ! Linde's idea: use Lax-Friedrichs flux for Bn
      Flux_V(Bx_) = Flux_V(Bx_) - cMax*DiffBx
      Flux_V(By_) = Flux_V(By_) - cMax*DiffBy
      Flux_V(Bz_) = Flux_V(Bz_) - cMax*DiffBz

      ! Sokolov's algorithm !!!
      ! Fix the energy diffusion
      Flux_V(Energy_) = Flux_V(Energy_) - cMax*DiffE

      ! Weighted average of the normal speed and electric field
      Unormal = WeightRight*UnRight + WeightLeft*UnLeft
      Enormal = WeightRight*EnRight + WeightLeft*EnLeft !^CFG IF BORISCORR

    end subroutine artificial_wind
    !^CFG END AWFLUX
    !=======================================================================

    subroutine write_test_info
      use ModVarIndexes
      integer :: iVar
      !--------------------------------------------------------------------
      write(*,*)'Hat state for face=',iDir,&
           ' at I=',iFace,' J=',jFace,' K=',kFace
      write(*,*)'rho=',0.5*(StateLeft_V(Rho_)+StateRight_V(Rho_))
      write(*,*)'Un =',0.5*(StateLeft_V(U_+iDir)+StateRight_V(U_+iDir))
      write(*,*)'P  =',0.5*(StateLeft_V(P_)+StateRight_V(P_))
      write(*,*)'B  =', &
           0.5*(StateLeft_V(Bx_:Bz_) + StateRight_V(Bx_:Bz_)) + (/B0x,B0y,B0z/)
      write(*,*)'BB =', &
           sum( (0.5*(StateLeft_V(Bx_:Bz_) + StateRight_V(Bx_:Bz_)) &
           + (/B0x,B0y,B0z/))**2)

      write(*,*)'Fluxes for dir=',iDir,' at I=',iFace,' J=',jFace,' K=',kFace
      write(*,*)'Eigenvalue_maxabs=', Cmax/Area
      do iVar = 1, nVar + 1
         write(*,'(a,i2,4(1pe13.5))') 'Var,F,F_L,F_R,dU=',&
              iVar ,&
              Flux_V(iVar), &
              FluxLeft_V(iVar)/Area, &
              FluxRight_V(iVar)/Area,&
              StateRightCons_V(iVar)-StateLeftCons_V(iVar)
      end do

    end subroutine write_test_info

  end subroutine get_numerical_flux

  !===========================================================================

  subroutine get_physical_flux(iDir, State_V, B0x, B0y, B0z, &
       StateCons_V, Flux_V, Un, En)

    integer, intent(in) :: iDir               ! direction of flux
    real,    intent(in) :: State_V(nVar)      ! input primitive state
    real,    intent(in) :: B0x, B0y, B0z      ! B0
    real,    intent(out):: StateCons_V(nVar+1)! conservative states with energy
    real,    intent(out):: Flux_V(nVar+1)     ! fluxes for all states
    real,    intent(out):: Un                 ! normal velocity
    real,    intent(out):: En                 ! normal electric field

    if(UseBoris)then           !^CFG IF BORISCORR BEGIN
       call get_boris_flux
    else                       !^CFG END BORISCORR
       call get_mhd_flux
       En = 0.0
    end if                     !^CFG IF BORISCORR

  contains

    !^CFG IF BORISCORR BEGIN
    subroutine get_boris_flux

      use ModPhysics, ONLY: g, inv_gm1, Inv_C2light, InvClight
      use ModMain,    ONLY: x_, y_, z_
      use ModVarIndexes

      ! Variables for conservative state and flux calculation
      real :: Rho, Ux, Uy, Uz, Bx, By, Bz, p, e, FullBx, FullBy, FullBz, FullBn
      real :: B2, FullB2, pTotal, pTotal2, UDotB
      real :: Ex, Ey, Ez, E2Half
      real :: Bn, B0n
      integer :: iVar
      !-----------------------------------------------------------------------

      ! Extract primitive variables
      Rho     = State_V(Rho_)
      Ux      = State_V(Ux_)
      Uy      = State_V(Uy_)
      Uz      = State_V(Uz_)
      Bx      = State_V(Bx_)
      By      = State_V(By_)
      Bz      = State_V(Bz_)
      p       = State_V(p_)

      B2      = Bx**2 + By**2 + Bz**2

      FullBx  = Bx + B0x
      FullBy  = By + B0y
      FullBz  = Bz + B0z

      ! Electric field divided by speed of light: 
      ! E= - U x B / c = (B x U)/c
      Ex      = (FullBy*Uz - FullBz*Uy) * InvClight
      Ey      = (FullBz*Ux - FullBx*Uz) * InvClight
      Ez      = (FullBx*Uy - FullBy*Ux) * InvClight

      ! Electric field squared/c^2
      E2Half  = 0.5*(Ex**2 + Ey**2 + Ez**2)

      ! Calculate energy
      e = inv_gm1*p + 0.5*(Rho*(Ux**2 + Uy**2 + Uz**2) + B2)

      ! Calculate conservative state
      StateCons_V(1:nVar)  = State_V

      ! The full momentum contains the ExB/c^2 term:
      ! rhoU_Boris = rhoU - ((U x B) x B)/c^2 = rhoU + (U B^2 - B U.B)/c^2
      UDotB   = Ux*FullBx + Uy*FullBy + Uz*FullBz
      FullB2  = FullBx**2 + FullBy**2 + FullBz**2
      StateCons_V(RhoUx_)  = Rho*Ux + (Ux*FullB2 - FullBx*UdotB)*inv_c2LIGHT
      StateCons_V(RhoUy_)  = Rho*Uy + (Uy*FullB2 - FullBy*UdotB)*inv_c2LIGHT
      StateCons_V(RhoUz_)  = Rho*Uz + (Uz*FullB2 - FullBz*UdotB)*inv_c2LIGHT

      ! The full energy contains the electric field energy
      StateCons_V(Energy_) = e + E2Half

      ! Calculate some intermediate values for flux calculations
      pTotal  = p + 0.5*B2 + B0x*Bx + B0y*By + B0z*Bz
      pTotal2 = pTotal + E2Half

      ! Normal direction
      Un     = Ux*AreaX  + Uy*AreaY  + Uz*AreaZ
      B0n    = B0x*AreaX + B0y*AreaY + B0z*AreaZ
      Bn     = Bx*AreaX  + By*AreaY  + Bz*AreaZ
      FullBn = B0n + Bn
      En     = Ex*AreaX + Ey*AreaY + Ez*AreaZ

      ! f_i[rho]=m_i
      Flux_V(Rho_)   = Rho*Un

      ! f_i[m_k]=m_i*m_k/rho - b_k*b_i -B0_k*b_i - B0_i*b_k - E_i*E_k
      !          +n_i*[p + B0_j*b_j + 0.5*(b_j*b_j + E_j*E_j)]
      Flux_V(RhoUx_) = Un*Rho*Ux - Bn*FullBx - B0n*Bx - En*Ex + pTotal2*Areax
      Flux_V(RhoUy_) = Un*Rho*Uy - Bn*FullBy - B0n*By - En*Ey + pTotal2*Areay
      Flux_V(RhoUz_) = Un*Rho*Uz - Bn*FullBz - B0n*Bz - En*Ez + pTotal2*Areaz

      ! f_i[b_k]=u_i*(b_k+B0_k) - u_k*(b_i+B0_i)
      Flux_V(Bx_) = Un*FullBx - Ux*FullBn
      Flux_V(By_) = Un*FullBy - Uy*FullBn
      Flux_V(Bz_) = Un*FullBz - Uz*FullBn

      ! f_i[p]=u_i*p
      Flux_V(p_)  = Un*p

      ! f_i[e]=(u_i*(ptotal+e+(b_k*B0_k))-(b_i+B0_i)*(b_k*u_k))
      Flux_V(Energy_) = &
           Un*(pTotal + e) - FullBn*(Ux*Bx + Uy*By + Uz*Bz)

      ! f_i[scalar] = Un*scalar
      do iVar = ScalarFirst_, ScalarLast_
         Flux_V(iVar) = Un*State_V(iVar)
      end do

    end subroutine get_boris_flux

    !==========================================================================
    !^CFG END BORISCORR

    subroutine get_mhd_flux

      use ModPhysics, ONLY: g, inv_gm1, inv_c2LIGHT
      use ModMain,    ONLY: x_, y_, z_
      use ModVarIndexes

      ! Variables for conservative state and flux calculation
      real :: Rho, Ux, Uy, Uz, Bx, By, Bz, p, e, FullBx, FullBy, FullBz, FullBn
      real :: Bn, B0n
      real :: B2, pTotal
      real :: Gamma2                           !^CFG IF SIMPLEBORIS
      integer :: iVar
      !-----------------------------------------------------------------------

      ! Extract primitive variables
      Rho     = State_V(Rho_)
      Ux      = State_V(Ux_)
      Uy      = State_V(Uy_)
      Uz      = State_V(Uz_)
      Bx      = State_V(Bx_)
      By      = State_V(By_)
      Bz      = State_V(Bz_)
      p       = State_V(p_)

      B2      = Bx**2 + By**2 + Bz**2

      ! Calculate energy
      e = inv_gm1*p + 0.5*(Rho*(Ux**2 + Uy**2 + Uz**2) + B2)

      ! Calculate conservative state
      StateCons_V(1:nVar)  = State_V
      StateCons_V(RhoUx_)  = Rho*Ux
      StateCons_V(RhoUy_)  = Rho*Uy
      StateCons_V(RhoUz_)  = Rho*Uz
      StateCons_V(Energy_) = e

      ! Calculate some intermediate values for flux calculations

      FullBx  = Bx + B0x
      FullBy  = By + B0y
      FullBz  = Bz + B0z
      pTotal  = p + 0.5*B2 + B0x*Bx + B0y*By + B0z*Bz

      ! Normal direction
      Un     = Ux*AreaX  + Uy*AreaY  + Uz*AreaZ
      B0n    = B0x*AreaX + B0y*AreaY + B0z*AreaZ
      Bn     = Bx*AreaX  + By*AreaY  + Bz*AreaZ
      FullBn = B0n + Bn

      ! f_i[rho]=m_i
      Flux_V(Rho_)   = Rho*Un

      ! f_i[m_k]=m_i*m_k/rho - b_k*b_i -B0_k*b_i - B0_i*b_k + n_i*[ptotal]
      Flux_V(RhoUx_) = Un*Rho*Ux - Bn*FullBx - B0n*Bx + pTotal*AreaX
      Flux_V(RhoUy_) = Un*Rho*Uy - Bn*FullBy - B0n*By + pTotal*AreaY
      Flux_V(RhoUz_) = Un*Rho*Uz - Bn*FullBz - B0n*Bz + pTotal*AreaZ

      ! f_i[b_k]=u_i*(b_k+B0_k) - u_k*(b_i+B0_i)
      Flux_V(Bx_) = Un*FullBx - Ux*FullBn
      Flux_V(By_) = Un*FullBy - Uy*FullBn
      Flux_V(Bz_) = Un*FullBz - Uz*FullBn

      ! f_i[p]=u_i*p
      Flux_V(p_)  = Un*p

      ! f_i[e]=(u_i*(ptotal+e+(b_k*B0_k))-(b_i+B0_i)*(b_k*u_k))
      Flux_V(Energy_) = &
           Un*(pTotal + e) - FullBn*(Ux*Bx + Uy*By + Uz*Bz)

      ! f_i[scalar] = Un*scalar
      do iVar = ScalarFirst_, ScalarLast_
         Flux_V(iVar) = Un*State_V(iVar)
      end do

      !^CFG IF SIMPLEBORIS BEGIN
      if(UseBorisSimple)then
         ! Correct the momentum using the (1+VA2/c^2)
         Gamma2 = 1.0 + (FullBx**2 + FullBy**2 + FullBz**2)/Rho*inv_c2LIGHT
         StateCons_V(RhoUx_:RhoUz_) = StateCons_V(RhoUx_:RhoUz_)*Gamma2
      end if
      !^CFG END SIMPLEBORIS

    end subroutine get_mhd_flux

  end subroutine get_physical_flux

  !===========================================================================

  subroutine get_speed_max(iDir, State_V, B0x, B0y, B0z, cMax, cLeft, cRight)

    integer, intent(in) :: iDir
    real,    intent(in) :: State_V(nVar)
    real,    intent(in) :: B0x, B0y, B0z
    real, optional, intent(out) :: Cmax     ! maximum speed relative to lab
    real, optional, intent(out) :: Cleft    ! maximum left speed
    real, optional, intent(out) :: Cright   ! maximum right speed

    if(UseBoris)then                             !^CFG IF BORISCORR BEGIN
       call get_boris_speed
    else                                         !^CFG END BORISCORR
       call get_mhd_speed
    endif                                        !^CFG IF BORISCORR

  contains

    !^CFG IF BORISCORR BEGIN
    !========================================================================
    subroutine get_boris_speed

      use ModMain,    ONLY: x_, y_, z_
      use ModVarIndexes
      use ModPhysics, ONLY: g, inv_c2LIGHT

      real :: InvRho, Sound2, FullBx, FullBy, FullBz
      real :: Alfven2, Alfven2Normal, Un, Fast2, Discr, Fast, Slow
      real :: GammaA2, GammaU2
      real :: UnBoris, Sound2Boris, Alfven2Boris, Alfven2NormalBoris
      !-----------------------------------------------------------------------
      InvRho = 1.0/State_V(Rho_)
      Sound2 = g*State_V(p_)*InvRho
      FullBx = State_V(Bx_) + B0x
      FullBy = State_V(By_) + B0y
      FullBz = State_V(Bz_) + B0z
      Alfven2= (FullBx**2 + FullBy**2 + FullBz**2)*InvRho

      Un = State_V(Ux_)*AreaX + State_V(Uy_)*AreaY + State_V(Uz_)*AreaZ
      Alfven2Normal = &
           InvRho*(AreaX*FullBx + AreaY*FullBy + AreaZ*FullBz)**2/Area2

      ! "Alfven Lorentz" factor
      GammaA2 = 1.0/(1.0 + Alfven2*inv_c2LIGHT) 

      ! 1-gA^2*Un^2/c^2
      GammaU2 = max(0.0, 1.0 - GammaA2*Un**2/Area2*inv_c2LIGHT) 

      ! Modified speeds
      Sound2Boris        = Sound2        *GammaA2*(1+Alfven2Normal*inv_c2LIGHT)
      Alfven2Boris       = Alfven2       *GammaA2*GammaU2
      Alfven2NormalBoris = Alfven2Normal *GammaA2*GammaU2

      ! Approximate slow and fast wave speeds
      Fast2  = Sound2Boris + Alfven2Boris
      Discr  = sqrt(max(0.0, Fast2**2 - 4.0*Sound2*Alfven2NormalBoris))

      ! Get fast and slow speeds multiplied with the face area
      Fast = sqrt( Area2*0.5*(          Fast2 + Discr) )
      Slow = sqrt( Area2*0.5*( max(0.0, Fast2 - Discr) ) )

      ! In extreme cases "slow" wave can be faster than "fast" wave
      ! so take the maximum of the two

      if(DoAw)then                                        !^CFG IF AWFLUX BEGIN
         Un      = min(UnRight, UnLeft)
         Cleft   = min(Un*GammaA2 - Fast, Un - Slow)
         Un      = max(UnLeft, UnRight)
         Cright  = max(Un*GammaA2 + Fast, Un + Slow)
         Cmax    = max(Cright, -Cleft)
      else                                                !^CFG END AWFLUX
         UnBoris            = Un*GammaA2
         if(present(Cmax))   Cmax   = max(abs(UnBoris) + Fast, abs(Un) + Slow)
         if(present(Cleft))  Cleft  = min(UnBoris - Fast, Un - Slow)
         if(present(Cright)) Cright = max(UnBoris + Fast, Un + Slow)
      end if                                              !^CFG IF AWFLUX

    end subroutine get_boris_speed
    !^CFG END BORISCORR
    !========================================================================

    subroutine get_mhd_speed

      use ModMain,    ONLY: x_, y_, z_
      use ModVarIndexes
      use ModPhysics, ONLY: g, inv_c2LIGHT

      real :: InvRho, Sound2, FullBx, FullBy, FullBz
      real :: Alfven2, Alfven2Normal, Un, Fast2, Discr, Fast
      real :: dB1dB1 ! For AW scheme only
      !------------------------------------------------------------------------
      InvRho = 1.0/State_V(Rho_)
      Sound2 = g*State_V(p_)*InvRho
      FullBx = State_V(Bx_) + B0x
      FullBy = State_V(By_) + B0y
      FullBz = State_V(Bz_) + B0z
      if(DoAw)then                                       !^CFG IF AWFLUX BEGIN
         ! According to I. Sokolov adding (Bright-Bleft)^2/4 to
         ! the average field squared (Bright+Bleft)^2/4 results in 
         ! an upper estimate of the left and right Alfven speeds 
         ! max(Bleft^2/RhoLeft, Bright^2/RhoRight)/
         !
         ! For B0=Bleft=0 and Bright=1 RhoLeft=RhoRight=1 
         ! this is clearly not true.
         !
         dB1dB1 = 0.25*sum((StateRight_V(Bx_:Bz_)-StateLeft_V(Bx_:Bz_))**2)
         Alfven2= (FullBx**2 + FullBy**2 + FullBz**2 + dB1dB1)*InvRho
      else                                               !^CFG END AWFLUX
         Alfven2= (FullBx**2 + FullBy**2 + FullBz**2)*InvRho
      end if                                             !^CFG IF AWFLUX

      Un = State_V(Ux_)*AreaX + State_V(Uy_)*AreaY + State_V(Uz_)*AreaZ
      Alfven2Normal = &
           InvRho*(AreaX*FullBx + AreaY*FullBy + AreaZ*FullBz)**2/Area2

      Fast2  = Sound2 + Alfven2
      Discr  = sqrt(max(0.0, Fast2**2 - 4*Sound2*Alfven2Normal))

      ! Fast speed multiploed with the face area
      if(UseBorisSimple)then                         !^CFG IF SIMPLEBORIS BEGIN
         Fast = sqrt( Area2*0.5*(Fast2 + Discr) &
              /       (1.0 + Alfven2*Inv_C2light) )
      else                                           !^CFG END SIMPLEBORIS
         Fast = sqrt( Area2*0.5*(Fast2 + Discr) )
      end if                                         !^CFG IF SIMPLEBORIS

      if(DoAw)then                                   !^CFG IF AWFLUX BEGIN
         Cleft   = min(UnLeft, UnRight) - Fast
         Cright  = max(UnLeft, UnRight) + Fast
         Cmax    = max(Cright, -Cleft)
      else                                           !^CFG END AWFLUX
         if(present(Cmax))   Cmax   = abs(Un) + Fast
         if(present(Cleft))  Cleft  = Un - Fast
         if(present(Cright)) Cright = Un + Fast
      end if                                         !^CFG IF AWFLUX

    end subroutine get_mhd_speed

  end subroutine get_speed_max

end module ModFaceFlux

!^CFG IF ROEFLUX BEGIN
!==============================================================================
subroutine roe_solver(iDir, Flux_V)

  use ModVarIndexes, ONLY: U_, B_

  use ModFaceFlux, ONLY: &
       iFace, jFace, kFace, Area, DoTestCell, &
       StateLeft_V,  StateRight_V, FluxLeft_V, FluxRight_V, &
       StateLeftCons_V, StateRightCons_V, B0x, B0y, B0z, Cmax, Unormal

  use ModVarIndexes, ONLY: nVar, Rho_, RhoUx_, RhoUy_, RhoUz_, &
       Ux_, Uy_, Uz_, Bx_, By_, Bz_, p_, Energy_

  use ModMain,     ONLY: x_, y_, z_, GlobalBlk
  use ModGeometry, ONLY: true_cell
  use ModPhysics,  ONLY: g,gm1,inv_gm1
  use ModNumConst

  implicit none

  integer, intent(in) :: iDir
  real,    intent(out):: Flux_V(nVar+1)

  integer, parameter :: nFlux=nVar+1, nWave=nVar
  integer, parameter :: RhoUn_=RhoUx_, RhoUt1_=RhoUy_, RhoUt2_=RhoUz_, &
       B1n_=Bx_, B1t1_=By_, B1t2_=Bz_

  integer, parameter :: EntropyW_=1, AlfvenRW_=2, AlfvenLW_=3, &
       SlowRW_=4, FastRW_=5, SlowLW_=6, FastLW_=7, DivBW_=8 

  integer :: iFlux, iVar, iWave

  ! Left face
  real :: RhoL,UnL,Ut1L,Ut2L
  real :: BnL,Bt1L,Bt2L,BbL
  real :: B1nL,B1t1L,B1t2L,Bb1L
  real :: pL,eL,aL,CsL,CfL

  ! Right face
  real :: RhoR,UnR,Ut1R,Ut2R
  real :: BnR,Bt1R,Bt2R,BbR
  real :: B1nR,B1t1R,B1t2R
  real :: pR,eR,aR,CsR,CfR

  ! Average (hat)
  real :: RhoH,UnH,Ut1H,Ut2H
  real :: BnH,Bt1H,Bt2H,BbH
  real :: B1nH,B1t1H,B1t2H,Bb1H
  real :: pH,eH,UuH
  real :: aH,CsH,CfH

  ! More face variables
  real :: B0n,B0t1,B0t2

  real :: BetaY, BetaZ, AlphaS, AlphaF

  real :: RhoInvL,RhoInvR,RhoInvH
  real :: RhoSqrtH,    RhoSqrtL,    RhoSqrtR, &
       RhoInvSqrtH, RhoInvSqrtL, RhoInvSqrtR

  ! Jump in the conservative state
  real, dimension(nWave) :: dCons_V

  ! Eigenvalues and jumps in characteristic variable
  real, dimension(nWave) :: Eigenvalue_V, DeltaWave_V 

  ! Eigenvectors
  real, dimension(nVar ,nWave):: EigenvectorL_VV       ! Left Eigenvectors
  real, dimension(nWave,nFlux):: EigenvectorR_VV       ! Right Eigenvector

  ! Fluxes
  real, dimension(nFlux)      :: Diffusion_V !!! temporary

  ! Logical to use Rusanov flux at inner boundary
  logical :: UseFluxRusanov

  ! Misc. scalar variables
  real :: SignBnH, Tmp1, Tmp2, Tmp3, Gamma1A2Inv, DtInvVolume, AreaFace
  !---------------------------------------------------------------------------
  select case (iDir)
  case (x_)
     !\
     ! B0 on the face
     !/
     B0n  = B0x
     B0t1 = B0y
     B0t2 = B0z
     !\
     ! Left face
     !/
     RhoL  =  StateLeft_V(rho_)
     UnL   =  StateLeft_V(Ux_)
     Ut1L  =  StateLeft_V(Uy_)
     Ut2L  =  StateLeft_V(Uz_)
     B1nL  =  StateLeft_V(Bx_)
     B1t1L =  StateLeft_V(By_)
     B1t2L =  StateLeft_V(Bz_)
     pL    =  StateLeft_V(P_ )
     !\
     ! Right face
     !/
     RhoR  =  StateRight_V(rho_)
     UnR   =  StateRight_V(Ux_ )
     Ut1R  =  StateRight_V(Uy_ )
     Ut2R  =  StateRight_V(Uz_ )
     B1nR  =  StateRight_V(Bx_ )
     B1t1R =  StateRight_V(By_ )
     B1t2R =  StateRight_V(Bz_ )
     pR    =  StateRight_V(P_  )

     dCons_V(Rho_)    = StateRightCons_V(Rho_)   - StateLeftCons_V(Rho_)
     dCons_V(RhoUn_)  = StateRightCons_V(RhoUx_) - StateLeftCons_V(RhoUx_)
     dCons_V(RhoUt1_) = StateRightCons_V(RhoUy_) - StateLeftCons_V(RhoUy_)
     dCons_V(RhoUt2_) = StateRightCons_V(RhoUz_) - StateLeftCons_V(RhoUz_)
     dCons_V(B1n_)    = StateRightCons_V(Bx_)    - StateLeftCons_V(Bx_)
     dCons_V(B1t1_)   = StateRightCons_V(By_)    - StateLeftCons_V(By_)
     dCons_V(B1t2_)   = StateRightCons_V(Bz_)    - StateLeftCons_V(Bz_)
     ! Put the conservative energy difference in place of the pressure
     dCons_V(p_)      = StateRightCons_V(Energy_)- StateLeftCons_V(Energy_)

     UseFluxRusanov= true_cell(iFace-1, jFace, kFace, globalBLK) &
          .neqv.     true_cell(iFace  , jFace, kFace, globalBLK)

  case (y_)
     !\
     ! y face
     !/
     !\
     ! B0 on the face
     !/
     B0n  = B0y
     B0t1 = B0z
     B0t2 = B0x
     !\
     ! Left face
     !/
     RhoL  =  StateLeft_V(rho_)
     UnL   =  StateLeft_V(Uy_ )
     Ut1L  =  StateLeft_V(Uz_ )
     Ut2L  =  StateLeft_V(Ux_ )
     B1nL  =  StateLeft_V(By_ )
     B1t1L =  StateLeft_V(Bz_ )
     B1t2L =  StateLeft_V(Bx_ )
     pL    =  StateLeft_V(P_  )
     !\
     ! Right face
     !/
     RhoR  =  StateRight_V(rho_)
     UnR   =  StateRight_V(Uy_ )
     Ut1R  =  StateRight_V(Uz_ )
     Ut2R  =  StateRight_V(Ux_ )
     B1nR  =  StateRight_V(By_ )
     B1t1R =  StateRight_V(Bz_ )
     B1t2R =  StateRight_V(Bx_ )
     pR    =  StateRight_V(P_  )

     dCons_V(Rho_)    = StateRightCons_V(Rho_)   - StateLeftCons_V(Rho_)
     dCons_V(RhoUn_)  = StateRightCons_V(RhoUy_) - StateLeftCons_V(RhoUy_)
     dCons_V(RhoUt1_) = StateRightCons_V(RhoUz_) - StateLeftCons_V(RhoUz_)
     dCons_V(RhoUt2_) = StateRightCons_V(RhoUx_) - StateLeftCons_V(RhoUx_)
     dCons_V(B1n_)    = StateRightCons_V(By_)    - StateLeftCons_V(By_)
     dCons_V(B1t1_)   = StateRightCons_V(Bz_)    - StateLeftCons_V(Bz_)
     dCons_V(B1t2_)   = StateRightCons_V(Bx_)    - StateLeftCons_V(Bx_)
     ! Put the conservative energy difference in place of the pressure
     dCons_V(p_)      = StateRightCons_V(Energy_)- StateLeftCons_V(Energy_)

     UseFluxRusanov= true_cell(iFace, jFace-1, kFace, globalBLK) &
          .neqv.     true_cell(iFace, jFace  , kFace, globalBLK)

  case (z_)
     !\
     ! z face
     !/
     !\
     ! B0 on the face
     !/
     B0n  = B0z
     B0t1 = B0x
     B0t2 = B0y
     !\
     ! Left face
     !/
     RhoL  =  StateLeft_V(rho_)
     UnL   =  StateLeft_V(Uz_ )
     Ut1L  =  StateLeft_V(Ux_ )
     Ut2L  =  StateLeft_V(Uy_ )
     B1nL  =  StateLeft_V(Bz_ )
     B1t1L =  StateLeft_V(Bx_ )
     B1t2L =  StateLeft_V(By_ )
     pL    =  StateLeft_V(P_  )
     !\
     ! Right face
     !/
     RhoR  =  StateRight_V(rho_)
     UnR   =  StateRight_V(Uz_ )
     Ut1R  =  StateRight_V(Ux_ )
     Ut2R  =  StateRight_V(Uy_ )
     B1nR  =  StateRight_V(Bz_ )
     B1t1R =  StateRight_V(Bx_ )
     B1t2R =  StateRight_V(By_ )
     pR    =  StateRight_V(P_  )

     dCons_V(Rho_)    = StateRightCons_V(Rho_)   - StateLeftCons_V(Rho_)
     dCons_V(RhoUn_ ) = StateRightCons_V(RhoUz_) - StateLeftCons_V(RhoUz_)
     dCons_V(RhoUt1_) = StateRightCons_V(RhoUx_) - StateLeftCons_V(RhoUx_)
     dCons_V(RhoUt2_) = StateRightCons_V(RhoUy_) - StateLeftCons_V(RhoUy_)
     dCons_V(B1n_)    = StateRightCons_V(Bz_)    - StateLeftCons_V(Bz_)
     dCons_V(B1t1_)   = StateRightCons_V(Bx_)    - StateLeftCons_V(Bx_)
     dCons_V(B1t2_)   = StateRightCons_V(By_)    - StateLeftCons_V(By_)
     ! Put the conservative energy difference in place of the pressure
     dCons_V(p_)      = StateRightCons_V(Energy_)- StateLeftCons_V(Energy_)

     UseFluxRusanov= true_cell(iFace, jFace, kFace-1, globalBLK) &
          .neqv.     true_cell(iFace, jFace, kFace  , globalBLK)

  end select

  RhoInvL = 1./RhoL
  BnL  = B0n+B1nL
  Bt1L = B0t1+B1t1L
  Bt2L = B0t2+B1t2L
  BbL  = BnL**2 + Bt1L**2 + Bt2L**2
  aL   = g*pL*RhoInvL

  RhoInvR = 1./RhoR
  BnR  = B0n+B1nR
  Bt1R = B0t1+B1t1R
  Bt2R = B0t2+B1t2R
  BbR  = BnR**2 + Bt1R**2 + Bt2R**2
  aR   = g*pR*RhoInvR

  !\
  ! Hat face
  !/
  RhoH = 0.5*(RhoL + RhoR)
  RhoInvH = 1./RhoH
  UnH  = 0.5*(  UnL +   UnR)
  Ut1H = 0.5*( Ut1L +  Ut1R)
  Ut2H = 0.5*( Ut2L +  Ut2R)
  BnH  = 0.5*(  BnL +   BnR)
  Bt1H = 0.5*( Bt1L +  Bt1R)
  Bt2H = 0.5*( Bt2L +  Bt2R)
  B1nH = 0.5*( B1nL +  B1nR)
  B1t1H= 0.5*(B1t1L + B1t1R)
  B1t2H= 0.5*(B1t2L + B1t2R)
  pH   = 0.5*(   pL +    pR)
  BbH  = BnH**2  + Bt1H**2  + Bt2H**2
  Bb1H = B1nH**2 + B1t1H**2 + B1t2H**2
  aH   = g*pH*RhoInvH

  !if(aL<0.0)then
  !   write(*,*)'NEGATIVE aL Me, iDir, i, j, k, globalBLK',&
  !        aL,iProc,iDir,i,j,k,&
  !        x_BLK(i,j,k,globalBLK),&
  !        y_BLK(i,j,k,globalBLK),&
  !        z_BLK(i,j,k,globalBLK)
  !   call stop_mpi
  !end if
  aL=sqrt(aL)
  aR=sqrt(aR)
  aH=sqrt(aH)

  eL = aL*aL + BbL*RhoInvL
  CfL = max(0., (eL**2 - 4.*aL**2 * BnL**2 * RhoInvL))
  eR = aR**2 + BbR*RhoInvR
  CfR = max(0., (eR**2 - 4.*aR**2 * BnR**2 * RhoInvR))
  eH = aH**2 + BbH*RhoInvH
  CfH = max(0., (eH**2 - 4.*aH**2 * BnH**2 * RhoInvH))

  CfL=sqrt(CfL)
  CfR=sqrt(CfR)
  CfH=sqrt(CfH)

  CsL  = max(0.,0.5*(eL-CfL))
  CfL  = 0.5*(eL+CfL)

  CsR  = max(0.,0.5*(eR-CfR))
  CfR  = 0.5*(eR+CfR)

  CsH  = max(0.,0.5*(eH-CfH))
  CfH  = 0.5*(eH+CfH)
  UuH  = UnH**2 + Ut1H**2 + Ut2H**2
  eH   = pH*inv_gm1 + 0.5*RhoH*UuH + 0.5*Bb1H

  CsL=sqrt(CsL)
  CsR=sqrt(CsR)
  CsH=sqrt(CsH)
  CfL=sqrt(CfL)
  CfR=sqrt(CfR)
  CfH=sqrt(CfH)

  CsL  = min(CsL,aL)
  CfL  = max(CfL,aL)
  CsR  = min(CsR,aR)
  CfR  = max(CfR,aR)
  CsH  = min(CsH,aH)
  CfH  = max(CfH,aH)
  !\
  ! Non-dimensional scaling factors
  !/
  Tmp1 = max(1.00e-08, Bt1H**2 + Bt2H**2)
  Tmp1=sqrt(1./Tmp1)

  if (Tmp1 < 1.0e04) then
     BetaY = Bt1H*Tmp1
     BetaZ = Bt2H*Tmp1
  else
     BetaY = cSqrtHalf
     BetaZ = cSqrtHalf
  end if

  Tmp1 = CfH**2 - CsH**2
  if (Tmp1 > 1.0e-08) then
     AlphaF = max(0.00,(aH**2  - CsH**2)/Tmp1)
     AlphaS = max(0.00,(CfH**2 - aH**2 )/Tmp1)

     AlphaF = sqrt(AlphaF)
     AlphaS = sqrt(AlphaS)
  else if (BnH**2 * RhoInvH <= aH**2 ) then
     AlphaF = 1.00
     AlphaS = 0.00
  else
     AlphaF = 0.00
     AlphaS = 1.00
  endif

  !\
  ! Set some values that are reused over and over
  !/

  RhoSqrtH   =sqrt(RhoH)
  RhoSqrtL   =sqrt(RhoL)
  RhoSqrtR   =sqrt(RhoR)
  RhoInvSqrtH=1./RhoSqrtH
  RhoInvSqrtL=1./RhoSqrtL
  RhoInvSqrtR=1./RhoSqrtR


  SignBnH     = sign(1.,BnH)
  Gamma1A2Inv = gm1 / aH**2

  !\
  ! Eigenvalues
  !/
  Eigenvalue_V(EntropyW_) = UnH
  Eigenvalue_V(AlfvenRW_) = UnH + BnH*RhoInvSqrtH
  Eigenvalue_V(AlfvenLW_) = UnH - BnH*RhoInvSqrtH
  Eigenvalue_V(SlowRW_)   = UnH + CsH
  Eigenvalue_V(FastRW_)   = UnH + CfH
  Eigenvalue_V(SlowLW_)   = UnH - CsH
  Eigenvalue_V(FastLW_)   = UnH - CfH
  Eigenvalue_V(DivBW_)    = UnH

  !\
  ! Entropy fix for Eigenvalues
  !/
  Tmp1 = (UnR) - (UnL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(1)) < Tmp1*0.5) then
     Eigenvalue_V(1) = sign(1.,Eigenvalue_V(1))*   &
          ((Eigenvalue_V(1)*Eigenvalue_V(1)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR+BnR*RhoInvSqrtR) - &
       (UnL+BnL*RhoInvSqrtL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(2)) < Tmp1*0.5) then
     Eigenvalue_V(2) = sign(1.,Eigenvalue_V(2))*   &
          ((Eigenvalue_V(2)*Eigenvalue_V(2)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR-BnR*RhoInvSqrtR) - &
       (UnL-BnL*RhoInvSqrtL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(3)) < Tmp1*0.5) then
     Eigenvalue_V(3) = sign(1.,Eigenvalue_V(3))*   &
          ((Eigenvalue_V(3)*Eigenvalue_V(3)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR+CsR) - (UnL+CsL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(4)) < Tmp1*0.5) then
     Eigenvalue_V(4) = sign(1.,Eigenvalue_V(4))*   &
          ((Eigenvalue_V(4)*Eigenvalue_V(4)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR+CfR) - (UnL+CfL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(5)) < Tmp1*0.5) then
     Eigenvalue_V(5) = sign(1.,Eigenvalue_V(5))*   &
          ((Eigenvalue_V(5)*Eigenvalue_V(5)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR-CsR) - (UnL-CsL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(6)) < Tmp1*0.5) then
     Eigenvalue_V(6) = sign(1.,Eigenvalue_V(6))*   &
          ((Eigenvalue_V(6)*Eigenvalue_V(6)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR-CfR) - (UnL-CfL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(7)) < Tmp1*0.5) then
     Eigenvalue_V(7) = sign(1.,Eigenvalue_V(7))*   &
          ((Eigenvalue_V(7)*Eigenvalue_V(7)/Tmp1) + Tmp1*0.25)
  end if

  Tmp1 = (UnR) - (UnL)
  Tmp1 = max(0.,4.*Tmp1)
  if (abs(Eigenvalue_V(8)) < Tmp1*0.5) then
     Eigenvalue_V(8) = sign(1.,Eigenvalue_V(8))*   &
          ((Eigenvalue_V(8)*Eigenvalue_V(8)/Tmp1) + Tmp1*0.25)
  end if

  !\
  ! Timur's divergence wave fix!!!
  !/
  !original  version
  !      Eigenvalue_V(8)=abs(Eigenvalue_V(8))+aH
  !
  !Enhanced diffusion, the maximum eigenvalue
  Eigenvalue_V(DivbW_) = max( Eigenvalue_V(FastRW_), &
       -Eigenvalue_V(FastLW_))

  ! The original version was proposed by Timur Linde for heliosphere
  ! simulations. Enhanced version was found to be more robust on 8/20/01
  ! The original version was commented out in versions 6x resulting in 
  ! worse stability for the Roe solver.

  ! At inner BC replace all eigenvalues with the enhanced eigenvalue
  ! of divB, which is the maximum eigenvalue
  if(UseFluxRusanov) Eigenvalue_V(1:nWave) = Eigenvalue_V(DivBW_)

  !\
  ! Eigenvectors
  !/
  Tmp1=1./(2.*RhoH*aH**2)
  Tmp2=RhoInvH*cSqrtHalf
  Tmp3=RhoInvSqrtH*cSqrtHalf

  ! Left eigenvector for Entropy wave
  EigenvectorL_VV(1,1) = 1.-0.5*Gamma1A2Inv*UuH
  EigenvectorL_VV(2,1) = Gamma1A2Inv*UnH
  EigenvectorL_VV(3,1) = Gamma1A2Inv*Ut1H
  EigenvectorL_VV(4,1) = Gamma1A2Inv*Ut2H
  EigenvectorL_VV(5,1) = Gamma1A2Inv*B1nH
  EigenvectorL_VV(6,1) = Gamma1A2Inv*B1t1H
  EigenvectorL_VV(7,1) = Gamma1A2Inv*B1t2H
  EigenvectorL_VV(8,1) = -Gamma1A2Inv

  ! Left eigenvector for Alfven wave +
  EigenvectorL_VV(1,2) = (Ut1H*BetaZ-  &
       Ut2H*BetaY)*Tmp2
  EigenvectorL_VV(2,2) = 0.
  EigenvectorL_VV(3,2) = -(BetaZ*Tmp2)
  EigenvectorL_VV(4,2) = (BetaY*Tmp2)
  EigenvectorL_VV(5,2) = 0.
  EigenvectorL_VV(6,2) = (BetaZ*Tmp3)
  EigenvectorL_VV(7,2) = -(BetaY*Tmp3)
  EigenvectorL_VV(8,2) = 0.

  ! Left eigenvector for Alfven wave -
  EigenvectorL_VV(1,3) = (Ut1H*BetaZ-  &
       Ut2H*BetaY)*Tmp2
  EigenvectorL_VV(2,3) = 0.
  EigenvectorL_VV(3,3) = -(BetaZ*Tmp2)
  EigenvectorL_VV(4,3) = (BetaY*Tmp2)
  EigenvectorL_VV(5,3) = 0.
  EigenvectorL_VV(6,3) = -(BetaZ*Tmp3)
  EigenvectorL_VV(7,3) = (BetaY*Tmp3)
  EigenvectorL_VV(8,3) = 0.

  ! Left eigenvector for Slow magnetosonic wave +
  EigenvectorL_VV(1,4) = Tmp1* &
       (AlphaS*(gm1*UuH/2. - &
       UnH*CsH) - &
       AlphaF*CfH*SignBnH* &
       (Ut1H*BetaY+ &
       Ut2H*BetaZ))
  EigenvectorL_VV(2,4) = Tmp1* &
       (AlphaS*(-UnH*gm1+CsH))
  EigenvectorL_VV(3,4) = Tmp1* &
       (-gm1*AlphaS*Ut1H + &
       AlphaF*CfH*BetaY*SignBnH)
  EigenvectorL_VV(4,4) = Tmp1* &
       (-gm1*AlphaS*Ut2H + &
       AlphaF*CfH*BetaZ*SignBnH)
  EigenvectorL_VV(5,4) = Tmp1* &
       (-gm1*B1nH*AlphaS)
  EigenvectorL_VV(6,4) = Tmp1* &
       (-AlphaF*BetaY*aH* &
       RhoSqrtH-gm1*B1t1H*AlphaS)
  EigenvectorL_VV(7,4) = Tmp1* &
       (-AlphaF*BetaZ*aH* &
       RhoSqrtH-gm1*B1t2H*AlphaS)
  EigenvectorL_VV(8,4) = Tmp1* &
       (gm1*AlphaS)

  ! Left eigenvector for Fast magnetosonic wave +
  EigenvectorL_VV(1,5) = Tmp1* &
       (AlphaF*(gm1*UuH/2. - &
       UnH*CfH)+AlphaS* &
       CsH*SignBnH* &
       (Ut1H*BetaY + &
       Ut2H*BetaZ))
  EigenvectorL_VV(2,5) = Tmp1* &
       (AlphaF*(-UnH*gm1+CfH))
  EigenvectorL_VV(3,5) = Tmp1* &
       (-gm1*AlphaF*Ut1H - &
       AlphaS*CsH*BetaY*SignBnH)
  EigenvectorL_VV(4,5) = Tmp1* &
       (-gm1*AlphaF*Ut2H - &
       AlphaS*CsH*BetaZ*SignBnH)
  EigenvectorL_VV(5,5) = Tmp1* &
       (-gm1*B1nH*AlphaF)
  EigenvectorL_VV(6,5) = Tmp1* &
       (AlphaS*BetaY*aH* &
       RhoSqrtH-gm1*B1t1H*AlphaF)
  EigenvectorL_VV(7,5) = Tmp1*                   &
       (AlphaS*BetaZ*aH* &
       RhoSqrtH-gm1*B1t2H*AlphaF)
  EigenvectorL_VV(8,5) = Tmp1* &
       (gm1*AlphaF)

  ! Left eigenvector for Slow magnetosonic wave -
  EigenvectorL_VV(1,6) = Tmp1* &
       (AlphaS*(gm1*UuH/2. + &
       UnH*CsH) + &
       AlphaF*CfH*SignBnH* &
       (Ut1H*BetaY + &
       Ut2H*BetaZ))
  EigenvectorL_VV(2,6) = Tmp1*(AlphaS*(-UnH*gm1-CsH))
  EigenvectorL_VV(3,6) = Tmp1*(-gm1*AlphaS*Ut1H - AlphaF*CfH*BetaY*SignBnH)
  EigenvectorL_VV(4,6) = Tmp1*(-gm1*AlphaS*Ut2H - AlphaF*CfH*BetaZ*SignBnH)
  EigenvectorL_VV(5,6) = Tmp1*(-gm1*B1nH*AlphaS)
  EigenvectorL_VV(6,6) = Tmp1*(-AlphaF*BetaY*aH*RhoSqrtH-gm1*B1t1H*AlphaS)
  EigenvectorL_VV(7,6) = Tmp1*(-AlphaF*BetaZ*aH*RhoSqrtH-gm1*B1t2H*AlphaS)
  EigenvectorL_VV(8,6) = Tmp1*(gm1*AlphaS)

  ! Left eigenvector for Fast magnetosonic wave -
  EigenvectorL_VV(1,7) = Tmp1* &
       (AlphaF*(gm1*UuH/2. + UnH*CfH) - &
       AlphaS*CsH*SignBnH* &
       (Ut1H*BetaY + Ut2H*BetaZ))
  EigenvectorL_VV(2,7) = Tmp1*(AlphaF*(-UnH*gm1-CfH))
  EigenvectorL_VV(3,7) = Tmp1*(-gm1*AlphaF*Ut1H + AlphaS*CsH*BetaY*SignBnH)
  EigenvectorL_VV(4,7) = Tmp1*(-gm1*AlphaF*Ut2H + AlphaS*CsH*BetaZ*SignBnH)
  EigenvectorL_VV(5,7) = Tmp1*(-gm1*B1nH*AlphaF)
  EigenvectorL_VV(6,7) = Tmp1*(AlphaS*BetaY*aH*RhoSqrtH-gm1*B1t1H*AlphaF)
  EigenvectorL_VV(7,7) = Tmp1*(AlphaS*BetaZ*aH*RhoSqrtH-gm1*B1t2H*AlphaF)
  EigenvectorL_VV(8,7) = Tmp1*(gm1*AlphaF)

  ! Left eigenvector for Divergence wave
  EigenvectorL_VV(1,8) = 0.
  EigenvectorL_VV(2,8) = 0.
  EigenvectorL_VV(3,8) = 0.
  EigenvectorL_VV(4,8) = 0.
  EigenvectorL_VV(5,8) = 1.
  EigenvectorL_VV(6,8) = 0.
  EigenvectorL_VV(7,8) = 0.
  EigenvectorL_VV(8,8) = 0.

  !coefficient for pressure component of the Right vector
  Tmp1=g*max(pL,pR) 

  !Pressure component is not linearly independent and obeys the 
  ! equation as follows:
  !EigenvectorR_VV(1:8,9)=(0.5*UuH*EigenvectorR_VV(1:8,1)-&
  !                     UnH*EigenvectorR_VV(1:8,2)-&
  !                     Ut1H*EigenvectorR_VV(1:8,3)-&
  !                     Ut2H*EigenvectorR_VV(1:8,4)-&
  !                     B1nH*EigenvectorR_VV(1:8,5)-&
  !                     B1t1H*EigenvectorR_VV(1:8,6)-&
  !                     B1t2H*EigenvectorR_VV(1:8,7)+
  !                     EigenvectorR_VV(1:8,8))*inv_gm1         

  ! Right eigenvector for Entropy wave
  EigenvectorR_VV(1,1) = 1.
  EigenvectorR_VV(1,2) = UnH
  EigenvectorR_VV(1,3) = Ut1H
  EigenvectorR_VV(1,4) = Ut2H
  EigenvectorR_VV(1,5) = 0.
  EigenvectorR_VV(1,6) = 0.
  EigenvectorR_VV(1,7) = 0.
  EigenvectorR_VV(1,Energy_) = 0.5*UuH
  EigenvectorR_VV(1,P_)=cZero

  ! Right eigenvector for Alfven wave +
  EigenvectorR_VV(2,1) = 0.
  EigenvectorR_VV(2,2) = 0.
  EigenvectorR_VV(2,3) = -BetaZ*RhoH*cSqrtHalf
  EigenvectorR_VV(2,4) = BetaY*RhoH*cSqrtHalf
  EigenvectorR_VV(2,5) = 0.
  EigenvectorR_VV(2,6) = BetaZ*RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(2,7) = -BetaY*RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(2,Energy_) = (BetaY*Ut2H - &
       BetaZ*Ut1H)*RhoH*cSqrtHalf &
       +(B1t1H*BetaZ-B1t2H*BetaY)* &
       RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(2,P_)=cZero

  ! Right eigenvector for Alfven wave -
  EigenvectorR_VV(3,1) = 0.
  EigenvectorR_VV(3,2) = 0.
  EigenvectorR_VV(3,3) = -BetaZ*RhoH*cSqrtHalf
  EigenvectorR_VV(3,4) = BetaY*RhoH*cSqrtHalf
  EigenvectorR_VV(3,5) = 0.
  EigenvectorR_VV(3,6) = -BetaZ*RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(3,7) = BetaY*RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(3,Energy_) = (BetaY*Ut2H - &
       BetaZ*Ut1H)*RhoH*cSqrtHalf &
       -(B1t1H*BetaZ-B1t2H*BetaY)* &
       RhoSqrtH*cSqrtHalf
  EigenvectorR_VV(3,P_)=cZero

  ! Right eigenvector for Slow magnetosonic wave +
  EigenvectorR_VV(4,1) = RhoH*AlphaS
  EigenvectorR_VV(4,2) = RhoH*AlphaS*(UnH+CsH)
  EigenvectorR_VV(4,3) = RhoH*(AlphaS*Ut1H + AlphaF*CfH*BetaY*SignBnH)
  EigenvectorR_VV(4,4) = RhoH*(AlphaS*Ut2H + AlphaF*CfH*BetaZ*SignBnH)
  EigenvectorR_VV(4,5) = 0.
  EigenvectorR_VV(4,6) = -AlphaF*aH*BetaY*RhoSqrtH
  EigenvectorR_VV(4,7) = -AlphaF*aH*BetaZ*RhoSqrtH
  EigenvectorR_VV(4,Energy_) = AlphaS*(RhoH*UuH*0.5 + &
       g*pH*inv_gm1+RhoH*UnH* &
       CsH)-AlphaF*(aH* &
       RhoSqrtH*(BetaY*B1t1H + &
       BetaZ*B1t2H)-RhoH* &
       CfH*SignBnH* &
       (Ut1H*BetaY+Ut2H*BetaZ))
  EigenvectorR_VV(4,P_)=Tmp1*AlphaS

  ! Right eigenvector for Fast magnetosonic wave +
  EigenvectorR_VV(5,1) = RhoH*AlphaF
  EigenvectorR_VV(5,2) = RhoH*AlphaF* (UnH+CfH)
  EigenvectorR_VV(5,3) = RhoH* (AlphaF*Ut1H - AlphaS*CsH*BetaY*SignBnH)
  EigenvectorR_VV(5,4) = RhoH* (AlphaF*Ut2H - AlphaS*CsH*BetaZ*SignBnH)
  EigenvectorR_VV(5,5) = 0.
  EigenvectorR_VV(5,6) = AlphaS*aH*BetaY*RhoSqrtH
  EigenvectorR_VV(5,7) = AlphaS*aH*BetaZ*RhoSqrtH
  EigenvectorR_VV(5,Energy_) = AlphaF*(RhoH*UuH*0.5 + &
       g*pH*inv_gm1+RhoH*UnH* &
       CfH)+AlphaS*(aH* &
       RhoSqrtH*(BetaY*B1t1H+ &
       BetaZ*B1t2H)-RhoH* &
       CsH*SignBnH* &
       (Ut1H*BetaY+Ut2H*BetaZ))
  EigenvectorR_VV(5,P_)=Tmp1*AlphaF

  ! Right eigenvector for Slow magnetosonic wave -
  EigenvectorR_VV(6,1) = RhoH*AlphaS
  EigenvectorR_VV(6,2) = RhoH*AlphaS*(UnH-CsH)
  EigenvectorR_VV(6,3) = RhoH* (AlphaS*Ut1H - AlphaF*CfH*BetaY*SignBnH)
  EigenvectorR_VV(6,4) = RhoH* (AlphaS*Ut2H - AlphaF*CfH*BetaZ*SignBnH)
  EigenvectorR_VV(6,5) = 0.
  EigenvectorR_VV(6,6) = - AlphaF*aH*BetaY*RhoSqrtH
  EigenvectorR_VV(6,7) = - AlphaF*aH*BetaZ*RhoSqrtH
  EigenvectorR_VV(6,Energy_) = AlphaS*(RhoH*UuH*0.5 + &
       g*pH*inv_gm1-RhoH*UnH* &
       CsH)-AlphaF*(aH* &
       RhoSqrtH*(BetaY*B1t1H+ &
       BetaZ*B1t2H)+RhoH* &
       CfH*SignBnH* &
       (Ut1H*BetaY+Ut2H*BetaZ))
  EigenvectorR_VV(6,P_)=Tmp1*AlphaS

  ! Right eigenvector for Fast magnetosonic wave -
  EigenvectorR_VV(7,1) = RhoH*AlphaF
  EigenvectorR_VV(7,2) = RhoH*AlphaF* (UnH-CfH)
  EigenvectorR_VV(7,3) = RhoH* &
       (AlphaF*Ut1H + AlphaS*CsH*BetaY*SignBnH)
  EigenvectorR_VV(7,4) = RhoH* &
       (AlphaF*Ut2H + AlphaS*CsH*BetaZ*SignBnH)
  EigenvectorR_VV(7,5) = 0.
  EigenvectorR_VV(7,6) = AlphaS*aH*BetaY*RhoSqrtH
  EigenvectorR_VV(7,7) = AlphaS*aH*BetaZ*RhoSqrtH
  EigenvectorR_VV(7,Energy_) = AlphaF*(RhoH*UuH*0.5 + &
       g*pH*inv_gm1-RhoH*UnH* &
       CfH)+AlphaS*(aH* &
       RhoSqrtH*(BetaY*B1t1H + &
       BetaZ*B1t2H)+RhoH* &
       CsH*SignBnH* &
       (Ut1H*BetaY+Ut2H*BetaZ))
  EigenvectorR_VV(7,P_)=Tmp1*AlphaF

  ! Right eigenvector for Divergence wave
  EigenvectorR_VV(8,1) = 0.
  EigenvectorR_VV(8,2) = 0.
  EigenvectorR_VV(8,3) = 0.
  EigenvectorR_VV(8,4) = 0.
  EigenvectorR_VV(8,5) = 1.
  EigenvectorR_VV(8,6) = 0.
  EigenvectorR_VV(8,7) = 0.
  EigenvectorR_VV(8,Energy_) = B1nH
  EigenvectorR_VV(8,P_) = cZero

  !\
  ! Alphas (elemental wave strengths)
  !/
  ! matmul is slower than the loop for the NAG F95 compiler
  ! DeltaWave_V = matmul(dCons_V, EigenvectorL_VV)
  do iWave = 1, nWave
     DeltaWave_V(iWave) = sum(dCons_V*EigenvectorL_VV(:,iWave))
  end do
  !\
  ! Calculate the Roe Interface fluxes 
  ! F = A * 0.5 * [ F_L+F_R - sum_k(|lambda_k| * alpha_k * r_k) ]
  !/
  ! First get the diffusion: sum_k(|lambda_k| * alpha_k * r_k)
  !  Diffusion_V = matmul(abs(Eigenvalue_V)*DeltaWave_V, EigenvectorR_VV)
  do iFlux = 1, nFlux
     Diffusion_V(iFlux) = &
          sum(abs(Eigenvalue_V)*DeltaWave_V*EigenvectorR_VV(:,iFlux))
  end do

  ! Rotate n,t1,t2 components back to x,y,z components
  select case (iDir)
  case (x_)
     Flux_V(rho_   ) = Diffusion_V(1)
     Flux_V(rhoUx_ ) = Diffusion_V(2)
     Flux_V(rhoUy_ ) = Diffusion_V(3)
     Flux_V(rhoUz_ ) = Diffusion_V(4)
     Flux_V(Bx_    ) = Diffusion_V(5)
     Flux_V(By_    ) = Diffusion_V(6)
     Flux_V(Bz_    ) = Diffusion_V(7)
     Flux_V(P_     ) = Diffusion_V(8)
     Flux_V(Energy_) = Diffusion_V(9)

  case (y_)

     Flux_V(rho_   ) = Diffusion_V(1)
     Flux_V(rhoUx_ ) = Diffusion_V(4)
     Flux_V(rhoUy_ ) = Diffusion_V(2)
     Flux_V(rhoUz_ ) = Diffusion_V(3)
     Flux_V(Bx_    ) = Diffusion_V(7)
     Flux_V(By_    ) = Diffusion_V(5)
     Flux_V(Bz_    ) = Diffusion_V(6)
     Flux_V(P_     ) = Diffusion_V(8)
     Flux_V(Energy_) = Diffusion_V(9)

  case (z_)

     Flux_V(rho_   ) = Diffusion_V(1)
     Flux_V(rhoUx_ ) = Diffusion_V(3)
     Flux_V(rhoUy_ ) = Diffusion_V(4)
     Flux_V(rhoUz_ ) = Diffusion_V(2)
     Flux_V(Bx_    ) = Diffusion_V(6)
     Flux_V(By_    ) = Diffusion_V(7)
     Flux_V(Bz_    ) = Diffusion_V(5)
     Flux_V(P_     ) = Diffusion_V(8)
     Flux_V(Energy_) = Diffusion_V(9)

  end select

  Flux_V  = 0.5*(FluxLeft_V + FluxRight_V - Area*Flux_V)

  Unormal = Area*UnH
  Cmax    = Area*(abs(UnH) + CfH)

end subroutine roe_solver
!^CFG END ROEFLUX
