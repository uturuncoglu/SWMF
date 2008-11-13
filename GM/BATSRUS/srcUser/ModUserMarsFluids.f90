module ModUser
  ! This is the user module for Mars 

  use ModSize
  use ModUserEmpty,               &
       IMPLEMENTED1 => user_set_ics,                    &
       IMPLEMENTED2 => user_calc_sources,               &
       IMPLEMENTED3 => user_init_point_implicit,        &
       IMPLEMENTED4 => user_init_session,               &
       IMPLEMENTED5 => user_read_inputs,                &
       IMPLEMENTED6 => user_face_bcs,                   &
       IMPLEMENTED7 => user_set_plot_var,               &
       IMPLEMENTED8 => user_set_boundary_cells

  use ModMultiFluid

  use ModNumConst, ONLY: cSqrtHalf, cDegToRad

  include 'user_module.h' !list of public methods

  !\
  ! Here you must define a user routine Version number and a 
  ! descriptive string.
  !/
  real,              parameter :: VersionUserModule = 1.2
  character (len=*), parameter :: NameUserModule = &
       'Mars 5 Fluids MHD code, Dalal Najib'

  real    :: CollisionCoefDim = 1.0, CollisionCoef
  ! Radius within which the point implicit scheme should be used
  real :: rPointImplicit = 2.0

  !Mars stuff etc

  !logical ::  UseMultiSpecies=.true.
  integer, parameter :: MaxSpecies=nIonFluid, MaxNuSpecies=8,  &
       MaxReactions=10
  integer :: nSpecies=4, nNuSpecies=3, &
       nReactions=10

  real, allocatable:: nDenNuSpecies_CBI(:,:,:,:,:), &
       TempNuSpecies_CBI(:,:,:,:),  Productrate_CB(:,:,:,:),  &
       Ionizationrate_CBI (:,:,:,:,:), MaxSiSpecies_CB(:,:,:,:),&
       MaxLiSpecies_CB(:,:,:,:),MaxSLSpecies_CB(:,:,:,:),&
       nu_BLK(:,:,:,:),nu1_BLK(:,:,:,:)

  real, dimension(MaxReactions) :: ReactionRate_I
  real, dimension(MaxReactions,nIonFluid):: CoeffSpecies_II, &
       dSdRho_II !, dLdRho_II
  real, dimension(nIonFluid)::LossSpecies_I, &
       SiSpecies_I,  LiSpecies_I,  PhoIon_I, Recb_I


  !the reactions considered:(p means ion, em means electron)
  !the prefered order of a reaction is ions, Nus, hv and electrons
  integer, parameter :: &!reaction number
       CO2_hv__CO2p_em_=1 ,&!CO2+hv-->CO2p+em  
       O_hv__Op_em_    =2 ,&!O+hv-->Op+em       
       CO2p_O__O2p_CO_ =3 ,&!CO2p+O-->O2p+CO   
       Op_CO2__O2p_CO_ =4 ,&!Op+CO2-->O2p+CO
       CO2p_O__Op_CO2_ =5 ,&!CO2p+O-->Op+CO2
       O2p_em__O_O_    =6 ,&!O2p+em-->O+O 
       CO2p_em__CO_O_  =7 ,&!CO2p+em-->CO+O
       Hp_O__Op_H_     =8 ,&!Hp+O-->Op+H
       Op_H__Hp_O_     =9 ,&!Op+H-->Hp+O   
       H_hv__Hp_em_    =10  !H+hv-->Hp+em

  real, dimension(MaxReactions) :: Rate_I
  real, dimension(MaxReactions) :: &
       Ratedim_I=(/ 2.47e-7, 8.89e-8, 1.64e-10, 1.1e-9, &
       9.60e-11, 7.38e-8, 3.1e-7, 5.084e-10, 6.4e-10, 5.58e-8 /)  !cm^3 s^(-1)

  integer, parameter :: &! order of ion species
       Hp_  =1, &
       O2p_ =2, &
       Op_  =3, &
       CO2p_=4

  character (len=10), dimension(nIonFluid):: &
       ion_name_I=(/'Hp  ', 'O2p ', 'Op  ','CO2p'/)

  ! real, dimension(MaxSpecies)::  &
  !      MassSpecies_I=(/1., 32., 16., 44. /)  !atm

  !  MassSpecies_I(Hp_)=1	 !atm
  !  MassSpecies_I(CO2p_)=44     !atm
  !  MassSpecies_I(O2p_)=32      !atm
  !  MassSpecies_I(Op_)=16       !atm

  integer, parameter :: & ! order of Neutral species
       CO2_=1 ,&
       O_=2   ,&    
       H_=3, &
       Oh_=4   ,&
       Ohx_=5 , &
       Hx_=6, &
       Ox_=7 ,&
       CO2x_=8   

  real, dimension(MaxNuSpecies)::CrossSection_I,&
       CrossSectionDim_I=(/2.6e-17,1.5e-17,0.0,1.5e-17,&
       1.5e-17,0.0,1.5e-17,2.6e-17/)

  real:: Productrate0,Optdep
  real, dimension(MaxNuSpecies)::NuMassSpecies_I=(/44,16,1,16,16,1,16, 44/)
  !  NuMassSpecies_I(CO2_)=44	!atm
  !  NuMassSpecies_I(O_)=16	!atm

  real, dimension(MaxNuSpecies):: HNuSpecies_I=1.0,HNuSpeciesDim_I=1.0
  !HNuSpecies_dim_I(CO2_)=6.7e3   !m
  !HNuSpecies_dim_I(O_)=18.4e3    !m

  real, dimension(MaxNuSpecies):: BodynDenNuSpecies_I,&
       BodynDenNuSpDim_I=(/1.1593e12, 3.2278e9, 1.1307e7, 1.951e4, &
       1.5248e3, 9.4936e5, 5.2695e8, 2.2258e11/)

  real, dimension(nIonFluid):: BodyRhoSpecies_I
  integer, parameter :: & ! other numbers
       em_=-1 ,&
       hv_=-2   

  real :: TNu_body_dim = 300.0, TNu_body, Tnu, Tnu_dim ! neutral temperature 
  real :: Ti_body_dim=300.0, Ti_body  !ion temperature at the body
  real :: Tp_body_dim=600.0, Tp_body  !dimensionless temperature 
  !of new created ions / plasma (Tp_body=2.0*Ti_body)

  real :: T300_dim = 300.0, T300 , Ti_dim =300., Tp
  real :: nu0_dim=4.0e-10,nu0

  real :: Te_new_dim=1000., KTe0

  ! coefficient of Mars magnetic field
  real, dimension(0:61,0:61) :: cmars, dmars
  integer :: NNm
  real :: mars_eps=1e-4
  logical :: UseMarsB0 = .false.
  real :: rot = 1.0, thetilt = 0.0
  logical :: UseHotO = .false.
  logical :: UseTempCont=.false.
  logical :: UseSolarMax=.false.
  logical :: UseImpactIon=.false.
  real, dimension(32,nIonFluid)::Impact_ION,Impact_ION_dim=0.0 
  real, dimension(32):: Temp_dim
  logical :: UseChargeEx=.true.

  integer,parameter::NLong=73, NLat=36, MaxAlt=21
  real :: Long_I(NLong), Lat_I(NLat), Alt_I(MaxAlt)
  real :: Temp(NLong, NLat, MaxAlt)
  real :: Den_CO2(NLong, NLat, MaxAlt)!,Den_CO2_dim(NLong, NLat, NAlt)
  real :: Den_O(NLong, NLat, MaxAlt)!,Den_O_dim(NLong, NLat, NAlt)
  real :: ICO2p(NLong, NLat, MaxAlt)!,ICO2p_dim(NLong, NLat, NAlt)
  real :: IOp(NLong, NLat, MaxAlt)!,IOp_dim(NLong, NLat, NAlt)
  logical ::UseMarsAtm=.false.
  integer :: NAlt=21


  !end mars stuff

  logical:: UseOldEnergy=.false.

contains

  !============================================================================
  subroutine user_calc_sources
    use ModPointImplicit, ONLY: UsePointImplicit_B,UsePointImplicit,&
         IsPointImplSource, iVarPointImpl_I, IsPointImplMatrixSet, DsDu_VVC
    use ModMain,    ONLY: GlobalBlk, nI, nJ, nK,&
         PROCTEST,GLOBALBLK,BLKTEST, iTest,jTest,kTest
    use ModPhysics, ONLY: inv_gm1,Rbody,gm1,UnitTemperature_,Si2No_V
    use ModAdvance, ONLY: State_VGB, Source_VC,VdtFace_x,&
         VdtFace_y,VdtFace_z
    use ModGeometry,ONLY: r_BLK,x_BLK,y_BLK,z_BLK,R_BLK,&
         vInv_CB
    use ModNumConst,ONLY: cZero,cHalf,cOne,cTolerance
!!$    use ModVarIndexes,ONLY: Rho_, HpRho_, O2pRho_, OpRho_, CO2pRho_, &
!!$         RhoUx_, RhoUy_, RhoUz_, HpP_,O2pP_,OpP_,CO2pP_, P_, Energy_, Bx_, By_, Bz_
    use ModVarIndexes
    use ModMain,     ONLY: iTest, jTest, kTest, ProcTest, BlkTest
    use ModProcMH,   ONLY: iProc
    !    use ModAdvance,  ONLY: Source_VC,Energy_
    !    use ModNumConst, ONLY: cZero
    integer :: iBlock, i, j, k,iSpecies,iFluid
    real    :: Coef,SourcesLossMax,vdtmin
    real :: inv_rho,inv_rho2,uu2,Productrate,kTi,kTe
    real    :: NumDens, InvNumDens
    real, dimension(nIonFluid) :: NumDens_I, InvRho_I,InvRho2_I,uu2_I,&
         Ux_I, Uy_I, Uz_I, Temp_I,temps_I,&
         LossNumRho_I, SourceNumRho_I, Lossx_I,LossNumx_I,&
         RLNumRhox_I, tmp_I
    real :: alt, Te_dim = 300.0, tmp
    real:: totalNumRho, totalLossRho,totalSourceRho, totalLossNumRho, &
         totalSourceNumRho, totalLossx, totalLossNumx,  AverageTemp, SourceLossMax
    real :: totalPSNumRho=0.0,totalRLNumRhox=0.0, temps, testvar
    character (len=*), parameter :: NameSub = 'user_calc_sources'
    logical :: DoTest, DoTestMe, DoTestCell

    !--------------------------------------------------------------------

    iBlock = GlobalBlk

    ! Do not provide explicit source term when point-implicit scheme is used
    ! IsPointImplSource is true only when called from ModPointImplicit
    if(UsePointImplicit .and. .not. IsPointImplSource) RETURN

    ! Check if inside rPointImplicit. 
    ! Set UsePointImplicit_B=F so the point implicit
    ! evaluation is not done at all.
    UsePointImplicit_B(iBlock) = r_BLK(1,1,1,iBlock) < rPointImplicit

    !if(.not.UsePointImplicit_B(iBlock)) RETURN

    if(iProc==PROCtest.and.iBlock==BLKtest)then
       call set_oktest(NameSub,DoTest,DoTestMe)
    else
       DoTest=.false.; DoTestMe=.false.
    end if

    !    if(DoTestMe)then
    !   write(*,*)'before Source(rhoU)=', Source_VC(6:8,itest,jtest,ktest)
    !      write(*,*)NameSub,' Source(p,E)', Source_VC(iP,iTest,jTest,kTest)
    !   end if

    !chemistry etc

    do k = 1, nK ;   do j = 1, nJ ;  do i = 1, nI

       DoTestCell = DoTestMe .and. i==iTest .and. j==jTest .and. k==kTest

       NumDens_I  = State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I(:)
       NumDens    = sum(NumDens_I)
       InvNumDens = 1.0/NumDens

       Temp_I     = State_VGB(iPIon_I,i,j,k,iBlock)/(2*NumDens_I)
       !Temp_I=TNu_body
       !State_VGB(iPIon_I,i,j,k,iBlock)=2*Temp_I*NumDens_I

       !if(DoTestCell)then
       !write(*,*)'Temp_I before calculation in calc_source=',Temp_I
       !end if

       AverageTemp= sum(State_VGB(iPIon_I,i,j,k,iBlock))*InvNumDens

!!$       InvRho_I = 1.0/State_V(iRhoIon_I)
       InvRho_I = 1.0/State_VGB(iRho_I(2:nFluid),i,j,k,iBlock)
!!$       Ux_I  = InvRho_I*State_VGB(iUxIon_I)
!!$       Uy_I  = InvRho_I*State_VGB(iUyIon_I)
!!$       Uz_I  = InvRho_I*State_VGB(iUzIon_I)

       if (R_BLK(i,j,k,iBlock) < Rbody) CYCLE

       InvRho_I = 1.0/State_VGB(iRhoIon_I,i,j,k,iBlock)
       inv_rho = 1.0/sum(State_VGB(iRhoIon_I,i,j,k,iBlock))
       inv_rho2 = inv_rho**2
       uu2 = sum(State_VGB(RhoUx_:RhoUz_,i,j,k,iBlock)**2) * inv_rho2

       write(*,*)'DoTestCell=',DoTestCell

       do iFluid = 1, nIonFluid
          uu2_I(iFluid) = &
               ( State_VGB(iRhoUxIon_I(iFluid),i,j,k,iBlock)**2  &
               + State_VGB(iRhoUyIon_I(iFluid),i,j,k,iBlock)**2  &
               + State_VGB(iRhoUzIon_I(iFluid),i,j,k,iBlock)**2) &
               * InvRho_I(iFluid)**2
       end do

       !uu2_I(:)=uu2

       ReactionRate_I=0.0
       CoeffSpecies_II(:,:)=0.0
       PhoIon_I = 0.0
       Recb_I(:)=0.0
       LossSpecies_I=0.0
       SiSpecies_I(:)=0.0
       LiSpecies_I(:)=0.0

       totalSourceRho=0.0
       totalLossRho=0.0
       totalLossNumRho=0.0
       totalSourceNumRho=0.0
       totalLossx=0.0
       totalLossNumx=0.0
       totalPSNumRho=0.0
       totalRLNumRhox=0.0

       LossNumRho_I=0.0
       SourceNumRho_I=0.0
       Lossx_I=0.0
       LossNumx_I=0.0
       RLNumRhox_I=0.0

       MaxSLSpecies_CB(i,j,k,iBlock)=1.0e-3

       Productrate= Productrate_CB(i,j,k,iBlock)

       ReactionRate_I(H_hv__Hp_em_)= &
            Rate_I(H_hv__Hp_em_)*nDenNuSpecies_CBI(i,j,k,iBlock,H_)
       PhoIon_I(Hp_)=ReactionRate_I(H_hv__Hp_em_)*Productrate
       PhoIon_I(CO2p_)=Ionizationrate_CBI(i,j,k,iBlock,CO2_)
       PhoIon_I(Op_)=Ionizationrate_CBI(i,j,k,iBlock,O_)

       !charge exchange

       ReactionRate_I(CO2p_O__O2p_CO_)= &
            Rate_I(CO2p_O__O2p_CO_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

       ReactionRate_I(Op_CO2__O2p_CO_)= &
            Rate_I(Op_CO2__O2p_CO_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,CO2_)&
            *exp(log(Tnu_body_dim/Ti_dim)*0.39)
       !write(*,*)'temp_I(3)dim=',(Temp_I(3)/Si2No_V(UnitTemperature_))
       CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

       ReactionRate_I(CO2p_O__Op_CO2_)= &
            Rate_I(CO2p_O__Op_CO2_)&
            * nDenNuSpecies_CBI(i,j,k,iBlock,O_)
       CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)


       ! Recombination

       kTi=State_VGB(P_,i,j,k,iBlock)/NumDens/2.0   
       kTe=kTi

       if(kTi <= 0.0)then
          write(*,*)'i,j,k,iBlock=',i,j,k,iBlock
          write(*,*)'xyz=',x_BLK(i,j,k,iBlock),y_BLK(i,j,k,iBlock), &
               z_BLK(i,j,k,iBlock)
          write(*,*)'NumDens_I=',NumDens_I
          write(*,*)'NumDens=',NumDens
          write(*,*)'kTi,p=',kTi,State_VGB(P_,i,j,k,iBlock)
          call stop_mpi('DEBUG')
       end if

       ReactionRate_I(O2p_em__O_O_)=Rate_I(O2p_em__O_O_)       
       Recb_I(O2p_) = ReactionRate_I(O2p_em__O_O_)*(TNu_body/kTi)**0.56

       !            ReactionRate_I(O2p_em__O_O_)*exp(log(TNu_body/Temp_I(2))*0.56)

       ReactionRate_I(CO2p_em__CO_O_)=Rate_I(CO2p_em__CO_O_)       
       Recb_I(CO2p_)=ReactionRate_I(CO2p_em__CO_O_)*&
            sqrt(TNu_body/kTi)

       !end if  !(x>0.0)

       LossSpecies_I = sum( CoeffSpecies_II, DIM=1 )

       do iFluid=1, nIonFluid
          !LossSpecies_I = LossSpecies_I + CoeffSpecies_II(iFluid, :)

          dSdRho_II(1:nIonFluid, iFluid)= &
               CoeffSpecies_II(1:nIonFluid,iFluid)*MassIon_I(:)/MassIon_I(iFluid)

       enddo

       SiSpecies_I = PhoIon_I*MassIon_I

       ! if(DoTestCell)then
       !    
       !    do iFluid=1,nIonFluid
       !        write(*,*)'iFluid=',iFluid
       !        write(*,*)NameSub,' dSdRho_II(iFluid,:)',dSdRho_II(iFluid,:)
       !    end do            
       !     end if


       do iFluid=1, nIonFluid
          SiSpecies_I(1:nIonFluid)=&
               SiSpecies_I(1:nIonFluid)  &
               +dSdRho_II(1:nIonFluid, iFluid) &
               *State_VGB(iRhoIon_I(iFluid), i,j,k, iBlock)


          LiSpecies_I(iFluid)= &
               LiSpecies_I(iFluid)  &
               +(LossSpecies_I(iFluid) +Recb_I(iFluid)*NumDens)&
               *State_VGB(iRhoIon_I(iFluid), i,j,k, iBlock)
       enddo

!!$       
!!$       if(DoTestCell)then
!!$          write(*,*)NameSub,' State_VGB(iRhoIon_I(:))',State_VGB(iRhoIon_I(:),iTest,jTest,kTest,BLKTest)
!!$          do iFluid=1,nIonFluid
!!$              write(*,*)'iFluid=',iFluid
!!$              write(*,*)NameSub,' CoeffSpecies_II(iFluid,:)',CoeffSpecies_II(iFluid,:)
!!$          end do 
!!$           write(*,*)'SiSpecies_I(:)=',SiSpecies_I(:)
!!$           write(*,*)'LiSpecies_I(:)=',LiSpecies_I(:)
!!$           !write(*,*)'Recb_I(:)=',Recb_I(:)
!!$           end if
!!$
!!$       



       totalSourceRho=sum(SiSpecies_I(1:nIonFluid))    
       totalLossRho=sum(LiSpecies_I(1:nIonFluid))    
       !sum of the (Loss term) of all ion species
       totalLossNumRho = sum(LiSpecies_I/MassIon_I)
       LossNumRho_I = LiSpecies_I/MassIon_I
       !sum of the (loss term/atom mass) of all ..
       totalSourceNumRho = sum(SiSpecies_I/MassIon_I)
       SourceNumRho_I    = SiSpecies_I/MassIon_I
       ! sum of the (Source term/atom mass) of all..
       totalLossx=totalLossRho*inv_rho
       Lossx_I = LiSpecies_I*invRho_I
       totalLossNumx = totalLossNumRho/NumDens
       LossNumx_I = LossNumRho_I/NumDens_I
       totalPSNumRho = sum(PhoIon_I)
       ! sum of the photonionziation source/atom mass) of all..
       totalRLNumRhox = sum(Recb_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I)
       RLNumRhox_I    =     Recb_I*State_VGB(iRhoIon_I,i,j,k,iBlock)/MassIon_I
       !sum of the (loss term/atom mass) due to recombination    

       MaxSLSpecies_CB(i,j,k,iBlock)=maxval(abs(SiSpecies_I(1:nIonFluid)+&
            LiSpecies_I(1:nIonFluid) ) /&
            (State_VGB(iRhoIon_I(1:nIonFluid), i,j,k, iBlock)+1e-20))&
            /vInv_CB(i,j,k,iBlock)

       !          VdtFace_x(i,j,k) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               VdtFace_x(i,j,k) )
       !          VdtFace_y(i,j,k) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               VdtFace_y(i,j,k) )
       !          VdtFace_z(i,j,k) = max (MaxSLSpecies_CB(i,j,k,iBlock),&
       !               VdtFace_z(i,j,k) )

       Source_VC(iRhoIon_I(:),i,j,k) =Source_VC(iRhoIon_I(:),i,j,k) &
            +SiSpecies_I(:) &
            -LiSpecies_I(:)

       if(DoTestCell)then
            write(*,*)'!!!!!debugging: checking Hp density'
            write(*,*)'Source_VC(iRhoIon_I(HpRho)) =',Source_VC(iRhoIon_I(1),itest,jtest,ktest) 
            end if


       !write(*,*)'sum(SiSpecies_I(1:nIonFluid))=',sum(SiSpecies_I(1:nIonFluid))

!!$       if(DoTestCell)then
!!$          write(*,*)'debugging: checking the momenta before'
!!$          write(*,*)'Source_VC(rhoUx_     ,i,j,k)=',Source_VC(rhoUx_     ,itest,jtest,ktest)
!!$          write(*,*)'sum(Source_VC(iRhoUxIon_I(:),i,j,k))=',sum( Source_VC(iRhoUxIon_I(:),itest,jtest,ktest))
!!$          write(*,*)'Source_VC(rhoUy_     ,i,j,k)=',Source_VC(rhoUy_     ,itest,jtest,ktest) 
!!$          write(*,*)'sum(Source_VC(iRhoUyIon_I(:),i,j,k))=', sum(Source_VC(iRhoUyIon_I(:),itest,jtest,ktest))
!!$           write(*,*)'Source_VC(rhoUz_     ,i,j,k)=',Source_VC(rhoUz_     ,itest,jtest,ktest) 
!!$          write(*,*)'sum(Source_VC(iRhoUzIon_I(:),i,j,k))=', sum(Source_VC(iRhoUzIon_I(:),itest,jtest,ktest))
!!$
!!$          end if


       Source_VC(rho_     ,i,j,k)=Source_VC(rho_     ,i,j,k)&
            +sum(SiSpecies_I(1:nIonFluid))&
            -sum(LiSpecies_I(1:nIonFluid))

       Source_VC(rhoUx_     ,i,j,k)= Source_VC(rhoUx_     ,i,j,k) &
            -State_VGB(Ux_,i,j,k,iBlock)*totalLossx&
            -nu_BLK(i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)

       Source_VC(iRhoUxIon_I(:),i,j,k)=Source_VC(iRhoUxIon_I(:),i,j,k)&
            -State_VGB(iRhoUxIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUxIon_I(:),i,j,k,iBlock) 

       Source_VC(rhoUy_     ,i,j,k) = Source_VC(rhoUy_     ,i,j,k)  &
            -State_VGB(Uy_,i,j,k,iBlock)*totalLossx&
            -nu_BLK(i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)

       Source_VC(iRhoUyIon_I(:),i,j,k)= Source_VC(iRhoUyIon_I(:),i,j,k)&
            -State_VGB(iRhoUyIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUyIon_I(:),i,j,k,iBlock)

       Source_VC(rhoUz_     ,i,j,k)= Source_VC(rhoUz_     ,i,j,k)  &
            -State_VGB(Uz_,i,j,k,iBlock)*totalLossx&
            -nu_BLK(i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)

       Source_VC(iRhoUzIon_I(:),i,j,k)= Source_VC(iRhoUzIon_I(:),i,j,k)&
            -State_VGB(iRhoUzIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUzIon_I(:),i,j,k,iBlock)
       
!!$       if(DoTestCell)then
!!$          write(*,*)'!!!!!!!debugging: checking the momenta after'
!!$
!!$          write(*,*)'term added to rhoUx=', -State_VGB(Ux_,i,j,k,iBlock)*totalLossx&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(Ux_,i,j,k,iBlock)
!!$          write(*,*)'term in x directions for ions momenta=', -State_VGB(iRhoUxIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUxIon_I(:),i,j,k,iBlock)
!!$          write(*,*)'term added to rhoUy=' ,-State_VGB(Uy_,i,j,k,iBlock)*totalLossx&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(Uy_,i,j,k,iBlock)
!!$          write(*,*)'term in y directions for ions momenta=', -State_VGB(iRhoUyIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUyIon_I(:),i,j,k,iBlock)
!!$          write(*,*)'term added to rhoUz=', -State_VGB(Uz_,i,j,k,iBlock)*totalLossx&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(Uz_,i,j,k,iBlock)
!!$          write(*,*)'term in z directions for ions momenta=', -State_VGB(iRhoUzIon_I(:),i,j,k,iBlock)*Lossx_I(:)&
!!$            -nu_BLK(i,j,k,iBlock)*State_VGB(iRhoUzIon_I(:),i,j,k,iBlock)
!!$ 
!!$          write(*,*)'!!!!!!!!!!!!!!!Now tthe sources'
!!$          write(*,*)'Source_VC(rhoUx_     ,i,j,k)=',Source_VC(rhoUx_     ,itest,jtest,ktest)
!!$          write(*,*)'sum(Source_VC(iRhoUxIon_I(:),i,j,k))=',sum( Source_VC(iRhoUxIon_I(:),itest,jtest,ktest))
!!$          write(*,*)'Source_VC(rhoUy_     ,i,j,k)=',Source_VC(rhoUy_     ,itest,jtest,ktest) 
!!$          write(*,*)'sum(Source_VC(iRhoUyIon_I(:),i,j,k))=', sum(Source_VC(iRhoUyIon_I(:),itest,jtest,ktest))
!!$           write(*,*)'Source_VC(rhoUz_     ,i,j,k)=',Source_VC(rhoUz_     ,itest,jtest,ktest) 
!!$          write(*,*)'sum(Source_VC(iRhoUzIon_I(:),i,j,k))=', sum(Source_VC(iRhoUzIon_I(:),itest,jtest,ktest))
!!$
!!$          end if


       !----- pressure and energy source terms

       if(UseOldEnergy)then
          tmp = (totalSourceNumRho*Tp_body + totalPSNumRho*T300*20.) &
               -(totalLossNumx+totalRLNumRhox)*State_VGB(P_,i,j,k,iBlock)

          tmp_I = (SourceNumRho_I*Tp_body + PhoIon_I*T300*20.) &
               -(LossNumx_I + RLNumRhox_I)*State_VGB(iPIon_I,i,j,k,iBlock)

          if (DoTestCell)then
             write(*,*)'tmp before condition=',tmp
             write(*,*)'tmp_I before condition=',tmp_I
          end if

          Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
               + (inv_gm1*tmp-0.50*uu2*(totalLossRho))&
               - 0.5*State_VGB(rho_,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)


          do iFluid = 1, nIonFluid
             Source_VC(Energy_+iFluid,i,j,k) = &
                  Source_VC(Energy_+iFluid,i,j,k)&
                  +(inv_gm1*tmp_I(iFluid) - 0.5*uu2_I(iFluid)*LiSpecies_I(iFluid))&
                  -0.5*State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)*uu2_I(iFluid)*&
                  nu_BLK(i,j,k,iBlock)
          end do

          !Source_VC(Energy_+1:Energy_+nFluid,i,j,k) = &
          !     Source_VC(Energy_+1:Energy_+nFluid,i,j,k)&
          !     + (inv_gm1*tmp_I - 0.5*uu2_I*LiSpecies_I)&
          !     - 0.5*State_VGB(iRhoIon_I,i,j,k,iBlock)*uu2_I*&
          !     nu_BLK(i,j,k,iBlock)

          Source_VC(P_ ,i,j,k) = Source_VC(P_ ,i,j,k) &
               + (tmp + 0.5*uu2*(totalSourceRho)*gm1) &
               + gm1*0.5*State_VGB(rho_,i,j,k,iBlock)*uu2* &
               nu_BLK(i,j,k,iBlock)

          Source_VC(iPIon_I,i,j,k) = Source_VC(iPIon_I,i,j,k) &
               + (tmp_I + 0.5*uu2_I*SiSpecies_I*gm1)&
               + gm1*0.5*State_VGB(iRhoIon_I,i,j,k,iBlock)*uu2_I*&
               nu_BLK(i,j,k,iBlock)

          if (DoTestCell)then
             write(*,*)'kTi=',kTi
             write(*,*)'TNu_body=',TNu_body
             write(*,*)'Temp_I=',Temp_I
          end if

          if(kTi > TNu_body)then

             Source_VC(Energy_,i,j,k) = Source_VC(Energy_,i,j,k) &
                  -nu_BLK(i,j,k,iBlock)*NumDens*inv_gm1&
                  *(kTi-TNu_body)

             Source_VC(P_ ,i,j,k) = Source_VC(P_ ,i,j,k)  &
                  -nu_BLK(i,j,k,iBlock)*NumDens*inv_gm1&
                  *(kTi-TNu_body)

          end if

          do iFluid=1, nIonFluid

             !!!if(Temp_I(iFluid) > TNu_body)then
             !!!
             !!!   Source_VC(iPIon_I(iFluid) ,i,j,k) = &
             !!!        Source_VC(iPIon_I(iFluid),i,j,k)&
             !!!        - nu_BLK(i,j,k,iBlock)*NumDens_I(iFluid)*inv_gm1*&
             !!!        (Temp_I(iFluid)-TNu_body)
             !!!
             !!!   Source_VC(Energy_+iFluid,i,j,k) = &
             !!!        Source_VC(Energy_+iFluid,i,j,k)&
             !!!        -nu_BLK(i,j,k,iBlock)*NumDens_I(iFluid)*inv_gm1&
             !!!        *(Temp_I(iFluid)-TNu_body)
             !!!
             !!!end if
          end do

       else

          tmp = totalSourceNumRho*TNu_body            &
               + NumDens*(TNu_body-KTi)*nu_BLK(i,j,k,iBlock) &
               + totalPSNumRho*kTe0                &
               - totalLossNumRho*kTi               &
               - totalRLNumRhox*NumDens*KTe

!!$       tmp_I(:)=SourceNumRho_I*TNu_body            &
!!$            + NumDens_I(:)*(TNu_body-Temp_I(:))*nu_BLK(i,j,k,iBlock) &
!!$            + PhoIon_I(:)*kTe0                &
!!$            - LossNumRho_I(:)*Temp_I(:)               &
!!$            - RLNumRhox_I(:)*NumDens_I(:)*KTe

          tmp_I = SourceNumRho_I*TNu_body            &
               + NumDens_I*(TNu_body-Temp_I)*nu_BLK(i,j,k,iBlock) &
               + PhoIon_I*kTe0                &
               - LossNumRho_I*Temp_I               &
               - RLNumRhox_I*NumDens_I*KTe

          testvar=State_VGB(rho_ ,i,j,k,iBlock)


          Source_VC(Energy_ ,i,j,k) =  Source_VC(Energy_ ,i,j,k)&
               -0.5*State_VGB(rho_ ,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)-0.50*uu2*(totalLossRho) +inv_gm1*tmp

          do iFluid=1,nIonFluid
             Source_VC(Energy_ + iFluid,i,j,k) =  Source_VC(Energy_ + iFluid,i,j,k)&
                  -0.5*State_VGB(iRhoIon_I(iFluid),i,j,k,iBlock)*uu2_I(iFluid)*&
                  nu_BLK(i,j,k,iBlock)-0.50*uu2_I(iFluid)*(LiSpecies_I(iFluid)) &
                  +inv_gm1*tmp_I(iFluid)
          end do

          if(DoTestCell)write(*,*)'Source_VC(P_     ,i,j,k)=',Source_VC(P_     ,iTest,jTest,kTest)

          Source_VC(P_     ,i,j,k)  = Source_VC(P_     ,i,j,k)   &
               +0.5*gm1*State_VGB(rho_ ,i,j,k,iBlock)*uu2*&
               nu_BLK(i,j,k,iBlock)  &
               +0.50*(gm1)*uu2*(totalSourceRho) &
               +tmp
       

          !if(DoTestCell)then
          !write(*,*)'the component of Source_VC(P) are the following'   
          !write(*,*)'0.5*gm1*State_VGB(rho_ ,i,j,k,iBlock)*uu2*nu_BLK(i,j,k,iBlock)=',&
          !     0.5*gm1*State_VGB(rho_ ,i,j,k,iBlock)*uu2*nu_BLK(i,j,k,iBlock)
          !write(*,*)'0.50*(gm1)*uu2*(totalSourceRho)=',0.50*(gm1)*uu2*(totalSourceRho)
          !write(*,*)'tmp=',tmp
          !write(*,*)'totalSourceNumRho*TNu_body=',totalSourceNumRho*TNu_body
          !write(*,*)'NumDens*(TNu_body-KTi)*nu_BLK(i,j,k,iBlock)=',NumDens*(TNu_body-KTi)*nu_BLK(i,j,k,iBlock)
          !write(*,*)'totalPSNumRho*kTe0 =',totalPSNumRho*kTe0
          !write(*,*)'totalLossNumRho*kTi=',totalLossNumRho*kTi
          !write(*,*)'totalRLNumRhox*NumDens*KTe=',totalRLNumRhox*NumDens*KTe
          !end if

           if(DoTestCell)then
          write(*,*)'the component of Source_VC(P) are the following'
          write(*,*)'0.5*gm1*State_VGB(rho_ ,i,j,k,iBlock)*uu2*nu_BLK(i,j,k,iBlock)=',&
               0.5*gm1*State_VGB(rho_ ,i,j,k,iBlock)*uu2*nu_BLK(i,j,k,iBlock)
          write(*,*)'0.50*(gm1)*uu2*(totalSourceRho)=',0.50*(gm1)*uu2*(totalSourceRho)
          write(*,*)'tmp=',tmp
          write(*,*)'totalSourceNumRho*TNu_body=',totalSourceNumRho*TNu_body
          write(*,*)'totalSourceNumRho=',totalSourceNumRho
          write(*,*)'TNu_body=',TNu_body
          write(*,*)'NumDens*(TNu_body-KTi)*nu_BLK(i,j,k,iBlock)=',NumDens*(TNu_body-KTi)&
               *nu_BLK(i,j,k,iBlock)
          write(*,*)'NumDens=',NumDens
          write(*,*)'(TNu_body-KTi)=',(TNu_body-KTi)
          write(*,*)'nu_BLK(i,j,k,iBlock)=',nu_BLK(i,j,k,iBlock)
          write(*,*)'totalPSNumRho*kTe0 =',totalPSNumRho*kTe0
          write(*,*)'totalPSNumRho=',totalPSNumRho
          write(*,*)'kTe0=',kTe0
          write(*,*)'totalLossNumRho*kTi=',totalLossNumRho*kTi
          write(*,*)'totalLossNumRho=',totalLossNumRho
          write(*,*)'kTi=',kTi
          write(*,*)'totalRLNumRhox*NumDens*KTe=',totalRLNumRhox*NumDens*KTe
          write(*,*)'totalRLNumRhox=',totalRLNumRhox
          write(*,*)'NumDens=',NumDens
          write(*,*)'KTe=',KTe
          end if


          Source_VC(iPIon_I,i,j,k)  = Source_VC(iPIon_I,i,j,k)   &
               + 0.5*gm1*State_VGB(iRhoIon_I,i,j,k,iBlock)*uu2_I*&
               nu_BLK(i,j,k,iBlock)  &
               + 0.5*gm1*uu2_I*SiSpecies_I &
               + tmp_I

          !Source_VC(P_     ,i,j,k)  = Source_VC(P_     ,i,j,k)   &
               !+sum(Source_VC(iPIon_I,i,j,k))

       end if

       !write(*,*)'DoTestCell=',DoTestCell

       if(DoTestCell)then
          !write(*,*)'tmp_I   =',tmp_I
          !write(*,*)'0.5*gm1*uu2_I*SiSpecies_I=', 0.5*uu2_I*SiSpecies_I*gm1
          !write(*,*)'0.5*gm1*uu2_I*Rho_I*nu_BLK=', &
          !     gm1*0.5*State_VGB(iRhoIon_I,i,j,k,iBlock)*uu2_I*&
          !     nu_BLK(i,j,k,iBlock)
          !write(*,*)'SourceNumRho_I*Tp_body=', SourceNumRho_I*Tp_body
          !write(*,*)'PhoIon_I*T300*20.     =', PhoIon_I*T300*20.
          !write(*,*)'LossNumx_I*P_I        =', &
          !     LossNumx_I*State_VGB(iPIon_I,i,j,k,iBlock)
          !write(*,*)'RLNumRhox_I*P_I       =', &
          !     RLNumRhox_I*State_VGB(iPIon_I,i,j,k,iBlock)
          !write(*,*)'Source_VC(O2pP_)=',Source_VC(O2pP_,i,j,k)
          !write(*,*)'Temp_I, TNu_body=', Temp_I, TNu_body
         ! write(*,*)'nu*NumDens_I*invgm1*(T_I-Tnu)=',&
        !       nu_BLK(i,j,k,iBlock)*NumDens_I*inv_gm1*(Temp_I-TNu_body)
          
          write(*,*)'SiSpecies_I=',SiSpecies_I
          write(*,*)'LiSpecies_I=',LiSpecies_I

       end if

       if(DoTestCell)then

!!$          write(*,*)NameSub,'totalSourceNumRho= ', totalSourceNumRho
!!$          write(*,*)NameSub,' totalLossNumRho=',totalLossNumRho
!!$          write(*,*)NameSub,'totalLossx=', totalLossx
!!$          write(*,*)NameSub,'totalLossNumx=', totalLossNumx
!!$          write(*,*)NameSub,'totalPSNumRho=',totalPSNumRho
!!$          write(*,*)NameSub,'totalRLNumRhox=', totalRLNumRhox
!!$          write(*,*)NameSub,' Tmp_I=',Tmp_I(:)
          write(*,*)NameSub,' State_VGB(iPIon_I(:))',State_VGB(iPIon_I(:),iTest,jTest,kTest,BLKTest)
          write(*,*)NameSub,' Source_VC(rho_ )=', Source_VC(rho_       ,iTest,jTest,kTest)
          write(*,*)NameSub,'  Source_VC(iRhoIon_I(3:4),:,:,:)=', Source_VC(iRhoIon_I(:),iTest,jTest,kTest)
          write(*,*)NameSub,' Source_VC(rhoUx_)=', Source_VC(rhoUx_     ,iTest,jTest,kTest)
          write(*,*)NameSub,'Source_VC(rhoUy_)=',Source_VC(rhoUy_     ,iTest,jTest,kTest)
          write(*,*)NameSub,' Source_VC(rhoUz_)=', Source_VC(rhoUz_     ,iTest,jTest,kTest)         

       end if

    end do; end do; end do     ! end of the i,j,k loop

    !write(*,*)'hi'
    if(DoTestMe)then
       write(*,*)'Hello'
       end if

    !end of chemistry

  end subroutine user_calc_sources
  !=============================================================================
  subroutine user_init_point_implicit

    use ModPointImplicit, ONLY: iVarPointImpl_I, IsPointImplMatrixSet
    !------------------------------------------------------------------------

    ! All ion momenta are implicit
    if(IsMhd)then
       allocate(iVarPointImpl_I(4*nIonFluid))

       do iFluid = 1, nIonFluid
          iVarPointImpl_I(4*iFluid-3) = iRhoUx_I(iFluid)
          iVarPointImpl_I(4*iFluid-2) = iRhoUy_I(iFluid)
          iVarPointImpl_I(4*iFluid-1) = iRhoUz_I(iFluid)
          iVarPointImpl_I(4*iFluid)   = iP_I(iFluid)
       end do
    else
       allocate(iVarPointImpl_I(4*(nIonFluid-1)))
       do iFluid = 1, nIonFluid-1
          iVarPointImpl_I(4*iFluid-3) = iRhoUx_I(iFluid+1)
          iVarPointImpl_I(4*iFluid-2) = iRhoUy_I(iFluid+1)
          iVarPointImpl_I(4*iFluid-1) = iRhoUz_I(iFluid+1)
          iVarPointImpl_I(4*iFluid)   = iP_I(iFluid+1)
       end do
    end if

    IsPointImplMatrixSet = .false.

  end subroutine user_init_point_implicit
  ! ===========================================================================

  subroutine user_init_session
    use ModMain
    use ModPhysics
    use ModVarIndexes

    integer::iBoundary
    !--------------------------------------------------------------------------
    !For Outer Boundaries
    !do iBoundary=East_,Top_
    !   FaceState_VI(HpRho_,iBoundary)    = SW_rho
    !   FaceState_VI(O2pRho_,iBoundary)   = cTiny8
    !   FaceState_VI(OpRho_,iBoundary)    = cTiny8
    !   FaceState_VI(CO2pRho_,iBoundary)  = cTiny8     
    !end do
    call set_multiSp_ICs  
    !    Rbody = 1.0 + 140.0e3/Mars
    BodyRho_I(1) = sum(BodyRhoSpecies_I(1:MaxSpecies))
    !BodyRho_I(2:nFluid)=BodyRhoSpecies_I(1:MaxSpecies)
    BodyP_I(1)   =sum(BodyRhoSpecies_I(1:MaxSpecies)&
         /MassFluid_I(2:nFluid))*Tp_body
    write(*,*)'BodyP_I(1)=',BodyP_I(1)
    write(*,*)'sum(BodyRhoSpecies_I(1:MaxSpecies)=',sum(BodyRhoSpecies_I(1:MaxSpecies))
    write(*,*)'Tp_body=',Tp_body
    BodyP_I(2:nFluid)=sum(BodyRhoSpecies_I(1:MaxSpecies)&
         /MassFluid_I(2:nFluid))*Tp_body

    FaceState_VI(rho_,body1_)=BodyRho_I(1)
    FaceState_VI(iRhoIon_I,body1_) = BodyRhoSpecies_I
    FaceState_VI(P_,body1_)=BodyP_I(1)

    CellState_VI(:,body1_:Top_)=FaceState_VI(:,body1_:Top_)
    !write(*,*)'BodyRho_I(HpRho_) is =',BodyRho_I(2)
    do iBoundary=body1_,Top_  
       CellState_VI(rhoUx_:rhoUz_,iBoundary) = &
            FaceState_VI(Ux_:Uz_,iBoundary)*FaceState_VI(rho_,iBoundary)       
    end do

    !UnitUser_V(iRho_I(2:nFluid))   = No2Io_V(UnitRho_)/MassFluid_I(2:nFluid)
    !write(*,*)'UnitUser_V(iRho_I(2:nFluid))=',UnitUser_V(iRho_I(2:nFluid))

    if(.not.allocated(nDenNuSpecies_CBI))then
       allocate(nDenNuSpecies_CBI(nI, nJ, nK, nBLK, MaxNuSpecies))
       allocate( TempNuSpecies_CBI(1:nI, 1:nJ, 1:nK, nBLK))
       allocate(Productrate_CB(1:nI, 1:nJ, 1:nK, nBLK))
       allocate(Ionizationrate_CBI(1:nI, 1:nJ, 1:nK, nBLK,2))
       allocate(MaxSiSpecies_CB(1:nI, 1:nJ, 1:nK, nBLK))
       allocate(MaxLiSpecies_CB(1:nI, 1:nJ, 1:nK, nBLK))
       allocate(MaxSLSpecies_CB(1:nI, 1:nJ, 1:nK, nBLK))
       allocate(nu_BLK(1:nI,1:nJ,1:nK,nBLK))
       allocate(nu1_BLK(1:nI,1:nJ,1:nK,nBLK))
    end if

  end subroutine user_init_session
  !============================================================================
  subroutine user_read_inputs
    use ModMain
    use ModProcMH,    ONLY: iProc
    use ModReadParam

    character (len=100) :: NameCommand
    integer:: i, j, k, n, m
    character (len=60):: TGCMFilename  
    character (len=100) :: line
    !------------------------------------------------------------------------

    do
       if(.not.read_line() ) EXIT
       if(.not.read_command(NameCommand)) CYCLE
       select case(NameCommand)

       case('#USERINPUTEND')
          if(iProc==0) write(*,*)'USERINPUTEND'
          EXIT

       case('#USEOLDENERGY')
          call read_var('UseOldEnergy',UseOldEnergy)
          if(.not.UseOldEnergy)then
             call read_var('Te_new_dim',Te_new_dim)
             !change temperature from ev to k
             Te_new_dim = Te_new_dim * 11610.0
          end if


       case("#UseMarsB0")  !if or not include crustal magnetic field of Mars
          call read_var('UseMarsB0',UseMarsB0)
          if(UseMarsB0) then
             call read_var('NNm', NNm)
             call read_var('rot',rot)
             call read_var('thetilt', thetilt)
             rot= rot/180.0*3.141592653589793238462643383279
             thetilt= thetilt/180.0*3.141592653589793238462643383279
             cmars = 0.0
             dmars = 0.0
             open(15,file='marsmgsp.txt')
             do i=0,NNm 
                read(15,*)n,(cmars(n-1,m),m=0,n-1),(dmars(n-1,m),m=0,n-1)
             end do
             close(15)
          endif

       case("#UseSolarMax") !solar cycle condition
          call read_var('UseSolarMax',UseSolarMax)

       case("#UseHotO")  !adding hot Oxygen or not
          call read_var('UseHotO',UseHotO)

       case("#UseTempCont") !add hoc term of the energy source
          call read_var('UseTempCont',UseTempCont)          

       case('#REACTIONS')
          call read_var('UseImpactIon',UseImpactIon)
          call read_var('UseChargeEx',UseChargeEx)
          open(15,file='read_in.dat')
          do i=1,32
             read(15,*)Temp_dim(i),Impact_ION_dim(i,Op_),Impact_ION_dim(i,Hp_)
          end do
          close(15)

       case("#UseMarsAtm")
          call read_var('UseMarsAtm',UseMarsAtm)
          if(UseMarsAtm)then
             call read_var('TGCMFilename',TGCMFilename)
             call read_var('NAlt', Nalt)
             open(15,file=TGCMFilename,status="old")
             read(15,*)line
             write(*,*)line, Nalt
             do k = 1, NAlt
                do j=1, NLat
                   do i=1, NLong
                      read(15,*)Long_I(i),Lat_I(j),Alt_I(k),Temp(i,j,k),Den_CO2(i,j,k),&
                           Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)
                   end do
                end do
             end do
             close(15)
             write(*,*)Long_I(Nlong),Lat_I(NLat),Alt_I(Nalt)
             write(*,*)Long_I(1),Lat_I(1),Alt_I(1)
             write(*,*)'Den_O(i,j,k),ICO2p(i,j,k),IOp(i,j,k)=',&
                  Den_O(Nlong,Nlat,Nalt),ICO2p(Nlong,Nlat,Nalt),&
                  IOp(Nlong,Nlat,Nalt)

          end if
       case('#POINTIMPLICITREGION')
          call read_var('rPointImplicit',rPointImplicit)

       case default
          if(iProc==0) call stop_mpi( &
               'read_inputs: unrecognized command: '//NameCommand)
       end select
    end do
  end subroutine user_read_inputs


  !============================================================================
  subroutine Mars_Input
    use ModMain
    use ModPhysics
    use ModConst
    use ModGeometry,ONLY:x_BLK,y_BLK,z_BLK,R_BLK,dx_BLK,dy_BLK,dz_BLK,&
         XyzStart_BLK,TypeGeometry
    implicit none

    real, parameter :: TINY=1.0E-12 
    real :: hh, theta, phi, dR, dtheta, dphi, dH, Hscale, HCO2, HO, grav
    real:: tempICO2p, tempIOp
    real:: xLat, xLong,xAlt
    integer :: i,j,k,n, m
    integer:: iAlt, jLong, kLat, ip1,jp1,kp1
    logical:: oktest=.false., oktestme=.false.
    !-----------------------------------------------------------------------
    !------ Interpolation/Expolation for Tn,nCO2,nO,PCO2p,POp ----- 

    dR=dx_BLK(globalBLK)
    dPhi=dy_BLK(globalBLK)
    dTheta=dz_BLK(globalBLK)

    select case(TypeGeometry)                                   
    case('cartesian')                                           
       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    case('spherical','spherical_lnr')
       ! at least part of the block is outside the body 
       if (R_BLK(nI,1,1,globalBLK) >= Rbody) then  

          do k=1,nK
             Theta = (k-1)*dTheta  + xyzStart_BLK(Theta_,globalBLK)      
             Theta =  180*(0.5-Theta/cPi)
             kLat=int((theta+87.5)/5.0+1.0)
             kp1=min(kLat+1, NLat)
             kLat = max(kLat,1)

             do j=1,nJ  
                Phi = (j-1)*dPhi  + xyzStart_BLK(Phi_,globalBLK)
                if(phi>cPi)then 
                   phi=phi-2*cPi
                end if
                Phi = 180*(Phi/cPi) 
                jLong=int((phi+180)/5.0+1.0)                 
                jp1=min(jLong+1,NLong)
                jLong=max(jLong,1)

                do i=nI,1,-1                    
                   hh = (R_BLK(i,j,k,globalBLK)-1.00)*3396.00
                   !                 write(*,*)'hh=', hh, i,j,k,globalBLK
                   xLong=0.2*(Phi-Long_I(jLong))
                   xLat=0.2*(Theta-Lat_I(kLat))
                   if(hh.le.100.0)then  !inside the body
                      tempNuSpecies_CBI(i,j,k,globalBLK)= &
                           tempNuSpecies_CBI(i+1,j,k,globalBLK)
                      nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)=&
                           nDenNuSpecies_CBI(i+1,j,k,globalBLK,CO2_)
                      nDenNuSpecies_CBI(i,j,k,globalBLK,O_)= &
                           nDenNuSpecies_CBI(i+1,j,k,globalBLK,O_)

                      !                    tempICO2p=max(tempICO2p,TINY)
                      !                    tempIOP=max(tempIOp,TINY)
                      Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=&
                           Ionizationrate_CBI(i+1,j,k,globalBLK,CO2_)
                      Ionizationrate_CBI(i,j,k,globalBLK,O_)=&
                           Ionizationrate_CBI(i+1,j,k,globalBLK,O_)
                   elseif(hh.le.Alt_I(NAlt))then
                      iAlt=int((hh -100.0)/10.0+1.0)
                      ip1=min(iAlt+1,NAlt)
                      if(iAlt.lt.1)then 
                         write(*,*)'wrong ialt',iAlt
                      end if
                      xalt=0.1*(hh-Alt_I(iAlt))
                      !interpolate
                      tempNuSpecies_CBI(i,j,k,globalBLK)=          &
                           ((Temp(jLong,kLat,iAlt)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ialt))*(1-xLat) &
                           +(Temp(jLong,kp1,iAlt)*(1-xLong)        &
                           + xLong*Temp(jp1, kp1, ialt))*xLat)*(1-xAlt)&
                           +((Temp(jLong,kLat,ip1)*(1-xLong)       &
                           + xLong*Temp(jp1, kLat, ip1))*(1-xLat)   &
                           +(Temp(jLong,kp1,ip1)*(1-xLong)         &
                           + xLong*Temp(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)=&         
                           ((Den_CO2(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_CO2(jLong,kLat,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,ip1)*(1-xLong)+xLong*Den_CO2(jp1, kp1, ip1))*xLat)*xAlt

                      nDenNuSpecies_CBI(i,j,k,globalBLK,O_)=&
                           ((Den_O(jLong,kLat,iAlt)*(1-xLong)+xLong*Den_O(jp1, kLat, ialt))*(1-xLat)+&
                           (Den_O(jLong,kp1,iAlt)*(1-xLong)+xLong*Den_O(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((Den_O(jLong,kLat,ip1)*(1-xLong)+xLong*Den_O(jp1, kLat, ip1))*(1-xLat)+&
                           (Den_O(jLong,kp1,ip1)*(1-xLong)+xLong*Den_O(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=&
                           ((ICO2p(jLong,kLat,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, ialt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,iAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((ICO2p(jLong,kLat,ip1)*(1-xLong)+xLong*ICO2p(jp1, kLat, ip1))*(1-xLat)+&
                           (ICO2p(jLong,kp1,ip1)*(1-xLong)+xLong*ICO2p(jp1, kp1, ip1))*xLat)*xAlt

                      tempIOP=&
                           ((IOp(jLong,kLat,iAlt)*(1-xLong)+xLong*IOp(jp1, kLat, ialt))*(1-xLat)+&
                           (IOp(jLong,kp1,iAlt)*(1-xLong)+xLong*IOp(jp1, kp1, ialt))*xLat)*(1-xAlt)+&
                           ((IOp(jLong,kLat,ip1)*(1-xLong)+xLong*IOp(jp1, kLat, ip1))*(1-xLat)+&
                           (IOp(jLong,kp1,ip1)*(1-xLong)+xLong*IOp(jp1, kp1, ip1))*xLat)*xAlt

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      !                   Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=tempICO2p*nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)
                      !                   Ionizationrate_CBI(i,j,k,globalBLK,O_)=tempIOP*nDenNuSpecies_CBI(i,j,k,globalBLK,O_)
                      Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,globalBLK,O_)=tempIOP
                   else  !hh.gt.Alt_I(NAlt)

                      dH= hh - Alt_I(NAlt)
                      tempNuSpecies_CBI(i,j,k,globalBLK)= &
                           (Temp(jLong,kLat,NAlt)*(1-xLong)+xLong*Temp(jp1, kLat,NAlt))*(1-xLat)+&
                           (Temp(jLong,kp1,NAlt)*(1-xLong)+xLong*Temp(jp1,kp1,NAlt))*xLat

                      tempICO2p=&
                           (ICO2p(jLong,kLat,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kLat, NAlt))*(1-xLat)+&
                           (ICO2p(jLong,kp1,NAlt)*(1-xLong)+xLong*ICO2p(jp1, kp1, NAlt))*xLat

                      tempIOP=&
                           (IOp(jLong,kLat,NAlt)*(1-xLong)+xLong*IOp(jp1, kLat, NAlt))*(1-xLat)+&
                           (IOp(jLong,kp1,NAlt)*(1-xLong)+xLong*IOp(jp1, kp1, NAlt))*xLat

                      ! grav=3.72/R_BLK(i,j,k,globalBLK)/R_BLK(i,j,k,globalBLK)
                      grav=3.72/(1.0+300.0/3396.0)/(1.0+300.0/3396.0)

                      Hscale=cBoltzmann*&
                           tempNuSpecies_CBI(i,j,k,globalBLK)/grav/cProtonMass!in m unit

                      HCO2= Hscale/NuMassSpecies_I(CO2_)/1.0e3
                      HO= Hscale/NuMassSpecies_I(O_)/1.0e3

                      nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)=&
                           ((Den_CO2(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_CO2(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_CO2(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_CO2(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HCO2)
                      nDenNuSpecies_CBI(i,j,k,globalBLK,O_)=&
                           ((Den_O(jLong,kLat,NAlt)*(1-xLong)+xLong*Den_O(jp1, kLat,Nalt))*(1-xLat)+&
                           (Den_O(jLong,kp1,NAlt)*(1-xLong)+xLong*Den_O(jp1,kp1,Nalt))*xLat)&
                           *exp(-dH/HO)

                      tempICO2p=max(tempICO2p,TINY)
                      tempIOP=max(tempIOp,TINY)
                      !                    Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=tempICO2p*nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)
                      !                    Ionizationrate_CBI(i,j,k,globalBLK,O_)=tempIOP*nDenNuSpecies_CBI(i,j,k,globalBLK,O_)
                      Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=tempICO2p
                      Ionizationrate_CBI(i,j,k,globalBLK,O_)=tempIOP

                   end if !hh.lt.or.gt.300km
                end do
             end do
          end do
       end if
    case default

       call stop_mpi('Unknown geometry type = '//TypeGeometry)

    end select
    if(oktestme)then
       write(*,*)'Mars input end', &
            dR,dPhi,dTheta, globalBLK, &
            maxval(nDenNuSpecies_CBI(nI,:,:,globalBLK,CO2_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,globalBLK,CO2_)),&
            maxval(R_BLK(nI,:,:,globalBLK)),&
            minval(R_BLK(1,:,:,globalBLK))
       write(*,*)'Mars input end2',&
            globalBLK, maxval(nDenNuSpecies_CBI(nI,:,:,globalBLK,O_)),&
            minval(nDenNuSpecies_CBI(nI,:,:,globalBLK,O_)),&
            maxval(Ionizationrate_CBI(nI,:,:,globalBLK,CO2_)),&
            minval(Ionizationrate_CBI(nI,:,:,globalBLK,O_)),&
            maxval(R_BLK(nI,:,:,globalBLK)),&
            minval(R_BLK(1,:,:,globalBLK))
    end if
  end subroutine Mars_input

  !============================================================================
  subroutine set_multiSp_ICs
    use ModMain
    use ModConst
    use ModIO
    use ModPhysics

    real :: Productrate
    logical::oktest=.false., oktestme=.false.
    !---------------------------------------------------------------
    if(oktestme)then
       write(*,*)'in set_multisp_ICs, No2Io_V(UnitN_),t=',&
            No2Io_V(UnitN_),No2Io_V(UnitT_)
       write(*,*)'No2Si_V(UnitX_), temperature=',&
            No2Si_V(UnitX_), No2Si_V(UnitTemperature_)
       write(*,*)'kTp=',SW_p*Tp_body_dim/SW_T_dim, &
            Tp_body_dim/No2Si_V(UnitTemperature_)
       write(*,*)'BodynDenNuSpecies_dim_I(:)',&
            BodynDenNuSpdim_I(:)
    end if

    if(UseSolarMax)then
       Tnu_body_dim = 134.0      ! neutral temperature 
       BodynDenNuSpDim_I(CO2_)= 4.435e12
       BodynDenNuSpDim_I(O_)= 8.0283e9
       BodynDenNuSpDim_I(H_)= 1.8374e6
       BodynDenNuSpDim_I(Oh_)= 6.3119e4
       BodynDenNuSpDim_I(Ohx_)= 3.9646e3
       BodynDenNuSpDim_I(Hx_)= 7.3638e4
       BodynDenNuSpDim_I(Ox_)= 5.1736e8
       BodynDenNuSpDim_I(CO2x_)= 8.0807e10

       HNuSpeciesDim_I(O_)=13.34 !scale height in KM
       HNuSpeciesDim_I(Ox_)=50.025 
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=6.5631
       HNuSpeciesDim_I(CO2x_)=17.064

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=610.0

       RateDim_I(CO2_hv__CO2p_em_)=7.3e-7
       RateDim_I(O_hv__Op_em_) = 2.734e-7
       RateDim_I(H_hv__Hp_em_) = 8.59e-8

    else  ! for solar min condition

       Tnu_body_dim = 117.0      ! neutral temperature 

       BodynDenNuSpDim_I(CO2_)= 1.1593e12
       BodynDenNuSpDim_I(O_)= 3.2278e9
       BodynDenNuSpDim_I(H_)= 1.1307e7
       BodynDenNuSpDim_I(Oh_)= 1.951e4
       BodynDenNuSpDim_I(Ohx_)= 1.5248e3
       BodynDenNuSpDim_I(Hx_)= 9.4936e5
       BodynDenNuSpDim_I(Ox_)= 5.2695e8
       BodynDenNuSpDim_I(CO2x_)= 2.2258e11

       HNuSpeciesDim_I(O_)=9.486  !scale height in km
       HNuSpeciesDim_I(Ox_)=30.45  
       HNuSpeciesDim_I(Oh_)=290.5
       HNuSpeciesDim_I(Ohx_)=2436.6

       HNuSpeciesDim_I(CO2_)=5.2667
       HNuSpeciesDim_I(CO2x_)=10.533

       HNuSpeciesDim_I(H_)=13.133
       HNuSpeciesDim_I(Hx_)=586.6

       RateDim_I(CO2_hv__CO2p_em_)=2.47e-7
       RateDim_I(O_hv__Op_em_) = 8.89e-8
       RateDim_I(H_hv__Hp_em_) = 5.58e-8

    end if

    Ti_body_dim = Tnu_body_dim  !ion temperature at the body
    Tp_body_dim = 2.0*Tnu_body_dim 


    TNu_body= TNu_body_dim*Si2No_V(UnitTemperature_)
    Ti_body = Ti_body_dim*Si2No_V(UnitTemperature_)
    Tp_body = Tp_body_dim*Si2No_V(UnitTemperature_)
    T300 = T300_dim*Si2No_V(UnitTemperature_)

    kTe0=max(Te_new_dim, Tnu_body_dim)*Si2No_V(UnitTemperature_)

    if(oktest)then
       write(*,*)'Tnu_body=',Tnu_body, TNu_body_dim
       write(*,*)'T300=', T300, T300_dim
       write(*,*)'Tp_body=', Tp_body, Tp_body_dim       
    end if

    nu0=nu0_dim*No2Io_V(UnitN_)*No2Io_V(UnitT_)
    BodynDenNuSpecies_I(:)=&
         BodynDenNuSpDim_I(:)*Io2No_V(UnitN_)
    HNuSpecies_I(:)=&
         HNuSpeciesDim_I(:)*1.0e3*Si2No_V(UnitX_)

    ! normlize the reaction rate
    Rate_I(CO2_hv__CO2p_em_)= &
         Ratedim_I(CO2_hv__CO2p_em_)*No2Io_V(UnitT_)
    Rate_I(O_hv__Op_em_)=  &
         Ratedim_I(O_hv__Op_em_)*No2Io_V(UnitT_)
    Rate_I(CO2p_O__O2p_CO_)=  &
         Ratedim_I(CO2p_O__O2p_CO_)  &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(Op_CO2__O2p_CO_)=  &
         Ratedim_I(Op_CO2__O2p_CO_)*exp(log(8.0/3.0*T300/Tnu_body)*0.39) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_O__Op_CO2_)=  &
         Ratedim_I(CO2p_O__Op_CO2_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(O2p_em__O_O_)=  &
         Ratedim_I(O2p_em__O_O_)*exp(log(4.0*T300/TNu_body)*0.56)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)
    Rate_I(CO2p_em__CO_O_)=  &
         Ratedim_I(CO2p_em__CO_O_)*sqrt(T300/TNu_body)&
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)

    Rate_I(H_hv__Hp_em_)=  &
         Ratedim_I(H_hv__Hp_em_)*No2Io_V(UnitT_)
    Rate_I(Hp_O__Op_H_)=  &
         Ratedim_I(Hp_O__Op_H_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)    
    Rate_I(Op_H__Hp_O_)=  &
         Ratedim_I(Op_H__Hp_O_) &
         *No2Io_V(UnitT_)*No2Io_V(UnitN_)


    ReactionRate_I(CO2_hv__CO2p_em_)= &
         Rate_I(CO2_hv__CO2p_em_)*BodynDenNuSpecies_I(CO2_)
    PhoIon_I(CO2p_)=ReactionRate_I(CO2_hv__CO2p_em_) 

    ReactionRate_I(O_hv__Op_em_)= &
         Rate_I(O_hv__Op_em_)*BodynDenNuSpecies_I(O_)
    PhoIon_I(Op_)=ReactionRate_I(O_hv__Op_em_) 

    !charge exchange
    ReactionRate_I(CO2p_O__O2p_CO_)= &
         Rate_I(CO2p_O__O2p_CO_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(O2p_,CO2p_)=ReactionRate_I(CO2p_O__O2p_CO_)

    ReactionRate_I(Op_CO2__O2p_CO_)= &
         Rate_I(Op_CO2__O2p_CO_)* BodynDenNuSpecies_I(CO2_)
    CoeffSpecies_II(O2p_, Op_)=ReactionRate_I(Op_CO2__O2p_CO_)

    ReactionRate_I(CO2p_O__Op_CO2_)= &
         Rate_I(CO2p_O__Op_CO2_)* BodynDenNuSpecies_I(O_)
    CoeffSpecies_II(Op_,CO2p_)=ReactionRate_I(CO2p_O__Op_CO2_)


    CrossSection_I=CrossSectiondim_I*No2Io_V(unitN_)*No2Si_V(unitX_)*1.0e2

    Optdep =  sum(BodynDenNuSpecies_I*CrossSection_I*HNuSpecies_I)
    Productrate0 = max(exp(-Optdep), 1.0e-5)

    if(oktest)then
       write(*,*)'=======in set_multisp=============='
       write(*,*)'BodynDenNuSpecies_I=',BodynDenNuSpecies_I
       write(*,*)'HNuSpecies_I=',HNuSpecies_I
       write(*,*)'solar min, Procductrate=', productrate0, Optdep
       write(*,*)'CrossSection_dim_I*unitUSER_n*unitSI_x=',CrossSectiondim_I,&
            No2Io_V(unitN_),No2Si_V(unitX_)  
       write(*,*)''
    end if

    !ion density at the body
    BodyRhoSpecies_I(Hp_)=SW_rho*0.3

    BodyRhoSpecies_I(CO2p_)= Rate_I(CO2_hv__CO2p_em_)*Productrate0*&
         BodynDenNuSpecies_I(CO2_)/BodynDenNuSpecies_I(O_)/&
         (Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))
    BodyRhoSpecies_I(Op_)= (Rate_I(O_hv__Op_em_)*Productrate0+&
         Rate_I(CO2p_O__Op_CO2_)*BodyRhoSpecies_I(CO2p_))&
         *BodynDenNuSpecies_I(O_)/(BodynDenNuSpecies_I(CO2_)+3.0e5)/&
         Rate_I(Op_CO2__O2p_CO_)
    BodyRhoSpecies_I(O2p_)= SQRT((BodynDenNuSpecies_I(O_)*&
         BodyRhoSpecies_I(CO2p_)*Rate_I(CO2p_O__O2p_CO_)+ &
         BodynDenNuSpecies_I(CO2_)*BodyRhoSpecies_I(Op_)*&
         Rate_I(Op_CO2__O2p_CO_))/Rate_I(O2p_em__O_O_))
    BodyRhoSpecies_I(:)=BodyRhoSpecies_I(:)*&
         MassFluid_I(2:nFluid)

    if(oktest)then
       write(*,*)' set parameters of Mars: BodyRhoSpecies_I(i)=',&
            BodyRhoSpecies_I(1:nSpecies)
       write(*,*)'neutral density=', &
            BodynDenNuSpecies_I(:)
       write(*,*)'nu0=',nu0
       write(*,*)'Rate_I=', Rate_I
       write(*,*)'Rate_dim_I=', Ratedim_I       
    end if

  end subroutine set_multiSp_ICs

  !============================================================================

  ! This subroutine allows the user to apply initial conditions to the domain
  ! which are problem specific and cannot be created using the predefined
  ! options in BATSRUS.
  ! The variables specific to the problem are loaded from ModUser

  subroutine user_set_ICs

    use ModProcMH, ONLY : iProc
    use ModMain
    use ModAdvance
    use ModGeometry, ONLY : x2,y2,z2,x_BLK,y_BLK,z_BLK,R_BLK,true_cell
    use ModIO, ONLY : restart
    use ModPhysics
    use ModNumConst

    real :: Rmax, SinSlope, CosSlope,CosSZA, tempo
    real :: B4, dB4dx, zeta4, q4, epsi4, plobe, &
         XFace, YFace, ZFace
    real :: temp1,temp2,temp3
    integer :: i,j,k,q
    real, dimension(nIonFluid)::Temp_I
    integer:: iBoundary
    character (len=*), parameter :: NameSub = 'user_set_ics'
    logical:: DoTest, DoTestMe, DoTestCell
    !-------------------------------------------------------------------------

    if(iProc==PROCtest .and. globalBLK==BLKtest)then
       call set_oktest(NameSub, DoTest, DoTestMe)
    else
       DoTest=.false.; DoTestMe=.false.
    end if

    if(DoTestMe)then
       write(*,*)'in set_ics'
       write(*,*)'BodynDenNuSpecies_I(:)=',&
            BodynDenNuSpecies_I(:)
       WRITE(*,*)''
       write(*,*)'HNuSpecies_I(1:nNuSpecies)=',HNuSpecies_I(:)
       WRITE(*,*)''
       write(*,*)'Rbody=', Rbody
       write(*,*)''
    end if

    !calculate neutral density
    do k=1,nK; do j=1,nJ; do i=1,nI
       DoTestCell = DoTestMe .and. i==iTest .and. j==jTest .and. k==kTest
       !write(*,*)' BodynDenNuSpecies_I(:)=', BodynDenNuSpecies_I(:)
       if(R_BLK(i,j,k,globalBLK)<= Rbody)then
          nDenNuSpecies_CBI(i,j,k,globalBLK,:)=&
               BodynDenNuSpecies_I(:) 

       else if(R_BLK(i,j,k,globalBLK)< 3.0) then         
          nDenNuSpecies_CBI(i,j,k,globalBLK,:)=&
               BodynDenNuSpecies_I(:)*& 
               exp(-(R_BLK(i,j,k,globalBLK)-Rbody)&
               /HNuSpecies_I(:))

       else
          nDenNuSpecies_CBI(i,j,k,globalBLK,:)=0.0

       end if
!!$      write(*,*)'nDenNuSpecies_CBI(i,j,k,globalBLK,1:MaxNuSpecies)=',nDenNuSpecies_CBI(i,j,k,globalBLK,1:MaxNuSpecies) 
    end do; end do; end do

!!$    if(DoTestMe)then
!!$       write(*,*)'nDenNuSpecies_CBI(itest,jtest,ktest,BLKtest,1:nNuSPecies)=',&
!!$            nDenNuSpecies_CBI(itest,jtest,ktest,BLKtest,1:nNuSPecies) 
!!$       WRITE(*,*)''
!!$       !write(*,*)'nu(testcell)=', nu_BLK(itest,jtest,ktest,BLKtest)
!!$       WRITE(*,*)''
!!$    end if

    !    call neutral_density_averages  !calculate averaged neutral density

    ! calculate optical depth and producation rate
    do k=1,nK; do j=1,nJ; do i=1,nI
       cosSZA=(cHalf+sign(cHalf,x_BLK(i,j,k,globalBLK)))*&
            x_BLK(i,j,k,globalBLK)/max(R_BLK(i,j,k,globalBLK),1.0e-3)&
            +5.0e-4

       Optdep =max( sum(nDenNuSpecies_CBI(i,j,k,globalBLK,1:MaxNuSpecies)*&
            CrossSection_I(1:MaxNuSpecies)*HNuSpecies_I(1:MaxNuSpecies)),&
            6.0e-3)/cosSZA        
       if( Optdep<11.5 .and. x_BLK(i,j,k,globalBLK) > 0.0) then 
          Productrate_CB(i,j,k,globalBLK) = max(exp(-Optdep), 1.0e-5)
       else
          Productrate_CB(i,j,k,globalBLK) = 1.0e-5
       end if

    end do; end do; end do

    do k=1,nK; do j=1,nJ; do i=1,nI
       if(UseHotO) then
          nu_BLK(i,j,k,globalBLK)=&
               sum(nDenNuSpecies_CBI(i,j,k,globalBLK,:))*nu0         
          nDenNuSpecies_CBI(i,j,k,globalBLK,O_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,O_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,Ox_)

          nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,CO2x_)

          nDenNuSpecies_CBI(i,j,k,globalBLK,O_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,O_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,Oh_)+&
               nDenNuSpecies_CBI(i,j,k,globalBLK,Ohx_)

          nDenNuSpecies_CBI(i,j,k,globalBLK,H_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,H_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,Hx_)

       else
          nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,CO2x_)

          nDenNuSpecies_CBI(i,j,k,globalBLK,O_)= &
               nDenNuSpecies_CBI(i,j,k,globalBLK,O_)+ &
               nDenNuSpecies_CBI(i,j,k,globalBLK,Ox_)

          nu_BLK(i,j,k,globalBLK)=(nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)+&
               nDenNuSpecies_CBI(i,j,k,globalBLK,O_))*nu0

          nDenNuSpecies_CBI(i,j,k,globalBLK,H_)= 1.0e-5

       end if

    end do; end do; end do 


    if(UseMarsAtm)then
       if(maxval(R_BLK(:,:,:,globalBLK))<3.0*Rbody) call Mars_input

       do k=1,nK; do j=1,nJ; do i=1,nI
          if(UseHotO) then
             nDenNuSpecies_CBI(i,j,k,globalBLK,Oh_)= &
                  nDenNuSpecies_CBI(i,j,k,globalBLK,Oh_)+&
                  nDenNuSpecies_CBI(i,j,k,globalBLK,Ohx_)

             nDenNuSpecies_CBI(i,j,k,globalBLK,O_)= &
                  nDenNuSpecies_CBI(i,j,k,globalBLK,O_)+ &
                  nDenNuSpecies_CBI(i,j,k,globalBLK,Oh_)

             nu_BLK(i,j,k,globalBLK)=(nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,globalBLK,O_)+&
                  nDenNuSpecies_CBI(i,j,k,globalBLK,H_) )*nu0
          else              
             nu_BLK(i,j,k,globalBLK)=(nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)+&
                  nDenNuSpecies_CBI(i,j,k,globalBLK,O_))*nu0

             nDenNuSpecies_CBI(i,j,k,globalBLK,H_)= 1.0e-5

          end if

          Ionizationrate_CBI(i,j,k,globalBLK,CO2_)=&
               Ionizationrate_CBI(i,j,k,globalBLK,CO2_)*&
               nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)
          Ionizationrate_CBI(i,j,k,globalBLK,O_)=&
               Ionizationrate_CBI(i,j,k,globalBLK,O_)*&
               nDenNuSpecies_CBI(i,j,k,globalBLK,O_)

       end do; end do; end do 
    else
       do k=1,nK; do j=1,nJ; do i=1,nI
          Ionizationrate_CBI(i,j,k,globalBLK,O_)= &
               Rate_I(O_hv__Op_em_)&
               *nDenNuSpecies_CBI(i,j,k,globalBLK,O_)&
               *Productrate_CB(i,j,k,globalBLK)

          Ionizationrate_CBI(i,j,k,globalBLK,CO2_)= &
               Rate_I(CO2_hv__CO2p_em_)&
               *nDenNuSpecies_CBI(i,j,k,globalBLK,CO2_)&
               *Productrate_CB(i,j,k,globalBLK)
       end do;end do; end do
    end if
    nu1_BLK(:,:,:,globalBLK)=nu_BLK(:,:,:,globalBLK)



    do k=1-gcn,nK+gcn;do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
       if (R_BLK(i,j,k,globalBLK)< Rbody) then
          cosSZA=(cHalf+sign(cHalf,x_BLK(i,j,k,globalBLK)))*&
               x_BLK(i,j,k,globalBLK)/max(R_BLK(i,j,k,globalBLK),1.0e-3)+&
               1.0e-3
          State_VGB(:,i,j,k,globalBLK)   =  CellState_VI(:,body1_)
          State_VGB(OpRho_,i,j,k,globalBLK)= &
               CellState_VI(OpRho_,body1_)*cosSZA
          State_VGB(O2pRho_,i,j,k,globalBLK)= &
               CellState_VI(OpRho_,body1_)*sqrt(cosSZA)
          State_VGB(CO2pRho_,i,j,k,globalBLK)= &
               CellState_VI(OpRho_,body1_)*cosSZA
          State_VGB(rho_,i,j,k,globalBLK)  = &
               sum( State_VGB(iRho_I(2:nFluid),i,j,k,globalBLK))

          State_VGB(iPIon_I,i,j,k,globalBLK) = Tp_body*State_VGB(iRhoIon_I,i,j,k,globalBLK) &
               /MassIon_I(:) 
          State_VGB(P_,i,j,k,globalBLK) = &
               max(SW_p,sum(State_VGB(iPIon_I,i,j,k,globalBLK)))
       else

          State_VGB(:,i,j,k,globalBLK)   = CellState_VI(:,1)

          !write(*,*)'CellState_VI(HpRho_,1_)=',CellState_VI(HpRho_,1)
          !write(*,*)'State_VGB(HpRho_,i,j,k,globalBLK)=',State_VGB(HpRho_,i,j,k,globalBLK)
!!$           write(*,*)'State_VGB(rho_,i,j,k,globalBLK)=',State_VGB(rho_,i,j,k,globalBLK)
          State_VGB(Ux_:bz_,i,j,k,globalBLK)   =0.0
       end if
    end do;end do; end do;


!!$    if(DoTestMe)&
!!$         write(*,*)'state_VGB(body1_)=',&
!!$         CellState_VI(:,body1_),'cell_state_VI(:,1)=',CellState_VI(:,1)

    do k=1,nK; do j=1,nJ; do i=1,nI
       State_VGB(iRhoUx_I,i,j,k,globalBLK) = 0.0
       State_VGB(iRhoUy_I,i,j,k,globalBLK) = 0.0
       State_VGB(iRhoUz_I,i,j,k,globalBLK) = 0.0

       if (.not. (true_cell(i,j,k,globalBLK).and. &
            R_BLK(i,j,k,globalBLK)<1.5*Rbody) ) CYCLE


       cosSZA=(cHalf+sign(cHalf,x_BLK(i,j,k,globalBLK)))*&
            x_BLK(i,j,k,globalBLK)/max(R_BLK(i,j,k,globalBLK),1.0e-3)+&
            1.0e-3

       State_VGB(CO2pRho_,i,j,k,globalBLK)= &
            Ionizationrate_CBI(i,j,k,globalBLK,CO2_) &
            /nDenNuSpecies_CBI(i,j,k,globalBLK,O_)   &
            /(Rate_I(CO2p_O__O2p_CO_)+Rate_I(CO2p_O__Op_CO2_))

       !write(*,*)'State_VGB(CO2pRho_,i,j,k,globalBLK)',State_VGB(CO2pRho_,i,j,k,globalBLK)

       State_VGB(OpRho_,i,j,k,globalBLK)= &
            (Ionizationrate_CBI(i,j,k,globalBLK,O_) &
            +Rate_I(CO2p_O__Op_CO2_)                &
            *State_VGB(CO2pRho_,i,j,k,globalBLK)    &
            *nDenNuSpecies_CBI(i,j,k,globalBLK,O_)) &
            /(nDenNuSpecies_CBI(i,j,k,globalBLK, CO2_)+4.0e6)&
            /Rate_I(Op_CO2__O2p_CO_)

       !write(*,*)'State_VGB(OpRho_,i,j,k,globalBLK)',State_VGB(OpRho_,i,j,k,globalBLK)

       State_VGB(O2pRho_,i,j,k,globalBLK)= &
            SQRT((nDenNuSpecies_CBI(i,j,k,globalBLK,O_)*&
            State_VGB(CO2pRho_,i,j,k,globalBLK)*&
            Rate_I(CO2p_O__O2p_CO_)+&
            nDenNuSpecies_CBI(i,j,k,globalBLK, CO2_)*&
            State_VGB(OpRho_,i,j,k,globalBLK)*&
            Rate_I(Op_CO2__O2p_CO_)+1e-10)/Rate_I(O2p_em__O_O_))

       !write(*,*)'State_VGB(O2pRho_,i,j,k,globalBLK)',State_VGB(O2pRho_,i,j,k,globalBLK)

       ! Convert to mass densities
       State_VGB(iRho_I(2:nFluid),i,j,k,globalBLK)=&
            State_VGB(iRho_I(2:nFluid),i,j,k,globalBLK)*&
            MassFluid_I(2:nFluid)

   
 end do; end do; end do

 !


 do k=1,nK; do j=1,nJ; do i=1,nI

    if(.not.true_cell(i,j,k,globalBLK))CYCLE

    !IC for velocity
    State_VGB(iRhoUx_I,i,j,k,globalBLK) = 0.0
    State_VGB(iRhoUy_I,i,j,k,globalBLK) = 0.0
    State_VGB(iRhoUz_I,i,j,k,globalBLK) = 0.0


    State_VGB(rho_,i,j,k,globalBLK)   =&
         sum(State_VGB(iRhoIon_I,i,j,k,globalBLK))
  
    do q=1,nSpecies
       !write(*,*)'I got to the low density ratio'
       if(State_VGB(iRhoIon_I(q),i,j,k,globalBLK) < &
            LowDensityRatio* State_VGB(Rho_,i,j,k,globalBLK))then
          State_VGB(iRhoIon_I(q),i,j,k,globalBLK)= LowDensityRatio*&
               State_VGB(Rho_,i,j,k,globalBLK)
       end if
    end do
     
     State_VGB(rho_,i,j,k,globalBLK)   =&
         sum(State_VGB(iRhoIon_I,i,j,k,globalBLK))
    
      State_VGB(P_,i,j,k,globalBLK) = &
               max(SW_p,(sum(State_VGB(iRhoIon_I(:),i,j,k,globalBLK)/(MassIon_I(:))))*Tp_body)

 State_VGB(iPIon_I(1:nIonFluid),i,j,k,globalBLK)=State_VGB(P_,i,j,k,globalBLK)&
/(sum(State_VGB(iRhoIon_I(:),i,j,k,globalBLK)/(MassIon_I(:))))*&
         State_VGB(iRhoIon_I(1:nIonFluid),i,j,k,globalBLK)&
         /MassFluid_I(2:nFluid)


    Temp_I=State_VGB(iPIon_I(1:nIonFluid),i,j,k,globalBLK)/&
         (State_VGB(iRhoIon_I(1:nIonFluid),i,j,k,globalBLK)/MassFluid_I(2:nFluid))


   

    !if(DoTestMe)then
    !   write(*,*)'Tp_body=',Tp_body
    !   write(*,*)'Temp_I in set_ICs=',Temp_I
    !   write(*,*)'TNu_body=',TNu_body
    !   write(*,*)'Ti_body=',Ti_body
    !end if

 end do; end do; end do

 if(DoTestMe)then
       write(*,*)'!!!!Checking that the low density ratio is applied '
       write(*,*)'State_VGB(iRhoIon_I(:))=',State_VGB(iRhoIon_I(:),iTest,jTest,kTest,BLKtest)
       write(*,*)'sum(State_VGB(iRhoIon_I(:)))=',sum(State_VGB(iRhoIon_I(:),iTest,jTest,kTest,BLKtest))
       write(*,*)'State_VGB(Rho_)=',State_VGB(Rho_,iTest,jTest,kTest,BLKtest)
       end if


 time_BLK(:,:,:,globalBLK) = 0.00


end subroutine user_set_ICs

  !============================================================================
  subroutine user_face_bcs(VarsGhostFace_V)

    use ModSize,       ONLY: nDim,West_,North_,Top_	
    use ModMain,       ONLY: UseRotatingBc, iTest, jTest, kTest, ProcTest, BlkTest, GLOBALBLK
    use ModProcMH,   ONLY: iProc
    use ModVarIndexes, ONLY: nVar, OpRho_, O2pRho_, CO2pRho_, HpRho_,HpP_,O2pP_,OpP_,CO2pP_,iRhoUx_I,iRhoUy_I,iRhoUz_I
    use ModPhysics,    ONLY: SW_rho, SW_p, SW_T_dim
    use ModFaceBc,     ONLY: FaceCoords_D, VarsTrueFace_V, iFace,jFace,kFace

    real, intent(out):: VarsGhostFace_V(nVar)

    real:: XFace,YFace,ZFace,rFace,rFace2
    real:: v_phi(3)
    real:: cosSZA 
    real:: uDotR_I(nFluid), bDotR
    integer:: i,j,k
    character (len=*), parameter :: NameSub = 'user_face_bcs'
    logical:: DoTest, DoTestMe, DoTestCell
    !-------------------------------------------------------------------------

    if(iProc==PROCtest .and. globalBLK==BLKtest)then
       call set_oktest(NameSub, DoTest, DoTestMe)
    else
       DoTest=.false.; DoTestMe=.false.
    end if

    DoTestCell = DoTestMe .and. iFace==iTest .and. jFace==jTest .and. kFace==kTest

    XFace = FaceCoords_D(1)
    YFace = FaceCoords_D(2)
    ZFace = FaceCoords_D(3)

    rFace2 = XFace**2 + YFace**2 + ZFace**2
    rFace  = sqrt(rFace2)

    !Apply boundary conditions
    cosSZA=(0.5+sign(0.5,XFace)) * XFace/max(RFace,1.0e-3) + 1.0e-3

    VarsGhostFace_V(OpRho_)  = BodyRhoSpecies_I(Op_) *cosSZA

    VarsGhostFace_V(O2pRho_) = BodyRhoSpecies_I(O2p_)*sqrt(cosSZA)

    VarsGhostFace_V(CO2pRho_)= BodyRhoSpecies_I(CO2p_)*cosSZA

    VarsGhostFace_V(HpRho_)  = SW_rho*0.3
    VarsGhostFace_V(rho_) = sum(VarsGhostFace_V(iRhoIon_I))    

    VarsGhostFace_V(iPIon_I)=Tp_body*VarsGhostFace_V(iRhoIon_I)/MassIon_I

    VarsGhostFace_V(P_)=VarsGhostFace_V(HpP_)+VarsGhostFace_V(O2pP_)&
         +VarsGhostFace_V(OpP_)+VarsGhostFace_V(CO2pP_)


    ! Reflective in radial direction
    uDotR_I = (VarsTrueFace_V(iRhoUx_I)*FaceCoords_D(1)+ &
         VarsTrueFace_V(iRhoUy_I)*FaceCoords_D(2)+ &
         VarsTrueFace_V(iRhoUz_I)*FaceCoords_D(3))/ rFace2

    ! bDotR = sum(VarsTrueFace_V(Bx_:Bz_)*FaceCoords_D)/rFace2

    VarsGhostFace_V(iRhoUx_I) = VarsTrueFace_V(iRhoUx_I) - 2*uDotR_I*FaceCoords_D(1)
    VarsGhostFace_V(iRhoUy_I) = VarsTrueFace_V(iRhoUy_I) - 2*uDotR_I*FaceCoords_D(2)
    VarsGhostFace_V(iRhoUz_I) = VarsTrueFace_V(iRhoUz_I) - 2*uDotR_I*FaceCoords_D(3)
    ! VarsGhostFace_V(Bx_:Bz_) = VarsTrueFace_V(Bx_:Bz_) - 2*bDotR*FaceCoords_D
    VarsGhostFace_V(Bx_:Bz_) = 0.0

    If(DoTestCell) then
       write(*,*)'VarsGhostFace_V(iRhoIon_I)=',VarsGhostFace_V(iRhoIon_I)
       write(*,*)'VarsGhostFace_V(iRhoUx_I(1))=',VarsGhostFace_V(iRhoUx_I(1))
       write(*,*)'VarsGhostFace_V(iRhoUy_I(1))=',VarsGhostFace_V(iRhoUy_I(1))
       write(*,*)'VarsGhostFace_V(iRhoUz_I(1))=',VarsGhostFace_V(iRhoUz_I(1))
       write(*,*)'VarsGhostFace_V(P_)=',VarsGhostFace_V(P_)
        write(*,*)'VarsGhostFace_V(Bx_ : Bz_)=',VarsGhostFace_V(Bx_: Bz_)
    end if   

    ! Apply corotation?
    if (UseRotatingBc) then
       call calc_corotation_velocities(FaceCoords_D, v_phi)
       VarsGhostFace_V(iRhoUx_I) = VarsGhostFace_V(iRhoUx_I) + 2*v_phi(1)
       VarsGhostFace_V(iRhoUy_I) = VarsGhostFace_V(iRhoUy_I) + 2*v_phi(2)
       VarsGhostFace_V(iRhoUz_I) = VarsGhostFace_V(iRhoUz_I) + 2*v_phi(3)
    end if

  end subroutine user_face_bcs

  !========================================================================

  subroutine user_set_boundary_cells(iBLK)
    use ModGeometry
    use ModMain	
    use ModNumConst	

    integer,intent(in)::iBLK
    !-----------------------------------------------------------------------
    !  SHOULD define IsBoundaryCell_GI(:,:,:,ExtraBc_) using
    !  a boundary condition for iBLK block
    !  EXAMPLE: OUTER SPHERICAL BOUNDARY of radius of 100.
    !  IsBoundaryCell_GI(:,:,:,ExtraBc_) = R_BLK(:,:,:,iBLK)<100.
    if (index(TypeGeometry,'spherical')>0)then
       if(XyzStart_BLK(Theta_,iBLK)<dz_BLK(iBLK))then
          !	IsBoundaryCell_GI(:,:,1-gcn:0,ExtraBc_)=.true.
          !	IsBoundaryCell_GI(1:nI,1:nJ,1-gcn:0,ExtraBc_)=.false.

          !	IsBoundaryCell_GI(:,:,1-gcn:0,ExtraBc_)=.true.
          IsBoundaryCell_GI(nI+1:nI+gcn,:,1-gcn:0,ExtraBc_)=.true.
          IsBoundaryCell_GI(1-gcn:0,:,1-gcn:0,ExtraBc_)=.true.	
       elseif(XyzStart_BLK(Theta_,iBLK)+nK*dz_BLK(iBLK)>cPi)then
          !        IsBoundaryCell_GI(:,:,nK+1:nK+gcn,ExtraBc_)=.true.
          !        IsBoundaryCell_GI(1:nI,1:nJ,nK+1:nK+gcn,ExtraBc_)=.false.

          !        IsBoundaryCell_GI(:,:,nK+1:nK+gcn,ExtraBc_)=.true.
          IsBoundaryCell_GI(nI+1:nI+gcn,:,nK+1:nK+gcn,ExtraBc_)=.true.
          IsBoundaryCell_GI(1-gcn:0,:,nK+1:nK+gcn,ExtraBc_)=.true.
       end if
    end if
  end subroutine user_set_boundary_cells

  !========================================================================
  !============================================================================

  subroutine user_set_plot_var(iBlock, NameVar, IsDimensional, &
       PlotVar_G, PlotVarBody, UsePlotVarBody, &
       NameTecVar, NameTecUnit, NameIdlUnit, IsFound)

    use ModPhysics, ONLY: rBody, No2Io_V, UnitRho_, BodyRho_I
    use ModMain, ONLY: Body1_
    use ModAdvance, ONLY: State_VGB
    use ModGeometry, ONLY: x_BLK, y_BLK, z_BLK, r_BLK, IsBoundaryBlock_IB
    use ModMain, ONLY: iTest, jTest, kTest, ProcTest, BlkTest, &
         GLOBALBLK
    use ModProcMH,   ONLY: iProc
    use ModSize, ONLY: nI, nJ, nK
    use ModMultiFluid

    integer,          intent(in)   :: iBlock
    character(len=*), intent(in)   :: NameVar
    logical,          intent(in)   :: IsDimensional
    real,             intent(out)  :: PlotVar_G(-1:nI+2, -1:nJ+2, -1:nK+2)
    real,             intent(out)  :: PlotVarBody
    logical,          intent(out)  :: UsePlotVarBody
    character(len=*), intent(inout):: NameTecVar
    character(len=*), intent(inout):: NameTecUnit
    character(len=*), intent(inout):: NameIdlUnit
    logical,          intent(out)  :: IsFound

    character (len=*), parameter :: NameSub = 'user_set_plot_var'
    
    integer :: iVar, i, j, k, iIon
    logical :: oktest,oktest_me

    !-------------------------------------------------------------------
    if(iProc==PROCtest .and. iBlock==BLKtest)then
       call set_oktest('user_set_plot_var',oktest,oktest_me)
    else
       oktest=.false.; oktest_me=.false.
    end if

    IsFound=.true.

    select case(NameVar)
    case('hp')
       iVar=HpRho_
       iIon=1
    case('o2p')
       iVar=O2pRho_
       iIon=2
    case('op')
       iVar=OpRho_
       iIon=3
    case('co2p')
       iVar=CO2pRho_
       iIon=4
    case default
       IsFound= .false.
       call stop_mpi(NameSub//': unimplemented variable='//NameVar)
    end select
    NameTecUnit = '[amu/cm3]'
    NameIdlUnit = 'amu/cm3'
    PlotVar_G   = State_VGB(iVar,:,:,:,iBlock)/MassIon_I(iIon)
    PlotVarBody = BodyRho_I(iIon+1)

    if(IsDimensional) PlotVar_G = PlotVar_G*No2Io_V(UnitRho_)

  end subroutine user_set_plot_var

!==============================================================================
end module ModUser
