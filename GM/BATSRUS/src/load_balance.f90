!^CFG COPYRIGHT UM
subroutine load_balance(DoMoveCoord, DoMoveData, nBlockMoved)
  use ModProcMH
  use ModMain
  use ModImplicit, ONLY : UsePartImplicit !^CFG IF IMPLICIT
  use ModAdvance, ONLY: iTypeAdvance_B, iTypeAdvance_BP, &
       SkippedBlock_, SteadyBlock_, SteadyBoundBlock_, ExplBlock_, ImplBlock_
  use ModPartSteady, ONLY: UsePartSteady
  use ModAMR, ONLY : availableBLKs
  use ModParallel
  use ModIO
  use ModMpi
  implicit none

  ! Load balance grid using Peano-Hilbert ordering of blocks
  ! Coordinates are moved if DoMoveCoord is true.
  ! Data is moved with the blocks if DoMoveData is true.

  logical, intent(in) :: DoMoveCoord, DoMoveData
  integer, intent(out):: nBlockMoved

  ! Maximum number of attempts to accomplish the load balancing
  ! The algorithm needs multiple tries if the actual number of blocks
  ! is very close to the maximum number of blocks and many blocks are moved
  integer, parameter :: MaxTry = 100
  
  ! Set this logical to .false. to return to the previous version,
  ! which did not move the B0, body force and heating variables.
  ! There is another declaration in subroutine move_block! Change together!!!
  logical, parameter :: DoMoveExtraData = .true.

  ! Maximum number of block types to be load balanced separately
  integer, parameter :: MaxBlockType = 2

  ! Index for block type
  integer :: iType

  ! Global index for the various block types
  integer :: iBlockALL_I(MaxBlockType)

  ! Number of blocks for each type
  integer :: nBlockALL_I(MaxBlockType)

  integer :: iError
  integer :: iBlockALL, iBlock
  integer :: iTypeAdvanceLimit

  integer :: iBlockFrom, iProcFrom, iBlockTo, iProcTo, iTry

  logical :: SkippedAnyBlock

  logical :: DoTest, DoTestMe

  logical :: DoFixVar_B(MaxBlock)

  !---------------------------------------------------------------------------
  call set_oktest('load_balance',DoTest,DoTestMe)

  if (DoTest) write(*,*)'load_balance: iProc, DoMoveCoord, DoMoveData=',&
       iProc, DoMoveCoord, DoMoveData

  ! starting value of number of blocks moved between processors
  nBlockMoved = 0

  ! Find the last used block on the processor
  do iBlock = nBlockMax,1,-1
     nBlock = iBlock
     if (.not.unusedBLK(iBlock)) EXIT
  end do

  if(DoMoveData.and. .not.DoMoveCoord)call stop_mpi(&
       'ERROR in load_balance: DoMoveData=T and DoMoveCoord=F !!!')

  ! Set the separator between the block types to be load balanced separately
  if(UsePartSteady)then
     ! Type1 is steady boundary + explicit, Type2 is steady
     iTypeAdvanceLimit = SteadyBlock_
  elseif(UsePartImplicit)then                !^CFG IF IMPLICIT
     ! Type1 is implicit, type2 is explicit  !^CFG IF IMPLICIT
     iTypeAdvanceLimit = ExplBlock_          !^CFG IF IMPLICIT
  else
     ! Type1 is explicit or implicit (for fully implicit), Type2 is empty
     iTypeAdvanceLimit = SkippedBlock_
  endif

  ! Set initial block advance types here 
  ! if data is not yet associated with the blocks.
  if(.not. DoMoveData)then
     where(unusedBLK)
        iTypeAdvance_B = SkippedBlock_
     elsewhere
        iTypeAdvance_B = ExplBlock_
     endwhere
     ! Gather global information (note: UnusedBlock_BP is not set yet!!!)
     call MPI_ALLGATHER(iTypeAdvance_B, MaxBlock, MPI_INTEGER, &
          iTypeAdvance_BP, MaxBlock, MPI_INTEGER, iComm, iError)
  end if

  !^CFG IF IMPLICIT BEGIN
  !\
  ! Select blocks for implicit/local time stepping
  !/
  ! Find number of implicit and explicit blocks.   
  ! Partial selection is only possible if dt_BLK and coords are known
  ! i.e. if DoMoveCoord is true.
  call select_stepping(DoMoveCoord)
  !^CFG END IMPLICIT

  ! Number of blocks of type 1 and type 2
  nBlockALL_I(1) = count(iTypeAdvance_BP(1:nBlockMax,:) >  iTypeAdvanceLimit)
  nBlockALL_I(2) = count(iTypeAdvance_BP(1:nBlockMax,:) <= iTypeAdvanceLimit &
       .and.             iTypeAdvance_BP(1:nBlockMax,:) >  SkippedBlock_ )


  if(DoTestMe)write(*,*)'load_balance starting: nBlockMax=',nBlockMax
  if(DoTest)then
     write(*,*)'load_balance starting: me, nBlock, nBlockUsed=',&
          iProc, nBlock, count(.not.unusedBLK(1:nBlock))
     if(iTypeAdvanceLimit > SkippedBlock_) then
        write(*,*)'load_balance iProc, MaxType, MinType=',iProc,&
             maxval(iTypeAdvance_BP(1:nBlockMax,:)),&
             minval(iTypeAdvance_BP(1:nBlockMax,:))
        write(*,*)'load_balance starting iProc, nBlockALL_I=',iProc,nBlockALL_I
        write(*,*)'load_balance starting iProc, nType1=',&
             iProc,count(iTypeAdvance_B(1:nBlockMax) > iTypeAdvanceLimit)
     end if
  end if

  if (nProc==1) RETURN

  if (index(test_string,'NOLOADBALANCE')>0) RETURN

  call timing_start('load_balance')

  if(DoMoveData) DoFixVar_B = .false. ! initialize variable fixing flags

  TRY: do iTry=1,MaxTry

     skippedAnyBlock=.false.
     iBlockALL_I = 0

     TREE: do iBlockALL  = 1, nBlockALL

        iBlockFrom = iBlock_A(iBlockALL)
        iProcFrom  = iProc_A(iBlockALL)

        iProcTo = (nProc*iBlockALL-1)/nBlockALL

        ! Figure out the block type
        if(iTypeAdvance_BP(iBlockFrom,iProcFrom) > iTypeAdvanceLimit)then
           iType = 1
        else
           iType = 2
        endif

        ! Increase the index for this block type and select target processor
        iBlockALL_I(iType) = iBlockALL_I(iType) + 1
        iProcTo = (nProc*iBlockALL_I(iType)-1) / nBlockALL_I(iType)

        !DEBUG
        !if(iProc==0)write(*,*)'iBlockALL,iBlockFrom,iProcFrom,iProcTo=',&
        !     iBlockALL,iBlockFrom,iProcFrom,iProcTo

        if(iProcTo == iProcFrom) CYCLE TREE

        ! Check if ProcTo would get too many blocks
        if(availableBLKs(0,iProcTo) > MaxBlock) then
           skippedAnyBlock = .true.
           CYCLE TREE
        end if

        ! Select next available block
        iBlockTo = availableBLKs(availableBLKs(0,iProcTo),iProcTo)
        availableBLKs(0,iProcTo)   = availableBLKs(0,iProcTo)+1
        availableBLKs(0,iProcFrom) = availableBLKs(0,iProcFrom)-1
        availableBLKs(availableBLKs(0,iProcFrom),iProcFrom) = iBlockFrom

        ! Move the block from ProcFrom to Procto
        call move_block(DoMoveCoord, DoMoveData, iBlockALL,&
             iBlockFrom, iProcFrom, iBlockTo, iProcTo)

        nBlockMoved=nBlocKMoved+1

        if(DoMoveData .and. iProc==iProcTo) DoFixVar_B(iBlockTo)=.true.

        iBlock_A(iBlockALL) = iBlockTo
        iProc_A(iBlockALL)  = iProcTo

     end do TREE
     if(.not.skippedAnyBlock) EXIT TRY
  end do TRY

  call MPI_ALLREDUCE(nBlock, nBlockMax, 1, MPI_INTEGER, MPI_MAX, &
       iComm, iError)

  ! Change decomposition ID if any blocks were moved
  if(nBlockMoved > 0)iNewDecomposition = mod(iNewDecomposition+1,10000)

  ! Fix variables if any data was moved
  if(DoMoveData .and. nBlockMoved > 0)then
     !call timing_start('load_fix_var')
     do iBlock = 1,nBlock
        if(.not.DoFixVar_B(iBlock)) CYCLE
        globalBLK = iBlock
        if(useConstrainB) call Bface2Bcenter          !^CFG IF CONSTRAINB
        call correctE

        if(DoMoveExtraData)then
           call set_b0_matrix(iBlock)                 !^CFG IF CARTESIAN
           !call calc_b0source_covar(iBlock)          !^CFG IF NOT CARTESIAN
           call init_conservative_facefluxes(iBlock)
        else
           call calc_other_soln_vars(iBlock)
        end if
     end do
     !call timing_stop('load_fix_var')
  end if

  call timing_stop('load_balance')

  if(DoTestMe)write(*,*)'load_balance finished: ',&
       'nTry, nBlockMax, nBlockMoved=',&
       iTry, nBlockMax, nBlockMoved

  if(DoTest)write(*,*)&
       'load_balance finished: me, nBlock, nBlockUsed=',&
       iProc, nBlock, count(.not.unusedBLK(1:nBlock)), &
       count(iTypeAdvance_B /= SkippedBlock_)

  if(DoTest .and. iTypeAdvanceLimit > SkippedBlock_) &
       write(*,*)'load_balance finished: iProc, nType1=',&
       iProc,count(iTypeAdvance_B(1:nBlockMax) > iTypeAdvanceLimit)

end subroutine load_balance

!=============================================================================
subroutine move_block(DoMoveCoord, DoMoveData, iBlockALL, &
     iBlockFrom, iProcFrom, iBlockTo,iProcTo)

  ! Move block with global block number iBlockALL 
  ! from block iBlockFrom from processor iProcFrom 
  ! to   block iBlockTo   on   processor iProcTo.
  ! Move flow variables if DoMoveData is true.

  use ModProcMH
  use ModMain
  use ModVarIndexes
  use ModAdvance, ONLY : State_VGB, &
       fbody_x_BLK, fbody_y_BLK, fbody_z_BLK, qheat_BLK, &
       B0xCell_BLK, B0yCell_BLK, B0zCell_BLK, &
       B0xFace_x_BLK, B0yFace_x_BLK, B0zFace_x_BLK, &
       B0xFace_y_BLK, B0yFace_y_BLK, B0zFace_y_BLK, &
       B0xFace_z_BLK, B0yFace_z_BLK, B0zFace_z_BLK, &
       iTypeAdvance_B, iTypeAdvance_BP, SkippedBlock_
  use ModGeometry, ONLY : dx_BLK,dy_BLK,dz_BLK,xyzStart_BLK
  use ModParallel
  use ModImplicit                                         !^CFG IF IMPLICIT
  use ModCT, ONLY : Bxface_BLK,Byface_BLK,Bzface_BLK      !^CFG IF CONSTRAINB
  use ModRaytrace, ONLY : ray                             !^CFG IF RCM
  use ModMpi
  implicit none

  logical, intent(in) :: DoMoveCoord, DoMoveData
  integer, intent(in) :: iBlockALL, iBlockFrom, iProcFrom, iBlockTo,iProcTo

  ! Set this logical to .false. to return to the previous version,
  ! which did not move the B0, body force and heating variables.
  ! There is another declaration in subroutine load_balance! Change together!!!
  logical, parameter :: DoMoveExtraData = .true.

  integer, parameter :: nScalarBLK=13, &
       nCellGhostBLK=(nI+4)*(nJ+4)*(nK+4)
  integer, parameter :: nExtraData = &
       3*nCellGhostBLK +                 & ! B0*Cell
       3*((nI+3)*(nJ+2)*(nK+2)           & ! B0*Face_x
       +  (nI+2)*(nJ+3)*(nK+2)           & ! B0*Face_y
       +  (nI+2)*(nJ+2)*(nK+3)) +        & ! B0*Face_x
       4*nIJK                              ! fbody_* and qheat
       
  integer, parameter :: nDataBLK= &
       nwIJK +                           & !^CFG IF IMPLICIT
       3*2*nIJK +                        & !^CFG IF RCM
       nScalarBLK +                      & ! scalars
       nVar*nCellGhostBLK +              & ! State_VGB
       nExtraData                          ! B0, fbody, qheat

  real, dimension(nDataBLK) :: BlockData_I
  integer :: iData, itag, i, j, k, i1,i2, iVar, iw, iSize
  integer :: iError
  integer :: status(MPI_STATUS_SIZE,1)

  logical :: DoTest, DoTestMe
  !----------------------------------------------------------------------------

  if((iProcFrom==PROCtest .and. iBlockFrom==BLKtest).or. &
       (iProcTo  ==PROCtest .and. iBlockTo  ==BLKtest))then
     call set_oktest('move_block',DoTest,DoTestMe)
  else
     DoTest=.false.; DoTestMe=.false.
  end if
  if(DoTestMe)write(*,*)'iBlockALL,iBlockFrom,iProcFrom, iBlockTo,iProcTo=',&
       iBlockALL,iBlockFrom,iProcFrom, iBlockTo,iProcTo

  if (iProc == iProcFrom) then
     if (DoMoveCoord) call send_block_data
     unusedBLK(iBlockFrom)   = .true.
     iTypeAdvance_B(iBlockFrom) = SkippedBlock_
     do
        if (nBlock==0) EXIT
        if (.not.unusedBLK(nBlock)) EXIT
        nBlock = nBlock-1
     end do
  end if

  if (iProc == iProcTo) then
     unusedBLK(iBlockTo) = .false.
     iTypeAdvance_B(iBlockTo) = iTypeAdvance_BP(iBlockFrom,iProcFrom)

     if (DoMoveCoord) call recv_block_data
     nBlock = max(nBlock, iBlockTo)
     global_block_number(iBlockTo) = iBlockALL
  end if

  ! Update global advance type info
  iTypeAdvance_BP(iBlockTo, iProcTo) = iTypeAdvance_BP(iBlockFrom,iProcFrom)
  iTypeAdvance_BP(iBlockFrom,iProcFrom) = SkippedBlock_

  ! Fix pointers
  call move_octree_block(iBlockFrom,iProcFrom,iBlockTo,iProcTo)

contains
  !============================================================================
  subroutine send_block_data
    globalBLK = iBlockFrom
    BlockData_I(1)   = dx_BLK(iBlockFrom)
    BlockData_I(2)   = dy_BLK(iBlockFrom)
    BlockData_I(3)   = dz_BLK(iBlockFrom)
    BlockData_I(4:6) = xyzStart_BLK(:,iBlockFrom)
    BlockData_I(7)   = dt_BLK(iBlockFrom)
    BlockData_I(8:13)= real(neiLev(:,iBlockFrom))

    iData = nScalarBLK

    if(DoMoveData)then
       if (UseConstrainB) then                      !^CFG IF CONSTRAINB BEGIN
          do iVar=1,Bx_-1
             do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
                iData = iData+1
                BlockData_I(iData) = State_VGB(iVar,i,j,k,iBlockFrom)
             end do; end do; end do
          end do
          do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
             iData = iData+1
             BlockData_I(iData) = Bxface_BLK(i,j,k,iBlockFrom)
          end do; end do; end do
          do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
             iData = iData+1
             BlockData_I(iData) = Byface_BLK(i,j,k,iBlockFrom)
          end do; end do; end do
          do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
             iData = iData+1
             BlockData_I(iData) = Bzface_BLK(i,j,k,iBlockFrom)
          end do; end do; end do
          do iVar=Bz_+1,nVar
             do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
                iData = iData+1
                BlockData_I(iData) = State_VGB(iVar,i,j,k,iBlockFrom)
             end do; end do; end do
          end do
       else                                         !^CFG END CONSTRAINB
          do k=1-gcn,nK+gcn;do j=1-gcn,nJ+gcn;do i=1-gcn,nI+gcn;do iVar=1,nVar
             iData = iData+1
             BlockData_I(iData) = State_VGB(iVar,i,j,k,iBlockFrom)
          end do; end do; end do; end do
       end if                                       !^CFG IF CONSTRAINB

       if(DoMoveExtraData)then
          ! B0*Cell
          do k=1-gcn,nK+gcn;do j=1-gcn,nJ+gcn;do i=1-gcn,nI+gcn
             iData = iData+1
             BlockData_I(iData) = B0xCell_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0yCell_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0zCell_BLK(i,j,k,iBlockFrom)
          end do; end do; end do

          ! B0*Face_x
          do k=0,nK+1; do j=0,nJ+1; do i=0,nI+2
             iData = iData+1
             BlockData_I(iData) = B0xFace_x_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0yFace_x_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0zFace_x_BLK(i,j,k,iBlockFrom)
          end do; end do; end do

          ! B0*Face_y
          do k=0,nK+1; do j=0,nJ+2; do i=0,nI+1
             iData = iData+1
             BlockData_I(iData) = B0xFace_y_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0yFace_y_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0zFace_y_BLK(i,j,k,iBlockFrom)
          end do; end do; end do

          ! B0*Face_z
          do k=0,nK+2; do j=0,nJ+1; do i=0,nI+1
             iData = iData+1
             BlockData_I(iData) = B0xFace_z_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0yFace_z_BLK(i,j,k,iBlockFrom)
             iData = iData+1
             BlockData_I(iData) = B0zFace_z_BLK(i,j,k,iBlockFrom)
          end do; end do; end do

          ! fbody*
          if(UseGravity.or.UseRotatingFrame)then
             do k=1,nK; do j=1,nJ; do i=1,nI
                iData = iData+1
                BlockData_I(iData) = fBody_x_BLK(i,j,k,iBlockFrom)
                iData = iData+1
                BlockData_I(iData) = fBody_y_BLK(i,j,k,iBlockFrom)
                iData = iData+1
                BlockData_I(iData) = fBody_z_BLK(i,j,k,iBlockFrom)
             end do; end do; end do
          end if

          ! heating
          if(UseUserHeating)then
             do k=1,nK; do j=1,nJ; do i=1,nI
                iData = iData+1
                BlockData_I(iData) = qHeat_BLK(i,j,k,iBlockFrom)
             end do; end do; end do
          end if
       end if ! DoMoveExtraData

       if(UseBDF2 .and. n_prev > 0)then             !^CFG IF IMPLICIT BEGIN
          do iw=1,nw; do k=1,nK; do j=1,nJ; do i=1,nI; iData = iData+1
             BlockData_I(iData) = w_prev(i,j,k,iw,iBlockFrom)
          end do; end do; end do; end do
       end if                                       !^CFG END IMPLICIT

       if(UseIM)then                                !^CFG IF RCM BEGIN
          do k=1,nK; do j=1,nJ; do i=1,nI; do i2=1,2; do i1=1,3
             iData = iData+1
             BlockData_I(iData) = ray(i1,i2,i,j,k,iBlockFrom)
          end do; end do; end do; end do; end do
       end if                                       !^CFG END RCM
    end if

    if(DoTest)write(*,*)'sending BlockData_I: iData=',iData,' from',&
         iProc,' to',iProcTo
    itag=1
    call MPI_SEND(BlockData_I, iData, MPI_REAL, iProcTo, &
         itag, iComm, iError)

    if(DoTest)write(*,*)'send done, me=',iProc

  end subroutine send_block_data

  !============================================================================
  subroutine recv_block_data

    globalBLK = iBlockTo

    if(DoTest)write(*,*)'recv BlockData_I: iData=',nDataBLK,' from',&
         iProcFrom,' to',iProc

    if(DoMoveData)then
       ! This is only an upper estimate of the number of reals received
       iSize = nDataBLK
       if(.not.DoMoveExtraData) &
            iSize = iSize - nExtraData
       if(.not.(UseBDF2 .and. n_prev > 0)) &    !^CFG IF IMPLICIT
            iSize = iSize - nWIJK               !^CFG IF IMPLICIT
       if(.not.(UseIM)) &                       !^CFG IF RCM
            iSize = iSize - 3*2*nIJK            !^CFG IF RCM
    else
       iSize= nScalarBLK
    end if
    itag=1
    call MPI_RECV(BlockData_I, iSize, MPI_REAL, iProcFrom, &
         itag, iComm, status, iError)

    if(DoTest)write(*,*)'recv done, me=',iProc

    dx_BLK(iBlockTo)           = BlockData_I(1)
    dy_BLK(iBlockTo)           = BlockData_I(2)
    dz_BLK(iBlockTo)           = BlockData_I(3)
    xyzStart_BLK(1:3,iBlockTo) = BlockData_I(4:6)
    dt_BLK(iBlockTo)           = BlockData_I(7)
    neiLev(:,iBlockTo)         = nint(BlockData_I(8:13))
    ! Put neighbor info into other arrays 
    ! (used for B0 face restriction)
    neiLeast(iBlockTo)         = neiLev(east_,iBlockTo)
    neiLwest(iBlockTo)         = neiLev(west_,iBlockTo)
    neiLsouth(iBlockTo)        = neiLev(south_,iBlockTo)
    neiLnorth(iBlockTo)        = neiLev(north_,iBlockTo)
    neiLbot(iBlockTo)          = neiLev(bot_,iBlockTo)
    neiLtop(iBlockTo)          = neiLev(top_,iBlockTo)

    ! Fix geometry
    call fix_block_geometry(iBlockTo)

    if (.not.DoMoveData) RETURN

    ! Read rest of the blockData buffer
    iData = nScalarBLK
    if (UseConstrainB) then                      !^CFG IF CONSTRAINB BEGIN
       do iVar=1,Bx_-1
          do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
             iData = iData+1
             State_VGB(iVar,i,j,k,iBlockTo) = BlockData_I(iData)
          end do; end do; end do
       end do
       do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
          iData = iData+1
          Bxface_BLK(i,j,k,iBlockTo) = BlockData_I(iData) 
       end do; end do; end do
       do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
          iData = iData+1
          Byface_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do
       do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
          iData = iData+1
          Bzface_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do
       do iVar=Bz_+1,nVar
          do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn
             iData = iData+1
             State_VGB(iVar,i,j,k,iBlockTo) = BlockData_I(iData)
          end do; end do; end do
       end do
    else                                         !^CFG END CONSTRAINB
       do k=1-gcn,nK+gcn; do j=1-gcn,nJ+gcn; do i=1-gcn,nI+gcn; do iVar=1,nVar
          iData = iData+1
          State_VGB(iVar,i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do; end do
    end if                                      !^CFG IF CONSTRAINB

    if(DoMoveExtraData)then
       ! B0*Cell
       do k=1-gcn,nK+gcn;do j=1-gcn,nJ+gcn;do i=1-gcn,nI+gcn
          iData = iData+1
          B0xCell_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0yCell_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0zCell_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do

       ! B0*Face_x
       do k=0,nK+1; do j=0,nJ+1; do i=0,nI+2
          iData = iData+1
          B0xFace_x_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0yFace_x_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0zFace_x_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do
       
       ! B0*Face_y
       do k=0,nK+1; do j=0,nJ+2; do i=0,nI+1
          iData = iData+1
          B0xFace_y_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0yFace_y_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0zFace_y_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do

       ! B0*Face_z
       do k=0,nK+2; do j=0,nJ+1; do i=0,nI+1
          iData = iData+1
          B0xFace_z_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0yFace_z_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          iData = iData+1
          B0zFace_z_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do

       ! fbody*
       if(UseGravity.or.UseRotatingFrame)then
          do k=1,nK; do j=1,nJ; do i=1,nI
             iData = iData+1
             fBody_x_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
             iData = iData+1
             fBody_y_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
             iData = iData+1
             fBody_z_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          end do; end do; end do
       else
          fbody_x_BLK(:,:,:,iBlockTo) = 0.0
          fbody_y_BLK(:,:,:,iBlockTo) = 0.0
          fbody_z_BLK(:,:,:,iBlockTo) = 0.0
       end if

       ! heating
       if(UseUserHeating)then
          do k=1,nK; do j=1,nJ; do i=1,nI
             iData = iData+1
             qHeat_BLK(i,j,k,iBlockTo) = BlockData_I(iData)
          end do; end do; end do
       else
          qheat_BLK(:,:,:,iBlockTo) = 0.0
       end if
    end if ! DoMoveExtraData

    if(UseBDF2 .and. n_prev > 0)then            !^CFG IF IMPLICIT BEGIN
       do iw=1,nw; do k=1,nK; do j=1,nJ; do i=1,nI
          iData = iData+1
          w_prev(i,j,k,iw,iBlockTo) = BlockData_I(iData)
       end do; end do; end do; end do
    end if                                      !^CFG END IMPLICIT

    if(UseIM)then                               !^CFG IF RCM BEGIN
       do k=1,nK; do j=1,nJ; do i=1,nI; do i2=1,2; do i1=1,3
          iData = iData+1
          ray(i1,i2,i,j,k,iBlockTo) = BlockData_I(iData)
       end do; end do; end do; end do; end do
    end if                                      !^CFG END RCM

  end subroutine recv_block_data
  !============================================================================
end subroutine move_block

!^CFG IF IMPLICIT BEGIN
!=============================================================================
subroutine select_stepping(DoPartSelect)
  ! Set logical arrays for implicit blocks, 
  ! set number of implicit and explicit blocks,
  ! and if DoPartSelect is true then select explicit and implicit blocks
  ! based on the stepping selection criteria.

  use ModProcMH
  use ModMain
  use ModAdvance,  ONLY : iTypeAdvance_B, iTypeAdvance_BP, &
       SkippedBlock_, ExplBlock_, ImplBlock_
  use ModGeometry, ONLY : Rmin_BLK
  use ModImplicit, ONLY : UseFullImplicit,UsePartImplicit, &
       ImplCritType, ExplCFL,rImplicit
  use ModMpi
  implicit none

  logical, intent(in) :: DoPartSelect

  integer :: nBlockExpl, nBlockImpl
  integer :: iError
  logical :: oktest, oktest_me
  !---------------------------------------------------------------------------
  call set_oktest('select_stepping',oktest,oktest_me)

  if(oktest)then
     write(*,*) 'select_stepping starting with iProc ',&
          'UseFullImplicit, UsePartImplicit, UsePartLocal, DoPartSelect=',&
          iProc, UseFullImplicit, UsePartImplicit, UsePartLocal, DoPartSelect
     write(*,*)'select_stepping starting with ',&
          'iProc,nBlockExpl, nBlockImpl, nBlockSkipped=', iProc, &
          count(iTypeAdvance_B == ExplBlock_),&
          count(iTypeAdvance_B == ImplBlock_),&
          count(iTypeAdvance_B == SkippedBlock_)
     write(*,*)'select_stepping starting with ',&
          'iProc,nBlockExplALL, nBlockImplALL, nBlockSkippedALL=', iProc, &
          count(iTypeAdvance_BP == ExplBlock_),&
          count(iTypeAdvance_BP == ImplBlock_),&
          count(iTypeAdvance_BP == SkippedBlock_)
     write(*,*)'select_stepping starting with ',&
          'iProc,min(advance),max(advance)=',iProc, &
          minval(iTypeAdvance_B),maxval(iTypeAdvance_B)
     write(*,*)'select_stepping starting with ',&
          'iProc,min(advanceall),max(advanceall)=',iProc, &
          minval(iTypeAdvance_BP),maxval(iTypeAdvance_BP)
  end if

  if(((UsePartLocal .or. UsePartImplicit) .and. .not. DoPartSelect) &
       .or. .not. (UseImplicit .or. UsePartLocal))then
     nBlockExplALL    = nBlockALL
     nBlockImplALL    = 0
     where(iTypeAdvance_BP(1:nBlockMax,:) == ImplBlock_) &
          iTypeAdvance_BP(1:nBlockMax,:) = ExplBlock_
     iTypeAdvance_B(1:nBlockMax) = iTypeAdvance_BP(1:nBlockMax,iProc)
          
  elseif(UseFullImplicit)then
     nBlockExplALL = 0
     nBlockImplALL = nBlockALL
     where(iTypeAdvance_BP(1:nBlockMax,:) /= SkippedBlock_) &
          iTypeAdvance_BP(1:nBlockMax,:) = ImplBlock_
     iTypeAdvance_B(1:nBlockMax) = iTypeAdvance_BP(1:nBlockMax,iProc)
  else

     if(iProc==0.and.lVerbose>0)&
          write(*,*)'select_stepping: ImplCritType=',ImplCritType

     ! First set all blocks to be explicit
     where(iTypeAdvance_B(1:nBlockMax) /= SkippedBlock_) &
          iTypeAdvance_B(1:nBlockMax) = ExplBlock_

     ! Select implicitly treated blocks
     select case(ImplCritType)
     case('dt')
        ! Just checking
        if(.not.time_accurate)call stop_mpi(&
             'ImplCritType=dt is only valid in time_accurate mode')

        ! Set implicitBLK based on the time step.
        do globalBLK=1,nBlockMax
           if(unusedBLK(globalBLK)) CYCLE

           ! Obtain the time step based on CFL condition

           ! For first iteration calculate dt_BLK when inside time loop,
           ! otherwise use the available dt_BLK from previous time step,
           ! or from the restart file, or simply 0 set in read_inputs.
           ! The latter two choices will be overruled later anyways.
           if(iteration_number==0 .and. time_loop)then
              ! For first iteration in the time loop
              ! calculate stable time step
              call calc_facevalues(.false.)
              call calc_facefluxes(.false.)
              call calc_timestep
           end if

           ! If the smallest allowed timestep is below the fixed DtFixed
           ! then only implicit scheme will work
           if(dt_BLK(globalBLK)*explCFL <= DtFixed) &
                iTypeAdvance_B(globalBLK) = ImplBlock_
        end do

        if(oktest_me)write(*,*)&
             'SELECT: advancetype,dt_BLK,explCFL,dt=',&
             iTypeAdvance_B(BLKtest),dt_BLK(BLKtest),explCFL,dt

     case('r','R')
        ! implicitly treated blocks are within rImplicit and not Unused
        where(rMin_BLK(1:nBlockMax) <= rImplicit .and. &
             .not.UnusedBLK(1:nBlockMax)) &
             iTypeAdvance_B(1:nBlockMax) = ImplBlock_
     case('test')
        if(iProc==PROCtest) iTypeAdvance_B(BLKtest) = ImplBlock_
     end select

     ! Gather global information
     call MPI_ALLGATHER(iTypeAdvance_B, MaxBlock, MPI_INTEGER, &
          iTypeAdvance_BP, MaxBlock, MPI_INTEGER, iComm, iError)

     nBlockImplALL = count(iTypeAdvance_BP == ImplBlock_)
     nBlockExplALL = count(iTypeAdvance_BP == ExplBlock_)

     if(iProc==0.and.lVerbose>0)oktest_me=.true. ! report for part implicit
  end if
  if(oktest)then
     write(*,*)'select_stepping finished with ',&
          'iProc,nBlockExpl, nBlockImpl, nBlockSkipped=', iProc, &
          count(iTypeAdvance_B == ExplBlock_),&
          count(iTypeAdvance_B == ImplBlock_),&
          count(iTypeAdvance_B == SkippedBlock_)
     write(*,*)'select_stepping finished with ',&
          'iProc,nBlockExplALL, nBlockImplALL, nBlockSkippedALL=', iProc, &
          count(iTypeAdvance_BP == ExplBlock_),&
          count(iTypeAdvance_BP == ImplBlock_),&
          count(iTypeAdvance_BP == SkippedBlock_)
     write(*,*)'select_stepping finished with ',&
          'iProc,min(advance),max(advance)=',iProc, &
          minval(iTypeAdvance_B),maxval(iTypeAdvance_B)
     write(*,*)'select_stepping finished with ',&
          'iProc,min(advanceall),max(advanceall)=',iProc, &
          minval(iTypeAdvance_BP),maxval(iTypeAdvance_BP)
  end if
end subroutine select_stepping
!^CFG END IMPLICIT
