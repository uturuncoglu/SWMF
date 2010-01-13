!^CFG COPYRIGHT UM
module ModCovariant
  use ModSize
  use ModNumConst
  use ModMain,ONLY:unusedBLK
  use ModCoordTransform
  use ModParallel,ONLY:BLKneighborLEV,NOBLK
  use ModUtilities,ONLY: check_allocate
  implicit none
  save

  real,dimension(nBLK) :: &
       FaceArea2MinI_B, FaceArea2MinJ_B, FaceArea2MinK_B

  logical::UseCovariant=.false.


  !For a vertex-based logically cartesian (spherical, cylindircal) grid 
  !(UseVertexBasedGrid=.true.) the node coordinates are defined
  !in terms of an arbitrary pointwide transformation of nodes of an 
  !original cartesian (spherical,cylindrical) block adaptive grid.
  !Advantage: the possiblity to use the arbitrary transformation.
  !Disadvantages: the cell center coordinates can not be definied unambigously
  !and the difference of the state variables across the face does not evaluate
  !the gradient in the direction, normal to this face (stricly speaking).
  !Cell-centered grids are used if UseVertexBasedGrid=.false. (default value)
  !Advantage: for some particular geometries (spherical, cylindrical) the 
  !control volumes are the Voronoy cells (any face is perpendicular to the line
  !connecting the centers of the neighboring cells). 
  !Disadvantages: even in these particular cases it is not easy to properly define 
  !the face area vectors at the resolution change. More general cell-centered grid 
  !either is not logically cartesian, or does not consist of the Voronoy cells only.
  !
  logical :: UseVertexBasedGrid=.true.
  character (len=20) ::TypeGeometry='cartesian'  
  character (len=100) ::NameGridFile='' ! used for spherical_genr geometry                    
  real,allocatable,dimension(:,:,:,:,:):: &            
       FaceAreaI_DFB,FaceAreaJ_DFB,FaceAreaK_DFB
  integer,allocatable,dimension(:,:,:,:)::OldLevel_IIIB
  logical,dimension(-1:1,-1:1,-1:1)::IsNotCorner_III

  !Parameters of 
  real::rTorusLarge=6.0,rTorusSmall=0.50
  integer,parameter::nToroidalBoundaryPoints=1024
  !
  real,dimension(0:nToroidalBoundaryPoints)::TorusSurface_I
  logical::IsInitializedTorusGeometry=.false.

  !-- ADDED FOR general r grid in spherical geometry!
  !-- Main Idea is to have a tabulated function that maps
  !-- a general coordinate to log(r). This way, r resolution can 
  !-- arbitrarily defined. Gen coord is taken to be 0 to 1.0
  !-- but will linearly extrapolate outside of this domain
  integer :: nGrid
  real :: DeltaGen 
  real, pointer, dimension(:) :: yR_I

contains
  subroutine allocate_face_area_vectors
    integer::iError
    if(allocated(FaceAreaI_DFB))return
    allocate(FaceAreaI_DFB(nDim,1:nI+1,1:nJ,1:nK,nBLK),stat=iError)
    call check_allocate(iError,'Face Areas are not allocated')
    allocate(FaceAreaJ_DFB(nDim,1:nI,1:nJ+1,1:nK,nBLK),stat=iError)
    call check_allocate(iError,'Face Areas are not allocated')
    allocate(FaceAreaK_DFB(nDim,1:nI,1:nJ,1:nK+1,nBLK),stat=iError)
    call check_allocate(iError,'Face Areas are not allocated')
    FaceAreaI_DFB=cOne; FaceAreaJ_DFB=cOne; FaceAreaK_DFB=cOne
  end subroutine allocate_face_area_vectors
  !---------------------------------------------------------------------------------
  subroutine allocate_old_levels
    if(allocated(OldLevel_IIIB))return
    allocate(OldLevel_IIIB(-1:1,-1:1,-1:1,nBLK))
    OldLevel_IIIB=NOBLK
    IsNotCorner_III=.true.
    IsNotCorner_III(-1,-1,-1)=.false.
    IsNotCorner_III(+1,-1,-1)=.false.
    IsNotCorner_III(-1,+1,-1)=.false.
    IsNotCorner_III(+1,+1,-1)=.false.
    IsNotCorner_III(-1,-1,+1)=.false.
    IsNotCorner_III(+1,-1,+1)=.false.
    IsNotCorner_III(-1,+1,+1)=.false.
    IsNotCorner_III(+1,+1,+1)=.false.
  end subroutine allocate_old_levels
  !---------------------------------------------------------------------------------
  logical function do_fix_geometry_at_reschange(iBlock)
    integer,intent(in)::iBlock
    if(unusedBLK(iBlock).or.(.not.UseCovariant).or.(.not.UseVertexBasedGrid))then
       do_fix_geometry_at_reschange=.false.
       return
    end if
    do_fix_geometry_at_reschange=any(IsNotCorner_III(:,:,:).and.&
         OldLevel_IIIB(:,:,:,iBlock)/=BLKneighborLEV(:,:,:,iBlock).and.&
         (OldLevel_IIIB(:,:,:,iBlock)==-1.or.BLKneighborLEV(:,:,:,iBlock)==-1))
  end function do_fix_geometry_at_reschange

  !---------------------------------------------------------------------------------
  subroutine get_face_area_i(&
       XyzNodes_DIII,&                      !(in) Cartesian coordinates of nodes
       iStart,iMax,jStart,jMax,kStart,kMax,&!(in) FACE indexes. 
       FaceAreaI_DF)                        !(out)Face area vectors
    !-----------------------------------------------------------------------------
    integer,intent(in)::iStart,iMax,jStart,jMax,kStart,kMax
    real,intent(in),dimension(nDim,iStart:iMax,jStart:jMax+1,kStart:kMax+1)::&
         XyzNodes_DIII
    real,intent(out),dimension(nDim,iStart:iMax,jStart:jMax,kStart:kMax)::&
         FaceAreaI_DF
    !-----------------------------------------------------------------------------
    integer::i,j,k
    do k=kStart,kMax; do j=jStart,jMax; do i=iStart,iMax
       FaceAreaI_DF(:,i,j,k)=cHalf*&
            cross_product(XyzNodes_DIII(:,i,j+1,k+1)-XyzNodes_DIII(:,i,j  ,k),&
            XyzNodes_DIII(:,i,j  ,k+1)-XyzNodes_DIII(:,i,j+1,k))
    end do; end do; end do
  end subroutine get_face_area_i
  !---------------------------------------------------------------------------------
  subroutine get_face_area_j(&
       XyzNodes_DIII,&                      !(in) Cartesian coordinates of nodes
       iStart,iMax,jStart,jMax,kStart,kMax,&!(in) FACE indexes. 
       FaceAreaJ_DF)                        !(out)Face area vectors
    !-----------------------------------------------------------------------------
    integer,intent(in)::iStart,iMax,jStart,jMax,kStart,kMax
    real,intent(in),dimension(nDim,iStart:iMax+1,jStart:jMax,kStart:kMax+1)::&
         XyzNodes_DIII
    real,intent(out),dimension(nDim,iStart:iMax,jStart:jMax,kStart:kMax)::&
         FaceAreaJ_DF
    !-----------------------------------------------------------------------------
    integer::i,j,k
    do k=kStart,kMax; do j=jStart,jMax; do i=iStart,iMax
       FaceAreaJ_DF(:,i,j,k)=cHalf*&
            cross_product(XyzNodes_DIII(:,i+1,j,k+1)-XyzNodes_DIII(:,i,j  ,k),&
            XyzNodes_DIII(:,i+1,j  ,k)-XyzNodes_DIII(:,i,j,k+1))
    end do; end do; end do
  end subroutine get_face_area_j
  !---------------------------------------------------------------------------------
  subroutine get_face_area_k(&
       XyzNodes_DIII,&                      !(in) Cartesian coordinates of nodes
       iStart,iMax,jStart,jMax,kStart,kMax,&!(in) FACE indexes. 
       FaceAreaK_DF)                        !(out)Face area vectors
    !-----------------------------------------------------------------------------
    integer,intent(in)::iStart,iMax,jStart,jMax,kStart,kMax
    real,intent(in),dimension(nDim,iStart:iMax+1,jStart:jMax+1,kStart:kMax)::&
         XyzNodes_DIII
    real,intent(out),dimension(nDim,iStart:iMax,jStart:jMax,kStart:kMax)::&
         FaceAreaK_DF
    !-----------------------------------------------------------------------------
    integer::i,j,k
    do k=kStart,kMax; do j=jStart,jMax; do i=iStart,iMax
       FaceAreaK_DF(:,i,j,k)=cHalf*&
            cross_product(XyzNodes_DIII(:,i+1,j+1,k)-XyzNodes_DIII(:,i,j  ,k),&
            XyzNodes_DIII(:,i,j+1  ,k)-XyzNodes_DIII(:,i+1,j,k))
    end do; end do; end do
  end subroutine get_face_area_k
  !-------------------------------------------------------------!
  logical function is_axial_geometry()
    is_axial_geometry=index(TypeGeometry,'spherical')  >0.or.&
         index(TypeGeometry,'cylindrical')>0.or.&
         index(TypeGeometry,'axial')>0 
  end function is_axial_geometry

  !-------------------------------------------------------------!
  real function gen_to_r(Gen)
    real,intent(in) :: Gen
    
    real :: LogR   ! Logarithm of R

    ! Weight coefficient to be used in interpolation
    real :: Weight 

    ! While Gen passes the span from 0.0 to 1.0,
    ! RealIndex passes the values from 0.0 to nGrid -1

    real :: RealIndex

    !i passes the values from 1 to nGrid 
    integer :: i
    !----------------------------


    RealIndex = Gen / DeltaGen

    i = floor(1.0 + RealIndex)

    !--- if less than minimum, use linear slope of first 2 pnts
    if (i <=  1) then 

       LogR = RealIndex * (yR_I(2) - yR_I(1))  + yR_I(1)

    !--- if more than maximum, use linear slope of last 2 pnts
    elseif (i >= nGrid ) then 
        LogR = (RealIndex - (nGrid -1)) * (yR_I(nGrid) - yR_I(nGrid-1)) &
              + yR_I(nGrid)
    else
       
       Weight = RealIndex - (i -1)
       
       LogR = (cOne - Weight) * yR_I(i) + Weight * yR_I(i+1)
    end if

    gen_to_r = exp(LogR)
  end function gen_to_r
 !-------------------------------------------------------------!
  real function r_to_gen(R)
    !Input parameters:
    real,intent(in) :: R !The value of radial coordinate
    
    real :: LogR   ! Logarithm of R

    ! Weight coefficient to be used in interpolation
    real :: Weight 

    ! While Gen passes the span from 0.0 to 1.0,

    !i passes the values from 1 to nGrid 
    integer :: i
    !----------------------------

    if(R <= 0.0) call stop_mpi('Negative R in r_to_gen')
    LogR = log(R)
    
    !--- if less than minimum, use linear slope of 2st pnts
    if (LogR <= yR_I(1)) then 

       r_to_gen = DeltaGen / (yR_I(2) - yR_I(1)) * (LogR - yR_I(1))

    elseif (LogR >= yR_I(nGrid)) then 
       r_to_gen = DeltaGen / (yR_I(nGrid) - yR_I(nGrid-1)) * (LogR - yR_I(ngrid))&
                  + 1.0
    else
       i = maxloc(yR_I, DIM=1, MASK=( yR_I <= LogR) )    
       Weight = (LogR - yR_I(i)) / (yR_I(i+1) - yR_I(i))

       r_to_gen = DeltaGen * (Weight + (i - 1))
    end if

  end function r_to_gen
  !-------------------------------------------------------------!
  subroutine read_grid_file(NameFile)
    use ModIoUnit, ONLY: io_unit_new
    use ModIO, ONLY: iUnitOut

    character (LEN=*), intent(in) :: NameFile

    integer :: iUnit,i,iError
    real :: ytemp  
    ! This function is for reading in the general grid function.
    ! For simplicity the files Must be in a fixed format that is read by this
    ! routine.

    ! The formate is a single column with number of points in the 
    ! first row, and the folling rows the log(r) value from gen = 0 to 1

    iUnit = io_unit_new()
    open(iUnit,file=trim(NameFile),status='old',iostat=iError)
    if(iError /= 0) call CON_stop('read_grid_file' // &
       'could not open grid file = ' // trim(NameFile))

    ! read in nGrid and then define yR_I, and DeltaGen
    read(iUnit,*,iostat=iError) nGrid

    nullify(yR_I)
    allocate(yR_I(nGrid),stat=iError) 
    yR_I = 0.0

    DeltaGen = 1.0/(nGrid-1)

    ! read in tabulated logR as a function of gen
    do i=1,nGrid
       read(iUnit,*,iostat=iError) ytemp
       yR_I(i) = ytemp
    enddo
    close(iUnit)

  end subroutine read_grid_file
  !----------------------------!
  subroutine set_fake_grid_file
    !Set the
    nullify(yR_I)
    allocate(yR_I(1))  
    yR_I = 0.0 
  end subroutine set_fake_grid_file

end module ModCovariant


