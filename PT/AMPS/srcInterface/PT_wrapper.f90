!  Copyright (C) 2002 Regents of the University of Michigan, portions used with permission 
!  For more information, see http://csem.engin.umich.edu/tools/swmf
!  Wrapper for the empty PTOM (PT) component
!  $Id$
!==========================================================================
subroutine PT_set_param(CompInfo, TypeAction)

  use CON_comp_info
  use ModReadParam
  implicit none

  character (len=*), parameter :: NameSub='PT_set_param'
  integer :: iComm,iProc,nProc

  ! Arguments
  type(CompInfoType), intent(inout) :: CompInfo   ! Information for this comp.
  character (len=*), intent(in)     :: TypeAction ! What to do
  character(len=lStringLine), allocatable :: StringLineF_I(:) ! Contains the PARAM.in segment
  !-------------------------------------------------------------------------

  !call AMPS_TimeStep()


  select case(TypeAction)
  case('VERSION')
     call put(CompInfo,&
          Use        =.true., &
          NameVersion='AMPS', &
          Version    =1.0)

  case('MPI')
     call get(CompInfo, iComm=iComm, iProc=iProc, nProc=nProc)
     call AMPS_SetMpiCommunicator(iComm, iProc, nProc)

  case('READ', 'CHECK')
     ! get section of PARAM.in that contains the PT module
     allocate(StringLineF_I(i_line_read()+1:n_line_read()))
     call read_text(StringLineF_I)
     if(n_line_read()-i_line_read()+1 > 0) then
        call amps_read_param(StringLineF_I,n_line_read()-i_line_read(), lStringLine,iProc)
     end if
  case('STDOUT')
     ! call AMPS_set_stdout

  case('FILEOUT')
     ! call AMPS_set_fileout

  case('GRID')
     ! Grid info depends on BATSRUS

  case default
     call CON_stop(NameSub//': PT_ERROR: empty version cannot be used!')
  end select

end subroutine PT_set_param

!==============================================================================

subroutine PT_init_session(iSession, TimeSimulation)

  implicit none

  !INPUT PARAMETERS:
  integer,  intent(in) :: iSession         ! session number (starting from 1)
  real,     intent(in) :: TimeSimulation   ! seconds from start time

  character(len=*), parameter :: NameSub='PT_init_session'

! call AMPS_init(iSession, TimeSimulation)

end subroutine PT_init_session

!==============================================================================

subroutine PT_finalize(TimeSimulation)

  implicit none

  !INPUT PARAMETERS:
  real,     intent(in) :: TimeSimulation   ! seconds from start time

  character(len=*), parameter :: NameSub='PT_finalize'

  ! call AMPS_finalize

end subroutine PT_finalize

!==============================================================================

subroutine PT_save_restart(TimeSimulation)

  implicit none

  !INPUT PARAMETERS:
  real,     intent(in) :: TimeSimulation   ! seconds from start time

  character(len=*), parameter :: NameSub='PT_save_restart'

end subroutine PT_save_restart

!==============================================================================

subroutine PT_run(TimeSimulation,TimeSimulationLimit)

  implicit none

  !INPUT/OUTPUT ARGUMENTS:
  real, intent(inout) :: TimeSimulation   ! current time of component

  !INPUT ARGUMENTS:
  real, intent(in) :: TimeSimulationLimit ! simulation time not to be exceeded

  character(len=*), parameter :: NameSub='PT_run'

! call AMPS_run(TimeSimulation, TimeSimulationLimit)
call AMPS_TimeStep(TimeSimulation, TimeSimulationLimit) 

end subroutine PT_run

!==============================================================================
subroutine PT_get_grid_info(nDimOut, iGridOut, iDecompOut)

  ! Provide information about AMPS grid

  implicit none

  integer, intent(out):: nDimOut    ! grid dimensionality
  integer, intent(out):: iGridOut   ! grid index (increases with AMR)
  integer, intent(out):: iDecompOut ! decomposition index
  
  character(len=*), parameter :: NameSub = 'PT_get_grid_info'
  !---------------------------------------------------------------------------
  nDimOut    = 3
  iGridOut   = 1
  iDecompOut = 1
  
  call amps_mesh_id(iDecompOut)

end subroutine PT_get_grid_info
!==============================================================================
subroutine PT_put_from_gm( &
     NameVar, nVar, nPoint, Pos_DI, Data_VI, iPoint_I)

  use CON_coupler, ONLY: i_proc, PT_, n_proc

  implicit none

!  logical,          intent(in)   :: UseData ! true when data is transferred
                                            ! false if positions are asked
  character(len=*), intent(inout):: NameVar ! List of variables
  integer,          intent(inout):: nVar    ! Number of variables in Data_VI
  integer,          intent(inout):: nPoint  ! Number of points in Pos_DI
  real, pointer:: Pos_DI(:,:)               ! Position vectors

  real,    intent(in), optional:: Data_VI(:,:)    ! Recv data array
  integer, intent(in), optional:: iPoint_I(nPoint)! Order of data

  integer:: iPoint, i, iProc, nProc

  character(len=*), parameter :: NameSub='PT_put_from_gm'
!  integer,parameter::datafile=2
!  character(len=255)::datafilename
!  integer,parameter::pointsfile=1
!  character(len=255)::pointsfilename
  !--------------------------------------------------------------------------
  iProc = i_proc(PT_)
  nProc = n_proc(PT_)

  if(.not. present(Data_VI))then
     ! Set variable names
 !    NameVar = 'rho ux uy uz bx by bz p'
     ! Set number of variables needed
 !    nVar = 8

     ! set number of grid points on this processor
     call amps_get_center_point_number(nPoint)  !kkkkjjj10
     ! allocate array
     allocate(Pos_DI(3,nPoint))

!     write(pointsfilename,*) "points",iProc,".txt"
!     open(unit=pointsfile,file=trim(adjustl(pointsfilename)), &
!     action="write",status="replace")

     ! Fill in values
!     write(pointsfile,*)NameSub,': iProc, iPoint, Pos'        
!     do iPoint = 1, nPoint
!        Pos_DI(:,iPoint) = (/ 100.0*(iProc*nPoint+iPoint)/nPoint/nProc-25, &
!             100.0*(iProc*nPoint+iPoint)/nPoint/nProc-50,
!             -100.0*(iProc*nPoint+iPoint)/nPoint/nProc+75/)
!        write(pointsfile,*)NameSub, iProc, iPoint, Pos_DI(:,iPoint)
!     end do

!     close(pointsfile)

     call amps_get_center_point_coordinates(Pos_DI) 

     RETURN
  end if

!  write(datafilename,*) "data",iProc,".txt"
!  open(unit=datafile,file=trim(adjustl(datafilename)),&
!      action="write",status="replace")

!  write(*,*) NameSub,': nVar=',nVar

!  write(*,*) NameSub,': Data_VI=',Data_VI(2:5,1)

!  write(datafile,*)NameSub,': iProc, iPoint, i, Data'
!  do iPoint = 1, nPoint
!     i = iPoint_I(iPoint)
!     write(datafile,*)NameSub, iProc, iPoint, i, Data_VI(:,i)
     ! Here should convert from SI to AMPS units and store data
!  end do

!  close(datafile)

  call amps_recieve_gm2amps_center_point_data(NameVar//char(0),nVar, &
       Data_VI,iPoint_I)

end subroutine PT_put_from_gm

