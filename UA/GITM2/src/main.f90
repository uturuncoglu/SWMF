
program GITM

  use ModInputs
  use ModTime
  use ModGITM

  implicit none

  integer :: iBlock

  ! ------------------------------------------------------------------------
  ! initialize stuff
  ! ------------------------------------------------------------------------

  call init_mpi
  call start_timing("GITM")
  call delete_stop

  call init_planet
  call set_defaults

  call read_inputs(cInputFile)
  call set_inputs

  call initialize_gitm(CurrentTime)

  call write_output

  call report("Starting Main Time Loop",0)

  ! ------------------------------------------------------------------------
  ! Run for a few iterations
  ! ------------------------------------------------------------------------

  do while (CurrentTime < EndTime)

     call calc_pressure

     !!! We may have to split cMax and Dt calculation!!!
     Dt = 1.e32

     call calc_timestep_vertical
     if (.not. Is1D) call calc_timestep_horizontal

     call advance

     if (.not.IsFramework) call check_stop

     iStep = iStep + 1

     call write_output

  enddo

  ! ------------------------------------------------------------------------
  ! Finish run
  ! ------------------------------------------------------------------------

  call finalize_gitm

!!!  contains
!!!  
!!!    subroutine write_output
!!!  
!!!      real, external :: get_timing
!!!      integer :: i, nMLTsTmp,nLatsTmp
!!!      logical :: IsDone
!!!  
!!!      if (floor((tSimulation-dt)/DtReport) /= &
!!!           floor((tsimulation)/DtReport) .and. iDebugLevel >= 0) then
!!!         write(*,"(a,i6,a,3i2.2,a,f10.2,a)") "iStep ", iStep, &
!!!              ", Time : ",iTimeArray(4:6), &
!!!              ", RealTime : ",get_timing("GITM")/60.0," min"
!!!      endif
!!!  
!!!      if (floor((tSimulation-dt)/DtLogFile) /= &
!!!           floor((tsimulation)/DtLogFile)) then
!!!         call logfile("UA/data")
!!!      endif
!!!  
!!!      IsDone = .false.
!!!      do i = 1, nOutputTypes
!!!         if (floor((tSimulation-dt)/DtPlot(i)) /= &
!!!              floor((tsimulation)/DtPlot(i)) .or. tSimulation == 0.0) then
!!!            if (.not. IsDone .and. .not. Is1D) then
!!!               call UA_calc_electrodynamics(nMLTsTmp, nLatsTmp)
!!!               IsDone = .true.
!!!            endif
!!!            do iBlock = 1, nBlocks
!!!               call output("UA/data/",iBlock, i)
!!!            enddo
!!!         endif
!!!      enddo
!!!  
!!!      call move_satellites
!!!  
!!!      if (floor((tSimulation-dt)/DtRestart) /= &
!!!           floor((tsimulation)/DtRestart)) then
!!!         call write_restart("UA/restartOUT/")
!!!      endif
!!!  
!!!    end subroutine write_output

end program GITM

!============================================================================
! The following subroutines are here so that we can use SWMF library routines
! Also some features available in SWMF mode only require empty subroutines
! for compilation of the stand alone code.
!============================================================================

subroutine CON_stop(StringError)

  implicit none
  character (len=*), intent(in) :: StringError
  call stop_gitm(StringError)

end subroutine CON_stop


subroutine CON_io_unit_new(iUnit)

  implicit none
  integer, intent(in) :: iUnit

  return

end subroutine CON_io_unit_new

!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

subroutine merge_str(str1, str2)

  character (len=100) :: str1, str2, temp
  integer :: i, j, k

  i = 1
  do while (iachar(str1(i:i)) /= 32 .and. &
            iachar(str1(i:i)) /= 9  .and. &
            i < 100) 
     i=i+1
  enddo

  j = 1
  do while (iachar(str2(j:j)) /= 32 .and. &
            iachar(str2(j:j)) /= 9  .and. &
            j < 100) 
     j=j+1
  enddo

  temp = str1
  do k = i,100
     temp(k:k) = ' '
  enddo

  if (i+j-1 > 100) j = 100 - i + 1

  temp(i:i+j-1) = str2(1:j)

  str2 = temp

end subroutine merge_str

