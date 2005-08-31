!^CMP COPYRIGHT UM
!BOP
!MODULE: CON_variables - The most basic variables for CON
!INTERFACE:
module CON_variables
  !USES:
  use ModReadParam, ONLY: lStringLine

  implicit none

  !DESCRIPTION:
  ! This module contains a number of public variables used by CON
  !EOP
  !BOC
  ! Version number for SWMF
  real, parameter :: VersionSwmf = 2.1

  ! Logical to decide if the SWMF running stand alone or as part of something
  logical :: IsStandAlone = .true.

  ! Error code for the SWMF when it runs as part of a larger application
  integer :: iErrorSwmf = 0

  ! Description string for the problem being solved
  character (len=lStringLine) :: StringDescription='Please describe me!'

  ! Number of bytes in the default real number (precision)
  ! nByteReal = 4 if range(1.0) is smaller than range(1.D0), otherwise 8
  ! This is standard F90 initialization expression but may give warnings:
  integer, parameter :: nByteReal = 4 + (1.00000000041 - 1.0)*10000000000.0

  ! Shall we use timing module
  logical :: UseTiming = .true.
  integer :: DnTiming  = -2       ! Show timing at the end of run only
  
  ! How verbose should we be
  integer :: lVerbose = 1
  
  ! How strict shoule we be. 
  ! If true stop with error if false write warning but try to correct problem.
  logical :: UseStrict=.true.

  ! Names of subroutines to be tested
  character (len=lStringLine) :: StringTest = ' '

  ! Time to start testing
  real    :: tStartTest     = -1.0
  integer :: nIterStartTest = -1
  integer :: iProcTest      = 0

  !EOC
end module CON_variables
