module IE_ModMain

  implicit none
  save

  integer, parameter :: iono_init=1, iono_fac=2, iono_read=3, iono_save=4, &
       iono_save_restart=5, iono_solve=6

  !\
  ! Ionospheric Model Parameters
  !/
  logical :: UseFakeRegion2=.false.
  integer :: conductance_model
  logical :: UseFullCurrent
  real    :: f107_flux, PolarCapPedConductance, StarLightPedConductance
  real    :: Hall_to_Ped_Ratio

  !\
  ! Time variables obtained from CON_physics
  !/
  logical               :: time_accurate
  integer, dimension(7) :: Time_Array
  real                  :: Time_Simulation

  !\
  ! Counter for number of solves (like nStep in GM)
  !/
  integer               :: nSolve = 0

  !\
  ! Logical which tells if there is any new information to use
  !/
  logical               :: IsNewInput = .false.

  !\
  ! Character string selecting the potential sent to the IM module
  ! Possible values are 'north', 'south', 'average', 'cpcpmin'
  !/
  character (len=7) :: TypeImCouple = 'north   '

  !\
  ! Logical for coupling the UA current
  !/
  logical :: DoCoupleUaCurrent = .false.

  !\
  ! Dipole parameters obtained from CON_physics
  !/
  real :: ThetaTilt, SinThetaTilt, CosThetaTilt

  integer                           :: iDebugLevel = 0

end module IE_ModMain
