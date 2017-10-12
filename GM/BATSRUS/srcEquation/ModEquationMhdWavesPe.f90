!  Copyright (C) 2002 Regents of the University of Michigan
!  portions used with permission 
!  For more information, see http://csem.engin.umich.edu/tools/swmf
module ModVarIndexes

  use ModSingleFluid
  use ModExtraVariables, &
       Redefine1 => nWave, &
       Redefine2 => WaveFirst_, &
       Redefine3 => WaveLast_, &
       Redefine4 => Erad_, &
       Redefine5 => Pe_, &
       Redefine6 => Ehot_

  implicit none

  save

  ! This equation module contains the standard MHD equations with wave energy
  character (len=*), parameter :: &
       NameEquation='MHD + Alfven waves + electron pressure'

  ! loop variable for implied do-loop over spectrum
  integer, private :: iWave

  ! Number of wave bins in spectrum
  integer, parameter :: nWave = 2
  integer, parameter :: nVar = 10 + nWave

  ! Named indexes for State_VGB and other variables
  ! These indexes should go subsequently, from 1 to nVar+1.
  ! The energy is handled as an extra variable, so that we can use
  ! both conservative and non-conservative scheme and switch between them.
  integer, parameter :: &
       Rho_       = 1,                  &
       RhoUx_     = 2,                  &
       RhoUy_     = 3,                  &
       RhoUz_     = 4,                  &
       Bx_        = 5,                  &
       By_        = 6,                  &
       Bz_        = 7,                  &
       Ehot_      = 8,                  &
       WaveFirst_ = 9,                  &
       WaveLast_  = WaveFirst_+nWave-1, &
       Pe_        = nVar-1,             &
       p_         = nVar,               &
       Energy_    = nVar+1

  ! This is for backward compatibility with single group radiation
  integer, parameter :: Erad_ = WaveFirst_

  ! This allows to calculate RhoUx_ as rhoU_+x_ and so on.
  integer, parameter :: RhoU_ = RhoUx_-1, B_ = Bx_-1

  ! These arrays are useful for multifluid
  integer, parameter :: iRho_I(nFluid)   = (/Rho_/)
  integer, parameter :: iRhoUx_I(nFluid) = (/RhoUx_/)
  integer, parameter :: iRhoUy_I(nFluid) = (/RhoUy_/)
  integer, parameter :: iRhoUz_I(nFluid) = (/RhoUz_/)
  integer, parameter :: iP_I(nFluid)     = (/p_/)

  ! The default values for the state variables:
  ! Variables which are physically positive should be set to 1,
  ! variables that can be positive or negative should be set to 0:
  real, parameter :: DefaultState_V(nVar+1) = (/ & 
       1.0, & ! Rho_
       0.0, & ! RhoUx_
       0.0, & ! RhoUy_
       0.0, & ! RhoUz_
       0.0, & ! Bx_
       0.0, & ! By_
       0.0, & ! Bz_
       0.0, & ! Ehot_
       (1.0, iWave=WaveFirst_,WaveLast_), &
       1.0, & ! Pe_
       1.0, & ! p_
       1.0 /) ! Energy_

  ! The names of the variables used in i/o
  character(len=4) :: NameVar_V(nVar+1) = (/ &
       'Rho ', & ! Rho_
       'Mx  ', & ! RhoUx_
       'My  ', & ! RhoUy_
       'Mz  ', & ! RhoUz_
       'Bx  ', & ! Bx_
       'By  ', & ! By_
       'Bz  ', & ! Bz_
       'Ehot', & ! Ehot_
       ('I?? ', iWave=WaveFirst_,WaveLast_), &
       'Pe  ', & ! Pe_
       'p   ', & ! p_
       'e   ' /) ! Energy_


  ! Primitive variable names
  integer, parameter :: U_ = RhoU_, Ux_ = RhoUx_, Uy_ = RhoUy_, Uz_ = RhoUz_

  ! There are no extra scalars
  integer, parameter :: ScalarFirst_ = 2, ScalarLast_ = 1


end module ModVarIndexes


