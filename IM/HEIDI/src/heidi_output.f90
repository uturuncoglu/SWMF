! File name: heidi_output.f90
!
! Contains: output routines for HEIDI
!	ECFL
!	WRESULT
!	PSRCLOSS
!
! Last Modified: March 2006, Mike Liemohn
!=========================================================================
!				ECFL
!	Checks the energy advection CFL numbers
!=========================================================================
subroutine ECFL

  use ModHeidiSize
  use ModHeidiIO
  use ModHeidiMain
  use ModHeidiDrifts
  use ModIoUnit, only : io_unit_new

  implicit none

  real             :: cfl,cmax
  real             :: acosd
  integer          :: i,j,k,l,Im,Km,Jm,Lm,ibad
  integer          :: ntc
  integer          :: iUnitOut 
  character(len=2) :: NameSpecies
  character(len=5) :: NameSuffix
  character(len=3) :: NameStep
  character(len=30):: NameOutputSpecies

  save ntc
  !--------------------------------------------------------------------------
  !Define parts of the output file names

  if (T.eq.0) then
     ntc=0
     NameStep='000'
  else
     ntc=ntc+1
     write(NameStep, '(i3.3)') ntc
  end if
  
  
11 format (I1)
12 format (I2)
13 format (I3)
  
  NameSuffix='_edr.'
  
  do S=1,NS
     if (SCALC(S).eq.1) then
        
	if (S.eq.1) NameSpecies='_e' 
	if (S.eq.2) NameSpecies='_h' 
	if (S.eq.3) NameSpecies='he' 
	if (S.eq.4) NameSpecies='_o' 
	
        if (S.eq.1) NameOutputSpecies='electron/'
	if (S.eq.2) NameOutputSpecies='hydrogen/'
	if (S.eq.3) NameOutputSpecies='helium/'
	if (S.eq.4) NameOutputSpecies='oxygen/'

        iUnitOut = io_unit_new()

        open(iUnitOut,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
             NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
	
        write (*,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep

        write (iUnitOut,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep
	write (iUnitOut,*) 'T,KP,A:',T,KP,A
	write (iUnitOut,38) 'L-shell','MLT(hr)','E(keV)','PA(deg)','n(cm-3)',   &
             'ABS CFL'
	write (iUnitOut,*) 'Energy advection |CFL| > 1 (entire grid)'
	Ibad=0
	CMAX=0.
	Im=0
	Jm=0
	Km=0
	Lm=0
	do I=1,IO
           do J=1,JO
              do K=1,KO
                 do L=1,LO	     ! UPA(I)-1 , changed to include the l.c.
                    CFL=abs(EDOT(I,J,K,L)*VR(I,J)+(COULI(I,K,L,S)+COULE(I,K,L,S))*XNE(I,J))
                    if (CFL.gt.1.) then
                       write (iUnitOut,39) LZ(I),MLT(J),EKEV(K),ACOSD(MU(L)),XNE(I,J),CFL
                       Ibad=Ibad +1
                    end if
                    if (CFL.gt.CMAX) then
                       CMAX=CFL
                       Im=I
                       Jm=J
                       Km=K
                       Lm=L
                    end if
                    if (Ibad.gt.1000) goto 50
                 end do	! L loop
              end do	! K loop
           end do	! J loop
	end do	! I loop
50	continue
	write (iUnitOut,*) 'Bad CFLs: ',Ibad
	write (iUnitOut,*) 'Max CFL: ',CMAX,Im,Jm,Km,Lm

	write (iUnitOut,*) 'Radial advection |CFL| > 1 (entire grid)'
	Ibad=0
	CMAX=0.
	do I=1,IO
           do J=1,JO
              CFL=abs(VR(I,J))
              if (CFL.gt.1.) then
                 write (iUnitOut,39) LZ(I),MLT(J),A,VR(I,J),CFL
                 Ibad=Ibad +1
              end if
              if (CFL.gt.CMAX) then
                 CMAX=CFL
                 Im=I
                 Jm=J
              end if
              if (Ibad.gt.1000) goto 51
           end do	! J loop
	end do	! I loop
51	continue
	write (iUnitOut,*) 'Bad CFLs: ',Ibad
	write (iUnitOut,*) 'Max CFL: ',CMAX,Im,Jm

	Ibad=0
	CMAX=0.
	write (iUnitOut,*) 'Azimuthal advection |CFL| > 1 (entire grid)'
	do I=1,IO
           do J=1,JO
              do K=1,KO
                 do L=1,LO	     
                    CFL=abs(P1(I,J)+P2(I,K,L))
                    if (CFL.gt.1.) then
                       write (iUnitOut,39) LZ(I),MLT(J),EKEV(K),ACOSD(MU(L)),A,P1(I,J),P2(I,K,L),CFL
                       Ibad=Ibad +1
                    end if
                    if (CFL.gt.CMAX) then
                       CMAX=CFL
                       Im=I
                       Jm=J
                       Km=K
                       Lm=L
                    end if
                    if (Ibad.gt.1000) goto 52
                 end do	! L loop
              end do	! K loop
           end do	! J loop
	end do	! I loop
52	continue
	write (iUnitOut,*) 'Bad CFLs: ',Ibad
	write (iUnitOut,*) 'Max CFL: ',CMAX,Im,Jm,Km,Lm

	write (iUnitOut,*) 'Mu advection |CFL| > 1 (entire grid)'
	Ibad=0
	CMAX=0.
	do I=1,IO
           do J=1,JO
              do L=1,LO	     
                 CFL=abs(MUDOT(I,J,L)*VR(I,J))
                 if (CFL.gt.1.) then
                    write (iUnitOut,39) LZ(I),MLT(J),ACOSD(MU(L)),A,CFL
                    Ibad=Ibad +1
                 end if
                 if (CFL.gt.CMAX) then
                    CMAX=CFL
                    Im=I
                    Jm=J
                    Lm=L
                 end if
                 if (Ibad.gt.1000) goto 53
              end do	! L loop
           end do	! J loop
	end do	! I loop
53	continue
	write (iUnitOut,*) 'Bad CFLs: ',Ibad
	write (iUnitOut,*) 'Max CFL: ',CMAX,Im,Jm,Lm

	close(iUnitOut)

     end if		! SCALC Check
  end do		! S LOOP

37 format(4(2X,A3,2X),7(2X,A7,2X))
38 format(20(2X,A7,2X))
39 format(10(1PE11.3))

  return
end subroutine ECFL
!=========================================================================
!				WRESULT
!       Routine prints all the results at time T after injection
!	IRES(1)  'psd'	F throughout magnetosphere
!	IRES(2)  'etf'	Equ. trapped F throughout magnetosphere
!	IRES(3)  'dep'	Flux tube energy deposition
!	IRES(4)  '*pf'	Total precipitation flux (3 E ranges)
!	IRES(5)  'flx'	Differential precipitation flux
!	IRES(6)  'los'	Particle and energy losses
!	IRES(7)  'pla'	Thermal plasma densities
!	IRES(8)  'cfl'	CFL numbers for advection operators
!	IRES(9)  'drf'	Drift velocities for advection
!	IRES(10) 'evl'	E vs. L distributions at given MLT and PA
!	IRES(11) 'lft'	Particle lifetimes
!	IRES(12) 'prs'	Pressures, densities, and Dst
!	IRES(13) 'fun'  Unformatted output of all F2
!	IRES(14) 'sal'	Continuous sources and losses of number/energy
!	IRES(15) 'fbc'	Nightside boundary condition distribution
!=========================================================================

subroutine WRESULT(LNC,XN,IFIR)

  use ModHeidiSize
  use ModHeidiIO
  use ModHeidiMain
  use ModHeidiDrifts
  use ModHeidiCurrents
  use ModHeidiWaves
  use ModHeidiDGCPM
  use ModIoUnit, only : io_unit_new,UNITTMP_
  
  implicit none
  
  real             :: flux,esum,csum,psum,erate,xlec,xlnc
  real             :: cfl,weight,xr2
  real             :: edr,xe,xn1,FUNI,FUNT
  real             :: acosd
  real             :: LNC(NR,NS),XN(NR,NS),XNO(NR)
  real             :: NSUM,TAUD,TAUBO,TAUCHE,EO(NR)
  real             :: AVEFL(NR,NT,NE),RFAC
  real             :: FZERO(NR,NT,NE),DEP(NR,NT),NBC(NT)
  integer          :: i,j,k,l
  integer          :: nec,nec1,nec2,il,ibad,ii
  integer          :: nlc,ifn,ir2,IFIR
  integer          :: ntc,NIC(3),IFM(38),PAV(3),EV(3)
  integer          :: iUnitOut 
  character(len=5) :: IPF(3)
  character(len=80):: filename
  character(len=2) :: NameSpecies
  character(len=3) :: NameStep
  character(len=5) :: NameSuffix
  character(len=20):: NameOutputSpecies
  integer          :: iUnitSal1,iUnitSal2,iUnitSal3,iUnitSal4



  external :: FUNI,FUNT
  save ntc
  
  data IPF/'_lpf.','_mpf.','_hpf.'/
  data IFM/2,7,13,20,28,35,42,47,50,52,54,56,58,60,62,64,66,68,70,   &
       2,11,21,31,41,51,61,70,75,79,82,83,84,85,86,87,88,89,90/
  data PAV,EV/2,20,42,5,19,31/
  !--------------------------------------------------------------------------

  !.......Define parts of the output file names
  if (IFIR.eq.1) then
     ntc=int(nint(TIME/TINT))
  else
     ntc=ntc+1
  end if
  write(NameStep, '(i3.3)') ntc 

11 format (I1)
12 format (I2)
13 format (I3)

  !.......Find output counters
  NLC=nint(real(LO-1)/10.)
  if (NLC.lt.1) NLC=1
  NEC=nint(real(KO-1)/25.)
  if (NEC.lt.1) NEC=1

  !.......Calculate bulk quantities
  call write_prefix; write(iUnitStdOut,*)  'Calling PRESSURES'
  call PRESSURES
  call write_prefix; write(iUnitStdOut,*) 'Calling CURRENTCALC'
  !	IF (IA.GE.8) CALL CURRENTCALC
  call CURRENTCALC  ! Do it all the time
  !	print *, 'WRESULT DST VALUES: ',(DST(S),S=1,NS)

  !.......L counter offset in PAD outputs
  IFN=0
  if (IPA.eq.0) IFN=19

  !.......Write the plasmaspheric thermal densities (IRES(7), 'pla' & 'dgcpm')
  if (IRES(7).eq.1 .and. me_world.eq.0) then
     call write_prefix; write(iUnitStdOut,*)  'Printing plasmasphere'
     !	  First create Dan's output file for his plotting software
     filename = NameOutputDir//trim(NameRun)//'_dgcpm_'//NameStep//'.dat'
     call getdensity(vthetacells,nthetacells,vphicells,nphicells,   &
          dendgcpm)
     !	call saveit(vthetacells,nthetacells,vphicells,nphicells,   &
     !              dendgcpm,gridx,gridy,gridoc,filename)
     call saveplasmasphere(filename)
     !	  Next create an output like we have made before
     NameSuffix='_pla.'
    
     open(UNITTMP_,file=NameOutputDir//trim(NameRun)//NameSuffix//NameStep,STATUS='UNKNOWN')
     write (UNITTMP_,*) 'Filename: '//NameOutputDir//trim(NameRun)//NameSuffix//NameStep
     write (UNITTMP_,*) 'Plasmaspheric thermal densities from the DGCPM'
     write (UNITTMP_,16) T,KP
     write (UNITTMP_,31) (MLT(J),J=1,JO)
     do I=2,IO
        write(UNITTMP_,29) LZ(I),(XNE(I,J),J=1,JO)
     end do
     close (UNITTMP_)
  end if

  !CC Output from Aaron's ionosphere code
  !CC Didn't work...we need Aaron to diagnose the problem
  !	print *, 'Saving Aaron''s ionosphere model output'
  if ((IA.ge.8 .and. IA.le.11) .or. IA.ge.13) then
     if (me_world.eq.0) call IonoHeidiWriteOutput(1,t,NameRun,NameStep)
  end if

  !CC Start the main loop over species we're calculating
  do S=1,NS
     !	print *, 'WRESULT: ',S,SCALC(S)
     if (SCALC(S).eq.1) then
                       
	if (S.eq.1) NameSpecies='_e' 
	if (S.eq.2) NameSpecies='_h' 
	if (S.eq.3) NameSpecies='he' 
	if (S.eq.4) NameSpecies='_o' 
	
        if (S.eq.1) NameOutputSpecies='electron/'
	if (S.eq.2) NameOutputSpecies='hydrogen/'
	if (S.eq.3) NameOutputSpecies='helium/'
	if (S.eq.4) NameOutputSpecies='oxygen/'



        !.......Find the energy and particle losses
	do I=2,IO
           XNO(I)=XN(I,S)
           XN(I,S)=0
           EO(I)=ENER(I,S)
           ENER(I,S)=0.
           do K=2,KO
              do L=2,LO-1
                 do J=1,JO
                    if(L.lt.UPA(I)) then
                       WEIGHT=F2(I,J,K,L,S)*WE(K)*WMU(L)
                       XN(I,S)=XN(I,S)+WEIGHT  		      ! N in LZ
                       ENER(I,S)=ENER(I,S)+EKEV(K)*WEIGHT     ! E in LZ
                    endif
                 end do
              end do	! K loop
           end do	! L loop
           
           LNC(I,S)=XNO(I)-XN(I,S)
           LEC(I,S)=EO(I)-ENER(I,S)

        end do

        !.......Start the output routines

        !.......Write the phase space distribution, F (IRES(1), 'psd')
        !	IF (MOD(T,21600.).LT.2*DT) THEN	! Only every 6 hours
	if (IRES(1).eq.1) then
           NameSuffix='_psd.'
           
           open(UNIT=UNITTMP_,file=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//&
                NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//NameOutputDir//trim(NameRun)//&
                trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           if (IFAC.eq.1) then
              write (UNITTMP_,*) 'Phase space flux function, PHI = 2EF/m^2'
           else
              write (UNITTMP_,*) 'Phase space distribution function, F'
           end if
           do I=2,IO,2  ! ,4
              do J=1,JO  ! ,3
                 write(UNITTMP_,45) T,LZ(I),MLT(J),KP,XNE(I,J)
                 write(UNITTMP_,44) (ACOSD(MU(IFM(L))),L=1+IFN,19+IFN)
                 do K=2,KO,NEC
                    write(UNITTMP_,43) EKEV(K),(F2(I,J,K,IFM(L),S)/   &
                         FFACTOR(I,K,IFM(L)),L=1+IFN,19+IFN)
                 end do	       ! K loop
              end do		! J loop
           end do		! I loop
           close(UNITTMP_)
	end if
        !	END IF					! 6 hour check

        !.......Write equatorially trapped distribution (IRES(2), 'etf')
	if (IRES(2).eq.1) then
           NameSuffix='_etf.'

           open(UNIT=UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Equatorially trapped distribution function, F'
           NIC(1)=2
           NIC(2)=IO/2
           NIC(3)=IO
           do II=1,3
              I=NIC(II)
              write(UNITTMP_,33) T,LZ(I),KP,ACOSD(MU(2))
              write(UNITTMP_,31) (MLT(J),J=1,JO,3)
              do K=1,KO,NEC
                 write(UNITTMP_,29) EKEV(K),(F2(I,J,K,2,S)/FFACTOR(I,K,L),   &
                      J=1,JO,3)
              end do
           end do
           close(UNITTMP_)
	end if

        !.......Write the plasmaspheric heating (IRES(3), 'dep')
	if (IRES(3).eq.1) then
           XR2=IO/2.
           IR2=IO/2
           NameSuffix='_dep.'
           

           iUnitOut = io_unit_new()

           open(UNIT=iUnitOut,file=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (iUnitOut,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep
           do I=2,IO			! Electron heating
              do J=1,JO
                 EDR=0.
                 do K=2,KO
                    FZERO(I,J,K)=0.
                    do L=2,UPA(I)-1
                       FZERO(I,J,K)=FZERO(I,J,K)+F2(I,J,K,L,S)*CEDR(K,L,S)   &
                            /FFACTOR(I,K,L)
                    end do
                    do L=UPA(I),LO
                       FZERO(I,J,K)=FZERO(I,J,K)+F2(I,J,K,L,S)   &
                            *CEDR(K,UPA(I)-1,S)/FFACTOR(I,K,L)
                    end do
                    if (IFAC.eq.1) FZERO(I,J,K)=FZERO(I,J,K)/EKEV(K)/FLUXFACT(S)
                    EDR=EDR+FZERO(I,J,K)*WE(K)
                 end do
                 EDR=EDR+EKEV(2)*FZERO(I,J,2)-EKEV(KO)*FZERO(I,J,KO)
                 DEP(I,J)=EDR*XNE(I,J)*LZ(I)
              end do
           end do
           write (iUnitOut,*) 'Energy deposition rates of thermal electrons'
           write (iUnitOut,*) 'through Coulomb collisions [eV/cm2/s]'
           write (iUnitOut,15) T,KP
           write (iUnitOut,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (iUnitOut,29) MLT(J),(DEP(I,J),I=2,IO)
           end do
           do I=2,IO			! Ion heating
              do J=1,JO
                 EDR=0.
                 do K=2,KO
                    FZERO(I,J,K)=0.
                    do L=2,UPA(I)-1
                       FZERO(I,J,K)=FZERO(I,J,K)+F2(I,J,K,L,S)*CIDR(K,L,S)   &
                            /FFACTOR(I,K,L)
                    end do
                    do L=UPA(I),LO
                       FZERO(I,J,K)=FZERO(I,J,K)+F2(I,J,K,L,S)   &
                            *CIDR(K,UPA(I)-1,S)/FFACTOR(I,K,L)
                    end do
                    if (IFAC.eq.1) FZERO(I,J,K)=FZERO(I,J,K)/EKEV(K)/FLUXFACT(S)
                    EDR=EDR+FZERO(I,J,K)*WE(K)
                 end do
                 EDR=EDR+EKEV(2)*FZERO(I,J,2)-EKEV(KO)*FZERO(I,J,KO)
                 DEP(I,J)=EDR*XNE(I,J)*LZ(I)
              end do
           end do
           write (iUnitOut,*) 'Heating rates of thermal ions'
           write (iUnitOut,*) 'through Coulomb collisions [eV/cm2/s]'
           write (iUnitOut,15) T,KP
           write (iUnitOut,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (iUnitOut,30) MLT(J),(DEP(I,J),I=2,IO)
           end do
           close(iUnitOut)
	end if

        !.......Write the precipitation flux (IRES(4), '*pf')
	if (IRES(4).eq.1) then	
           do II=1,3
              NameSuffix=IPF(II)
              NEC1=(II-1)*KO/3+1
              NEC2=II*KO/3
            
              open(UNIT=UNITTMP_,file=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                   NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
              write (UNITTMP_,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                   NameSpecies//NameSuffix//NameStep
              write (UNITTMP_,*) 'Total precipitation flux [1/cm2/s]'
              write (UNITTMP_,*) 'Integrated over the energy range:'
              write (UNITTMP_,*) 'Lower edge (keV): ',EKEV(NEC1)
              write (UNITTMP_,*) 'Upper edge (keV): ',EKEV(NEC2)
              write(UNITTMP_,71) T,KP
              write(UNITTMP_,72)
              do I=2,IO
                 do J=1,JO
                    FLUX=0.
                    do K=NEC1,NEC2
                       AVEFL(I,J,K)=0.
                       do L=UPA(I),LO-1
                          AVEFL(I,J,K)=AVEFL(I,J,K)+F2(I,J,K,L,S)*WMU(L)   &
                               /FFACTOR(I,K,L)
                       end do ! L loop
                       AVEFL(I,J,K)=AVEFL(I,J,K)/(MU(LO)-MU(UPA(I)))
                       if (IFAC.eq.2) AVEFL(I,J,K)=AVEFL(I,J,K)*FLUXFACT(S)*EKEV(K)
                       FLUX=FLUX+AVEFL(I,J,K)*PI*WE(K)
                    end do ! K loop
                    write(UNITTMP_,70) LZ(I),PHI(J),FLUX
                 end do	! J loop
              end do	! I loop
              close (UNITTMP_)
           end do
	endif

        !.......Write the differential precip flux (IRES(5), 'flx')
	if (IRES(5).eq.1) then	
           NameSuffix='_flx.'
           
           open(UNIT=UNITTMP_,file=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Differential precipitation fluxes'
           do I=4,IO,2
              !	   IF(I.EQ.4.OR.I.EQ.6.OR.I.EQ.10.OR.I.EQ.16) THEN
              IL=2
              !	    IF(I.EQ.16) IL=1
              do J=1,JO,3
                 !	     IF(J.EQ.1.OR.J.EQ.10.OR.J.EQ.16) THEN
                 write(UNITTMP_,34) T,LZ(I),MLT(J)
                 write(UNITTMP_,40) (ACOSD(MU(L)),L=UPA(I),LO-1,IL)
                 write(UNITTMP_,*)'     EKEV  \ FLUX[1/cm2/s/ster/keV]  AVEFL'
                 if (IFAC.eq.1) then
                    do K=2,KO,NEC
                       write(UNITTMP_,30) EKEV(K),(F2(I,J,K,L,S)/FFACTOR(I,K,L),   &
                            L=UPA(I),LO-1,IL),AVEFL(I,J,K)
                    end do
                 else
                    do K=2,KO,NEC
                       write(UNITTMP_,30) EKEV(K),(F2(I,J,K,L,S)*FLUXFACT(S)*EKEV(K)   &
                            /FFACTOR(I,K,L),L=UPA(I),LO-1,IL),AVEFL(I,J,K)
                    end do
                 end if
                 !	     END IF
              end do
              !	   END IF
           end do
           close (UNITTMP_)
        end if

        !..Write the particle & energy losses (IRES(6), 'los')
	if (IRES(6).eq.1) then
           NameSuffix='_los.'
           
           iUnitOut = io_unit_new() 

           open(UNIT=iUnitOut,file=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (iUnitOut,*) 'Filename: '//NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//NameSpecies//NameSuffix//NameStep
           write (iUnitOut,*) 'Particle and energy losses since last output'
           write(iUnitOut,71) T,KP
           ESUM=0
           NSUM=0
           CSUM=0
           PSUM=0
           ERATE=0
           write(iUnitOut,35)
           do I=2,IO
              XE=ENER(I,S)*FACTOR(S)
              XN1=XN(I,S)*FACTOR(S)
              XLEC=LEC(I,S)*FACTOR(S)
              XLNC=LNC(I,S)*FACTOR(S)
              ESUM=ESUM+XE
              NSUM=NSUM+XN1
              CSUM=CSUM+XLEC
              PSUM=PSUM+XLNC
              write(iUnitOut,30) LZ(I),XE,XN1,XLEC,XLNC
              do J=1,JO
                 do K=2,KO
                    ERATE=ERATE+EKEV(K)*AVEFL(I,J,K)*WE(K)
                 end do
              end do
              ERATE=ERATE*2*PI*Q*ECOF(I)*1e7	!to convert in cm and J
           end do
           write(iUnitOut,60) ESUM,NSUM,CSUM,PSUM
           write(iUnitOut,61) ERATE
           close (iUnitOut)
	end if

        !.......Print out the CFLs for the advection operators (IRES(8), 'cfl')
	if (IRES(8).eq.1) then
           NameSuffix='_cfl.'
           
           iUnitOut = io_unit_new()
           
           open (iUnitOut,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (iUnitOut,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (iUnitOut,*) 'CFL numbers for the advection operators'
           write (iUnitOut,71) T,KP
           write (iUnitOut,*)
           write (iUnitOut,*) 'Coulomb collsion Courant numbers (with e-)'
           do I=2,IO,IO-2
              do J=1,JO,3
                 write (iUnitOut,37) LZ(I),MLT(J),XNE(I,J)
                 write (iUnitOut,40) (ACOSD(MU(L)),L=2,LO,NLC),ACOSD(MU(LO-1))
                 do K=2,KO,KO-2
                    write (iUnitOut,30) EKEV(K),(COULE(I,K,L,S)*XNE(I,J),L=2,LO,NLC),   &
                         COULE(I,K,LO-1,S)*XNE(I,J)
                 end do
              end do
           end do
           write (iUnitOut,*)
           write (iUnitOut,*) 'Coulomb collsion Courant numbers (with ions)'
           do I=2,IO,IO-2
              do J=1,JO,3
                 write (iUnitOut,37) LZ(I),MLT(J),XNE(I,J)
                 write (iUnitOut,40) (ACOSD(MU(L)),L=2,LO,NLC),ACOSD(MU(LO-1))
                 do K=2,KO,KO-2
                    write (iUnitOut,30) EKEV(K),(COULI(I,K,L,S)*XNE(I,J),L=2,LO,NLC),   &
                         COULI(I,K,LO-1,S)*XNE(I,J)
                 end do
              end do
           end do
           write (iUnitOut,*)
           write (iUnitOut,*) 'Radial drift Courant numbers'
           write (iUnitOut,41) ' L \ MLT =',(MLT(J),J=1,JO,3)
           do I=2,IO,4
              write (iUnitOut,42) LZ(I),(VR(I,J),J=1,JO,3)
           end do
           write (iUnitOut,*)
           write (iUnitOut,*) 'Azimuthal drift Courant numbers'
           do I=2,IO,IO-2
              do J=1,JO,3
                 write (iUnitOut,36) LZ(I),MLT(J)
                 write (iUnitOut,40) (ACOSD(MU(L)),L=2,LO,NLC),ACOSD(MU(LO-1))
                 do K=2,KO,KO-2
                    write (iUnitOut,30) EKEV(K),(P1(I,J)+P2(I,K,L),L=2,LO,NLC),   &
                         P1(I,J)+P2(I,K,LO-1)
                 end do
              end do
           end do
           write (iUnitOut,*)
           write (iUnitOut,*) 'Energy drift Courant numbers'
           do I=2,IO,IO-2
              do J=1,JO,3
                 write (iUnitOut,36) LZ(I),MLT(J)
                 write (iUnitOut,40) (ACOSD(MU(L)),L=2,LO,NLC),ACOSD(MU(LO-1))
                 do K=2,KO,KO-2
                    write (iUnitOut,30) EKEV(K),(VR(I,J)*EDOT(I,J,K,L),L=2,LO,NLC),   &
                         VR(I,J)*EDOT(I,J,K,LO-1)
                 end do
              end do
           end do
           write (iUnitOut,*)
           write (iUnitOut,*) 'Mu drift Courant numbers'
           do I=2,IO,IO-2
              write (iUnitOut,*) ' L =',LZ(I)
              write (iUnitOut,40) (ACOSD(MU(L)),L=2,LO,NLC),ACOSD(MU(LO-1))
              do J=1,JO,3
                 write (iUnitOut,30) MLT(J),(VR(I,J)*MUDOT(I,J,L),L=2,LO,NLC),   &
                      VR(I,J)*MUDOT(I,J,LO-1)
              end do
           end do
           write (iUnitOut,*) 'Energy advection |CFL| > 1 (entire grid)'
           write (iUnitOut,38) 'L-shell','MLT(hr)','E(keV)','PA(deg)','n(cm-3)',   &
                'ABS CFL'
           Ibad=0
           do I=1,IO
              do J=1,JO
                 do K=1,KO
                    do L=1,LO	     ! UPA(I)-1 , changed to include the l.c.
                       CFL=abs(EDOT(I,J,K,L)*VR(I,J)+   &
                            (COULI(I,K,L,S)+COULE(I,K,L,S))*XNE(I,J))
                       if (CFL.gt.1.) then
                          write (iUnitOut,39)LZ(I),MLT(J),EKEV(K),ACOSD(MU(L)),XNE(I,J),CFL
                          Ibad=Ibad +1
                       end if
                    end do	! L loop
                 end do	! K loop
              end do	! J loop
           end do	! I loop
           write (iUnitOut,*) 'Bad CFLs: ',Ibad
           close(iUnitOut)
	end if

        !.......Print out the drift velocities (IRES(9), 'drf')
	if (IRES(9).eq.1) then
           NameSuffix='_drf.'
           
           open (UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Spatial coordinate drift velocities'
           write (UNITTMP_,71) T,KP
           write (UNITTMP_,*)
           write (UNITTMP_,*) 'Radial drift component'
           write (UNITTMP_,41) ' L \ MLT =',(MLT(J),J=1,JO,2)
           do I=2,IO,2
              write (UNITTMP_,42) LZ(I),(VR(I,J),J=1,JO,2)
           end do
           write (UNITTMP_,*)
           write (UNITTMP_,*) 'Azimuthal drift component at various E,mu'
           do L=1,3
              do K=1,3
                 write (UNITTMP_,36) EKEV(EV(K)),ACOSD(MU(PAV(L)))
                 write (UNITTMP_,41) ' L \ MLT =',(MLT(J),J=1,JO,2)
                 do I=2,IO,2
                    write (UNITTMP_,42) LZ(I),(P1(I,J)+P2(I,EV(K),PAV(L)),J=1,JO,2)
                 end do
              end do
           end do
           close(UNITTMP_)
	end if

        !.......Print out midnight energy vs. L distributions (IRES(10), 'evl')
        !.......Also noon distributions, at PA=90 deg and at UPA(I) boundary
	if (IRES(10).eq.1) then
           NameSuffix='_evl.'
           
           open (UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Energy-Lshell spectra at MLT=0 and PA=90'
           write (UNITTMP_,71) T,KP
           write(UNITTMP_,41) '   Ne =',(XNE(I,1),I=2,IO)
           write (UNITTMP_,41) ' E \ L =',(LZ(I),I=2,IO)
           do K=2,KO
              write(UNITTMP_,43) EKEV(K),(F2(I,1,K,2,S)/FFACTOR(I,K,2),I=2,IO)
           end do		! K loop
           write (UNITTMP_,*) 'Energy-Lshell spectra at MLT=12 and PA=90'
           write (UNITTMP_,71) T,KP
           write(UNITTMP_,41) '   Ne =',(XNE(I,13),I=2,IO)
           write (UNITTMP_,41) ' E \ L =',(LZ(I),I=2,IO)
           do K=2,KO
              write(UNITTMP_,43) EKEV(K),(F2(I,13,K,2,S)/FFACTOR(I,K,2),I=2,IO)
           end do		! K loop
           write (UNITTMP_,*) 'Energy-Lshell spectra at MLT=0 and PA=UPA'
           write (UNITTMP_,71) T,KP
           write(UNITTMP_,41) '   Ne =',(XNE(I,1),I=2,IO)
           write (UNITTMP_,41) ' E \ L =',(LZ(I),I=2,IO)
           do K=2,KO
              write(UNITTMP_,43) EKEV(K),(F2(I,1,K,UPA(I)-1,S)/   &
                   FFACTOR(I,K,UPA(I)-1),I=2,IO)
           end do		! K loop
           write (UNITTMP_,*) 'Energy-Lshell spectra at MLT=12 and PA=UPA'
           write (UNITTMP_,71) T,KP
           write(UNITTMP_,41) '   Ne =',(XNE(I,13),I=2,IO)
           write (UNITTMP_,41) ' E \ L =',(LZ(I),I=2,IO)
           do K=2,KO
              write(UNITTMP_,43) EKEV(K),(F2(I,13,K,UPA(I)-1,S)/   &
                   FFACTOR(I,K,UPA(I)-1),I=2,IO)
           end do		! K loop
           close(UNITTMP_)
	end if

        !.......Print lifetimes [hr] and Coulomb diff coeff (IRES(11), 'lft')
	if (IRES(11).eq.1) then
           NameSuffix='_lft.'
          
           open(UNIT=UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,71) T,KP
           do  I=4,IO,6
              L=UPA(I)-1
              do  J=1,1
                 write(UNITTMP_,*)' Lifetimes for ',NameSpecies,' rc ion; PA=',   &
                      ACOSD(MU(L))
                 write(UNITTMP_,*)' L   E[KEV]  TAUBO[HR] TAUCHE[HR] TAUD[HR]'
                 do K=2,KO
                    TAUD=2*pi*ME/abs(1.44E-2*RE-3*EKEV(K)*1000*LZ(I))/RE/   &
                         (1-FUNI(MU(L))/6/FUNT(MU(L)))/3600.
                    TAUBO=4*LZ(I)*RE/V(K,S)*FUNT(MU(L))/3600.
                    TAUCHE=-DT/ALOG(achar(I,K,L,S))/3600.
                    write(UNITTMP_,80) LZ(I),EBND(K),TAUBO,TAUCHE,TAUD
                 enddo
              enddo
           enddo
           close (UNITTMP_)
	end if

        !.......Print out pressures, densities, and Dst (IRES(12), 'prs')
	if (IRES(12).eq.1) then
           NameSuffix='_prs.'
           
           open (UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Pressures, densities, etc. for RC species ',NameSpecies
           write (UNITTMP_,71) T,KP
           write (UNITTMP_,*) 'Total Energy [keV]   Total Particles   Dst [nT]'
           write (UNITTMP_,73) ETOT(S),NTOT(S),Dst(S)
           call write_prefix; write(iUnitStdOut,*)  'WRESULT Dst: ',S,Dst(S)
           write (UNITTMP_,*) 'Equatorial density [cm-3]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(RNHT(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Equatorial perpendicular pressure [keV cm-3]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(PPER(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Equatorial parallel pressure [keV cm-3]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(PPAR(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Equatorial anisotropy [Tper/Tpar - 1]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(ANIS(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Equatorial energy density [keV cm-3]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(EDEN(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Equatorial azimuthal current [A m-2]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(JPER(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Total particle count in the spatial volume [ions]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(Nspace(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Total energy in the spatial volume [keV]'
           write (UNITTMP_,31) (LZ(I),I=2,IO)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(Espace(I,J,S),I=2,IO)
           end do
           write (UNITTMP_,*) 'Base convection potentials [kV]'
           write (UNITTMP_,31) (Lsh(I),I=1,Ir)
           do J=1,JO
              write (UNITTMP_,29) MLT(J),(BASEPOT(I,J)*1.E-3,I=1,Ir)
           end do
           !	 IF (IA.GE.8) THEN
	   write (UNITTMP_,*) 'Total azimuthal current [A]'
	   write (UNITTMP_,31) (Lsh(I),I=1,Ir)
	   do J=1,JO
              write (UNITTMP_,29) MLT(J),(Iphi(I,J,S),I=1,Ir)
	   end do
	   write (UNITTMP_,*) 'Total radial current [A]'
	   write (UNITTMP_,31) (Lsh(I),I=1,Ir)
	   do J=1,JO
              write (UNITTMP_,29) MLT(J),(Irad(I,J,S),I=1,Ir)
	   end do
	   write (UNITTMP_,*) 'Field-aligned current density into one ',   &
                'hemisphere [A m-2]'
	   write (UNITTMP_,31) (Lsh(I),I=1,Ir)
	   do J=1,JO
              write (UNITTMP_,29) MLT(J),(Jion1(I,J,S),I=1,Ir)
	   end do
	   write (UNITTMP_,*) 'Potentials from RC FACs [kV]'
	   write (UNITTMP_,31) (Lsh(I),I=1,Ir)
	   do J=1,JO
              write (UNITTMP_,29) MLT(J),(FPOT(I,J)*1.E-3,I=1,Ir)
	   end do
	   write (UNITTMP_,*) 'FAC density from all species into one ',   &
                'hemisphere [A m-2]'
	   write (UNITTMP_,31) (Lsh(I),I=1,Ir)
	   do J=1,JO
              write (UNITTMP_,29) MLT(J),(Jfac(I,J),I=1,Ir)
	   end do
           !	 END IF
           close (UNITTMP_)
	end if

        !.......Write out F2 to an unformatted file (IRES(13), 'unff')
	if (IRES(13).eq.1) then
           NameSuffix='.unff'
           
           !open(UNIT=UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
           !     NameSpecies//NameSuffix,status='unknown',   &
           !     form='unformatted')
           
          open(UNIT=UNITTMP_,FILE=NameRestartOutDir//trim(NameRun)//&
                NameSpecies//NameSuffix,status='unknown',   &
                form='unformatted')
           
           do L=1,NPA
              do K=1,NE
                 do J=1,NT
                    write(UNITTMP_) (f2(I,J,K,L,S),I=1,NR)
                 end do
              end do
           end do
           close(UNITTMP_)
	end if

        !.......Open file for source/loss continual output (IRES(14), 'sal')
        if (IRES(14).eq.1) then
           
           iUnitSal1 = io_unit_new()
           iUnitSal2 = io_unit_new()
           iUnitSal3 = io_unit_new()
           iUnitSal4 = io_unit_new()

           if (S.eq.1) iUnitSal = iUnitSal1
           if (S.eq.2) iUnitSal = iUnitSal2
           if (S.eq.3) iUnitSal = iUnitSal3
           if (S.eq.4) iUnitSal = iUnitSal4
           
           close (iUnitSal)
           NameSuffix='_sal.'
           open (iUnitSal,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (iUnitSal,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (iUnitSal,*) 'Sources and losses: continuous output'
           write (iUnitSal,71) T,KP
           write (iUnitSal,74) 'T','LMP6','LMP12','RNS','RNL','RES','REL',   &
                'ESN','ELN','ESE','ELE','ECN','ECE','ALN','ALE','CEN',   &
                'CEE','DNT','DET'
	end if

        !.......Print out nightside boundary condition (IRES(15), 'fbc')
	if (IRES(15).eq.1) then
           NBC(1:JO)=0.
           RFAC=4.*PI*sqrt(2.)*1.E-24*(Q*1.E3/MP/M1(S))**1.5
           do L=1,LO
              do K=2,KO
                 do J=1,JO
                    NBC(J)=NBC(J)+FGEOS(J,K,L,S)*ERNM(IO,K,L)*ERNH(K,S)*RFAC
                 end do
              end do
           end do
           NameSuffix='_fbc.'
           
           open (UNITTMP_,FILE=NameOutputDir//trim(NameOutputSpecies)//trim(NameRun)//&
                NameSpecies//NameSuffix//NameStep,STATUS='UNKNOWN')
           write (UNITTMP_,*) 'Filename: '//trim(NameRun)//trim(NameOutputSpecies)//NameSpecies//NameSuffix//NameStep
           write (UNITTMP_,*) 'Nightside boundary conditions'
           write (UNITTMP_,38) 'Ninj','Einj','Kinj','NSWB','USWB'
           write (UNITTMP_,39) Ninj,Einj,Kinj,NSWB,USWB
           write (UNITTMP_,*) 'Boundary densities'
           write (UNITTMP_,38) 'MLT','N,cm-3'
           do J=1,JO
              write (UNITTMP_,30) MLT(J),NBC(J)
           end do
           if (IFAC.eq.1) then
              write (UNITTMP_,*) 'Phase space flux function, PHI = 2EF/m^2'
           else
              write (UNITTMP_,*) 'Phase space distribution function, F'
           end if
           do J=-3,5,2
              II=J
              if (II.lt.0) II=II+JO
              write(UNITTMP_,45) T,LZ(IO)+DL1,MLT(II),KP,0.
              write(UNITTMP_,44) (ACOSD(MU(IFM(L))),L=1+IFN,19+IFN)
              do K=2,KO,NEC
                 write(UNITTMP_,43) EKEV(K),(FGEOS(II,K,IFM(L),S)/   &
                      FFACTOR(IO,K,IFM(L)),L=1+IFN,19+IFN)
              end do	! K loop
           end do		! J loop
           close (UNITTMP_)
	end if

     end if		! SCALC check
  end do		! S loop

15 format(' EKEV \ T =',F8.0,2X,'Kp =',F6.2,10X)
16 format('LSHELL\ T =',F8.0,2X,'Kp =',F6.2,10X)
29 format(F7.3,1P,40E11.3)
30 format(F7.3,1P,20E10.3)
31 format(' MLT\L ',40(3X,F8.2))
32 format(' EKEV \ T=',F8.0,' L=',F6.2,' Kp=',F6.2,' MLT=',F4.1)
33 format(' EKEV \ T=',F8.0,' L=',F6.2,' Kp=',F6.2,' PA=',F6.2)
34 format(' T=',F8.0,' L=',F6.2,' MLT=',F6.2)
35 format(2X,'L',4X,'Energy[keV]',3X,'# Part',3X,'Ener Loss',   &
       2X,'Part Loss')
36 format(' EKEV \ L=',F6.2,' MLT=',F4.1)
37 format(' EKEV \ L=',F6.2,' MLT=',F4.1,' Ne=',1PE11.3)
38 format(20(2X,A7,1X))
39 format(1P,20E10.3)
40 format(' PA =',20(2X,F8.2))
41 format(A10,1P,20E10.3)
42 format(F6.2,4X,1P,20E10.3)
43 format(F7.3,1P,20E9.2)
44 format(' PA =',20(1X,F8.2))
45 format(' EKEV \ T=',F8.0,' L=',F6.2,' MLT=',F4.1,' Kp=',F6.2,   &
       ' Ne=',1PE11.3)
46 format('EKEV = ',F7.3,' PA = ',F6.2)
60 format(/,4X,'Total',1P,5(2X,E10.3))
61 format(/,4X,'DEP ENERGY AT 1000 KM =',1PE11.4,' J/s')
70 format(F5.2,F10.6,1P,E13.3)
71 format(2X,'T =',F8.0,2X,'Kp =',F6.2)
72 format(2X,'L',4X,'AZIMUTH',4X,'I P FLUX')
73 format(1P,3E16.4)
74 format(A8,2A6,16A10)
80 format(F6.2,F9.3,1P,8E11.3)

  return
end subroutine WRESULT

!=========================================================================
!			     PSRCLOSS
!     Continuous printing of energy and particle sources and losses
!	Also resets values for the next time step
!=========================================================================

subroutine PSRCLOSS(T)

  use ModHeidiSize
  use ModHeidiIO

  implicit none

  real :: T,DN,DE
  !--------------------------------------------------------------------------

  DN=RNS-RNL+ESN-ELN+ECN-ALN-CEN
  DE=RES-REL+ESE-ELE+ECE-ALE-CEE
  write (iUnitSal,50) T,LMP(7),LMP(13),RNS,RNL,RES,REL,ESN,ELN,ESE,   &
       ELE,ECN,ECE,ALN,ALE,CEN,CEE,DN,DE
  RNS=0.	! Radial drift particle source
  RNL=0.	! Radial drift particle loss
  RES=0.	! Radial drift energy source
  REL=0.	! Radial drift energy loss
  ESN=0.	! Energy drift particle gain
  ELN=0.	! Energy drift particle loss
  ESE=0.	! Energy drift energy gain
  ELE=0.	! Energy drift energy loss
  ECN=0.	! Coulomb collision particle gain/loss
  ECE=0.	! Coulomb collision energy gain/loss
  ALN=0.	! Atmospheric loss particle gain/loss
  ALE=0.	! Atmospheric loss energy gain/loss
  CEN=0.	! Charge exchange particle gain/loss
  CEE=0.	! Charge exchange energy gain/loss
50 format(F8.1,2F6.2,1P,16E10.2)
  return

end subroutine PSRCLOSS

