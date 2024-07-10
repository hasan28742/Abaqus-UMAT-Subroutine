	SUBROUTINE UMAT (STRESS,STATEV,DDSDDE,SSE,SPD,    &
					SCD,RPL,DDSDDT,DRPLDE,DRPLDT,     &
					STRAN,DSTRAN,TIME,DTIME,TEMP,     &
					DTEMP,PREDEF,DPRED,CMNAME,NDI,    &
					NSHR,NTENS,NSTATV,PROPS,NPROPS,  &
					COORDS,DROT,PNEWDT,CELENT,DFGRD0, &
					DFGRD1,NOEL,NPT,LAYER,KSPT,       &
					JSTEP,KINC)
					
					
					
	implicit none
	! PROPS(1) - E
	! PROPS(2) - NU

	character*80,intent(in)::CMNAME
	INTEGER(kind=4),intent(in)::NTENS,NSTATV,NPROPS,&
								NDI,NSHR,NOEL,NPT,&
								KSPT,JSTEP,KINC,LAYER

	REAL(KIND=8)::SSE,SPD,SCD,RPL,DRPLDT,&
				  DTIME,TEMP,DTEMP,PNEWDT,CELENT

	REAL(KIND=8)::STRESS(NTENS),STATEV(NSTATV),&
				  DDSDDE(NTENS,NTENS),DDSDDT(NTENS),&
				  DRPLDE(NTENS),STRAN(NTENS),&
				  DSTRAN(NTENS),TIME(2),&
				  PREDEF(1),DPRED(1),&
				  PROPS(NPROPS),COORDS(3),&
				  DROT(3,3),DFGRD0(3,3),&
				  DFGRD1(3,3)
				  
	REAL(KIND=8)::EMOD,NU,ALAME,BLAME,CLAME

	!-----------------------
	REAL(8),PARAMETER::ONE=1.0D0,TWO=2.0D0
	REAL(8):: LLAME, MLAME
	integer, parameter :: I = 6
	EMOD=PROPS(1)
	NU=PROPS(2)

	LLAME=Emod*NU/(ONE+NU)/(ONE-2*NU)
	MLAME=Emod/2/(1+NU)

	DDSDDE = 0


	DDSDDE(1,1)=LLAME+2*MLAME
	DDSDDE(1,2)=LLAME
	DDSDDE(1,3)=LLAME
	DDSDDE(2,2)=LLAME+2*MLAME
	DDSDDE(2,3)=LLAME
	DDSDDE(3,3)=LLAME+2*MLAME
	DDSDDE(4,4)=LLAME
	DDSDDE(5,5)=LLAME
	DDSDDE(6,6)=LLAME
	



	STRESS = STRESS + MATMUL(DDSDDE,DSTRAN)

	do i=1,NTENS
   		write (*,"(*(1x,i6))") DDSDDE(i,:) ! unlimited format used, with 6 positions for each integer, separated by a space
	end do
		!write (*,"(3(1x,i6))") transpose(DDSDDE) :
	print *, 'Hello, World!'
	!print *, DDSDDE
	PRINT *, STRESS


RETURN
end
