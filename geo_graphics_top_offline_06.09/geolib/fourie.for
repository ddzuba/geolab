      SUBROUTINE FOURIE(POROG,N,RI,RI1)
	!USE DFIMSL
      IMPLICIT REAL*4(A-H,O,P,R-Z)
	COMPLEX(4) RF(N),RF1(N)
	DIMENSION FRIQ(N)
	DIMENSION RI(N),RI1(N)

! ******************************************************************************* 
!	WDESKR=1000.D0/TDESKR/N			! ���������� ���� �� ��� ������ 
									! � ����������
	CALL FFTCF(N,CMPLX(RI),RF)      ! ������ ����� - ��������������

	ISUMFRQ=0									!
	SUMFRQ=0.						! ��������� ��������� ������� �������� ��� 
	SUM=0.d0						! ������� ����������������  �������
	N2=N/2							!
									!
	DO  I=1,N2						! ���������� ������ ������������� 
	FRIQ(I)=ABS(RF(I))				! �������������  �����
	SUMFRQ=SUMFRQ+(I-1)*FRIQ(I)		! ������ �������
	SUM=SUM+FRIQ(I)					! ����� ������
	END DO							!
	ISUMFRQ=SUMFRQ/SUM !*WDESKR		! ���������������� �������			 
! *******************************************************************************
C ���������� ������� - �������
	ISUMFRQ1=ISUMFRQ/POROG
	ISUMFRQ=N2-ISUMFRQ1
	DO I=1,N2
	IF(I.LT.ISUMFRQ1.OR.I.GT.ISUMFRQ) THEN
!	IF(I.LT.ISUMFRQ) THEN
	RF(I)=(0,0)
	RF(N-I+1)=(0,0)
	END IF
	END DO
! *******************************************************************************

	CALL FFTCB(N,RF,RF1)	    	! �������� �����-��������������
	
	DO I=1,N						! ��������� ����������
	RI1(I)= DBLE(RF1(I))/N			! 
	END DO							! 
	RETURN
      END